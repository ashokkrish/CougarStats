# R/regressionAndCorrelation.R

regressionAndCorrelationUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),

      # Data input mode (drives methodology choices and input widgets below)
      radioButtons(
        ns("dataInputMode"),
        tags$b("Data"),
        choices  = list("Upload File" = "upload", "Enter Raw Data" = "raw"),
        selected = "upload",
        inline   = TRUE
      ),

      # Upload widgets
      conditionalPanel(
        condition = "input.dataInputMode == 'upload'",
        ns = ns,
        HTML(uploadDataDisclaimer),
        fileInput(
          ns("regUserData"),
          label  = strong("Upload your data (.csv, .xls, .xlsx, .txt, .sas7bdat, .sav, .dta, .rds, .mtp, .mwx, .mpx)"),
          accept = c("text/csv", "text/comma-separated-values",
                     "text/tab-separated-values", "text/plain",
                     ".csv", ".txt", ".xls", ".xlsx",
                     ".sas7bdat", ".sav", ".dta", ".rds",
                     ".mtp", ".mwx", ".mpx")
        ),
        conditionalPanel(
          condition = "output.regShowSheetPicker == true",
          ns = ns,
          selectizeInput(
            ns("regSheet"),
            label    = strong("Choose a Sheet"),
            choices  = c(""),
            multiple = FALSE,
            options  = list(placeholder  = "Select a sheet",
                            onInitialize = I('function() { this.setValue(""); }'))
          )
        ),
        uiOutput(ns("regDataStatus"))
      ),

      # Raw entry widgets (SLR and POLYR only — methodology radio filters to those two)
      conditionalPanel(
        condition = "input.dataInputMode == 'raw'",
        ns = ns,
        withMathJax(
          textAreaInput(
            ns("rawY"),
            label       = strong("Response Variable (\\(y\\))"),
            value       = "2.48, 2.26, 2.47, 2.77, 2.99, 3.05, 3.18, 3.46, 3.03, 3.26, 2.67, 2.53",
            placeholder = "Enter numeric values separated by commas or spaces (e.g. 1,2,3 or 1 2 3)",
            rows        = 3
          ),
          textAreaInput(
            ns("rawX"),
            label       = strong("Explanatory Variable (\\(x\\))"),
            value       = "4.51, 3.58, 4.31, 5.06, 5.64, 4.99, 5.29, 5.83, 4.70, 5.61, 4.90, 4.20",
            placeholder = "Enter numeric values separated by commas or spaces (e.g. 1,2,3 or 1 2 3)",
            rows        = 3
          )
        )
      ),

      radioButtons(
        ns("multiple"),
        tags$b("Methodology"),
        choices  = list(
          "Simple Linear Regression and Correlation Analysis" = "SLR",
          "Multiple Linear Regression"                        = "MLR",
          "Polynomial Regression"                             = "POLYR",
          "Binary Logistic Regression"                        = "LOGR"
        ),
        selected = "SLR"
      ),

      uiOutput(ns("regressionSidebarUI"))
    ),
    mainPanel(
      uiOutput(ns("regressionMainPanelUI"))
    )
  )
}

regressionAndCorrelationServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ---- Raw entry validation (mirrors former slrraw_iv / polyraw_iv) -------
    sampleInfoRaw <- eventReactive({
      input$rawX
      input$rawY
    }, {
      dat  <- list()
      datx <- createNumLst(input$rawX)
      daty <- createNumLst(input$rawY)
      dat$diff <- length(datx) - length(daty)
      dat$xSD  <- if (length(datx) > 1) sd(datx) else 0
      dat$ySD  <- if (length(daty) > 1) sd(daty) else 0
      dat
    })

    regraw_iv <- InputValidator$new()
    regraw_iv$add_rule("rawX", sv_required())
    regraw_iv$add_rule("rawX", ~ if (nzchar(trimws(input$rawX)) && length(createNumLst(input$rawX)) == 0)
      "Data must be numeric values separated by commas or spaces (ie: 2,3,4 or 2 30 400).")
    regraw_iv$add_rule("rawX", ~ if (length(createNumLst(input$rawX)) < 4)
      "Sample data must include at least four numeric observations.")
    regraw_iv$add_rule("rawX", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$diff != 0)) "x and y must have the same number of observations.",
      error = function(e) NULL
    ))
    regraw_iv$add_rule("rawX", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$xSD == 0))
        "Explanatory variable has a standard deviation equal to zero (all values are identical). At least two distinct values are required.",
      error = function(e) NULL
    ))
    regraw_iv$add_rule("rawY", sv_required())
    regraw_iv$add_rule("rawY", ~ if (nzchar(trimws(input$rawY)) && length(createNumLst(input$rawY)) == 0)
      "Data must be numeric values separated by commas or spaces (ie: 2,3,4 or 2 30 400).")
    regraw_iv$add_rule("rawY", ~ if (length(createNumLst(input$rawY)) < 4)
      "Sample data must include at least four numeric observations.")
    regraw_iv$add_rule("rawY", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$diff != 0)) "x and y must have the same number of observations.",
      error = function(e) NULL
    ))
    regraw_iv$add_rule("rawY", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$ySD == 0))
        "Response variable is constant. Correlation is undefined when a variable has a standard deviation equal to zero.",
      error = function(e) NULL
    ))
    regraw_iv$condition(~ isTRUE(input$dataInputMode == "raw"))
    regraw_iv$enable()

    # ---- Sheet picker -------------------------------------------------------
    output$regShowSheetPicker <- reactive({
      if (is.null(input$regUserData)) return(FALSE)
      tolower(tools::file_ext(input$regUserData$name)) %in% c("xls", "xlsx")
    })
    outputOptions(output, "regShowSheetPicker", suspendWhenHidden = FALSE)

    observeEvent(input$regUserData, {
      req(input$regUserData)
      ext <- tolower(tools::file_ext(input$regUserData$name))
      if (ext %in% c("xls", "xlsx")) {
        sheets <- tryCatch(readxl::excel_sheets(input$regUserData$datapath),
                           error = function(e) character(0))
        freezeReactiveValue(input, "regSheet")
        updateSelectizeInput(session, "regSheet",
                             choices  = sheets,
                             selected = if (length(sheets)) sheets[1] else "")
      } else {
        updateSelectizeInput(session, "regSheet", choices = character(0), selected = "")
      }
    }, priority = 50)

    # ---- Uploaded file reactive ---------------------------------------------
    reg_upload_data <- eventReactive(list(input$regUserData, input$regSheet), {
      req(input$regUserData)
      ext  <- tolower(tools::file_ext(input$regUserData$name))
      path <- input$regUserData$datapath
      if (ext %in% c("xls", "xlsx")) {
        req(input$regSheet)
        req(input$regSheet %in% readxl::excel_sheets(path))
      }
      dat <- readUploadedDataFile(ext, path, input$regSheet)
      dat <- dat[, colSums(!is.na(dat)) > 0, drop = FALSE]
      dat <- dat[rowSums(!is.na(dat)) > 0, , drop = FALSE]
      dat
    })

    # ---- Shared data reactive (passed to all child modules) -----------------
    reg_data <- reactive({
      if (input$dataInputMode == "raw") {
        x_vals <- createNumLst(input$rawX)
        y_vals <- createNumLst(input$rawY)
        if (length(x_vals) >= 4 && length(y_vals) >= 4 && length(x_vals) == length(y_vals))
          data.frame(x = x_vals, y = y_vals)
        else
          NULL
      } else {
        tryCatch(reg_upload_data(), error = function(e) NULL)
      }
    })

    # Reactive conveying the current input mode to children that need it (SLR, POLYR)
    input_mode <- reactive({ input$dataInputMode })

    # ---- Data status label (upload mode only) --------------------------------
    output$regDataStatus <- renderUI({
      req(input$dataInputMode == "upload")
      dat <- tryCatch(reg_upload_data(), error = function(e) NULL)
      if (is.null(dat)) return(NULL)
      div(
        class = "alert alert-success",
        style = "padding: 5px 10px; font-size: 12px; margin-top: 2px; margin-bottom: 10px;",
        icon("circle-check"),
        HTML(paste0(" <strong>File loaded:</strong> ", input$regUserData$name,
                    " (", nrow(dat), " rows × ", ncol(dat), " columns)"))
      )
    })

    # Resets the parent file input — passed to children so their Reset button can clear it
    reset_upload <- function() shinyjs::reset("regUserData")

    # ---- Update methodology choices based on data input mode ----------------
    observeEvent(input$dataInputMode, {
      current <- isolate(input$multiple)
      if (input$dataInputMode == "raw") {
        updateRadioButtons(session, "multiple",
                           choices  = list(
                             "Simple Linear Regression and Correlation Analysis" = "SLR",
                             "Polynomial Regression"                             = "POLYR"
                           ),
                           selected = if (current %in% c("SLR", "POLYR")) current else "SLR")
      } else {
        updateRadioButtons(session, "multiple",
                           choices  = list(
                             "Simple Linear Regression and Correlation Analysis" = "SLR",
                             "Multiple Linear Regression"                        = "MLR",
                             "Polynomial Regression"                             = "POLYR",
                             "Binary Logistic Regression"                        = "LOGR"
                           ),
                           selected = current)
      }
    })

    # ---- Dynamic module routing (counter pattern preserved) -----------------
    slr_instance_counter  <- reactiveVal(0)
    mlr_instance_counter  <- reactiveVal(0)
    logr_instance_counter <- reactiveVal(0)
    polyr_instance_counter <- reactiveVal(0)

    current_slr_module_id   <- reactive({ paste0("slr_dynamic_instance_",  slr_instance_counter()) })
    current_mlr_module_id   <- reactive({ paste0("mlr_dynamic_instance_",  mlr_instance_counter()) })
    current_logr_module_id  <- reactive({ paste0("logr_dynamic_instance_", logr_instance_counter()) })
    current_polyr_module_id <- reactive({ paste0("polyr_dynamic_instance_", polyr_instance_counter()) })

    observeEvent(input$multiple, {
      if (input$multiple == "SLR") {
        slr_instance_counter(slr_instance_counter() + 1)
        output$regressionSidebarUI   <- renderUI({ req(current_slr_module_id()); SLRSidebarUI(session$ns(current_slr_module_id())) })
        output$regressionMainPanelUI <- renderUI({ req(current_slr_module_id()); SLRMainPanelUI(session$ns(current_slr_module_id())) })
      } else if (input$multiple == "MLR") {
        mlr_instance_counter(mlr_instance_counter() + 1)
        output$regressionSidebarUI   <- renderUI({ req(current_mlr_module_id()); MLRSidebarUI(session$ns(current_mlr_module_id())) })
        output$regressionMainPanelUI <- renderUI({ req(current_mlr_module_id()); MLRMainPanelUI(session$ns(current_mlr_module_id())) })
      } else if (input$multiple == "LOGR") {
        logr_instance_counter(logr_instance_counter() + 1)
        output$regressionSidebarUI   <- renderUI({ req(current_logr_module_id()); LogisticRegressionSidebarUI(session$ns(current_logr_module_id())) })
        output$regressionMainPanelUI <- renderUI({ req(current_logr_module_id()); LogisticRegressionMainPanelUI(session$ns(current_logr_module_id())) })
      } else if (input$multiple == "POLYR") {
        polyr_instance_counter(polyr_instance_counter() + 1)
        output$regressionSidebarUI   <- renderUI({ req(current_polyr_module_id()); PolynomialRegressionSidebarUI(session$ns(current_polyr_module_id())) })
        output$regressionMainPanelUI <- renderUI({ req(current_polyr_module_id()); PolynomialRegressionMainPanelUI(session$ns(current_polyr_module_id())) })
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    observeEvent(current_slr_module_id(), {
      req(input$multiple == "SLR")
      SLRServer(current_slr_module_id(), reg_data, input_mode, reset_upload)
    }, ignoreNULL = TRUE)

    observeEvent(current_mlr_module_id(), {
      req(input$multiple == "MLR")
      MLRServer(current_mlr_module_id(), reg_data, reset_upload)
    }, ignoreNULL = TRUE)

    observeEvent(current_logr_module_id(), {
      req(input$multiple == "LOGR")
      LogisticRegressionServer(current_logr_module_id(), reg_data, reset_upload)
    }, ignoreNULL = TRUE)

    observeEvent(current_polyr_module_id(), {
      req(input$multiple == "POLYR")
      PolynomialRegressionServer(current_polyr_module_id(), reg_data, input_mode, reset_upload)
    }, ignoreNULL = TRUE)

  })
}
