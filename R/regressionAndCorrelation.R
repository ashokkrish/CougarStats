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
        choices  = list("Upload Data" = "upload", "Enter Raw Data" = "raw"),
        selected = "upload",
        inline   = TRUE
      ),

      # Upload widgets
      conditionalPanel(
        condition = "input.dataInputMode == 'upload'",
        ns = ns,
        HTML(uploadDataDisclaimer),
        tags$style(HTML(sprintf(
          "#%s .progress { display: none !important; }
           #%s .form-group { margin-bottom: 2px !important; }",
          ns("regFileInputWrapper"), ns("regFileInputWrapper")
        ))),
        div(
          id = ns("regFileInputWrapper"),
          fileInput(
            ns("regUserData"),
            label  = strong("Upload your data (.csv, .xls, .xlsx, .txt, .sas7bdat, .sav, .dta, .rds, .mtp, .mwx, .mpx)"),
            accept = c("text/csv", "text/comma-separated-values",
                       "text/tab-separated-values", "text/plain",
                       ".csv", ".txt", ".xls", ".xlsx",
                       ".sas7bdat", ".sav", ".dta", ".rds",
                       ".mtp", ".mwx", ".mpx")
          ),
          uiOutput(ns("regUploadRequired"))
        ),
        actionButton(
          ns("regClearData"),
          label = "Clear Data",
          icon  = icon("trash"),
          class = "btn-danger btn-sm",
          style = "margin-top: 4px; margin-bottom: 8px;"
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
          "Polynomial Regression"                             = "POLYR",
          "Multiple Linear Regression"                        = "MLR",
          "Binary Logistic Regression"                        = "LOGR"
        ),
        selected = "SLR"
      ),
      conditionalPanel(
        condition = "input.dataInputMode == 'raw'",
        ns = ns,
        p(
          class = "text-muted",
          style = "font-size: 0.85em; margin-top: -8px;",
          tags$em("Note: Raw data entry is not available for Multiple Linear Regression and Binary Logistic Regression.")
        )
      ),

      # All four methodology sidebars are mounted once and kept in the DOM;
      # only the selected one is shown (same pattern as machineLearning.R).
      # Each module server is created once, the first time its methodology is
      # selected (see regressionAndCorrelationServer).
      tabsetPanel(
        id       = ns("regSidebarSwitch"),
        type     = "hidden",
        selected = "SLR",
        tabPanelBody("SLR",   SLRSidebarUI(ns("slr"))),
        tabPanelBody("POLYR", PolynomialRegressionSidebarUI(ns("polyr"))),
        tabPanelBody("MLR",   MLRSidebarUI(ns("mlr"))),
        tabPanelBody("LOGR",  LogisticRegressionSidebarUI(ns("logr")))
      )
    ),
    mainPanel(
      tabsetPanel(
        id       = ns("regMainPanelSwitch"),
        type     = "hidden",
        selected = "SLR",
        tabPanelBody("SLR",   SLRMainPanelUI(ns("slr"))),
        tabPanelBody("POLYR", PolynomialRegressionMainPanelUI(ns("polyr"))),
        tabPanelBody("MLR",   MLRMainPanelUI(ns("mlr"))),
        tabPanelBody("LOGR",  LogisticRegressionMainPanelUI(ns("logr")))
      ),
      hidden(div(
        id = ns("sharedDataPreview"),
        tags$h4("Uploaded Data",
                style = "color: #18536F; font-weight: bold; margin-bottom: 10px; margin-top: 10px;"),
        div(style = "overflow-x: auto;", DTOutput(ns("sharedDataTable"))),
        br()
      ))
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

    # Single source of truth for the raw-entry error messages, shared by the
    # per-field validator (shown below the entry boxes) and the main panel
    # warning shown when Calculate is pressed, so the two never diverge.
    rawXMessage <- function(val, info) {
      if (!nzchar(trimws(val))) return("Data entry is required.")
      if (length(createNumLst(val)) == 0)
        return("Data must be numeric values separated by commas or spaces (ie: 2,3,4 or 2 30 400).")
      if (length(createNumLst(val)) < 4)
        return("Sample data must include at least four numeric observations.")
      if (isTRUE(info$diff != 0))
        return("x and y must have the same number of observations.")
      if (isTRUE(info$xSD == 0))
        return("Explanatory variable has a standard deviation equal to zero (all values are identical). At least two distinct values are required.")
      NULL
    }
    rawYMessage <- function(val, info) {
      if (!nzchar(trimws(val))) return("Data entry is required.")
      if (length(createNumLst(val)) == 0)
        return("Data must be numeric values separated by commas or spaces (ie: 2,3,4 or 2 30 400).")
      if (length(createNumLst(val)) < 4)
        return("Sample data must include at least four numeric observations.")
      if (isTRUE(info$diff != 0))
        return("x and y must have the same number of observations.")
      if (isTRUE(info$ySD == 0))
        return("Response variable is constant. Correlation is undefined when a variable has a standard deviation equal to zero.")
      NULL
    }

    regraw_iv <- InputValidator$new()
    regraw_iv$add_rule("rawX", ~ tryCatch(rawXMessage(input$rawX, sampleInfoRaw()), error = function(e) NULL))
    regraw_iv$add_rule("rawY", ~ tryCatch(rawYMessage(input$rawY, sampleInfoRaw()), error = function(e) NULL))
    regraw_iv$condition(~ isTRUE(input$dataInputMode == "raw"))
    regraw_iv$enable()

    # Messages currently shown below the raw-entry boxes, exposed so the
    # methodology modules (SLR, POLYR) can mirror them in their own
    # "Calculate" warning banner instead of a generic hardcoded message.
    rawErrorMessages <- reactive({
      info <- sampleInfoRaw()
      list(
        x = rawXMessage(input$rawX, info),
        y = rawYMessage(input$rawY, info)
      )
    })

    # TRUE while a file is uploaded and not cleared. The file-content rules
    # below give no message about a file that was cleared (the file input
    # keeps its last value, see regDataCleared below).
    regHasFile <- function() !is.null(input$regUserData) && !isTRUE(regDataCleared())

    regupload_iv <- InputValidator$new()
    regupload_iv$add_rule("regUserData", sv_required())
    regupload_iv$add_rule("regUserData", ~ if (
      regHasFile() &&
      !(tolower(tools::file_ext(input$regUserData$name)) %in% UPLOAD_ACCEPTED_EXTENSIONS)
    ) "File format not accepted.")
    # Why the file reader rejected the file (e.g. too large, .rds is not a data
    # frame, the file cannot be read). The rules below stay silent for a file
    # that cannot be read.
    regupload_iv$add_rule("regUserData", ~ if (regHasFile()) uploadValidationMessage(reg_upload_data()))
    regupload_iv$add_rule("regUserData", ~ tryCatch(
      if (regHasFile() && isTRUE(nrow(reg_upload_data()) == 0)) "File is empty.",
      error = function(e) NULL
    ))
    regupload_iv$add_rule("regUserData", ~ tryCatch(
      if (regHasFile() && isTRUE(ncol(reg_upload_data()) < 2)) "Data must include at least two columns.",
      error = function(e) NULL
    ))
    regupload_iv$add_rule("regUserData", ~ tryCatch(
      if (regHasFile() && isTRUE(nrow(reg_upload_data()) < 4)) "Samples must include at least four numeric observations.",
      error = function(e) NULL
    ))
    regupload_iv$condition(~ isTRUE(input$dataInputMode == "upload"))
    regupload_iv$enable()

    # Shiny's fileInput never reports back to the server when it's reset —
    # input$regUserData keeps its last value, so reg_upload_data() would keep
    # returning the old file's data forever. This flag is the explicit signal
    # that a reset/clear happened; it's cleared the moment a genuinely new
    # file is chosen (which always fires a fresh input$regUserData event).
    regDataCleared <- reactiveVal(FALSE)

    # ---- Sheet picker -------------------------------------------------------
    output$regShowSheetPicker <- reactive({
      if (is.null(input$regUserData) || isTRUE(regDataCleared())) return(FALSE)
      tolower(tools::file_ext(input$regUserData$name)) %in% c("xls", "xlsx")
    })
    outputOptions(output, "regShowSheetPicker", suspendWhenHidden = FALSE)

    observeEvent(input$regUserData, {
      req(input$regUserData)
      regDataCleared(FALSE)
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
        sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
        validate(need(length(sheets) > 0, uploadReadErrorMsg(ext)))
        req(input$regSheet)
        req(input$regSheet %in% sheets)
      }
      # A file that cannot be read stops with the reader's short message.
      dat <- tryCatch(readUploadedDataFile(ext, path, input$regSheet),
                      uploadReadError = function(e) validate(conditionMessage(e)))
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
      } else if (isTRUE(regDataCleared())) {
        NULL
      } else {
        tryCatch(reg_upload_data(), error = function(e) NULL)
      }
    })

    # Reactive conveying the current input mode to children that need it (SLR, POLYR)
    input_mode <- reactive({ input$dataInputMode })

    # Fires on every raw-input keystroke; returns NULL in upload mode so children
    # can use ignoreNULL = TRUE to avoid clearing results when not in raw mode.
    raw_input_trigger <- reactive({
      req(input$dataInputMode == "raw")
      list(x = input$rawX, y = input$rawY)
    })

    # ---- Data status label (upload mode only) --------------------------------
    output$regDataStatus <- renderUI({
      req(input$dataInputMode == "upload")
      dat <- reg_data()
      if (is.null(dat)) return(NULL)
      div(
        class = "alert alert-success",
        style = "padding: 5px 10px; font-size: 12px; margin-top: 2px; margin-bottom: 10px;",
        icon("circle-check"),
        HTML(paste0(" <strong>File loaded:</strong> ", input$regUserData$name,
                    " (", nrow(dat), " rows × ", ncol(dat), " columns)"))
      )
    })

    # Clears the uploaded file: resets the file input, tells reg_data() the file
    # is gone (see regDataCleared above) and empties the sheet picker.
    # shinyjs::reset() namespaces its id with the current reactive domain, so it
    # is run with this module's session: reset_upload() below is called from
    # the children's Reset observers, whose domain is the child's session.
    clear_upload <- function() {
      withReactiveDomain(session, shinyjs::reset("regUserData"))
      regDataCleared(TRUE)
      updateSelectizeInput(session, "regSheet", choices = character(0), selected = "")
    }

    # Clears the parent's upload (file input, reg_data(), sheet picker) — passed
    # to children so their Reset button can clear it
    reset_upload <- function() clear_upload()

    # Restores raw data text boxes to their hardcoded defaults
    reset_raw_data <- function() {
      if (isTRUE(input$multiple == "POLYR")) {
        updateTextAreaInput(session, "rawX",
          value = "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10")
        updateTextAreaInput(session, "rawY",
          value = "4.997, 6.165, 6.95, 8.218, 9.405, 10.404, 10.425, 10.44, 9.393, 7.854, 5.168")
      } else {
        updateTextAreaInput(session, "rawX",
          value = "4.51, 3.58, 4.31, 5.06, 5.64, 4.99, 5.29, 5.83, 4.70, 5.61, 4.90, 4.20")
        updateTextAreaInput(session, "rawY",
          value = "2.48, 2.26, 2.47, 2.77, 2.99, 3.05, 3.18, 3.46, 3.03, 3.26, 2.67, 2.53")
      }
    }

    # ---- Upload error state (red border + Required text on the file input) ----
    upload_error <- reactiveVal(FALSE)

    output$regUploadRequired <- renderUI({
      if (!upload_error()) return(NULL)
      tags$style(HTML(sprintf(
        "#%s .input-group { outline: 2px solid #dc3545 !important; border-radius: 4px; }",
        session$ns("regFileInputWrapper")
      )))
    })

    # Clear the upload error whenever a file loads successfully
    observe({
      dat <- tryCatch(reg_upload_data(), error = function(e) NULL)
      if (!is.null(dat)) upload_error(FALSE)
    })

    clear_trigger <- reactiveVal(0)

    observeEvent(input$regClearData, {
      clear_upload()
      upload_error(FALSE)
      clear_trigger(clear_trigger() + 1)
    })

    # ---- Update methodology choices based on data input mode ----------------
    # Switching between Raw/Upload otherwise leaves the previous upload's file,
    # sheet selection, and variable pickers silently in place; clear them so
    # re-entering Upload mode always starts from a fresh, empty picker.
    observeEvent(input$dataInputMode, {
      clear_upload()
      upload_error(FALSE)
      clear_trigger(clear_trigger() + 1)
    }, ignoreInit = TRUE)

    observeEvent(input$multiple, { upload_error(FALSE) }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Raw data entry is only available for SLR and POLYR: grey out MLR and LOGR.
    setRawOnlyMethods <- function(disabled) {
      shinyjs::runjs(sprintf(
        "['MLR','LOGR'].forEach(function(v) {
          var el = document.querySelector('#%s input[value=\"' + v + '\"]');
          var lbl = el ? el.closest('label') : null;
          if (!el || !lbl) return;
          el.disabled = %s; lbl.style.opacity = '%s'; lbl.style.pointerEvents = '%s';
        });",
        session$ns("multiple"),
        if (disabled) "true" else "false",
        if (disabled) "0.4" else "",
        if (disabled) "none" else ""
      ))
    }

    modeObserverRan <- FALSE
    observeEvent(input$dataInputMode, {
      current <- isolate(input$multiple)
      if (input$dataInputMode == "raw") {
        if (!(current %in% c("SLR", "POLYR")))
          updateRadioButtons(session, "multiple", selected = "SLR")
        setRawOnlyMethods(TRUE)
      } else if (modeObserverRan) {
        # The radios start enabled, so the first (startup) run in upload mode
        # has nothing to undo.
        setRawOnlyMethods(FALSE)
      }
      modeObserverRan <<- TRUE
    })

    hide_shared <- reactiveVal(FALSE)

    observeEvent(input$multiple, {
      if (input$dataInputMode == "raw") {
        if (input$multiple == "POLYR") {
          updateTextAreaInput(session, "rawY", value = "4.997, 6.165, 6.95, 8.218, 9.405, 10.404, 10.425, 10.44, 9.393, 7.854, 5.168")
          updateTextAreaInput(session, "rawX", value = "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10")
        } else if (input$multiple == "SLR") {
          updateTextAreaInput(session, "rawY", value = "2.48, 2.26, 2.47, 2.77, 2.99, 3.05, 3.18, 3.46, 3.03, 3.26, 2.67, 2.53")
          updateTextAreaInput(session, "rawX", value = "4.51, 3.58, 4.31, 5.06, 5.64, 4.99, 5.29, 5.83, 4.70, 5.61, 4.90, 4.20")
        }
      }
    }, ignoreInit = TRUE)

    # ---- Methodology modules: one server per method, created once ----------
    # Each child server is created the first time its methodology is selected
    # and then kept for the session, with a stable id ("slr", "polyr", "mlr",
    # "logr"); switching methodology only swaps the visible pane of the hidden
    # tabsetPanels in the UI. (Previously a new server instance was spawned on
    # every switch and the old ones were never destroyed.)
    #
    # A child only reacts while its methodology is selected. The shared
    # reactives are handed to it through gates (methodGate) that stop with a
    # silent req() while it is not selected, so an unselected child does no
    # work and sends no UI messages; its writes to the shared reactiveVals are
    # ignored too (methodGateRV). When a child is selected again, the gates
    # re-fire its observers on the shared data, input mode and clear trigger,
    # so it starts again from a clean state with the current data, as a newly
    # spawned instance did before.
    regMethods <- c("SLR", "POLYR", "MLR", "LOGR")
    methodActive  <- lapply(setNames(regMethods, regMethods), function(m) reactiveVal(FALSE))
    methodStarted <- setNames(rep(FALSE, length(regMethods)), regMethods)
    # Results navbar of each child; its first tab is "data_tab".
    methodNavbarIds <- c(SLR = "slr-slrNavbarPage", POLYR = "polyr-polyNavbarPage",
                         MLR = "mlr-mainPanel",     LOGR  = "logr-mainPanel")

    methodGate <- function(active, r) reactive({
      req(active())
      r()
    })
    methodGateRV <- function(active, rv) function(value) {
      if (missing(value)) return(rv())
      if (isTRUE(isolate(active()))) rv(value)
      invisible(NULL)
    }

    startMethodServer <- function(m) {
      active    <- methodActive[[m]]
      is_active <- function() active()
      gated_reg_data      <- methodGate(active, reg_data)
      gated_clear_trigger <- methodGate(active, clear_trigger)
      gated_upload_error  <- methodGateRV(active, upload_error)
      gated_hide_shared   <- methodGateRV(active, hide_shared)
      switch(m,
        SLR = SLRServer("slr", gated_reg_data, methodGate(active, input_mode), reset_upload,
          gated_upload_error, gated_clear_trigger,
          hide_shared = gated_hide_shared, reset_raw_data = reset_raw_data,
          raw_error_msgs = rawErrorMessages, raw_input_trigger = methodGate(active, raw_input_trigger),
          is_active = is_active),
        POLYR = PolynomialRegressionServer("polyr", gated_reg_data, methodGate(active, input_mode), reset_upload,
          gated_upload_error, gated_clear_trigger,
          hide_shared = gated_hide_shared, reset_raw_data = reset_raw_data,
          raw_error_msgs = rawErrorMessages, raw_input_trigger = methodGate(active, raw_input_trigger),
          is_active = is_active),
        MLR = MLRServer("mlr", gated_reg_data, reset_upload, gated_upload_error, gated_clear_trigger,
          hide_shared = gated_hide_shared, is_active = is_active),
        LOGR = LogisticRegressionServer("logr", gated_reg_data, reset_upload, gated_upload_error, gated_clear_trigger,
          hide_shared = gated_hide_shared, is_active = is_active)
      )
    }

    observeEvent(input$multiple, {
      m <- input$multiple
      req(m %in% regMethods)
      updateTabsetPanel(session, "regSidebarSwitch",   selected = m)
      updateTabsetPanel(session, "regMainPanelSwitch", selected = m)
      for (k in regMethods) methodActive[[k]](identical(k, m))
      if (!methodStarted[[m]]) {
        methodStarted[[m]] <<- TRUE
        startMethodServer(m)
      } else {
        # Selected again: put the child's results navbar back on its first tab,
        # as in a newly created UI. Otherwise a tab left selected from an earlier
        # Calculate stays "active" while the child's reset hides it, and the
        # child's next updateNavbarPage() to that same tab shows an empty pane.
        updateNavbarPage(session, methodNavbarIds[[m]], selected = "data_tab")
      }
    })

    # ---- Shared data preview (shown immediately on upload, above child UI) ----
    output$sharedDataTable <- renderDT({
      req(input$dataInputMode == "upload", !is.null(reg_data()))
      datatable(
        reg_data(),
        options = list(
          pageLength   = 25,
          lengthMenu   = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          scrollX      = TRUE
        )
      )
    })
    outputOptions(output, "sharedDataTable", suspendWhenHidden = FALSE)

    observeEvent(list(reg_data(), input$dataInputMode, input$multiple), {
      hide_shared(FALSE)
    }, ignoreNULL = FALSE, priority = 10)

    observe({
      dat_present <- !is.null(reg_data()) && isTRUE(input$dataInputMode == "upload")
      if (dat_present && !isTRUE(hide_shared())) {
        shinyjs::show("sharedDataPreview")
      } else {
        shinyjs::hide("sharedDataPreview")
      }
    })

  })
}
