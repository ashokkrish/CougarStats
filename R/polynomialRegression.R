# R/polynomialRegression.R

# =========================================================================== #
# ---- UI Components -------------------------------------------------------- #
# =========================================================================== #

PolynomialRegressionSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(withMathJax(div(
    id = ns("polyInputPanel"),

    radioButtons(
      inputId      = ns("polyDataInput"),
      label        = strong("Data"),
      choiceValues = list("Enter Raw Data", "Upload Data"),
      choiceNames  = list("Enter Raw Data", "Upload Data"),
      selected     = "Enter Raw Data",
      inline       = TRUE
    ),

    # ---- Raw Data Entry ---------------------------------------------------
    conditionalPanel(
      ns        = ns,
      condition = "input.polyDataInput == 'Enter Raw Data'",

      textAreaInput(
        inputId     = ns("polyY"),
        label       = strong("Response Variable (\\( y \\))"),
        value       = "4.997, 6.165, 6.95, 8.218, 9.405, 10.404, 10.425, 10.44, 9.393, 7.854, 5.168",
        placeholder = "Enter numeric values separated by commas or spaces (e.g. 1,2,3 or 1 2 3)",
        rows        = 3
      ),

      textAreaInput(
        inputId     = ns("polyX"),
        label       = strong("Explanatory Variable (\\( x \\))"),
        value       = "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10",
        placeholder = "Enter numeric values separated by commas or spaces (e.g. 1,2,3 or 1 2 3)",
        rows        = 3
      )
    ),

    # ---- File Upload ------------------------------------------------------
    conditionalPanel(
      ns        = ns,
      condition = "input.polyDataInput == 'Upload Data'",

      HTML(uploadDataDisclaimer),

      fileInput(
        inputId = ns("polyUserData"),
        label   = strong("Upload your data (.csv, .xls, .xlsx, .txt, .sas7bdat, .sav, .dta, .rds, .mtp, .mwx, .mpx)"),
        accept  = c("text/csv",
                    "text/comma-separated-values",
                    "text/tab-separated-values",
                    "text/plain",
                    ".csv", ".txt", ".xls", ".xlsx",
                    ".sas7bdat", ".sav", ".dta", ".rds",
                    ".mtp", ".mwx", ".mpx")
      ),

      conditionalPanel(
        ns        = ns,
        condition = "output.polyShowSheetPicker == true",
        selectizeInput(
          inputId  = ns("polySheet"),
          label    = strong("Choose a Sheet"),
          choices  = c(""),
          multiple = FALSE,
          options  = list(
            placeholder  = "Select a sheet",
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      ),

      selectizeInput(
        inputId = ns("polyResponse"),
        label   = strong("Choose the Response Variable (\\( y \\))"),
        choices = c(""),
        options = list(
          placeholder  = "Select a variable",
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),

      selectizeInput(
        inputId = ns("polyExplanatory"),
        label   = strong("Choose the Explanatory Variable (\\( x \\))"),
        choices = c(""),
        options = list(
          placeholder  = "Select a variable",
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),

    br(),
    p(strong("Model Options")),
    hr(),

    numericInput(
      inputId = ns("polyDegree"),
      label   = strong("Polynomial Degree (must be ≥ 2)"),
      value   = 2,
      min     = 2,
      step    = 1
    ),

    br(),

    actionButton(
      inputId = ns("goPolynomial"),
      label   = "Calculate",
      class   = "act-btn"
    ),

    actionButton(
      inputId = ns("resetPolynomial"),
      label   = "Reset Values",
      class   = "act-btn"
    )
  )))
}


PolynomialRegressionMainPanelUI <- function(id) {
  ns <- NS(id)

  tagList(withMathJax(
    useShinyjs(),

    tags$script(HTML("
      function copyPlotToClipboard(plotId) {
        var plotDiv = document.getElementById(plotId);
        if (!plotDiv) return;
        var btn = document.querySelector('[data-copy-plot=\"' + plotId + '\"]');
        Plotly.toImage(plotDiv, {format: 'png', width: plotDiv.offsetWidth, height: plotDiv.offsetHeight})
          .then(function(dataUrl) { return fetch(dataUrl); })
          .then(function(res) { return res.blob(); })
          .then(function(blob) {
            return navigator.clipboard.write([new ClipboardItem({'image/png': blob})]);
          })
          .then(function() {
            if (btn) {
              var orig = btn.innerHTML;
              btn.innerHTML = '<i class=\"fa fa-check\"></i> Copied!';
              btn.disabled = true;
              setTimeout(function() { btn.innerHTML = orig; btn.disabled = false; }, 2000);
            }
          })
          .catch(function(err) {
            alert('Could not copy to clipboard. Your browser may not support this feature, or the page must be served over HTTPS.');
          });
      }
    ")),

    hidden(div(
      id = ns("polyResultsPanel"),
      uiOutput(ns("polyMissingRowsWarning")),

      navbarPage(
        title = NULL,
        id    = ns("polyNavbarPage"),
        theme = bs_theme(version = 4),

        # ---- Model Tab ----------------------------------------------------
        tabPanel(
          title = "Model",
          value = "Model",

          titlePanel("Estimated equation of the polynomial regression model"),
          br(),
          uiOutput(ns("polyModelEquation")),
          br()
        ),

        # ---- Scatterplot Tab ----------------------------------------------
        tabPanel(
          title = "Scatterplot",
          value = "Scatterplot",

          titlePanel("Scatterplot"),
          br(),

          # Local degree control — independent from the model degree
          fluidRow(
            column(4,
              numericInput(
                inputId = ns("polyScatterDegree"),
                label   = strong("Degree of fitted curve"),
                value   = 2,
                min     = 2,
                step    = 1
              )
            )
          ),

          uiOutput(ns("polyScatterIntervalWarning")),

          plotOptionsMenuUI(
            id          = ns("polyScatter"),
            plotType    = "Scatterplot",
            title       = "Scatterplot",
            xlab        = "x",
            ylab        = "y",
            dim         = "in px",
            includeFlip = FALSE
          ),

          plotlyOutput(ns("polyScatterplot"),
                       height = "700px",
                       width  = "100%"),

          tags$button(
            class            = "btn btn-default copy-plot-btn",
            `data-copy-plot` = ns("polyScatterplot"),
            onclick          = paste0("copyPlotToClipboard('", ns("polyScatterplot"), "')"),
            tags$i(class = "fa fa-clipboard"),
            "Copy to Clipboard"
          ),

          br()
        ),

        # ---- Inference Tab ------------------------------------------------
        tabPanel(
          title = "Inference",
          value = "Inference",

          uiOutput(ns("polyInference"))
        ),

        # ---- Diagnostic Plots Tab -----------------------------------------
        tabPanel(
          title = "Diagnostic Plots",
          value = "Diagnostic Plots",
          fluidPage(
            uiOutput(ns("polyDiagnosticPlotsWarning")),
            plotOutput(ns("polyDiagPlot1")),
            plotOutput(ns("polyDiagPlot2")),
            plotOutput(ns("polyDiagPlot3")),
            plotOutput(ns("polyDiagPlot4")),
            plotOutput(ns("polyDiagPlot5"))
          )
        ),

        # ---- Uploaded Data Tab --------------------------------------------
        tabPanel(
          title = "Uploaded Data",
          value = "Uploaded Data",
          div(
            DTOutput(ns("polyViewUploadTab")),
            style = "width: 75%"
          ),
          br(),
          br()
        )

      ) # navbarPage
    )), # polyResultsPanel

    # Uploaded data preview panel — only visible in Upload Data mode
    hidden(div(
      id = ns("polyUploadedDataPanel"),
      tags$h4(
        "Uploaded Data",
        style = "color: #18536F; font-weight: bold; margin-bottom: 15px; margin-top: 10px;"
      ),
      uiOutput(ns("polyUploadedDataContent")),
      br()
    ))
  ))
}


# =========================================================================== #
# ---- Server --------------------------------------------------------------- #
# =========================================================================== #

PolynomialRegressionServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ---- Reactive values --------------------------------------------------
    fileState    <- reactiveValues(status = NULL)
    nDroppedRows <- reactiveVal(0)

    # Store datx/daty so the scatterplot can rerender on degree change
    # without needing to re-click Calculate
    storedDatx <- reactiveVal(NULL)
    storedDaty <- reactiveVal(NULL)

    # ---- Upload data reactive ---------------------------------------------
    polyUploadData <- eventReactive(list(input$polyUserData, input$polySheet), {
      req(input$polyUserData)
      ext  <- tolower(tools::file_ext(input$polyUserData$name))
      path <- input$polyUserData$datapath

      if (ext %in% c("xls", "xlsx")) {
        req(input$polySheet)
        req(input$polySheet %in% readxl::excel_sheets(path))
      }

      dat <- readUploadedDataFile(ext, path, input$polySheet)
      dat <- dat[, colSums(!is.na(dat)) > 0, drop = FALSE]
      dat <- dat[rowSums(!is.na(dat)) > 0, , drop = FALSE]
      dat
    })

    # ---- Input Validators -------------------------------------------------
    poly_iv       <- InputValidator$new()
    polyraw_iv    <- InputValidator$new()
    polyupload_iv <- InputValidator$new()
    polyupvars_iv <- InputValidator$new()

    poly_iv$add_rule("polyDegree", sv_required())
    poly_iv$add_rule("polyDegree", ~ {
      d <- input$polyDegree
      if (!is.na(d) && (d != floor(d) || d < 2))
        "Polynomial degree must be a whole number ≥ 2."
    })

    polyraw_iv$add_rule("polyY", sv_required())
    polyraw_iv$add_rule("polyY", ~ {
      if (nzchar(trimws(input$polyY)) && length(createNumLst(input$polyY)) == 0)
        "Data must be numeric values separated by commas or spaces (e.g. 2,3,4 or 2 3 4)."
    })
    polyraw_iv$add_rule("polyY", ~ {
      yvals <- createNumLst(input$polyY)
      xvals <- createNumLst(input$polyX)
      if (length(yvals) > 0 && length(xvals) > 0 && length(yvals) != length(xvals))
        "x and y must have the same number of observations."
    })

    polyraw_iv$add_rule("polyX", sv_required())
    polyraw_iv$add_rule("polyX", ~ {
      if (nzchar(trimws(input$polyX)) && length(createNumLst(input$polyX)) == 0)
        "Data must be numeric values separated by commas or spaces (e.g. 2,3,4 or 2 3 4)."
    })
    polyraw_iv$add_rule("polyX", ~ {
      xvals <- createNumLst(input$polyX)
      n     <- length(xvals)
      d     <- input$polyDegree
      if (n > 0 && !is.na(d)) {
        if (d >= n)
          paste0("Degree must be less than n (", n, "). Maximum degree for this data is ", n - 1, ".")
      }
    })
    polyraw_iv$add_rule("polyX", ~ {
      yvals <- createNumLst(input$polyY)
      xvals <- createNumLst(input$polyX)
      if (length(xvals) > 0 && length(yvals) > 0 && length(xvals) != length(yvals))
        "x and y must have the same number of observations."
    })

    polyupload_iv$add_rule("polyUserData", sv_required())
    polyupload_iv$add_rule("polyUserData", ~ {
      if (is.null(fileState$status) || fileState$status == "reset") "Required"
    })
    polyupload_iv$add_rule("polyUserData", ~ {
      if (!(tolower(tools::file_ext(input$polyUserData$name)) %in% UPLOAD_ACCEPTED_EXTENSIONS))
        "File format not accepted."
    })
    polyupload_iv$add_rule("polyUserData", ~ tryCatch(
      if (isTRUE(nrow(polyUploadData()) == 0)) "File is empty.",
      error = function(e) NULL
    ))
    polyupload_iv$add_rule("polyUserData", ~ tryCatch(
      if (isTRUE(ncol(polyUploadData()) < 2))
        "Data must include one response and at least one explanatory variable.",
      error = function(e) NULL
    ))

    polyupvars_iv$add_rule("polyResponse",    sv_required())
    polyupvars_iv$add_rule("polyExplanatory", sv_required())
    polyupvars_iv$add_rule("polyExplanatory", ~ tryCatch({
      raw  <- suppressWarnings(as.numeric(as.data.frame(polyUploadData())[, input$polyExplanatory]))
      n    <- length(na.omit(raw))
      d    <- input$polyDegree
      if (!is.na(d) && n > 0 && d >= n)
        paste0("Degree must be less than n (", n, "). Maximum degree for this data is ", n - 1, ".")
    }, error = function(e) NULL))

    polyraw_iv$condition(~ isTRUE(input$polyDataInput == "Enter Raw Data"))
    polyupload_iv$condition(~ isTRUE(input$polyDataInput == "Upload Data"))
    polyupvars_iv$condition(~ isTRUE(input$polyDataInput == "Upload Data" && polyupload_iv$is_valid()))

    poly_iv$add_validator(polyraw_iv)
    poly_iv$add_validator(polyupload_iv)
    poly_iv$add_validator(polyupvars_iv)

    poly_iv$enable()
    polyraw_iv$enable()
    polyupload_iv$enable()
    polyupvars_iv$enable()

    # ---- Reset results when data input mode changes -----------------------
    observeEvent(input$polyDataInput, {
      hide("polyResultsPanel")
      hideTab(inputId = "polyNavbarPage", target = "Uploaded Data")
      hideTab(inputId = "polyNavbarPage", target = "Inference")
      hideTab(inputId = "polyNavbarPage", target = "Diagnostic Plots")
      storedDatx(NULL)
      storedDaty(NULL)
      nDroppedRows(0)

      if (input$polyDataInput == "Upload Data" &&
          !is.null(fileState$status) &&
          fileState$status == "uploaded") {
        show("polyUploadedDataPanel")
      } else {
        hide("polyUploadedDataPanel")
      }
    }, ignoreInit = TRUE)

    # ---- Reset results when raw inputs change -----------------------------
    observeEvent(list(input$polyX, input$polyY, input$polyDegree), {
      req(input$polyDataInput == "Enter Raw Data")
      hide("polyResultsPanel")
      hideTab(inputId = "polyNavbarPage", target = "Inference")
      hideTab(inputId = "polyNavbarPage", target = "Diagnostic Plots")
      storedDatx(NULL)
      storedDaty(NULL)
    }, ignoreInit = TRUE)

    # ---- Plot options module ----------------------------------------------
    plotOptionsMenuServer("polyScatter")

    # ---- Sheet picker visibility ------------------------------------------
    output$polyShowSheetPicker <- reactive({
      if (is.null(input$polyUserData)) return(FALSE)
      tolower(tools::file_ext(input$polyUserData$name)) %in% c("xls", "xlsx")
    })
    outputOptions(output, "polyShowSheetPicker", suspendWhenHidden = FALSE)

    observeEvent(input$polyUserData, {
      req(input$polyUserData)
      ext <- tolower(tools::file_ext(input$polyUserData$name))
      if (ext %in% c("xls", "xlsx")) {
        sheets <- tryCatch(readxl::excel_sheets(input$polyUserData$datapath),
                           error = function(e) character(0))
        freezeReactiveValue(input, "polySheet")
        updateSelectizeInput(session, "polySheet",
                             choices  = sheets,
                             selected = if (length(sheets)) sheets[1] else "")
      } else {
        updateSelectizeInput(session, "polySheet", choices = character(0), selected = "")
      }
    }, priority = 50)

    observeEvent(list(input$polyUserData, input$polySheet), {
      req(input$polyUserData)
      fileState$status <- "uploaded"

      ext <- tolower(tools::file_ext(input$polyUserData$name))
      if (ext %in% c("xls", "xlsx") && (is.null(input$polySheet) || input$polySheet == "")) return()

      req(polyUploadData())
      cols <- colnames(polyUploadData())
      updateSelectizeInput(session, "polyResponse",    choices = cols)
      updateSelectizeInput(session, "polyExplanatory", choices = cols)
      show("polyUploadedDataPanel")
    })

    # ---- Uploaded data preview -------------------------------------------
    output$polyUploadedDataContent <- renderUI({
      if (is.null(input$polyUserData) ||
          is.null(fileState$status)   ||
          fileState$status == "reset") {
        div(
          class = "alert alert-info",
          style = "margin-top: 15px;",
          tags$b("No data uploaded. "),
          "Please upload a file using the sidebar to view your data here."
        )
      } else {
        DTOutput(session$ns("polyViewUpload"))
      }
    })

    output$polyViewUpload <- renderDT({
      req(input$polyUserData)
      dat <- polyUploadData()
      datatable(dat, options = list(
        pageLength = 25,
        lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
        scrollX    = TRUE
      ))
    })
    outputOptions(output, "polyViewUpload", suspendWhenHidden = FALSE)

    output$polyViewUploadTab <- renderDT({
      req(input$polyUserData)
      dat <- polyUploadData()
      datatable(dat, options = list(
        pageLength = 25,
        lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
        scrollX    = TRUE
      ))
    })

    # ---- Scatterplot (reactive to local degree) ---------------------------
    # Fits a fresh model whenever the scatter degree input changes, using
    # the stored datx/daty from the last Calculate press.
    scatterModel <- reactive({
      req(storedDatx(), storedDaty())
      datx <- storedDatx()
      daty <- storedDaty()
      n    <- length(datx)
      d    <- input$polyScatterDegree
      req(!is.null(d), !is.na(d), d >= 2, d < n)
      lm(daty ~ poly(datx, as.integer(d), raw = TRUE))
    })

    output$polyScatterIntervalWarning <- renderUI({
      req(storedDatx())
      d <- input$polyScatterDegree
      n <- length(storedDatx())

      if (is.null(d) || is.na(d) || d < 2) {
        div(
          class = "alert alert-danger",
          style = "margin-top: 10px;",
          tags$b("Invalid degree: "),
          "Degree must be a whole number ≥ 2."
        )
      } else if (d >= n) {
        div(
          class = "alert alert-danger",
          style = "margin-top: 10px;",
          tags$b("Invalid degree: "),
          paste0(
            "Degree must be less than n (", n, "). ",
            "Maximum degree for this data is ", n - 1, "."
          )
        )
      }
    })

    output$polyScatterplot <- renderPlotly({
      req(scatterModel(), storedDatx(), storedDaty())
      df <- data.frame(x = storedDatx(), y = storedDaty())
      RenderScatterplot(
        df,
        scatterModel(),
        input[["polyScatter-Title"]],
        input[["polyScatter-Xlab"]],
        input[["polyScatter-Ylab"]],
        input[["polyScatter-Colour"]],
        input[["polyScatter-PointsColour"]],
        input[["polyScatter-LineWidth"]],
        input[["polyScatter-PointSize"]],
        input[["polyScatter-Gridlines"]],
        isTRUE(input[["polyScatter-confidenceInterval"]]) && df.residual(scatterModel()) > 0,
        isTRUE(input[["polyScatter-predictionInterval"]]) && df.residual(scatterModel()) > 0,
        input[["polyScatter-showRegressionLine"]]
      ) %>%
        style(name = "Data Points — Polynomial Regression Curve", traces = 2)
    })

    # ---- Calculate button -------------------------------------------------
    observeEvent(input$goPolynomial, {
      toggle("polyResultsPanel", condition = poly_iv$is_valid())
      if (!poly_iv$is_valid()) return()

      hide("polyUploadedDataPanel")

      showTab(inputId = "polyNavbarPage", target = "Inference")
      showTab(inputId = "polyNavbarPage", target = "Diagnostic Plots")

      if (input$polyDataInput == "Upload Data") {
        showTab(inputId = "polyNavbarPage", target = "Uploaded Data")
      } else {
        hideTab(inputId = "polyNavbarPage", target = "Uploaded Data")
      }

      # -- Extract data ------------------------------------------------------
      degree <- as.integer(input$polyDegree)

      if (input$polyDataInput == "Upload Data") {
        req(input$polyExplanatory %in% colnames(polyUploadData()))
        req(input$polyResponse    %in% colnames(polyUploadData()))
        raw_x        <- suppressWarnings(as.numeric(as.data.frame(polyUploadData())[, input$polyExplanatory]))
        raw_y        <- suppressWarnings(as.numeric(as.data.frame(polyUploadData())[, input$polyResponse]))
        complete_idx <- !is.na(raw_x) & !is.na(raw_y)
        datx         <- raw_x[complete_idx]
        daty         <- raw_y[complete_idx]
        nDroppedRows(sum(!complete_idx))
      } else {
        datx <- createNumLst(input$polyX)
        daty <- createNumLst(input$polyY)
        nDroppedRows(0)
      }

      # Store for scatterplot reactive use
      storedDatx(datx)
      storedDaty(daty)

      # Sync scatter degree to model degree on each Calculate press
      updateNumericInput(session, "polyScatterDegree", value = degree)

      # -- Fit model ---------------------------------------------------------
      model <- lm(daty ~ poly(datx, degree, raw = TRUE))
      coefs <- coef(model)

      # -- Missing rows warning ----------------------------------------------
      output$polyMissingRowsWarning <- renderUI({
        n <- nDroppedRows()
        if (n > 0) {
          div(
            class = "alert alert-warning",
            role  = "alert",
            style = "margin-top: 10px;",
            tags$b("⚠️ Missing Data Detected: "),
            sprintf("%d row%s with missing values removed before analysis.",
                    n, if (n == 1) "" else "s")
          )
        }
      })

      # -- Model tab ---------------------------------------------------------
      output$polyModelEquation <- renderUI({

        fmt_coef <- function(x) fmt_sci_latex(x, 4)

        sym_terms <- paste0(
          "\\hat{\\beta}_{0}",
          paste(sapply(seq_len(degree), function(k) {
            if (k == 1) sprintf(" + \\hat{\\beta}_{1} x")
            else        sprintf(" + \\hat{\\beta}_{%d} x^{%d}", k, k)
          }), collapse = "")
        )

        b0        <- coefs[1]
        num_terms <- fmt_coef(b0)
        for (k in seq_len(degree)) {
          bk  <- coefs[k + 1]
          sgn <- if (bk >= 0) " + " else " - "
          if (k == 1) {
            num_terms <- paste0(num_terms, sgn, fmt_coef(abs(bk)), " x")
          } else {
            num_terms <- paste0(num_terms, sgn, fmt_coef(abs(bk)), " x^{", k, "}")
          }
        }

        withMathJax(
          p(sprintf(
            "The estimated equation of the degree-%d polynomial regression model is",
            degree
          )),
          p(sprintf("\\( \\qquad \\hat{y} = %s \\)", sym_terms)),
          br(),
          p("The fitted model with estimated coefficients is"),
          p(sprintf("\\( \\qquad \\hat{y} = %s \\)", num_terms)),
          br(),
          p(tags$b("Interpretation:")),
          p(HTML(paste0(
            "The degree-", degree, " polynomial model was fitted to the data. ",
            "\\( \\hat{\\beta}_0 = ", fmt_coef(b0), " \\) is the estimated value of \\( y \\) when \\( x = 0 \\). ",
            "The remaining coefficients capture the curvature of the relationship between \\( x \\) and \\( y \\)."
          )))
        )
      })

    }) # goPolynomial

    # ---- Inference tab ----------------------------------------------------

    # Helper: rebuild the poly model from stored data + current degree
    polyModel <- reactive({
      req(storedDatx(), storedDaty())
      degree <- as.integer(input$polyDegree)
      lm(storedDaty() ~ poly(storedDatx(), degree, raw = TRUE))
    })

    output$polyInference <- renderUI({
      req(storedDatx(), storedDaty())

      fluidPage(
        tags$style(HTML("
          .poly-inference-tabs .nav-tabs {
            border-bottom: none;
            background-color: #f8f9fa;
            display: flex;
            padding: 0;
            margin-bottom: 16px;
          }
          .poly-inference-tabs .nav-tabs > li > a {
            color: #18536F;
            font-weight: bold;
            font-size: 15px;
            border: none !important;
            border-radius: 0 !important;
            padding: 10px 24px;
            background-color: #f8f9fa !important;
          }
          .poly-inference-tabs .nav-tabs > li.active > a,
          .poly-inference-tabs .nav-tabs > li.active > a:focus,
          .poly-inference-tabs .nav-tabs > li.active > a:hover,
          .poly-inference-tabs .nav-tabs > li > a.active,
          .poly-inference-tabs .nav-tabs > li > a.active:focus,
          .poly-inference-tabs .nav-tabs > li > a.active:hover {
            background-color: #18536F !important;
            color: white !important;
            border: none !important;
            border-radius: 0 !important;
            font-weight: bold !important;
          }
          .poly-inference-tabs .nav-tabs > li > a:hover {
            background-color: #d0dce8 !important;
            color: #1a3a5c !important;
          }
        ")),
        div(
          class = "poly-inference-tabs",
          tabsetPanel(
            tabPanel(
              title = "Parameter Estimates",
              br(),
              fluidRow(uiOutput(ns("polyParamEstimates")))
            ),
            tabPanel(
              title = "ANOVA",
              br(),
              fluidRow(uiOutput(ns("polyAnovaHypotheses"))),
              br(),
              fluidRow(tableOutput(ns("polyAnovaTable"))),
              br(),
              fluidRow(
                column(12,
                  p(strong("F Distribution")),
                  p("The shaded region represents the rejection region at α = 0.05. The dashed red line is the observed F statistic and the dashed blue line is the critical value."),
                  plotOutput(ns("polyAnovaFPlot"), height = "350px")
                )
              ),
              br(),
              fluidRow(uiOutput(ns("polyAnovaPValue"))),
              br(),
              fluidRow(uiOutput(ns("polyRSquared"))),
              br()
            ),
            tabPanel(
              title = "INE",
              br(),
              uiOutput(ns("polyIneAssumptions"))
            )
          )
        )
      )
    })

    # Parameter Estimates
    output$polyParamCoefTable <- renderTable({
      model  <- polyModel()
      degree <- as.integer(input$polyDegree)

      coefs <- as.data.frame(summary(model)$coefficients)
      coefs <- tibble::rownames_to_column(coefs, "Term")
      term_labels <- c("Intercept", paste0("x^", seq_len(degree)))
      if (nrow(coefs) == length(term_labels)) coefs$Term <- term_labels
      names(coefs)[names(coefs) == "Pr(>|t|)"] <- "P-value"

      ci <- as.data.frame(confint(model))
      colnames(ci) <- c("Lower 95% CI", "Upper 95% CI")
      ci <- tibble::rownames_to_column(ci, "Term")
      if (nrow(ci) == length(term_labels)) ci$Term <- term_labels

      tbl <- dplyr::left_join(coefs, ci, by = "Term")
      tibble::column_to_rownames(tbl, var = "Term")
    }, rownames = TRUE, na = "", striped = TRUE, align = "c", digits = 3)

    output$polyParamEstimates <- renderUI({
      req(storedDatx(), storedDaty())
      column(12,
        p(strong("Coefficients and Confidence Intervals")),
        tableOutput(ns("polyParamCoefTable"))
      )
    })

    # ANOVA hypotheses
    output$polyAnovaHypotheses <- renderUI({
      model  <- polyModel()
      k      <- model$rank - 1
      n      <- length(storedDatx())
      withMathJax(
        p(strong("Analysis of Variance (ANOVA)")),
        p(
          r"{\( H_0: \beta_1 = \beta_2 = \cdots = \beta_k = 0\)}",
          br(),
          r"{\( H_a: \) At least one \(\beta_j\ne 0\), where \(j = 1, \cdots, k\).}"
        ),
        p(r"{\( \alpha = 0.05\ \)}"),
        p(
          sprintf(r"{\( n = %i \)}", n),
          br(),
          sprintf(r"{\( k = %i \)}", k)
        ),
        p(r"[where \(n\) is the sample size and \(k\) is the degree of the polynomial model.]")
      )
    })

    # ANOVA table
    output$polyAnovaTable <- renderTable({
      model <- polyModel()
      k     <- model$rank - 1
      n     <- length(storedDatx())
      av    <- anova(model)

      SSR <- sum(av[["Sum Sq"]][-nrow(av)])
      SSE <- av[["Sum Sq"]][nrow(av)]
      SST <- SSR + SSE
      MSR <- SSR / k
      MSE <- SSE / (n - k - 1)
      F_stat <- MSR / MSE
      p_val  <- pf(F_stat, k, n - k - 1, lower.tail = FALSE)

      tibble::tribble(
        ~"Source", ~"df", ~"SS", ~"MS", ~"F", ~"P-value",
        "<strong>Regression (Model)</strong>", as.integer(k),       SSR, MSR,    F_stat, p_val,
        "<strong>Error (Residual)</strong>",   as.integer(n-k-1),   SSE, MSE,    NA,     NA,
        "<strong>Total</strong>",              as.integer(n-1),      SST, NA,     NA,     NA
      )
    }, na = "", striped = TRUE, align = "c",
       sanitize.text.function = function(x) x)

    # F distribution plot
    output$polyAnovaFPlot <- renderPlot({
      model  <- polyModel()
      k      <- model$rank - 1
      n      <- length(storedDatx())
      av     <- anova(model)
      SSR    <- sum(av[["Sum Sq"]][-nrow(av)])
      SSE    <- av[["Sum Sq"]][nrow(av)]
      MSR    <- SSR / k
      MSE    <- SSE / (n - k - 1)
      f_stat <- MSR / MSE
      f_crit <- qf(0.95, k, n - k - 1)
      x_max  <- f_crit * 3
      x      <- seq(0, x_max, length.out = 1000)
      y      <- df(x, k, n - k - 1)
      plot_df <- data.frame(x = x, y = y)

      ggplot(plot_df, aes(x = x, y = y)) +
        geom_line(lwd = 1) +
        geom_area(data = subset(plot_df, x >= f_crit),
                  aes(x = x, y = y), fill = "red", alpha = 0.3) +
        {if (f_stat <= x_max)
          geom_area(data = subset(plot_df, x >= f_stat),
                    aes(x = x, y = y), fill = "blue", alpha = 0.3)} +
        geom_vline(xintercept = f_crit, colour = "red",  linewidth = 0.8, linetype = "dashed") +
        {if (f_stat <= x_max)
          geom_vline(xintercept = f_stat, colour = "blue", linewidth = 0.8, linetype = "dashed")} +
        labs(title = "F Distribution", x = "F", y = "Density") +
        annotate("text", x = f_crit, y = max(y) * 0.2,
                 label = sprintf("F critical\n= %.4f", f_crit),
                 hjust = -0.1, color = "red", size = 3.5) +
        annotate("text", x = x_max * 0.6, y = max(y) * 0.9,
                 label = sprintf("p-value = %.4f", pf(f_stat, k, n-k-1, lower.tail = FALSE)),
                 color = "black", size = 4) +
        {if (f_stat <= x_max)
          annotate("text", x = f_stat, y = max(y) * 0.4,
                   label = sprintf("F statistic\n= %.4f", f_stat),
                   hjust = -0.1, color = "blue", size = 3.5)} +
        theme_classic() +
        theme(plot.title  = element_text(hjust = 0.5, face = "bold"),
              axis.title  = element_text(size = 12, face = "bold"),
              axis.text   = element_text(size = 10, face = "bold")) +
        coord_cartesian(clip = "off")
    })

    # ANOVA p-value / conclusion
    output$polyAnovaPValue <- renderUI({
      model  <- polyModel()
      k      <- model$rank - 1
      n      <- length(storedDatx())
      av     <- anova(model)
      SSR    <- sum(av[["Sum Sq"]][-nrow(av)])
      SSE    <- av[["Sum Sq"]][nrow(av)]
      MSR    <- SSR / k
      MSE    <- SSE / (n - k - 1)
      F_stat <- MSR / MSE
      p_val  <- pf(F_stat, k, n - k - 1, lower.tail = FALSE)

      withMathJax(
        p(strong("Test Statistic:")),
        p(sprintf(
          r"{\(\displaystyle F = \frac{\text{MSR}}{\text{MSE}} = \frac{%s}{%s} = %0.2f \)}",
          fmt_sci_latex(MSR, 2), fmt_sci_latex(MSE, 2), F_stat
        )),
        p(strong("Conclusion:")),
        p(sprintf(
          r"[Since the p-value is %s than \(\alpha\) (\(%0.3f %s 0.05\)), %s.]",
          if (p_val <= 0.05) "less" else "greater",
          p_val,
          if (p_val <= 0.05) r"[\le]" else r"[>]",
          if (p_val <= 0.05)
            r"[we reject the null hypothesis (\(H_0\)) and conclude there is enough statistical evidence to support the alternative hypothesis (\(H_a\))]"
          else
            r"[we do not reject the null hypothesis (\(H_0\)) and conclude there isn't enough statistical evidence to support the alternative hypothesis (\(H_a\)).]"
        ))
      )
    })

    # R-squared
    output$polyRSquared <- renderUI({
      model  <- polyModel()
      k      <- model$rank - 1
      n      <- length(storedDatx())
      av     <- anova(model)
      SSR    <- sum(av[["Sum Sq"]][-nrow(av)])
      SSE    <- av[["Sum Sq"]][nrow(av)]
      SST    <- SSR + SSE

      withMathJax(
        p(strong(r"{ \(R^2\) and Adjusted \(R^2\) :}")),
        br(),
        p(sprintf(
          r"[\( \displaystyle R^2 = \frac{\text{SSR}}{\text{SST}} = \frac{%s}{%s} = %0.4f\)]",
          fmt_sci_latex(SSR, 4), fmt_sci_latex(SST, 4), SSR / SST
        )),
        p(sprintf(
          r"{\( \displaystyle R^2_{\text{adj}} = 1 - \left[ \left( 1-R^2 \right) \frac{n-1}{n-k-1} \right] = %0.4f \)}",
          summary(model)$adj.r.squared
        )),
        p(
          strong("Interpretation:"),
          sprintf(
            r"[Approximately \(%.2f\%%\) of the variation in the response variable is explained by the polynomial regression model when adjusted for the degree and the sample size.]",
            summary(model)$adj.r.squared * 100
          )
        ),
        br(),
        p(strong("Akaike Information Criteria (AIC):")),
        p(sprintf(r"[AIC = \(%0.4f\)]", AIC(model))),
        br(),
        p(strong("Bayesian Information Criteria (BIC):")),
        p(sprintf(r"[BIC = \(%0.4f\)]", BIC(model))),
        br()
      )
    })

    # INE assumption tests (Independence, Normality, Equal Variance)
    polyIneTestConfig <- list(
      list(
        assumption = "Independence",
        procedure  = "Durbin-Watson Test",
        min_n      = 5,
        run        = function(model) {
          dw <- lmtest::dwtest(model)
          list(statistic = round(dw$statistic, 4), p_value = round(dw$p.value, 4), note = NULL)
        }
      ),
      list(
        assumption = "Normality",
        procedure  = "Shapiro-Wilk Test",
        min_n      = 3,
        run        = function(model) {
          sw <- shapiro.test(residuals(model))
          list(statistic = round(sw$statistic, 4), p_value = round(sw$p.value, 4), note = NULL)
        }
      ),
      list(
        assumption = "Normality",
        procedure  = "Anderson-Darling Test",
        min_n      = 7,
        run        = function(model) {
          ad <- nortest::ad.test(residuals(model))
          list(statistic = round(ad$statistic, 4), p_value = round(ad$p.value, 4), note = NULL)
        }
      ),
      list(
        assumption = "Normality",
        procedure  = "Kolmogorov-Smirnov Test",
        min_n      = 3,
        run        = function(model) {
          ks <- ks.test(residuals(model), "pnorm",
                        mean = mean(residuals(model)),
                        sd   = sd(residuals(model)))
          list(statistic = round(ks$statistic, 4), p_value = round(ks$p.value, 4), note = NULL)
        }
      ),
      list(
        assumption = "Equal Variance (Homoskedasticity)",
        procedure  = "Breusch-Pagan Test",
        min_n      = 5,
        run        = function(model) {
          bp <- lmtest::bptest(model)
          list(statistic = round(bp$statistic, 4), p_value = round(bp$p.value, 4), note = NULL)
        }
      ),
      list(
        assumption = "Equal Variance (Homoskedasticity)",
        procedure  = "White Test",
        min_n      = 4,
        run        = function(model) {
          wt <- skedastic::white(model)
          list(statistic = round(wt$statistic, 4), p_value = round(wt$p.value, 4), note = NULL)
        }
      )
    )

    output$polyIneAssumptions <- renderUI({
      req(storedDatx(), storedDaty())
      model <- polyModel()
      alpha <- 0.05
      n     <- length(storedDatx())

      results <- lapply(polyIneTestConfig, function(cfg) {
        if (n < cfg$min_n) {
          return(data.frame(
            Assumption  = cfg$assumption,
            Procedure   = cfg$procedure,
            `P-Value`   = NA_character_,
            Conclusion  = paste("Requires n ≥", cfg$min_n),
            check.names = FALSE
          ))
        }
        result <- tryCatch(cfg$run(model), error = function(e) {
          list(statistic = NULL, p_value = NULL, note = paste("Error:", e$message))
        })
        pval_str <- if (!is.null(result$p_value)) as.character(result$p_value) else "—"
        conclusion <- if (!is.null(result$note)) {
          result$note
        } else if (!is.null(result$p_value)) {
          if (result$p_value <= alpha)
            paste0("Reject H₀ (p = ", result$p_value, " ≤ 0.05)")
          else
            paste0("Fail to reject H₀ (p = ", result$p_value, " > 0.05)")
        } else {
          "—"
        }
        data.frame(
          Assumption  = cfg$assumption,
          Procedure   = cfg$procedure,
          `P-Value`   = pval_str,
          Conclusion  = conclusion,
          check.names = FALSE
        )
      })

      tableData <- do.call(rbind, results)

      tagList(
        p(strong("Independence, Normality and Equal Variance (I.N.E) Assumptions"),
          style = "font-size: 16px;"),
        p(paste("Testing at α =", alpha, "| n =", n),
          style = "color: #666; font-size: 13px;"),
        br(),
        reactable(
          tableData,
          bordered   = TRUE,
          striped    = FALSE,
          highlight  = TRUE,
          pagination = FALSE,
          fullWidth  = TRUE,
          columns = list(
            Assumption = colDef(name = "Assumption", minWidth = 220, style = list(fontWeight = "bold")),
            Procedure  = colDef(name = "Procedure",  minWidth = 180),
            `P-Value`  = colDef(name = "P-Value",    minWidth = 100, align = "center"),
            Conclusion = colDef(name = "Conclusion", minWidth = 250)
          )
        ),
        br()
      )
    })

    # ---- Diagnostic Plots -------------------------------------------------

    hasPolyLeveragePlotIssue <- reactiveVal(FALSE)

    observe({
      req(storedDatx(), storedDaty())
      h <- hatvalues(polyModel())
      hasPolyLeveragePlotIssue(all(abs(h - 0.5) < .Machine$double.eps^0.5))
    })

    output$polyDiagnosticPlotsWarning <- renderUI({
      if (hasPolyLeveragePlotIssue()) {
        div(
          class = "alert alert-warning",
          tags$b("⚠ Diagnostic Plot Warning: "),
          "Residuals vs Leverage could not be produced because all leverage values are 0.5."
        )
      }
    })
    outputOptions(output, "polyDiagnosticPlotsWarning", suspendWhenHidden = FALSE)

    output$polyDiagPlot1 <- renderPlot({
      model <- polyModel()
      par(font.main = 2, font.lab = 2)
      plot(model, which = 1, pch = 20, main = "", lwd = 2, ann = FALSE, sub.caption = "", caption = "")
      title(main = "Residuals vs Fitted Values", cex.main = 1.2)
      title(xlab = expression(bold(Fitted~Values~(hat(italic(y))))))
      title(ylab = expression(bold(Residuals~plain("(")*italic(e)*plain(")"))))
      abline(h = 0, col = "black", lty = 2, lwd = 1.5)
    })

    output$polyDiagPlot2 <- renderPlot({
      model <- polyModel()
      par(font.main = 2, font.lab = 2)
      plot(model, which = 2, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "")
      title(main = "Q-Q Residuals", cex.main = 1.2)
      title(xlab = "Theoretical Quantiles")
    })

    output$polyDiagPlot3 <- renderPlot({
      model <- polyModel()
      par(font.main = 2, font.lab = 2)
      plot(model, which = 3, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "", ann = FALSE)
      title(main = "Scale-Location", cex.main = 1.2)
      title(ylab = "sqrt(|Standardized Residuals|)")
    })

    output$polyDiagPlot4 <- renderPlot({
      model <- polyModel()
      par(font.main = 2, font.lab = 2)
      plot(model, which = 5, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "")
      title(main = "Residuals vs Leverage", cex.main = 1.2)
    })

    output$polyDiagPlot5 <- renderPlot({
      model <- polyModel()
      par(font.main = 2, font.lab = 2)
      hist(residuals(model), main = "", xlab = "", col = "darkgreen", border = "white")
      title(main = "Histogram of Residuals", cex.main = 1.2)
      title(xlab = expression(bold(Residuals~plain("(")*italic(e)*plain(")"))))
    })

    # ---- Reset button -----------------------------------------------------
    observeEvent(input$resetPolynomial, {
      updateTextAreaInput(session, "polyY",
        value = "4.997, 6.165, 6.95, 8.218, 9.405, 10.404, 10.425, 10.44, 9.393, 7.854, 5.168")
      updateTextAreaInput(session, "polyX",
        value = "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10")
      updateNumericInput(session, "polyDegree",        value = 2)
      updateNumericInput(session, "polyScatterDegree", value = 2)

      # Clear file input and dependent dropdowns
      shinyjs::reset("polyUserData")
      updateSelectizeInput(session, "polySheet",       choices = character(0), selected = "")
      updateSelectizeInput(session, "polyResponse",    choices = character(0), selected = "")
      updateSelectizeInput(session, "polyExplanatory", choices = character(0), selected = "")

      storedDatx(NULL)
      storedDaty(NULL)
      nDroppedRows(0)
      fileState$status <- "reset"
      hide("polyResultsPanel")
      hide("polyUploadedDataPanel")
      hideTab(inputId = "polyNavbarPage", target = "Uploaded Data")
      hideTab(inputId = "polyNavbarPage", target = "Inference")
      hideTab(inputId = "polyNavbarPage", target = "Diagnostic Plots")
      hasPolyLeveragePlotIssue(FALSE)
    })

  })
}
