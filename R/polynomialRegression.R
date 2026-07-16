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
        )

      ) # navbarPage
    )), # polyResultsPanel

    # Uploaded data preview panel
    div(
      id = ns("polyUploadedDataPanel"),
      tags$h4(
        "Uploaded Data",
        style = "color: #18536F; font-weight: bold; margin-bottom: 15px; margin-top: 10px;"
      ),
      uiOutput(ns("polyUploadedDataContent")),
      br()
    )
  ))
}


# =========================================================================== #
# ---- Server --------------------------------------------------------------- #
# =========================================================================== #

PolynomialRegressionServer <- function(id) {
  moduleServer(id, function(input, output, session) {

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
      } else if (df.residual(scatterModel()) == 0) {
        div(
          class = "alert alert-warning",
          style = "margin-top: 10px;",
          tags$b("⚠️ Perfect Fit Detected: "),
          paste0(
            "A degree-", d, " polynomial with ", n,
            " observations has 0 residual degrees of freedom. ",
            "Confidence and prediction intervals cannot be computed."
          )
        )
      }
    })

    output$polyScatterplot <- renderPlotly({
      req(scatterModel(), storedDatx(), storedDaty())
      df         <- data.frame(x = storedDatx(), y = storedDaty())
      has_resid_df <- df.residual(scatterModel()) > 0
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
        input[["polyScatter-confidenceInterval"]] && has_resid_df,
        input[["polyScatter-predictionInterval"]] && has_resid_df,
        input[["polyScatter-showRegressionLine"]]
      )
    })

    # ---- Calculate button -------------------------------------------------
    observeEvent(input$goPolynomial, {
      toggle("polyResultsPanel", condition = poly_iv$is_valid())
      if (!poly_iv$is_valid()) return()

      hide("polyUploadedDataPanel")

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
          tags$table(
            class = "table table-bordered table-sm",
            style = "width: auto; min-width: 220px;",
            tags$thead(
              tags$tr(
                tags$th("Parameter"),
                tags$th("Estimate")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(withMathJax("\\( \\hat{\\beta}_{0} \\)")),
                tags$td(fmt_coef(b0))
              ),
              lapply(seq_len(degree), function(k) {
                tags$tr(
                  tags$td(withMathJax(sprintf("\\( \\hat{\\beta}_{%d} \\)", k))),
                  tags$td(fmt_coef(coefs[k + 1]))
                )
              })
            )
          ),
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

    # ---- Reset button -----------------------------------------------------
    observeEvent(input$resetPolynomial, {
      updateTextAreaInput(session, "polyY",
        value = "4.997, 6.165, 6.95, 8.218, 9.405, 10.404, 10.425, 10.44, 9.393, 7.854, 5.168")
      updateTextAreaInput(session, "polyX",
        value = "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10")
      updateNumericInput(session, "polyDegree",        value = 2)
      updateNumericInput(session, "polyScatterDegree", value = 2)
      storedDatx(NULL)
      storedDaty(NULL)
      fileState$status <- "reset"
      hide("polyResultsPanel")
      show("polyUploadedDataPanel")
    })

  })
}
