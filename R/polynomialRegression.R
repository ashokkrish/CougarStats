# R/polynomialRegression.R

# Largest polynomial degree accepted (model and scatterplot curve). A raw
# polynomial of much higher degree is numerically rank-deficient for any real
# data, and the cost of the fit grows with the square of the degree.
POLY_MAX_DEGREE <- 50L

# Most data points the scatterplot draws. Every point is sent to the browser
# and drawn as an SVG marker (about 100 bytes of JSON each), so a much larger
# plot freezes the browser tab. The fit and every other result always use all
# of the rows.
POLY_MAX_PLOT_POINTS <- 100000L

# Least-squares fit of a raw polynomial. The variable names datx/daty matter:
# the scatterplot predicts on a data frame whose column is called datx.
polyFitModel <- function(datx, daty, degree) {
  lm(daty ~ poly(datx, degree, raw = TRUE))
}

# White's test for heteroskedasticity (squares of the regressors, no
# cross-products): n * R^2 of the regression of the squared residuals on the
# regressors and their squares, compared with a chi-squared distribution.
# This is the computation of skedastic::white(model), but that function builds
# several n x n matrices (about 20 GB at 50,000 observations) only to sum the
# squared deviations of the squared residuals, which is done here directly.
polyWhiteTest <- function(model) {
  X    <- stats::model.matrix(model)
  X    <- X[, colSums(X != 1) > 0, drop = FALSE]          # drop the intercept
  Z    <- cbind(1, X, X^2)
  esq  <- stats::resid(model)^2
  aux  <- stats::lm.fit(Z, esq)
  stat <- length(esq) * (1 - sum(aux$residuals^2) / sum((esq - mean(esq))^2))
  list(statistic = stat,
       p.value   = stats::pchisq(stat, df = ncol(Z) - 1, lower.tail = FALSE))
}

# =========================================================================== #
# ---- UI Components -------------------------------------------------------- #
# =========================================================================== #

PolynomialRegressionSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(withMathJax(div(
    id = ns("polyInputPanel"),

    # Var pickers — shown only in upload mode once data is loaded
    hidden(div(
      id = ns("polyVarPickersPanel"),

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
    )),

    br(),
    p(strong("Model Options")),
    hr(),

    numericInput(
      inputId = ns("polyDegree"),
      label   = strong("Polynomial Degree (must be ≥ 2)"),
      value   = 2,
      min     = 2,
      max     = POLY_MAX_DEGREE,
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

  tagList(
    useShinyjs(),

    ## copyPlotToClipboard() for the Copy to Clipboard button is defined once in
    ## www/copyPlotToClipboard.js, loaded from ui.R.

    uiOutput(ns("polyNoDataWarn")),
    uiOutput(ns("polyResponseWarn")),
    uiOutput(ns("polyExplanatoryWarn")),
    uiOutput(ns("polyValidation")),

    hidden(div(
      id = ns("polyResultsPanel"),
      uiOutput(ns("polyPerfectFitWarning")),
      uiOutput(ns("polyMissingRowsWarning")),

      div(
        id = ns("polyNavbarContent"),

      navbarPage(
        title = NULL,
        id    = ns("polyNavbarPage"),
        theme = bs_theme(version = 4),

        tabPanel(
          title = "Data",
          value = "data_tab",
          br(),
          div(style = "overflow-x: auto;", DTOutput(ns("polyViewUploadTab"))),
          br()
        ),

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
                max     = POLY_MAX_DEGREE,
                step    = 1
              ),
              p(
                class = "text-muted",
                style = "font-size: 0.85em; margin-top: -8px;",
                tags$em("Note: this degree only affects the curve displayed on this scatterplot and does not change the model used in any other tab.")
              )
            )
          ),

          uiOutput(ns("polyScatterIntervalWarning")),
          uiOutput(ns("polyScatterSampleNote")),

          plotOptionsMenuUI(
            id                  = ns("polyScatter"),
            plotType            = "Scatterplot",
            title               = "Scatterplot",
            xlab                = "x",
            ylab                = "y",
            colour              = "#00AA00",
            dim                 = "in px",
            includeGridlines    = FALSE,
            includeFlip         = FALSE,
            regressionLineLabel = "Polynomial Regression Curve",
            includeLinearLine   = TRUE,
            lineControlLabel    = "Polynomial Curve"
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

      ) # navbarPage

      ) # polyNavbarContent
    )), # polyResultsPanel

    # The Data tab only exists in Upload Data mode once results are shown, so
    # its link starts hidden in the page itself (the same inline display:none
    # that hideTab() sets, and that showTab() undoes) rather than being hidden
    # by the server when it starts.
    tags$script(HTML(sprintf(
      "$(function() { $('#%s a[data-value=\"data_tab\"]').parent().hide(); });",
      ns("polyNavbarPage")
    )))
  )
}


# =========================================================================== #
# ---- Server --------------------------------------------------------------- #
# =========================================================================== #

PolynomialRegressionServer <- function(id, reg_data, input_mode, reset_upload, upload_error = NULL, clear_trigger = NULL, hide_shared = NULL, reset_raw_data = NULL, raw_error_msgs = NULL, raw_input_trigger = NULL, is_active = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ---- Reactive values --------------------------------------------------
    nDroppedRows   <- reactiveVal(0)
    nNonFiniteRows <- reactiveVal(0)
    polyNoDataWarn <- reactiveVal(FALSE)
    polyResponseWarn    <- reactiveVal(FALSE)
    polyExplanatoryWarn <- reactiveVal(FALSE)

    # Store datx/daty so the scatterplot can rerender on degree change
    # without needing to re-click Calculate
    storedDatx <- reactiveVal(NULL)
    storedDaty <- reactiveVal(NULL)

    # The degree and the fitted model of the last successful Calculate. The
    # Model and Inference tabs read these (never the live degree box), so
    # editing the degree box cannot make them refit with a half-typed value.
    storedDegree <- reactiveVal(NULL)
    storedModel  <- reactiveVal(NULL)

    # Content of the warning outputs below. They are plain outputs created
    # once; the observers only change these values.
    polyValidationUI <- reactiveVal(NULL)
    polyPerfectFit   <- reactiveVal(FALSE)

    # Forgets the results of the last Calculate and the warnings shown with them
    polyClearResults <- function() {
      storedDatx(NULL)
      storedDaty(NULL)
      storedDegree(NULL)
      storedModel(NULL)
      polyPerfectFit(FALSE)
      nDroppedRows(0)
      nNonFiniteRows(0)
    }

    # ---- Upload warning outputs -------------------------------------------
    output$polyNoDataWarn <- renderUI({
      if (!polyNoDataWarn()) return(NULL)
      div(class = "alert alert-danger", style = "margin-top: 15px;",
          icon("triangle-exclamation"),
          strong(" Please Upload Data before calculating."))
    })
    output$polyResponseWarn <- renderUI({
      if (!polyResponseWarn()) return(NULL)
      div(class = "alert alert-warning", style = "margin-top: 15px;",
          icon("triangle-exclamation"),
          strong(" Please select a Response Variable before calculating."))
    })
    output$polyExplanatoryWarn <- renderUI({
      if (!polyExplanatoryWarn()) return(NULL)
      div(class = "alert alert-warning", style = "margin-top: 15px;",
          icon("triangle-exclamation"),
          strong(" Please select an Explanatory Variable before calculating."))
    })

    output$polyValidation <- renderUI({ polyValidationUI() })

    output$polyMissingRowsWarning <- renderUI({
      n <- nDroppedRows()
      if (n > 0) {
        div(
          class = "alert alert-warning",
          role  = "alert",
          style = "margin-top: 10px;",
          tags$b("⚠️ Missing Data Detected: "),
          sprintf("%d row%s with %s removed before analysis.",
                  n, if (n == 1) "" else "s",
                  if (nNonFiniteRows() > 0) "missing or non-finite values" else "missing values")
        )
      }
    })

    output$polyPerfectFitWarning <- renderUI({
      if (isTRUE(polyPerfectFit())) {
        div(
          class = "alert alert-warning",
          role  = "alert",
          style = "margin-top: 10px;",
          tags$b("⚠️ Perfect Fit Detected: "),
          "This may indicate that ",
          tags$b("x and y are identical or linearly dependent,"),
          " which can produce unreliable inference and diagnostic plots. Standard statistical significance tests cannot run on perfect fits. Please check your data."
        )
      }
    })

    # ---- Selected columns (upload mode) -----------------------------------
    # Parsed once and shared by the validators and by Calculate.
    polyRawX <- reactive({
      dat <- reg_data()
      req(!is.null(dat), input$polyExplanatory %in% colnames(dat))
      as.data.frame(dat)[[input$polyExplanatory]]
    })
    polyRawY <- reactive({
      dat <- reg_data()
      req(!is.null(dat), input$polyResponse %in% colnames(dat))
      as.data.frame(dat)[[input$polyResponse]]
    })
    polyNumX <- reactive(suppressWarnings(as.numeric(polyRawX())))
    polyNumY <- reactive(suppressWarnings(as.numeric(polyRawY())))
    polyDistinctX <- reactive({
      datx <- polyNumX()
      length(unique(datx[is.finite(datx)]))
    })

    # ---- Input Validators -------------------------------------------------
    poly_iv       <- InputValidator$new()
    polyupvars_iv <- InputValidator$new()

    poly_iv$add_rule("polyDegree", sv_required())
    poly_iv$add_rule("polyDegree", ~ {
      d <- input$polyDegree
      if (!is.na(d) && (d != floor(d) || d < 2))
        "Polynomial degree must be a whole number ≥ 2."
    })
    poly_iv$add_rule("polyDegree", ~ {
      d <- input$polyDegree
      if (!is.na(d) && d > POLY_MAX_DEGREE)
        paste0("Polynomial degree must be at most ", POLY_MAX_DEGREE, ".")
    })

    polyupvars_iv$add_rule("polyResponse",    sv_required())
    polyupvars_iv$add_rule("polyExplanatory", sv_required())
    polyupvars_iv$add_rule("polyExplanatory", ~ tryCatch({
      raw <- polyRawX()
      if (length(raw) == 0 || any(is.na(polyNumX()) & !is.na(raw)))
        "Explanatory variable contains non-numeric data."
    }, error = function(e) NULL))
    polyupvars_iv$add_rule("polyExplanatory", ~ tryCatch({
      n <- sum(!is.na(polyNumX()))
      d <- input$polyDegree
      if (!is.na(d) && n > 0 && d >= n - 1)
        paste0("A degree-", d, " polynomial requires at least ", d + 2, " observations (currently ", n, ").")
    }, error = function(e) NULL))
    polyupvars_iv$add_rule("polyExplanatory", ~ tryCatch({
      k <- polyDistinctX()
      d <- input$polyDegree
      if (!is.na(d) && k > 0 && d >= k)
        paste0("A degree-", d, " polynomial requires at least ", d + 1, " distinct values of x (currently ", k, ").")
    }, error = function(e) NULL))
    polyupvars_iv$add_rule("polyExplanatory", ~ tryCatch({
      datx <- polyNumX()
      datx <- datx[is.finite(datx)]
      if (length(datx) > 0 && sd(datx) == 0)
        "Explanatory variable has a standard deviation equal to zero (all values are identical). At least two distinct values are required."
    }, error = function(e) NULL))
    polyupvars_iv$add_rule("polyResponse", ~ tryCatch({
      raw <- polyRawY()
      if (length(raw) == 0 || any(is.na(polyNumY()) & !is.na(raw)))
        "Response variable contains non-numeric data."
    }, error = function(e) NULL))
    polyupvars_iv$add_rule("polyResponse", ~ tryCatch({
      daty <- polyNumY()
      daty <- daty[is.finite(daty)]
      if (length(daty) > 0 && sd(daty) == 0)
        "Response variable is constant. At least two distinct values are required."
    }, error = function(e) NULL))

    polyupvars_iv$condition(~ isTRUE(input_mode() == "upload" && !is.null(reg_data())))
    poly_iv$add_validator(polyupvars_iv)

    poly_iv$enable()
    polyupvars_iv$enable()

    # ---- Clear no-data warning when data is uploaded ----------------------
    observeEvent(reg_data(), {
      if (!is.null(reg_data())) polyNoDataWarn(FALSE)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # Clear results whenever raw data inputs change.
    # Uses raw_input_trigger (watches rawX/rawY directly) rather than reg_data() so
    # the clear fires on every keystroke regardless of whether the data is currently
    # valid — reg_data() can stay NULL mid-keystroke and silently skip the clear.
    observeEvent(raw_input_trigger(), {
      if (!is.null(is_active) && !is_active()) return()
      hide("polyResultsPanel")
      polyClearResults()
      polyValidationUI(NULL)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Clear vars warning when a variable is selected ------------------
    observeEvent(input$polyExplanatory, {
      if (isTruthy(input$polyExplanatory)) polyExplanatoryWarn(FALSE)
    }, ignoreInit = TRUE)
    observeEvent(input$polyResponse, {
      if (isTruthy(input$polyResponse)) polyResponseWarn(FALSE)
    }, ignoreInit = TRUE)

    # ---- Reset results when input mode changes ----------------------------
    observeEvent(input_mode(), {
      polyValidationUI(NULL)
      polyNoDataWarn(FALSE)
      polyResponseWarn(FALSE)
      polyExplanatoryWarn(FALSE)
      hide("polyResultsPanel")
      hideTab(inputId = "polyNavbarPage", target = "Inference")
      polyClearResults()
      if (input_mode() == "upload" && !is.null(reg_data())) {
        show("polyVarPickersPanel")
      } else {
        hide("polyVarPickersPanel")
      }
    }, ignoreInit = TRUE)

    # ---- Reset results when degree changes (both modes) -------------------
    observeEvent(input$polyDegree, {
      polyValidationUI(NULL)
      hide("polyResultsPanel")
      hideTab(inputId = "polyNavbarPage", target = "Inference")
      polyClearResults()
    }, ignoreInit = TRUE)

    # ---- Plot options module ----------------------------------------------
    plotOptionsMenuServer("polyScatter")

    # Keeps the variable pickers in step with the uploaded data. Raw entry has
    # no pickers (the input_mode() observer above hides them), so typing in the
    # raw boxes, which changes reg_data() on nearly every keystroke, does
    # nothing here.
    observeEvent(list(reg_data(), input_mode()), {
      if (input_mode() != "upload") return()
      dat <- reg_data()
      if (is.null(dat)) {
        hide("polyVarPickersPanel")
        return()
      }
      cols <- colnames(dat)
      updateSelectizeInput(session, "polyResponse",    choices = cols)
      updateSelectizeInput(session, "polyExplanatory", choices = cols)
      shinyjs::delay(0, {
        show("polyVarPickersPanel")
      })
    })

    # ---- Uploaded data preview -------------------------------------------
    output$polyViewUploadTab <- renderDT({
      req(input_mode() == "upload", !is.null(reg_data()))
      dat <- reg_data()
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
      req(!is.null(d), !is.na(d), d >= 2, d <= POLY_MAX_DEGREE, d < n)
      fit <- tryCatch(polyFitModel(datx, daty, as.integer(d)), error = function(e) e)
      validate(need(!inherits(fit, "error"), paste0(
        "The degree-", as.integer(d), " curve could not be fitted to this data (",
        if (inherits(fit, "error")) conditionMessage(fit) else "", "). Please choose a lower degree.")))
      fit
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
      } else if (d > POLY_MAX_DEGREE) {
        div(
          class = "alert alert-danger",
          style = "margin-top: 10px;",
          tags$b("Invalid degree: "),
          paste0("Degree must be at most ", POLY_MAX_DEGREE, ".")
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
      } else if (d == n - 1) {
        div(
          class = "alert alert-warning",
          style = "margin-top: 10px;",
          tags$b("Perfect fit: "),
          paste0(
            "A degree-", d, " polynomial through ", n, " points is a perfect fit ",
            "(residual df = 0). Confidence and prediction intervals are not available."
          )
        )
      }
    })

    # Points drawn on the scatterplot. Above POLY_MAX_PLOT_POINTS rows only an
    # evenly spaced subset of the rows (always including the smallest and the
    # largest x) is drawn, so the plot stays responsive; the fitted curve and
    # bands always use every observation. Computed once per Calculate.
    scatterPoints <- reactive({
      req(storedDatx(), storedDaty())
      datx <- storedDatx()
      daty <- storedDaty()
      n    <- length(datx)
      if (n > POLY_MAX_PLOT_POINTS) {
        idx  <- unique(c(round(seq(1, n, length.out = POLY_MAX_PLOT_POINTS)),
                         which.min(datx), which.max(datx)))
        datx <- datx[idx]
        daty <- daty[idx]
      }
      data.frame(x = datx, y = daty)
    })

    output$polyScatterSampleNote <- renderUI({
      req(storedDatx())
      n <- length(storedDatx())
      if (n > POLY_MAX_PLOT_POINTS) {
        p(
          class = "text-muted",
          style = "font-size: 0.85em;",
          tags$em(sprintf(
            "Note: to keep the plot responsive, %s of the %s points are drawn (evenly spaced rows). The fitted curve, the bands and all other results use all %s observations.",
            format(POLY_MAX_PLOT_POINTS, big.mark = ","), format(n, big.mark = ","), format(n, big.mark = ",")
          ))
        )
      }
    })

    output$polyScatterplot <- renderPlotly({
      req(scatterModel(), storedDatx(), storedDaty())
      datx <- storedDatx()
      daty <- storedDaty()
      df   <- scatterPoints()

      p <- RenderScatterplot(
        df,
        scatterModel(),
        input[["polyScatter-Title"]],
        input[["polyScatter-Xlab"]],
        input[["polyScatter-Ylab"]],
        input[["polyScatter-Colour"]],
        input[["polyScatter-PointsColour"]],
        input[["polyScatter-RegLineWidth"]],
        input[["polyScatter-ConfidenceBandWidth"]],
        input[["polyScatter-PredictionBandWidth"]],
        input[["polyScatter-PointSize"]],
        input[["polyScatter-Gridlines"]],
        isTRUE(input[["polyScatter-confidenceInterval"]]) && df.residual(scatterModel()) > 0,
        isTRUE(input[["polyScatter-predictionInterval"]]) && df.residual(scatterModel()) > 0,
        input[["polyScatter-showRegressionLine"]],
        input[["polyScatter-ConfidenceBandColour"]],
        input[["polyScatter-PredictionBandColour"]],
        input[["polyScatter-RegLineOpacity"]],
        input[["polyScatter-ConfidenceBandOpacity"]],
        input[["polyScatter-PredictionBandOpacity"]]
      ) %>%
        style(name = "Polynomial Regression Curve", traces = 2)

      if (isTRUE(input[["polyScatter-showLinearLine"]])) {
        linear_model <- lm(daty ~ datx)
        x_seq        <- seq(min(datx), max(datx), length.out = 200)
        y_linear     <- predict(linear_model, newdata = data.frame(datx = x_seq))
        lw           <- as.numeric(input[["polyScatter-LinearLineWidth"]]) * 2
        lin_alpha    <- input[["polyScatter-LinearLineOpacity"]] / 100
        lin_rgba     <- sprintf("rgba(255,0,0,%.2f)", lin_alpha)
        p <- p %>% add_trace(
          x             = x_seq,
          y             = y_linear,
          type          = "scatter",
          mode          = "lines",
          name          = "Linear Regression Line",
          inherit       = FALSE,
          line          = list(color = lin_rgba, width = lw),
          hovertemplate = "<b>Linear Fit:</b> %{y:.4f}<br><extra></extra>"
        )
      }

      p
    })

    # ---- Calculate button -------------------------------------------------
    # Tells the user why nothing was calculated, and hides the results panel
    # (which an earlier step of Calculate may already have shown) so that no
    # result of an earlier run is left on screen.
    polyCalcFailed <- function(msg, duration = 8) {
      showNotification(msg, type = "error", duration = duration)
      polyClearResults()
      hide("polyResultsPanel")
      # An earlier successful Calculate hid the shared data preview: show it
      # again, or the main panel is left empty once the message has gone.
      if (input_mode() == "upload" && !is.null(hide_shared)) hide_shared(FALSE)
    }

    polyCalculate <- function() {
      if (input_mode() == "upload" && is.null(reg_data())) {
        if (!is.null(upload_error)) upload_error(TRUE)
        polyNoDataWarn(TRUE)
        polyResponseWarn(FALSE)
        polyExplanatoryWarn(FALSE)
        polyValidationUI(NULL)
        hide("polyResultsPanel")
        return()
      }
      if (!is.null(upload_error)) upload_error(FALSE)
      polyNoDataWarn(FALSE)

      if (input_mode() == "upload") {
        missingResponse    <- !isTruthy(input$polyResponse)
        missingExplanatory <- !isTruthy(input$polyExplanatory)
        polyResponseWarn(missingResponse)
        polyExplanatoryWarn(missingExplanatory)
        if (missingResponse || missingExplanatory) {
          polyValidationUI(NULL)
          hide("polyResultsPanel")
          return()
        }
      } else {
        polyResponseWarn(FALSE)
        polyExplanatoryWarn(FALSE)
        rawMsgs <- if (!is.null(raw_error_msgs)) raw_error_msgs() else list(x = NULL, y = NULL)
        msgX <- rawMsgs$x
        msgY <- rawMsgs$y
        if (!is.null(msgX) || !is.null(msgY) || is.null(reg_data())) {
          if (is.null(msgX) && is.null(msgY))
            msgX <- "x and y must have the same number of valid numeric observations."
          polyValidationUI(
            tagList(
              if (!is.null(msgY)) div(
                class = "alert alert-danger", style = "margin-top: 15px; margin-bottom: 5px;",
                icon("triangle-exclamation"), " ",
                strong(msgY), " — Response variable (y)"
              ),
              if (!is.null(msgX)) div(
                class = "alert alert-danger", style = "margin-top: 5px;",
                icon("triangle-exclamation"), " ",
                strong(msgX), " — Explanatory variable (x)"
              )
            )
          )
          hide("polyResultsPanel")
          return()
        }
      }

      polyClearResults()
      show("polyResultsPanel")
      toggle("polyNavbarContent", condition = poly_iv$is_valid())

      polyValidationUI(NULL)

      if (!poly_iv$is_valid()) return()

      # -- Extract data ------------------------------------------------------
      degree <- input$polyDegree
      req(is.numeric(degree), is.finite(degree), degree == floor(degree),
          degree >= 2, degree <= POLY_MAX_DEGREE)
      degree <- as.integer(degree)

      if (input_mode() == "upload") {
        raw_x <- polyNumX()
        raw_y <- polyNumY()
      } else {
        raw_x <- reg_data()$x
        raw_y <- reg_data()$y
      }

      if (is.null(raw_x) || is.null(raw_y)) {
        hide("polyNavbarContent")
        polyValidationUI(
          div(
            class = "alert alert-danger",
            style = "margin-top: 15px;",
            icon("triangle-exclamation"),
            strong(" x and y must have the same number of valid numeric observations.")
          )
        )
        return()
      }

      # Complete, finite pairs only (an uploaded column can hold Inf)
      complete_idx <- is.finite(raw_x) & is.finite(raw_y)
      datx         <- raw_x[complete_idx]
      daty         <- raw_y[complete_idx]
      nDroppedRows(sum(!complete_idx))
      nNonFiniteRows(sum(is.infinite(raw_x) | is.infinite(raw_y)))

      if (length(datx) <= degree + 1) {
        polyCalcFailed(paste0(
          "After removing missing values, fewer than ", degree + 2,
          " complete observations remain for a degree-", degree,
          " polynomial. Please choose different variables or a lower degree."
        ))
        return()
      }

      nDistinctX <- length(unique(datx))
      if (nDistinctX <= degree) {
        polyCalcFailed(paste0(
          "A degree-", degree, " polynomial requires at least ", degree + 1,
          " distinct values of x, but only ", nDistinctX,
          " remain after removing missing values. Please choose different variables or a lower degree."
        ))
        return()
      }

      # -- Fit model ---------------------------------------------------------
      model <- tryCatch(polyFitModel(datx, daty, degree), error = function(e) e)
      if (inherits(model, "error")) {
        polyCalcFailed(paste0(
          "The degree-", degree, " polynomial model could not be fitted to this data (",
          conditionMessage(model), "). Please check the data for extremely large values or choose a lower degree."
        ), duration = 10)
        return()
      }
      if (anyNA(coef(model))) {
        polyCalcFailed(paste0(
          "A degree-", degree, " polynomial cannot be estimated for this data: the powers of x are too close to ",
          "being linearly dependent, so some coefficients cannot be determined. Please choose a lower degree."
        ), duration = 10)
        return()
      }

      # -- Perfect fit detection ---------------------------------------------
      r_squared    <- summary(model)$r.squared
      isPerfectFit <- isTRUE(all.equal(r_squared, 1))

      # -- Show the results --------------------------------------------------
      showTab(inputId = "polyNavbarPage", target = "Inference")
      if (input_mode() == "upload") {
        showTab(inputId = "polyNavbarPage", target = "data_tab")
        if (!is.null(hide_shared)) hide_shared(TRUE)
      }

      updateNavbarPage(session, "polyNavbarPage", selected = "Model")

      # Store for scatterplot reactive use and for the Model / Inference tabs
      storedDatx(datx)
      storedDaty(daty)
      storedDegree(degree)
      storedModel(model)

      polyPerfectFit(isPerfectFit)
      if (isPerfectFit) {
        hideTab(inputId = "polyNavbarPage", target = "Inference")
      }

      # Sync scatter degree to model degree on each Calculate press
      updateNumericInput(session, "polyScatterDegree", value = degree)
    } # polyCalculate

    # An uncaught error in an observer would end the user's session, so any
    # unexpected failure is reported to the user instead. A req()/validate()
    # that stops the calculation is not an error: it is passed on to Shiny
    # untouched (the results panel is hidden first, as nothing was calculated).
    observeEvent(input$goPolynomial, {
      tryCatch(
        withCallingHandlers(
          polyCalculate(),
          shiny.silent.error = function(e) hide("polyResultsPanel")
        ),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          message("Polynomial regression: Calculate failed: ", conditionMessage(e))
          polyCalcFailed(paste0("The polynomial regression could not be calculated: ", conditionMessage(e)), duration = 10)
        }
      )
    })

    # ---- Model tab --------------------------------------------------------
    output$polyModelEquation <- renderUI({

      model  <- polyModel()
      degree <- storedDegree()
      coefs  <- coef(model)

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
        p("The estimated polynomial regression model is"),
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

    # ---- Inference tab ----------------------------------------------------

    # The model fitted by the last Calculate (see storedModel above)
    polyModel <- reactive({
      req(storedModel(), storedDegree())
      storedModel()
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
              p(strong("ANOVA Table:")),
              fluidRow(DTOutput(ns("polyAnovaTable"))),
              br(),
              fluidRow(
                column(12,
                  p(strong("F Distribution")),
                  p("The shaded region represents the rejection region at α = 0.05. The dashed red line is the observed F statistic and the dashed blue line is the critical value."),
                  plotOutput(ns("polyAnovaFPlot"))
                )
              ),
              br(),
              fluidRow(uiOutput(ns("polyAnovaPValue"))),
              br(),
              fluidRow(uiOutput(ns("polyRSquared"))),
              br()
            ),
            tabPanel(
              title = "LINE",
              br(),
              uiOutput(ns("polyIneAssumptions"))
            ),
            tabPanel(
              title = "Diagnostic Plots",
              fluidPage(
                br(),
                uiOutput(ns("polyDiagnosticPlotsWarning")),
                plotOutput(ns("polyDiagPlot1")),
                plotOutput(ns("polyDiagPlot2")),
                plotOutput(ns("polyDiagPlot3")),
                plotOutput(ns("polyDiagPlot4")),
                plotOutput(ns("polyDiagPlot5"))
              )
            )
          )
        )
      )
    })

    # Parameter Estimates
    output$polyParamCoefTable <- renderTable({
      model  <- polyModel()
      degree <- storedDegree()

      coefs <- as.data.frame(summary(model)$coefficients)
      coefs <- tibble::rownames_to_column(coefs, "Term")
      sup_digits  <- c("⁰","¹","²","³","⁴","⁵","⁶","⁷","⁸","⁹")
      # Superscript exponent, one digit at a time (degrees of 10 and above)
      sup_exp     <- function(k) vapply(k, function(e)
        paste(sup_digits[as.integer(strsplit(as.character(e), "")[[1]]) + 1], collapse = ""), "")
      term_labels <- c("Intercept", "x", paste0("x", sup_exp(seq(2, degree))))
      if (nrow(coefs) == length(term_labels)) coefs$Term <- term_labels
      names(coefs)[names(coefs) == "Pr(>|t|)"] <- "P-value"

      ci <- as.data.frame(confint(model))
      colnames(ci) <- c("Lower 95% CI", "Upper 95% CI")
      ci <- tibble::rownames_to_column(ci, "Term")
      if (nrow(ci) == length(term_labels)) ci$Term <- term_labels

      tbl <- dplyr::left_join(coefs, ci, by = "Term")
      tibble::column_to_rownames(tbl, var = "Term")
    }, rownames = TRUE, na = "", striped = TRUE, align = "c", digits = 4)

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
    output$polyAnovaTable <- renderDT({
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

      p_val_display <- if (p_val < 0.0001 && p_val > 0) "P < 0.0001" else sprintf("%.4f", p_val)

      data <- data.frame(
        df        = c(as.integer(k), as.integer(n - k - 1), as.integer(n - 1)),
        SS        = c(SSR, SSE, SST),
        MS        = c(MSR, MSE, NA),
        F         = c(F_stat, NA, NA),
        `P-Value` = c(p_val_display, NA_character_, NA_character_),
        check.names = FALSE
      )
      rownames(data) <- c("Regression (Model)", "Error (Residual)", "Total")

      colNames <- c("df", "Sum of Squares (SS)", "Mean Sum of Squares (MS)", "F-ratio", "P-Value")

      .aw <- function(hdr, vals, digits = NULL, big_mark = "", px = 9L, pad = 28L, min_w = 60L) {
        vs    <- vals[!is.na(vals)]
        fmted <- if (!is.null(digits) && length(vs) > 0)
          sapply(as.numeric(vs), function(v) formatC(v, format = "f", digits = digits, big.mark = big_mark))
        else as.character(vs)
        max(min_w, max(nchar(c(hdr, fmted))) * px + pad)
      }
      w0 <- .aw("Sources of Variation",    rownames(data))
      w1 <- .aw("df",                       data$df,          digits = 0)
      w2 <- .aw("Sum of Squares (SS)",       data$SS,          digits = 4, big_mark = ",")
      w3 <- .aw("Mean Sum of Squares (MS)",  data$MS,          digits = 4, big_mark = ",")
      w4 <- .aw("F-ratio",                   data$F,           digits = 4, big_mark = ",")
      w5 <- .aw("P-Value",                   data[["P-Value"]])

      headers <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th("Sources of Variation",
               style = "border: 1px solid rgba(0, 0, 0, 0.15);
                          border-bottom: 1px solid rgba(0, 0, 0, 0.3);"),
            lapply(colNames, th,
                   style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                              border-top: 1px solid rgba(0, 0, 0, 0.15);')
          )
        )
      ))

      datatable(
        data,
        class = 'cell-border stripe compact',
        container = headers,
        options = list(
          dom = 't',
          pageLength = -1,
          ordering = FALSE,
          searching = FALSE,
          paging = FALSE,
          autoWidth = FALSE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = 0:5),
            list(width = paste0(w0, 'px'), targets = 0),
            list(width = paste0(w1, 'px'), targets = 1),
            list(width = paste0(w2, 'px'), targets = 2),
            list(width = paste0(w3, 'px'), targets = 3),
            list(width = paste0(w4, 'px'), targets = 4),
            list(width = paste0(w5, 'px'), targets = 5)
          )
        ),
        selection = "none",
        escape = FALSE,
        filter = "none"
      ) %>%
        formatRound(columns = 1, digits = 0) %>%
        formatRound(columns = 2:4, digits = 4) %>%
        formatStyle(columns = c(0, 4), fontWeight = 'bold') %>%
        formatStyle(
          columns = 1:5,
          target = 'row',
          fontWeight = styleRow(3, "bold")
        )
    })

    # F distribution plot
    output$polyAnovaFPlot <- renderPlot({
      model  <- polyModel()
      k      <- model$rank - 1
      n      <- length(storedDatx())

      if (n - k - 1 <= 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "F distribution cannot be computed:\nThe model is a perfect fit (residual df = 0).",
                     hjust = 0.5, vjust = 0.5, size = 5) +
            theme_void()
        )
      }

      av     <- anova(model)
      SSR    <- sum(av[["Sum Sq"]][-nrow(av)])
      SSE    <- av[["Sum Sq"]][nrow(av)]
      MSR    <- SSR / k
      MSE    <- SSE / (n - k - 1)
      f_stat <- MSR / MSE
      f_crit <- qf(0.95, k, n - k - 1)
      anovaFPlot(round(f_stat, 4), round(f_crit, 4), df1 = k, df2 = n - k - 1)
    }, height = 400, width = 650)

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

      if (is.nan(p_val) || is.na(p_val)) {
        return(tagList(
          p(strong("Test Statistic:")),
          p("F statistic cannot be computed: the model is a perfect fit (residual df = 0)."),
          p(strong("Conclusion:")),
          p("With zero residual degrees of freedom, the ANOVA F-test is unreliable.")
        ))
      }

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

    # LINE assumption tests (Linearity, Independence, Normality, Equal Variance)
    polyIneTestConfig <- list(
      list(
        assumption = "Linearity",
        procedure  = "Residuals vs Fitted Plot",
        min_n      = 3,
        run        = function(model) {
          list(statistic = NULL, p_value = NULL,
               note = "See Residuals vs Fitted plot in Diagnostic Plots tab")
        }
      ),
      list(
        assumption = "Linearity",
        procedure  = "Rainbow Test",
        min_n      = 6,
        run        = function(model) {
          rb <- lmtest::raintest(model)
          list(statistic = round(rb$statistic, 4), p_value = round(rb$p.value, 4), note = NULL)
        }
      ),
      list(
        assumption = "Linearity",
        procedure  = "Ramsey RESET Test",
        min_n      = 6,
        run        = function(model) {
          rt <- lmtest::resettest(model)
          list(statistic = round(rt$statistic, 4), p_value = round(rt$p.value, 4), note = NULL)
        }
      ),
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
        max_n      = 5000,    # shapiro.test() only accepts 3 to 5000 observations
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
          resids <- residuals(model)
          has_ties <- anyDuplicated(resids) > 0
          ks <- suppressWarnings(ks.test(resids, "pnorm",
                                         mean = mean(resids),
                                         sd   = sd(resids)))
          note <- if (has_ties) "Ties detected in residuals; p-value may be unreliable." else NULL
          list(statistic = round(ks$statistic, 4), p_value = round(ks$p.value, 4), note = note)
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
          wt <- polyWhiteTest(model)
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
        if (!is.null(cfg$max_n) && n > cfg$max_n) {
          return(data.frame(
            Assumption  = cfg$assumption,
            Procedure   = cfg$procedure,
            `P-Value`   = NA_character_,
            Conclusion  = paste("Requires n ≤", cfg$max_n),
            check.names = FALSE
          ))
        }
        result <- tryCatch(cfg$run(model), error = function(e) {
          list(statistic = NULL, p_value = NULL, note = paste("Error:", e$message))
        })
        # A test that returns no usable p-value must not break the whole table
        if (!is.null(result$p_value) && !(length(result$p_value) == 1 && is.finite(result$p_value))) {
          result <- list(statistic = NULL, p_value = NULL,
                         note = "The test statistic could not be computed for these data.")
        }
        pval_str <- if (!is.null(result$p_value)) as.character(result$p_value) else "—"
        conclusion <- if (!is.null(result$p_value)) {
          base <- if (result$p_value <= alpha)
            paste0("Reject H₀ (p = ", result$p_value, " ≤ 0.05)")
          else
            paste0("Fail to reject H₀ (p = ", result$p_value, " > 0.05)")
          if (!is.null(result$note)) paste0(base, " — ", result$note) else base
        } else if (!is.null(result$note)) {
          result$note
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
        p(strong("Linearity, Independence, Normality and Equal Variance (L.I.N.E) Assumptions"),
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

    hasPolyLeveragePlotIssue <- reactive({
      h <- hatvalues(polyModel())
      all(abs(h - 0.5) < .Machine$double.eps^0.5)
    })

    output$polyDiagnosticPlotsWarning <- renderUI({
      if (isTRUE(hasPolyLeveragePlotIssue())) {
        div(
          class = "alert alert-warning",
          tags$b("⚠ Diagnostic Plot Warning: "),
          "Residuals vs Leverage could not be produced because all leverage values are 0.5."
        )
      }
    })

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
      if (model$df.residual == 0) {
        plot.new()
        text(0.5, 0.5, "Q-Q plot not available:\nModel is a perfect fit (residual df = 0).", cex = 1.2)
        return(invisible(NULL))
      }
      par(font.main = 2, font.lab = 2)
      plot(model, which = 2, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "")
      title(main = "Q-Q Residuals", cex.main = 1.2)
      title(xlab = "Theoretical Quantiles")
    })

    output$polyDiagPlot3 <- renderPlot({
      model <- polyModel()
      if (model$df.residual == 0) {
        plot.new()
        text(0.5, 0.5, "Scale-Location plot not available:\nModel is a perfect fit (residual df = 0).", cex = 1.2)
        return(invisible(NULL))
      }
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
    polyr_do_reset <- function() {
      polyNoDataWarn(FALSE)
      polyResponseWarn(FALSE)
      polyExplanatoryWarn(FALSE)
      updateNumericInput(session, "polyDegree",        value = 2)
      updateNumericInput(session, "polyScatterDegree", value = 2)
      dat <- reg_data()
      if (input_mode() == "upload" && !is.null(dat)) {
        cols <- colnames(dat)
        updateSelectizeInput(session, "polyResponse",    choices = cols, selected = "")
        updateSelectizeInput(session, "polyExplanatory", choices = cols, selected = "")
        shinyjs::delay(0, show("polyVarPickersPanel"))
      } else {
        hide("polyVarPickersPanel")
        updateSelectizeInput(session, "polyResponse",    choices = c(""), selected = "")
        updateSelectizeInput(session, "polyExplanatory", choices = c(""), selected = "")
      }
      polyClearResults()
      polyValidationUI(NULL)
      if (!is.null(hide_shared)) hide_shared(FALSE)
      hide("polyResultsPanel")
      hideTab(inputId = "polyNavbarPage", target = "data_tab")
      hideTab(inputId = "polyNavbarPage", target = "Inference")
    }

    observeEvent(input$resetPolynomial, {
      reset_upload()
      if (!is.null(reset_raw_data)) reset_raw_data()
      polyr_do_reset()
    })

    if (!is.null(clear_trigger)) {
      observeEvent(clear_trigger(), {
        if (!is.null(is_active) && !is_active()) return()
        polyr_do_reset()
      }, ignoreInit = TRUE)
    }

  })
}
