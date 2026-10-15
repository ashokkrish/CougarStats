# R/multipleLinearRegression.R

## mnemonic: corset
## To set below the diagonal to NA, just t(corsetAboveDiagNA).
## Always calls "cor" with "use = 'complete.obs'".
corsetAboveDiagNA <- function(dat) {
  dat <- cor(dat, use = "complete.obs")
  for (column in seq(ncol(dat))) {
    if (!(column == ncol(dat))) {
      dat[column, (column + 1):ncol(dat)] <- NA
    }
  }
  return(dat)
}

## White's test of homoskedasticity: the computation of skedastic::white()
## (auxiliary regression of the squared residuals on the explanatory variables
## and their squares, statistic n * (1 - SSE_aux / SST)), without the n x n
## centring matrix that skedastic::white() builds. That matrix needs memory
## proportional to n^2 (about 600 MB for 5,000 rows and 10 GB for 20,000),
## which can exhaust the memory of the server. Same statistic and p-value.
mlrWhiteTest <- function(model) {
  X <- stats::model.matrix(model)
  ones <- apply(X, 2, function(x) all(x == 1))
  if (any(ones)) X <- X[, !ones, drop = FALSE]
  n <- nrow(X)
  Z <- cbind(1, X, X^2)
  esq <- stats::resid(model)^2
  e_aux <- stats::lm.fit(Z, esq)$residuals
  sst <- sum(esq^2) - sum(esq)^2 / n
  statistic <- as.double(n * (1 - sum(e_aux^2) / sst))
  list(statistic = statistic,
       p.value = stats::pchisq(statistic, df = ncol(Z) - 1, lower.tail = FALSE))
}

MLRSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "MLRSidebar",
      useShinyjs(),
      withMathJax(
        helpText("Only numeric variables are selectable."),
        pickerInput(
          ns("responseVariable"),
          strong("Response Variable (\\(y\\))"),
          choices = NULL,
          multiple = FALSE,
          options = list(
            `live-search` = TRUE,
            title = "Nothing selected"
          )
        ),

        helpText("Select two or more explanatory variables (numeric)."),
        uiOutput(ns("singleOrMultipleHelpText")),
        pickerInput(
          inputId = ns("explanatoryVariables"),
          label   = HTML("<strong>Explanatory Variables (\\(x_1, x_2, x_3...x_n\\)) </strong>"),
          choices  = NULL,
          multiple = TRUE,
          options  = list(
            `actions-box`       = TRUE,
            `live-search`       = TRUE,
            selectedTextFormat  = "values",
            multipleSeperator   = ", "
          )
        ),
        actionButton(ns("calculate"), "Calculate", class = "act-btn"),
        actionButton(ns("reset"), "Reset Values", class = "act-btn")
      )))
}

MLRMainPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        #linear-model-equations mjx-container[display='true'] {
          display: flex !important;
          justify-content: flex-start !important;
          text-align: left !important;
          padding-left: 0px !important;
          margin-left: 0px !important;
        }
      "))
    ),
    uiOutput(ns("noFileWarning")),
    uiOutput(ns("mlrResponseWarn")),
    uiOutput(ns("mlrExplanatoryWarn")),
    hidden(div(id = ns("mlrNavPanel"),
      navbarPage(title = NULL,
                 tabPanel(
                   title = "Data",
                   value = "data_tab",
                   br(),
                   div(style = "overflow-x: auto;", DTOutput(ns("uploadedDataTable"))),
                   br()
                 ),
                 tabPanel(title = "Model", uiOutput(ns("Equations")) ),
                 tabPanel(title = "Inference", uiOutput(ns("ANOVAAndInference"))),
                 id = ns("mainPanel"),
                 theme = bs_theme(version = 4))
    ))
  )
}

MLRServer <- function(id, reg_data, reset_upload, upload_error = NULL, clear_trigger = NULL, hide_shared = NULL, is_active = NULL) {
  moduleServer(id, function(input, output, session) {

    encodedData <- reactiveVal(NULL)

    ## Names of the numeric columns of a data set (the only selectable ones).
    mlrNumericCols <- function(dat) names(dat)[vapply(dat, is.numeric, logical(1))]

    ## The scatterplot matrix is a p x p grid of panels drawn on the shared R
    ## process and is fixed at 550 x 550 px, so it is limited to this many
    ## variables and rows (the correlation matrix and the VIFs use everything).
    mlrScatmatMaxVars <- 10L
    mlrScatmatMaxRows <- 5000L

    # ============================================================
    # State of the results
    # ============================================================
    noFileCalculate    <- reactiveVal(FALSE)
    mlrResponseWarn    <- reactiveVal(FALSE)
    mlrExplanatoryWarn <- reactiveVal(FALSE)

    # TRUE from a Calculate with a usable selection until the data are replaced
    # or cleared. Nothing is fitted before that: the results panel is hidden.
    mlrCalculated <- reactiveVal(FALSE)
    # Variables the results are computed for (list(resp, expl)), or NULL. It
    # follows the pickers while mlrCalculated() is TRUE, but it is a reactiveVal
    # so that the outputs only recompute when the selection really changes.
    mlrSpec    <- reactiveVal(NULL)
    # Why the current selection cannot be fitted (shown in place of the
    # results), or NULL.
    mlrProblem <- reactiveVal(NULL)
    # Bumped whenever the results are discarded, so that the Model and Inference
    # tabs start again from their first tab on the next Calculate.
    mlrEpoch   <- reactiveVal(0)
    # The removed-predictor notice that was last shown (see mlrPredictors).
    mlrNoticeKey <- ""

    # ============================================================
    # LINE Assumption Tests Config
    # ============================================================
    mlrLineTestConfig <- list(

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
          wt <- mlrWhiteTest(model)
          list(statistic = round(wt$statistic, 4), p_value = round(wt$p.value, 4), note = NULL)
        }
      )
    )

    ns <- session$ns

    # ============================================================
    # Data and pickers
    # ============================================================

    ## Discards the results of an earlier Calculate and collapses the
    ## Data/Model/Inference tabs until Calculate is run again.
    mlr_clear_results <- function() {
      hide("mlrNavPanel")
      mlrCalculated(FALSE)
      mlrSpec(NULL)
      mlrProblem(NULL)
      mlrNoticeKey <<- ""
      mlrEpoch(isolate(mlrEpoch()) + 1)
    }

    ## TRUE from the moment MLR is selected again until the observer below has
    ## run. The parent then fires it again for the current data, which must
    ## start from empty pickers, as a newly created module did.
    mlrReselected <- FALSE
    if (!is.null(is_active)) {
      observeEvent(is_active(), {
        if (isTRUE(is_active())) mlrReselected <<- TRUE
      }, ignoreInit = TRUE, priority = 10)
    }

    ## Runs when the data change, and also when they are cleared (reg_data()
    ## becomes NULL), so that no stale columns or results are left behind.
    observeEvent(reg_data(), {
      mlr_clear_results()
      mlrResponseWarn(FALSE)
      mlrExplanatoryWarn(FALSE)
      noFileCalculate(FALSE)
      dat <- reg_data()
      # The identical data chosen again (the same file uploaded once more): the
      # results are discarded but the variable selection is kept.
      sameData <- !mlrReselected && !is.null(dat) && identical(dat, encodedData())
      mlrReselected <<- FALSE
      encodedData(dat)
      if (sameData) return()
      cols <- if (is.null(dat)) character(0) else mlrNumericCols(dat)

      updatePickerInput(
        session,
        inputId  = "responseVariable",
        choices  = cols,
        selected = character(0)
      )

      # populate the explanatory-variables picker
      updatePickerInput(
        session,
        inputId = "explanatoryVariables",
        choices = cols
      )
    }, ignoreNULL = FALSE)

    mlr_do_reset <- function() {
      mlrResponseWarn(FALSE)
      mlrExplanatoryWarn(FALSE)
      if (!is.null(hide_shared)) hide_shared(FALSE)
      mlr_clear_results()
      dat <- encodedData()
      cols <- if (is.null(dat)) character(0) else mlrNumericCols(dat)
      updatePickerInput(session, "responseVariable", selected = character(0))
      # the explanatory picker leaves out the response variable that was chosen
      # last; restore every numeric column
      updatePickerInput(session, "explanatoryVariables", choices = cols, selected = character(0))
      noFileCalculate(FALSE)
    }

    observeEvent(input$reset, {
      reset_upload()
      mlr_do_reset()
    })

    if (!is.null(clear_trigger)) {
      observeEvent(clear_trigger(), { mlr_do_reset() }, ignoreInit = TRUE)
    }

    observeEvent(input$responseVariable, {
      # only run when we have data and a non-empty response variable
      req(encodedData(), input$responseVariable)

      # the response variable is not offered as an explanatory variable
      choices <- setdiff(mlrNumericCols(encodedData()), input$responseVariable)

      # if the responseVar was in the explanatory selection, remove it
      selected_vars <- setdiff(input$explanatoryVariables, input$responseVariable)

      # update the widget
      updatePickerInput(
        session,
        "explanatoryVariables",
        choices    = choices,
        selected   = selected_vars
      )
    })

    output$singleOrMultipleHelpText <- renderUI({
      if (length(input$explanatoryVariables) > 1) {
        div(class = "text-success", span("Multiple explanatory variables result in a multiple linear regression."))
      } else if (length(input$explanatoryVariables) == 1) {
        div(class = "text-danger", span("Select at least one more explanatory variable; a single explanatory variable results in a simple linear regression."))
      }
    })

    # ============================================================
    # Selection check, Calculate and warnings
    # ============================================================

    ## Checks the pickers against the data once Calculate has been pressed.
    ## Returns list(spec, problem): the variables to model, or the message
    ## explaining why the selection cannot be fitted.
    mlrSelection <- reactive({
      req(mlrCalculated())
      dat  <- encodedData()
      resp <- input$responseVariable
      # the response variable is never an explanatory variable (the picker
      # drops it, but only after a round trip to the browser)
      expl <- setdiff(as.character(input$explanatoryVariables), resp)

      msgs <- character(0)
      if (is.null(dat)) {
        msgs <- "Upload some data."
      } else {
        if (!isTruthy(resp))
          msgs <- c(msgs, "A response variable is required.")
        if (!(isTruthy(resp) && length(resp) == 1 && nzchar(resp[1]) &&
              (resp[1] %in% names(dat)) && is.numeric(dat[[resp[1]]])))
          msgs <- c(msgs, "Response variable must be an existing, single, and numeric column.")
        if (!isTruthy(expl))
          msgs <- c(msgs, "Explanatory variables are required.")
        if (length(expl) < 2)
          msgs <- c(msgs, "Two or more explanatory variables must be selected.")
        nonNumeric <- expl[!vapply(expl, function(v) is.numeric(dat[[v]]), logical(1))]
        if (length(nonNumeric) > 0)
          msgs <- c(msgs, sprintf("All explanatory variables must be numeric. These variables are non-numeric: %s.",
                                  paste(nonNumeric, collapse = ", ")))
      }

      if (length(msgs) > 0) {
        list(spec = NULL, problem = paste(msgs, collapse = "\n"))
      } else {
        list(spec = list(resp = resp, expl = expl), problem = NULL)
      }
    })

    observe({
      req(mlrCalculated())
      sel <- mlrSelection()
      mlrSpec(sel$spec)
      mlrProblem(sel$problem)
    })

    ## Shows the reason the selection cannot be fitted instead of the results.
    mlrValidateOutput <- function() {
      msg <- mlrProblem()
      if (!is.null(msg)) validate(need(FALSE, msg))
    }

    # Clear main-panel warnings when variables are selected
    observeEvent(input$responseVariable, {
      if (isTruthy(input$responseVariable)) mlrResponseWarn(FALSE)
    })

    observeEvent(input$explanatoryVariables, {
      if (length(input$explanatoryVariables) >= 2) mlrExplanatoryWarn(FALSE)
    })

    observeEvent(input$calculate, {
      tryCatch({
        if (!isTruthy(encodedData())) {
          noFileCalculate(TRUE)
          if (!is.null(upload_error)) upload_error(TRUE)
          return()
        } else {
          noFileCalculate(FALSE)
          if (!is.null(upload_error)) upload_error(FALSE)
        }

        # Validate response and explanatory variables
        hasResponseVar    <- isTruthy(input$responseVariable)
        hasExplanatoryVars <- length(setdiff(input$explanatoryVariables, input$responseVariable)) >= 2

        # Only show tabs if validation passes
        if (!hasResponseVar || !hasExplanatoryVars) {
          mlrResponseWarn(!hasResponseVar)
          mlrExplanatoryWarn(!hasExplanatoryVars)
          hide("mlrNavPanel")
          mlrCalculated(FALSE)
          mlrSpec(NULL)
          mlrProblem(NULL)
          return()
        }
        mlrResponseWarn(FALSE)
        mlrExplanatoryWarn(FALSE)

        mlrCalculated(TRUE)
        show("mlrNavPanel")
        if (!is.null(hide_shared)) hide_shared(TRUE)
        updateNavbarPage(session, "mainPanel", selected = "Model")
      }, error = function(e) {
        # a req()/validate() stop is not a failure: leave it to Shiny
        if (inherits(e, "shiny.silent.error")) stop(e)
        showNotification(paste("Calculation failed:", conditionMessage(e)), type = "error")
      })
    })

    observe({
      if(isTruthy(encodedData())){
        noFileCalculate(FALSE)
        if (!is.null(upload_error)) upload_error(FALSE)
      }
    }) |> bindEvent(encodedData(), ignoreNULL = FALSE, ignoreInit = TRUE)

    output$noFileWarning <- renderUI({
      if (!noFileCalculate()) return(NULL)
      div(
        class = "alert alert-danger",
        style = "margin-top: 15px;",
        icon("triangle-exclamation"),
        strong(" Please Upload Data before calculating.")
      )
    })

    output$mlrResponseWarn <- renderUI({
      if (!mlrResponseWarn()) return(NULL)
      div(class = "alert alert-warning", style = "margin-top: 15px;",
          icon("triangle-exclamation"),
          strong(" Please select a Response Variable before calculating."))
    })

    output$mlrExplanatoryWarn <- renderUI({
      if (!mlrExplanatoryWarn()) return(NULL)
      div(class = "alert alert-warning", style = "margin-top: 15px;",
          icon("triangle-exclamation"),
          strong(" Please select at least two Explanatory Variables before calculating."))
    })

    # ============================================================
    # Shared model reactives (every output below reads these)
    # ============================================================

    ## The model fitted to the variables chosen at Calculate (and kept in step
    ## with the pickers afterwards). It is fitted once and shared by all outputs.
    mlrModel <- reactive({
      spec <- mlrSpec()
      req(spec)
      dat <- encodedData()
      req(dat)

      validate(need(
        any(complete.cases(dat[, c(spec$resp, spec$expl), drop = FALSE])),
        "None of the rows has values for all of the selected variables."
      ))

      fit <- tryCatch(
        lm(reformulate(sprintf("`%s`", spec$expl), sprintf("`%s`", spec$resp)), data = dat),
        error = function(e) e
      )
      validate(need(
        !inherits(fit, "error"),
        paste("The model could not be fitted:", if (inherits(fit, "error")) conditionMessage(fit))
      ))
      fit
    })

    mlrSummary <- reactive(summary(mlrModel()))

    mlrConfint <- reactive({
      as.data.frame(confint(mlrModel()))
    })

    ## ANOVA quantities shared by the F plot, the ANOVA table and the text.
    ## n is the number of observations the model was fitted to (rows with
    ## missing values are left out by lm).
    mlrAnova <- reactive({
      model <- mlrModel()

      anovaModel <- anova(model)
      ss_vals <- anovaModel[["Sum Sq"]]
      SSR <- sum(ss_vals[-length(ss_vals)])  # all but the residuals
      SSE <- ss_vals[length(ss_vals)]        # only the residuals

      # Use model rank for k to match ANOVA table
      k   <- model$rank - 1
      n   <- nobs(model)
      df2 <- n - k - 1

      validate(
        need(k >= 1, "The model has no explanatory terms that can be estimated."),
        need(df2 >= 1, "There are not enough complete observations to estimate the error variance (more observations than model parameters are needed).")
      )

      MSR <- SSR / k
      MSE <- SSE / df2
      F_stat <- MSR / MSE
      validate(need(!is.na(F_stat), "The F statistic cannot be computed (the response variable may be constant)."))

      list(
        model = model, anovaModel = anovaModel,
        SSR = SSR, SSE = SSE, SST = SSR + SSE,
        k = k, n = n, df2 = df2,
        MSR = MSR, MSE = MSE, F_stat = F_stat,
        p_val = pf(F_stat, k, df2, lower.tail = FALSE)
      )
    })

    ## Explanatory variables cleaned for the multicollinearity diagnostics.
    mlrPredictors <- reactive({
      spec <- mlrSpec()
      req(spec)
      dat <- encodedData()
      req(dat)

      df <- dat[, spec$expl, drop = FALSE]

      removed_zero_var <- character(0)
      removed_aliased  <- character(0)

      # --------------------------------------------------------
      # Remove zero variance columns
      # --------------------------------------------------------

      zero_var <- vapply(df, function(x) isTRUE(sd(x, na.rm = TRUE) == 0), logical(1))

      if(any(zero_var)) {

        removed_zero_var <- names(df)[zero_var]

        df <- df[, !zero_var, drop = FALSE]
      }

      # --------------------------------------------------------
      # Detect aliased / linearly dependent predictors
      # --------------------------------------------------------

      if (ncol(df) > 0) {

        model <- tryCatch(
          lm(reformulate(sprintf("`%s`", colnames(df)), sprintf("`%s`", spec$resp)), data = dat),
          error = function(e) e
        )
        validate(need(
          !inherits(model, "error"),
          paste("The multicollinearity diagnostics could not be computed:", if (inherits(model, "error")) conditionMessage(model))
        ))

        aliased <- alias(model)$Complete

        if(!is.null(aliased)) {

          removed_aliased <- gsub("^`|`$", "", rownames(aliased))

          df <- df[, !colnames(df) %in% removed_aliased, drop = FALSE]
        }
      }

      # --------------------------------------------------------
      # Notifications (once for each different set of removed variables, not
      # on every recompute)
      # --------------------------------------------------------

      notices <- c(
        if (length(removed_zero_var) > 0)
          paste("Removed zero variance variable(s):", paste(removed_zero_var, collapse = ", ")),
        if (length(removed_aliased) > 0)
          paste("Removed linearly dependent variable(s):", paste(removed_aliased, collapse = ", "))
      )
      noticeKey <- paste(notices, collapse = "\n")

      if (!identical(noticeKey, mlrNoticeKey)) {
        mlrNoticeKey <<- noticeKey
        for (notice in notices) showNotification(notice, type = "warning")
      }

      validate(
        need(
          ncol(df) >= 2,
          "Not enough valid explanatory variables remain after removing problematic predictors."
        )
      )

      df
    })

    # ============================================================
    # Model and Inference tabs
    # ============================================================

    output$Equations <- renderUI({
      mlrEpoch()
      mlrValidateOutput()

      fluidPage(
        ## NOTE: variables and equations are both in linearModelEquations.
        fluidRow(uiOutput(ns("linearModelEquations")))
      )
    })

    # Reactive coefficients and confidence intervals table

    output$linearModelCoefConfint <- renderTable(
      {
        modelCoefficients <-
          rownames_to_column(
            as.data.frame(mlrSummary()$coefficients),
            "Source"
          )

        # Rename "Pr(>|t|)" to "P-value"
        names(modelCoefficients)[names(modelCoefficients) == "Pr(>|t|)"] <- "P-value"

        # Get CIs, convert to data frame, and rename columns
        modelConfidenceIntervals <- mlrConfint()
        colnames(modelConfidenceIntervals) <- c("Lower 95% CI", "Upper 95% CI")
        modelConfidenceIntervals <- rownames_to_column(modelConfidenceIntervals, "Source")

        final_table <- dplyr::left_join(modelCoefficients,
                                        modelConfidenceIntervals,
                                        by = "Source"
        )

        tibble::column_to_rownames(final_table, var = "Source")
      },
      rownames = TRUE,
      na = "",
      striped = TRUE,
      align = "c",
      digits = 4
    )

    output$lmCoefConfintTableCaption <- renderUI({
      modelConfidenceIntervals <- mlrConfint()

      tagList(if (anyNA(modelConfidenceIntervals)) {
        p("Some model components (e.g. dependent variables) did not have confidence intervals or coefficients, and are dropped from the table.")
      } else {
        p("")
      })
    })

    output$mlrLineAssumptions <- renderUI({
      model <- mlrModel()

      alpha <- 0.05
      n     <- nrow(model.frame(model))

      results <- lapply(mlrLineTestConfig, function(cfg) {
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

        # a test that gives no p-value for these data (e.g. a perfect fit) is
        # reported as such
        if (!is.null(result$p_value) && is.na(result$p_value)) {
          result <- list(statistic = NULL, p_value = NULL,
                         note = "The test could not be computed for these data.")
        }

        pval_str <- if (!is.null(result$p_value)) as.character(result$p_value) else "—"

        conclusion <- if (!is.null(result$p_value)) {
          base <- if (result$p_value <= alpha) {
            paste0("Reject H₀ (p = ", result$p_value, " ≤ 0.05)")
          } else {
            paste0("Fail to reject H₀ (p = ", result$p_value, " > 0.05)")
          }
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
            Assumption = colDef(
              name     = "Assumption",
              minWidth = 200,
              style    = list(fontWeight = "bold")
            ),
            Procedure  = colDef(name = "Procedure",  minWidth = 180),
            `P-Value`  = colDef(name = "P-Value",    minWidth = 100, align = "center"),
            Conclusion = colDef(name = "Conclusion", minWidth = 250)
          )
        ),
        br()
      )
    })

    output$anovaFDistributionPlot <- renderPlot({
      a <- mlrAnova()

      f_crit <- qf(0.95, a$k, a$df2)

      anovaFPlot(round(a$F_stat, 4), round(f_crit, 4), a$k, a$df2)
    }, height = 400, width = 650)



    output$linearModelEquations <- renderUI({
      model <- mlrModel()
      spec  <- mlrSpec()

      withMathJax(
        div(
          id = "linear-model-equations",

          {
            all_coefs <- coef(model)
            names(all_coefs) <- gsub("^`|`$", "", names(all_coefs))

            intercept <- all_coefs["(Intercept)"]

            vars_definitions <- c()
            sym_terms <- c()
            num_terms <- c()

            for(i in seq_along(spec$expl)) {

              var_name <- spec$expl[i]

              val <- all_coefs[var_name]

              if (is.na(val)) {
                val <- all_coefs[paste0("`", var_name, "`")]
              }

              if(!is.na(val)) {

                # Escape LaTeX special characters
                safe_name <- var_name
                safe_name <- gsub("\\\\", "\\\\\\\\", safe_name)
                safe_name <- gsub("_", "\\\\_", safe_name)
                safe_name <- gsub("\\$", "\\\\\\$", safe_name)

                vars_definitions <- c(
                  vars_definitions,
                  sprintf("x_{%d} = \\text{%s}", i, safe_name)
                )

                sym_terms <- c(
                  sym_terms,
                  sprintf("\\hat{\\beta}_{%d}x_{%d}", i, i)
                )

                num_terms <- c(
                  num_terms,
                  sprintf("%.3fx_{%d}", val, i)
                )
              }
            }

            safe_response <- spec$resp
            safe_response <- gsub("\\\\", "\\\\\\\\", safe_response)
            safe_response <- gsub("_", "\\\\_", safe_response)
            safe_response <- gsub("\\$", "\\\\\\$", safe_response)

            vars_definition_latex <- paste0(
              "<div style='text-align:left;'>",
              "\\(",
              "y = \\text{", safe_response, "}",
              "\\)",
              "<br>",
              paste(
                paste0("\\(", vars_definitions, "\\)"),
                collapse = "<br>"
              ),
              "</div>"
            )

            sym_eq <- paste0(
              "<div style='text-align:left;'>",
              "\\(",
              "\\hat{y} = ",
              "\\hat{\\beta}_0 + ",
              paste(sym_terms, collapse = " + "),
              "\\)",
              "</div>"
            )

            num_eq <- paste0(
              "<div style='text-align:left;'>",
              "\\(",
              "\\hat{y} = ",
              round(intercept, 3),
              " + ",
              paste(num_terms, collapse = " + "),
              "\\)",
              "</div>"
            )

            sym_eq <- gsub("\\+ -", "- ", sym_eq)
            num_eq <- gsub("\\+ -", "- ", num_eq)

            tagList(
              p("The variables in the model are"),
              HTML(vars_definition_latex),

              br(),
              p("The estimated multiple linear regression equation is"),

              HTML(sym_eq),
              HTML(num_eq)
            )
          }
        )
      )
    })


    # Reactive ANOVA tab outputs
    output$anovaHypotheses <- renderUI({
      a <- mlrAnova()
      omitted <- nrow(encodedData()) - a$n

      withMathJax(
        p(strong("Analysis of Variance (ANOVA)")),
        p(
          r"{\( H_0: \beta_1 = \beta_2 = \cdots = \beta_k = 0\)}",
          br(),
          r"{\( H_a: \) At least one \(\beta_j\ne 0\), where \(j = 1, \cdots, k\).}"
        ),
        #br(),
        p(r"{\( \alpha = 0.05\ \)}"),
        #br(),
        p(
          sprintf(r"{\( n = %i \)}", a$n),
          br(),
          sprintf(r"{\( k = %i \)}", a$k)
        ),
        p(r"[where \(n\) is the sample size and \(k\) is the number of explanatory variables in the multiple regression model.]"),
        if (omitted > 0)
          p(class = "text-muted",
            sprintf("Note: rows with a missing value in the response or an explanatory variable are left out of the model (%s of %s rows).",
                    format(omitted, big.mark = ","), format(nrow(encodedData()), big.mark = ",")))
      )
    })

    output$anovaTable <- renderDT({
      a <- mlrAnova()

      SSR <- a$SSR
      SSE <- a$SSE
      SST <- a$SST
      k   <- a$k
      n   <- a$n
      MSR <- a$MSR
      MSE <- a$MSE
      F_stat <- a$F_stat
      p_val  <- a$p_val

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

    output$anovaPValueMethod <- renderUI({
      a <- mlrAnova()

      MSR <- a$MSR
      MSE <- a$MSE
      F_stat <- a$F_stat

      withMathJax(
        p(strong("Test Statistic:")),
        p(sprintf(
          r"{\(\displaystyle F = \frac{\text{MSR}}{\text{MSE}} = \frac{%s}{%s} = %0.2f \)}",
          fmt_sci_latex(MSR, 2), fmt_sci_latex(MSE, 2), F_stat
        )),
        p(strong("Conclusion:")),
        {
          pValue <- a$p_val
          p(sprintf(
            r"[Since the p-value is %s than \(\alpha\) (\(%0.3f %s 0.05\)), %s.]",
            if (pValue <= 0.05) "less" else "greater",
            pValue,
            if (pValue <= 0.05) r"[\le]" else r"[>]",
            if (pValue <= 0.05) {
              r"[we reject the null hypothesis (\(H_0\)) and conclude there is enough statistical evidence to support the alternative hypothesis (\(H_a\))]"
            } else {
              r"[we do not reject the null hypothesis (\(H_0\)) and conclude there isn't enough statistical evidence to support the alternative hypothesis (\(H_a\)).]"
            }
          ))
        }
      )
    })

    output$rsquareAdjustedRSquareInterpretation <- renderUI({
      a <- mlrAnova()
      model <- a$model
      adj_r2 <- mlrSummary()$adj.r.squared

      SSR    <- a$SSR
      SSE    <- a$SSE
      SST    <- a$SST
      df_res <- a$anovaModel$Df[nrow(a$anovaModel)]   # n - k - 1
      mse    <- SSE / df_res
      rse    <- sqrt(mse)

      withMathJax(
        p(strong(r"{ \(R^2\) and Adjusted \(R^2\) :}")),
        br(),
        p(sprintf(
          r"[\( \displaystyle R^2 = \frac{\text{SSR}}{\text{SST}} = \frac{%s}{%s} = %0.4f\)]",
          fmt_sci_latex(SSR, 4), fmt_sci_latex(SST, 4),
          SSR / SST
        )),
        p(sprintf(
          r"{
\(
\displaystyle
R^2_{\text{adj}} = 1 - \left[ \left( 1-R^2 \right) \frac{n-1}{n-k-1} \right] = %0.4f
\)
}",
          adj_r2
        )),
        p(
          strong("Interpretation:"),
          sprintf(
            r"[Approximately \(%.2f\%%\) of the variation in the response variable is explained by the multiple linear regression model when adjusted for the number of explanatory variables and the sample size.]",
            adj_r2 * 100
          )
        ),
        br(),
        p(strong("Residual Standard Error (RSE):")),
        p(sprintf(
          r"[\( \displaystyle RSE = \sqrt{\frac{\mathrm{SSE}}{n-k-1}} = \sqrt{\mathrm{MSE}} = \sqrt{%s} = %0.4f \)]",
          fmt_sci_latex(mse, 4), rse
        )),
        br(),
        p(strong("Akaike Information Criteria (AIC):")),
        p(sprintf(r"[AIC = \(%0.4f\)]", AIC(model))),
        br(),
        p(strong("Bayesian Information Criteria (BIC):")),
        p(sprintf(r"[BIC = \(%0.4f\)]", BIC(model))),
        br(),
        p(strong("Mallows' Cp:")),
        p(sprintf(r"[Cp = \(%0.4f\)]", olsrr::ols_mallows_cp(model, model))),
        br()
      )
    })

    # Reactive Multicollinearity Detection tab outputs
    output$simpleCorrelationMatrix <- renderTable({

      predictors <- mlrPredictors()

      tryCatch(
        corsetAboveDiagNA(predictors),
        error = function(e) validate(need(FALSE, paste("The correlation matrix could not be computed:", conditionMessage(e))))
      )

    },
    rownames = TRUE,
    striped = TRUE,
    na = "",
    align = "c")

    output$vifs <- renderTable({

      spec <- mlrSpec()
      predictors <- mlrPredictors()

      clean_df <- cbind(
        encodedData()[spec$resp],
        predictors
      )

      model <- lm(
        reformulate(
          sprintf("`%s`", colnames(predictors)),
          sprintf("`%s`", spec$resp)
        ),
        data = clean_df
      )

      as.data.frame(car::vif(model))

    },
    rownames = TRUE,
    align = "c")

    ## What the scatterplot matrix shows: at most mlrScatmatMaxVars variables
    ## and mlrScatmatMaxRows (evenly spaced) rows, so that it stays quick to draw.
    mlrScatmatInput <- reactive({
      predictors <- mlrPredictors()

      vars <- head(colnames(predictors), mlrScatmatMaxVars)
      rows <- if (nrow(predictors) > mlrScatmatMaxRows)
        round(seq(1, nrow(predictors), length.out = mlrScatmatMaxRows))
      else seq_len(nrow(predictors))

      list(
        data = predictors[rows, vars, drop = FALSE],
        vars = length(vars), allVars = ncol(predictors),
        rows = length(rows), allRows = nrow(predictors)
      )
    })

    output$ggscatmat <- renderPlot({
      s <- mlrScatmatInput()

      GGally::ggscatmat(
        s$data,
        columns = colnames(s$data)
      )
    })

    output$ggscatmatNote <- renderUI({
      s <- mlrScatmatInput()

      parts <- c(
        if (s$vars < s$allVars)
          sprintf("the first %d of the %d explanatory variables", s$vars, s$allVars),
        if (s$rows < s$allRows)
          sprintf("%s evenly spaced rows of the %s",
                  format(s$rows, big.mark = ","), format(s$allRows, big.mark = ","))
      )
      if (length(parts) == 0) return(NULL)

      p(class = "text-muted", style = "font-size: 0.9em;",
        paste0("To keep this plot quick to draw, it shows ", paste(parts, collapse = " and "),
               ". The correlation matrix above and the variance inflation factors below use all of them."))
    })

    ## The diagnostic plots need residual degrees of freedom.
    mlrDiagnosticModel <- reactive({
      model <- mlrModel()
      validate(need(
        model$df.residual >= 1,
        "There are not enough complete observations to draw the diagnostic plots (more observations than model parameters are needed)."
      ))
      model
    })

    # Reactive Diagnostic Plots tab outputs
    output$mlrResidualsPanelPlot1 <- renderPlot({
      model <- mlrDiagnosticModel()
      plot(model, which = 1, pch = 20, main = "", lwd = 2, sub.caption = "")
    })

    output$mlrResidualsPanelPlot2 <- renderPlot({
      model <- mlrDiagnosticModel()
      plot(model, which = 2, pch = 20, main = "", lwd = 2, sub.caption = "")
    })

    output$mlrResidualsPanelPlot3 <- renderPlot({
      model <- mlrDiagnosticModel()
      plot(model, which = 3, pch = 20, main = "", lwd = 2, sub.caption = "")
    })

    output$mlrResidualsPanelPlot4 <- renderPlot({
      model <- mlrDiagnosticModel()
      plot(model, which = 5, pch = 20, main = "", lwd = 2, sub.caption = "")
    })

    output$mlrResidualsPanelPlot5 <- renderPlot({
      model <- mlrDiagnosticModel()
      par(font.main = 2, font.lab = 2)
      hist(residuals(model), main = "", xlab = "",
           col = "darkgreen", border = "white")
      title(main = "Histogram of Residuals", cex.main = 1.2)
      title(xlab = expression(bold(Residuals~plain("(")*italic(e)*plain(")"))))
    })

    # Reactive Uploaded Data tab output
    output$uploadedDataTable <- renderDT({
      # Re-rendered when Calculate shows the results panel: an update sent in the
      # same flush that hides the panel, as a new upload does, is dropped by the
      # browser and the Data tab would keep the old file.
      mlrCalculated()
      req(reg_data())
      datatable(reg_data(),
                options = list(pageLength = 25,
                               lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All"))))
    })

    # NEW MERGED ANOVA AND INFERENCE TAB

    ## The tabset is built from the validity of the selection only, not from
    ## the selection itself, so that changing a variable does not rebuild it
    ## (and throw the user back to the first sub-tab).
    output$ANOVAAndInference <- renderUI({
      mlrEpoch()
      mlrValidateOutput()

      fluidPage(
        tags$style(HTML("
      .correlation-tabs .nav-tabs {
        border-bottom: none;
        background-color: #f8f9fa;
        display: flex;
        padding: 0;
        margin-bottom: 16px;
      }
      .correlation-tabs .nav-tabs > li > a {
        color: #18536F;
        font-weight: bold;
        font-size: 15px;
        border: none !important;
        border-radius: 0 !important;
        padding: 10px 24px;
        background-color: #f8f9fa !important;
      }
      .correlation-tabs .nav-tabs > li.active > a,
      .correlation-tabs .nav-tabs > li.active > a:focus,
      .correlation-tabs .nav-tabs > li.active > a:hover,
      .correlation-tabs .nav-tabs > li > a.active,
      .correlation-tabs .nav-tabs > li > a.active:focus,
      .correlation-tabs .nav-tabs > li > a.active:hover {
        background-color: #18536F !important;
        color: white !important;
        border: none !important;
        border-radius: 0 !important;
        font-weight: bold !important;
      }
      .correlation-tabs .nav-tabs > li > a:hover {
        background-color: #d0dce8 !important;
        color: #1a3a5c !important;
      }
    ")),
    div(
      class = "correlation-tabs",
      tabsetPanel(
        tabPanel(
          title = "Parameter Estimates",
          br(),
          fluidRow(column(
            12,
            p(strong("Coefficients and Confidence Intervals")),
            tableOutput(ns("linearModelCoefConfint")),
            uiOutput(ns("lmCoefConfintTableCaption"))
          ))
        ),
        tabPanel(
          title = "ANOVA",
          br(),
          fluidRow(uiOutput(ns("anovaHypotheses"))),
          br(),
          p(strong("ANOVA Table:")),
          fluidRow(DTOutput(ns("anovaTable"))),
          br(),
          fluidRow(
            column(12,
                   p(strong("F Distribution")),
                   p("The shaded region represents the rejection region at α = 0.05. The dashed red line is the observed F statistic and the dashed blue line is the critical value."),
                   plotOutput(ns("anovaFDistributionPlot"))
            )
          ),
          br(),
          fluidRow(uiOutput(ns("anovaPValueMethod"))),
          br(),
          fluidRow(uiOutput(ns("rsquareAdjustedRSquareInterpretation"))),
          br()
        ),

        tabPanel(
          title = "LINE",
          br(),
          uiOutput(ns("mlrLineAssumptions"))
        ),
        tabPanel(
          title = "Multicollinearity Detection",
          br(),
          fluidRow(column(
            12, p(strong("Correlation matrix")),
            p("Correlation values below –0.75 or above 0.75 indicate strong linear association between explanatory variables. Such strong correlations may signal potential multicollinearity. Consider whether one of the correlated variables can be removed, combined, or otherwise addressed."),
            tableOutput(ns("simpleCorrelationMatrix"))
          )),
          fluidRow(column(
            12, p(strong("Graphical methods")),
            p("Along the diagonal of this plot are the distributions of the data in each variable. Below the diagonal are the scatterplots of one variable against another; if the points form a more-or-less straight line then the variables are correlated. Above the diagonal are the correlation coefficients between two variables."),

            div(
              style = "text-align:left;",
              plotOutput(ns("ggscatmat"), width = "550px", height = "550px"),
              uiOutput(ns("ggscatmatNote"))
            )
          )),
          br(),
          fluidRow(column(
            12, p(strong("Variance Inflation Factors (VIFs)")),
            p("A VIF greater than 10 suggests strong multicollinearity caused by the respective variable with that variance inflation factor. VIFs between 5 and 10 hint at moderate multicollinearity. Values less than 5 are acceptable, with only a low degree of multicollinearity detected."),
            tableOutput(ns("vifs"))
          ))
        ),
        tabPanel(
          title = "Diagnostic Plots",
          fluidPage(
            br(),
            fluidRow(
              column(12,
                plotOutput(ns("mlrResidualsPanelPlot1")),
                br(),
                plotOutput(ns("mlrResidualsPanelPlot2")),
                br(),
                plotOutput(ns("mlrResidualsPanelPlot3")),
                br(),
                plotOutput(ns("mlrResidualsPanelPlot4")),
                br(),
                plotOutput(ns("mlrResidualsPanelPlot5")),
                br()
              )
            )
          )
        ),
      )
    )
      )
    })

  })
}
