# R/xgboost.R

# ============== UI ==============

XGBSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    sliderInput(
      ns("split"),
      label = strong("Train/Test Split (%)"),
      min = 50, max = 90, value = 80, step = 1
    ),

    numericInput(
      ns("nrounds"),
      strong("Number of Boosting Rounds"),
      value = 100, min = 1, step = 10
    ),

    numericInput(
      ns("max_depth"),
      strong("Max Tree Depth"),
      value = 6, min = 1, step = 1
    ),

    numericInput(
      ns("eta"),
      strong(HTML("Learning Rate (&eta;)")),
      value = 0.3, min = 0.01, max = 1, step = 0.01
    ),

    numericInput(
      ns("subsample"),
      strong("Subsample Ratio"),
      value = 1, min = 0.1, max = 1, step = 0.05
    ),

    numericInput(
      ns("colsample_bytree"),
      strong("Column Sample per Tree"),
      value = 1, min = 0.1, max = 1, step = 0.05
    ),

    div(
      style = "font-size: 15px; color: #6c757d; margin-top: 8px; margin-bottom: 6px;",
      "Select a categorical variable, must have 2 or more unique categories."
    ),

    div(
      id = ns("responseWrapper"),
      pickerInput(
        ns("response"),
        strong("Response Variable (Class)"),
        choices = NULL,
        multiple = TRUE,
        options = list(`live-search` = TRUE, title = "Nothing selected", `max-options` = 1)
      ),
      uiOutput(ns("responseError")),
      uiOutput(ns("responseContinuousWarning"))
    ),

    div(
      id = ns("predictorsWrapper"),
      pickerInput(
        ns("predictors"),
        strong(HTML("Explanatory Variables (<em>x</em><sub>1</sub>, <em>x</em><sub>2</sub>, ..., <em>x</em><sub>k</sub>)")),
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Nothing selected")
      ),
      uiOutput(ns("predictorsError"))
    ),

    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"),     "Reset Values", class = "act-btn")
  )
}

XGBMainPanelUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    suppressWarnings(tippy::use_tippy()),
    navbarPage(
      title = NULL,

      tabPanel(
        title = "Results",
        value = "model_summary_tab",
        uiOutput(ns("modelSummaryContainer"))
      ),

      tabPanel(
        title = "Plots",
        value = "plots_tab",
        uiOutput(ns("xgbVarImpContainer"))
      ),

      tabPanel(
        title = "Uploaded Data",
        value = "uploaded_data_tab",
        uiOutput(ns("uploadedDataContainer"))
      ),

      id       = ns("xgbMainPanel"),
      selected = "uploaded_data_tab",
      theme    = bs_theme(version = 4)
    )
  )
}


# ============== SERVER ==============

XGBServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    # ---- Reactive state ----
    summary_ready           <- reactiveVal(FALSE)
    summary_ever_calculated <- reactiveVal(FALSE)
    calc_results            <- reactiveVal(NULL)
    xgb_message             <- reactiveVal(NULL)
    noFileCalculate         <- reactiveVal(FALSE)
    responseError           <- reactiveVal(FALSE)
    predictorsError         <- reactiveVal(FALSE)
    responseContinuous      <- reactiveVal(FALSE)

    # ---- Input validation ----
    xgb_iv <- shinyvalidate::InputValidator$new()
    xgb_iv$add_rule("nrounds",   shinyvalidate::sv_required())
    xgb_iv$add_rule("nrounds",   shinyvalidate::sv_gte(1, message = "Must be at least 1."))
    xgb_iv$add_rule("max_depth", shinyvalidate::sv_required())
    xgb_iv$add_rule("max_depth", shinyvalidate::sv_gte(1, message = "Must be at least 1."))
    xgb_iv$add_rule("eta", shinyvalidate::sv_required())
    xgb_iv$add_rule("eta", function(v) {
      if (!is.na(v) && (v <= 0 || v > 1)) "Must be between 0 (exclusive) and 1."
    })
    xgb_iv$add_rule("subsample", shinyvalidate::sv_required())
    xgb_iv$add_rule("subsample", function(v) {
      if (!is.na(v) && (v <= 0 || v > 1)) "Must be between 0 (exclusive) and 1."
    })
    xgb_iv$add_rule("colsample_bytree", shinyvalidate::sv_required())
    xgb_iv$add_rule("colsample_bytree", function(v) {
      if (!is.na(v) && (v <= 0 || v > 1)) "Must be between 0 (exclusive) and 1."
    })
    xgb_iv$enable()

    # ---- Hide tabs until Calculate succeeds ----
    session$onFlushed(function() {
      hideTab(inputId = "xgbMainPanel", target = "model_summary_tab")
      hideTab(inputId = "xgbMainPanel", target = "plots_tab")
    }, once = TRUE)

    # ---- Uploaded Data tab ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(helpText("No data yet. Upload a dataset in the Data Import tab to view it here."))
      } else {
        DT::DTOutput(session$ns("xgbUploadTable"))
      }
    })

    output$xgbUploadTable <- DT::renderDT({
      req(data())
      DT::datatable(
        data(),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX    = TRUE
        )
      )
    })

    # ---- Populate dropdowns after upload ----
    observeEvent(data(), {
      noFileCalculate(FALSE)
      req(data())

      df           <- data()
      cols         <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]

      pre_predictors <- intersect(shared_explanatory(), numeric_cols)
      shared_resp    <- shared_response()
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% cols) shared_resp else character(0)

      updatePickerInput(session, "response",   choices = cols,         selected = pre_response)
      updatePickerInput(session, "predictors", choices = numeric_cols, selected = pre_predictors)
    }, ignoreNULL = TRUE)

    # ---- Keep response out of predictors ----
    observeEvent(input$response, {
      shared_response(input$response)
      req(data())

      df           <- data()
      cols         <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]

      available    <- setdiff(numeric_cols, input$response)
      selected     <- intersect(input$predictors, available)
      updatePickerInput(session, "predictors", choices = available, selected = selected)

      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      responseContinuous(isTruthy(input$response) && ml_is_continuous_response(df[[input$response[1]]]))
    }, ignoreInit = TRUE)

    output$responseContinuousWarning <- renderUI({
      if (responseContinuous()) {
        div(
          class = "alert alert-warning",
          style = "font-size: 12px; margin-top: 4px; margin-bottom: 10px;",
          icon("triangle-exclamation"),
          ml_continuous_response_message
        )
      }
    })

    observeEvent(input$predictors, {
      shared_explanatory(input$predictors)
      if (length(input$predictors) >= 1) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    })

    # ---- Clear outputs when settings change ----
    observeEvent(
      list(data(), input$response, input$predictors, input$split,
           input$nrounds, input$max_depth, input$eta,
           input$subsample, input$colsample_bytree),
      {
        if (isTRUE(summary_ready())) summary_ready(FALSE)
        calc_results(NULL)
        xgb_message(NULL)
        hideTab(inputId = "xgbMainPanel", target = "model_summary_tab")
        hideTab(inputId = "xgbMainPanel", target = "plots_tab")
        updateNavbarPage(session, "xgbMainPanel", selected = "uploaded_data_tab")
      },
      ignoreInit = TRUE
    )

    # ---- Tab containers ----
    output$modelSummaryContainer <- renderUI({
      if (!isTRUE(summary_ready())) {
        if (!isTRUE(summary_ever_calculated())) {
          return(tagList(helpText("No results yet. Upload a dataset, choose variables, then click Calculate.")))
        }
        return(tagList(helpText("Settings changed. Click Calculate to update results.")))
      }
      uiOutput(session$ns("modelSummaryUI"))
    })

    # ---- Inline error outputs ----
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(class = "shiny-output-error-validation",
                 "Required: Cannot calculate without a data file.")
      } else {
        msg <- xgb_message()
        if (is.null(msg)) return(NULL)
        div(style = "margin-top:10px;", div(class = "alert alert-danger", msg))
      }
    })

    output$responseError <- renderUI({
      if (responseError()) {
        tags$div(class = "text-danger",
                 style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
                 icon("exclamation-circle"), "Please select a response variable.")
      }
    })

    output$predictorsError <- renderUI({
      if (predictorsError()) {
        tags$div(class = "text-danger",
                 style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
                 icon("exclamation-circle"), "Please select at least one explanatory variable.")
      }
    })

    # ---- Calculate ----
    observeEvent(input$calculate, {

      xgb_message(NULL)

      # 1. Data must be loaded
      if (!isTruthy(data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }

      # 2. Response variable
      if (!isTruthy(input$response)) {
        responseError(TRUE)
        shinyjs::addClass(id = "responseWrapper", class = "has-error")
      } else {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      # 3. Predictors
      if (!isTruthy(input$predictors) || length(input$predictors) < 1) {
        predictorsError(TRUE)
        shinyjs::addClass(id = "predictorsWrapper", class = "has-error")
      } else {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }

      if (!isTruthy(input$response) || !isTruthy(input$predictors) || length(input$predictors) < 1) return()

      # 4. Numeric input validation
      req(xgb_iv$is_valid())

      resp_col <- input$response[1]
      df       <- data()

      # Continuous response guard — block classification on a continuous variable
      if (ml_is_continuous_response(df[[resp_col]])) {
        responseContinuous(TRUE)
        return()
      }

      # 5. Drop incomplete rows
      analysis_df <- df[, c(input$predictors, resp_col), drop = FALSE]
      analysis_df <- na.omit(analysis_df)

      if (nrow(analysis_df) == 0) {
        xgb_message("No complete cases remain after removing missing values.")
        return()
      }

      analysis_df[[resp_col]] <- as.factor(analysis_df[[resp_col]])
      n_classes <- nlevels(analysis_df[[resp_col]])

      # 6. At least 2 classes
      if (n_classes < 2) {
        xgb_message("Response variable must have at least 2 unique classes.")
        return()
      }

      # 7. Not too many classes
      if (n_classes > floor(nrow(analysis_df) / 2)) {
        xgb_message("The selected response variable has too many unique values to be treated as a categorical class variable for XGBoost.")
        return()
      }

      # 8. Zero variance check
      sds <- sapply(analysis_df[, input$predictors, drop = FALSE], sd, na.rm = TRUE)
      zero_var <- names(sds)[is.na(sds) | sds == 0]
      if (length(zero_var) > 0) {
        xgb_message(paste0(
          "These variable(s) have zero variance and cannot be used: ",
          paste(zero_var, collapse = ", "), "."
        ))
        return()
      }

      # 9. Coerce predictors to double
      for (col in input$predictors) {
        analysis_df[[col]] <- as.double(analysis_df[[col]])
      }

      # 10. Train / test split
      n       <- nrow(analysis_df)
      n_train <- floor(n * (input$split / 100))

      if (n_train < 10) {
        xgb_message("Training set is too small. Try increasing the split percentage or using a larger dataset.")
        return()
      }

      set.seed(123)
      train_idx <- sample(seq_len(n), size = n_train)
      train_df  <- analysis_df[ train_idx, , drop = FALSE]
      test_df   <- analysis_df[-train_idx, , drop = FALSE]

      # 11. Class distribution (full dataset)
      class_dist_df <- as.data.frame(table(analysis_df[[resp_col]]))
      colnames(class_dist_df) <- c("Class", "Count")

      # 12. XGBoost requires 0-indexed integer labels
      class_levels    <- levels(analysis_df[[resp_col]])
      label_to_int    <- function(x) as.integer(factor(x, levels = class_levels)) - 1L

      xgb_label_train <- label_to_int(train_df[[resp_col]])
      xgb_label_test  <- label_to_int(test_df[[resp_col]])

      X_train <- as.matrix(train_df[, input$predictors, drop = FALSE])
      X_test  <- as.matrix(test_df[,  input$predictors, drop = FALSE])

      dtrain <- xgboost::xgb.DMatrix(data = X_train, label = xgb_label_train)
      dtest  <- xgboost::xgb.DMatrix(data = X_test)

      # 13. Set objective based on binary vs. multi-class
      if (n_classes == 2) {
        params <- list(
          objective        = "binary:logistic",
          eval_metric      = "error",
          max_depth        = as.integer(input$max_depth),
          eta              = as.numeric(input$eta),
          subsample        = as.numeric(input$subsample),
          colsample_bytree = as.numeric(input$colsample_bytree)
        )
      } else {
        params <- list(
          objective        = "multi:softmax",
          eval_metric      = "merror",
          num_class        = n_classes,
          max_depth        = as.integer(input$max_depth),
          eta              = as.numeric(input$eta),
          subsample        = as.numeric(input$subsample),
          colsample_bytree = as.numeric(input$colsample_bytree)
        )
      }

      # 14. Fit
      xgb_fit <- tryCatch(
        xgboost::xgb.train(
          params  = params,
          data    = dtrain,
          nrounds = as.integer(input$nrounds),
          verbose = 0
        ),
        error = function(e) {
          xgb_message(paste("XGBoost could not be computed:", e$message))
          NULL
        }
      )
      if (is.null(xgb_fit)) return()

      # 15. Predict on test set
      raw_pred <- tryCatch(
        predict(xgb_fit, dtest),
        error = function(e) {
          xgb_message(paste("Predictions could not be generated:", e$message))
          NULL
        }
      )
      if (is.null(raw_pred)) return()

      pred_int <- if (n_classes == 2) {
        as.integer(raw_pred > 0.5)
      } else {
        as.integer(raw_pred)   # multi:softmax returns 0-indexed class integers
      }

      pred_labels   <- factor(class_levels[pred_int + 1L], levels = class_levels)
      actual_labels <- factor(class_levels[xgb_label_test + 1L], levels = class_levels)

      # 16. Confusion matrix and accuracy
      confusion_mat <- table(Actual = actual_labels, Predicted = pred_labels)
      accuracy      <- sum(diag(confusion_mat)) / sum(confusion_mat)

      # 17. Per-class sensitivity and specificity
      classes <- rownames(confusion_mat)
      class_metrics_df <- do.call(rbind, lapply(classes, function(cls) {
        tp   <- confusion_mat[cls, cls]
        fn   <- sum(confusion_mat[cls, ]) - tp
        fp   <- sum(confusion_mat[, cls]) - tp
        tn   <- sum(confusion_mat) - tp - fn - fp
        sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
        spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
        data.frame(Class = cls,
                   Sensitivity = round(sens, 4),
                   Specificity = round(spec, 4),
                   stringsAsFactors = FALSE)
      }))

      # 18. Classification report (Precision / Recall / F1 / Support)
      xgb_class_report <- knn_classification_report(actual_labels, pred_labels)$report

      # 19. Feature importance (Gain)
      imp_mat <- tryCatch(
        xgboost::xgb.importance(feature_names = input$predictors, model = xgb_fit),
        error = function(e) NULL
      )
      importance_df <- if (!is.null(imp_mat) && nrow(imp_mat) > 0) {
        df_imp <- data.frame(
          Variable = imp_mat$Feature,
          Gain     = round(imp_mat$Gain,      4),
          Cover    = round(imp_mat$Cover,     4),
          Frequency = round(imp_mat$Frequency, 4),
          stringsAsFactors = FALSE
        )
        df_imp[order(df_imp$Gain, decreasing = FALSE), ]
      } else {
        data.frame(Variable = character(0), Gain = numeric(0),
                   Cover = numeric(0), Frequency = numeric(0))
      }

      res <- list(
        fit              = xgb_fit,
        train_df         = train_df,
        test_df          = test_df,
        response         = resp_col,
        predictors       = input$predictors,
        class_levels     = class_levels,
        class_dist       = class_dist_df,
        n_total          = n,
        n_train          = n_train,
        n_test           = n - n_train,
        split            = input$split,
        nrounds          = as.integer(input$nrounds),
        max_depth        = as.integer(input$max_depth),
        eta              = as.numeric(input$eta),
        subsample        = as.numeric(input$subsample),
        colsample_bytree = as.numeric(input$colsample_bytree),
        n_classes        = n_classes,
        confusion        = confusion_mat,
        accuracy         = accuracy,
        class_metrics    = class_metrics_df,
        class_report     = xgb_class_report,
        importance       = importance_df
      )

      calc_results(res)

      # ---- Model Summary UI ----
      output$modelSummaryUI <- renderUI({
        r       <- calc_results()
        req(r)
        correct <- sum(diag(r$confusion))
        total   <- sum(r$confusion)

        tagList(
          tags$h4("Model Summary"),
          tableOutput(session$ns("xgbModelInfo")),
          tags$hr(),

          tags$h4("Class Distribution (Full Dataset)"),
          tableOutput(session$ns("xgbClassDist")),
          tags$hr(),

          tags$h4("Classification Report (Test Set)"),
          tableOutput(session$ns("xgbClassReport")),
          tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),
          tags$hr(),

          tags$h4("Confusion Matrix (Test Set)"),
          tableOutput(session$ns("xgbConfusionTable")),
          tags$h5(tags$strong("Accuracy Calculation"),
                  style = "margin-top: 14px; margin-bottom: 2px;"),
          withMathJax(),
          tags$p(HTML(sprintf(
            "\\( \\text{Accuracy} = \\dfrac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\dfrac{%d}{%d} = %.2f\\%% \\)",
            correct, total, r$accuracy * 100
          ))),
          tags$script(HTML("if(window.MathJax){ MathJax.Hub ? MathJax.Hub.Queue(['Typeset',MathJax.Hub]) : MathJax.typesetPromise(); }")),
          tags$hr(),

          tags$h4("Per-Class Metrics"),
          tableOutput(session$ns("xgbClassMetrics")),
          tags$hr(),

          tags$div(
            style = paste(
              "background-color: #f8f9fa;",
              "border-left: 4px solid #dee2e6;",
              "border-radius: 4px;",
              "padding: 16px 20px;",
              "margin-top: 6px;"
            ),
            tags$h5(tags$strong("Interpretation of Results"),
                    style = "margin-top: 0; margin-bottom: 12px;"),
            tags$p(
              style = "margin-bottom: 8px;",
              paste0(
                "XGBoost is a gradient boosting algorithm that builds trees sequentially, ",
                "each one correcting the errors of the previous. ",
                "The model was trained on ", r$n_train, " observations and tested on ",
                r$n_test, " observations."
              )
            ),
            tags$p(
              style = "margin-bottom: 0;",
              paste0(
                "The model correctly classified ", round(r$accuracy * 100, 2),
                "% of observations in the test set. ",
                "Higher learning rates learn faster but may overfit; ",
                "lower rates are more robust but require more rounds."
              )
            )
          )
        )
      })

      # ---- Results renderTable calls ----
      output$xgbModelInfo <- renderTable({
        r <- calc_results()
        req(r)
        data.frame(
          Item = c(
            "Type",
            "Number of Boosting Rounds",
            "Max Tree Depth",
            "Learning Rate (η)",
            "Subsample Ratio",
            "Column Sample per Tree",
            "Number of Predictors",
            "Total Observations",
            "Training Observations",
            "Test Observations",
            "Train/Test Split",
            "Accuracy"
          ),
          Value = c(
            "Classification",
            as.character(r$nrounds),
            as.character(r$max_depth),
            as.character(r$eta),
            as.character(r$subsample),
            as.character(r$colsample_bytree),
            as.character(length(r$predictors)),
            as.character(r$n_total),
            as.character(r$n_train),
            as.character(r$n_test),
            paste0(r$split, "%"),
            sprintf("%.4f", r$accuracy)
          ),
          check.names = FALSE
        )
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$xgbClassDist <- renderTable({
        r <- calc_results()
        req(r)
        r$class_dist
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$xgbClassReport <- renderTable({
        r <- calc_results()
        req(r)
        r$class_report
      }, rownames = FALSE, striped = TRUE, bordered = TRUE,
         sanitize.colnames.function = function(x) {
           tips <- c(
             Precision = "Of all instances predicted as this class, the fraction that are truly this class. High precision means few false positives.",
             Recall    = "Of all actual instances of this class, the fraction correctly predicted. High recall means few false negatives.",
             F1        = "Harmonic mean of Precision and Recall — balances both into a single score.",
             Support   = "Number of actual instances of this class in the dataset."
           )
           sapply(x, function(col) {
             if (col %in% names(tips)) {
               paste0('<b><span data-tippy-content="', tips[[col]],
                      '" style="cursor:help;border-bottom:1px dotted #555;">', col, '</span></b>')
             } else {
               paste0("<b>", col, "</b>")
             }
           }, USE.NAMES = FALSE)
         })

      output$xgbConfusionTable <- renderTable({
        r <- calc_results()
        req(r)
        cm <- as.data.frame.matrix(r$confusion)
        cm$Actual <- paste0("<b>", rownames(cm), "</b>")
        cm <- cm[, c("Actual", setdiff(names(cm), "Actual"))]
        rownames(cm) <- NULL
        cm
      }, rownames = FALSE, striped = TRUE, bordered = TRUE,
         sanitize.text.function = identity,
         sanitize.colnames.function = function(x) {
           sapply(x, function(col) {
             if (col == "Actual") "<b>Actual \\ Predicted</b>" else paste0("<b>", col, "</b>")
           }, USE.NAMES = FALSE)
         })

      output$xgbClassMetrics <- renderTable({
        r <- calc_results()
        req(r)
        r$class_metrics
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      # ---- Plots ----
      output$xgbVarImpContainer <- renderUI({
        r <- calc_results()
        req(r)
        tagList(
          tags$h4("Variable Importance (Gain)", style = "margin-top: 10px;"),
          plotOutput(session$ns("xgbVarImpPlot"), height = "450px"),
          tags$div(
            style = "margin-top: 12px; font-size: 14px; color: #444;",
            tags$p(
              tags$strong("Gain: "),
              "The fractional contribution of each feature to the model based on the total improvement ",
              "in accuracy it brings to the splits it is used in. Higher gain = more important feature."
            ),
            tags$p(
              tags$strong("Cover: "),
              "The relative number of observations related to this feature. Higher cover = used on more data points."
            ),
            tags$p(
              tags$strong("Frequency: "),
              "The percentage of times the feature appears in trees across all boosting rounds."
            )
          )
        )
      })

      output$xgbVarImpPlot <- renderPlot({
        r <- calc_results()
        req(r)

        imp_df <- r$importance
        if (nrow(imp_df) == 0) {
          plot.new()
          text(0.5, 0.5, "No variable importance available.", cex = 0.9, col = "#555555")
          return()
        }

        max_chars <- max(nchar(imp_df$Variable), na.rm = TRUE)
        left_mar  <- max(4, ceiling(max_chars * 0.6))

        par(mar = c(5, left_mar, 4, 2))

        barplot(
          imp_df$Gain,
          names.arg = imp_df$Variable,
          horiz     = TRUE,
          las       = 1,
          col       = "#4472C4",
          main      = "Variable Importance (Gain)",
          xlab      = "Gain",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.names = 0.95
        )
      })

      # ---- Show tabs and navigate ----
      summary_ready(TRUE)
      summary_ever_calculated(TRUE)

      showTab(inputId = "xgbMainPanel", target = "model_summary_tab")
      showTab(inputId = "xgbMainPanel", target = "plots_tab")

      shinyjs::delay(100, {
        updateNavbarPage(session, "xgbMainPanel", selected = "model_summary_tab")
      })

    }, ignoreInit = TRUE)

    # ---- Reset ----
    observeEvent(input$reset, {
      hideTab(inputId = "xgbMainPanel", target = "model_summary_tab")
      hideTab(inputId = "xgbMainPanel", target = "plots_tab")

      summary_ready(FALSE)
      summary_ever_calculated(FALSE)
      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      responseContinuous(FALSE)
      xgb_message(NULL)
      calc_results(NULL)

      shinyjs::removeClass(id = "responseWrapper",   class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")

      updatePickerInput(session,  "response",   selected = character(0))
      updatePickerInput(session,  "predictors", selected = character(0))
      updateNumericInput(session, "nrounds",          value = 100)
      updateNumericInput(session, "max_depth",         value = 6)
      updateNumericInput(session, "eta",               value = 0.3)
      updateNumericInput(session, "subsample",         value = 1)
      updateNumericInput(session, "colsample_bytree",  value = 1)
      updateSliderInput(session,  "split",             value = 80)

      updateNavbarPage(session, "xgbMainPanel", selected = "uploaded_data_tab")
    })

  })
}
