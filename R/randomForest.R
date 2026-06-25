# R/randomForest.R

# ============== UI ==============

RFSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    sliderInput(
      ns("split"),
      label = strong("Train/Test split (%)"),
      min = 50, max = 90, value = 80, step = 1
    ),

    numericInput(
      ns("ntree"),
      strong("Number of Trees"),
      value = 500,
      min = 100,
      max = 2000,
      step = 50
    ),

    numericInput(
      ns("mtry"),
      strong("Variables per Split (leave blank for auto)"),
      value = NA,
      min = 1,
      step = 1
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

RFMainPanelUI <- function(id) {
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
        uiOutput(ns("rfVarImpContainer")),
        uiOutput(ns("rfPDPContainer")),
        uiOutput(ns("rfICEContainer")),
        uiOutput(ns("rfALEContainer"))
      ),

      tabPanel(
        title = "Uploaded Data",
        value = "uploaded_data_tab",
        uiOutput(ns("uploadedDataContainer"))
      ),

      id       = ns("rfMainPanel"),
      selected = "uploaded_data_tab",
      theme    = bs_theme(version = 4)
    )
  )
}


# ============== SERVER ==============

RFServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    # ---- Reactive state ----
    summary_ready <- reactiveVal(FALSE)

    summary_ever_calculated <- reactiveVal(FALSE)

    calc_results <- reactiveVal(NULL)
    rf_message   <- reactiveVal(NULL)

    noFileCalculate <- reactiveVal(FALSE)
    responseError   <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    responseContinuous <- reactiveVal(FALSE)

    # ---- Input validation (ntree) ----
    rf_iv <- shinyvalidate::InputValidator$new()
    rf_iv$add_rule("ntree", shinyvalidate::sv_required())
    rf_iv$add_rule("ntree", shinyvalidate::sv_gte(100, message = "Must be at least 100."))
    rf_iv$add_rule("ntree", function(value) {
      if (!is.na(value) && isTruthy(value) && value > 2000) "Must be at most 2000."
    })
    rf_iv$enable()

    # ---- Hide result tabs until Calculate succeeds ----
    session$onFlushed(function() {
      hideTab(inputId = "rfMainPanel", target = "model_summary_tab")
      hideTab(inputId = "rfMainPanel", target = "plots_tab")
    }, once = TRUE)

    # ---- Uploaded Data tab ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(helpText("No data yet. Upload a dataset in the Data Import tab to view it here."))
      } else {
        DT::DTOutput(session$ns("rfUploadTable"))
      }
    })

    output$rfUploadTable <- DT::renderDT({
      req(data())
      DT::datatable(
        data(),
        options = list(
          pageLength  = 25,
          lengthMenu  = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX     = TRUE
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

      n_rows         <- nrow(df)
      valid_response <- cols[sapply(cols, function(col) {
        n_uniq <- length(unique(na.omit(df[[col]])))
        n_uniq >= 2 && n_uniq <= floor(n_rows / 2)
      })]
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% valid_response) shared_resp else character(0)

      updatePickerInput(session, "response",   choices = valid_response, selected = pre_response)
      updatePickerInput(session, "predictors", choices = numeric_cols,   selected = pre_predictors)
    }, ignoreNULL = TRUE)

    # ---- Keep response out of predictors list ----
    observeEvent(input$response, {
      shared_response(input$response)
      req(data())

      df           <- data()
      cols         <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]

      available_predictors <- setdiff(numeric_cols, input$response)
      selected_predictors  <- intersect(input$predictors, available_predictors)
      updatePickerInput(session, "predictors",
                        choices  = available_predictors,
                        selected = selected_predictors)

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

    # ---- Clear outputs when any setting changes after Calculate ----
    observeEvent(
      list(data(), input$response, input$predictors, input$split, input$ntree, input$mtry),
      {
        if (isTRUE(summary_ready())) summary_ready(FALSE)

        calc_results(NULL)
        rf_message(NULL)

        hideTab(inputId = "rfMainPanel", target = "model_summary_tab")
        hideTab(inputId = "rfMainPanel", target = "plots_tab")
        updateNavbarPage(session, "rfMainPanel", selected = "uploaded_data_tab")
      },
      ignoreInit = TRUE
    )

    # ---- Tab containers (show placeholder until calculate succeeds) ----
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
        msg <- rf_message()
        if (is.null(msg)) return(NULL)
        div(
          style = "margin-top:10px;",
          div(class = "alert alert-danger", msg)
        )
      }
    })

    output$responseError <- renderUI({
      if (responseError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Please select a response variable."
        )
      }
    })

    output$predictorsError <- renderUI({
      if (predictorsError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Please select at least one explanatory variable."
        )
      }
    })

    # ---- Calculate ----
    observeEvent(input$calculate, {

      rf_message(NULL)

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

      if (!isTruthy(input$response) || !isTruthy(input$predictors) || length(input$predictors) < 1) {
        return()
      }

      # 4. Numeric inputs (ntree)
      req(rf_iv$is_valid())

      resp_col <- input$response[1]
      df       <- data()

      # Continuous response guard — block classification on a continuous variable
      if (ml_is_continuous_response(df[[resp_col]])) {
        responseContinuous(TRUE)
        return()
      }

      # 5. Check for missing values before fitting
      analysis_df <- df[, c(input$predictors, resp_col), drop = FALSE]

      na_cols <- names(which(sapply(analysis_df, function(x) any(is.na(x)))))
      if (length(na_cols) > 0) {
        rf_message(paste0(
          "The following column(s) contain missing values (NA): ",
          paste(na_cols, collapse = ", "),
          ". Please remove or impute missing values before calculating."
        ))
        return()
      }

      analysis_df <- na.omit(analysis_df)

      if (nrow(analysis_df) == 0) {
        rf_message("No complete cases remain after removing missing values.")
        return()
      }

      analysis_df[[resp_col]] <- as.factor(analysis_df[[resp_col]])
      n_classes <- nlevels(analysis_df[[resp_col]])

      # 6. At least 2 classes
      if (n_classes < 2) {
        rf_message("Response variable must have at least 2 unique classes.")
        return()
      }

      # 7. Not too many classes (same upper-limit check as CART)
      if (n_classes > floor(nrow(analysis_df) / 2)) {
        rf_message("The selected response variable has too many unique values to be treated as a categorical class variable for Random Forest.")
        return()
      }

      # 8. Zero variance check
      vars <- sapply(analysis_df[, input$predictors, drop = FALSE], var, na.rm = TRUE)
      zero_var_cols <- names(vars)[is.na(vars) | vars < .Machine$double.eps]
      if (length(zero_var_cols) > 0) {
        rf_message(paste0(
          "These selected variable(s) have zero variance and cannot be used in Random Forest: ",
          paste(zero_var_cols, collapse = ", "), "."
        ))
        return()
      }

      # 9. Coerce all predictor columns to canonical types before model fitting and iml.
      #    is.numeric() covers both double and integer in R, so every numeric-type column
      #    is unconditionally cast to double. Character columns become factors.
      for (col in input$predictors) {
        v <- analysis_df[[col]]
        if (is.numeric(v)) {
          analysis_df[[col]] <- as.double(v)
        } else if (is.character(v)) {
          analysis_df[[col]] <- as.factor(v)
        }
      }

      # 10. Resolve mtry — NA / blank means auto (NULL → randomForest uses floor(sqrt(p)))
      mtry_val <- if (!is.numeric(input$mtry) || is.na(input$mtry)) {
        NULL
      } else {
        as.integer(input$mtry)
      }

      if (!is.null(mtry_val) && mtry_val > length(input$predictors)) {
        rf_message(paste0(
          "mtry (", mtry_val, ") cannot exceed the number of selected predictors (",
          length(input$predictors), ")."
        ))
        return()
      }

      # 11. Train / test split
      n       <- nrow(analysis_df)
      n_train <- floor(n * (input$split / 100))

      if (n_train < 10) {
        rf_message("Training set is too small. Try increasing the split percentage or using a larger dataset.")
        return()
      }

      set.seed(123)
      train_idx <- sample(seq_len(n), size = n_train)
      train_df  <- analysis_df[ train_idx, , drop = FALSE]
      test_df   <- analysis_df[-train_idx, , drop = FALSE]

      # 10. Class distribution from full (cleaned) dataset
      class_dist_df <- as.data.frame(table(analysis_df[[resp_col]]))
      colnames(class_dist_df) <- c("Class", "Count")

      # 11. Build RF formula
      rf_formula <- as.formula(paste(
        paste0("`", resp_col, "`"),
        "~",
        paste(paste0("`", input$predictors, "`"), collapse = " + ")
      ))

      # 12. Fit Random Forest
      rf_args <- list(
        formula    = rf_formula,
        data       = train_df,
        ntree      = as.integer(input$ntree),
        importance = TRUE
      )
      if (!is.null(mtry_val)) rf_args$mtry <- mtry_val

      rf_fit <- tryCatch(
        do.call(randomForest::randomForest, rf_args),
        error = function(e) {
          rf_message(paste("Random Forest could not be computed:", e$message))
          NULL
        }
      )
      if (is.null(rf_fit)) return()

      # 13. Predict on test set
      test_pred <- tryCatch(
        predict(rf_fit, test_df[, input$predictors, drop = FALSE]),
        error = function(e) {
          rf_message(paste("Predictions could not be generated:", e$message))
          NULL
        }
      )
      if (is.null(test_pred)) return()

      # 14. Confusion matrix
      confusion_mat <- table(
        Actual    = test_df[[resp_col]],
        Predicted = test_pred
      )
      accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)

      # 15. Per-class sensitivity and specificity
      classes <- rownames(confusion_mat)
      class_metrics_df <- do.call(rbind, lapply(classes, function(cls) {
        tp  <- confusion_mat[cls, cls]
        fn  <- sum(confusion_mat[cls, ]) - tp
        fp  <- sum(confusion_mat[, cls]) - tp
        tn  <- sum(confusion_mat) - tp - fn - fp
        sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
        spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
        data.frame(
          Class       = cls,
          Sensitivity = round(sens, 4),
          Specificity = round(spec, 4),
          stringsAsFactors = FALSE
        )
      }))

      # 16. OOB error rate (final tree)
      oob_error <- rf_fit$err.rate[nrow(rf_fit$err.rate), "OOB"]

      # 17. Classification report (test set)
      rf_class_report <- knn_classification_report(test_df[[resp_col]], test_pred)$report

      # 18. PDP and ICE via iml — predictor built once, both methods computed in one pass
      iml_results <- tryCatch({
        predictor_iml <- suppressWarnings(
          iml::Predictor$new(
            model            = rf_fit,
            data             = train_df[, input$predictors, drop = FALSE],
            y                = train_df[[resp_col]],
            predict.function = function(model, newdata) predict(model, newdata, type = "prob")
          )
        )

        pdp_list <- suppressWarnings(lapply(input$predictors, function(pvar) {
          tryCatch(
            iml::FeatureEffect$new(predictor_iml, feature = pvar, method = "pdp")$results,
            error = function(e) NULL
          )
        }))
        names(pdp_list) <- input$predictors

        ice_list <- suppressWarnings(lapply(input$predictors, function(pvar) {
          tryCatch(
            iml::FeatureEffect$new(predictor_iml, feature = pvar, method = "ice")$results,
            error = function(e) NULL
          )
        }))
        names(ice_list) <- input$predictors

        ale_list <- suppressWarnings(lapply(input$predictors, function(pvar) {
          tryCatch(
            iml::FeatureEffect$new(predictor_iml, feature = pvar, method = "ale")$results,
            error = function(e) NULL
          )
        }))
        names(ale_list) <- input$predictors

        list(pdp = pdp_list, ice = ice_list, ale = ale_list)
      }, error = function(e) NULL)

      pdp_results <- if (!is.null(iml_results)) iml_results$pdp else NULL
      ice_results <- if (!is.null(iml_results)) iml_results$ice else NULL
      ale_results <- if (!is.null(iml_results)) iml_results$ale else NULL

      res <- list(
        fit           = rf_fit,
        train_df      = train_df,
        test_df       = test_df,
        response      = resp_col,
        predictors    = input$predictors,
        class_dist    = class_dist_df,
        n_total       = n,
        n_train       = n_train,
        n_test        = n - n_train,
        ntree         = as.integer(input$ntree),
        mtry_used     = rf_fit$mtry,
        oob_error     = oob_error,
        confusion     = confusion_mat,
        accuracy      = accuracy,
        class_metrics = class_metrics_df,
        class_report  = rf_class_report,
        pdp_results   = pdp_results,
        ice_results   = ice_results,
        ale_results   = ale_results
      )

      calc_results(res)

      # ---- Model Summary UI ----
      output$modelSummaryUI <- renderUI({
        r <- calc_results()
        req(r)

        oob_pct        <- round(r$oob_error * 100, 2)
        acc_pct        <- round(r$accuracy  * 100, 2)
        accuracy_label <- if (r$oob_error < 0.10) "high" else if (r$oob_error <= 0.25) "moderate" else "low"
        rf_correct     <- sum(diag(r$confusion))
        rf_total       <- sum(r$confusion)

        tagList(
          tags$h4("Model Summary"),
          uiOutput(session$ns("rfSummaryTable")),
          tags$hr(),
          tags$h4("Class Distribution (Full Dataset)"),
          tableOutput(session$ns("rfClassDistTable")),
          tags$hr(),
          tags$h4("Classification Report (Test Set)"),
          tableOutput(session$ns("rfClassReport")),
          tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),
          tags$hr(),
          tags$h4("Confusion Matrix (Test Set)"),
          tableOutput(session$ns("rfConfusionTable")),
          tags$h5(tags$strong("Accuracy Calculation"),
                  style = "margin-top: 14px; margin-bottom: 2px;"),
          withMathJax(),
          tags$p(HTML(sprintf(
            "\\( \\text{Accuracy} = \\dfrac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\dfrac{%d}{%d} = %.2f\\%% \\)",
            rf_correct, rf_total, r$accuracy * 100
          ))),
          tags$script(HTML("if(window.MathJax){ MathJax.Hub ? MathJax.Hub.Queue(['Typeset',MathJax.Hub]) : MathJax.typesetPromise(); }")),
          tags$hr(),
          tags$h4("Per-Class Metrics"),
          tableOutput(session$ns("rfClassMetrics")),
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
                "The OOB (Out-of-Bag) Error Rate is an internal estimate of prediction error ",
                "computed using observations not used in building individual trees. ",
                "An OOB error of ", oob_pct, "% indicates ", accuracy_label,
                " classification accuracy."
              )
            ),
            tags$p(
              style = "margin-bottom: 0;",
              paste0(
                "The model correctly classified ", acc_pct,
                "% of observations in the test set."
              )
            )
          )
        )
      })

      output$rfSummaryTable <- renderUI({
        r <- calc_results()
        req(r)

        tip <- function(label, tooltip) {
          tags$span(
            `data-tippy-content` = tooltip,
            style = "cursor: help; border-bottom: 1px dotted #555;",
            label
          )
        }

        rows <- list(
          list("Type",            "Classification"),
          list(
            tip("Number of Trees",
                "Total trees built in the forest. More trees improve stability but increase computation time."),
            as.character(r$ntree)
          ),
          list(
            tip("Variables per Split (mtry)",
                "Number of variables randomly considered at each tree split. Controls model diversity."),
            as.character(r$mtry_used)
          ),
          list("Number of Predictors",   as.character(length(r$predictors))),
          list("Total Observations",     as.character(r$n_total)),
          list("Training Observations",  as.character(r$n_train)),
          list("Test Observations",      as.character(r$n_test)),
          list(
            tip("OOB Error Rate",
                "Out-of-Bag error rate. An internal accuracy estimate using data not seen during training. Lower is better."),
            paste0(round(r$oob_error * 100, 2), "%")
          )
        )

        tagList(
          tags$table(
            class = "table table-sm table-bordered table-striped",
            style = "max-width: 480px;",
            tags$thead(
              tags$tr(
                tags$th("Item",  style = "width: 60%;"),
                tags$th("Value")
              )
            ),
            tags$tbody(
              lapply(rows, function(row) {
                tags$tr(tags$td(row[[1]]), tags$td(row[[2]]))
              })
            )
          ),
          tags$script("if (typeof tippy !== 'undefined') tippy('[data-tippy-content]');")
        )
      })

      output$rfClassDistTable <- renderTable({
        r <- calc_results()
        req(r)
        r$class_dist
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$rfClassReport <- renderTable({
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

      output$rfConfusionTable <- renderTable({
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

      output$rfClassMetrics <- renderTable({
        r <- calc_results()
        req(r)
        r$class_metrics
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$rfVarImpContainer <- renderUI({
        tagList(
          tags$h4("Variable Importance Plots", style = "margin-top: 10px;"),
          plotOutput(session$ns("rfVarImpPlot"), height = "550px"),
          tags$div(
            style = "margin-top: 12px; font-size: 14px; color: #444;",
            tags$p(
              tags$strong("Mean Decrease in Accuracy: "),
              "Measures how much the model's accuracy drops when the values of a variable are randomly shuffled. ",
              "A large drop means the model relied heavily on that variable — it is highly important."
            ),
            tags$p(
              tags$strong("Mean Decrease in Gini: "),
              "Measures how much each variable contributes to reducing impurity (disorder) across all splits in all trees. ",
              "A higher value means the variable is consistently useful for separating the classes."
            )
          )
        )
      })

      output$rfVarImpPlot <- renderPlot({
        r <- calc_results()
        req(r)

        imp_acc  <- randomForest::importance(r$fit, type = 1)
        imp_gini <- randomForest::importance(r$fit, type = 2)

        df_acc <- data.frame(
          Variable = rownames(imp_acc),
          Value    = imp_acc[, 1],
          stringsAsFactors = FALSE
        )
        df_acc <- df_acc[order(df_acc$Value, decreasing = FALSE), ]

        df_gini <- data.frame(
          Variable = rownames(imp_gini),
          Value    = imp_gini[, 1],
          stringsAsFactors = FALSE
        )
        df_gini <- df_gini[order(df_gini$Value, decreasing = FALSE), ]

        max_chars <- max(nchar(c(df_acc$Variable, df_gini$Variable)), na.rm = TRUE)
        left_mar  <- max(4, ceiling(max_chars * 0.6))

        par(
          mfrow = c(1, 2),
          oma   = c(0, 0, 0, 0),
          mar   = c(5, left_mar, 4, 2)
        )

        barplot(
          df_acc$Value,
          names.arg = df_acc$Variable,
          horiz     = TRUE,
          las       = 1,
          col       = "#4472C4",
          main      = "Mean Decrease in Accuracy",
          xlab      = "Mean Decrease in Accuracy",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.names = 0.95
        )

        barplot(
          df_gini$Value,
          names.arg = df_gini$Variable,
          horiz     = TRUE,
          las       = 1,
          col       = "#ED7D31",
          main      = "Mean Decrease in Gini Impurity",
          xlab      = "Mean Decrease in Gini",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.names = 0.95
        )
      })

      # Dynamic container: sets height based on predictor count then renders plotOutput
      output$rfPDPContainer <- renderUI({
        r <- calc_results()
        req(r, !is.null(r$pdp_results))

        n_pred    <- length(r$predictors)
        n_cols    <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
        n_rows    <- ceiling(n_pred / n_cols)
        height_px <- max(380, n_rows * 380)

        tagList(
          tags$hr(),
          tags$h4("Partial Dependence Plots", style = "margin-top: 10px;"),
          plotOutput(session$ns("rfPDPPlot"), height = paste0(height_px, "px"))
        )
      })

      output$rfPDPPlot <- renderPlot({
        r <- calc_results()
        req(r, !is.null(r$pdp_results))

        n_pred  <- length(r$predictors)
        n_cols  <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
        n_rows  <- ceiling(n_pred / n_cols)

        cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")

        par(
          mfrow = c(n_rows, n_cols),
          oma   = c(0, 0, 0, 0),
          mar   = c(8, 4, 5, 2)
        )

        for (pred_var in r$predictors) {
          pdp_df <- r$pdp_results[[pred_var]]

          if (is.null(pdp_df)) {
            plot.new()
            title(main = pred_var, cex.main = 1.1, font.main = 2)
            text(0.5, 0.5, "Could not compute PDP", cex = 0.9)
            next
          }

          cls_list <- as.character(unique(pdp_df$.class))

          plot(
            x    = NULL,
            xlim = range(pdp_df[[pred_var]], na.rm = TRUE),
            ylim = c(0, 1),
            main = pred_var,
            xlab = pred_var,
            ylab = "Predicted Probability",
            cex.main  = 1.3,
            font.main = 2,
            cex.lab   = 1.1,
            font.lab  = 2,
            cex.axis  = 1.0,
            bty       = "l"
          )

          for (j in seq_along(cls_list)) {
            cls_df <- pdp_df[as.character(pdp_df$.class) == cls_list[j], ]
            cls_df <- cls_df[order(cls_df[[pred_var]]), ]
            lines(cls_df[[pred_var]], cls_df$.value,
                  col = cls_cols[j], lwd = 2)
          }

          # Below the x-axis label, not just at the bottom of the plot area
          legend("bottom", legend = cls_list,
                 col = cls_cols[seq_along(cls_list)],
                 lwd = 2, bty = "n", cex = 0.8, horiz = TRUE,
                 xpd = TRUE, inset = c(0, -0.4))

          mtext("Shows the marginal effect of this variable on the predicted outcome,",
                side = 3, line = 0.9, cex = 0.65, col = "#555555")
          mtext("holding all other variables constant",
                side = 3, line = 0.2, cex = 0.65, col = "#555555")
        }

        # blank out remaining grid cells for odd predictor counts
        remaining <- n_rows * n_cols - n_pred
        for (k in seq_len(remaining)) plot.new()
      })

      output$rfICEContainer <- renderUI({
        r <- calc_results()
        req(r, !is.null(r$ice_results))

        n_pred    <- length(r$predictors)
        n_cols    <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
        n_rows    <- ceiling(n_pred / n_cols)
        height_px <- max(380, n_rows * 380)

        tagList(
          tags$hr(),
          tags$h4("Individual Conditional Expectation Plots", style = "margin-top: 10px;"),
          plotOutput(session$ns("rfICEPlot"), height = paste0(height_px, "px"))
        )
      })

      output$rfICEPlot <- renderPlot({
        r <- calc_results()
        req(r, !is.null(r$ice_results))

        n_pred  <- length(r$predictors)
        n_cols  <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
        n_rows  <- ceiling(n_pred / n_cols)

        cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")

        par(
          mfrow = c(n_rows, n_cols),
          oma   = c(0, 0, 0, 0),
          mar   = c(8, 4, 5, 2)
        )

        for (pred_var in r$predictors) {
          ice_df <- r$ice_results[[pred_var]]

          if (is.null(ice_df)) {
            plot.new()
            title(main = pred_var, cex.main = 1.1, font.main = 2)
            text(0.5, 0.5, "Could not compute ICE", cex = 0.9)
            next
          }

          cls_list <- as.character(unique(ice_df$.class))

          plot(
            x    = NULL,
            xlim = range(ice_df[[pred_var]], na.rm = TRUE),
            ylim = c(0, 1),
            main = pred_var,
            xlab = pred_var,
            ylab = "Predicted Probability",
            cex.main  = 1.3,
            font.main = 2,
            cex.lab   = 1.1,
            font.lab  = 2,
            cex.axis  = 1.0,
            bty       = "l"
          )

          for (j in seq_along(cls_list)) {
            cls_ice <- ice_df[as.character(ice_df$.class) == cls_list[j], ]
            x_vals  <- sort(unique(cls_ice[[pred_var]]))
            obs_ids <- sort(unique(cls_ice$.id))

            y_mat <- matrix(NA_real_, nrow = length(x_vals), ncol = length(obs_ids))
            for (i in seq_along(obs_ids)) {
              obs_df      <- cls_ice[cls_ice$.id == obs_ids[i], ]
              obs_df      <- obs_df[order(obs_df[[pred_var]]), ]
              y_mat[, i]  <- obs_df$.value
            }

            matlines(x_vals, y_mat,
                     col = adjustcolor(cls_cols[j], alpha.f = 0.2),
                     lty = 1, lwd = 0.7)
          }

          legend("bottom", legend = cls_list,
                 col = cls_cols[seq_along(cls_list)],
                 lwd = 2, bty = "n", cex = 0.8, horiz = TRUE,
                 xpd = TRUE, inset = c(0, -0.4))

          mtext("Shows how the prediction changes for each individual observation as this variable changes.",
                side = 3, line = 0.9, cex = 0.65, col = "#555555")
          mtext("Each line represents one observation.",
                side = 3, line = 0.2, cex = 0.65, col = "#555555")
        }

        remaining <- n_rows * n_cols - n_pred
        for (k in seq_len(remaining)) plot.new()
      })

      output$rfALEContainer <- renderUI({
        r <- calc_results()
        req(r, !is.null(r$ale_results))

        n_pred    <- length(r$predictors)
        n_cols    <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
        n_rows    <- ceiling(n_pred / n_cols)
        height_px <- max(380, n_rows * 380)

        tagList(
          tags$hr(),
          tags$h4("Accumulated Local Effects Plots", style = "margin-top: 10px;"),
          plotOutput(session$ns("rfALEPlot"), height = paste0(height_px, "px"))
        )
      })

      output$rfALEPlot <- renderPlot({
        r <- calc_results()
        req(r, !is.null(r$ale_results))

        n_pred  <- length(r$predictors)
        n_cols  <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
        n_rows  <- ceiling(n_pred / n_cols)

        cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")

        par(
          mfrow = c(n_rows, n_cols),
          oma   = c(0, 0, 0, 0),
          mar   = c(8, 4, 5, 2)
        )

        for (pred_var in r$predictors) {
          ale_df <- r$ale_results[[pred_var]]

          if (is.null(ale_df)) {
            plot.new()
            title(main = pred_var, cex.main = 1.1, font.main = 2)
            text(0.5, 0.5, "Could not compute ALE", cex = 0.9)
            next
          }

          cls_list <- as.character(unique(ale_df$.class))
          y_range  <- range(ale_df$.value, na.rm = TRUE)

          plot(
            x    = NULL,
            xlim = range(ale_df[[pred_var]], na.rm = TRUE),
            ylim = y_range,
            main = pred_var,
            xlab = pred_var,
            ylab = "ALE",
            cex.main  = 1.3,
            font.main = 2,
            cex.lab   = 1.1,
            font.lab  = 2,
            cex.axis  = 1.0,
            bty       = "l"
          )

          abline(h = 0, col = "grey70", lty = 2, lwd = 1)

          for (j in seq_along(cls_list)) {
            cls_df <- ale_df[as.character(ale_df$.class) == cls_list[j], ]
            cls_df <- cls_df[order(cls_df[[pred_var]]), ]
            lines(cls_df[[pred_var]], cls_df$.value,
                  col = cls_cols[j], lwd = 2)
          }

          legend("bottom", legend = cls_list,
                 col = cls_cols[seq_along(cls_list)],
                 lwd = 2, bty = "n", cex = 0.8, horiz = TRUE,
                 xpd = TRUE, inset = c(0, -0.4))

          mtext("Shows the accumulated local effect of this variable on the predicted outcome.",
                side = 3, line = 0.9, cex = 0.65, col = "#555555")
          mtext("More reliable than PDP when predictors are correlated.",
                side = 3, line = 0.2, cex = 0.65, col = "#555555")
        }

        remaining <- n_rows * n_cols - n_pred
        for (k in seq_len(remaining)) plot.new()
      })

      # ---- Show tabs and navigate ----
      summary_ready(TRUE)
      summary_ever_calculated(TRUE)

      showTab(inputId = "rfMainPanel", target = "model_summary_tab")
      showTab(inputId = "rfMainPanel", target = "plots_tab")

      shinyjs::delay(100, {
        updateNavbarPage(session, "rfMainPanel", selected = "model_summary_tab")
      })

    }, ignoreInit = TRUE)

    # ---- Reset ----
    observeEvent(input$reset, {
      hideTab(inputId = "rfMainPanel", target = "model_summary_tab")
      hideTab(inputId = "rfMainPanel", target = "plots_tab")

      summary_ready(FALSE)
      summary_ever_calculated(FALSE)

      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      responseContinuous(FALSE)
      rf_message(NULL)
      calc_results(NULL)

      shinyjs::removeClass(id = "responseWrapper",   class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")

      updatePickerInput(session, "response",   selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNumericInput(session, "ntree", value = 500)
      updateNumericInput(session, "mtry",  value = NA)
      updateSliderInput(session,  "split", value = 80)

      updateNavbarPage(session, "rfMainPanel", selected = "uploaded_data_tab")
    })

  })
}
