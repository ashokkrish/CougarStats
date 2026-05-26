# R/randomForest.R

# ============== UI ==============

RFSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    sliderInput(
      ns("split"),
      label = strong("Train/Test Split (%)"),
      min = 50, max = 95, value = 80, step = 1
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
      uiOutput(ns("responseError"))
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
    navbarPage(
      title = NULL,

      tabPanel(
        title = "Model Summary",
        value = "model_summary_tab",
        uiOutput(ns("modelSummaryContainer"))
      ),

      tabPanel(
        title = "Plots",
        value = "plots_tab",
        uiOutput(ns("plotsContainer"))
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
    plots_ready   <- reactiveVal(FALSE)

    summary_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated   <- reactiveVal(FALSE)

    calc_results <- reactiveVal(NULL)
    plot_results <- reactiveVal(NULL)
    rf_message   <- reactiveVal(NULL)

    noFileCalculate <- reactiveVal(FALSE)
    responseError   <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)

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
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% cols) shared_resp else character(0)

      updatePickerInput(session, "response",   choices = cols,         selected = pre_response)
      updatePickerInput(session, "predictors", choices = numeric_cols, selected = pre_predictors)
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
    }, ignoreInit = TRUE)

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
        if (isTRUE(plots_ready()))   plots_ready(FALSE)

        calc_results(NULL)
        plot_results(NULL)
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

    output$plotsContainer <- renderUI({
      if (!isTRUE(plots_ready())) {
        if (!isTRUE(plots_ever_calculated())) {
          return(tagList(helpText("No plots yet. Upload a dataset, choose variables, then click Calculate.")))
        }
        return(tagList(helpText("Settings changed. Click Calculate to update plots.")))
      }
      tagList(
        tags$h4("Variable Importance Plot"),
        plotOutput(session$ns("rfVarImportPlot"), height = "450px"),
        tags$hr(),
        tags$h4("Partial Dependence Plots (PDP)"),
        uiOutput(session$ns("rfPDPContainer")),
        tags$hr(),
        tags$h4("Individual Conditional Expectation (ICE)"),
        uiOutput(session$ns("rfICEContainer")),
        tags$hr(),
        tags$h4("Accumulated Local Effects (ALE)"),
        uiOutput(session$ns("rfALEContainer")),
        tags$p(
          style = "color: #6c757d; font-style: italic; font-size: 13px; margin-top: 8px;",
          tags$strong("Note:"),
          " ALE plots are preferred over PDPs when predictor variables are correlated, ",
          "as ALE accounts for the joint distribution of features and avoids extrapolation ",
          "into unlikely regions of the feature space."
        ),
        tags$hr(),
        tags$h4("SHAP Summary"),
        uiOutput(session$ns("rfSHAPContainer"))
      )
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

      # 5. Drop incomplete rows
      analysis_df <- df[, c(input$predictors, resp_col), drop = FALSE]
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

      # 8. Resolve mtry — NA / blank means auto (NULL → randomForest uses floor(sqrt(p)))
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

      # 9. Train / test split
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

      # 17. Build iml Predictor for interpretability plots (probability predictions)
      iml_predictor <- tryCatch(
        iml::Predictor$new(
          model = rf_fit,
          data  = train_df[, input$predictors, drop = FALSE],
          y     = train_df[[resp_col]],
          type  = "prob"
        ),
        error = function(e) NULL
      )

      res <- list(
        fit           = rf_fit,
        train_df      = train_df,
        test_df       = test_df,
        iml_predictor = iml_predictor,
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
        class_metrics = class_metrics_df
      )

      calc_results(res)
      plot_results(res)

      # ---- Model Summary UI ----
      output$modelSummaryUI <- renderUI({
        r <- calc_results()
        req(r)
        tagList(
          tags$h4("Random Forest Model Summary"),
          tableOutput(session$ns("rfSummaryTable")),
          tags$hr(),
          tags$h4("Class Distribution (Full Dataset)"),
          tableOutput(session$ns("rfClassDistTable")),
          tags$hr(),
          tags$h4("Confusion Matrix (Test Set)"),
          tableOutput(session$ns("rfConfusionTable")),
          tags$hr(),
          tags$h4("Per-Class Metrics"),
          tableOutput(session$ns("rfClassMetrics")),
          tags$br(),
          tags$p(
            tags$strong("Overall Accuracy: "),
            paste0(round(r$accuracy * 100, 2), "%")
          )
        )
      })

      output$rfSummaryTable <- renderTable({
        r <- calc_results()
        req(r)
        data.frame(
          Item = c(
            "Type",
            "Number of Trees",
            "Variables per Split (mtry)",
            "Number of Predictors",
            "Total Observations",
            "Training Observations",
            "Test Observations",
            "OOB Error Rate"
          ),
          Value = c(
            "Classification",
            as.character(r$ntree),
            as.character(r$mtry_used),
            as.character(length(r$predictors)),
            as.character(r$n_total),
            as.character(r$n_train),
            as.character(r$n_test),
            paste0(round(r$oob_error * 100, 2), "%")
          ),
          check.names = FALSE
        )
      }, rownames = FALSE)

      output$rfClassDistTable <- renderTable({
        r <- calc_results()
        req(r)
        r$class_dist
      }, rownames = FALSE)

      output$rfConfusionTable <- renderTable({
        r <- calc_results()
        req(r)
        cm <- as.data.frame.matrix(r$confusion)
        cm$Actual <- rownames(cm)
        cm <- cm[, c("Actual", setdiff(names(cm), "Actual"))]
        rownames(cm) <- NULL
        cm
      }, rownames = FALSE)

      output$rfClassMetrics <- renderTable({
        r <- calc_results()
        req(r)
        r$class_metrics
      }, rownames = FALSE)

      # ---- Variable Importance Plot (iml FeatureImp, loss = 'ce') ----
      output$rfVarImportPlot <- renderPlot({
        r <- plot_results()
        req(r)

        if (is.null(r$iml_predictor)) {
          return(
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = "Variable importance could not be initialized.", size = 5) +
              theme_void()
          )
        }

        feat_imp <- tryCatch(
          iml::FeatureImp$new(r$iml_predictor, loss = "ce",
                              compare = "ratio", n.repetitions = 5),
          error = function(e) NULL
        )

        if (is.null(feat_imp)) {
          return(
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = "Variable importance could not be computed.", size = 5) +
              theme_void()
          )
        }

        imp_df <- feat_imp$results
        imp_df <- imp_df[order(imp_df$importance, decreasing = FALSE), ]
        imp_df$feature <- factor(imp_df$feature, levels = imp_df$feature)

        ggplot(imp_df, aes(x = importance, y = feature)) +
          geom_col(fill = "#18536F") +
          labs(
            title = "Variable Importance (Permutation-Based, Loss: Cross-Entropy)",
            x     = "Importance (Ratio of Permuted to Baseline Loss)",
            y     = "Predictor"
          ) +
          theme_bw() +
          theme(
            plot.title   = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title.x = element_text(face = "bold", size = 13),
            axis.title.y = element_text(face = "bold", size = 13),
            axis.text.x  = element_text(face = "bold", size = 11),
            axis.text.y  = element_text(face = "bold", size = 11)
          )
      }, res = 96)

      # ---- PDP Container ----
      output$rfPDPContainer <- renderUI({
        r <- plot_results()
        req(r)
        if (is.null(r$iml_predictor)) {
          return(div(class = "alert alert-warning",
                     "PDP plots could not be initialized."))
        }
        n_preds     <- length(r$predictors)
        n_cols      <- min(3L, n_preds)
        n_rows      <- ceiling(n_preds / n_cols)
        plot_height <- paste0(max(300L, n_rows * 280L), "px")
        plotOutput(session$ns("rfPDPPlots"), height = plot_height)
      })

      output$rfPDPPlots <- renderPlot({
        r <- plot_results()
        req(r)
        if (is.null(r$iml_predictor)) return(NULL)

        preds  <- r$predictors
        n_cols <- min(3L, length(preds))

        plot_list <- lapply(preds, function(var) {
          tryCatch({
            fe <- iml::FeatureEffect$new(r$iml_predictor, feature = var, method = "pdp")
            fe$plot() +
              labs(title = var) +
              theme_bw() +
              theme(
                plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
                axis.title.x = element_text(face = "bold", size = 10),
                axis.title.y = element_text(face = "bold", size = 10),
                axis.text    = element_text(size = 9)
              )
          }, error = function(e) {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = paste0(var, "\n(unavailable)"), size = 4) +
              theme_void() +
              labs(title = var) +
              theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))
          })
        })

        gridExtra::grid.arrange(grobs = plot_list, ncol = n_cols)
      }, res = 96)

      # ---- ICE Container ----
      output$rfICEContainer <- renderUI({
        r <- plot_results()
        req(r)
        if (is.null(r$iml_predictor)) {
          return(div(class = "alert alert-warning",
                     "ICE plots could not be initialized."))
        }
        n_preds     <- length(r$predictors)
        n_cols      <- min(3L, n_preds)
        n_rows      <- ceiling(n_preds / n_cols)
        plot_height <- paste0(max(300L, n_rows * 280L), "px")
        plotOutput(session$ns("rfICEPlots"), height = plot_height)
      })

      output$rfICEPlots <- renderPlot({
        r <- plot_results()
        req(r)
        if (is.null(r$iml_predictor)) return(NULL)

        preds  <- r$predictors
        n_cols <- min(3L, length(preds))

        plot_list <- lapply(preds, function(var) {
          tryCatch({
            fe <- iml::FeatureEffect$new(r$iml_predictor, feature = var, method = "ice")
            fe$plot() +
              labs(title = var) +
              theme_bw() +
              theme(
                plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
                axis.title.x = element_text(face = "bold", size = 10),
                axis.title.y = element_text(face = "bold", size = 10),
                axis.text    = element_text(size = 9)
              )
          }, error = function(e) {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = paste0(var, "\n(unavailable)"), size = 4) +
              theme_void() +
              labs(title = var) +
              theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))
          })
        })

        gridExtra::grid.arrange(grobs = plot_list, ncol = n_cols)
      }, res = 96)

      # ---- ALE Container ----
      output$rfALEContainer <- renderUI({
        r <- plot_results()
        req(r)
        if (is.null(r$iml_predictor)) {
          return(div(class = "alert alert-warning",
                     "ALE plots could not be initialized."))
        }
        n_preds     <- length(r$predictors)
        n_cols      <- min(3L, n_preds)
        n_rows      <- ceiling(n_preds / n_cols)
        plot_height <- paste0(max(300L, n_rows * 280L), "px")
        plotOutput(session$ns("rfALEPlots"), height = plot_height)
      })

      output$rfALEPlots <- renderPlot({
        r <- plot_results()
        req(r)
        if (is.null(r$iml_predictor)) return(NULL)

        preds  <- r$predictors
        n_cols <- min(3L, length(preds))

        plot_list <- lapply(preds, function(var) {
          tryCatch({
            fe <- iml::FeatureEffect$new(r$iml_predictor, feature = var, method = "ale")
            fe$plot() +
              labs(title = var) +
              theme_bw() +
              theme(
                plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
                axis.title.x = element_text(face = "bold", size = 10),
                axis.title.y = element_text(face = "bold", size = 10),
                axis.text    = element_text(size = 9)
              )
          }, error = function(e) {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = paste0(var, "\n(unavailable)"), size = 4) +
              theme_void() +
              labs(title = var) +
              theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5))
          })
        })

        gridExtra::grid.arrange(grobs = plot_list, ncol = n_cols)
      }, res = 96)

      # ---- SHAP Summary Container ----
      output$rfSHAPContainer <- renderUI({
        r <- plot_results()
        req(r)
        n_preds     <- length(r$predictors)
        plot_height <- paste0(max(400L, n_preds * 40L + 150L), "px")
        plotOutput(session$ns("rfSHAPPlot"), height = plot_height)
      })

      output$rfSHAPPlot <- renderPlot({
        r <- plot_results()
        req(r)

        # Cap at 1000 rows so tree-traversal stays fast
        shap_data <- r$train_df[, r$predictors, drop = FALSE]
        if (nrow(shap_data) > 1000L) {
          set.seed(42L)
          shap_data <- shap_data[sample(nrow(shap_data), 1000L), , drop = FALSE]
        }

        sv <- tryCatch({
          unified <- treeshap::randomForest.unify(r$fit, shap_data)
          ts      <- treeshap::treeshap(unified, x = shap_data,
                                        interactions = FALSE, verbose = FALSE)
          shapviz::shapviz(ts)
        }, error = function(e) NULL)

        if (is.null(sv)) {
          return(
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = "SHAP values could not be computed.", size = 5) +
              theme_void()
          )
        }

        shapviz::sv_importance(sv, kind = "beeswarm") +
          labs(
            title = "SHAP Summary: Features Ordered by Mean (Increasing Dominance)",
            x     = "SHAP value (impact on model output)"
          ) +
          theme_bw() +
          theme(
            plot.title   = element_text(hjust = 0.5, face = "bold", size = 15),
            axis.title.x = element_text(face = "bold", size = 13),
            axis.title.y = element_text(face = "bold", size = 13),
            axis.text.x  = element_text(face = "bold", size = 11),
            axis.text.y  = element_text(face = "bold", size = 11)
          )
      }, res = 96)

      # ---- Show tabs and navigate ----
      summary_ready(TRUE)
      plots_ready(TRUE)

      summary_ever_calculated(TRUE)
      plots_ever_calculated(TRUE)

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
      plots_ready(FALSE)

      summary_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)

      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      rf_message(NULL)
      calc_results(NULL)
      plot_results(NULL)

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
