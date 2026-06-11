# R/linearDiscriminantAnalysis.R

LDASidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    div(
      style = "font-size: 15px; color: #6c757d; margin-top: 8px; margin-bottom: 6px; ",
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
    
    checkboxInput(
      ns("useCV"),
      "Use Leave-One-Out Cross-Validation",
      value = FALSE
    ),
    
    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

LDAMainPanelUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    suppressWarnings(tippy::use_tippy()),
    navbarPage(
      title = NULL,

      tabPanel(
        title = "Results",
        value = "results_tab",
        uiOutput(ns("resultsContainer"))
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

      id = ns("ldaMainPanel"),
      selected = "uploaded_data_tab",
      theme = bs_theme(version = 4)
    )
  )
}

LDAServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    prepare_lda_response <- function(x) {
      x_no_na <- x[!is.na(x)]
      
      if (length(x_no_na) == 0) {
        return(NULL)
      }

      if (is.factor(x) || is.character(x) || is.logical(x)) {
        return(as.factor(x))
      }

      if (is.numeric(x) || is.integer(x)) {
        unique_vals <- unique(x_no_na)
        
        if (length(unique_vals) < 2) {
          return(as.factor(x))
        }
        
        return(factor(x, levels = sort(unique_vals)))
      }
      as.factor(x)
    }

    results_ready <- reactiveVal(FALSE)
    plots_ready <- reactiveVal(FALSE)

    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)

    calc_results <- reactiveVal(NULL)
    plot_results <- reactiveVal(NULL)
    lda_message <- reactiveVal(NULL)
    
    noFileCalculate <- reactiveVal(FALSE)
    responseError <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    
    observeEvent(TRUE, {
      shinyjs::delay(0, {
        hideTab(inputId = "ldaMainPanel", target = "results_tab")
        hideTab(inputId = "ldaMainPanel", target = "plots_tab")
      })
    }, once = TRUE)
    
    # Uploaded Data tab
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DTOutput(session$ns("ldaUploadTable"))
      }
    })

    output$ldaUploadTable <- renderDT({
      req(data())
      
      datatable(
        data(),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX = TRUE
        )
      )
    })

    # Populate response/predictor choices
    observeEvent(data(), {
      noFileCalculate(FALSE)
      req(data())

      df <- data()
      cols <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]

      pre_predictors <- intersect(shared_explanatory(), numeric_cols)
      shared_resp    <- shared_response()
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% cols) shared_resp else character(0)

      updatePickerInput(session, "response",   choices = cols,         selected = pre_response)
      updatePickerInput(session, "predictors", choices = numeric_cols, selected = pre_predictors)

      results_ready(FALSE)
      plots_ready(FALSE)
      calc_results(NULL)
      plot_results(NULL)
      lda_message(NULL)
    })
    
    observeEvent(input$response, {
      req(data())
      
      df <- data()
      cols <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]
      
      available_predictors <- setdiff(numeric_cols, input$response)
      selected_predictors <- intersect(input$predictors, available_predictors)
      
      updatePickerInput(
        session,
        "predictors",
        choices = available_predictors,
        selected = selected_predictors
      )
    }, ignoreInit = TRUE)

    # Clear outputs if settings change after calculate
    observeEvent(
      list(
        data(),
        input$response,
        input$predictors,
        input$useCV
      ),
      {
        if (isTRUE(results_ready())) {
          results_ready(FALSE)
          calc_results(NULL)
        }
        
        if (isTRUE(plots_ready())) {
          plots_ready(FALSE)
          plot_results(NULL)
        }
        
        hideTab(inputId = "ldaMainPanel", target = "results_tab")
        hideTab(inputId = "ldaMainPanel", target = "plots_tab")
      },
      ignoreInit = TRUE
    )

    # Results tab container
    output$resultsContainer <- renderUI({
      if (!isTRUE(results_ready())) {

        if (!isTRUE(results_ever_calculated())) {
          return(tagList(
            helpText("No results yet. Upload a dataset, choose variables, then click Calculate.")
          ))
        }

        return(tagList(
          helpText("Settings changed. Click Calculate to update results.")
        ))
      }

      uiOutput(session$ns("resultsUI"))
    })

    # Plots tab container
    output$plotsContainer <- renderUI({
      if (!isTRUE(plots_ready())) {

        if (!isTRUE(plots_ever_calculated())) {
          return(tagList(
            helpText("No plots yet. Upload a dataset, choose variables, then click Calculate.")
          ))
        }

        return(tagList(
          helpText("Settings changed. Click Calculate to update plots.")
        ))
      }

      tagList(
        tags$h4("LDA Plot"),
        plotOutput(session$ns("ldaPlot"), height = "500px")
      )
    })
    
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(
          class = "shiny-output-error-validation",
          "Required: Cannot calculate without a data file."
        )
      } else {
        msg <- lda_message()
        
        if (is.null(msg)) return(NULL)
        
        div(
          style = "margin-top:10px;",
          div(
            class = "alert alert-danger",
            msg
          )
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

    observeEvent(input$response, {
      shared_response(input$response)
      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }
    })

    observeEvent(input$predictors, {
      shared_explanatory(input$predictors)
      if (length(input$predictors) >= 1) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    })
    
    # Main calculation
    observeEvent(input$calculate, {
    if (!isTruthy(data())) {
      noFileCalculate(TRUE)
      return()
    } else {
      noFileCalculate(FALSE)
    }
    
    if (!isTruthy(input$response)) {
      responseError(TRUE)
      shinyjs::addClass(id = "responseWrapper", class = "has-error")
    } else {
      responseError(FALSE)
      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
    }
    
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
    
    resp_col <- input$response[1]
    df <- data()

      analysis_df <- df[, c(input$predictors, resp_col), drop = FALSE]
      analysis_df <- na.omit(analysis_df)

      analysis_df[[resp_col]] <- prepare_lda_response(analysis_df[[resp_col]])

      if (is.null(analysis_df[[resp_col]])) {
        showNotification("Response variable could not be prepared for LDA.", type = "error", duration = 8)
        return()
      }

      predictor_df <- analysis_df[, input$predictors, drop = FALSE]
      numeric_check <- sapply(predictor_df, is.numeric)

      class_counts    <- table(analysis_df[[resp_col]])
      class_dist_df   <- as.data.frame(class_counts)
      colnames(class_dist_df) <- c("Class", "Count")

      if (nlevels(analysis_df[[resp_col]]) > floor(nrow(analysis_df) / 2)) {
        showNotification(
          "The selected response variable has too many unique values to be treated as a categorical class variable for LDA.",
          type = "error",
          duration = 8
        )
        return()
      }
      
      if (!all(numeric_check)) {
        showNotification("All explanatory variables must be numeric for LDA.", type = "error", duration = 8)
        return()
      }
      
      if (nlevels(analysis_df[[resp_col]]) < 2) {
        showNotification("Response variable must have at least 2 classes.", type = "error", duration = 8)
        return()
      }

      if (length(input$predictors) < 1) {
        showNotification("Select at least one explanatory variables", type = "error", duration = 8)
        return()
      }

      if (any(class_counts < 2)) {
        lda_message(
          "Each class must have at least 2 observations for LDA. One or more classes in your selected response variable have fewer than 2 rows."
        )
        return()
      }

      # Zero variance check
      sds <- sapply(predictor_df, sd, na.rm = TRUE)
      zero_var_cols <- names(sds)[is.na(sds) | sds == 0]
      if (length(zero_var_cols) > 0) {
        lda_message(paste0(
          "These selected variable(s) have zero variance and cannot be used in LDA: ",
          paste(zero_var_cols, collapse = ", "), "."
        ))
        return()
      }

      response_var <- paste0("`", resp_col, "`")
      predictor_vars <- paste0("`", input$predictors, "`")
      
      lda_formula <- as.formula(
        paste(response_var, "~", paste(predictor_vars, collapse = " + "))
      )
      lda_message(NULL)
      lda_fit <- tryCatch(
        MASS::lda(lda_formula, data = analysis_df),
        error = function(e) {
          showNotification(
            paste("LDA could not be computed:", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      )
      
      req(lda_fit)
      
      lda_pred <- predict(lda_fit, analysis_df[, input$predictors, drop = FALSE])

      confusion_mat <- table(
        Actual = analysis_df[[resp_col]],
        Predicted = lda_pred$class
      )

      accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)

      singular_vals_sq <- lda_fit$svd^2
      prop_trace <- singular_vals_sq / sum(singular_vals_sq)

      scores_mat <- as.data.frame(lda_pred$x)
      
      if (!"LD1" %in% names(scores_mat)) {
        scores_mat$LD1 <- 0
      }
      if (!"LD2" %in% names(scores_mat)) {
        scores_mat$LD2 <- 0
      }
      
      scores_df <- data.frame(
        LD1 = scores_mat$LD1,
        LD2 = scores_mat$LD2,
        Class = analysis_df[[resp_col]]
      )

      cv_confusion <- NULL
      cv_accuracy <- NULL

      if (isTRUE(input$useCV)) {
        lda_cv <- MASS::lda(lda_formula, data = analysis_df, CV = TRUE)

        cv_confusion <- table(
          Actual = analysis_df[[resp_col]],
          Predicted = lda_cv$class
        )

        cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
      }

      class_report_preds <- if (isTRUE(input$useCV)) lda_cv$class else lda_pred$class
      lda_class_report   <- knn_classification_report(analysis_df[[resp_col]], class_report_preds)$report

      res <- list(
        fit = lda_fit,
        confusion = confusion_mat,
        accuracy = accuracy,
        prop_trace = prop_trace,
        scores = scores_df,
        cv_confusion = cv_confusion,
        cv_accuracy = cv_accuracy,
        response = resp_col,
        predictors = input$predictors,
        n = nrow(analysis_df),
        class_dist = class_dist_df,
        class_report = lda_class_report
      )

      calc_results(res)
      plot_results(res)

      results_ready(TRUE)
      plots_ready(TRUE)
      results_ever_calculated(TRUE)
      plots_ever_calculated(TRUE)

      # Results UI
      output$resultsUI <- renderUI({
        r <- calc_results()
        req(r)

        conf_used <- if (isTRUE(input$useCV) && !is.null(r$cv_confusion)) r$cv_confusion else r$confusion
        acc_used  <- if (isTRUE(input$useCV) && !is.null(r$cv_accuracy))  r$cv_accuracy  else r$accuracy
        correct   <- sum(diag(conf_used))
        total     <- sum(conf_used)

        tagList(
          tags$h4("Model Summary"),
          tableOutput(session$ns("ldaModelInfo")),
          tags$hr(),

          tags$h4("Class Distribution (Full Dataset)"),
          tableOutput(session$ns("ldaClassDist")),
          tags$hr(),

          tags$h4("Classification Report"),
          tableOutput(session$ns("ldaClassReport")),
          tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),
          tags$hr(),

          tags$h4("Confusion Matrix"),
          tableOutput(session$ns("confusionMatrix")),
          tags$h5(tags$strong("Accuracy Calculation"),
                  style = "margin-top: 14px; margin-bottom: 2px;"),
          withMathJax(
            tags$p(sprintf(
              "\\[ \\text{Accuracy} = \\frac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\frac{%d}{%d} = %.2f\\%% \\]",
              correct, total, acc_used * 100
            ))
          ),
          tags$hr(),

          tags$h4("Prior Probabilities"),
          tableOutput(session$ns("ldaPriors")),

          tags$h4("Group Means"),
          tableOutput(session$ns("groupMeans")),

          tags$h4("Coefficients of Linear Discriminants"),
          tableOutput(session$ns("coefficients")),

          tags$h4("Proportion of Trace"),
          tableOutput(session$ns("propTrace"))
        )
      })

      output$ldaModelInfo <- renderTable({
        r <- calc_results()
        req(r)

        acc <- if (isTRUE(input$useCV) && !is.null(r$cv_accuracy)) {
          round(r$cv_accuracy, 4)
        } else {
          round(r$accuracy, 4)
        }

        data.frame(
          Item = c(
            "Number of Classes",
            "Number of Predictors",
            "Number of Complete Cases",
            "Cross Validation",
            "Accuracy"
          ),
          Value = c(
            length(r$fit$prior),
            length(r$predictors),
            r$n,
            if (isTRUE(input$useCV)) "Leave-One-Out" else "None",
            acc
          ),
          check.names = FALSE
        )
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)
      
      output$ldaClassDist <- renderTable({
        r <- calc_results()
        req(r)
        r$class_dist
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$ldaClassReport <- renderTable({
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

      output$ldaPriors <- renderTable({
        r <- calc_results()
        req(r)

        data.frame(
          Class = names(r$fit$prior),
          Prior_Probability = round(as.numeric(r$fit$prior), 4),
          check.names = FALSE
        )
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$groupMeans <- renderTable({
        r <- calc_results()
        req(r)
        round(r$fit$means, 4)
      }, rownames = TRUE, striped = TRUE, bordered = TRUE)

      output$coefficients <- renderTable({
        r <- calc_results()
        req(r)
        round(r$fit$scaling, 4)
      }, rownames = TRUE, striped = TRUE, bordered = TRUE)

      output$propTrace <- renderTable({
        r <- calc_results()
        req(r)

        data.frame(
          Discriminant = paste0("LD", seq_along(r$prop_trace)),
          Proportion = round(r$prop_trace, 4),
          check.names = FALSE
        )
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$confusionMatrix <- renderTable({
        r <- calc_results()
        req(r)

        cm <- if (isTRUE(input$useCV) && !is.null(r$cv_confusion)) {
          as.data.frame.matrix(r$cv_confusion)
        } else {
          as.data.frame.matrix(r$confusion)
        }

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

      output$ldaPlot <- renderPlot({
        r <- plot_results()
        req(r)

        scores   <- r$scores
        classes  <- as.character(levels(scores$Class))
        n_cls    <- length(classes)
        cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")[seq_len(min(n_cls, 5))]
        col_vec  <- cls_cols[match(as.character(scores$Class), classes)]

        par(mar = c(5, 4, 4, 2))

        plot(
          scores$LD1,
          scores$LD2,
          col       = col_vec,
          pch       = 19,
          cex       = 1.1,
          main      = "LDA Plot",
          xlab      = "LD1",
          ylab      = "LD2",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.axis  = 1.0,
          bty       = "l"
        )

        legend(
          "topright",
          legend = classes,
          col    = cls_cols[seq_along(classes)],
          pch    = 19,
          pt.cex = 1.1,
          bty    = "n",
          cex    = 0.95,
          title  = "Class"
        )
      })

      showTab(inputId = "ldaMainPanel", target = "results_tab")
      showTab(inputId = "ldaMainPanel", target = "plots_tab")
      
      updateNavbarPage(session, "ldaMainPanel", selected = "results_tab")

    }, ignoreInit = TRUE)

    # Reset
    observeEvent(input$reset, {
      hideTab(inputId = "ldaMainPanel", target = "results_tab")
      hideTab(inputId = "ldaMainPanel", target = "plots_tab")
      
      results_ready(FALSE)
      plots_ready(FALSE)
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)

      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      lda_message(NULL)
      
      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")

      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNavbarPage(session, "ldaMainPanel", selected = "uploaded_data_tab")
    })
  })
}