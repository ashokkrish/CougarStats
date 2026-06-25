# R/decisionTrees.R

CARTSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    numericInput(
      ns("max_depth"),
      strong("Maximum Tree Depth"),
      value = 10,
      min = 1
    ),

    numericInput(
      ns("min_split"),
      strong("Minimum Split Size"),
      value = 20,
      min = 1
    ),

    numericInput(
      ns("cp"),
      strong("Complexity Parameter"),
      value = 0.01,
      min = 0.01,
      step = 0.01
    ),
    
    div(
      style = "font-size: 15px; color: #6c757d; margin-top: 8px; margin-bottom: 6px; ",
      "Select a categorical variable, must have 2 or more unique categories."
    ),
    
    div(
      id = ns("responseWrapper"),
      pickerInput(
        ns("response"),
        strong(HTML("Response Variable (Class)")),
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
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

CARTMainPanelUI <- function(id) {
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
      
      id = ns("cartMainPanel"),
      selected = "uploaded_data_tab",
      theme = bs_theme(version = 4)
    )
  )
}

CARTServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    results_ready <- reactiveVal(FALSE)
    plots_ready <- reactiveVal(FALSE)
    
    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)
    
    calc_results <- reactiveVal(NULL)
    plot_results <- reactiveVal(NULL)
    cart_message <- reactiveVal(NULL)
    
    noFileCalculate <- reactiveVal(FALSE)
    responseError <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    responseContinuous <- reactiveVal(FALSE)

    cart_iv <- shinyvalidate::InputValidator$new()
    cart_iv$add_rule("max_depth", shinyvalidate::sv_required())
    cart_iv$add_rule("max_depth", shinyvalidate::sv_gte(1, message = "Must be at least 1."))
    cart_iv$add_rule("min_split", shinyvalidate::sv_required())
    cart_iv$add_rule("min_split", shinyvalidate::sv_gte(1, message = "Must be at least 1."))
    cart_iv$add_rule("cp", shinyvalidate::sv_required())
    cart_iv$add_rule("cp", shinyvalidate::sv_gt(0, message = "Must be greater than 0."))
    cart_iv$enable()
    
    session$onFlushed(function() {
      hideTab(inputId = "cartMainPanel", target = "results_tab")
      hideTab(inputId = "cartMainPanel", target = "plots_tab")
    }, once = TRUE)
    
    # Uploaded Data tab
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DTOutput(session$ns("cartUploadTable"))
      }
    })
    
    output$cartUploadTable <- renderDT({
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
    
    # Populate response/predictor choices after upload
    observeEvent(data(), {
      noFileCalculate(FALSE)
      req(data())
      
      df <- data()
      cols <- colnames(df)
      
      pre_predictors <- intersect(shared_explanatory(), cols)
      shared_resp    <- shared_response()

      n_rows         <- nrow(df)
      valid_response <- cols[sapply(cols, function(col) {
        n_uniq <- length(unique(na.omit(df[[col]])))
        n_uniq >= 2 && n_uniq <= floor(n_rows / 2)
      })]
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% valid_response) shared_resp else character(0)

      updatePickerInput(session, "response",   choices = valid_response, selected = pre_response)
      updatePickerInput(session, "predictors", choices = cols,           selected = pre_predictors)

      results_ready(FALSE)
      plots_ready(FALSE)
      calc_results(NULL)
      plot_results(NULL)
      cart_message(NULL)
    })
    
    # Keep response out of predictors
    observeEvent(input$response, {
      req(data())
      
      df <- data()
      cols <- colnames(df)
      
      available_predictors <- setdiff(cols, input$response)
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
        input$max_depth,
        input$min_split,
        input$cp
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
        
        hideTab(inputId = "cartMainPanel", target = "results_tab")
        hideTab(inputId = "cartMainPanel", target = "plots_tab")
        updateNavbarPage(session, "cartMainPanel", selected = "uploaded_data_tab")
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
        tags$h4("Decision Tree Diagram"),
        plotOutput(session$ns("treePlot"), height = "550px"),
        
        tags$h4("Variable Importance"),
        plotOutput(session$ns("varImportancePlot"), height = "450px")
      )
    })
    
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(
          class = "shiny-output-error-validation",
          "Required: Cannot calculate without a data file."
        )
      } else {
        msg <- cart_message()
        
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

      if (isTruthy(data()) && isTruthy(input$response)) {
        responseContinuous(ml_is_continuous_response(data()[[input$response[1]]]))
      } else {
        responseContinuous(FALSE)
      }
    })

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

    observeEvent(input$calculate, {
      if (!isTruthy(data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }
      
      # input validation
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
      
      req(cart_iv$is_valid())

      if (!isTruthy(input$response) ||
          !isTruthy(input$predictors) ||
          length(input$predictors) < 1) {
        return()
      }
      
      resp_col <- input$response[1]
      df <- data()

      # Continuous response guard — block classification on a continuous variable
      if (ml_is_continuous_response(df[[resp_col]])) {
        responseContinuous(TRUE)
        return()
      }

      analysis_df <- df[, c(input$predictors, resp_col), drop = FALSE]

      na_cols <- names(which(sapply(analysis_df, function(x) any(is.na(x)))))
      if (length(na_cols) > 0) {
        cart_message(paste0(
          "The following column(s) contain missing values (NA): ",
          paste(na_cols, collapse = ", "),
          ". Please remove or impute missing values before calculating."
        ))
        return()
      }

      analysis_df <- na.omit(analysis_df)

      if (nrow(analysis_df) == 0) {
        showNotification("No complete cases remain after removing missing values.", type = "error", duration = 8)
        return()
      }

      analysis_df[[resp_col]] <- as.factor(analysis_df[[resp_col]])

      if (nlevels(analysis_df[[resp_col]]) < 2) {
        showNotification("Response variable must have at least 2 classes.", type = "error", duration = 8)
        return()
      }

      if (nlevels(analysis_df[[resp_col]]) > floor(nrow(analysis_df) / 2)) {
        showNotification(
          "The selected response variable has too many unique values to be treated as a categorical class variable for CART.",
          type = "error",
          duration = 8
        )
        return()
      }
      
      predictor_df      <- analysis_df[, input$predictors, drop = FALSE]
      numeric_predictors <- names(predictor_df)[sapply(predictor_df, is.numeric)]

      if (length(numeric_predictors) > 0) {
        sds <- sapply(predictor_df[, numeric_predictors, drop = FALSE], sd, na.rm = TRUE)
        zero_var_cols <- names(sds)[is.na(sds) | sds == 0]
        if (length(zero_var_cols) > 0) {
          cart_message(paste0(
            "These selected variable(s) have zero variance and cannot be used in CART: ",
            paste(zero_var_cols, collapse = ", "), "."
          ))
          return()
        }
      }
      
      cart_formula <- as.formula(
        paste(
          paste0("`", resp_col, "`"),
          "~",
          paste(paste0("`", input$predictors, "`"), collapse = " + ")
        )
      )
      
      cart_message(NULL)
      
      cart_fit <- tryCatch(
        rpart::rpart(
          formula = cart_formula,
          data = analysis_df,
          method = "class",
          control = rpart::rpart.control(
            maxdepth = as.integer(input$max_depth),
            minsplit = as.integer(input$min_split),
            cp = as.numeric(input$cp)
          )
        ),
        error = function(e) {
          showNotification(
            paste("Decision tree could not be computed:", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      )
      
      req(cart_fit)
      
      cart_pred <- tryCatch(
        predict(cart_fit, analysis_df[, input$predictors, drop = FALSE], type = "class"),
        error = function(e) {
          showNotification(
            paste("Predictions could not be generated:", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      )
      
      req(cart_pred)
      
      confusion_mat <- table(
        Actual = analysis_df[[resp_col]],
        Predicted = cart_pred
      )
      
      accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
      
      class_dist_df     <- as.data.frame(table(analysis_df[[resp_col]]))
      colnames(class_dist_df) <- c("Class", "Count")

      cart_class_report <- knn_classification_report(analysis_df[[resp_col]], cart_pred)$report

      importance <- cart_fit$variable.importance
      if (is.null(importance)) {
        importance <- numeric(0)
      }
      
      importance_df <- data.frame(
        Variable = names(importance),
        Importance = as.numeric(importance),
        stringsAsFactors = FALSE
      )
      
      if (nrow(importance_df) > 0) {
        importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), , drop = FALSE]
        importance_df$ImportancePct <- (importance_df$Importance / sum(importance_df$Importance)) * 100
      }
      
      res <- list(
        fit = cart_fit,
        confusion = confusion_mat,
        accuracy = accuracy,
        response = resp_col,
        predictors = input$predictors,
        n = nrow(analysis_df),
        importance = importance_df,
        class_dist = class_dist_df,
        class_report = cart_class_report
      )
      
      calc_results(res)
      plot_results(res)
      
      results_ready(TRUE)
      plots_ready(TRUE)
      results_ever_calculated(TRUE)
      plots_ever_calculated(TRUE)
      
      output$resultsUI <- renderUI({
        r <- calc_results()
        req(r)
        
        correct <- sum(diag(r$confusion))
        total   <- sum(r$confusion)

        tagList(
          tags$h4("Model Summary"),
          tableOutput(session$ns("cartModelInfo")),
          tags$hr(),

          tags$h4("Class Distribution (Full Dataset)"),
          tableOutput(session$ns("cartClassDist")),
          tags$hr(),

          tags$h4("Classification Report"),
          tableOutput(session$ns("cartClassReport")),
          tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),
          tags$hr(),

          tags$h4("Confusion Matrix"),
          tableOutput(session$ns("confusionMatrixResults")),
          tags$h5(tags$strong("Accuracy Calculation"),
                  style = "margin-top: 14px; margin-bottom: 2px;"),
          withMathJax(),
          tags$p(HTML(sprintf(
            "\\( \\text{Accuracy} = \\dfrac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\dfrac{%d}{%d} = %.2f\\%% \\)",
            correct, total, r$accuracy * 100
          ))),
          tags$script(HTML("if(window.MathJax){ MathJax.Hub ? MathJax.Hub.Queue(['Typeset',MathJax.Hub]) : MathJax.typesetPromise(); }"))
        )
      })
      
      output$cartModelInfo <- renderTable({
        r <- calc_results()
        req(r)

        data.frame(
          Item = c(
            "Number of Classes",
            "Number of Predictors",
            "Number of Complete Cases",
            "Maximum Tree Depth",
            "Minimum Split Size",
            "Complexity Parameter",
            "Accuracy"
          ),
          Value = c(
            as.character(length(unique(analysis_df[[resp_col]]))),
            as.character(length(r$predictors)),
            as.character(r$n),
            as.character(as.integer(input$max_depth)),
            as.character(as.integer(input$min_split)),
            as.character(input$cp),
            as.character(round(r$accuracy, 4))
          ),
          check.names = FALSE
        )
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$cartClassDist <- renderTable({
        r <- calc_results()
        req(r)
        r$class_dist
      }, rownames = FALSE, striped = TRUE, bordered = TRUE)

      output$cartClassReport <- renderTable({
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

      output$confusionMatrixResults <- renderTable({
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
      
      output$treePlot <- renderPlot({
        r <- plot_results()
        req(r)
        
        par(mar = c(1, 1, 3, 1))
        
        rpart.plot::rpart.plot(
          r$fit,
          main = "Decision Tree Diagram",
          extra = 104,
          fallen.leaves = TRUE,
          tweak = 1.05,
          under = TRUE,
          faclen = 0,
          varlen = 0,
          shadow.col = 0,
          box.palette = "Blues",
          col        = "black",
          split.col  = "black",
          under.col  = "black",
          branch.col = "black",
          border.col = "black"
        )
      }, res = 96)
      
      output$varImportancePlot <- renderPlot({
        r <- plot_results()
        req(r)

        if (nrow(r$importance) == 0) {
          plot.new()
          text(0.5, 0.5, "No variable importance available for this model.",
               cex = 0.9, col = "#555555")
          return()
        }

        imp_df    <- r$importance[order(r$importance$ImportancePct, decreasing = FALSE), ]
        max_chars <- max(nchar(imp_df$Variable), na.rm = TRUE)
        left_mar  <- max(4, ceiling(max_chars * 0.6))

        par(mar = c(5, left_mar, 4, 2))

        barplot(
          imp_df$ImportancePct,
          names.arg = imp_df$Variable,
          horiz     = TRUE,
          las       = 1,
          col       = "#18536F",
          main      = "Variable Importance",
          xlab      = "Importance (%)",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.names = 0.95
        )
      })
      
      showTab(inputId = "cartMainPanel", target = "results_tab")
      showTab(inputId = "cartMainPanel", target = "plots_tab")
      
      shinyjs::delay(100, {
        updateNavbarPage(session, "cartMainPanel", selected = "results_tab")
      })
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$reset, {
      hideTab(inputId = "cartMainPanel", target = "results_tab")
      hideTab(inputId = "cartMainPanel", target = "plots_tab")
      
      results_ready(FALSE)
      plots_ready(FALSE)
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)
      
      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      responseContinuous(FALSE)
      cart_message(NULL)

      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      
      updateNumericInput(session, "max_depth", value = 10)
      updateNumericInput(session, "min_split", value = 20)
      updateNumericInput(session, "cp", value = 0.01)
      
      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      
      updateNavbarPage(session, "cartMainPanel", selected = "uploaded_data_tab")
    })
  })
}