# R/decisionTrees.R

CARTSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    div(
      id = ns("maxDepthWrapper"),
      numericInput(
        ns("max_depth"),
        "Maximum Tree Depth",
        value = 10,
        min = 0
      ),
      uiOutput(ns("maxDepthError"))
    ),
    
    div(
      id = ns("minSplitWrapper"),
      numericInput(
        ns("min_split"),
        "Minimum Split Size",
        value = 20,
        min = 0
      ),
      uiOutput(ns("minSplitError"))
    ),
    
    div(
      id = ns("cpWrapper"),
      numericInput(
        ns("cp"),
        "Complexity Parameter",
        value = 0.01,
        min = 0,
        step = 0.01
      ),
      uiOutput(ns("cpError"))
    ),
    
    div(
      style = "font-size: 12px; color: #6c757d; margin-top: 8px; margin-bottom: 12px;",
      "Note: CougarStats does not store, log, or share any data you upload. All uploaded files exist only for the duration of your session and are permanently deleted when the session ends."
    ),
    
    div(
      id = ns("responseWrapper"),
      pickerInput(
        ns("response"),
        HTML("Response Variable (<em>y</em>)"),
        choices = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE, title = "Nothing selected")
      ),
      uiOutput(ns("responseError"))
    ),
    
    div(
      id = ns("predictorsWrapper"),
      pickerInput(
        ns("predictors"),
        HTML("Explanatory Variables (<em>x</em><sub>1</sub>, <em>x</em><sub>2</sub>, ..., <em>x</em><sub>k</sub>)"),
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Nothing selected")
      ),
      uiOutput(ns("predictorsError"))
    ),
    
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

CARTMainPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    navbarPage(
      title = NULL,
      
      tabPanel(
        title = "Data Import",
        value = "data_import_tab",
        div(id = ns("importContainer")),
        uiOutput(ns("fileImportUserMessage")),
        import_file_ui(
          id = ns("dataImport"),
          title = ""
        )
      ),
      
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
      theme = bs_theme(version = 4)
    )
  )
}

CARTServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    uploadedTibble <- import_file_server(
      id = "dataImport",
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class = "tbl_df"
    )
    
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
    maxDepthError <- reactiveVal(FALSE)
    minSplitError <- reactiveVal(FALSE)
    cpError <- reactiveVal(FALSE)
    
    observeEvent(TRUE, {
      shinyjs::delay(0, {
        hideTab(inputId = "cartMainPanel", target = "results_tab")
        hideTab(inputId = "cartMainPanel", target = "plots_tab")
        hideTab(inputId = "cartMainPanel", target = "uploaded_data_tab")
      })
    }, once = TRUE)
    
    # Uploaded Data tab
    output$uploadedDataContainer <- renderUI({
      if (is.null(uploadedTibble$data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DTOutput(session$ns("cartUploadTable"))
      }
    })
    
    output$cartUploadTable <- renderDT({
      req(uploadedTibble$data())
      
      datatable(
        uploadedTibble$data(),
        options = list(
          pageLength = -1,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX = TRUE
        )
      )
    })
    
    # Populate response/predictor choices after upload
    observeEvent(uploadedTibble$data(), {
      noFileCalculate(FALSE)
      req(uploadedTibble$data())
      
      df <- uploadedTibble$data()
      cols <- colnames(df)
      
      updatePickerInput(session, "response", choices = cols, selected = character(0))
      updatePickerInput(session, "predictors", choices = cols, selected = character(0))
      
      results_ready(FALSE)
      plots_ready(FALSE)
      calc_results(NULL)
      plot_results(NULL)
      cart_message(NULL)
    })
    
    # Keep response out of predictors
    observeEvent(input$response, {
      req(uploadedTibble$data())
      
      df <- uploadedTibble$data()
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
        uploadedTibble$data(),
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
        hideTab(inputId = "cartMainPanel", target = "uploaded_data_tab")
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
    
    output$maxDepthError <- renderUI({
      if (maxDepthError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Maximum Tree Depth is required and cannot be negative."
        )
      }
    })
    
    output$minSplitError <- renderUI({
      if (minSplitError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Minimum Split Size is required and cannot be negative."
        )
      }
    })
    
    output$cpError <- renderUI({
      if (cpError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Complexity Parameter is required and cannot be negative."
        )
      }
    })
    
    observeEvent(input$response, {
      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }
    })
    
    observeEvent(input$predictors, {
      if (length(input$predictors) >= 1) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    })
    
    observeEvent(input$max_depth, {
      valid <- !is.null(input$max_depth) && !is.na(input$max_depth) && input$max_depth >= 0
      if (valid) {
        maxDepthError(FALSE)
        shinyjs::removeClass(id = "maxDepthWrapper", class = "has-error")
      }
    })
    
    observeEvent(input$min_split, {
      valid <- !is.null(input$min_split) && !is.na(input$min_split) && input$min_split >= 0
      if (valid) {
        minSplitError(FALSE)
        shinyjs::removeClass(id = "minSplitWrapper", class = "has-error")
      }
    })
    
    observeEvent(input$cp, {
      valid <- !is.null(input$cp) && !is.na(input$cp) && input$cp >= 0
      if (valid) {
        cpError(FALSE)
        shinyjs::removeClass(id = "cpWrapper", class = "has-error")
      }
    })
    
    observeEvent(input$calculate, {
      if (!isTruthy(uploadedTibble$data())) {
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
      
      max_depth_valid <- !is.null(input$max_depth) && !is.na(input$max_depth) && input$max_depth >= 0
      min_split_valid <- !is.null(input$min_split) && !is.na(input$min_split) && input$min_split >= 0
      cp_valid <- !is.null(input$cp) && !is.na(input$cp) && input$cp >= 0
      
      if (!max_depth_valid) {
        maxDepthError(TRUE)
        shinyjs::addClass(id = "maxDepthWrapper", class = "has-error")
      } else {
        maxDepthError(FALSE)
        shinyjs::removeClass(id = "maxDepthWrapper", class = "has-error")
      }
      
      if (!min_split_valid) {
        minSplitError(TRUE)
        shinyjs::addClass(id = "minSplitWrapper", class = "has-error")
      } else {
        minSplitError(FALSE)
        shinyjs::removeClass(id = "minSplitWrapper", class = "has-error")
      }
      
      if (!cp_valid) {
        cpError(TRUE)
        shinyjs::addClass(id = "cpWrapper", class = "has-error")
      } else {
        cpError(FALSE)
        shinyjs::removeClass(id = "cpWrapper", class = "has-error")
      }
      
      if (!isTruthy(input$response) ||
          !isTruthy(input$predictors) ||
          length(input$predictors) < 1 ||
          !max_depth_valid ||
          !min_split_valid ||
          !cp_valid) {
        return()
      }
      
      df <- uploadedTibble$data()
      
      analysis_df <- df[, c(input$predictors, input$response), drop = FALSE]
      analysis_df <- na.omit(analysis_df)
      
      if (nrow(analysis_df) == 0) {
        showNotification("No complete cases remain after removing missing values.", type = "error", duration = 8)
        return()
      }
      
      analysis_df[[input$response]] <- as.factor(analysis_df[[input$response]])
      
      if (nlevels(analysis_df[[input$response]]) < 2) {
        showNotification("Response variable must have at least 2 classes.", type = "error", duration = 8)
        return()
      }
      
      if (nlevels(analysis_df[[input$response]]) > floor(nrow(analysis_df) / 2)) {
        showNotification(
          "The selected response variable has too many unique values to be treated as a categorical class variable for CART.",
          type = "error",
          duration = 8
        )
        return()
      }
      
      predictor_df <- analysis_df[, input$predictors, drop = FALSE]
      
      zero_var <- names(predictor_df)[sapply(predictor_df, function(x) {
        length(unique(x[!is.na(x)])) < 2
      })]
      
      if (length(zero_var) > 0) {
        cart_message(
          paste(
            "The following explanatory variable(s) have zero variance and cannot be used:",
            paste(zero_var, collapse = ", ")
          )
        )
        return()
      }
      
      cart_formula <- as.formula(
        paste(
          paste0("`", input$response, "`"),
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
        Actual = analysis_df[[input$response]],
        Predicted = cart_pred
      )
      
      accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
      
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
      }
      
      res <- list(
        fit = cart_fit,
        confusion = confusion_mat,
        accuracy = accuracy,
        response = input$response,
        predictors = input$predictors,
        n = nrow(analysis_df),
        importance = importance_df
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
        
        tagList(
          tags$h4("Model Information"),
          tableOutput(session$ns("cartModelInfo")),
          
          tags$h4("Confusion Matrix"),
          tableOutput(session$ns("confusionMatrixResults"))
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
            length(unique(analysis_df[[input$response]])),
            length(r$predictors),
            r$n,
            input$max_depth,
            input$min_split,
            input$cp,
            round(r$accuracy, 4)
          ),
          check.names = FALSE
        )
      }, rownames = FALSE)
      
      output$confusionMatrixResults <- renderTable({
        r <- calc_results()
        req(r)
        
        cm <- as.data.frame.matrix(r$confusion)
        cm$Actual <- rownames(cm)
        cm <- cm[, c("Actual", setdiff(names(cm), "Actual"))]
        rownames(cm) <- NULL
        cm
      }, rownames = FALSE)
      
      output$treePlot <- renderPlot({
        r <- plot_results()
        req(r)
        
        rpart.plot::rpart.plot(
          r$fit,
          main = "Decision Tree Diagram"
        )
      })
      
      output$varImportancePlot <- renderPlot({
        r <- plot_results()
        req(r)
        
        if (nrow(r$importance) == 0) {
          plot.new()
          text(0.5, 0.5, "No variable importance available for this model.")
          return()
        }
        
        imp_df <- r$importance
        imp_df$Variable <- factor(imp_df$Variable, levels = rev(imp_df$Variable))
        
        ggplot(imp_df, aes(x = Variable, y = Importance)) +
          geom_col(fill = "#18536F") +
          coord_flip() +
          labs(
            title = "Variable Importance",
            x = NULL,
            y = "Importance"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12)
          )
      })
      
      showTab(inputId = "cartMainPanel", target = "results_tab")
      showTab(inputId = "cartMainPanel", target = "plots_tab")
      showTab(inputId = "cartMainPanel", target = "uploaded_data_tab")
      
      shinyjs::delay(100, {
        updateNavbarPage(session, "cartMainPanel", selected = "results_tab")
      })
      
    }, ignoreInit = TRUE)
    
    observeEvent(input$reset, {
      hideTab(inputId = "cartMainPanel", target = "results_tab")
      hideTab(inputId = "cartMainPanel", target = "plots_tab")
      hideTab(inputId = "cartMainPanel", target = "uploaded_data_tab")
      
      results_ready(FALSE)
      plots_ready(FALSE)
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)
      
      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      maxDepthError(FALSE)
      minSplitError(FALSE)
      cpError(FALSE)
      cart_message(NULL)
      
      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      shinyjs::removeClass(id = "maxDepthWrapper", class = "has-error")
      shinyjs::removeClass(id = "minSplitWrapper", class = "has-error")
      shinyjs::removeClass(id = "cpWrapper", class = "has-error")
      
      updateNumericInput(session, "max_depth", value = 10)
      updateNumericInput(session, "min_split", value = 20)
      updateNumericInput(session, "cp", value = 0.01)
      
      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      
      updateNavbarPage(session, "cartMainPanel", selected = "data_import_tab")
    })
  })
}