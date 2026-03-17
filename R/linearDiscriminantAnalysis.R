# R/linearDiscriminantAnalysis.R

LDASidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    pickerInput(
      ns("response"),
      strong("Response Variable (Class)"),
      choices = NULL,
      multiple = FALSE,
      options = list(`live-search` = TRUE, title = "Nothing selected")
    ),

    pickerInput(
      ns("predictors"),
      strong("Predictor Variables"),
      choices = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Nothing selected")
    ),

    checkboxInput(
      ns("useCV"),
      "Use Leave-One-Out Cross-Validation",
      value = FALSE
    ),

    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

LDAMainPanelUI <- function(id) {
  ns <- NS(id)

  tagList(
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

      id = ns("ldaMainPanel"),
      theme = bs_theme(version = 4)
    )
  )
}

LDAServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    uploadedTibble <- import_file_server(
      id = "dataImport",
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class = "tbl_df"
    )
    ##################################################
    prepare_lda_response <- function(x) {
      x_no_na <- x[!is.na(x)]
      
      if (length(x_no_na) == 0) {
        return(NULL)
      }
      
      # If already categorical-like, just convert to factor
      if (is.factor(x) || is.character(x) || is.logical(x)) {
        return(as.factor(x))
      }
      
      # If numeric/integer, allow coded categories like 1,2,3
      if (is.numeric(x) || is.integer(x)) {
        unique_vals <- unique(x_no_na)
        
        # Optional safety check:
        # if every row is basically its own class, this is probably not a categorical response
        if (length(unique_vals) < 2) {
          return(as.factor(x))
        }
        
        return(factor(x, levels = sort(unique_vals)))
      }
      
      # Fallback
      as.factor(x)
    }

    results_ready <- reactiveVal(FALSE)
    plots_ready <- reactiveVal(FALSE)

    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)

    calc_results <- reactiveVal(NULL)
    plot_results <- reactiveVal(NULL)
    lda_message <- reactiveVal(NULL)
    
    # Uploaded Data tab
    output$uploadedDataContainer <- renderUI({
      if (is.null(uploadedTibble$data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DTOutput(session$ns("ldaUploadTable"))
      }
    })

    output$ldaUploadTable <- renderDT({
      req(uploadedTibble$data())

      datatable(
        uploadedTibble$data(),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX = TRUE
        )
      )
    })

    # Populate response/predictor choices
    observeEvent(uploadedTibble$data(), {
      req(uploadedTibble$data())

      df <- uploadedTibble$data()
      cols <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]

      updatePickerInput(session, "response", choices = cols, selected = character(0))
      updatePickerInput(session, "predictors", choices = numeric_cols, selected = character(0))

      results_ready(FALSE)
      plots_ready(FALSE)
      calc_results(NULL)
      plot_results(NULL)
      lda_message(NULL)
    })
    
    observeEvent(input$response, {
      req(uploadedTibble$data())
      
      df <- uploadedTibble$data()
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
        uploadedTibble$data(),
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
      msg <- lda_message()
      
      if (is.null(msg)) return(NULL)
      
      div(
        style = "margin-top:10px;",
        div(
          class = "alert alert-danger",
          msg
        )
      )
    })

    # Main calculation
    observeEvent(input$calculate, {
      req(uploadedTibble$data())
      req(isTruthy(input$response))
      req(isTruthy(input$predictors))
      req(length(input$predictors) >= 1)

      df <- uploadedTibble$data()

      analysis_df <- df[, c(input$predictors, input$response), drop = FALSE]
      analysis_df <- na.omit(analysis_df)
      
      analysis_df[[input$response]] <- prepare_lda_response(analysis_df[[input$response]])
      
      if (is.null(analysis_df[[input$response]])) {
        showNotification("Response variable could not be prepared for LDA.", type = "error", duration = 8)
        return()
      }
      
      predictor_df <- analysis_df[, input$predictors, drop = FALSE]
      numeric_check <- sapply(predictor_df, is.numeric)
      
      class_counts <- table(analysis_df[[input$response]])
      
      if (nlevels(analysis_df[[input$response]]) > floor(nrow(analysis_df) / 2)) {
        showNotification(
          "The selected response variable has too many unique values to be treated as a categorical class variable for LDA.",
          type = "error",
          duration = 8
        )
        return()
      }
      
      if (nlevels(analysis_df[[input$response]]) > floor(nrow(analysis_df) / 2)) {
        showNotification(
          "The selected response variable has too many unique values to be treated as a categorical class variable for LDA.",
          type = "error",
          duration = 8
        )
        return()
      }
      
      if (!all(numeric_check)) {
        showNotification("All predictor variables must be numeric for LDA.", type = "error", duration = 8)
        return()
      }
      
      if (nlevels(analysis_df[[input$response]]) < 2) {
        showNotification("Response variable must have at least 2 classes.", type = "error", duration = 8)
        return()
      }
      
      if (length(input$predictors) < 1) {
        showNotification("Select at least one predictor.", type = "error", duration = 8)
        return()
      }
      
      if (any(class_counts < 2)) {
        lda_message(
          "Each class must have at least 2 observations for LDA. One or more classes in your selected response variable have fewer than 2 rows."
        )
        return()
        return()
      }

      lda_formula <- as.formula(
        paste(input$response, "~", paste(input$predictors, collapse = " + "))
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
        Actual = analysis_df[[input$response]],
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
        Class = analysis_df[[input$response]]
      )

      cv_confusion <- NULL
      cv_accuracy <- NULL

      if (isTRUE(input$useCV)) {
        lda_cv <- MASS::lda(lda_formula, data = analysis_df, CV = TRUE)

        cv_confusion <- table(
          Actual = analysis_df[[input$response]],
          Predicted = lda_cv$class
        )

        cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
      }

      res <- list(
        fit = lda_fit,
        confusion = confusion_mat,
        accuracy = accuracy,
        prop_trace = prop_trace,
        scores = scores_df,
        cv_confusion = cv_confusion,
        cv_accuracy = cv_accuracy,
        response = input$response,
        predictors = input$predictors,
        n = nrow(analysis_df)
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

        tagList(
          tags$h4("Model Information"),
          tableOutput(session$ns("ldaModelInfo")),
          
          tags$h4("Prior Probabilities"),
          tableOutput(session$ns("ldaPriors")),

          tags$h4("Group Means"),
          tableOutput(session$ns("groupMeans")),

          tags$h4("Coefficients of Linear Discriminants"),
          tableOutput(session$ns("coefficients")),

          tags$h4("Proportion of Trace"),
          tableOutput(session$ns("propTrace")),

          tags$h4("Confusion Matrix"),
          tableOutput(session$ns("confusionMatrix"))
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
      }, rownames = FALSE)
      
      output$ldaPriors <- renderTable({
        r <- calc_results()
        req(r)
        
        data.frame(
          Class = names(r$fit$prior),
          Prior_Probability = round(as.numeric(r$fit$prior), 4),
          check.names = FALSE
        )
      }, rownames = FALSE)

      output$groupMeans <- renderTable({
        r <- calc_results()
        req(r)
        round(r$fit$means, 4)
      }, rownames = TRUE)

      output$coefficients <- renderTable({
        r <- calc_results()
        req(r)
        round(r$fit$scaling, 4)
      }, rownames = TRUE)

      output$propTrace <- renderTable({
        r <- calc_results()
        req(r)
        
        data.frame(
          Discriminant = paste0("LD", seq_along(r$prop_trace)),
          Proportion = round(r$prop_trace, 4),
          check.names = FALSE
        )
      }, rownames = FALSE)

      output$confusionMatrix <- renderTable({
        r <- calc_results()
        req(r)
        
        cm <- if (isTRUE(input$useCV) && !is.null(r$cv_confusion)) {
          as.data.frame.matrix(r$cv_confusion)
        } else {
          as.data.frame.matrix(r$confusion)
        }
        
        cm$Actual <- rownames(cm)
        cm <- cm[, c("Actual", setdiff(names(cm), "Actual"))]
        rownames(cm) <- NULL
        cm
      }, rownames = FALSE)

      output$ldaPlot <- renderPlot({
        r <- plot_results()
        req(r)

        ggplot(r$scores, aes(x = LD1, y = LD2, color = Class)) +
          geom_point(size = 3, alpha = 0.8) +
          labs(
            title = "LDA Plot",
            x = "LD1",
            y = "LD2",
            color = "Class"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold")
          )
      })

      updateNavbarPage(session, "ldaMainPanel", selected = "results_tab")

    }, ignoreInit = TRUE)

    # Reset
    observeEvent(input$reset, {
      results_ready(FALSE)
      plots_ready(FALSE)
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)

      calc_results(NULL)
      plot_results(NULL)

      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNavbarPage(session, "ldaMainPanel", selected = "data_import_tab")
    })
  })
}