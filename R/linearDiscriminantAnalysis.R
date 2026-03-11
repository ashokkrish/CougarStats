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

    actionButton(ns("calculate"), "Calculate LDA", class = "act-btn"),
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

    results_ready <- reactiveVal(FALSE)
    plots_ready <- reactiveVal(FALSE)

    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)

    calc_results <- reactiveVal(NULL)
    plot_results <- reactiveVal(NULL)

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
    })

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
            helpText("No results yet. Upload a dataset, choose variables, then click Calculate LDA.")
          ))
        }

        return(tagList(
          helpText("Settings changed. Click Calculate LDA to update results.")
        ))
      }

      uiOutput(session$ns("resultsUI"))
    })

    # Plots tab container
    output$plotsContainer <- renderUI({
      if (!isTRUE(plots_ready())) {

        if (!isTRUE(plots_ever_calculated())) {
          return(tagList(
            helpText("No plots yet. Upload a dataset, choose variables, then click Calculate LDA.")
          ))
        }

        return(tagList(
          helpText("Settings changed. Click Calculate LDA to update plots.")
        ))
      }

      tagList(
        tags$h4("LDA Plot"),
        plotOutput(session$ns("ldaPlot"), height = "500px")
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

      analysis_df[[input$response]] <- as.factor(analysis_df[[input$response]])

      predictor_df <- analysis_df[, input$predictors, drop = FALSE]
      numeric_check <- sapply(predictor_df, is.numeric)

      validate(
        need(all(numeric_check), "All predictor variables must be numeric for LDA."),
        need(nlevels(analysis_df[[input$response]]) >= 2, "Response variable must have at least 2 classes."),
        need(nlevels(analysis_df[[input$response]]) >= 3, "The LD1 vs LD2 plot requires at least 3 classes.")
      )

      lda_formula <- as.formula(
        paste(input$response, "~", paste(input$predictors, collapse = " + "))
      )

      lda_fit <- MASS::lda(lda_formula, data = analysis_df)
      lda_pred <- predict(lda_fit, analysis_df[, input$predictors, drop = FALSE])

      confusion_mat <- table(
        Actual = analysis_df[[input$response]],
        Predicted = lda_pred$class
      )

      accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)

      singular_vals_sq <- lda_fit$svd^2
      prop_trace <- singular_vals_sq / sum(singular_vals_sq)

      scores_df <- data.frame(
        LD1 = lda_pred$x[, 1],
        LD2 = lda_pred$x[, 2],
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