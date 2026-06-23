#R/principalComponentAnalysis.

library(shiny)
library(bslib)
library(dplyr)
library(car) # For powerTransform
library(ggplot2)
library(GGally) # For ggpairs
library(ggfortify) # For autoplot
library(moments) # For skewness
library(forecast) # For BoxCox.lambda and BoxCox function
library(gridExtra) # For arranging plots
library(knitr)
library(DT)
library(psych) # For rotated solution

# ============== UI ==============

PCASidebarUI <- function(id) {
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
        strong("Response Variable"),
        choices = NULL,
        multiple = TRUE,
        options = list(`live-search` = TRUE, title = "Optional", `max-options` = 1)
      )
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
    div(
      id = ns("transformationContainer"),
      style = "display: none;",
      radioButtons(ns("transformation"),
                   label = strong("Data Transformation"),
                   choices = c("Original scale",
                               "Logarithmic transformation",
                               "Box-Cox transformation",
                               "Standardized Box-Cox transformation"),
                   selected = "Original scale")
    ),
    div(
      id = ns("numFactorsContainer"),
      numericInput(ns("numFactors"), "Number of Factors", value = 2, min = 1, step = 1)
    ),
    div(
      id = ns("pcXContainer"),
      selectInput(ns("pcX"), "X-axis component", choices = NULL)
    ),
    div(
      id = ns("pcYContainer"),
      selectInput(ns("pcY"), "Y-axis component", choices = NULL)
    ),
    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

PCAMainPanelUI <- function(id) {
  ns <- NS(id)
  navbarPage(title = NULL,
             id = ns("mainPanel"),
             selected = "uploaded_data_tab",
             theme = bs_theme(version = 4),
               tabPanel(title = "Results",
                        value = "pca_results_tab",
                        h4("Principal Component Analysis Summary"),
                        tableOutput(ns("pcaSummary")),
                        hr(),
                        h4("Component Loadings (Unrotated)"),
                        tableOutput(ns("pcaLoadings")),
                        hr(),
                        h4("Component Loadings (Varimax Rotation)"),
                        tableOutput(ns("rotatedLoadings")),
                        hr(),
                        h4("Correlation Matrix"),
                        div(class = "pca-matrix-table",
                            tableOutput(ns("correlationMatrix"))),
                        hr(),
                        h4("Covariance Matrix"),
                        div(class = "pca-matrix-table",
                            tableOutput(ns("covarianceMatrix"))),
                        hr(),
                        h4("Eigenvalues"),
                        div(class = "pca-matrix-table",
                            tableOutput(ns("eigenvaluesTable"))),
                        hr(),
                        h4("Eigenvectors"),
                        div(class = "pca-matrix-table",
                            tableOutput(ns("eigenvectorsTable"))),
                        hr(),
                        uiOutput(ns("pcaInterpretation"))
               ),
               tabPanel(
                 title = "Plots",
                 value = "plots_tab",
                 
                 h3("Scree Plot"),
                 plotOutput(ns("screePlot"), height = "350px"),
                 hr(),
                 
                 h3("Score Plot"),
                 plotOutput(ns("scorePlot"), height = "450px"),
                 hr(),
                
                 h3("Biplot"),
                 plotOutput(ns("biplot"), height = "500px"),
                 hr(),
                 
                 h3("Loadings Heatmap"),
                 plotOutput(ns("loadingsHeatmap"), height = "450px")
               ),
               
               tabPanel(title = "Data Transformations", value = "transformations_tab", plotOutput(ns("transformationHistograms"))),
               
               tabPanel(
                 title = "Uploaded Data",
                 value = "uploaded_data_tab",
                 uiOutput(ns("uploadedDataContainer"))
               )
    )
}


# ============== SERVER ==============


PCAServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pca_results <- reactiveVal(NULL)
    analysis_data <- reactiveVal(NULL)
    original_data <- reactiveVal(NULL)
    noFileCalculate <- reactiveVal(FALSE)
    grouping_var <- reactiveVal(NULL)
    predictorsError <- reactiveVal(FALSE)
    pca_message <- reactiveVal(NULL)

    pca_iv <- shinyvalidate::InputValidator$new()
    pca_iv$add_rule("numFactors", shinyvalidate::sv_required())
    pca_iv$add_rule("numFactors", shinyvalidate::sv_gte(1, message = "Must be at least 1."))
    pca_iv$enable()
    
    session$onFlushed(function() {
      hideTab(inputId = "mainPanel", target = "pca_results_tab")
      hideTab(inputId = "mainPanel", target = "plots_tab")
      hideTab(inputId = "mainPanel", target = "transformations_tab")
      shinyjs::hide("numFactorsContainer")
      shinyjs::hide("pcXContainer")
      shinyjs::hide("pcYContainer")
      shinyjs::hide("transformationContainer")
    }, once = TRUE)
    
    observeEvent(data(), {
      df <- data()
      req(df)

      numeric_cols <- names(dplyr::select_if(df, is.numeric))
      all_cols <- colnames(df)

      pre_predictors <- intersect(shared_explanatory(), numeric_cols)
      shared_resp    <- shared_response()
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% all_cols) shared_resp else character(0)

      updatePickerInput(session, "predictors", choices = numeric_cols, selected = pre_predictors)
      updatePickerInput(session, "response",   choices = all_cols,     selected = pre_response)
    }, ignoreNULL = TRUE)
    
    observeEvent(input$predictors, {
      shared_explanatory(input$predictors)
      if (length(input$predictors) >= 2) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    }, ignoreInit = TRUE)
    observeEvent(input$response,   { shared_response(input$response)     }, ignoreInit = TRUE)

    # Clear results when user changes PCA options
    observeEvent(
      list(input$predictors, input$response, input$transformation),
      {
        pca_results(NULL)
        analysis_data(NULL)
        original_data(NULL)
        grouping_var(NULL)
        pca_message(NULL)

        hideTab(inputId = "mainPanel", target = "pca_results_tab")
        hideTab(inputId = "mainPanel", target = "plots_tab")
        hideTab(inputId = "mainPanel", target = "transformations_tab")
        shinyjs::hide("numFactorsContainer")
        shinyjs::hide("pcXContainer")
        shinyjs::hide("pcYContainer")
        updateNavbarPage(session, "mainPanel", selected = "uploaded_data_tab")
      },
      ignoreInit = TRUE
    )
    
    
    # ---- Uploaded Data container ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DT::DTOutput(ns("pcaUploadTable"))
      }
    })
    
    # ---- Uploaded Data table ----
    output$pcaUploadTable <- DT::renderDT({
      req(data())
      df <- data()
      
      DT::datatable(
        df,
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX = TRUE
        )
      )
    })
    
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(class = "shiny-output-error-validation",
                 "Required: Cannot calculate without a data file.")
      } else {
        msg <- pca_message()
        if (is.null(msg)) return(NULL)
        div(
          style = "margin-top:10px;",
          div(class = "alert alert-danger", msg)
        )
      }
    })

    output$predictorsError <- renderUI({
      if (predictorsError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Please select at least two explanatory variables."
        )
      }
    })
    
    observeEvent(input$calculate, {

      # Clear any previous calculation error on each new attempt
      pca_message(NULL)

      req(pca_iv$is_valid())

      if (!isTruthy(data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }

      if (!isTruthy(input$predictors) || length(input$predictors) < 2) {
        predictorsError(TRUE)
        shinyjs::addClass(id = "predictorsWrapper", class = "has-error")
        return()
      } else {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }

      if (input$numFactors > length(input$predictors)) {
        pca_message("Number of factors cannot exceed the number of selected variables.")
        return()
      }

      tryCatch({

        df <- data()

        group_col <- if (isTruthy(input$response)) input$response else NULL

        row_ok <- complete.cases(df[, input$predictors, drop = FALSE])
        selected_data <- df[row_ok, input$predictors, drop = FALSE]
        selected_data <- as.data.frame(selected_data)

        if (!is.null(group_col)) {
          grp <- df[row_ok, group_col, drop = TRUE]
          grp <- as.character(grp)
          grp[is.na(grp)] <- "Missing"

          n_groups <- length(unique(grp))
          if (n_groups <= 12) {
            grouping_var(factor(grp))
          } else {
            grouping_var(NULL)
          }
        } else {
          grouping_var(NULL)
        }

        selected_data[] <- lapply(selected_data, function(x) as.numeric(as.character(x)))

        keep <- complete.cases(selected_data)
        selected_data <- selected_data[keep, , drop = FALSE]
        if (!is.null(group_col)) grouping_var(grouping_var()[keep])

        if (nrow(selected_data) < 2) {
          pca_message("After removing missing values, there are fewer than 2 complete rows. PCA needs at least 2 rows.")
          return()
        }

        original_data(selected_data)

        transformed_data <- switch(
          input$transformation,
          "Original scale" = selected_data,
          "Logarithmic transformation" = {
            if (any(selected_data < 0)) {
              pca_message("Log transformation cannot be applied to negative data.")
              return(NULL)
            }
            if (any(selected_data == 0)) {
              log(selected_data + 1)
            } else {
              log(selected_data)
            }
          },
          "Box-Cox transformation" = { ... },
          "Standardized Box-Cox transformation" = { ... }
        )

        if (is.null(transformed_data)) return()

        analysis_data(transformed_data)

        sds <- sapply(transformed_data, sd, na.rm = TRUE)
        zero_var_cols <- names(sds)[is.na(sds) | sds == 0]
        if (length(zero_var_cols) > 0) {
          pca_message(paste0(
            "These selected variable(s) have zero variance and cannot be used in PCA: ",
            paste(zero_var_cols, collapse = ", "), "."
          ))
          return()
        }

        pca <- prcomp(transformed_data, center = TRUE, scale. = TRUE)
        pca_results(pca)

        pcs <- colnames(pca$x)
        updateSelectInput(session, "pcX", choices = pcs, selected = pcs[1])
        updateSelectInput(session, "pcY", choices = pcs, selected = pcs[min(2, length(pcs))])
        updateNumericInput(session, "numFactors", value = min(input$numFactors, length(pcs)), max = length(pcs))

        shinyjs::show("numFactorsContainer")
        shinyjs::show("pcXContainer")
        shinyjs::show("pcYContainer")

        showTab(inputId = "mainPanel", target = "pca_results_tab")
        showTab(inputId = "mainPanel", target = "plots_tab")

        updateNavbarPage(session, "mainPanel", selected = "pca_results_tab")

      }, error = function(e) {
        pca_message(paste("PCA error:", conditionMessage(e)))
      })

    })
    
    
    # --- Render Outputs ---
    
    output$scorePlot <- renderPlot({
      req(pca_results(), input$pcX, input$pcY)
      
      scores <- as.data.frame(pca_results()$x)
      validate(
        need(input$pcX %in% names(scores), "Invalid X component."),
        need(input$pcY %in% names(scores), "Invalid Y component."),
        need(input$pcX != input$pcY, "Choose two different components.")
      )
      
      ggplot(scores, aes(x = .data[[input$pcX]], y = .data[[input$pcY]])) +
        geom_point() +
        
        geom_hline(yintercept = 0, linewidth = 0.8) +
        geom_vline(xintercept = 0, linewidth = 0.8) +
        
        labs(title = "Score Plot", x = input$pcX, y = input$pcY) +
        theme_minimal() +
        theme(
          plot.title  = element_text(face = "bold", size = 18, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(face = "bold", size = 14)
        )
    })
    
    output$biplot <- renderPlot({
      req(pca_results(), input$pcX, input$pcY)
      req(input$mainPanel == "plots_tab")
      
      # Make sure the output has a real size (prevents zero-dimension viewport)
      w <- session$clientData[[paste0("output_", ns("biplot"), "_width")]]
      h <- session$clientData[[paste0("output_", ns("biplot"), "_height")]]
      req(!is.null(w), !is.null(h), w > 10, h > 10)
      
      ax1 <- as.integer(sub("PC", "", input$pcX))
      ax2 <- as.integer(sub("PC", "", input$pcY))
      validate(
        need(!is.na(ax1) && !is.na(ax2), "Invalid components selected."),
        need(ax1 != ax2, "Choose two different components.")
      )
      
      grp <- grouping_var()
      n_ind <- nrow(pca_results()$x)
      valid_grp <- !is.null(grp) && is.atomic(grp) && length(grp) == n_ind
      
      args <- list(
        X = pca_results(),
        axes = c(ax1, ax2),
        geom.ind = "point",
        pointshape = 19,
        repel = FALSE
      )
      
      if (valid_grp) {
        grp <- as.factor(grp)
        counts <- table(grp)
        args$habillage <- grp
        args$addEllipses <- all(counts >= 3)
      }
      
      p <- do.call(factoextra::fviz_pca_biplot, args)
      
      p +
        labs(
          title = "PCA - Biplot",
          x = input$pcX,
          y = input$pcY
        ) +
        theme_minimal() +
        theme(
          plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title.x    = element_text(face = "bold", size = 14),
          axis.title.y    = element_text(face = "bold", size = 14),
          axis.text.x     = element_text(face = "bold", size = 12),
          axis.text.y     = element_text(face = "bold", size = 12),
          legend.position = "bottom"
        )
    }, res = 96)

    output$loadingsHeatmap <- renderPlot({
      req(pca_results(), input$numFactors)
      
      loadings <- as.data.frame(pca_results()$rotation)
      
      k <- min(input$numFactors, ncol(loadings))
      validate(need(k >= 1, "Choose at least 1 factor."))
      
      loadings_k <- loadings[, 1:k, drop = FALSE]
      
      loadings_melted <- reshape2::melt(as.matrix(loadings_k))
      colnames(loadings_melted) <- c("Variable", "PC", "Loading")
      
      loadings_melted$PC <- factor(
        loadings_melted$PC,
        levels = unique(loadings_melted$PC)
      )
      
      ggplot(loadings_melted, aes(x = PC, y = Variable, fill = Loading)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient2(
          low = "#2E9DFD",
          mid = "white",
          high = "#FC4E07",
          midpoint = 0
        ) +
        geom_text(aes(label = round(Loading, 2)), color = "black", size = 3) +
        labs(
          title = "PCA Loadings Heatmap",
          x = "Principal Component",
          y = "Variables",
          fill = "Loading Value"
        ) +
        theme_minimal() +
        theme(
          plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
          axis.title.x    = element_text(face = "bold", size = 14),
          axis.title.y    = element_text(face = "bold", size = 14),
          axis.text       = element_text(face = "bold", size = 12, colour = "black"),
          legend.position = "bottom"
        )
    }, res = 96)
    
    
    output$pcaSummary <- renderTable({
      req(pca_results())
      summary_df <- as.data.frame(summary(pca_results())$importance)[, 1:input$numFactors, drop = FALSE]
      round(summary_df, 3)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)

    output$pcaLoadings <- renderTable({
      req(pca_results())
      loadings_df <- as.data.frame(pca_results()$rotation)[, 1:input$numFactors, drop = FALSE]
      round(loadings_df, 3)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)
    
    output$pcaInterpretation <- renderUI({
      req(pca_results())

      summary_data  <- summary(pca_results())$importance
      loadings_data <- pca_results()$rotation

      cum_var_pct <- round(summary_data["Cumulative Proportion", input$numFactors] * 100, 2)
      variance_label <- if (cum_var_pct >= 80) "excellent"
                         else if (cum_var_pct >= 70) "good"
                         else if (cum_var_pct >= 50) "moderate"
                         else "limited"

      # Per-component loading detail, characterizing each retained component
      # by the variable that influences it most strongly.
      loading_details <- lapply(seq_len(input$numFactors), function(i) {
        component_name     <- colnames(loadings_data)[i]
        component_loadings <- loadings_data[, i]

        top_variable_index <- which.max(abs(component_loadings))
        top_variable_name  <- rownames(loadings_data)[top_variable_index]
        top_loading_value  <- round(component_loadings[top_variable_index], 3)

        tags$p(
          style = "margin-bottom: 6px;",
          tags$strong(component_name, ": "),
          "Most strongly influenced by ", tags$strong(top_variable_name),
          " with a loading of ", tags$strong(top_loading_value),
          ". Variables with high absolute loadings (close to 1 or -1) are important for ",
          "interpreting this component."
        )
      })

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
            "Principal Component Analysis reduces ", nrow(loadings_data), " variables down to ",
            ncol(loadings_data), " uncorrelated components, ranked by how much variance each ",
            "one captures."
          )
        ),

        tags$p(tags$strong("Variance Explained"), style = "margin-bottom: 6px;"),
        tags$p(
          style = "margin-bottom: 4px;",
          tags$strong("Standard deviation: "),
          "The amount of variance in the data captured by each component. Higher values mean ",
          "more variance is captured."
        ),
        tags$p(
          style = "margin-bottom: 4px;",
          tags$strong("Proportion of Variance: "),
          "The percentage of total variance accounted for by each individual component."
        ),
        tags$p(
          style = "margin-bottom: 8px;",
          tags$strong("Cumulative Proportion: "),
          "The running total of variance explained as components are added. A common rule of ",
          "thumb is to retain enough components to explain 70-80% of total variance — the Scree ",
          "Plot's \"elbow\" point is a visual way to confirm this cutoff."
        ),
        tags$p(
          style = "margin-bottom: 12px;",
          paste0(
            "Your selected ", input$numFactors, " component", if (input$numFactors != 1) "s" else "",
            " together explain ", cum_var_pct, "% of the total variance, which is ", variance_label,
            " coverage."
          )
        ),

        tags$p(tags$strong("Component Loadings"), style = "margin-bottom: 6px;"),
        tags$p(
          style = "margin-bottom: 8px;",
          "Loadings are the correlations between the original variables and the principal ",
          "components. They indicate how much each original variable contributes to each component."
        ),
        loading_details
      )
    })
    
    output$transformationHistograms <- renderPlot({
      req(analysis_data(), original_data())
      
      orig_data_long <- original_data() %>%
        tidyr::gather(key = "variable", value = "value")
      
      trans_data_long <- analysis_data() %>%
        tidyr::gather(key = "variable", value = "value")
      
      p1 <- ggplot(orig_data_long, aes(x = value)) +
        geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
        facet_wrap(~variable, scales = "free") +
        ggtitle("Original Data")
      
      p2 <- ggplot(trans_data_long, aes(x = value)) +
        geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
        facet_wrap(~variable, scales = "free") +
        ggtitle(paste("Transformed Data:", input$transformation))
      
      gridExtra::grid.arrange(p1, p2, ncol = 1)
    })
    
    output$correlationMatrix <- renderTable({
      req(analysis_data())
      round(cor(analysis_data()), 4)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE, align = "c")

    output$covarianceMatrix <- renderTable({
      req(analysis_data())
      round(cov(analysis_data()), 4)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE, align = "c")

    output$eigenvaluesTable <- renderTable({
      req(pca_results())

      eigenvalues <- pca_results()$sdev^2
      prop_var    <- eigenvalues / sum(eigenvalues)
      cum_var     <- cumsum(prop_var)

      data.frame(
        Component = paste0("PC", seq_along(eigenvalues)),
        Eigenvalue = round(eigenvalues, 4),
        `Proportion of Variance` = round(prop_var, 4),
        `Cumulative Proportion` = round(cum_var, 4),
        check.names = FALSE
      )
    }, rownames = FALSE, striped = TRUE, bordered = TRUE, align = "c")

    output$eigenvectorsTable <- renderTable({
      req(pca_results())

      loadings <- pca_results()$rotation
      df <- as.data.frame(round(loadings, 4))
      df <- cbind(Variable = rownames(loadings), df)
      rownames(df) <- NULL
      df
    }, rownames = FALSE, striped = TRUE, bordered = TRUE, align = "c")

    output$screePlot <- renderPlot({
      req(pca_results())
      
      suppressWarnings(
        factoextra::fviz_eig(
          pca_results(),
          addlabels = TRUE,
          ylim = c(0, 100),
          main = "Scree Plot: Variance Explained by Principal Components",
          xlab = "Principal Components",
          ylab = "Percentage of Explained Variance",
          barfill = "#2E9DFD",
          barcolor = "#2E9DFD",
          linecolor = "#FC4E07"
        ) +
          theme_minimal() +
          theme(
            plot.title   = element_text(face = "bold", size = 18, hjust = 0.5),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.text.x  = element_text(size = 12),
            axis.text.y  = element_text(face = "bold", size = 12),
            panel.border = element_blank(),
            axis.line.x  = element_line(colour = "black", linewidth = 1),
            axis.line.y  = element_line(colour = "black", linewidth = 1)
          )
      )
    }, res = 96)
    
    output$rotatedLoadings <- renderTable({
      req(analysis_data())

      validate(
        need(input$numFactors < ncol(analysis_data()),
             "Number of factors must be less than the number of selected variables for rotated PCA.")
      )

      cor_mat <- cor(analysis_data(), use = "complete.obs")
      cor_mat <- suppressWarnings(psych::cor.smooth(cor_mat))

      rotated_pca <- suppressWarnings(
        psych::principal(
          cor_mat,
          nfactors = input$numFactors,
          rotate = "varimax",
          scores = FALSE
        )
      )

      rotated_loadings_df <- as.data.frame(unclass(rotated_pca$loadings))
      round(rotated_loadings_df, 3)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)
    
    observeEvent(input$reset, {
      hideTab(inputId = "mainPanel", target = "pca_results_tab")
      hideTab(inputId = "mainPanel", target = "transformations_tab")
      hideTab(inputId = "mainPanel", target = "plots_tab")
      shinyjs::hide("numFactorsContainer")
      shinyjs::hide("pcXContainer")
      shinyjs::hide("pcYContainer")

      pca_results(NULL)
      analysis_data(NULL)
      original_data(NULL)
      noFileCalculate(FALSE)
      predictorsError(FALSE)
      pca_message(NULL)
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")

      updatePickerInput(session, "predictors", selected = character(0))
      updatePickerInput(session, "response", selected = character(0))
      updateRadioButtons(session, "transformation", selected = "Original scale")
      updateNumericInput(session, "numFactors", value = 2)
      updateSelectInput(session, "pcX", choices = character(0))
      updateSelectInput(session, "pcY", choices = character(0))

      updateNavbarPage(session, "mainPanel", selected = "uploaded_data_tab")
    })
    
    observe({
      outputOptions(output, "biplot", suspendWhenHidden = TRUE)
      outputOptions(output, "scorePlot", suspendWhenHidden = TRUE)
      outputOptions(output, "screePlot", suspendWhenHidden = TRUE)
      outputOptions(output, "loadingsHeatmap", suspendWhenHidden = TRUE)
    })
    
  })
}

