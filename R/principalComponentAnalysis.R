#R/principalComponentAnalysis.

library(shiny)
library(datamods)
library(bslib)
library(dplyr)
library(car) # For powerTransform
library(shinyalert)
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
    div(
      class = "si-label",
      tags$b("PCA Options")
    ),
    pickerInput(
      ns("pcaVariables"),
      "Variables for PCA",
      choices = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        title = "Nothing selected"
      )
    ),
    radioButtons(ns("transformation"),
                 label = "Data Transformation",
                 choices = c("Original scale",
                             "Logarithmic transformation",
                             "Box-Cox transformation",
                             "Standardized Box-Cox transformation"),
                 selected = "Original scale"),
    numericInput(ns("numFactors"), "Number of Factors", value = 2, min = 1, step = 1),
    selectInput(ns("pcX"), "X-axis component", choices = NULL),
    selectInput(ns("pcY"), "Y-axis component", choices = NULL),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

PCAMainPanelUI <- function(id) {
  ns <- NS(id)
  navbarPage(title = NULL,
             id = ns("mainPanel"),
             selected = "data_import_tab",
             theme = bs_theme(version = 4),
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
               tabPanel(title = "Results",
                        value = "pca_results_tab",
                        h3("Principal Component Analysis Summary"),
                        DTOutput(ns("pcaSummary")),
                        hr(),
                        h3("Component Loadings (Unrotated)"),
                        DTOutput(ns("pcaLoadings")),
                        hr(),
                        h3("Component Loadings (Varimax Rotation)"),
                        DTOutput(ns("rotatedLoadings")),
                        hr(),
                        h3("Correlation Matrix"),
                        DTOutput(ns("correlationMatrix")),
                        hr(),
                        h3("Covariance Matrix"),
                        DTOutput(ns("covarianceMatrix")),
                        #hr(),
                        #h3("Scree Plot"),
                        #plotOutput(ns("screePlot")),
                        hr(),
                        h3("Interpretation of Results"),
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


PCAServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    imported_data <- import_file_server(
      id = "dataImport",
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class = "tbl_df"
    )
    
    pca_results <- reactiveVal(NULL)
    analysis_data <- reactiveVal(NULL)
    original_data <- reactiveVal(NULL)
    noFileCalculate <- reactiveVal(FALSE)
    grouping_var <- reactiveVal(NULL)
    
    session$onFlushed(function() {
      hideTab(inputId = "mainPanel", target = "pca_results_tab")
      hideTab(inputId = "mainPanel", target = "plots_tab")
      hideTab(inputId = "mainPanel", target = "transformations_tab")
      hideTab(inputId = "mainPanel", target = "uploaded_data_tab")
    }, once = TRUE)
    
    observeEvent(imported_data$data(), {
      df <- imported_data$data()
      req(df)
      
      numeric_cols <- names(dplyr::select_if(df, is.numeric))
      updatePickerInput(session, "pcaVariables", choices = numeric_cols)
    })
    
    # Clear results when user changes PCA options
    observeEvent(
      list(input$pcaVariables, input$transformation, input$numFactors),
      {
        pca_results(NULL)
        analysis_data(NULL)
        original_data(NULL)
        
        hideTab(inputId = "mainPanel", target = "pca_results_tab")
        hideTab(inputId = "mainPanel", target = "plots_tab")
        hideTab(inputId = "mainPanel", target = "transformations_tab")
        hideTab(inputId = "mainPanel", target = "uploaded_data_tab")
      },
      ignoreInit = TRUE
    )
    
    
    # ---- Uploaded Data container ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(imported_data$data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DT::DTOutput(ns("pcaUploadTable"))
      }
    })
    
    # ---- Uploaded Data table ----
    output$pcaUploadTable <- DT::renderDT({
      req(imported_data$data())
      df <- imported_data$data()
      
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
        NULL
      }
    })
    
    observeEvent(input$calculate, {
      
      tryCatch({
        
        if (!isTruthy(imported_data$data())) {
          noFileCalculate(TRUE)
          return()
        } else {
          noFileCalculate(FALSE)
        }
        
        if (!isTruthy(input$pcaVariables) || length(input$pcaVariables) < 2) {
          shinyalert::shinyalert("Input Error", "Please select at least two variables for PCA.",
                                 type = "error", confirmButtonCol = "#18536F")
          return()
        }
        
        if (input$numFactors > length(input$pcaVariables)) {
          shinyalert::shinyalert("Input Error", "Number of factors cannot exceed the number of selected variables.",
                                 type = "error", confirmButtonCol = "#18536F")
          return()
        }
        
        df <- imported_data$data()
        
        non_num <- names(df)[!sapply(df, is.numeric)]
        group_col <- if (length(non_num) > 0) non_num[1] else NULL
        
        row_ok <- complete.cases(df[, input$pcaVariables, drop = FALSE])
        selected_data <- df[row_ok, input$pcaVariables, drop = FALSE]
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
          shinyalert::shinyalert(
            "Input Error",
            "After removing missing values, there are fewer than 2 complete rows. PCA needs at least 2 rows.",
            type = "error",
            confirmButtonCol = "#18536F"
          )
          return()
        }
        
        original_data(selected_data)
        
        transformed_data <- switch(
          input$transformation,
          "Original scale" = selected_data,
          "Logarithmic transformation" = {
            if (any(selected_data < 0)) {
              shinyalert::shinyalert("Error", "Log transformation cannot be applied to negative data.",
                                     type = "error", confirmButtonCol = "#18536F")
              return(NULL)
            }
            if (any(selected_data == 0)) {
              shinyalert::shinyalert("Info", "Data contains zeros. Applying a log(x+1) transformation.",
                                     type = "info", confirmButtonCol = "#18536F")
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
          shinyalert::shinyalert(
            "Input Error",
            paste0("These selected variables have zero variance and cannot be used in PCA: ",
                   paste(zero_var_cols, collapse = ", "), "."),
            type = "error",
            confirmButtonCol = "#18536F"
          )
          return()
        }
        
        pca <- prcomp(transformed_data, center = TRUE, scale. = TRUE)
        pca_results(pca)
        
        pcs <- colnames(pca$x)
        updateSelectInput(session, "pcX", choices = pcs, selected = pcs[1])
        updateSelectInput(session, "pcY", choices = pcs, selected = pcs[min(2, length(pcs))])
        
        showTab(inputId = "mainPanel", target = "pca_results_tab")
        showTab(inputId = "mainPanel", target = "plots_tab")
        showTab(inputId = "mainPanel", target = "transformations_tab")
        showTab(inputId = "mainPanel", target = "uploaded_data_tab")
        
        updateNavbarPage(session, "mainPanel", selected = "pca_results_tab")
        
      }, error = function(e) {
        shinyalert::shinyalert(
          "PCA crashed",
          paste("Error message:", conditionMessage(e)),
          type = "error",
          confirmButtonCol = "#18536F"
        )
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
          axis.text   = element_text(size = 12)
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
      
      # Only repel if the plot area is reasonably sized
      use_repel <- (w >= 300 && h >= 300)
      
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
      
      p + ggplot2::labs(x = input$pcX, y = input$pcY)
    }, res = 96)
    
    output$loadingsHeatmap <- renderPlot({
      req(pca_results())
      
      L <- as.data.frame(pca_results()$rotation)
      
      L <- L[, 1:min(10, ncol(L)), drop = FALSE]
      
      df_long <- data.frame(
        Feature = rep(rownames(L), times = ncol(L)),
        Component = rep(colnames(L), each = nrow(L)),
        Loading = as.vector(as.matrix(L))
      )
      
      ggplot(df_long, aes(x = Component, y = Feature, fill = Loading)) +
        geom_tile() +
        geom_text(aes(label = round(Loading, 2)), size = 3) +
        labs(title = "Loadings Heatmap", x = NULL, y = NULL) +
        theme_minimal()
    })
    
    
    output$pcaSummary <- renderDT({
      req(pca_results())
      summary_df <- as.data.frame(summary(pca_results())$importance)[, 1:input$numFactors, drop = FALSE]
      datatable(summary_df, options = list(dom = 't')) %>% formatRound(columns = 1:ncol(summary_df), digits = 3)
    })
    
    output$pcaLoadings <- renderDT({
      req(pca_results())
      loadings_df <- as.data.frame(pca_results()$rotation)[, 1:input$numFactors, drop = FALSE]
      datatable(loadings_df, options = list(dom = 't')) %>% formatRound(columns = 1:ncol(loadings_df), digits = 3)
    })
    
    output$pcaInterpretation <- renderUI({
      req(pca_results())
      
      summary_data <- summary(pca_results())$importance
      loadings_data <- pca_results()$rotation
      
      # Interpretation of Summary
      interpretation_summary <- tagList(
        h4("Importance of Components"),
        p(strong("Standard deviation:"), " This measures the amount of variance in the data explained by each principal component. Higher values mean more variance is captured."),
        p(strong("Proportion of Variance:"), " This shows the percentage of the total variance that is accounted for by each principal component."),
        p(strong("Cumulative Proportion:"), " This is the cumulative sum of the proportion of variance. It helps in deciding how many components to retain. A common rule of thumb is to select enough components to explain 70-80% of the total variance.")
      )
      
      # Interpretation of Loadings
      interpretation_loadings <- tagList(
        h4("Component Loadings"),
        p("Loadings are the correlations between the original variables and the principal components. They indicate how much each original variable contributes to each component.")
      )
      
      loading_details <- lapply(1:input$numFactors, function(i) {
        component_name <- colnames(loadings_data)[i]
        component_loadings <- loadings_data[, i]
        
        # Find the variable with the highest absolute loading
        top_variable_index <- which.max(abs(component_loadings))
        top_variable_name <- rownames(loadings_data)[top_variable_index]
        top_loading_value <- round(component_loadings[top_variable_index], 3)
        
        # Characterize the component
        p(strong(component_name, ":"), 
          " This component is most strongly influenced by ", 
          strong(top_variable_name), 
          " with a loading of ", 
          strong(top_loading_value), 
          ". Variables with high absolute loadings (close to 1 or -1) are important for interpreting this component.")
      })
      
      tagList(interpretation_summary, interpretation_loadings, loading_details)
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
    
    output$correlationMatrix <- renderDT({
      req(analysis_data())
      cor_matrix <- cor(analysis_data())
      datatable(cor_matrix, options = list(dom = 't')) %>% formatRound(columns = 1:ncol(cor_matrix), digits = 3)
    })
    
    output$covarianceMatrix <- renderDT({
      req(analysis_data())
      cov_matrix <- cov(analysis_data())
      datatable(cov_matrix, options = list(dom = 't')) %>% formatRound(columns = 1:ncol(cov_matrix), digits = 3)
    })
    
    output$screePlot <- renderPlot({
      req(pca_results())
      
      scree_data <- data.frame(
        Component = 1:length(pca_results()$sdev),
        Variance = pca_results()$sdev^2
      )
      
      ggplot(scree_data, aes(x = Component, y = Variance)) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        
        geom_hline(yintercept = 0, linewidth = 0.8) +
        geom_vline(xintercept = 0, linewidth = 0.8) +
        
        scale_x_continuous(
          limits = c(0, max(scree_data$Component) + 0.5),
          breaks = scree_data$Component
        ) +
        
        labs(
          title = "Scree Plot",
          x = "Principal Component",
          y = "Eigenvalue (Variance)"
        ) +
        theme_minimal() +
        theme(
          plot.title  = element_text(face = "bold", size = 18, hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 14),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text   = element_text(size = 12)
        )
    })
    
    output$rotatedLoadings <- renderDT({
      req(analysis_data())
      
      # Perform PCA with rotation using psych::principal
      rotated_pca <- psych::principal(analysis_data(), nfactors = input$numFactors, rotate = "varimax")
      
      # Extract rotated loadings
      rotated_loadings_df <- as.data.frame(unclass(rotated_pca$loadings))
      
      datatable(rotated_loadings_df, options = list(dom = 't')) %>% formatRound(columns = 1:ncol(rotated_loadings_df), digits = 3)
    })
    
    observeEvent(input$reset, {
      hideTab(inputId = "mainPanel", target = "pca_results_tab")
      hideTab(inputId = "mainPanel", target = "transformations_tab")
      hideTab(inputId = "mainPanel", target = "plots_tab")
      hideTab(inputId = "mainPanel", target = "uploaded_data_tab")
      
      updateNavbarPage(session, "mainPanel", selected = "data_import_tab")
      
      
      pca_results(NULL)
      analysis_data(NULL)
      original_data(NULL)
      updatePickerInput(session, "pcaVariables", selected = character(0))
      updateRadioButtons(session, "transformation", selected = "Original scale")
      updateNumericInput(session, "numFactors", value = 2)
      updateSelectInput(session, "pcX", choices = character(0))
      updateSelectInput(session, "pcY", choices = character(0))
      
      updateNavbarPage(session, "mainPanel", selected = "data_import_tab")
    })
    
    observe({
      outputOptions(output, "biplot", suspendWhenHidden = TRUE)
      outputOptions(output, "scorePlot", suspendWhenHidden = TRUE)
      outputOptions(output, "screePlot", suspendWhenHidden = TRUE)
      outputOptions(output, "loadingsHeatmap", suspendWhenHidden = TRUE)
    })
    
  })
}