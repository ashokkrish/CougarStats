library(shiny)
library(shinyjs)
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
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

PCAMainPanelUI <- function(id) {
  ns <- NS(id)
  # Wrap in tagList to include useShinyjs()
  tagList(
    useShinyjs(),
    navbarPage(title = NULL,
               id = ns("mainPanel"),
               theme = bs_theme(version = 4),
               tabPanel(title = "Data Import",
                        value = "data_import_tab",
                        uiOutput(ns("fileImportUserMessage")),
                        import_file_ui(id = ns("dataImport"), title = "")
               ),
               tabPanel(title = "PCA Results",
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
                        hr(),
                        h3("Scree Plot"),
                        plotOutput(ns("screePlot")),
                        hr(),
                        h3("Interpretation of Results"),
                        uiOutput(ns("pcaInterpretation"))
               ),
               tabPanel(title = "Data Transformations", value = "transformations_tab", plotOutput(ns("transformationHistograms")))
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
      btn_show_data = TRUE,
      return_class = "tbl_df"
    )
    
    pca_results <- reactiveVal(NULL)
    analysis_data <- reactiveVal(NULL)
    original_data <- reactiveVal(NULL)
    noFileCalculate <- reactiveVal(FALSE)
    
    # Run once on startup to hide tabs
    observeEvent(TRUE, {
      shinyjs::delay(0, {
        hideTab(inputId = "mainPanel", target = "pca_results_tab")
        hideTab(inputId = "mainPanel", target = "transformations_tab")
      })
    }, once = TRUE)
    
    observeEvent(imported_data$data(), {
      df <- imported_data$data()
      if (!is.null(df)) {
        numeric_cols <- names(dplyr::select_if(df, is.numeric))
        updatePickerInput(session, "pcaVariables", choices = numeric_cols)
      }
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
      if (!isTruthy(imported_data$data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }
      
      if (!isTruthy(input$pcaVariables) || length(input$pcaVariables) < 2) {
        shinyalert::shinyalert("Input Error", "Please select at least two variables for PCA.", type = "error", confirmButtonCol = "#18536F")
        return()
      }
      
      if(input$numFactors > length(input$pcaVariables)) {
        shinyalert::shinyalert("Input Error", "Number of factors cannot exceed the number of selected variables.", type = "error", confirmButtonCol = "#18536F")
        return()
      }
      
      data <- imported_data$data()
      selected_data <- data %>%
        dplyr::select(all_of(input$pcaVariables)) %>%
        na.omit()
      
      original_data(selected_data)
      
      if (ncol(selected_data) < 2) {
        shinyalert::shinyalert("Error", "PCA requires at least two numeric columns.", type = "error", confirmButtonCol = "#18536F")
        return()
      }
      
      transformed_data <- switch(
        input$transformation,
        "Original scale" = selected_data,
        "Logarithmic transformation" = {
          if (any(selected_data < 0)) {
            shinyalert::shinyalert("Error", "Log transformation cannot be applied to negative data.", type = "error", confirmButtonCol = "#18536F")
            return(NULL)
          }
          if (any(selected_data == 0)) {
            shinyalert::shinyalert("Info", "Data contains zeros. Applying a log(x+1) transformation.", type = "info", confirmButtonCol = "#18536F")
            log(selected_data + 1)
          } else {
            log(selected_data)
          }
        },
        "Box-Cox transformation" = {
          if (any(selected_data <= 0)) {
            shinyalert::shinyalert("Error", "Box-Cox transformation requires positive data.", type = "error", confirmButtonCol = "#18536F")
            return(NULL)
          }
          boxcox_transformed <- as.data.frame(lapply(selected_data, function(x) {
            bc <- forecast::BoxCox(x, forecast::BoxCox.lambda(x))
            return(bc)
          }))
          names(boxcox_transformed) <- names(selected_data)
          boxcox_transformed
        },
        "Standardized Box-Cox transformation" = {
          if (any(selected_data <= 0)) {
            shinyalert::shinyalert("Error", "Box-Cox transformation requires positive data.", type = "error", confirmButtonCol = "#18536F")
            return(NULL)
          }
          boxcox_transformed <- as.data.frame(lapply(selected_data, function(x) {
            bc <- forecast::BoxCox(x, forecast::BoxCox.lambda(x))
            return(bc)
          }))
          names(boxcox_transformed) <- names(selected_data)
          as.data.frame(scale(boxcox_transformed))
        }
      )
      
      if (is.null(transformed_data)) return()
      
      analysis_data(transformed_data)
      
      pca <- prcomp(transformed_data, center = TRUE, scale. = TRUE)
      pca_results(pca)
      
      showTab(inputId = "mainPanel", target = "pca_results_tab")
      showTab(inputId = "mainPanel", target = "transformations_tab")
      updateNavbarPage(session, "mainPanel", selected = "pca_results_tab")
    })
    
    
    # --- Render Outputs ---
    
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
      
      # Create a data frame for plotting
      scree_data <- data.frame(
        Component = 1:length(pca_results()$sdev),
        Variance = pca_results()$sdev^2
      )
      
      ggplot(scree_data, aes(x = Component, y = Variance)) +
        geom_line(aes(y = Variance), color = "black", size = 1) +
        geom_point(color = "black", size = 3) +
        labs(title = "Scree Plot",
             x = "Principal Component",
             y = "Eigenvalue (Variance)") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 14) # Enlarge axis text
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
      pca_results(NULL)
      analysis_data(NULL)
      original_data(NULL)
      updatePickerInput(session, "pcaVariables", selected = character(0))
      updateRadioButtons(session, "transformation", selected = "Original scale")
      updateNumericInput(session, "numFactors", value = 2)
      updateNavbarPage(session, "mainPanel", selected = "data_import_tab")
    })
  })
}