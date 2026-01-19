# R/regressionAndCorrelation.R

regressionAndCorrelationUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      radioButtons(ns("multiple"),
                   tags$b("Methodology"),
                   choices = list("Simple Linear Regression and Correlation Analysis" = "SLR",
                                  "Multiple Linear Regression" = "MLR",
                                  "Binary Logistic Regression" = "LOGR",
                                  "k-Nearest Neighbors" = "KNN"),
                   selected = "SLR"
      ),
      uiOutput(ns("regressionSidebarUI"))
    ),
    mainPanel(
      uiOutput(ns("regressionMainPanelUI"))
    )
  )
}

regressionAndCorrelationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # For SLR - can remain static
    SLR_MODULE_ID_STATIC <- "slr_static_instance"
    SLRServer(SLR_MODULE_ID_STATIC)
    
    # --- Dynamic ID Generation for MLR ---
    mlr_instance_counter <- reactiveVal(0)
    current_mlr_module_id <- reactive({
      paste0("mlr_dynamic_instance_", mlr_instance_counter())
    })
    
    # --- Dynamic ID Generation for Logistic Regression (LOGR) ---
    logr_instance_counter <- reactiveVal(0)
    current_logr_module_id <- reactive({
      paste0("logr_dynamic_instance_", logr_instance_counter())
    })
    
    # # --- Dynamic ID Generation for PCA ---
    # pca_instance_counter <- reactiveVal(0)
    # current_pca_module_id <- reactive({
    #   paste0("pca_dynamic_instance_", pca_instance_counter())
    # })
    
    #Dynamic ID generation for KNN
    knn_instance_counter <- reactiveVal(0)
    current_knn_module_id <- reactive({
      paste0("knn_dynamic_instance_", knn_instance_counter())
    })
  
    
    # Observer for the main radio button (input$multiple)
    observeEvent(input$multiple, {
      if (input$multiple == "MLR") {
        mlr_instance_counter(mlr_instance_counter() + 1)
        output$regressionSidebarUI <- renderUI({
          req(current_mlr_module_id())
          MLRSidebarUI(session$ns(current_mlr_module_id()))
        })
        output$regressionMainPanelUI <- renderUI({
          req(current_mlr_module_id())
          MLRMainPanelUI(session$ns(current_mlr_module_id()))
        })
      } else if (input$multiple == "SLR") {
        output$regressionSidebarUI <- renderUI({ SLRSidebarUI(session$ns(SLR_MODULE_ID_STATIC)) })
        output$regressionMainPanelUI <- renderUI({ SLRMainPanelUI(session$ns(SLR_MODULE_ID_STATIC)) })
      } else if (input$multiple == "LOGR") {
        logr_instance_counter(logr_instance_counter() + 1)
        output$regressionSidebarUI <- renderUI({
          req(current_logr_module_id())
          LogisticRegressionSidebarUI(session$ns(current_logr_module_id()))
        })
        output$regressionMainPanelUI <- renderUI({
          req(current_logr_module_id())
          LogisticRegressionMainPanelUI(session$ns(current_logr_module_id()))
        })
      # } else if (input$multiple == "PCA") {
      # pca_instance_counter(pca_instance_counter() + 1)
      # output$regressionSidebarUI <- renderUI({
      #   req(current_pca_module_id())
      #   PCASidebarUI(session$ns(current_pca_module_id()))
      # })
      # output$regressionMainPanelUI <- renderUI({
      #   req(current_pca_module_id())
      #   PCAMainPanelUI(session$ns(current_pca_module_id()))
      # })
      #}
      } else if (input$multiple == "KNN") {
        knn_instance_counter(knn_instance_counter() + 1)
        
        output$regressionSidebarUI <- renderUI({
          req(current_knn_module_id())
          KNNSidebarUI(session$ns(current_knn_module_id()))
        })
        output$regressionMainPanelUI <- renderUI({
          req(current_knn_module_id())
          KNNMainPanelUI(session$ns(current_knn_module_id()))
        })
      }
      }, ignoreNULL = FALSE, ignoreInit = FALSE) # Corrected this line
    
    
    observeEvent(current_mlr_module_id(), {
      req(input$multiple == "MLR")
      MLRServer(current_mlr_module_id())
    }, ignoreNULL = TRUE)
    
    observeEvent(current_logr_module_id(), {
      req(input$multiple == "LOGR")
      LogisticRegressionServer(current_logr_module_id())
    }, ignoreNULL = TRUE)
    
    # observeEvent(current_pca_module_id(), {
    #   req(input$multiple == "PCA")
    #   PCAServer(current_pca_module_id())
    # }, ignoreNULL = TRUE)
    
    observeEvent(current_knn_module_id(), {
      req(input$multiple == "KNN")
      KNNServer(current_knn_module_id())
    }, ignoreNULL = TRUE)
    
  })
}