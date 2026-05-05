# R/machineLearning.R

machineLearningUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      radioButtons(ns("method"),
                   tags$b("Methodology"),
                   choices = list(
                     "Principal Component Analysis" = "PCA",
                     "k-Nearest Neighbors"          = "KNN",
                     "Linear Discriminant Analysis" = "LDA",
                     "Decision Trees (CART)"        = "CART"
                   ),
                   selected = "PCA"
      ),
      uiOutput(ns("mlSidebarUI"))
    ),
    mainPanel(
      uiOutput(ns("mlMainPanelUI"))
    )
  )
}

machineLearningServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Dynamic ID counters for each method
    pca_instance_counter  <- reactiveVal(0)
    knn_instance_counter  <- reactiveVal(0)
    lda_instance_counter  <- reactiveVal(0)
    cart_instance_counter <- reactiveVal(0)
    
    current_pca_module_id  <- reactive({ paste0("ml_pca_",  pca_instance_counter()) })
    current_knn_module_id  <- reactive({ paste0("ml_knn_",  knn_instance_counter()) })
    current_lda_module_id  <- reactive({ paste0("ml_lda_",  lda_instance_counter()) })
    current_cart_module_id <- reactive({ paste0("ml_cart_", cart_instance_counter()) })
    
    observeEvent(input$method, {
      if (input$method == "PCA") {
        pca_instance_counter(pca_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({ PCASidebarUI(session$ns(current_pca_module_id())) })
        output$mlMainPanelUI <- renderUI({ PCAMainPanelUI(session$ns(current_pca_module_id())) })
      } else if (input$method == "KNN") {
        knn_instance_counter(knn_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({ KNNSidebarUI(session$ns(current_knn_module_id())) })
        output$mlMainPanelUI <- renderUI({ KNNMainPanelUI(session$ns(current_knn_module_id())) })
      } else if (input$method == "LDA") {
        lda_instance_counter(lda_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({ LDASidebarUI(session$ns(current_lda_module_id())) })
        output$mlMainPanelUI <- renderUI({ LDAMainPanelUI(session$ns(current_lda_module_id())) })
      } else if (input$method == "CART") {
        cart_instance_counter(cart_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({ CARTSidebarUI(session$ns(current_cart_module_id())) })
        output$mlMainPanelUI <- renderUI({ CARTMainPanelUI(session$ns(current_cart_module_id())) })
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    observeEvent(current_pca_module_id(), 
                 { req(input$method == "PCA");  
                   PCAServer(current_pca_module_id()) }, 
                 ignoreNULL = TRUE)
    
    observeEvent(current_knn_module_id(), 
                 { req(input$method == "KNN"); 
                   KNNServer(current_knn_module_id()) }, 
                 ignoreNULL = TRUE)
    
    observeEvent(current_lda_module_id(), 
                 { req(input$method == "LDA"); 
                   LDAServer(current_lda_module_id()) }, 
                 ignoreNULL = TRUE)
    
    observeEvent(current_cart_module_id(),
                 { req(input$method == "CART"); 
                   CARTServer(current_cart_module_id()) }, 
                 ignoreNULL = TRUE)
    
  })
}