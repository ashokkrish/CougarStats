# R/machineLearning.R

machineLearningUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      HTML(uploadDataDisclaimer),
      fileInput(ns("mlDataFile"),
                tags$b("Upload Data (.csv, .tsv, .txt, .xls, .xlsx)"),
                accept = c("text/csv", "text/comma-separated-values", "text/plain",
                           ".csv", ".tsv", ".txt", ".xls", ".xlsx")),
      radioButtons(ns("method"),
                   tags$b("Methodology"),
                   choices = list(
                     "Principal Component Analysis" = "PCA",
                     "k-Nearest Neighbors"          = "KNN",
                     "Linear Discriminant Analysis" = "LDA",
                     "Decision Trees (CART)"        = "CART",
                     "Random Forest"                = "RF"
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

    ml_data <- reactiveVal(NULL)
    shared_explanatory <- reactiveVal(NULL)
    shared_response    <- reactiveVal(NULL)

    observeEvent(input$mlDataFile, {
      req(input$mlDataFile)
      ext <- tolower(tools::file_ext(input$mlDataFile$name))
      df <- switch(ext,
        csv  = read_csv(input$mlDataFile$datapath, show_col_types = FALSE),
        tsv  = read_tsv(input$mlDataFile$datapath, show_col_types = FALSE),
        txt  = read_tsv(input$mlDataFile$datapath, show_col_types = FALSE),
        xls  = read_xls(input$mlDataFile$datapath),
        xlsx = read_xlsx(input$mlDataFile$datapath)
      )
      ml_data(df)
    })

    # Dynamic ID counters for each method
    pca_instance_counter  <- reactiveVal(0)
    knn_instance_counter  <- reactiveVal(0)
    lda_instance_counter  <- reactiveVal(0)
    cart_instance_counter <- reactiveVal(0)
    rf_instance_counter   <- reactiveVal(0)

    current_pca_module_id  <- reactive({ paste0("ml_pca_",  pca_instance_counter()) })
    current_knn_module_id  <- reactive({ paste0("ml_knn_",  knn_instance_counter()) })
    current_lda_module_id  <- reactive({ paste0("ml_lda_",  lda_instance_counter()) })
    current_cart_module_id <- reactive({ paste0("ml_cart_", cart_instance_counter()) })
    current_rf_module_id   <- reactive({ paste0("ml_rf_",   rf_instance_counter()) })

    observeEvent(input$method, {
      if (input$method == "PCA") {
        pca_instance_counter(pca_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({
          req(current_pca_module_id())
          PCASidebarUI(session$ns(current_pca_module_id()))
        })
        output$mlMainPanelUI <- renderUI({
          req(current_pca_module_id())
          PCAMainPanelUI(session$ns(current_pca_module_id()))
        })
      } else if (input$method == "KNN") {
        knn_instance_counter(knn_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({
          req(current_knn_module_id())
          KNNSidebarUI(session$ns(current_knn_module_id()))
        })
        output$mlMainPanelUI <- renderUI({
          req(current_knn_module_id())
          KNNMainPanelUI(session$ns(current_knn_module_id()))
        })
      } else if (input$method == "LDA") {
        lda_instance_counter(lda_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({
          req(current_lda_module_id())
          LDASidebarUI(session$ns(current_lda_module_id()))
        })
        output$mlMainPanelUI <- renderUI({
          req(current_lda_module_id())
          LDAMainPanelUI(session$ns(current_lda_module_id()))
        })
      } else if (input$method == "CART") {
        cart_instance_counter(cart_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({
          req(current_cart_module_id())
          CARTSidebarUI(session$ns(current_cart_module_id()))
        })
        output$mlMainPanelUI <- renderUI({
          req(current_cart_module_id())
          CARTMainPanelUI(session$ns(current_cart_module_id()))
        })
      } else if (input$method == "RF") {
        rf_instance_counter(rf_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({
          req(current_rf_module_id())
          RFSidebarUI(session$ns(current_rf_module_id()))
        })
        output$mlMainPanelUI <- renderUI({
          req(current_rf_module_id())
          RFMainPanelUI(session$ns(current_rf_module_id()))
        })
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    observeEvent(current_pca_module_id(), {
      req(input$method == "PCA")
      PCAServer(current_pca_module_id(), ml_data, shared_explanatory, shared_response)
    }, ignoreNULL = TRUE)

    observeEvent(current_knn_module_id(), {
      req(input$method == "KNN")
      KNNServer(current_knn_module_id(), ml_data, shared_explanatory, shared_response)
    }, ignoreNULL = TRUE)

    observeEvent(current_lda_module_id(), {
      req(input$method == "LDA")
      LDAServer(current_lda_module_id(), ml_data, shared_explanatory, shared_response)
    }, ignoreNULL = TRUE)

    observeEvent(current_cart_module_id(), {
      req(input$method == "CART")
      CARTServer(current_cart_module_id(), ml_data, shared_explanatory, shared_response)
    }, ignoreNULL = TRUE)

    observeEvent(current_rf_module_id(), {
      req(input$method == "RF")
      RFServer(current_rf_module_id(), ml_data, shared_explanatory, shared_response)
    }, ignoreNULL = TRUE)

  })
}
