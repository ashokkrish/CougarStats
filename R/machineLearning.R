# R/machineLearning.R

# A column with a small number of unique values can still be a continuous
# measurement (e.g. 4, 40, 42.5, 168) rather than a true categorical class
# label. Non-integer values are a strong signal that it's continuous, which
# the unique-value-count heuristic alone misses on small datasets.
ml_is_continuous_response <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(FALSE)
  is.numeric(x) && any(x != floor(x))
}

ml_continuous_response_message <- paste(
  "Invalid response variable: kNN classification requires the response variable:", 
  "to be a categorical factor representing class labels. A continuous numeric",
  "response was detected. Consider using Linear Regression instead."
)

machineLearningUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      HTML(uploadDataDisclaimer),
      fileInput(ns("mlDataFile"),
                tags$b("Upload Data (.csv, .xls, .xlsx, or .txt)"),
                accept = c("text/csv", "text/comma-separated-values", "text/plain",
                           ".csv", ".tsv", ".txt", ".xls", ".xlsx")),
      actionButton(
        ns("loadIris"),
        label = tagList(icon("seedling"), "Load Example Dataset (iris)"),
        class = "btn btn-outline-secondary btn-sm w-100",
        style = "margin-top: -8px; margin-bottom: 6px; border: 2px solid #aaa; border-radius: 4px;"
      ),
      uiOutput(ns("mlDataStatus")),
      radioButtons(ns("method"),
                   tags$b("Methodology"),
                   choices = list(
                     "Principal Component Analysis" = "PCA",
                     "k-Nearest Neighbors"          = "KNN",
                     "Linear Discriminant Analysis" = "LDA",
                     "Decision Trees (CART)"        = "CART",
                     "Random Forest"                = "RF",
                     "Gradient Boosting (XGBoost)"  = "XGB"
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

    ml_data            <- reactiveVal(NULL)
    shared_explanatory <- reactiveVal(NULL)
    shared_response    <- reactiveVal(NULL)
    data_source        <- reactiveVal(NULL)   # tracks what is currently loaded

    # ---- File upload ----
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
      ml_data(as.data.frame(df))
      data_source(list(
        type = "file",
        name = input$mlDataFile$name,
        rows = nrow(df),
        cols = ncol(df)
      ))
    })

    # ---- Example dataset ----
    observeEvent(input$loadIris, {
      shinyjs::reset("mlDataFile")      # clear old filename from the widget
      shared_explanatory(NULL)          # clear stale cross-dataset selections
      shared_response(NULL)
      ml_data(as.data.frame(iris))
      data_source(list(
        type = "iris",
        name = "iris",
        rows = 150L,
        cols = 5L
      ))
    })

    # ---- Data source status label ----
    output$mlDataStatus <- renderUI({
      src <- data_source()
      if (is.null(src)) return(NULL)
      if (src$type == "iris") {
        div(
          class = "alert alert-info",
          style = "padding: 5px 10px; font-size: 12px; margin-top: 2px; margin-bottom: 10px;",
          icon("circle-check"),
          HTML(paste0(" <strong>Example dataset loaded:</strong> iris (",
                      src$rows, " rows × ", src$cols, " columns)"))
        )
      } else {
        div(
          class = "alert alert-success",
          style = "padding: 5px 10px; font-size: 12px; margin-top: 2px; margin-bottom: 10px;",
          icon("circle-check"),
          HTML(paste0(" <strong>File loaded:</strong> ", src$name, " (",
                      src$rows, " rows × ", src$cols, " columns)"))
        )
      }
    })

    # ---- Dynamic ID counters for each method ----
    pca_instance_counter  <- reactiveVal(0)
    knn_instance_counter  <- reactiveVal(0)
    lda_instance_counter  <- reactiveVal(0)
    cart_instance_counter <- reactiveVal(0)
    rf_instance_counter   <- reactiveVal(0)
    xgb_instance_counter  <- reactiveVal(0)

    current_pca_module_id  <- reactive({ paste0("ml_pca_",  pca_instance_counter()) })
    current_knn_module_id  <- reactive({ paste0("ml_knn_",  knn_instance_counter()) })
    current_lda_module_id  <- reactive({ paste0("ml_lda_",  lda_instance_counter()) })
    current_cart_module_id <- reactive({ paste0("ml_cart_", cart_instance_counter()) })
    current_rf_module_id   <- reactive({ paste0("ml_rf_",   rf_instance_counter()) })
    current_xgb_module_id  <- reactive({ paste0("ml_xgb_",  xgb_instance_counter()) })

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
      } else if (input$method == "XGB") {
        xgb_instance_counter(xgb_instance_counter() + 1)
        output$mlSidebarUI  <- renderUI({
          req(current_xgb_module_id())
          XGBSidebarUI(session$ns(current_xgb_module_id()))
        })
        output$mlMainPanelUI <- renderUI({
          req(current_xgb_module_id())
          XGBMainPanelUI(session$ns(current_xgb_module_id()))
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

    observeEvent(current_xgb_module_id(), {
      req(input$method == "XGB")
      XGBServer(current_xgb_module_id(), ml_data, shared_explanatory, shared_response)
    }, ignoreNULL = TRUE)

  })
}
