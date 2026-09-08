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
  "Invalid response variable: kNN classification requires the response variable",
  "to be a categorical factor representing class labels. A continuous numeric",
  "response was detected. Consider using Linear Regression instead."
)

machineLearningUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      # Signals the server once Shiny has finished binding the (large,
      # all-six-methods-at-once) static tab markup below. Needed so module
      # registration below doesn't run before the client has bound this tab's
      # widgets -- see the note near that registration for the remaining
      # caveat this does NOT resolve on its own.
      tags$script(HTML(sprintf(
        "$(document).on('shiny:sessioninitialized', function(event) {
           Shiny.setInputValue('%s', true);
         });",
        ns("clientReady")
      ))),
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
      # All six sidebars are mounted once and kept in the DOM; only the
      # active one is shown. This (plus the matching switch below and the
      # once-only *Server() calls in machineLearningServer) is what lets
      # each module's own reactiveVals and hideTab()/onFlushed(once=TRUE)
      # logic keep working unmodified while avoiding server-instance churn.
      tabsetPanel(
        id = ns("sidebarSwitch"),
        type = "hidden",
        selected = "PCA",
        tabPanelBody("PCA",  PCASidebarUI(ns("ml_pca"))),
        tabPanelBody("KNN",  KNNSidebarUI(ns("ml_knn"))),
        tabPanelBody("LDA",  LDASidebarUI(ns("ml_lda"))),
        tabPanelBody("CART", CARTSidebarUI(ns("ml_cart"))),
        tabPanelBody("RF",   RFSidebarUI(ns("ml_rf"))),
        tabPanelBody("XGB",  XGBSidebarUI(ns("ml_xgb")))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("mainPanelSwitch"),
        type = "hidden",
        selected = "PCA",
        tabPanelBody("PCA",  PCAMainPanelUI(ns("ml_pca"))),
        tabPanelBody("KNN",  KNNMainPanelUI(ns("ml_knn"))),
        tabPanelBody("LDA",  LDAMainPanelUI(ns("ml_lda"))),
        tabPanelBody("CART", CARTMainPanelUI(ns("ml_cart"))),
        tabPanelBody("RF",   RFMainPanelUI(ns("ml_rf"))),
        tabPanelBody("XGB",  XGBMainPanelUI(ns("ml_xgb")))
      )
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

      # Wrap in tryCatch so a malformed file (bad encoding, corrupt workbook,
      # etc.) surfaces a message instead of an unhandled read_* error.
      df <- tryCatch({
        switch(ext,
          csv  = read_csv(input$mlDataFile$datapath, show_col_types = FALSE),
          tsv  = read_tsv(input$mlDataFile$datapath, show_col_types = FALSE),
          txt  = read_tsv(input$mlDataFile$datapath, show_col_types = FALSE),
          xls  = read_xls(input$mlDataFile$datapath),
          xlsx = read_xlsx(input$mlDataFile$datapath),
          NULL  # unrecognized extension
        )
      }, error = function(e) NULL)

      if (is.null(df)) {
        showNotification(
          paste0("Could not read \"", input$mlDataFile$name, "\". Please upload ",
                 "a valid .csv, .tsv, .txt, .xls, or .xlsx file."),
          type = "error", duration = 8
        )
        return(invisible(NULL))
      }

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

    # ---- Method switch: keep the hidden-tabset UIs in sync with the radio ----
    # The sidebar/main-panel tabsetPanels already default to "PCA" (matching
    # the radioButtons' default), so there's no first-load render to race
    # against here. req() just skips the momentary NULL during the server's
    # first flush, before the client's initial radio value has round-tripped
    # back; the observer re-fires once the real value arrives.
    observeEvent(input$method, {
      req(input$method)
      updateTabsetPanel(session, "sidebarSwitch",    selected = input$method)
      updateTabsetPanel(session, "mainPanelSwitch",  selected = input$method)
    })

    # ---- Module servers: registered exactly once each, for the life of the
    # session. Switching methods no longer creates/destroys module server
    # instances (that was the source of the observer leak and the resulting
    # "no tabsetPanel with id ..." console errors) -- it only swaps which
    # tabsetPanel pane is visible. Because each module's own UI stays mounted
    # in the DOM the whole time, its internal reactiveVals keep working
    # unmodified, with no reset-on-switch mechanism needed: results/plots
    # simply persist if the user switches away and back, instead of clearing.
    #
    # Registration waits for input$clientReady (set by the sessioninitialized
    # script above) rather than running inline at session start, so the six
    # modules' own tab-hiding runs against a DOM the client has actually
    # finished binding. NOTE: as tested, this alone is not sufficient -- see
    # the conversation writeup on each module's session$onFlushed(hideTab...)
    # pattern needing a matching internal tweak before results tabs will
    # actually start hidden under this architecture.
    observeEvent(input$clientReady, {
      PCAServer("ml_pca",   ml_data, shared_explanatory, shared_response)
      KNNServer("ml_knn",   ml_data, shared_explanatory, shared_response)
      LDAServer("ml_lda",   ml_data, shared_explanatory, shared_response)
      CARTServer("ml_cart", ml_data, shared_explanatory, shared_response)
      RFServer("ml_rf",     ml_data, shared_explanatory, shared_response)
      XGBServer("ml_xgb",   ml_data, shared_explanatory, shared_response)
    }, once = TRUE)

  })
}
