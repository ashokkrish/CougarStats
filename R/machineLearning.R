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

# Shown by every classification method (kNN, LDA, CART, Random Forest, XGBoost).
# The wording is the original app's, kept on purpose.
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
      # The shared reader (utilityFunctions.R) applies the upload size limits
      # and converts Windows-1252/Latin-1 text to UTF-8; a .tsv file is read
      # like a tab-delimited .txt file. A size-limit message from it is shown
      # as the notification.
      readProblem <- NULL
      df <- tryCatch({
        if (ext %in% c("csv", "tsv", "txt", "xls", "xlsx")) {
          readUploadedDataFile(if (ext == "tsv") "txt" else ext, input$mlDataFile$datapath)
        } else {
          NULL  # unrecognized extension
        }
      }, error = function(e) {
        if (inherits(e, "validation") && nzchar(conditionMessage(e))) {
          readProblem <<- conditionMessage(e)
        }
        NULL
      })

      if (is.null(df)) {
        if (is.null(readProblem)) {
          readProblem <- paste0("Could not read \"", input$mlDataFile$name, "\". Please upload ",
                                "a valid .csv, .tsv, .txt, .xls, or .xlsx file.")
        }
        showNotification(readProblem, type = "error", duration = 8)
        return(invisible(NULL))
      }

      # An empty file (0 bytes, blank lines, an empty sheet) parses without
      # error to a table with no columns or no rows, which the methods cannot
      # use (with zero columns their data() observers stop with an error).
      if (ncol(df) == 0 || nrow(df) == 0) {
        showNotification(
          paste0("\"", input$mlDataFile$name, "\" contains no data. Please upload ",
                 "a file with at least one column and one row of data."),
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
      start_method(input$method)
    })

    # ---- Module servers: each registered exactly once, for the life of the
    # session, the first time its method is selected (PCA, the default, starts
    # with this tab). Switching methods never creates/destroys module server
    # instances (that was the source of the observer leak and the resulting
    # "no tabsetPanel with id ..." console errors) -- it only swaps which
    # tabsetPanel pane is visible. Because each module's own UI stays mounted
    # in the DOM the whole time, its internal reactiveVals keep working
    # unmodified, with no reset-on-switch mechanism needed: results/plots
    # simply persist if the user switches away and back, instead of clearing.
    ml_servers <- list(
      PCA  = function(explanatory, response) PCAServer("ml_pca",   ml_data, explanatory, response),
      KNN  = function(explanatory, response) KNNServer("ml_knn",   ml_data, explanatory, response),
      LDA  = function(explanatory, response) LDAServer("ml_lda",   ml_data, explanatory, response),
      CART = function(explanatory, response) CARTServer("ml_cart", ml_data, explanatory, response),
      RF   = function(explanatory, response) RFServer("ml_rf",     ml_data, explanatory, response),
      XGB  = function(explanatory, response) XGBServer("ml_xgb",   ml_data, explanatory, response)
    )

    # Shared selections as they stood when the current dataset was loaded.
    # Each method's data() observer reads them to preselect its pickers. When
    # all six methods were registered up front, every one of them ran that
    # observer at load time; a method opened for the first time later must
    # therefore start from these same values, not from selections made since
    # in another method.
    loaded_with <- list(explanatory = NULL, response = NULL)
    observeEvent(ml_data(), {
      loaded_with <<- list(explanatory = shared_explanatory(),
                           response    = shared_response())
    }, priority = 100)

    start_method <- function(method) {
      start <- ml_servers[[method]]
      if (is.null(start)) return(invisible())
      ml_servers[[method]] <<- NULL

      # Reads made while the new module runs its first round of observers
      # return the load-time values; writes always go to the shared state.
      starting <- TRUE
      session$onFlushed(function() starting <<- FALSE, once = TRUE)
      as_loaded <- function(shared, field) {
        function(value) {
          if (!missing(value)) return(shared(value))
          current <- shared()
          if (starting) loaded_with[[field]] else current
        }
      }

      start(as_loaded(shared_explanatory, "explanatory"),
            as_loaded(shared_response, "response"))
    }

  })
}
