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
                tags$b("Upload Data (.csv, .xls, .xlsx, .txt, .sas7bdat, .sav, .dta, .rds, .mtp, .mwx, .mpx)"),
                accept = c("text/csv", "text/comma-separated-values",
                           "text/tab-separated-values", "text/plain",
                           ".csv", ".tsv", ".txt", ".xls", ".xlsx",
                           ".sas7bdat", ".sav", ".dta", ".rds",
                           ".mtp", ".mwx", ".mpx")),
      actionButton(
        ns("loadIris"),
        label = tagList(icon("seedling"), "Load Example Dataset (iris)"),
        class = "btn btn-outline-secondary btn-sm w-100",
        style = "margin-top: -8px; margin-bottom: 6px; border: 2px solid #aaa; border-radius: 4px;"
      ),
      actionButton(
        ns("mlClearData"),
        label = "Clear Data",
        icon  = icon("trash"),
        class = "btn-danger btn-sm",
        style = "margin-top: 4px; margin-bottom: 8px;"
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
    # Bumped by Clear Data. Each method module watches it, drops its results,
    # errors and variable choices and resets its settings (data() going NULL alone doesn't
    # reach them: their observeEvent(data(), ...) handlers ignore NULL).
    clear_trigger      <- reactiveVal(0)

    # ---- File upload ----
    observeEvent(input$mlDataFile, {
      req(input$mlDataFile)
      ext <- tolower(tools::file_ext(input$mlDataFile$name))

      # Same reader as Regression and Correlation (R/utilityFunctions.R), so
      # format support is maintained in one place. .tsv is read like .txt.
      # Excel files are read from their first sheet. Wrapped in tryCatch so a
      # malformed file (bad encoding, corrupt workbook, an .rds that isn't a
      # data frame, etc.) surfaces a message instead of an unhandled error.
      read_ext <- if (ext == "tsv") "txt" else ext
      read_err <- NULL
      df <- if (!(read_ext %in% UPLOAD_ACCEPTED_EXTENSIONS)) NULL else tryCatch(
        readUploadedDataFile(read_ext, input$mlDataFile$datapath),
        error = function(e) {
          read_err <<- conditionMessage(e)
          NULL
        }
      )

      if (is.null(df) || !is.data.frame(df)) {
        showNotification(
          paste0("Could not read \"", input$mlDataFile$name, "\". ",
                 if (!is.null(read_err) && nzchar(read_err)) paste0(read_err, " ") else "",
                 "Please upload a valid .csv, .xls, .xlsx, .txt, .sas7bdat, .sav, ",
                 ".dta, .rds, .mtp, .mwx, or .mpx file."),
          type = "error", duration = 8
        )
        return(invisible(NULL))
      }

      # SPSS (.sav) and Stata (.dta) value labels arrive as haven_labelled
      # columns, which the methods' as.factor() / model calls can't handle.
      # A column whose every value has a label (e.g. 1 = setosa, 2 = versicolor)
      # is a category, so it becomes a factor of those labels; any other
      # labelled column keeps its numbers.
      df[] <- lapply(df, function(x) {
        if (!inherits(x, "haven_labelled")) return(x)
        vals <- unique(as.vector(unclass(x)))
        vals <- vals[!is.na(vals)]
        labs <- as.vector(attr(x, "labels"))
        if (length(labs) > 0 && all(vals %in% labs)) {
          haven::as_factor(x, levels = "labels")
        } else {
          haven::zap_labels(x)
        }
      })

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

    # ---- Clear Data ----
    # Removes the loaded dataset (uploaded file or iris). Each method module
    # then clears its results and variable choices and resets its settings
    # (see their clear_trigger handlers); the selected methodology is kept.
    observeEvent(input$mlClearData, {
      shinyjs::reset("mlDataFile")      # clear the filename from the widget
      shared_explanatory(NULL)
      shared_response(NULL)
      ml_data(NULL)
      data_source(NULL)
      clear_trigger(clear_trigger() + 1)
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
      PCAServer("ml_pca",   ml_data, shared_explanatory, shared_response, clear_trigger)
      KNNServer("ml_knn",   ml_data, shared_explanatory, shared_response, clear_trigger)
      LDAServer("ml_lda",   ml_data, shared_explanatory, shared_response, clear_trigger)
      CARTServer("ml_cart", ml_data, shared_explanatory, shared_response, clear_trigger)
      RFServer("ml_rf",     ml_data, shared_explanatory, shared_response, clear_trigger)
      XGBServer("ml_xgb",   ml_data, shared_explanatory, shared_response, clear_trigger)
    }, once = TRUE)

  })
}
