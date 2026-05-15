# R/randomForest.R

RFSidebarUI <- function(id) {
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
        strong("Response Variable (Class)"),
        choices = NULL,
        multiple = TRUE,
        options = list(`live-search` = TRUE, title = "Nothing selected", `max-options` = 1)
      ),
      uiOutput(ns("responseError"))
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

    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

RFMainPanelUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    navbarPage(
      title = NULL,

      tabPanel(
        title = "Results",
        value = "results_tab",
        uiOutput(ns("resultsContainer"))
      ),

      tabPanel(
        title = "Plots",
        value = "plots_tab",
        uiOutput(ns("plotsContainer"))
      ),

      tabPanel(
        title = "Uploaded Data",
        value = "uploaded_data_tab",
        uiOutput(ns("uploadedDataContainer"))
      ),

      id = ns("rfMainPanel"),
      selected = "uploaded_data_tab",
      theme = bs_theme(version = 4)
    )
  )
}

RFServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    results_ready <- reactiveVal(FALSE)
    plots_ready   <- reactiveVal(FALSE)

    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated   <- reactiveVal(FALSE)

    noFileCalculate <- reactiveVal(FALSE)
    responseError   <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)

    session$onFlushed(function() {
      hideTab(inputId = "rfMainPanel", target = "results_tab")
      hideTab(inputId = "rfMainPanel", target = "plots_tab")
    }, once = TRUE)

    # ---- Uploaded Data tab ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DT::DTOutput(session$ns("rfUploadTable"))
      }
    })

    output$rfUploadTable <- DT::renderDT({
      req(data())
      DT::datatable(
        data(),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX = TRUE
        )
      )
    })

    # ---- Populate dropdowns after upload ----
    observeEvent(data(), {
      noFileCalculate(FALSE)
      req(data())

      df   <- data()
      cols <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]

      pre_predictors <- intersect(shared_explanatory(), numeric_cols)
      shared_resp    <- shared_response()
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% cols) shared_resp else character(0)

      updatePickerInput(session, "response",   choices = cols,         selected = pre_response)
      updatePickerInput(session, "predictors", choices = numeric_cols, selected = pre_predictors)
    }, ignoreNULL = TRUE)

    # ---- Keep response out of predictors ----
    observeEvent(input$response, {
      shared_response(input$response)
      req(data())

      df   <- data()
      cols <- colnames(df)
      numeric_cols <- cols[sapply(df, is.numeric)]

      available_predictors <- setdiff(numeric_cols, input$response)
      selected_predictors  <- intersect(input$predictors, available_predictors)

      updatePickerInput(session, "predictors",
                        choices  = available_predictors,
                        selected = selected_predictors)

      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$predictors, {
      shared_explanatory(input$predictors)
      if (length(input$predictors) >= 1) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    })

    # ---- Clear outputs when settings change ----
    observeEvent(
      list(data(), input$response, input$predictors),
      {
        if (isTRUE(results_ready())) results_ready(FALSE)
        if (isTRUE(plots_ready()))   plots_ready(FALSE)

        hideTab(inputId = "rfMainPanel", target = "results_tab")
        hideTab(inputId = "rfMainPanel", target = "plots_tab")
        updateNavbarPage(session, "rfMainPanel", selected = "uploaded_data_tab")
      },
      ignoreInit = TRUE
    )

    # ---- Results / Plots containers ----
    output$resultsContainer <- renderUI({
      if (!isTRUE(results_ready())) {
        if (!isTRUE(results_ever_calculated())) {
          return(tagList(helpText("No results yet. Upload a dataset, choose variables, then click Calculate.")))
        }
        return(tagList(helpText("Settings changed. Click Calculate to update results.")))
      }
      uiOutput(session$ns("resultsUI"))
    })

    output$plotsContainer <- renderUI({
      if (!isTRUE(plots_ready())) {
        if (!isTRUE(plots_ever_calculated())) {
          return(tagList(helpText("No plots yet. Upload a dataset, choose variables, then click Calculate.")))
        }
        return(tagList(helpText("Settings changed. Click Calculate to update plots.")))
      }
      uiOutput(session$ns("plotsUI"))
    })

    # ---- Inline error messages ----
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(class = "shiny-output-error-validation",
                 "Required: Cannot calculate without a data file.")
      }
    })

    output$responseError <- renderUI({
      if (responseError()) {
        tags$div(class = "text-danger",
                 style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
                 icon("exclamation-circle"), "Please select a response variable.")
      }
    })

    output$predictorsError <- renderUI({
      if (predictorsError()) {
        tags$div(class = "text-danger",
                 style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
                 icon("exclamation-circle"), "Please select at least one explanatory variable.")
      }
    })

    # ---- Calculate ----
    observeEvent(input$calculate, {
      if (!isTruthy(data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }

      if (!isTruthy(input$response)) {
        responseError(TRUE)
        shinyjs::addClass(id = "responseWrapper", class = "has-error")
      } else {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      if (!isTruthy(input$predictors) || length(input$predictors) < 1) {
        predictorsError(TRUE)
        shinyjs::addClass(id = "predictorsWrapper", class = "has-error")
      } else {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }

      if (!isTruthy(input$response) || !isTruthy(input$predictors) || length(input$predictors) < 1) {
        return()
      }

      # TODO: Random Forest implementation goes here

      results_ready(TRUE)
      plots_ready(TRUE)
      results_ever_calculated(TRUE)
      plots_ever_calculated(TRUE)

      output$resultsUI <- renderUI({
        tagList(helpText("Random Forest results will appear here."))
      })

      output$plotsUI <- renderUI({
        tagList(helpText("Random Forest plots will appear here."))
      })

      showTab(inputId = "rfMainPanel", target = "results_tab")
      showTab(inputId = "rfMainPanel", target = "plots_tab")

      shinyjs::delay(100, {
        updateNavbarPage(session, "rfMainPanel", selected = "results_tab")
      })

    }, ignoreInit = TRUE)

    # ---- Reset ----
    observeEvent(input$reset, {
      hideTab(inputId = "rfMainPanel", target = "results_tab")
      hideTab(inputId = "rfMainPanel", target = "plots_tab")

      results_ready(FALSE)
      plots_ready(FALSE)
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)

      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)

      shinyjs::removeClass(id = "responseWrapper",   class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")

      updatePickerInput(session, "response",   selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNavbarPage(session, "rfMainPanel", selected = "uploaded_data_tab")
    })

  })
}
