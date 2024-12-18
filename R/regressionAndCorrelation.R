regressionAndCorrelationUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      radioButtons(ns("multiple"),
                   tags$b("Regression type"),
                   list("Simple Linear Regression and Correlation Analysis" = "SLR",
                        "Multiple Linear Regression" = "MLR")),
      uiOutput(ns("simpleOrMultipleRegressionSidebarUI"))),
    mainPanel(
      useShinyjs(),
      uiOutput(ns("simpleOrMultipleRegressionMainPanelUI"))
    ))
}

regressionAndCorrelationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    SLRId <- "SLR"
    MLRId <- "MLR"
    SLRServer(SLRId)
    MLRServer(MLRId)

    ## DONE: alternate between the simple or multiple linear regression sidebar
    ## and main panel UI based on the value of the "multiple" radioButtons.
    observe({
      output$simpleOrMultipleRegressionSidebarUI <- renderUI({
        if (input$multiple == "MLR") MLRSidebarUI(session$ns(MLRId))
        else SLRSidebarUI(session$ns(SLRId))
      })

      output$simpleOrMultipleRegressionMainPanelUI <- renderUI({
        if (input$multiple == "MLR") MLRMainPanelUI(session$ns(MLRId))
        else SLRMainPanelUI(session$ns(SLRId))
      })
    })
  })
}
