regressionCorrelationUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      radioButtons(ns("multiple"),
                   "Regression type",
                   list("Simple Linear Regression and Correlation Analysis" = "SLR",
                        "Multiple Linear Regression" = "MLR")),
      uiOutput(ns("simpleOrMultipleRegressionSidebarUI"))),
    mainPanel(
      useShinyjs(),
      uiOutput(ns("simpleOrMultipleRegressionMainPanelUI"))
    ))
}

regressionCorrelationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    simpleLinearRegressionSidebarServer("slr")
    simpleLinearRegressionMainPanelServer("slr")

    importValue <- import_server("dataImport", return_class = "tbl_df")
    multipleLinearRegressionMainPanelServer("mlr", importValue)
    multipleLinearRegressionSidebarServer("mlr", importValue)

    ## DONE: alternate between the simple or multiple linear regression sidebar
    ## and main panel UI based on the value of the "multiple" radioButtons.
    observe({
      output$simpleOrMultipleRegressionSidebarUI <- renderUI({
        if (input$multiple == "MLR") multipleLinearRegressionSidebarUI("mlr", importValue)
        else simpleLinearRegressionSidebarUI("slr")
      })

      output$simpleOrMultipleRegressionMainPanelUI <- renderUI({
        if (input$multiple == "MLR") multipleLinearRegressionMainPanelUI("mlr", importValue)
        else simpleLinearRegressionMainPanelUI("slr")
      })
    })
  })
}
