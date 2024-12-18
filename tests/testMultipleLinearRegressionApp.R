library(shiny)

source("R/multipleLinearRegression.R")

testMLRModule <- function() {
  MLRId <- "MLR"

  ui <- page_sidebar(MLRMainPanelUI(MLRId),
                     sidebar = sidebar(MLRSidebarUI(MLRId)),
                     title = "Test application for the MLR module")

  server <- function(input, output, session) {
    MLRServer(MLRId)
  }

  shinyApp(ui, server)
}

testMLRModule()
