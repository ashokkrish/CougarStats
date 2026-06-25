library(shiny)

testMLRModule <- function() {
  source("R/multipleLinearRegression.R")
  MLRId <- "MLR"
  ui <- sidebarLayout(sidebarPanel(MLRSidebarUI(MLRId)),
                      mainPanel(MLRMainPanelUI(MLRId)))
  server <- function(input, output, session) {
    MLRServer(MLRId)
  }
  shinyApp(ui, server)
}

testMLRModule()
