library(shiny)

source("R/multipleLinearRegression.R")

testMLRModule <- function() {
  MLRId <- "MLR"

  ui <- sidebarLayout(sidebarPanel(MLRSidebarUI(MLRId)),
                      mainPanel(MLRMainPanelUI(MLRId)))

  server <- function(input, output, session) {
    MLRServer(MLRId)
  }

  shinyApp(ui, server)
}

testMLRModule()
