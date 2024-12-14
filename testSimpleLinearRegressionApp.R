library(shiny)
library(bslib)
source("R/simpleLinearRegression_alternate.R")

testSLRModule <- function() {
  SLRId <- "SLR"

  ui <- page_sidebar(SLRMainPanelUI(SLRId),
                     sidebar = sidebar(SLRSidebarUI(SLRId),
                                       width = 500),
                     title = "Test application for the SLR module")

  server <- function(input, output, session) {
    SLRServer(SLRId)
  }

  shinyApp(ui, server)
}
