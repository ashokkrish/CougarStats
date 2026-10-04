library(shiny)
library(bslib)
## global.R and every R/*.R file, the way the app loads them. The module files
## no longer source their helpers themselves, and global.R no longer sources
## the R/ files (Shiny autoloads them), so sourcing a few module files is not
## enough on its own.
shiny::loadSupport(renv = globalenv())

testRegressionAndCorrelation <- function() {
  moduleId <- "regressionAndCorrelation"
  ui <- fluidPage(regressionAndCorrelationUI(moduleId))
  server <- function(input, output, server) {
    regressionAndCorrelationServer(moduleId)
  }
  shinyApp(ui, server)
}
