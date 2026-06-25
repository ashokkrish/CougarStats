library(shiny)
library(bslib)
source("R/regressionAndCorrelation.R")
source("R/simpleLinearRegression.R")
source("R/multipleLinearRegression.R")

testRegressionAndCorrelation <- function() {
  moduleId <- "regressionAndCorrelation"
  ui <- fluidPage(regressionAndCorrelationUI(moduleId))
  server <- function(input, output, server) {
    regressionAndCorrelationServer(moduleId)
  }
  shinyApp(ui, server)
}
