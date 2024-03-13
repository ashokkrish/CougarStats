library(aplpack)
library(base)
library(bslib)
library(car)
library(colourpicker)
library(DescTools)
library(dplyr)
library(DT)
library(generics)
library(ggplot2)
library(e1071)
# library(excelR)
library(nortest)
library(readr)
library(readxl)
# library(rhandsontable)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyMatrix)
library(shinyvalidate)
library(shinyWidgets)
library(tinytex)
library(tools)
library(writexl)
library(xtable)
library(MASS)

source("R/ChiSquareTest.R")
source("R/RenderBoxplot.R")
source("R/RenderScatterplot.R")
source("R/RenderSideBySideBoxplot.R")
source('R/OneSampZInt.R')
source('R/OneSampTInt.R')
source("R/OneSampZTest.R")
source("R/OneSampTTest.R")
source('R/OnePropZInt.R')
source('R/OnePropZTest.R')
source('R/TwoSampZInt.R')
source('R/TwoSampTInt.R')
source('R/TwoSampZTest.R')
source('R/TwoSampTTest.R')
source('R/TwoPropZInt.R')
source('R/TwoPropZTest.R')

options(scipen = 999) # options(scipen = 0)
# options(shiny.reactlog = TRUE)

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"

# How many digits to round Critical Values
cvDigits <- 3
