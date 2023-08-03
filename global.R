library(aplpack)
library(base)
library(bslib)
library(car)
library(DescTools)
library(dplyr)
library(DT)
library(generics)
library(ggplot2)
library(e1071)
library(nortest)
library(readr)
library(readxl)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyvalidate)
library(shinyWidgets)
library(tinytex)
library(tools)
library(writexl)
library(xtable)
library(MASS)

source('R/OnePropZInt.R')
source('R/OnePropZTest.R')

options(scipen = 999) # options(scipen = 0)

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"