## install.packages("remotes")
## remotes::install_github("deepanshu88/shinyDarkmode")

options(conflicts.policy = TRUE)
library(conflicted)

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
library(plotly)
library(ggpubr)
library(ggsci)
library(e1071)
library(markdown)
library(nortest)
library(readr)
library(readxl)
library(rstatix)
library(shiny)
library(shinyDarkmode)
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
library(latex2exp)

source("R/authors.R")
source("R/ChiSquareTest.R")
source("R/descStats.R")
source('R/OneSampZInt.R')
source('R/OneSampTInt.R')
source("R/OneSampZTest.R")
source("R/OneSampTTest.R")
source('R/OnePropZInt.R')
source('R/OnePropZTest.R')
source('R/plotOptionsMenu.R')
source("R/probDist.R")
source("R/regCorr.R")
source("R/RenderBoxplot.R")
source("R/RenderMeanPlot.R")
source("R/RenderQQPlot.R")
source("R/RenderScatterplot.R")
source("R/RenderSideBySideBoxplot.R")
source("R/sampSizeEst.R")
source("R/statInfr.R")
source('R/TwoSampZInt.R')
source('R/TwoSampTInt.R')
source('R/TwoSampZTest.R')
source('R/TwoSampTTest.R')
source('R/TwoPropZInt.R')
source('R/TwoPropZTest.R')

options(scipen = 999) # options(scipen = 0)
## options(shiny.reactlog = TRUE)

## How many digits to round Critical Values
cvDigits <- 3

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"

## String List to Numeric List
createNumLst <- function(text) {
  text <- gsub("[^0-9.,-]","", text) #purge non-numeric characters
  text <- gsub("^,", "", text)      #purge any leading commas
  text <- gsub(",(,)+", ",", text)  #transform multiple consecutive commas into a single comma
  text <- gsub(",$", "", text)      #purge any trailing commas
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  suppressWarnings(na.omit(as.numeric(split)))
}

GetPlotHeight  <- function(plotToggle, pxValue, ui) {

  ifelse(plotToggle == 'in px' && !is.na(pxValue),
         height <- pxValue,
         height <- 400)

  ifelse(ui,
         return(paste0(height, "px")),
         return(height))
}

GetPlotWidth  <- function(plotToggle, pxValue, ui) {

  if(plotToggle == 'in px' && !is.na(pxValue)) {
    width <- pxValue

    if(ui) {
      width <- paste0(width, "px")
    }
  } else {
    width <- "auto"
  }

  return(width)
}

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

## NOTE: advanced understanding of R is required to interpret these results.
## It's not for the faint of heart.
## warning("What follows is the base R conflicts() report: all MASK-ED or MASK-ING symbols are given.",
##         immediate. = TRUE)
## print(conflicts(detail = TRUE))

## NOTE: see #41.
## warning("Following this is the conflicted::conflict_scout() report.",
##         immediate. = TRUE)
## print(conflicted::conflict_scout())
conflicted::conflicts_prefer(shinyjs::show, dplyr::filter, dplyr::select)
