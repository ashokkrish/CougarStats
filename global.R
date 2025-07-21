## install.packages("remotes")
## remotes::install_github("deepanshu88/shinyDarkmode")
## remotes::install_github("rsquaredacademy/olsrr")

## options(conflicts.policy = TRUE)
## library(conflicted)

library(aplpack)
library(bslib)
library(broom)
library(broom.helpers)
library(car)
library(colourpicker)
library(DescTools)
library(dplyr)
library(DT)
library(e1071)
library(forecast)
library(generics)
library(GGally)
library(ggfortify)
library(ggplot2)
library(ggpubr)
library(ggResidpanel)
library(ggsci)
library(gridExtra)
library(htmltools)
library(katex)
library(knitr)
library(latex2exp)
library(magrittr)
library(markdown)
library(moments)
library(nortest)
library(olsrr)
library(plotly)
library(psych)
library(readr)
library(readxl)
library(ResourceSelection)
library(rstatix)
library(shiny)
library(shinyalert)
library(shinyDarkmode)
library(shinyjs)
library(shinyMatrix)
library(shinythemes)
library(shinyvalidate)
library(shinyWidgets)
library(thematic)
library(tibble)
library(tidyr)
library(tinytex)
library(waiter)
library(writexl)
library(xtable)
library(MASS)
library(datamods)

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

source("R/simpleLinearRegression.R")
source("R/multipleLinearRegression.R")
source("R/regressionAndCorrelation.R")
source("R/logisticRegression.R")
source("R/principalComponentAnalysis.R")

options(scipen = 999) # options(scipen = 0)
## options(shiny.reactlog = TRUE)

## How many digits to round Critical Values
cvDigits <- 3

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"
  
  ## NOTE: advanced understanding of R is required to interpret these results.
  ## It's not for the faint of heart.
  ## warning("What follows is the base R conflicts() report: all MASK-ed or MASK-ing symbols are given.",
  ##         immediate. = TRUE)
  ## print(conflicts(detail = TRUE))
  
  ## NOTE: see #41.
  ## warning("Following this is the conflicted::conflict_scout() report.",
  ##         immediate. = TRUE)
  ## print(conflicted::conflict_scout())
  
  ## TODO: reenable this line before deployment.
  ## conflicted::conflicts_prefer(shinyjs::show, dplyr::filter, dplyr::select)
  
  ## See the theming issue brought up in #33; use thematic to attempt to make base
  ## R graphics compliant with ggplot theming, and to anticipate the impact of
  ## dark mode.
  ggplot2::theme_set(ggplot2::theme_minimal())
  thematic_shiny()