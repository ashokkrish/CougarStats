## NOTE: this is only used to test this module independent of the other
## submodules of statInfr, and loading only the required libraries.
library(shiny)
library(shinyvalidate)

## tracingState(): TRUE indicates that a package management function *may* be
## enabled, and you *may* need to temporarily disable it to install from an
## arbitrary repository.
## install.packages("shinyjs", repos = "https://krishnamurthylab.github.io/drat")
library(shinyjs)# NOTE: the CRAN version will work, but will unintentionally reset the file upload inputs.

library(DT)
library(bslib)
library(readr)
library(readxl)

## wrap this in HTML() function to output the message
## i.e HTML(uploadDataDisclaimer)
## TODO: move this disclaimer to the application-level utility functions file
## and wrap it in a file.
uploadDataDisclaimer <- withTags(small(
  style = "color: #999; display: block; margin-bottom: 4px;",
  em(b("Note:"), "CougarStats does not store, log, or share any data you upload. All uploaded files exist only for the duration of your session and are permanently deleted when the session ends.")
))


source("../utils.R", TRUE)# utility functions only used within statistical inference.
source("../../utilityFunctions.R", TRUE)# utility functions used across the application.
source("../../plotOptionsMenu.R", TRUE)
source("../../OneSampZInt.R", TRUE)

source("ui.R", TRUE)
source("server.R", TRUE)

#' @param super The ID of the application what would be consuming the module.
#'   This test function emulates such an application, so the parameter for the
#'   ID of that application is so-called super.
statInfrMethodOneApp <- function(super = "si") {
  ## NOTE: the ID of the Shiny module being tested is "methodOne", what is passed to
  ## the UI and server functions of the Shiny module under test, though these
  ## are namespaced (as is appropriate).
  id <- NS(super, "methodOne")
  ui <- statInfrMethodOneUI(id)
  ui <- withMathJax(fluidPage(
    useShinyjs(),
    titlePanel("Statistical Inference Method: One Sample"),
    sidebarLayout(ui$sidebarPanel, ui$mainPanel),
  ))
  server <- function(input, output, session) {
    statInfrMethodOneServer(id)
  }
  shinyApp(ui, server)
}

statInfrMethodOneApp()
