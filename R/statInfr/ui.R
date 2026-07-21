library(shiny)
library(htmltools)
library(shinyjs)
library(rlang)

statInfrUI <- function(id) {
  ns <- NS(id)

  methodology <- radioButtons(
    inputId = ns("methodology"),
    label = strong(style = "padding-botton: 10px;", "Methodology"),
    choices = list(
      "Inference about 1 sample" = "One",
      "Inference about 2 samples" = "Two",
      "Inference about more than 2 samples (e.g. ANOVA or Kruskal-Wallis)" = "Multiple",
      r"[Inference for Categorical Data (e.g \(\chi^2\) test)]" = "Categorical"
    )
  )

  getConditionalPanel <- function(symbol, main = FALSE, e = caller_env()) {
    conditionalPanel(sprintf("input.methodology == '%s'", symbol), {
      UI <- get(sprintf("statInfrMethod%sUI", symbol), mode = "function")
      if (main) UI(id)$mainPanel else UI(id)$sidebarpanel
    }, ns = ns)
  }

  makePanel <- function(..., sidebar = TRUE) {
    assign("f", if (sidebar) sidebarPanel else mainPanel)
    f(...,
      `class<-`(
        lapply(
          getConditionalPanel,
          c("One", "Two", "Multiple", "Categorical")
        ),
        c("shiny.tag.list", "list")
      ))
  }

  sidebarLayout(
    makePanel(useShinyjs(), methodology),
    ## FIXME: the built-in validate messages, which render where outputs
    ## lay, must be decomposed and relocated appropriately so that these
    ## messages do not display above the second-level navigation bar, breaking
    ## the visual connection. Further, they should each display within the
    ## appropriate tab or not at all!
    makePanel(uiOutput(ns("inferenceValidation")), sidebar = FALSE)
  )
}
