## NOTE: this file is observers.R. It contains only observers and specific
## utility functions used only within an observer proximal to the utility
## function (i.e. the utiltiy function is written immediately before the
## observer which necessitates the utility function).
##
## Table of Contents:
## 1. popuParameter is observed to: dynamically hide and show the tabs in the
## mainPanelNavbarPage related to the selected population parameter. It has the
## utiltiy function id_helper.
## 2. dataAvailability is observed to: show the Uploaded Data tabPanel when the
## user uploads data.
## 3. upload is observed to: update the choices in a selectInput using the
## column names of the user-uploaded data.
## 4. oneMeanBoxplot is observed to: show the Graphs tabPanel when the checkbox
## is checked and the data availability is either "raw" or an uploaded dataset,
## or to change to the Analysis tabPanel and hide the Graphs tabPanel when the
## checkbox is unchecked.

id_helper <- function(param, tab = c("Analysis", "Graphs", "UploadedData")) {
  if (!missing(tab)) {
    tab <- match.arg(tab)
  }
  sprintf("popuParameter%s%s", param, tab)
}
observeEvent(input$popuParameter, {
  req(input$mainPanelNavbarPage)
  ## Choose the tab to display next (after the handler finishes executing) based
  ## on the previously selected tab.
  nextTab <- if (grepl("Graphs", input$mainPanelNavbarPage, fixed = TRUE)) {
    "Graphs"
  } else {
    "Analysis"
  }
  switch(
    input$popuParameter,
    "Population Mean" = {
      hideTabs("mainPanelNavbarPage", id_helper("Proportion"))
      hideTabs("mainPanelNavbarPage", id_helper("StandardDeviation"))
      showTabs(
        "mainPanelNavbarPage",
        id_helper("Mean"),
        id_helper("Mean", nextTab)
      )
    },
    "Population Standard Deviation" = {
      hideTabs("mainPanelNavbarPage", id_helper("Mean"))
      hideTabs("mainPanelNavbarPage", id_helper("Proportion"))
      showTabs(
        "mainPanelNavbarPage",
        id_helper("StandardDeviation"),
        id_helper("StandardDeviation", nextTab)
      )
    },
    "Population Proportion" = {
      hideTabs("mainPanelNavbarPage", id_helper("Mean"))
      hideTabs("mainPanelNavbarPage", id_helper("StandardDeviation"))
      showTabs(
        "mainPanelNavbarPage",
        id_helper("Proportion"),
        id_helper("Proportion", nextTab)
      )
    },
    stop("popuParameter has an invalid value!")
  )
})

sigmaKnownCheckboxInputUI <- function() {
  checkboxInput(
    session$ns("sigmaKnown"),
    strong("Population Standard Deviation (\\(\\sigma\\)) is known")
  )
}
observeEvent(input$dataAvailability, {
  if (input$dataAvailability == "Upload Data") {
    showTabs(
      "mainPanelNavbarPage",
      id_helper("Mean", "UploadedData"),
      id_helper("Mean", "UploadedData")
    )
    ## updateNavbarPage(inputId = "mainPanelNavbarPage", selected = id_helper("Mean", "UploadedData"))
  } else if (is.null(input$upload$name)) {
    ## NOTE: this branch only makes sense when data has never been uploaded.
    ## Once data has been uploaded it becomes wildly difficult to reliably
    ## "reset" the server's state with respect to input$upload. The client-side
    ## sees the HTML of the widget reset, but that is just an interface to the
    ## server: the server's state with respect to uploaded data is unaffected by
    ## shinyjs::reset(). It makes more sense to leave the data be and to not
    ## reset the input$upload widget.
    updateNavbarPage(
      inputId = "mainPanelNavbarPage",
      selected = id_helper("Mean", "Analysis")
    )
    hideTabs("mainPanelNavbarPage", id_helper("Mean", "UploadedData"))
  }

  removeUI(
    selector = sprintf(
      "div.checkbox:has(label > #%s)",
      session$ns("sigmaKnown")
    ),
    multiple = TRUE,
    immediate = TRUE,
    session = session
  )
  sprintf("sigmaKnown%sSibling", gsub(" ", "", input$dataAvailability)) |>
    session$ns() |>
    sub(pattern = "^", replacement = "#", x = _) |>
    insertUI(
      where = "afterEnd",
      ui = withMathJax(sigmaKnownCheckboxInputUI()),
      immediate = TRUE,
      session = session
    )
})

observeEvent(input$upload, priority = 5, {
  freezeReactiveValue(input, "selectUploadVariable")
  updateSelectInput(
    "selectUploadVariable",
    choices = c(colnames(Upload())),
    session = session
  )
})

## Whenever there are changes in the value of the checkbox regarding the boxplot
## for a population mean, i.e. checked or unchecked, manipulate the tabs as
## appropriate.
observeEvent(input$oneMeanBoxplot, {
  if (input$oneMeanBoxplot && input$dataAvailability != "Summarized Data") {
    showTab(
      inputId = "mainPanelNavbarPage",
      target = id_helper("Mean", "Graphs")
    )
  } else {
    req(input$mainPanelNavbarPage)
    if (input$mainPanelNavbarPage == id_helper("Mean", "Graphs")) {
      updateTabsetPanel(
        inputId = "mainPanelNavbarPage",
        selected = id_helper("Mean", "Analysis")
      )
    }
    hideTab(
      inputId = "mainPanelNavbarPage",
      target = id_helper("Mean", "Graphs")
    )
  }
})
