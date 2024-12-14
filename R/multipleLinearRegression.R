library(shiny)
library(bslib)
library(datamods)
library(magrittr)

MLRSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(
      ## DONE: the choices need to be updated dynamically.
      selectInput(ns("responseVariable"),
                  "Response Variable (\\(y\\))",
                  choices = NULL),
      uiOutput(ns("singleOrMultipleHelpText")),
      selectInput(ns("explanatoryVariables"),
                  "Explanatory Variables (\\(x_1, x_2, x_3, \\cdots , x_n\\))",
                  multiple = TRUE,
                  choices = NULL)),
    )
}

MLRMainPanelUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(2),
             column(8, import_ui(id = ns("dataImport"), from = c("file", "copypaste")),),
             column(2)),
    fluidRow(),
    fluidRow(column(width = 4,
                    tags$b("Imported data:"),
                    verbatimTextOutput(outputId = ns("str_data")),
                    verbatimTextOutput(outputId = ns("lm"))),
             column(width = 8,
                    plotOutput(ns("plot")))))
}

MLRServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploadedTibble <- import_server("dataImport", return_class = "tbl_df")

    ## NOTE: example outputs.
    output$str_data <- renderPrint({
      validate(need(uploadedTibble$name(), "Upload some data."))
      str(uploadedTibble$data())
    })

    ## TODO: revert to a locally available datamods ui.
    ## DONE Update the choices for the select inputs when the uploadedTibble changes.
    observe({
      updateSelectInput(inputId = "responseVariable",
                        choices = colnames(uploadedTibble$data()))

      updateSelectizeInput(inputId = "explanatoryVariables",
                           choices = colnames(uploadedTibble$data()))
    }) |> bindEvent(uploadedTibble$data())

    ## TODO: revert to a locally available datamods ui.
    ## DONE Validate that the response variable is not included in the explanatory variables.
    observe({
      req(isTruthy(input$responseVariable))
      if (isTruthy(uploadedTibble$data())) {
        ## DONE The newly-selected response variable is always removed from the
        ## available choices for explanatory variables, but the explanatory
        ## variables are updated to only deselect the newly-selected response
        ## variable; other explanatory variables remain selected, if any, rather
        ## than deselecting all variables whenever the response variable is
        ## changed.
        updateSelectizeInput(
          inputId = "explanatoryVariables",
          choices =
            dplyr::select(uploadedTibble$data(),
                          !all_of(input$responseVariable)) %>%
            colnames(),
          selected = if (input$responseVariable %in% input$explanatoryVariables)
                       input$explanatoryVariables[input$explanatoryVariables !=
                                                  input$responseVariable]
                     else
                       input$explanatoryVariables
        )
      }
    }) |> bindEvent(input$responseVariable)

    ## TODO: revert to a locally available datamods ui.
    ## DONE dynamically render help text
    output$singleOrMultipleHelpText <- renderUI({
      if (length(input$explanatoryVariables) > 1) {
        helpText("Multiple explanatory variables results in a multiple linear regression")
      } else if (length(input$explanatoryVariables) == 1) {
        helpText("A single explanatory variable results in a simple linear regression")
      } else {
        helpText("Select at least one explanatory variable")
      }
    })

    ## TODO: revert to a locally available datamods ui.
    ## NOTE regardless of single or multiple linear regression, all the
    ## explanatory variables and the response variable must be numeric or
    ## factors (which are strings coded as numbers).
    output$lm <- renderPrint({
      nonnumericVariables <- NULL
      storeNameIfNotNumeric <- function(var) {
        if (is.numeric(uploadedTibble$data()[[var]]))
          return(TRUE)
        else {
          nonnumericVariables %<>% c(var)
          return(FALSE)
        }
      }
      validate(need(uploadedTibble$name(), "Upload some data."),
               need(is.numeric(uploadedTibble$data()[[input$responseVariable]]),
                    "The response variable must be numeric."),
               need(isTruthy(input$explanatoryVariables),
                    "One or more explanatory variables must be selected."),
               need(all(as.logical(lapply(input$explanatoryVariables,
                                          storeNameIfNotNumeric))),
                    sprintf("All explanatory variables must be numeric. These variables are non-numeric: %s.",
                            paste(nonnumericVariables, sep = ", "))))
      with(uploadedTibble$data(), {
        lm(reformulate(input$explanatoryVariables, input$responseVariable))
      })
    })
    output$plot <- renderPlot({
      validate(need(uploadedTibble$name(), "Upload some data."))
      validate(need(isTruthy(input$responseVariable),
                    "Select at least one explanatory variable."))
      responseVariableData <- uploadedTibble$data()[[input$responseVariable]]
      validate(need(is.numeric(responseVariableData),
                    "Response variable must be numeric."))
      hist(responseVariableData)
    })

  })
}
