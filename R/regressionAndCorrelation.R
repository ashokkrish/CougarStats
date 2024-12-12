regressionCorrelationUI <- function(id) {
  ns <- NS(id)
  fluidPage(useShinyjs(),
            ## TODO: when the width of the screen is smaller the middle column
            ## should take up twelve columns rather than eight.
            fluidRow(column(2),
                     column(8, import_ui(id = "dataImport",
                                         from = c("file", "copypaste", "env"))),
                     column(2)),
            br(),
            sidebarLayout(
              sidebarPanel(
                id = "sidebar",
                withMathJax(
                  ## DONE: the choices need to be updated dynamically.
                  selectInput(ns("responseVariable"),
                              "Response Variable (\\(y\\))",
                              choices = NULL),
                  uiOutput(ns("singleOrMultipleHelpText")),
                  selectInput(ns("explanatoryVariables"),
                              "Explanatory Variables (\\(x_1, x_2, x_3, \\cdots , x_n\\))",
                              multiple = TRUE,
                              choices = NULL),
                  conditionalPanel(ns = ns,
                                   "input.explanatoryVariables.length == 1",
                                   p(strong("Graph Options")),
                                   hr(),
                                   checkboxInput(
                                     inputId = ns("scatterPlot"),
                                     label   = "Scatterplot of \\( x\\) versus \\( y\\)",
                                     value   = TRUE)),
                  actionButton(
                    inputId = ns("calculate"),
                    label = "Calculate",
                    class = "act-btn"),

                  actionButton(
                    inputId = ns("reset"),
                    label = "Reset Values",
                    class = "act-btn")
                )
              ),
              mainPanel(uiOutput(ns("regressionCorrelationMainUI")))
            ))
}

regressionCorrelationServer <- function(id, uploadedTibble) {
  moduleServer(id, function(input, output, session) {
    ## TODO: reset this application area when the reset button is pressed.
    observe({ shinyjs::reset("sidebar") }) |> bindEvent(input$reset)

    ## TODO: alternate between the single or multiple linear regression UI based
    ## on the number of explanatory variables, and wait to draw their mainPanel
    ## UI's until the user presses the "Calculate" action button and the
    ## necessary data is available, otherwise display a helpful warning message
    ## in the mainPanel area.
    observe({
      output$regressionCorrelationMainUI <- renderUI({
        validate(need(isTruthy(uploadedTibble$data()),
                      "Import valid data using the above interface."),
                 need(length(input$explanatoryVariables) >= 1,
                      "Select at least one explanatory variable."))
        if (length(input$explanatoryVariables) == 1) {
          singleRegressionCorrelationUI("src") # call and produce the SLR UI.
        } else if (length(input$explanatoryVariables) > 1) {
          multipleRegressionCorrelationUI("mrc") # call and produce the MLR UI.
        }
      })
    }) |> bindEvent("calculate")

    ## DONE Update the choices for the select inputs when the uploadedTibble changes.
    observe({
      updateSelectInput(inputId = "responseVariable",
                        choices = colnames(uploadedTibble$data()))

      updateSelectizeInput(inputId = "explanatoryVariables",
                           choices = colnames(uploadedTibble$data()))
    }) |> bindEvent(uploadedTibble$data())

    ## DONE Validate that the response variable is not included in the explanatory variables.
    observe({
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

    return(
      list(
        responseVariable = reactive({ input$responseVariable }),
        explanatoryVariables = reactive({ input$explanatoryVariables }),
        calculate = reactive({ input$calculate })
      )
    )
  })
}
