multipleRegressionCorrelationUI <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(column(width = 4,
                            tags$b("Imported data:"),
                            verbatimTextOutput(outputId = ns("str_data")),
                            verbatimTextOutput(outputId = ns("lm"))),
                     column(width = 8,
                            plotOutput(ns("plot")))))
}

multipleRegressionCorrelationServer <- function(id, uploadedTibble, selectedVariables) {
  moduleServer(id, function(input, output, session) {
    ## NOTE: example outputs.
    output$str_data <- renderPrint({
      validate(need(uploadedTibble$name(), "Upload some data."))
      str(uploadedTibble$data())
    })

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
               need(is.numeric(uploadedTibble$data()[[selectedVariables$responseVariable()]]),
                    "The response variable must be numeric."),
               need(isTruthy(input$explanatoryVariables),
                    "One or more explanatory variables must be selected."),
               need(all(as.logical(lapply(input$explanatoryVariables,
                                          storeNameIfNotNumeric))),
                    sprintf("All explanatory variables must be numeric. These variables are non-numeric: %s.",
                            paste(nonnumericVariables, sep = ", "))))
      with(uploadedTibble$data(), {
        lm(reformulate(input$explanatoryVariables, selectedVariables$responseVariable()))
      })
    })
    output$plot <- renderPlot({
      validate(need(uploadedTibble$name(), "Upload some data."))
      validate(need(length(uploadedTibble$data()[[selectedVariables$responseVariable()]]) == 1, "Upload some data."))
      responseVariableData <- uploadedTibble$data()[[selectedVariables$responseVariable()]]
      validate(need(is.numeric(responseVariableData), "Response variable must be numeric."))
      hist(responseVariableData)
    })
  })
}
