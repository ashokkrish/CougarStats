library(shiny)
library(shinyjs)
library(bslib)
library(datamods)
library(magrittr)
library(ggplot2)

MLRSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "MLRSidebar",
      useShinyjs(),
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
        fluidRow(actionButton(ns("calculate"), "Calculate"),
                 actionButton(ns("reset"), "Reset")))
    ))
}

MLRMainPanelUI <- function(id) {
  ns <- NS(id)
  navbarPage(title = NULL, id = ns("MLRMainPanelTab"),
             tabPanel(title = "Data Import",
                      import_ui(id = ns("dataImport"),
                                from = c("file",
                                         "copypaste"))),
             tabPanel(title = "MLR",
                      fluidPage(
                        fluidRow(uiOutput(ns("linearModelEquations"))),
                        fluidRow(verbatimTextOutput(ns("linearModelSummary"))),
                        fluidRow(verbatimTextOutput(ns("linearModelANOVA"))),

                        ## NOTE: Ashok didn't ask for this, but it's here.
                        fluidRow(verbatimTextOutput(ns("linearModelAIC"))),
                        fluidRow(verbatimTextOutput(ns("linearModelBIC"))),
                      )),
             tabPanel(title = "Residual Analysis",
                      fluidPage(
                        fluidRow(plotOutput(ns("linearModelRegressionLineAndPoints")))
                      )),
             ## NOTE: if the version is 5 then import_ui will break! MAYBE
             ## FIXME: submit a bug report upstream?
             theme = bs_theme(version = 4))
}

MLRServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploadedTibble <- import_server("dataImport", return_class = "tbl_df")

    observe({ shinyjs::reset(id = "responseVariable") }) |> bindEvent(input$reset)
    observe({ shinyjs::reset(id = "explanatoryVariables") }) |> bindEvent(input$reset)

    ## Update the choices for the select inputs when the uploadedTibble changes.
    observe({
      updateSelectInput(inputId = "responseVariable",
                        choices = colnames(uploadedTibble$data()),
                        selected = character())

      updateSelectizeInput(inputId = "explanatoryVariables",
                           choices = colnames(uploadedTibble$data()))
    }) |> bindEvent(uploadedTibble$data())

    ## Validate that the response variable is not included in the explanatory variables.
    observe({
      req(isTruthy(input$responseVariable))
      if (isTruthy(uploadedTibble$data())) {
        ## The newly-selected response variable is always removed from the
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

    output$singleOrMultipleHelpText <- renderUI({
      if (length(input$explanatoryVariables) > 1) {
        helpText("Multiple explanatory variables results in a multiple linear regression")
      } else if (length(input$explanatoryVariables) == 1) {
        helpText("A single explanatory variable results in a simple linear regression")
      } else {
        helpText("Select at least one explanatory variable")
      }
    })

    ## NOTE regardless of (the implicitly chosen) single or multiple linear
    ## regression, all of the model variables must be numeric. MAYBE TODO:
    ## acommodate factors.
    observe({
      nonnumericVariables <- NULL
      storeNameIfNotNumeric <- function(var) {
        if (is.numeric(uploadedTibble$data()[[var]]))
          return(TRUE)
        else {
          ## Replace or concatenate to the value of the nonnumericVariables
          ## variable.
          if (is.null(nonnumericVariables)) nonnumericVarialbes <- var
          else nonnumericVariables %<>% c(var)
          return(FALSE)
        }
      }

      validate(need(uploadedTibble$name(), "Upload some data."),
               need(is.numeric(uploadedTibble$data()[[input$responseVariable]]),
                    "The response variable must be numeric."),
               need(isTruthy(input$explanatoryVariables),
                    "One or more explanatory variables must be selected."),
               ## MAYBE FIXME: this doesn't seem to return FALSE whilst
               ## is.numeric(factor(c("Male", "Female"))) would.
               need(all(as.logical(lapply(input$explanatoryVariables,
                                          storeNameIfNotNumeric))),
                    sprintf("All explanatory variables must be numeric. These variables are non-numeric: %s.",
                            paste(nonnumericVariables, sep = ", "))))

      output$linearModelEquations <- renderUI({
        tagList(
          withMathJax(
            p("The estimated regression equation is"),
            ## Three equations follow
            p(with(uploadedTibble$data(), {
              model <- lm(reformulate(input$explanatoryVariables, input$responseVariable))
              ## Reactively generate the LaTeX for the regression model equation.
              modelEquations <- with(as.list(coefficients(model)), {
                paste(
                  r"[\(]",
                  paste(
                    c(
                      r"[\text{}]",
                      sprintf(r"[\hat{y} = \hat{\beta_0} + %s]",
                              paste(
                                sprintf(
                                  r"[\hat{%s}]",
                                  paste0(r"[\beta_]",
                                         seq_along(input$explanatoryVariables))
                                ),
                                paste0(r"[x_]", seq_along(input$explanatoryVariables)),
                                collapse = "+"
                              )),
                      sprintf(r"[\hat{%s} = \hat{\beta_0} + %s]",
                              input$responseVariable,
                              paste(
                                sprintf(
                                  r"[\hat{%s}]",
                                  paste0(r"[\beta_]",
                                         seq_along(input$explanatoryVariables))
                                ),
                                input$explanatoryVariables,
                                collapse = "+"
                              )),
                      sprintf(r"[\hat{%s} = %.3f + %s]",
                              input$responseVariable,
                              get("(Intercept)"),
                              paste(
                                ## Get the values of the estimated coefficients from the
                                ## environment constructed by with(coefficients(model),
                                ## {...}).
                                as.character(lapply(mget(input$explanatoryVariables),
                                                    \(x) sprintf(r"[%.3f]", x))),
                                input$explanatoryVariables,
                                collapse = "+"
                              )),
                      ""
                    ),
                    collapse = r"[\\]"),
                  r"[\)]"
                )
              })
            }))
          )
        )
      })

      with(uploadedTibble$data(), {
        model <- lm(reformulate(input$explanatoryVariables, input$responseVariable))
        output$linearModelSummary <- renderPrint({ summary(model) })
        output$linearModelANOVA <- renderPrint({ anova(model) })
        output$linearModelAIC <- renderPrint({
          print(AIC(model))
          print(paste("The AIC of the model and the AIC of the log-likelihood of the model are equal?", all.equal(AIC(model), AIC(logLik(model)))))
        })
        output$linearModelBIC <- renderPrint({ BIC(model) })
        output$linearModelRegressionLineAndPoints <- renderPlot({
          ggplot(model, aes(x = model$fitted.values, y = model$residuals)) +
            geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
            geom_point() +
            labs(x = "Fitted values", y = "Residuals") +
            ggtitle("Residual Plot for Multiple Linear Regression")
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
    }) |> bindEvent(input$calculate)
  })
}
