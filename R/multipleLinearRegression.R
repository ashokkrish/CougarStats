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
      ## extendShinyjs(script = "enableDisableTabPanel.js", functions = c("disableTab", "enableTab")),
      ## includeCSS(path = "www/enableDisableTabPanel.css"),
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
        ## FIXME #44: styling issue.
        fluidRow(actionButton(ns("calculate"), "Calculate"),
                 actionButton(ns("reset"), "Reset")))
    ))
}

MLRMainPanelUI <- function(id) {
  ns <- NS(id)
  navbarPage(title = NULL,
             tabPanel(title = "Data Import",
                      import_ui(id = ns("dataImport"), from = c("file", "copypaste"))),
             tabPanel(title = "MLR",
                      fluidPage(
                        fluidRow(uiOutput(ns("linearModelEquations"))),
                        fluidRow(verbatimTextOutput(ns("linearModelSummary"))),
                        ## NOTE: Ashok didn't ask for these, but they're here.
                        fluidRow(verbatimTextOutput(ns("linearModelAIC"))),
                        fluidRow(verbatimTextOutput(ns("linearModelBIC")))
                      )),
             tabPanel(title = "ANOVA",
                      fluidRow(verbatimTextOutput(ns("linearModelANOVA")))),
             tabPanel(title = "Diagnostics",
                      fluidPage(fluidRow(plotOutput(ns("linearModelRegressionLineAndPoints"))))),
             tabPanel(title = "Diagnostic Plots"),

             ## FIXME: if the version is 5 then import_ui will break! NOTE:
             ## write a support request on the Posit Shiny subform asking for
             ## advice. I don't understand why this is this way.
             theme = bs_theme(version = 4))
}

MLRServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    uploadedTibble <- import_server("dataImport", return_class = "tbl_df")

    ## FIXME #45: why isn't the effect of disableTab useful from R, when the
    ## same function call in JavaScript will have an effect?
    ## js$disableTab("MLR")
    ## js$disableTab("Diagnostics")
    ## observe({
    ##   shinyjs::reset(id = "responseVariable")
    ##   shinyjs::reset(id = "explanatoryVariables")
    ##   js$disableTab("MLR")
    ##   js$disableTab("Diagnostics")
    ## }) |> bindEvent(input$reset)

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
      ## NOTE: why is the response variable requirement preceeding the
      ## conditional statement operating upon the boolean result of the
      ## availability of the uploaded data tibble? A little cart before the
      ## horse, but it hasn't been problematic yet. NOTE: it's delicate because
      ## the reactive being observed is the response variable itself; it's then
      ## checked for truthiness before proceeding.
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
        div(class = "text-success", span("Multiple explanatory variables result in a multiple linear regression."))
      } else if (length(input$explanatoryVariables) == 1) {
        div(class = "text-danger", span("Select at least one more explanatory variable; a single explanatory variable results in a simple linear regression."))
      } else {
        div(class = "text-primary", span("Select at least two explanatory variables."))
      }
    })

    observe({ # input$calculate
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

      isNAVariables <- NULL
      storeNameIfAnyNA <- function(var) {
        if (anyNA(uploadedTibble$data()[[var]]))
          return(TRUE)
        else {
          ## Replace or concatenate to the value of the isNAVariables variable.
          if (is.null(isNAVariables)) isNAVariables <- var
          else isNAVariables %<>% c(var)
          return(FALSE)
        }
      }

      output$linearModelEquations <- renderUI({
        validate(need(uploadedTibble$name(), "Upload some data."),
                 need(is.numeric(uploadedTibble$data()[[input$responseVariable]]),
                      "The response variable must be numeric."),
                 need(isTruthy(input$explanatoryVariables) && length(as.character(input$explanatoryVariables)) >= 2,
                      "Two or more explanatory variables must be selected."),
                 ## MAYBE FIXME: this doesn't seem to return FALSE whilst
                 ## is.numeric(factor(c("Male", "Female"))) would.
                 need(all(as.logical(lapply(input$explanatoryVariables,
                                            storeNameIfNotNumeric))),
                      sprintf("All explanatory variables must be numeric. These variables are non-numeric: %s.",
                              paste(nonnumericVariables, sep = ", "))),
                 need(!all(as.logical(lapply(input$explanatoryVariables,
                                             storeNameIfAnyNA))),
                      sprintf("All explanatory variables must not contain NAs. These variables contain NAs: %s.",
                              paste(isNAVariables, sep = ", "))))

        tagList(
          withMathJax(
            div(id = "linear-model-equations",
                p("The estimated regression equation is"),
                p(with(uploadedTibble$data(), {
                  model <- lm(reformulate(input$explanatoryVariables, input$responseVariable))
                  ## Reactively generate the LaTeX for the regression model equation.
                  modelEquations <- with(as.list(coefficients(model)), {
                    paste(
                      r"{\[}",
                      r"[\begin{align}]",
                      paste(
                        c(
                          sprintf(r"[\hat{y} &= \hat{\beta_0} &+ %s]",
                                  paste(
                                    sprintf(
                                      r"[\hat{%s}]",
                                      paste0(r"[\beta_]",
                                             seq_along(input$explanatoryVariables))
                                    ),
                                    paste0(r"[x_]", seq_along(input$explanatoryVariables)),
                                    collapse = "&+"
                                  )),
                          sprintf(r"[\hat{y} &= %.3f &+ %s]",
                                  get("(Intercept)"),
                                  paste(
                                    as.character(lapply(mget(input$explanatoryVariables), \(x) sprintf(r"[%.3f]", x))),
                                    paste0(r"[x_]", seq_along(input$explanatoryVariables)),
                                    collapse = "&+")
                                  ),
                          ""
                        ),
                        collapse = r"[\\]"
                      ),
                      r"[\end{align}]",
                      r"{\]}",
                      sep = r"{\\}"
                    )
                  })
                }))
                ))
        )
      })

      with(uploadedTibble$data(), {
        validate(need(isTruthy(input$explanatoryVariables),
                      "Two or more explanatory variables must be selected."),
                 need(isTruthy(input$responseVariable),
                      "A response variable must be selected."))

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

      ## FIXME #45: why isn't the effect of disableTab useful from R, when the
      ## same function call in JavaScript will have an effect?
      ## js$enableTab("MLR")
      ## js$enableTab("Diagnostics")
    }) |> bindEvent(input$calculate)
  })
}
