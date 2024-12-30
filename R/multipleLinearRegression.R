library(shiny)
library(shinyjs)
library(bslib)
library(datamods)
library(magrittr)
library(ggplot2)
library(dplyr)
library(knitr) # for kable to produce HTML tables.

MLRSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "MLRSidebar",
      useShinyjs(),
      ## FIXME #45: the script isn't working; unfortunately, it seems there is
      ## an upstream bug or a severely complex issue which is preventing the
      ## script objects from being correctly associated.
      ## extendShinyjs(script = "enableDisableTabPanel.js", functions = c("disableTab", "enableTab")),
      ## includeCSS(path = "www/enableDisableTabPanel.css"),
      withMathJax(
        helpText("Only numeric variables are selectable."),
        ## DONE: the choices need to be updated dynamically.
        selectInput(ns("responseVariable"),
                    "Response Variable (\\(y\\))",
                    choices = NULL),
        helpText("Only numeric variables are selectable."),
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
                        fluidRow(uiOutput(ns("linearModelAdjustedR2"))),
                        ## NOTE: Ashok didn't ask for these, but they're here.
                        ## fluidRow(verbatimTextOutput(ns("linearModelAIC"))),
                        ## fluidRow(verbatimTextOutput(ns("linearModelBIC")))
                      )),
             tabPanel(title = "ANOVA",
                      fluidPage(
                        fluidRow(uiOutput(ns("ANOVAHypothesisTesting"))),
                        fluidRow(tableOutput(ns("linearModelANOVA"))),
                        fluidRow(uiOutput(ns("ANOVAHypothesisTestingContinued")))
                      )),
             tabPanel(title = "Diagnostics",
                      fluidPage(fluidRow(plotOutput(ns("linearModelRegressionLineAndPoints"))))),
             tabPanel(title = "Diagnostic Plots"),

             ## FIXME #49: if the version is 5 then import_ui will break! NOTE:
             ## write a support request on the Posit Shiny subform asking for
             ## advice. I don't understand why this is this way.
             theme = bs_theme(version = 4))
}

MLRServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## TODO: display a help message in the sidebar indicating that non-numeric
    ## columns will not be available.
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
                        choices = colnames(select_if(uploadedTibble$data(), is.numeric)))

      updateSelectizeInput(inputId = "explanatoryVariables",
                           choices = colnames(select_if(uploadedTibble$data(), is.numeric)))
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
            dplyr::select(select_if(uploadedTibble$data(), is.numeric),
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

    MLRValidation <-
      quote(validate(
        need(uploadedTibble$name(), "Upload some data."),
        need(is.numeric(uploadedTibble$data()[[input$responseVariable]]),
             "The response variable must be numeric."),
        need(isTruthy(input$explanatoryVariables),
             "Explanatory variables are required."),
        need(length(as.character(input$explanatoryVariables)) >= 2,
             "Two or more explanatory variables must be selected."),
        need(all(as.logical(lapply(input$explanatoryVariables,
                                   storeNameIfNotNumeric))),
             sprintf("All explanatory variables must be numeric. These variables are non-numeric: %s.",
                     paste(nonnumericVariables, sep = ", "))),
        need(!all(as.logical(lapply(input$explanatoryVariables,
                                    storeNameIfAnyNA))),
             sprintf("All explanatory variables must not contain NAs. These variables contain NAs: %s.",
                     paste(isNAVariables, sep = ", ")))))

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

      output$ANOVAHypothesisTesting <- renderUI({
        withMathJax(
          p(strong("Analysis of Variance (ANOVA)")),
          ## TODO: use this if there are more than five independent variable
          ## selected, and while an alternative which uses the actual number is
          ## unimplemented.
          p(r"{\( H_0: \beta_1 = \beta_2 = \cdots = \beta_k \) (there is no linear relationship between the dependent variable and the independent variables.) }",
            br(),
            r"{\( H_a: \) At least one \(\beta_j\ne 0\), where \(j = 0, 1, \cdots, k\). (there is a linear relationship between the dependent variable and the independent variables.)}"),
          p(r"{In other words, "Taken collectively, does the entire set of explanatory variables contribute significantly to the prediction of the response?"}"),
          p(r"{\( \alpha = 0.05\ \)}"),
          p(sprintf(r"{\( n = %i \)}", nrow(uploadedTibble$data())),
            br(),
            sprintf(r"{\( k = %i \)}", nrow(uploadedTibble$data()) - 1))
        )
      })

      output$linearModelEquations <- renderUI({
        req(input$explanatoryVariables,
            cancelOuput = "progress")
        ## eval(MLRValidation)

        withMathJax(
          div(id = "linear-model-equations",
              p("The variables in the model are"),
              p(paste(r"{\[}",
                      r"(\begin{align})",
                      sprintf(r"[y    &= \text{%s} \\]", input$responseVariable),
                      paste(sprintf(r"[x_%d &= \text{%s}]",
                                    seq_along(input$explanatoryVariables),
                                    input$explanatoryVariables),
                            collapse = r"[\\]"),
                      r"(\end{align})",
                      r"{\]}")),
              p("The estimated regression equation is"),
              p(with(uploadedTibble$data(), {
                model <- lm(reformulate(as.character(lapply(input$explanatoryVariables, as.name)),
                                        as.name(input$responseVariable)))

                ## Reactively generate the LaTeX for the regression model equation.
                modelEquations <- with(as.list(coefficients(model)), {
                  req(as.list(coefficients(model)),
                      cancelOutput = "progress")
                  paste(
                    r"{\[}",
                    r"[\begin{align}]",
                    gsub(
                      pattern = r"{\+(\ +)?-}",
                      replacement = "-",
                      x = paste(
                        c(
                          sprintf(r"[\hat{y} &= \hat{\beta_0} &+ %s]",
                                  paste(
                                    sprintf(
                                      r"[\hat{%s}]",
                                      paste0(r"[\beta_]",
                                             seq_along(input$explanatoryVariables))
                                    ),
                                    paste0(r"{x_}", seq_along(input$explanatoryVariables)),
                                    collapse = "&+"
                                  )),
                          sprintf(r"[\hat{y} &= %.3f &+ %s]",
                                  get("(Intercept)"),
                                  paste(
                                    as.character(lapply(mget(as.character(lapply(input$explanatoryVariables, as.name))),
                                                        \(x) sprintf(r"[%.3f]", x))),
                                    paste0(r"{x_}", seq_along(input$explanatoryVariables)),
                                    collapse = "&+"
                                  )),
                          ""
                        ),
                        collapse = r"[\\]"
                      )),
                    r"[\end{align}]",
                    r"{\]}",
                    sep = r"{\\}"
                  )
                })
              })))
        )
      }) # output$linearModelEquations

      with(uploadedTibble$data(), {
        validate(need(isTruthy(input$explanatoryVariables),
                      "Two or more explanatory variables must be selected."),
                 need(isTruthy(input$responseVariable),
                      "A response variable must be selected."))

        model <- lm(reformulate(as.character(lapply(input$explanatoryVariables, as.name)),
                                as.name(input$responseVariable)))
        output$linearModelSummary <- renderPrint({ summary(model) })
        output$linearModelAdjustedR2 <- renderUI({
          eval(MLRValidation)
          withMathJax(p(strong(r"{ Adjusted \(R^2\) }")),
                      p(sprintf(r"{
\[
\begin{align}
R^2_{\text{adj}} & = 1 - \left[ \left( 1-R^2 \right) \frac{n-1}{n-k-1} \right] \\
R^2_{\text{adj}} & = %0.4f
\end{align}
\]
}",
summary(model)$adj.r.squared)),
                      p(r"[where \(k\) is the number of independent (explanatory) variables in the regression equation.]"),
                      p(strong("Interpretation:"),
                        sprintf(r"[therefore, %.2f%% of the variation in the response variable is explained by the multiple linear regression model when adjusted for the number of explanatory variables and the sample size.]",
                                summary(model)$adj.r.squared * 100)))
        })

        output$linearModelANOVA <- renderTable({
          options(knitr.kable.NA = "") # do not display NAs
          modelANOVA <- anova(model)
          SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
          SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)]       # only the residuals
          SST <- SSR + SSE
          k <- nrow(modelANOVA) - 1
          n <- nrow(uploadedTibble$data())
          MSR <- SSR / k
          MSE <- SSE / (n - k - 1)
          F <- MSR / MSE

          ## RETURN THE TABLE TO RENDER
          tibble::tribble(
                    ~"names",     ~"df",     ~"SS", ~"MS", ~"F", ~"p-Value of F",
                    "Regression", k,         SSR,   MSR,   NA,   NA,
                    "Residual",   n - k - 1, SSE,   MSE,   NA,   NA,
                    "Total",      n - 1,     SST,   NA,    F,    pf(F, k, n - k - 1, lower.tail = FALSE)
                  ) %>% tibble::column_to_rownames(var = "names")
        },
        rownames = TRUE,
        na = "",
        striped = TRUE,
        caption = "ANOVA Table")

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

      output$ANOVAHypothesisTestingContinued <- renderUI({
        with(uploadedTibble$data(), {
          model <- lm(reformulate(as.character(lapply(input$explanatoryVariables, as.name)),
                                  as.name(input$responseVariable)))
          anovaModel <- anova(model)
          regressionVariables <- nrow(anovaModel) - 1
          modelANOVA <- anova(model)
          SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
          SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)]       # only the residuals
          SST <- SSR + SSE
          k <- nrow(modelANOVA) - 1
          n <- nrow(uploadedTibble$data())
          MSR <- SSR / k
          MSE <- SSE / (n - k - 1)
          F <- MSR / MSE
          withMathJax(
            p(strong("Test statistic")),
            p(sprintf(r"{\(F = \frac{\text{MSR}}{\text{MSE}} = \frac{%0.2f}{%0.2f} = %0.2f \)}",
                      MSR, MSE, F)),
            p(sprintf(r"{
\[
\begin{align}
R^2 & = \frac{\text{SSR}}{\text{SST}} \\
R^2_{\text{adj}} & = \frac{%0.2f}{%0.2f} \\
R^2_{\text{adj}} & = %0.2f \\
\end{align}
\]
}",
anovaModel$SSR, anovaModel$SST, anovaModel$SSR / anovaModel$SST)),
            p(strong("Using the P-Value method:")),
            p(sprintf("The p-value for the F statistic for this test is: %0.3f",
                      pf(F, k, n - k - 1, lower.tail = FALSE)))
          )
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

### FIXME #45: why isn't the effect of disableTab useful from R, when the same
### function call in JavaScript will have an effect?
      ## js$enableTab("MLR")
      ## js$enableTab("Diagnostics")

### FIXME #53: this observer was meant to left-align display math /after/
### MathJax has rendered it; this works in the JavaScript console in a browser,
### but in the reactive context in which this runs and (more importantly) /when
### it runs/ it doens't work.
      ## observe({
      ##   reactiveValuesToList(input) # whenever any input changes run the following script.
      ##   JS("$('p.MathJax_LeftAlign div.MathJax_Display').css('text-align', 'left');")
      ## })
    }) |> bindEvent(input$calculate)
  })
}
