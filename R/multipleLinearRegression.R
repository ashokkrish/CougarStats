# R/multipleLinearRegression.R

## mnemonic: corset
## To set below the diagonal to NA, just t(corsetAboveDiagNA).
## Always calls "cor" with "use = 'complete.obs'".
corsetAboveDiagNA <- function(dat) {
  dat <- cor(dat, use = "complete.obs")
  for (column in seq(ncol(dat))) {
    if (!(column == ncol(dat))) {
      dat[column, (column + 1):ncol(dat)] <- NA
    }
  }
  return(dat)
}

MLRSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "MLRSidebar",
      useShinyjs(),
      withMathJax(
        helpText("Only numeric variables are selectable."),
        pickerInput(
          ns("responseVariable"),
          strong("Response Variable (\\(y\\))"),
          choices = NULL,
          multiple = FALSE,
          options = list(
            `live-search` = TRUE,
            title = "Nothing selected"
          )
        ),
        
        helpText("Select two or more explanatory variables (numeric)."),
        uiOutput(ns("singleOrMultipleHelpText")),
        pickerInput(
          inputId  = ns("explanatoryVariables"),
          label    = strong("Explanatory Variables (x₁, x₂, …, xₙ)"),
          choices  = NULL,
          multiple = TRUE,
          options  = list(
            `actions-box` = TRUE,   # “Select all / Deselect all” buttons
            `live-search` = TRUE,   # built-in search box
            selectedTextFormat = "values",
            multipleSeperator = ", "
          )
        ),
        actionButton(ns("calculate"), "Calculate", class = "act-btn"),
        actionButton(ns("reset"), "Reset Values", class = "act-btn")
      )))
}

MLRMainPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    navbarPage(title = NULL,
               tabPanel(
                 title = "Data Import",
                 value = "data_import_tab",
                 div(id = ns("importContainer")),
                 uiOutput(ns("fileImportUserMessage")),
                 import_file_ui(
                   id    = ns("dataImport"),
                   title = "")),
               tabPanel(title = "Model", uiOutput(ns("Equations")) ),
               tabPanel(title = "ANOVA", uiOutput(ns("ANOVA"))),
               tabPanel(title = "Multicollinearity Detection", uiOutput(ns("MulticollinearityDetection"))),
               tabPanel(title = "Diagnostic Plots", uiOutput(ns("DiagnosticPlots"))),
               tabPanel(title = "Uploaded Data", DTOutput(ns("uploadedDataTable"))),
               id = ns("mainPanel"),
               theme = bs_theme(version = 4))
  )
}

MLRServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    redrawing <- FALSE
    session$onFlushed(function() {
      if (redrawing) {
        shinyjs::runjs(r"[$('#rc-MLR-anovaTable > table > thead > tr').children().css({'font-style': 'italic'})]")
        redrawing <<- FALSE
      }
    },
    once = FALSE)
    
    uploadedTibble <- import_file_server(
      id = "dataImport",
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class = "tbl_df"
      
    )
    
    noFileCalculate <- reactiveVal(FALSE)
    
    observeEvent(TRUE, {
      shinyjs::delay(0, {
        hideTab(inputId = "mainPanel", target = "Model")
        hideTab(inputId = "mainPanel", target = "ANOVA")
        hideTab(inputId = "mainPanel", target = "Multicollinearity Detection")
        hideTab(inputId = "mainPanel", target = "Diagnostic Plots")
        hideTab(inputId = "mainPanel", target = "Uploaded Data")
      })
    }, once = TRUE)
    
    observeEvent(uploadedTibble$data(),{
      req(uploadedTibble$data())
      cols <- uploadedTibble$data() %>%
        dplyr::select_if(is.numeric) %>%
        colnames()
      
      updatePickerInput(
        session,
        inputId  = "responseVariable",
        choices  = cols,
        selected = character(0)
      )
      
      # populate the explanatory‐variables picker
      updatePickerInput(
        session,
        inputId = "explanatoryVariables",
        choices = cols
      )
    })
    ns <- session$ns
    
    observeEvent(input$reset, {
      hideTab(inputId = "mainPanel", target = "Model")
      hideTab(inputId = "mainPanel", target = "ANOVA")
      hideTab(inputId = "mainPanel", target = "Multicollinearity Detection")
      hideTab(inputId = "mainPanel", target = "Diagnostic Plots")
      hideTab(inputId = "mainPanel", target = "Uploaded Data")
      
      updatePickerInput(session, "responseVariable", selected = character(0))
      updatePickerInput(session, "explanatoryVariables", selected = character(0))
      
      updateNavbarPage(session, "mainPanel", selected = "data_import_tab")
    })
    
    ## Update the choices for the select inputs when the uploadedTibble changes.
    observe({
      updatePickerInput(inputId = "responseVariable",
                        choices = colnames(select_if(uploadedTibble$data(), is.numeric)),
                        selected = character(0))
      
      updatePickerInput(inputId = "explanatoryVariables",
                        choices = colnames(select_if(uploadedTibble$data(), is.numeric)))
    }) |> bindEvent(uploadedTibble$data())
    
    observeEvent(input$responseVariable, {
      # only run when we have data and a non-empty response variable
      req(uploadedTibble$data(), input$responseVariable)
      
      # grab just the numeric columns
      df_num <- uploadedTibble$data() %>% dplyr::select_if(is.numeric)
      
      # drop the response variable (any_of tolerates missing/empty)
      choices <- df_num %>%
        dplyr::select(-dplyr::any_of(input$responseVariable)) %>%
        colnames()
      
      # if the responseVar was in the explanatory selection, remove it
      selected_vars <- input$explanatoryVariables
      if (input$responseVariable %in% selected_vars) {
        selected_vars <- setdiff(selected_vars, input$responseVariable)
      }
      
      
      # update the widget
      updatePickerInput(
        session,
        "explanatoryVariables",
        choices    = choices,
        selected   = selected_vars
      )
    })
    
    output$singleOrMultipleHelpText <- renderUI({
      if (length(input$explanatoryVariables) > 1) {
        div(class = "text-success", span("Multiple explanatory variables result in a multiple linear regression."))
      } else if (length(input$explanatoryVariables) == 1) {
        div(class = "text-danger", span("Select at least one more explanatory variable; a single explanatory variable results in a simple linear regression."))
      }
    })
    
    MLRValidation <-
      quote(validate(
        need(uploadedTibble$name(), "Upload some data."),
        need(isTruthy(input$responseVariable), # This should ideally catch character(0)
             "A response variable is required."),
        # MODIFIED THIRD NEED CONDITION:
        need(
          (
            !is.null(input$responseVariable) &&
              length(input$responseVariable) == 1 &&
              nzchar(input$responseVariable[1]) &&
              (input$responseVariable[1] %in% names(uploadedTibble$data())) &&
              is.numeric(uploadedTibble$data()[[input$responseVariable[1]]])
          ),
          "Response variable must be an existing, single, and numeric column."
        ),
        # ... rest of your MLRValidation conditions
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
    
    output$fileImportUserMessage <- renderUI({ # This version responds to the button
      if (noFileCalculate()) {
        tags$div(class = "shiny-output-error-validation",
                 "Required: Cannot calculate without a data file.")
      } else {
        NULL
      }
    })
    
    observe({ # input$calculate
      if (!isTruthy(uploadedTibble$data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }
      
      isolate({
        
        output$anovaHypotheses <- renderUI({
          withMathJax(
            p(strong("Analysis of Variance (ANOVA)")),
            p(
              r"{\( H_0: \beta_1 = \beta_2 = \cdots = \beta_k = 0\)}",
              br(),
              r"{\( H_a: \) At least one \(\beta_j\ne 0\), where \(j = 1, \cdots, k\).}"
            ),
            br(),
            p(r"{\( \alpha = 0.05\ \)}"),
            br(),
            p(
              sprintf(r"{\( n = %i \)}", nrow(uploadedTibble$data())),
              br(),
              sprintf(r"{\( k = %i \)}", length(input$explanatoryVariables))
            )
          )
        })
        
        with(uploadedTibble$data(), {
          validate(
            need(
              isTruthy(input$explanatoryVariables),
              "Two or more explanatory variables must be selected."
            ),
            need(
              isTruthy(input$responseVariable),
              "A response variable must be selected."
            )
          )
          
          model <- lm(reformulate(
            as.character(lapply(input$explanatoryVariables, as.name)),
            as.name(input$responseVariable)
          ))
          
          modelCoefficients <-
            rownames_to_column(
              as.data.frame(summary(model)$coefficients),
              "Source"
            )
          
          # Get CIs, convert to data frame, and rename columns
          modelConfidenceIntervals <- as.data.frame(confint(model))
          colnames(modelConfidenceIntervals) <- c("Lower 95% CI", "Upper 95% CI")
          modelConfidenceIntervals <- rownames_to_column(modelConfidenceIntervals, "Source")
          
          output$linearModelCoefConfint <- renderTable(
            {
              final_table <- dplyr::left_join(modelCoefficients,
                                              modelConfidenceIntervals,
                                              by = "Source"
              )
              
              tibble::column_to_rownames(final_table, var = "Source")
            },
            rownames = TRUE, # This tells renderTable to display the row names
            na = "",
            striped = TRUE,
            align = "c",
            digits = 3
          )
          
          output$lmCoefConfintTableCaption <- renderUI({
            tagList(if (anyNA(modelConfidenceIntervals)) {
              p("Some model components (e.g. dependent variables) did not have confidence intervals or coefficients, and are dropped from the table.")
            } else {
              p("")
            })
          })
          
          output$rsquareAdjustedRSquareInterpretation <- renderUI({
            modelANOVA <- anova(model)
            SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
            SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)] # only the residuals
            SST <- SSR + SSE
            
            withMathJax(
              p(strong(r"{ \(R^2\) and Adjusted \(R^2\) }")),
              br(),
              p(sprintf(
                r"[\( \displaystyle R^2 = \frac{\text{SSR}}{\text{SST}} = \frac{%0.4f}{%0.4f} = %0.4f\)]",
                SSR,
                SST,
                SSR / SST
              )),
              p(sprintf(
                r"{
\(
\displaystyle
R^2_{\text{adj}} = 1 - \left[ \left( 1-R^2 \right) \frac{n-1}{n-k-1} \right] \\
R^2_{\text{adj}} = %0.4f
\)
}",
summary(model)$adj.r.squared
              )),
p(r"[where \(k\) is the number of independent (explanatory) variables in the regression model, and \(n\) is the number of observations in the dataset.]"),
p(
  strong("Interpretation:"),
  sprintf(
    r"[Roughly \(%.2f\%%\) of the variation in the response variable is explained by the multiple linear regression model when adjusted for the number of explanatory variables and the sample size.]",
    summary(model)$adj.r.squared * 100
  )
),
br(),
p(strong("Information Criteria")),
p(sprintf(r"[Akaike Information Criterion (AIC): \(%0.4f\)]", AIC(model))),
p(sprintf(r"[Bayesian Information Criterion (BIC): \(%0.4f\)]", BIC(model)))
            )
          })
          
          output$multicollinearityDetectionMainPanelUI <- renderUI({
            withMathJax(
              fluidRow(column(
                12, p(strong("Correlation matrix")),
                p("Values below -0.75 and above 0.75 are very strong indicators that there is multicollinearity, and that the component of the model should be removed."),
                tableOutput(session$ns("simpleCorrelationMatrix"))
              )),
              fluidRow(column(
                12, p(strong("Graphical methods")),
                p("Along the diagonal of this plot are the distributions of the data in each variable. Below the diagonal are the scatterplots of one variable against another; if the points form a more-or-less straight line then the variables are correlated. Above the diagonal are the correlation coefficients between two variables."),
                plotOutput(session$ns("ggscatmat"))
              )),
              fluidRow(column(
                12, p(strong("Variance Inflation Factors")),
                p("A VIF greater than 10 suggests strong multicollinearity caused by the respective variable with that variance inflation factor. VIFs between 5 and 10 hint at moderate multicollinearity. Values less than 5 are acceptable, with only a low degree of multicollinearity detected."),
                tableOutput(session$ns("vifs"))
              ))
            )
          })
          
          output$simpleCorrelationMatrix <- renderTable({
            corsetAboveDiagNA(uploadedTibble$data()[, input$explanatoryVariables])
          },
          rownames = TRUE,
          striped = TRUE,
          na = "",
          align = "c")
          
          output$vifs <- renderTable(
            {
              df <- tryCatch(
                {
                  as.data.frame(car::vif(model))
                },
                error = \(e) print(e)
              )
              validate(need(is.data.frame(df), "Couldn't produce a data frame from the VIF function of the model. Usually this means there are aliased coefficients in the model. Re-attempt after verifying the data, or renaming the columns."))
              df
            },
            rownames = TRUE,
            align = "c"
          )
          
          output$multicollinearityGgpairs <- renderPlot({
            ggpairs(anova(model))
          })
          
          output$ggscatmat <- renderPlot(
            {
              ggscatmat(uploadedTibble$data()[, input$explanatoryVariables], columns = input$explanatoryVariables)
            })
          
          output$ggnostic <- renderPlot({
            ggnostic(model)
          })
          
          output$anovaTable <- renderTable(
            {
              redrawing <<- TRUE
              options(knitr.kable.NA = "") # do not display NAs
              modelANOVA <- anova(model)
              SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
              SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)] # only the residuals
              SST <- SSR + SSE
              k <- length(input$explanatoryVariables)
              n <- nrow(uploadedTibble$data())
              MSR <- SSR / k
              MSE <- SSE / (n - k - 1)
              F <- MSR / MSE
              
              ## RETURN THE TABLE TO RENDER
              tibble::tribble(
                ~"Source", ~"df", ~"SS", ~"MS", ~"F", ~"P-value",
                "Regression", as.integer(k), SSR, MSR, F, pf(F, k, n - k - 1, lower.tail = FALSE),
                "Residual", as.integer(n - k - 1), SSE, MSE, NA, NA,
                "Total", as.integer(n - 1), SST, NA, NA, NA
              )
            },
            na = "",
            striped = TRUE,
            align = "c"
          )
          
          observe({
            shinyjs::runjs(r"[$('#rc-MLR-anovaTable > table > thead > tr').children().css({'font-style': 'italic'})]")
          })
          
          output$linearModelAIC <- renderPrint({
            print(AIC(model))
            print(paste("The AIC of the model and the AIC of the log-likelihood of the model are equal?", all.equal(AIC(model), AIC(logLik(model)))))
          })
          output$linearModelBIC <- renderPrint({
            BIC(model)
          })
          output$linearModelResidualsPanelPlot <- renderPlot({
            par(mfrow = c(2, 2))
            plot(model, pch = 20)
          })
        })
        
        output$anovaPValueMethod <- renderUI({
          with(uploadedTibble$data(), {
            model <- lm(reformulate(
              as.character(lapply(input$explanatoryVariables, as.name)),
              as.name(input$responseVariable)
            ))
            anovaModel <- anova(model)
            regressionVariables <- nrow(anovaModel) - 1
            modelANOVA <- anova(model)
            SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
            SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)] # only the residuals
            SST <- SSR + SSE
            k <- length(input$explanatoryVariables)
            n <- nrow(uploadedTibble$data())
            MSR <- SSR / k
            MSE <- SSE / (n - k - 1)
            F <- MSR / MSE
            withMathJax(
              p(strong("Test Statistic")),
              p(sprintf(
                r"{\(\displaystyle F = \frac{\text{MSR}}{\text{MSE}} = \frac{%0.2f}{%0.2f} = %0.2f \)}",
                MSR, MSE, F
              )),
              p(sprintf(
                r"{
\(
R^2 = \frac{\text{SSR}}{\text{SST}} \\
R^2_{\text{adj}} = \frac{%0.2f}{%0.2f} \\
R^2_{\text{adj}} = %0.2f \\
\)
}",
anovaModel$SSR, anovaModel$SST, anovaModel$SSR / anovaModel$SST
              )),
p(strong("Conclusion:")),
{
  pValue <- pf(F, k, n - k - 1, lower.tail = FALSE)
  p(sprintf(
    r"[Since the p-value is %s than \(\alpha\) (\(%0.3f %s 0.05\)), %s.]",
    if (pValue <= 0.05) "less" else "greater",
    pValue,
    if (pValue <= 0.05) r"[\le]" else r"[\ge]",
    if (pValue <= 0.05) {
      r"[we reject the null hypothesis (\(H_0\)) and conclude there is enough statistical evidence to support the alternative hypothesis (\(H_a\))]"
    } else {
      r"[we do not reject the null hypothesis (\(H_0\)) and conclude there isn't enough statistical evidence to support the alternative hypothesis (\(H_a\)).]"
    }
  ))
}
            )
          })
        })
        
        output$plot <- renderPlot({
          validate(need(uploadedTibble$name(), "Upload some data."))
          validate(need(
            isTruthy(input$responseVariable),
            "Select at least one explanatory variable."
          ))
          responseVariableData <- uploadedTibble$data()[[input$responseVariable]]
          validate(need(
            is.numeric(responseVariableData),
            "Response variable must be numeric."
          ))
          hist(responseVariableData)
        })
        
        output$linearModelEquations <- renderUI(withMathJax(
          div(
            id = "linear-model-equations",
            p("The variables in the model are"),
            p(paste(
              r"{\(}",
              sprintf(r"[y    = \text{%s} \\]", input$responseVariable),
              paste(
                sprintf(
                  r"[x_{%d} = \text{%s}]",
                  seq_along(input$explanatoryVariables),
                  input$explanatoryVariables
                ),
                collapse = r"[\\]"
              ),
              r"{\)}"
            )),
            p("The estimated multiple linear regression equation is"),
            p(with(uploadedTibble$data(), {
              req(uploadedTibble$data(), cancelOutput = TRUE)
              req(isTruthy(input$responseVariable), cancelOutput = TRUE)
              model <- lm(reformulate(
                as.character(lapply(input$explanatoryVariables, as.name)),
                as.name(input$responseVariable)
              ))
              
              ## Reactively generate the LaTeX for the regression model equation.
              modelEquations <- with(as.list(coefficients(model)), {
                req(as.list(coefficients(model)),
                    cancelOutput = "progress"
                )
                paste(
                  r"{\(}",
                  gsub(
                    pattern = r"{\+(\ +)?-}",
                    replacement = "-",
                    x = paste(
                      c(
                        sprintf(
                          r"[\hat{y} = \hat{\beta_0} + %s]",
                          paste(
                            sprintf(
                              r"[\hat{%s}]",
                              paste0(
                                r"[\beta_{]",
                                seq_along(input$explanatoryVariables),
                                r"[}]"
                              )
                            ),
                            paste0(r"{x_{}", seq_along(input$explanatoryVariables),
                                   r"[}]"),
                            collapse = "+"
                          )
                        ),
                        sprintf(
                          r"[\hat{y} = %.3f + %s]",
                          get("(Intercept)"),
                          paste(
                            as.character(lapply(
                              mget(as.character(lapply(input$explanatoryVariables, as.name))),
                              \(x) if(!is.na(x)) sprintf(r"[%.3f]", x) else ""
                            )),
                            paste0(r"[x_{]", seq_along(input$explanatoryVariables),
                                   r"[}]"),
                            collapse = "+"
                          )
                        ),
                        ""
                      ),
                      collapse = r"[\\]"
                    )
                  ),
                  r"{\)}",
                  sep = r"{\\}"
                )
              }) # <- modelEquations
            }))
          )
        ))
        
        output$linearModelCoefficientsAndConfidenceIntervals <- renderUI({
          column(
            12,
            p(strong("Coefficients and Confidence Intervals")),
            tableOutput(ns("linearModelCoefConfint")),
            uiOutput(ns("lmCoefConfintTableCaption"))
          )
        })
        
      }) ## end of isolation
      
      output$uploadedDataTable <- renderDT({
        req(uploadedTibble$data())
        datatable(uploadedTibble$data(),
                  options = list(pageLength = -1,
                                 lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All"))))
      })
      
      
      showTab(inputId = "mainPanel", target = "Model")
      showTab(inputId = "mainPanel", target = "ANOVA")
      showTab(inputId = "mainPanel", target = "Multicollinearity Detection")
      showTab(inputId = "mainPanel", target = "Diagnostic Plots")
      showTab(inputId = "mainPanel", target = "Uploaded Data")
      updateNavbarPage(session, "mainPanel", selected = "Model")
    }) |> bindEvent(input$calculate)
    
    observe({
      if(isTruthy(uploadedTibble$data())){
        noFileCalculate(FALSE)
      }
    }) |> bindEvent(uploadedTibble$data(), ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$Equations <- renderUI({
      eval(MLRValidation)
      
      fluidPage(
        ## NOTE: variables and equations are both in linearModelEquations.
        fluidRow(uiOutput(ns("linearModelEquations"))),
        fluidRow(uiOutput(ns("linearModelCoefficientsAndConfidenceIntervals")))
      )
    })
    
    output$ANOVA <- renderUI({
      eval(MLRValidation)
      
      fluidPage(
        fluidRow(uiOutput(ns("anovaHypotheses"))),
        fluidRow(tableOutput(ns("anovaTable"))),
        fluidRow(uiOutput(ns("anovaPValueMethod"))),
        fluidRow(uiOutput(ns("rsquareAdjustedRSquareInterpretation")))
      )
    })
    
    output$MulticollinearityDetection <- renderUI({
      eval(MLRValidation)
      
      uiOutput(ns("multicollinearityDetectionMainPanelUI"))
    })
    
    output$DiagnosticPlots <- renderUI({
      eval(MLRValidation)
      
      fluidPage(fluidRow(plotOutput(ns("linearModelResidualsPanelPlot"))))
    })
    
    
  })
}