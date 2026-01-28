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
        div(
          id = ns("responseVariableWrapper"),
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
          uiOutput(ns("responseVariableError"))
        ),
        
        helpText("Select two or more explanatory variables (numeric)."),
        uiOutput(ns("singleOrMultipleHelpText")),
        div(
          id = ns("explanatoryVariablesWrapper"),
          pickerInput(
            inputId  = ns("explanatoryVariables"),
            label    = strong("Explanatory Variables (x₁, x₂, …, xₖ)"),
            choices  = NULL,
            multiple = TRUE,
            options  = list(
              `actions-box` = TRUE,   # "Select all / Deselect all" buttons
              `live-search` = TRUE,   # built-in search box
              selectedTextFormat = "values",
              multipleSeperator = ", "
            )
          ),
          uiOutput(ns("explanatoryVariablesError"))
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
               tabPanel(title = "Inference", uiOutput(ns("Inference"))),
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
    
    # Reactive values for validation errors
    responseVarError <- reactiveVal(FALSE)
    explanatoryVarsError <- reactiveVal(FALSE)
    
    observeEvent(TRUE, {
      shinyjs::delay(0, {
        hideTab(inputId = "mainPanel", target = "Model")
        hideTab(inputId = "mainPanel", target = "Inference")
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
      hideTab(inputId = "mainPanel", target = "Inference")
      hideTab(inputId = "mainPanel", target = "ANOVA")
      hideTab(inputId = "mainPanel", target = "Multicollinearity Detection")
      hideTab(inputId = "mainPanel", target = "Diagnostic Plots")
      hideTab(inputId = "mainPanel", target = "Uploaded Data")
      
      updatePickerInput(session, "responseVariable", selected = character(0))
      updatePickerInput(session, "explanatoryVariables", selected = character(0))
      
      # Clear validation errors
      responseVarError(FALSE)
      explanatoryVarsError(FALSE)
      shinyjs::removeClass(id = "responseVariableWrapper", class = "has-error")
      shinyjs::removeClass(id = "explanatoryVariablesWrapper", class = "has-error")
      
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
        if (is.null(nonnumericVariables)) nonnumericVariables <- var
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
    
    output$responseVariableError <- renderUI({
      if (responseVarError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Please select a response variable."
        )
      }
    })
    
    output$explanatoryVariablesError <- renderUI({
      if (explanatoryVarsError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Please select at least two explanatory variables."
        )
      }
    })
    
    # Clear errors when variables are selected
    observeEvent(input$responseVariable, {
      if (isTruthy(input$responseVariable)) {
        responseVarError(FALSE)
        shinyjs::removeClass(id = "responseVariableWrapper", class = "has-error")
      }
    })
    
    observeEvent(input$explanatoryVariables, {
      if (length(input$explanatoryVariables) >= 2) {
        explanatoryVarsError(FALSE)
        shinyjs::removeClass(id = "explanatoryVariablesWrapper", class = "has-error")
      }
    })
    
    observe({ # input$calculate
      if (!isTruthy(uploadedTibble$data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }
      
      # Validate response variable
      hasResponseVar <- isTruthy(input$responseVariable)
      if (!hasResponseVar) {
        responseVarError(TRUE)
        shinyjs::addClass(id = "responseVariableWrapper", class = "has-error")
      } else {
        responseVarError(FALSE)
        shinyjs::removeClass(id = "responseVariableWrapper", class = "has-error")
      }
      
      # Validate explanatory variables (need at least 2)
      hasExplanatoryVars <- isTruthy(input$explanatoryVariables) && length(input$explanatoryVariables) >= 2
      if (!hasExplanatoryVars) {
        explanatoryVarsError(TRUE)
        shinyjs::addClass(id = "explanatoryVariablesWrapper", class = "has-error")
      } else {
        explanatoryVarsError(FALSE)
        shinyjs::removeClass(id = "explanatoryVariablesWrapper", class = "has-error")
      }
      
      # Only show tabs if validation passes
      if (!hasResponseVar || !hasExplanatoryVars) {
        return()
      }
      
      showTab(inputId = "mainPanel", target = "Model")
      showTab(inputId = "mainPanel", target = "Inference")
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
        fluidRow(uiOutput(ns("linearModelEquations")))
      )
    })
    
    output$Inference <- renderUI({
      eval(MLRValidation)
      
      fluidPage(
        fluidRow(uiOutput(ns("linearModelCoefficientsAndConfidenceIntervals")))
      )
    })
    
    # Reactive coefficients and confidence intervals table
    output$linearModelCoefConfint <- renderTable(
      {
        req(uploadedTibble$data())
        req(isTruthy(input$responseVariable))
        req(isTruthy(input$explanatoryVariables))
        req(length(as.character(input$explanatoryVariables)) >= 2)
        
        with(uploadedTibble$data(), {
          model <- lm(reformulate(
            as.character(lapply(input$explanatoryVariables, as.name)),
            as.name(input$responseVariable)
          ))
          
          modelCoefficients <-
            rownames_to_column(
              as.data.frame(summary(model)$coefficients),
              "Source"
            )
          
          # Rename "Pr(>|t|)" to "P-value"
          names(modelCoefficients)[names(modelCoefficients) == "Pr(>|t|)"] <- "P-value"
          
          # Get CIs, convert to data frame, and rename columns
          modelConfidenceIntervals <- as.data.frame(confint(model))
          colnames(modelConfidenceIntervals) <- c("Lower 95% CI", "Upper 95% CI")
          modelConfidenceIntervals <- rownames_to_column(modelConfidenceIntervals, "Source")
          
          final_table <- dplyr::left_join(modelCoefficients,
                                          modelConfidenceIntervals,
                                          by = "Source"
          )
          
          tibble::column_to_rownames(final_table, var = "Source")
        })
      },
      rownames = TRUE,
      na = "",
      striped = TRUE,
      align = "c",
      digits = 3
    )
    
    output$lmCoefConfintTableCaption <- renderUI({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        
        modelConfidenceIntervals <- as.data.frame(confint(model))
        
        tagList(if (anyNA(modelConfidenceIntervals)) {
          p("Some model components (e.g. dependent variables) did not have confidence intervals or coefficients, and are dropped from the table.")
        } else {
          p("")
        })
      })
    })
    
    # Reactive Model tab outputs
    output$linearModelCoefficientsAndConfidenceIntervals <- renderUI({
      column(
        12,
        p(strong("Coefficients and Confidence Intervals")),
        tableOutput(ns("linearModelCoefConfint")),
        uiOutput(ns("lmCoefConfintTableCaption"))
      )
    })
    
    output$linearModelEquations <- renderUI({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      withMathJax(
        div(
          id = "linear-model-equations",
          # 1. Calculate the model first to know what variables are kept
          with(uploadedTibble$data(), {
            req(uploadedTibble$data(), cancelOutput = TRUE)
            req(isTruthy(input$responseVariable), cancelOutput = TRUE)
            model <- lm(reformulate(
              as.character(lapply(input$explanatoryVariables, as.name)),
              as.name(input$responseVariable)
            ))
            
            # Get all coefficients (includes NAs for dropped variables)
            all_coefs <- coef(model)
            intercept <- all_coefs["(Intercept)"]
            
            # Lists to store the LaTeX strings
            vars_definitions <- c()
            sym_terms <- c()
            num_terms <- c()
            
            # Loop through ORIGINAL input variables to preserve indices (1, 2, 3...)
            for(i in seq_along(input$explanatoryVariables)) {
              var_name <- input$explanatoryVariables[i]
              val <- all_coefs[var_name]
              
              # If the coefficient is NOT NA, we include it.
              # If it IS NA (dropped), we skip it completely (creating a gap in the indices shown).
              if(!is.na(val)) {
                # Add to definitions list using index 'i'
                vars_definitions <- c(vars_definitions, sprintf(r"[x_{%d} = \text{%s}]", i, var_name))
                
                # Add to equation terms using index 'i'
                sym_terms <- c(sym_terms, sprintf(r"[\hat{\beta_{%d}} x_{%d}]", i, i))
                num_terms <- c(num_terms, sprintf(r"[%.3f x_{%d}]", val, i))
              }
            }
            
            # 3. Render the "Variables in the model" list
            vars_definition_latex <- paste(
              r"{\(}",
              sprintf(r"[y    = \text{%s} \\]", input$responseVariable),
              paste(vars_definitions, collapse = r"[\\]"),
              r"{\)}"
            )
            
            # 4. Render the Equation
            sym_eq <- sprintf(r"[\hat{y} = \hat{\beta_0} + %s]", paste(sym_terms, collapse = " + "))
            num_eq <- sprintf(r"[\hat{y} = %.3f + %s]", intercept, paste(num_terms, collapse = " + "))
            
            # Clean up double signs (e.g. "+ -")
            sym_eq <- gsub(pattern = r"{\+(\ +)?-}", replacement = "-", x = sym_eq)
            num_eq <- gsub(pattern = r"{\+(\ +)?-}", replacement = "-", x = num_eq)
            
            equation_latex <- paste(
              r"{\(}",
              sym_eq,
              num_eq,
              r"{\)}",
              sep = r"{\\}"
            )
            
            # Return the HTML structure
            tagList(
              p("The variables in the model are"),
              p(vars_definition_latex),
              p("The estimated multiple linear regression equation is"),
              p(equation_latex)
            )
          })
        )
      )
    })
    
    # Reactive ANOVA tab outputs
    output$anovaHypotheses <- renderUI({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        
        # Use model rank for k to match ANOVA table
        k <- model$rank - 1
        
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
            sprintf(r"{\( k = %i \)}", k)
          )
        )
      })
    })
    
    output$anovaTable <- renderTable(
      {
        req(uploadedTibble$data())
        req(isTruthy(input$responseVariable))
        req(isTruthy(input$explanatoryVariables))
        req(length(as.character(input$explanatoryVariables)) >= 2)
        
        redrawing <<- TRUE
        options(knitr.kable.NA = "") # do not display NAs
        
        with(uploadedTibble$data(), {
          model <- lm(reformulate(
            as.character(lapply(input$explanatoryVariables, as.name)),
            as.name(input$responseVariable)
          ))
          
          modelANOVA <- anova(model)
          SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
          SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)] # only the residuals
          SST <- SSR + SSE
          
          # Use model rank
          k <- model$rank - 1
          n <- nrow(uploadedTibble$data())
          MSR <- SSR / k
          MSE <- SSE / (n - k - 1)
          F_stat <- MSR / MSE
          
          ## RETURN THE TABLE TO RENDER
          tibble::tribble(
            ~"Source", ~"df", ~"SS", ~"MS", ~"F", ~"P-value",
            "<strong>Regression (Model)</strong>", as.integer(k), SSR, MSR, F_stat, pf(F_stat, k, n - k - 1, lower.tail = FALSE),
            "<strong>Residual (Error)</strong>", as.integer(n - k - 1), SSE, MSE, NA, NA,
            "Total", as.integer(n - 1), SST, NA, NA, NA
          )
        })
      },
      na = "",
      striped = TRUE,
      align = "c",
      sanitize.text.function = function(x) x
    )
    
    output$anovaPValueMethod <- renderUI({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        anovaModel <- anova(model)
        
        SSR <- sum(anovaModel$"Sum Sq"[-nrow(anovaModel)]) # all but the residuals
        SSE <- anovaModel$"Sum Sq"[nrow(anovaModel)] # only the residuals
        
        # Use model rank
        k <- model$rank - 1
        n <- nrow(uploadedTibble$data())
        MSR <- SSR / k
        MSE <- SSE / (n - k - 1)
        F_stat <- MSR / MSE
        
        withMathJax(
          p(strong("Test Statistic:")),
          p(sprintf(
            r"{\(\displaystyle F = \frac{\text{MSR}}{\text{MSE}} = \frac{%0.2f}{%0.2f} = %0.2f \)}",
            MSR, MSE, F_stat
          )),
          p(strong("Conclusion:")),
          {
            pValue <- pf(F_stat, k, n - k - 1, lower.tail = FALSE)
            p(sprintf(
              r"[Since the p-value is %s than \(\alpha\) (\(%0.3f %s 0.05\)), %s.]",
              if (pValue <= 0.05) "less" else "greater",
              pValue,
              if (pValue <= 0.05) r"[\le]" else r"[>]",
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
    
    output$rsquareAdjustedRSquareInterpretation <- renderUI({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        
        modelANOVA <- anova(model)
        SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
        SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)] # only the residuals
        SST <- SSR + SSE
        
        # Use model rank
        k <- model$rank - 1
        n <- nrow(uploadedTibble$data())
        
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
    })
    
    # Reactive Multicollinearity Detection tab outputs
    output$multicollinearityDetectionMainPanelUI <- renderUI({
      withMathJax(
        fluidRow(column(
          12, p(strong("Correlation matrix")),
          p("Values below -0.75 and above 0.75 are very strong indicators that there is multicollinearity, and that the component of the model should be removed."),
          tableOutput(ns("simpleCorrelationMatrix"))
        )),
        fluidRow(column(
          12, p(strong("Graphical methods")),
          p("Along the diagonal of this plot are the distributions of the data in each variable. Below the diagonal are the scatterplots of one variable against another; if the points form a more-or-less straight line then the variables are correlated. Above the diagonal are the correlation coefficients between two variables."),
          plotOutput(ns("ggscatmat"))
        )),
        fluidRow(column(
          12, p(strong("Variance Inflation Factors")),
          p("A VIF greater than 10 suggests strong multicollinearity caused by the respective variable with that variance inflation factor. VIFs between 5 and 10 hint at moderate multicollinearity. Values less than 5 are acceptable, with only a low degree of multicollinearity detected."),
          tableOutput(ns("vifs"))
        ))
      )
    })
    
    output$simpleCorrelationMatrix <- renderTable({
      req(uploadedTibble$data())
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      corsetAboveDiagNA(uploadedTibble$data()[, input$explanatoryVariables])
    },
    rownames = TRUE,
    striped = TRUE,
    na = "",
    align = "c")
    
    output$vifs <- renderTable(
      {
        req(uploadedTibble$data())
        req(isTruthy(input$responseVariable))
        req(isTruthy(input$explanatoryVariables))
        req(length(as.character(input$explanatoryVariables)) >= 2)
        with(uploadedTibble$data(), {
          model <- lm(reformulate(
            as.character(lapply(input$explanatoryVariables, as.name)),
            as.name(input$responseVariable)
          ))
          df <- tryCatch(
            {
              as.data.frame(car::vif(model))
            },
            error = \(e) NULL
          )
          validate(need(is.data.frame(df), "Couldn't produce a data frame from the VIF function of the model. Usually this means there are aliased coefficients in the model. Re-attempt after verifying the data, or renaming the columns."))
          df
        })
      },
      rownames = TRUE,
      align = "c"
    )
    
    output$ggscatmat <- renderPlot({
      req(uploadedTibble$data())
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      ggscatmat(uploadedTibble$data()[, input$explanatoryVariables], columns = input$explanatoryVariables)
    })
    
    # Reactive Diagnostic Plots tab outputs
    output$mlrResidualsPanelPlot1 <- renderPlot({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        plot(model, which = 1, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    output$mlrResidualsPanelPlot2 <- renderPlot({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        plot(model, which = 2, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    output$mlrResidualsPanelPlot3 <- renderPlot({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        plot(model, which = 3, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    output$mlrResidualsPanelPlot4 <- renderPlot({
      req(uploadedTibble$data())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(uploadedTibble$data(), {
        model <- lm(reformulate(
          as.character(lapply(input$explanatoryVariables, as.name)),
          as.name(input$responseVariable)
        ))
        plot(model, which = 5, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    # Reactive Uploaded Data tab output
    output$uploadedDataTable <- renderDT({
      req(uploadedTibble$data())
      datatable(uploadedTibble$data(),
                options = list(pageLength = -1,
                               lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All"))))
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
      
      fluidPage(
        plotOutput(ns("mlrResidualsPanelPlot1")),
        plotOutput(ns("mlrResidualsPanelPlot2")),
        plotOutput(ns("mlrResidualsPanelPlot3")),
        plotOutput(ns("mlrResidualsPanelPlot4"))
      )
    })
    
    
  })
}