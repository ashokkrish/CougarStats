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
        HTML(uploadDataDisclaimer),
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
            label = HTML("<strong>Explanatory Variables (\\(x_1, x_2, x_3...x_n\\)) </strong> "),
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
    tags$head(
      tags$style(HTML("
        #linear-model-equations mjx-container[display='true'] {
          display: flex !important;
          justify-content: flex-start !important;
          text-align: left !important;
          padding-left: 0px !important;
          margin-left: 0px !important;
        }
      "))
    ),
    navbarPage(title = NULL,
               tabPanel(
                 title = "Data Import",
                 value = "data_import_tab",
                 div(id = ns("importContainer")),
                 uiOutput(ns("fileImportUserMessage")),
                 import_file_ui(
                   id    = ns("dataImport"),
                   title = "")),
               tabPanel(
                 title = "Variable Encoding",
                 value = "encoding_tab",
                 uiOutput(ns("encodingUI"))
               ),
               tabPanel(title = "Model", uiOutput(ns("Equations")) ),
               tabPanel(title = "Inference", uiOutput(ns("ANOVAAndInference"))),
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
    
    encodedData <- reactiveVal(NULL)
    
    observeEvent(uploadedTibble$data(), {
      encodedData(uploadedTibble$data())
    })
    
    noFileCalculate <- reactiveVal(FALSE)
    
    # Reactive values for validation errors
    responseVarError <- reactiveVal(FALSE)
    explanatoryVarsError <- reactiveVal(FALSE)
    
    observeEvent(TRUE, {
      shinyjs::delay(0, {
        hideTab(inputId = "mainPanel", target = "Model")
        hideTab(inputId = "mainPanel", target = "Inference")  
        hideTab(inputId = "mainPanel", target = "ANOVA & Parameter Estimates")
        hideTab(inputId = "mainPanel", target = "Multicollinearity Detection")
        hideTab(inputId = "mainPanel", target = "Diagnostic Plots")
        hideTab(inputId = "mainPanel", target = "Uploaded Data")
      })
    }, once = TRUE)
    
    observeEvent(encodedData(),{
      req(encodedData())
      cols <- encodedData() %>%
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
      hideTab(inputId = "mainPanel", target = "ANOVA & Parameter Estimates")
      hideTab(inputId = "mainPanel", target = "Multicollinearity Detection")
      hideTab(inputId = "mainPanel", target = "Diagnostic Plots")
      hideTab(inputId = "mainPanel", target = "Uploaded Data")
      
      updatePickerInput(session, "responseVariable", selected = character(0))
      updatePickerInput(session, "explanatoryVariables", selected = character(0))
      
      # Clear validation errors # change this 2026-05-11
      responseVarError(FALSE)
      explanatoryVarsError(FALSE)
      shinyjs::removeClass(id = "responseVariableWrapper", class = "has-error")
      shinyjs::removeClass(id = "explanatoryVariablesWrapper", class = "has-error")
      
      updateNavbarPage(session, "mainPanel", selected = "data_import_tab")
    })
    
    ## Update the choices for the select inputs when the uploadedTibble changes.
    observe({
      updatePickerInput(inputId = "responseVariable",
                        choices = colnames(select_if(encodedData(), is.numeric)),
                        selected = character(0))
      
      updatePickerInput(inputId = "explanatoryVariables",
                        choices = colnames(select_if(encodedData(), is.numeric)))
    }) |> bindEvent(encodedData())
    
    observeEvent(input$responseVariable, {
      # only run when we have data and a non-empty response variable
      req(encodedData(), input$responseVariable)
      
      # grab just the numeric columns
      df_num <- encodedData() %>% dplyr::select_if(is.numeric) 
      
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
    
   # try to fix multi col
     # Clean predictors for multicollinearity diagnostics
    mlrPredictors <- reactive({
      
      req(encodedData())
      req(isTruthy(input$explanatoryVariables))
      req(input$responseVariable)
      
      df <- encodedData()[, input$explanatoryVariables, drop = FALSE]
      
      removed_zero_var <- c()
      removed_aliased  <- c()
      
      # --------------------------------------------------------
      # Remove zero variance columns
      # --------------------------------------------------------
      
      zero_var <- sapply(df, function(x)
        sd(x, na.rm = TRUE) == 0
      )
      
      if(any(zero_var)) {
        
        removed_zero_var <- names(df)[zero_var]
        
        df <- df[, !zero_var, drop = FALSE]
      }
      
      # --------------------------------------------------------
      # Detect aliased / linearly dependent predictors
      # --------------------------------------------------------
      
      temp_df <- cbind(
        y = encodedData()[[input$responseVariable]],
        df
      )
      
      model <- lm(y ~ ., data = temp_df)
      
      aliased <- alias(model)$Complete
      
      if(!is.null(aliased)) {
        
        removed_aliased <- rownames(aliased)
        
        df <- df[, !colnames(df) %in% removed_aliased, drop = FALSE]
      }
      
      # --------------------------------------------------------
      # Notifications
      # --------------------------------------------------------
      
      if(length(removed_zero_var) > 0) {
        
        showNotification(
          paste(
            "Removed zero variance variable(s):",
            paste(removed_zero_var, collapse = ", ")
          ),
          type = "warning"
        )
      }
      
      if(length(removed_aliased) > 0) {
        
        showNotification(
          paste(
            "Removed linearly dependent variable(s):",
            paste(removed_aliased, collapse = ", ")
          ),
          type = "warning"
        )
      }
      
      validate(
        need(
          ncol(df) >= 2,
          "Not enough valid explanatory variables remain after removing problematic predictors."
        )
      )
      
      df
    })
    
    # try to fix multi co 
    
    
    
    
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
              (input$responseVariable[1] %in% names(encodedData())) &&
              is.numeric(encodedData()[[input$responseVariable[1]]])
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
      if (is.numeric(encodedData()[[var]]))
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
      if (anyNA(encodedData()[[var]]))
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
    
    
    output$encodingUI <- renderUI({
      req(uploadedTibble$data())
      
      df <- uploadedTibble$data()
      catCols <- df %>% 
        dplyr::select_if(~ is.character(.) || is.factor(.)) %>% 
        colnames()
      
      if (length(catCols) == 0) {
        return(p("No categorical variables detected in the uploaded data."))
      }
      
      tagList(
        p(strong("Categorical Variable Encoding")),
        p("The following categorical variables were detected. Select an encoding method for each."),
        lapply(catCols, function(col) {
          uniqueVals <- as.character(unique(df[[col]]))
          
          fluidRow(
            column(4,
                   p(strong(col)),
                   em(paste("Unique values:", length(uniqueVals)))
            ),
            column(4,
                   selectInput(
                     ns(paste0("encoding_", col)),
                     label = NULL,
                     choices = c(
                       "Do not encode"  = "none",
                       "Dummy encoding" = "dummy",
                       "Label encoding" = "label"
                     )
                   )
            ),
            column(4,
                   # Only show drag-and-drop when label encoding is selected
                   conditionalPanel(
                     condition = sprintf(
                       "input['%s'] === 'label'",
                       ns(paste0("encoding_", col))
                     ),
                     p(em("Drag to set order (top = 1):")),
                     rank_list(
                       text    = NULL,
                       labels  = uniqueVals,
                       input_id = ns(paste0("order_", col))
                     )
                   )
            )
          )
        }),
        actionButton(ns("applyEncoding"), "Apply Encoding", class = "act-btn")
      )
    })
    
    observeEvent(input$applyEncoding, {
      req(uploadedTibble$data())
      
      df <- uploadedTibble$data()
      catCols <- df %>% 
        dplyr::select_if(~ is.character(.) || is.factor(.)) %>% 
        colnames()
      
      for (col in catCols) {
        method <- input[[paste0("encoding_", col)]]
        
        if (method == "label") {
          # Use the user-defined order from the drag-and-drop
          userOrder <- input[[paste0("order_", col)]]
          
          df[[col]] <- factor(df[[col]], levels = userOrder, ordered = TRUE)
          df[[col]] <- as.numeric(df[[col]])
          
        } else if (method == "dummy") {
          dummies <- model.matrix(~ . - 1, data = df[col])[, -1, drop = FALSE]
          colnames(dummies) <- paste0(col, "_", colnames(dummies))
          df[[col]] <- NULL
          df <- cbind(df, dummies)
        }
      }
      
      encodedData(df)
      
      newCols <- df %>% dplyr::select_if(is.numeric) %>% colnames()
      updatePickerInput(session, "responseVariable", choices = newCols, selected = character(0))
      updatePickerInput(session, "explanatoryVariables", choices = newCols, selected = character(0))
      
      updateNavbarPage(session, "mainPanel", selected = "encoding_tab")
      
      showNotification("Encoding applied successfully.", type = "message")
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
      if (!isTruthy(encodedData())) {
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
      showTab(inputId = "mainPanel", target = "ANOVA & Parameter Estimates")
      showTab(inputId = "mainPanel", target = "Multicollinearity Detection")
      showTab(inputId = "mainPanel", target = "Diagnostic Plots")
      showTab(inputId = "mainPanel", target = "Uploaded Data")
      updateNavbarPage(session, "mainPanel", selected = "Model")
    }) |> bindEvent(input$calculate)
    
    observe({
      if(isTruthy(encodedData())){
        noFileCalculate(FALSE)
      }
    }) |> bindEvent(encodedData(), ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$Equations <- renderUI({
      eval(MLRValidation)
      
      fluidPage(
        ## NOTE: variables and equations are both in linearModelEquations.
        fluidRow(uiOutput(ns("linearModelEquations")))
      )
    })
    
    # Corresponds to the inference tab, after calcualtions
    
    # output$Inference <- renderUI({
    #   eval(MLRValidation)
    #   
    #   fluidPage(
    #     fluidRow(uiOutput(ns("linearModelCoefficientsAndConfidenceIntervals"))),
    #     fluidRow(hr()),
    #     fluidRow(uiOutput(ns("bpTest"))),
    #     fluidRow(hr()),
    #     fluidRow(uiOutput(ns("whiteTest")))
    #   )
    # })
    
    
    
    # Reactive coefficients and confidence intervals table
    
    output$linearModelCoefConfint <- renderTable(
      {
        req(encodedData())
        req(isTruthy(input$responseVariable))
        req(isTruthy(input$explanatoryVariables))
        req(length(as.character(input$explanatoryVariables)) >= 2)
        
      
        
        with(encodedData(), {
        
          
          model <- lm(reformulate(
            sprintf("`%s`", input$explanatoryVariables),
            sprintf("`%s`", input$responseVariable)
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
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
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
    
    
    # Breush-Pagan Test
    output$bpTest <- renderUI({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        
        bp    <- lmtest::bptest(model)
        pval  <- bp$p.value
        bpStat <- bp$statistic
        df    <- bp$parameter
        
        withMathJax(
          p(strong("Breusch-Pagan Test for Heteroscedasticity")),
          p("Tests whether the variance of residuals is constant (homoscedasticity)."),
          p(r"{\( H_0: \) The variance of the residuals is constant / does not depend on the independent variable (Homoscedasticity is present).}"),
          p(r"{\( H_a: \) The variance of the residuals is not constant / depends on the independent variable (Heteroscedasticity is present).}"),
          p(r"{\( \alpha = 0.05 \)}"),
          p(sprintf(
            r"{\( BP = %.4f, \quad df = %d, \quad p\text{-value} = %.4f \)}",
            bpStat, df, pval
          )),
          p(
            strong("Conclusion:"),
            if (pval <= 0.05) {
              r"(Since the p-value is less than \(\alpha\), we reject \(H_0\) and conclude there is evidence of heteroscedasticity.)"
            } else {
              r"(Since the p-value is greater than \(\alpha\), we fail to reject \(H_0\) and conclude there is no significant evidence of heteroscedasticity.)"
            }
          )
        )
      })
    })
    
  # White Test
    
    output$whiteTest <- renderUI({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        
        white_test  <- skedastic::white(model)
        pval  <- white_test$p.value
        W_stat <- white_test$statistic
        df    <- white_test$parameter
        
        withMathJax(
          p(strong("White Test for Heteroscedasticity")),
          p("Tests whether the variance of residuals is constant (homoscedasticity)."),
          p(r"{\( H_0: \) Homoscedasticity — residual variance is constant.}"),
          p(r"{\( H_a: \) Heteroscedasticity — residual variance is not constant.}"),
          p(r"{\( \alpha = 0.05 \)}"),
          p(sprintf(
            r"{\( W = %.4f, \quad df = %d, \quad p\text{-value} = %.4f \)}",
            W_stat, df, pval
          )),
          p(
            strong("Conclusion:"),
            if (pval <= 0.05) {
              r"(Since the p-value is less than \(\alpha\), we reject \(H_0\) and conclude there is evidence of heteroscedasticity.)"
            } else {
              r"(Since the p-value is greater than \(\alpha\), we fail to reject \(H_0\) and conclude there is no significant evidence of heteroscedasticity.)"
            }
          )
        )
      })
    })
    
    output$anovaFDistributionPlot <- renderPlot({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      model <- lm(reformulate(
        sprintf("`%s`", input$explanatoryVariables),
        sprintf("`%s`", input$responseVariable)
      ), data = encodedData())
      
      anovaModel <- anova(model)
      ss_vals <- anovaModel[["Sum Sq"]]
      
      SSR    <- sum(ss_vals[-length(ss_vals)])
      SSE    <- ss_vals[length(ss_vals)]
      k      <- model$rank - 1
      n      <- nrow(encodedData())
      MSR    <- SSR / k
      MSE    <- SSE / (n - k - 1)
      
      df1    <- k
      df2    <- n - k - 1
      f_stat <- MSR / MSE
      p_val  <- pf(f_stat, df1, df2, lower.tail = FALSE)
      f_crit <- qf(0.95, df1, df2)
      x_max  <- f_crit * 3
      
      x <- seq(0, x_max, length.out = 1000)
      y <- df(x, df1, df2)
      
      plot_df <- data.frame(x = x, y = y)
      
      ggplot(plot_df, aes(x = x, y = y)) +
        
        # Main curve
        geom_line(lwd = 1) +
        
        # Rejection region shading (red)
        geom_area(
          data = subset(plot_df, x >= f_crit),
          aes(x = x, y = y),
          fill  = "red",
          alpha = 0.3
        ) +
        
        # P-value region shading (blue) - only if f_stat <= x_max
        {if(f_stat <= x_max)
          geom_area(
            data = subset(plot_df, x >= f_stat),
            aes(x = x, y = y),
            fill  = "blue",
            alpha = 0.3
          )
        } +
        
        # Critical value line
        geom_vline(
          xintercept = f_crit,
          colour     = "red",
          linewidth  = 0.8,
          linetype   = "dashed"
        ) +
        
        # F statistic line - only if within plot range
        {if(f_stat <= x_max)
          geom_vline(
            xintercept = f_stat,
            colour     = "blue",
            linewidth  = 0.8,
            linetype   = "dashed"
          )
        } +
        
        # Labels
        labs(
          title = "F Distribution",
          x     = "F",
          y     = "Density"
        ) +
        
        # Critical value annotation
        annotate("text",
                 x     = f_crit,
                 y     = max(y) * 0.2,
                 label = sprintf("F critical\n= %.4f", f_crit),
                 hjust = -0.1,
                 color = "red",
                 size  = 3.5) +
        
        # P-value annotation
        annotate("text",
                 x     = x_max * 0.6,
                 y     = max(y) * 0.9,
                 label = sprintf("p-value = %.4f", p_val),
                 color = "black",
                 size  = 4) +
        
        # F statistic annotation - only if within plot range
        {if(f_stat <= x_max)
          annotate("text",
                   x     = f_stat,
                   y     = max(y) * 0.4,
                   label = sprintf("F statistic\n= %.4f", f_stat),
                   hjust = -0.1,
                   color = "blue",
                   size  = 3.5)
        } +
        
        # Theme
        theme_classic() +
        theme(
          plot.title   = element_text(hjust = 0.5, face = "bold"),
          axis.title   = element_text(size = 12, face = "bold"),
          axis.text    = element_text(size = 10, face = "bold")
        ) +
        coord_cartesian(clip = "off")
    })
    
    
    
    output$linearModelEquations <- renderUI({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      withMathJax(
        div(
          id = "linear-model-equations",
          
          with(encodedData(), {
            
            model <- lm(reformulate(
              sprintf("`%s`", input$explanatoryVariables),
              sprintf("`%s`", input$responseVariable)
            ))
            
            all_coefs <- coef(model)
            names(all_coefs) <- gsub("^`|`$", "", names(all_coefs))
            
            intercept <- all_coefs["(Intercept)"]
            
            vars_definitions <- c()
            sym_terms <- c()
            num_terms <- c()
            
            for(i in seq_along(input$explanatoryVariables)) {
              
              var_name <- input$explanatoryVariables[i]
              
              val <- all_coefs[var_name]
              
              if (is.na(val)) {
                val <- all_coefs[paste0("`", var_name, "`")]
              }
              
              if(!is.na(val)) {
                
                # Escape LaTeX special characters
                safe_name <- var_name
                safe_name <- gsub("\\\\", "\\\\\\\\", safe_name)
                safe_name <- gsub("_", "\\\\_", safe_name)
                safe_name <- gsub("\\$", "\\\\\\$", safe_name)
                
                vars_definitions <- c(
                  vars_definitions,
                  sprintf("x_{%d} = \\text{%s}", i, safe_name)
                )
                
                sym_terms <- c(
                  sym_terms,
                  sprintf("\\hat{\\beta}_{%d}x_{%d}", i, i)
                )
                
                num_terms <- c(
                  num_terms,
                  sprintf("%.3fx_{%d}", val, i)
                )
              }
            }
            
            safe_response <- input$responseVariable
            safe_response <- gsub("\\\\", "\\\\\\\\", safe_response)
            safe_response <- gsub("_", "\\\\_", safe_response)
            safe_response <- gsub("\\$", "\\\\\\$", safe_response)
            
            vars_definition_latex <- paste0(
              "<div style='text-align:left;'>",
              "\\(",
              "y = \\text{", safe_response, "}",
              "\\)",
              "<br>",
              paste(
                paste0("\\(", vars_definitions, "\\)"),
                collapse = "<br>"
              ),
              "</div>"
            )
            
            sym_eq <- paste0(
              "<div style='text-align:left;'>",
              "\\(",
              "\\hat{y} = ",
              "\\hat{\\beta}_0 + ",
              paste(sym_terms, collapse = " + "),
              "\\)",
              "</div>"
            )
            
            num_eq <- paste0(
              "$$",
              "\\hat{y} = ",
              round(intercept, 3),
              " + ",
              paste(num_terms, collapse = " + "),
              "$$"
            )
            
            sym_eq <- gsub("\\+ -", "- ", sym_eq)
            num_eq <- gsub("\\+ -", "- ", num_eq)
            
            tagList(
              p("The variables in the model are"),
              HTML(vars_definition_latex),
              
              p("The estimated multiple linear regression equation is"),
              
              HTML(sym_eq),
              HTML(num_eq)
            )
          })
        )
      )
    })
    
    
    # Reactive ANOVA tab outputs
    output$anovaHypotheses <- renderUI({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
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
          #br(),
          p(r"{\( \alpha = 0.05\ \)}"),
          #br(),
          p(
            sprintf(r"{\( n = %i \)}", nrow(encodedData())),
            br(),
            sprintf(r"{\( k = %i \)}", k)
          ),
          p(r"[where \(n\) is the sample size and \(k\) is the number of explanatory variables in the multiple regression model.]")
        )
      })
    })
    
    output$anovaTable <- renderTable(
      {
        req(encodedData())
        req(isTruthy(input$responseVariable))
        req(isTruthy(input$explanatoryVariables))
        req(length(as.character(input$explanatoryVariables)) >= 2)
        
        redrawing <<- TRUE
        options(knitr.kable.NA = "") # do not display NAs
        
        with(encodedData(), {
          model <- lm(reformulate(
            sprintf("`%s`", input$explanatoryVariables),
            sprintf("`%s`", input$responseVariable)
          ))
          
          modelANOVA <- anova(model)
          SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
          SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)] # only the residuals
          SST <- SSR + SSE
          
          # Use model rank
          k <- model$rank - 1
          n <- nrow(encodedData())
          MSR <- SSR / k
          MSE <- SSE / (n - k - 1)
          F_stat <- MSR / MSE
          
          ## RETURN THE TABLE TO RENDER
          tibble::tribble(
            ~"Source", ~"df", ~"SS", ~"MS", ~"F", ~"P-value",
            "<strong>Regression (Model)</strong>", as.integer(k), SSR, MSR, F_stat, pf(F_stat, k, n - k - 1, lower.tail = FALSE),
            "<strong>Error (Residual)</strong>", as.integer(n - k - 1), SSE, MSE, NA, NA,
            "<strong>Total</strong>", as.integer(n - 1), SST, NA, NA, NA
          )
        })
      },
      na = "",
      striped = TRUE,
      align = "c",
      sanitize.text.function = function(x) x
    )
    
    output$anovaPValueMethod <- renderUI({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        anovaModel <- anova(model)
        
        SSR <- sum(anovaModel$"Sum Sq"[-nrow(anovaModel)]) # all but the residuals
        SSE <- anovaModel$"Sum Sq"[nrow(anovaModel)] # only the residuals
        
        # Use model rank
        k <- model$rank - 1
        n <- nrow(encodedData())
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
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        
        modelANOVA <- anova(model)
        SSR <- sum(modelANOVA$"Sum Sq"[-nrow(modelANOVA)]) # all but the residuals
        SSE <- modelANOVA$"Sum Sq"[nrow(modelANOVA)] # only the residuals
        SST <- SSR + SSE
        
        # Use model rank
        k <- model$rank - 1
        n <- nrow(encodedData())
        
        withMathJax(
          p(strong(r"{ \(R^2\) and Adjusted \(R^2\) :}")),
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
R^2_{\text{adj}} = 1 - \left[ \left( 1-R^2 \right) \frac{n-1}{n-k-1} \right] = %0.4f
\)
}",
            summary(model)$adj.r.squared
          )),
          p(
            strong("Interpretation:"),
            sprintf(
              r"[Roughly \(%.2f\%%\) of the variation in the response variable is explained by the multiple linear regression model when adjusted for the number of explanatory variables and the sample size.]",
              summary(model)$adj.r.squared * 100
            )
          ),
          br(),
          p(strong("Akaike Information Criteria (AIC):")),
          p(sprintf(r"[AIC = \(%0.4f\)]", AIC(model))),
          br(),
          p(strong("Bayesian Information Criteria (BIC):")),
          p(sprintf(r"[BIC = \(%0.4f\)]", BIC(model))),
          br(),
          p(strong("Mallows' Cp:")),
          p(sprintf(r"[Cp = \(%0.4f\)]", ols_mallows_cp(model, model))),
          br()
        )
      })
    })
    
    # Reactive Multicollinearity Detection tab outputs
    output$multicollinearityDetectionMainPanelUI <- renderUI({
      withMathJax(
        fluidRow(column(
          12, p(strong("Correlation matrix")),
          p("Correlation values below –0.75 or above 0.75 indicate strong linear association between explanatory variables. Such strong correlations may signal potential multicollinearity. Consider whether one of the correlated variables can be removed, combined, or otherwise addressed."),
          tableOutput(ns("simpleCorrelationMatrix"))
        )),
        fluidRow(column(
          12, p(strong("Graphical methods")),
          p("Along the diagonal of this plot are the distributions of the data in each variable. Below the diagonal are the scatterplots of one variable against another; if the points form a more-or-less straight line then the variables are correlated. Above the diagonal are the correlation coefficients between two variables."),
          
          div(
            style = "text-align:left;",
            plotOutput(ns("ggscatmat"), width = "550px", height = "550px")
          )
        )), 
        fluidRow(column(
          12, p(strong("Variance Inflation Factors (VIFs)")),
          p("A VIF greater than 10 suggests strong multicollinearity caused by the respective variable with that variance inflation factor. VIFs between 5 and 10 hint at moderate multicollinearity. Values less than 5 are acceptable, with only a low degree of multicollinearity detected."),
          tableOutput(ns("vifs"))
        ))
      )
    })
    
    output$simpleCorrelationMatrix <- renderTable({
      
      corsetAboveDiagNA(mlrPredictors())
      
    },
    rownames = TRUE,
    striped = TRUE,
    na = "",
    align = "c")
    
    output$vifs <- renderTable({
      
      req(input$responseVariable)
      
      clean_df <- cbind(
        encodedData()[input$responseVariable],
        mlrPredictors()
      )
      
      model <- lm(
        reformulate(
          sprintf("`%s`", colnames(mlrPredictors())),
          sprintf("`%s`", input$responseVariable)
        ),
        data = clean_df
      )
      
      as.data.frame(car::vif(model))
      
    },
    rownames = TRUE,
    align = "c")
    
    output$ggscatmat <- renderPlot({
      
      par(mar = c(4,4,1,1))
      
      ggscatmat(
        mlrPredictors(),
        columns = colnames(mlrPredictors())
      )
    })
    
    # Reactive Diagnostic Plots tab outputs
    output$mlrResidualsPanelPlot1 <- renderPlot({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        plot(model, which = 1, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    output$mlrResidualsPanelPlot2 <- renderPlot({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        plot(model, which = 2, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    output$mlrResidualsPanelPlot3 <- renderPlot({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        plot(model, which = 3, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    output$mlrResidualsPanelPlot4 <- renderPlot({
      req(encodedData())
      req(isTruthy(input$responseVariable))
      req(isTruthy(input$explanatoryVariables))
      req(length(as.character(input$explanatoryVariables)) >= 2)
      
      with(encodedData(), {
        model <- lm(reformulate(
          sprintf("`%s`", input$explanatoryVariables),
          sprintf("`%s`", input$responseVariable)
        ))
        plot(model, which = 5, pch = 20, main = "", lwd = 2, sub.caption = "")
      })
    })
    
    # Reactive Uploaded Data tab output
    output$uploadedDataTable <- renderDT({
      req(encodedData())
      datatable(encodedData(),
                options = list(pageLength = -1,
                               lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All"))))
    })

    
    output$MulticollinearityDetection <- renderUI({
      eval(MLRValidation)
      
      uiOutput(ns("multicollinearityDetectionMainPanelUI"))
    })
    
    # NEW MERGED ANOVA AND INFERENCE TAB 
    
    output$ANOVAAndInference <- renderUI({
      eval(MLRValidation)
      
      fluidPage(
        tags$style(HTML("
      .correlation-tabs .nav-tabs {
        border-bottom: none;
        background-color: #f8f9fa;
        display: flex;
        padding: 0;
        margin-bottom: 16px;
      }
      .correlation-tabs .nav-tabs > li > a {
        color: #18536F;
        font-weight: bold;
        font-size: 15px;
        border: none !important;
        border-radius: 0 !important;
        padding: 10px 24px;
        background-color: #f8f9fa !important;
      }
      .correlation-tabs .nav-tabs > li.active > a,
      .correlation-tabs .nav-tabs > li.active > a:focus,
      .correlation-tabs .nav-tabs > li.active > a:hover,
      .correlation-tabs .nav-tabs > li > a.active,
      .correlation-tabs .nav-tabs > li > a.active:focus,
      .correlation-tabs .nav-tabs > li > a.active:hover {
        background-color: #18536F !important;
        color: white !important;
        border: none !important;
        border-radius: 0 !important;
        font-weight: bold !important;
      }
      .correlation-tabs .nav-tabs > li > a:hover {
        background-color: #d0dce8 !important;
        color: #1a3a5c !important;
      }
    ")),
    div(
      class = "correlation-tabs",
      tabsetPanel(
        tabPanel(
          title = "Parameter Estimates",
          br(),
          fluidRow(uiOutput(ns("linearModelCoefficientsAndConfidenceIntervals"))),
          fluidRow(hr()),
          fluidRow(uiOutput(ns("bpTest"))),
          fluidRow(hr()),
          fluidRow(uiOutput(ns("whiteTest")))
        ),
        tabPanel(
          title = "ANOVA",
          br(),
          fluidRow(uiOutput(ns("anovaHypotheses"))),
          br(),
          fluidRow(tableOutput(ns("anovaTable"))),
          br(),
          fluidRow(
            column(12,
                   p(strong("F Distribution")),
                   p("The shaded region represents the rejection region at \u03b1 = 0.05. The dashed red line is the observed F statistic and the dashed blue line is the critical value."),
                   plotOutput(ns("anovaFDistributionPlot"), height = "350px")
            )
          ),
          br(),
          fluidRow(uiOutput(ns("anovaPValueMethod"))),
          br(),
          fluidRow(uiOutput(ns("rsquareAdjustedRSquareInterpretation"))),
          br()
        )
      )
    )
      )
    }) 
    
    output$DiagnosticPlots <- renderUI({
      eval(MLRValidation)
      
      fluidPage(
        fluidRow(
          column(12,
                 plotOutput(ns("mlrResidualsPanelPlot1")),
                 br(),
                 plotOutput(ns("mlrResidualsPanelPlot2")),
                 br(),
                 plotOutput(ns("mlrResidualsPanelPlot3")),
                 br(),
                 plotOutput(ns("mlrResidualsPanelPlot4")),
                 br()
          )
        )
      )
    })
  
    
    })
}



