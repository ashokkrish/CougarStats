# CougarStats-Darren/R/logisticRegression.R

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(datamods)
library(magrittr)
library(ggplot2)
library(dplyr)
library(broom)
library(broom.helpers)
library(DT)
library(DescTools)           # for PseudoR2()
library(ResourceSelection)   # for hoslem.test()
library(shinyalert)          # for pop-up notices

# --- UI Function for Logistic Regression Sidebar ---
LogisticRegressionSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("LOGRSidebar"), 
      withMathJax(
        helpText("Select a binary response variable (must have exactly two unique values)."),
        selectInput(
          ns("responseVariable"), 
          "Response Variable (\\(y\\)) – Binary",
          choices  = NULL,
          selectize = FALSE
        ),
        uiOutput(ns("responseVariableWarning")), 
        helpText("Select one or more explanatory variables (numeric or categorical)."),
        pickerInput(
          ns("explanatoryVariables"), 
          "Explanatory Variables (x₁, x₂, …, xₙ)",
          choices  = NULL,
          multiple = TRUE,
          options = list(
            `actions-box`       = TRUE,
            `live-search`       = TRUE,
            selectedTextFormat = "values",
            multipleSeperator  = ", "
          )
        ),
        actionButton(ns("calculate"), "Calculate",    class = "act-btn"), 
        actionButton(ns("reset"),     "Reset Values", class = "act-btn")     
      )
    )
  )
}

# --- UI Function for Logistic Regression Main Panel ---
LogisticRegressionMainPanelUI <- function(id) {
  ns <- NS(id)
  tagList( 
    div(id = ns("logrImportSection"), 
        import_file_ui(
          id    = ns("dataImportLogReg"), 
          title = ""),
        uiOutput(ns("fileImportUserMessage"))
    ),
    shinyjs::hidden( 
      div(id = ns("logrResultsSection"),
          navbarPage(title = "", 
                     id = ns("logrResultsTabs"), 
                     tabPanel(title = "Logistic Regression", uiOutput(ns("LogOutput"))),
                     tabPanel(title = "ANOVA", uiOutput(ns("ANOVA"))),
                     tabPanel(title = "Diagnostic Plots", uiOutput(ns("DiagPlot")))
          )
      )
    )
  )
}

# --- Server logic for Logistic Regression ---
LogisticRegressionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs() 
    
    imported <- import_file_server(
      id            = "dataImportLogReg", 
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class  = "tbl_df"
    )
    
    noFileCalculate <- reactiveVal(FALSE) 
    logr_isNAVariables <- NULL 
    
    storeNameIfAnyNA_logr <- function(var_name) {
      df_data <- imported$data() 
      if (is.null(df_data) || !isTruthy(var_name) || !(var_name %in% names(df_data))) return(FALSE)
      if (anyNA(df_data[[var_name]])) {
        logr_isNAVariables <<- c(logr_isNAVariables, var_name)
        return(TRUE)
      }
      return(FALSE)
    }
    
    hideResults <- function() {
      shinyjs::hide(id = "logrResultsSection") 
      output$LogOutput  <- renderUI(NULL)
      output$ANOVA <- renderUI(NULL)
      output$DiagPlot     <- renderUI(NULL)
    }
    hideResults() 
    
    observeEvent(imported$data(), {
      df <- imported$data()
      if (!is.null(df) && ncol(df) > 0) {
        vars <- names(df)
        updateSelectInput(session, "responseVariable", choices = c("", vars), selected = "")
        updatePickerInput(session, "explanatoryVariables", choices = vars, selected = character(0))
      } else { 
        updateSelectInput(session, "responseVariable", choices = c(""), selected = "")
        updatePickerInput(session, "explanatoryVariables", choices = c(""), selected = character(0))
      }
      output$responseVariableWarning <- renderUI(NULL) 
      hideResults()
    }, ignoreNULL = FALSE) 
    
    observeEvent(input$responseVariable, {
      df <- imported$data()
      response_var_name <- input$responseVariable 
      
      if (!is.null(df) && isTruthy(response_var_name) && response_var_name %in% names(df)) {
        vals <- na.omit(df[[response_var_name]])
        if (length(unique(vals)) != 2 && length(vals) > 0) { 
          output$responseVariableWarning <- renderUI(
            tags$p(class = "text-danger", style="font-size:0.9em; padding-top:5px;",
                   "Warning: Response variable must have exactly two unique values for logistic regression.")
          )
        } else {
          output$responseVariableWarning <- renderUI(NULL)
        }
        expls <- setdiff(names(df), response_var_name) 
        current_expl <- isolate(input$explanatoryVariables)
        valid_current_expl <- current_expl[current_expl %in% expls]
        updatePickerInput(session, "explanatoryVariables", choices = expls, selected = valid_current_expl)
      } else {
        output$responseVariableWarning <- renderUI(NULL)
        if(!is.null(df)) { 
          updatePickerInput(session, "explanatoryVariables", choices = names(df), selected = isolate(input$explanatoryVariables))
        } else { 
          updatePickerInput(session, "explanatoryVariables", choices = c(""), selected = character(0))
        }
      }
      hideResults()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$explanatoryVariables, {
      hideResults()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$reset, {
      updateSelectInput(session, "responseVariable", selected = "")
      updatePickerInput(session, "explanatoryVariables", selected = character(0))
      output$responseVariableWarning <- renderUI(NULL)
      noFileCalculate(FALSE)
      hideResults()
    })
    
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(class = "shiny-output-error-validation",
                 "Required: Cannot calculate without a data file.")
      } else { NULL }
    })
    
    observeEvent(input$calculate, {
      if (!isTruthy(imported$data())) {
        noFileCalculate(TRUE)
        hideResults()
        return()
      } else {
        noFileCalculate(FALSE)
      }
      
      df_orig <- imported$data()
      response_var_name <- input$responseVariable
      explanatory_vars_names <- isolate(input$explanatoryVariables) # Use isolate if not already done
      
      if (!isTruthy(response_var_name) || !isTruthy(explanatory_vars_names) || length(explanatory_vars_names) == 0) {
        shinyalert::shinyalert("Input Error", "Please select a response variable and at least one explanatory variable.", type = "error", confirmButtonColor = "#18536F")
        hideResults(); return()
      }
      
      selected_vars_for_model <- unique(c(response_var_name, explanatory_vars_names))
      df_model_data <- df_orig[, selected_vars_for_model, drop = FALSE]
      df_model_data_original_rows <- nrow(df_model_data) 
      df_model_data <- na.omit(df_model_data)
      rows_omitted <- df_model_data_original_rows - nrow(df_model_data)
      
      if(nrow(df_model_data) < (length(explanatory_vars_names) + 2) || nrow(df_model_data) == 0) {
        shinyalert::shinyalert("Error", "Not enough observations after removing NAs for model fitting, or no data selected.", type = "error", confirmButtonColor = "#18536F")
        hideResults(); return()
      }
      
      response_column_data <- df_model_data[[response_var_name]]
      if (!is.factor(response_column_data)) { response_column_data <- as.factor(response_column_data) }
      
      if (nlevels(response_column_data) != 2) {
        shinyalert::shinyalert("Error", "Response variable must resolve to exactly two distinct values after NA removal for logistic regression.", type = "error", confirmButtonColor = "#18536F")
        hideResults(); return()
      }
      reference_level <- levels(response_column_data)[1]
      target_level <- levels(response_column_data)[2] # Defined here for use in equations
      df_model_data[[response_var_name]] <- as.numeric(response_column_data) - 1
      
      for (col_name in explanatory_vars_names) {
        if (is.character(df_model_data[[col_name]])) {
          df_model_data[[col_name]] <- as.factor(df_model_data[[col_name]])
        }
      }
      
      formula_str <- paste0("`", response_var_name, "` ~ ", paste0("`", explanatory_vars_names, "`", collapse = " + "))
      
      fit <- tryCatch({
        glm(as.formula(formula_str), data = df_model_data, family = binomial(link = "logit"))
      }, error = function(e) {
        shinyalert::shinyalert("Model Fitting Error", paste("Error in glm:", e$message), type = "error", confirmButtonColor = "#18536F")
        return(NULL)
      })
      
      if (!isTruthy(fit)) { hideResults(); return() }
      
      isolate({
        output$LogOutput <- renderUI({
          # Construct symbolic terms for the general formula
          symbolic_linear_predictor_terms <- ""
          if (length(explanatory_vars_names) > 0) {
            term_parts <- sapply(seq_along(explanatory_vars_names), function(i) {
              var_name_clean <- gsub("`", "", explanatory_vars_names[i]) 
              var_name_latex <- gsub("_", "\\\\_", var_name_clean)      
              paste0("\\beta_{", i, "} X_{", i, "}") # Using generic X_i for simplicity in formula
              # Or, to use actual variable names:
              # paste0("\\beta_{", i, "} \\cdot \\text{(", var_name_latex, ")}")
            })
            symbolic_linear_predictor_terms <- paste0(" + ", paste(term_parts, collapse = " + "))
          }
          
          log_odds_equation_formula <- paste0("\\text{logit}(p) = \\beta_0", symbolic_linear_predictor_terms)
          prob_equation_logit_part_formula <- paste0("\\beta_0", symbolic_linear_predictor_terms)
          
          # General Probability Form using Sigmoid notation
          prob_equation_formula <- paste0("p = P(Y=\\text{",target_level,"} | X_1, ..., X_k) = \\frac{1}{1 + e^{-(", prob_equation_logit_part_formula, ")}}")
          
          # Explanatory variable legend for the general formula
          explanatory_legend <- ""
          if (length(explanatory_vars_names) > 0) {
            legend_items <- sapply(seq_along(explanatory_vars_names), function(i) {
              var_name_clean <- gsub("`", "", explanatory_vars_names[i])
              var_name_latex <- gsub("_", "\\\\_", var_name_clean)
              paste0("X_{", i, "} = \\text{", var_name_latex, "}")
            })
            explanatory_legend <- paste0("Where: ", paste(legend_items, collapse = ", "))
          }
          
          
          tagList(
            withMathJax(),
            if (rows_omitted > 0) {
              tags$p(class="text-info", paste(rows_omitted, "row(s) with missing values were omitted from the model fitting."))
            },
            tags$p(class="text-info", paste("Response variable '", response_var_name, "' was coded as: 0 for '", reference_level, "', 1 for '", target_level, "'.", sep="")),
            
            h4("General Logistic Regression Equation"),
            p(strong("Log-Odds Form (Logit):")), 
            p(HTML(katex::katex_html(log_odds_equation_formula, displayMode = TRUE))),
            br(), 
            p(strong("Probability Form (Sigmoid):")), 
            p(HTML(katex::katex_html(prob_equation_formula, displayMode = TRUE))),
            if (nchar(explanatory_legend) > 0) {
              p(HTML(katex::katex_html(explanatory_legend, displayMode = FALSE)))
            },
            br(),
          )
        })
        
        
        output$ANOVA <- renderUI({
        })
        
        
        output$DiagPlot <- renderUI({
        })
        
      }) # End isolate
      
      shinyjs::show(id = "logrResultsSection")
      updateNavbarPage(session, inputId = ns("logrResultsTabs"), selected = "Logistic Regression") 
    })
  })
}