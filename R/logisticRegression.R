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
      id = ns("LOGRSidebar"), # Using ns for the outer div ID as well
      # useShinyjs(), # Called in server or main UI (preferably once globally or per top-level module UI)
      withMathJax(
        helpText("Select a binary response variable (must have exactly two unique values)."),
        selectInput(
          ns("responseVariable"), # Consistent ID, ns-wrapped
          "Response Variable (\\(y\\)) – Binary",
          choices  = NULL,
          selectize = FALSE
        ),
        uiOutput(ns("responseVariableWarning")), # For warnings
        helpText("Select one or more explanatory variables (numeric or categorical)."),
        pickerInput(
          ns("explanatoryVariables"), # Consistent ID, ns-wrapped
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
        actionButton(ns("calculate"), "Calculate",    class = "act-btn"), # Consistent ID
        actionButton(ns("reset"),     "Reset Values", class = "act-btn")     # Consistent ID
      )
    )
  )
}

# --- UI Function for Logistic Regression Main Panel ---
LogisticRegressionMainPanelUI <- function(id) {
  ns <- NS(id)
  tagList( # Changed from navbarPage to tagList for better control over sections
    div(id = ns("logrImportSection"), # Section for data import
        import_file_ui(
          id    = ns("dataImportLogReg"), # Unique ID for this import module instance
          title = ""),
        uiOutput(ns("fileImportUserMessage"))
    ),
    shinyjs::hidden( # Results section is initially hidden
      div(id = ns("logrResultsSection"), 
          navbarPage(title = "Logistic Regression Analysis", # Title for the results navbar
                     id = ns("logrResultsTabs"), # ID for the results navbar itself
                     tabPanel(title = "Model Output", uiOutput(ns("ModelOutput"))),
                     tabPanel(title = "Coefficients & Odds Ratios", uiOutput(ns("Coefficients"))),
                     tabPanel(title = "Model Fit", uiOutput(ns("ModelFit")))
                     # Potentially add more tabs like Diagnostic Plots later
          )
      )
    )
  )
}

# --- Server logic for Logistic Regression ---
LogisticRegressionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs() # Call useShinyjs once in the server logic of the module
    
    imported <- import_file_server(
      id            = "dataImportLogReg", # Matches ID in import_file_ui
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class  = "tbl_df"
    )
    
    noFileCalculate <- reactiveVal(FALSE) # Renamed from noFileCalculate_logr for consistency
    logr_isNAVariables <- NULL # For NA checking
    
    storeNameIfAnyNA_logr <- function(var_name) {
      df_data <- imported$data() # Use the reactive data
      if (is.null(df_data) || !isTruthy(var_name) || !(var_name %in% names(df_data))) return(FALSE)
      if (anyNA(df_data[[var_name]])) {
        logr_isNAVariables <<- c(logr_isNAVariables, var_name)
        return(TRUE)
      }
      return(FALSE)
    }
    
    hideResults <- function() {
      shinyjs::hide(id = "logrResultsSection") # Target the correct div ID
      output$ModelOutput  <- renderUI(NULL)
      output$Coefficients <- renderUI(NULL)
      output$ModelFit     <- renderUI(NULL)
    }
    hideResults() # Call initially
    
    observeEvent(imported$data(), {
      df <- imported$data()
      if (!is.null(df) && ncol(df) > 0) {
        vars <- names(df)
        updateSelectInput(session, "responseVariable", choices = c("", vars), selected = "")
        updatePickerInput(session, "explanatoryVariables", choices = vars, selected = character(0))
      } else { # Clear if data is removed or empty
        updateSelectInput(session, "responseVariable", choices = c(""), selected = "")
        updatePickerInput(session, "explanatoryVariables", choices = c(""), selected = character(0))
      }
      output$responseVariableWarning <- renderUI(NULL) # Clear warning
      hideResults()
    }, ignoreNULL = FALSE) # ignoreNULL=FALSE to react to NULL data (e.g. file removed)
    
    observeEvent(input$responseVariable, {
      df <- imported$data()
      # req(df) # Ensure df is not NULL before proceeding
      
      response_var_name <- input$responseVariable # Already correct
      
      if (!is.null(df) && isTruthy(response_var_name) && response_var_name %in% names(df)) {
        vals <- na.omit(df[[response_var_name]])
        if (length(unique(vals)) != 2 && length(vals) > 0) { # Check only if there's data
          output$responseVariableWarning <- renderUI(
            tags$p(class = "text-danger", style="font-size:0.9em; padding-top:5px;", 
                   "Warning: Response variable must have exactly two unique values for logistic regression.")
          )
        } else {
          output$responseVariableWarning <- renderUI(NULL)
        }
        expls <- setdiff(names(df), response_var_name) # Correct
        current_expl <- isolate(input$explanatoryVariables)
        valid_current_expl <- current_expl[current_expl %in% expls]
        updatePickerInput(session, "explanatoryVariables", choices = expls, selected = valid_current_expl)
      } else {
        output$responseVariableWarning <- renderUI(NULL)
        if(!is.null(df)) { # If response is cleared but data exists
          updatePickerInput(session, "explanatoryVariables", choices = names(df), selected = isolate(input$explanatoryVariables))
        } else { # If no data
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
      
      logr_isNAVariables <<- NULL # Reset before validation
      eval(LOGRValidation_quote) # Validation will stop and show message if a need() fails
      
      df_orig <- imported$data()
      response_var_name <- input$responseVariable
      explanatory_vars_names <- input$explanatoryVariables
      
      selected_vars_for_model <- unique(c(response_var_name, explanatory_vars_names))
      df_model_data <- df_orig[, selected_vars_for_model, drop = FALSE]
      df_model_data_original_rows <- nrow(df_model_data) # For NA message
      df_model_data <- na.omit(df_model_data)
      rows_omitted <- df_model_data_original_rows - nrow(df_model_data)
      
      # Re-check rows after NA omission
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
      target_level <- levels(response_column_data)[2]
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
        output$ModelOutput <- renderUI({
          coefs <- coef(fit)
          intercept_val <- coefs[1]
          model_term_names <- names(coefs)[-1]
          
          log_odds_terms <- sapply(model_term_names, function(term_name) {
            beta_val <- coefs[term_name]
            op <- ifelse(beta_val >= 0, "+", "-")
            display_term_name <- gsub("`", "", term_name); display_term_name <- gsub("_", "\\\\_", display_term_name)
            sprintf("%s %.4f \\cdot \\text{(%s)}", op, abs(beta_val), display_term_name)
          })
          
          log_odds_equation <- paste0("\\text{log}\\left(\\frac{p}{1-p}\\right) = \\text{logit}(p) = ", sprintf("%.4f", intercept_val), paste(log_odds_terms, collapse = " "))
          prob_equation_logit_part <- paste0(sprintf("%.4f", intercept_val), paste(log_odds_terms, collapse = " "))
          prob_equation <- paste0("p = P(Y=\\text{",target_level,"} | X) = \\frac{e^{ (", prob_equation_logit_part, ") } }{1 + e^{ (", prob_equation_logit_part, ") } }")
          
          tagList(
            withMathJax(),
            if (rows_omitted > 0) {
              tags$p(class="text-info", paste(rows_omitted, "row(s) with missing values were omitted from the model fitting."))
            },
            tags$p(class="text-info", paste("Response variable '", response_var_name, "' was coded as: 0 for '", reference_level, "', 1 for '", target_level, "'.", sep="")),
            h4("Estimated Logistic Regression Equation"),
            p(strong("Log-Odds Form (Logit):")), p(HTML(katex::katex_html(log_odds_equation, displayMode = TRUE))),
            br(), p(strong("Probability Form:")), p(HTML(katex::katex_html(prob_equation, displayMode = TRUE))),
            br(), h4("Model Summary Statistics"), verbatimTextOutput(ns("summaryPrint"))
          )
        })
        output$summaryPrint <- renderPrint({ summary(fit) })
        
        output$Coefficients <- renderUI({
          tagList(h4("Coefficients, Odds Ratios, and Confidence Intervals"), DT::dataTableOutput(ns("coefTable")))
        })
        output$coefTable <- DT::renderDataTable({
          tidy_log_odds <- broom::tidy(fit, conf.int = TRUE, exponentiate = FALSE)
          tidy_odds_ratios <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
          
          coeffs_table <- tidy_log_odds %>%
            dplyr::left_join(
              tidy_odds_ratios %>% dplyr::select(term, estimate, conf.low, conf.high),
              by = "term", suffix = c("_log_odds", "_odds_ratio")
            ) %>%
            dplyr::transmute(
              Variable = term, `Coefficient (Log-Odds)` = estimate_log_odds,
              `Std. Error` = std.error, `z value` = statistic, `P-value` = p.value,
              `CI Low (Log-Odds)` = conf.low_log_odds, `CI High (Log-Odds)` = conf.high_log_odds,
              `Odds Ratio` = estimate_odds_ratio, `CI Low (Odds Ratio)` = conf.low_odds_ratio,
              `CI High (Odds Ratio)` = conf.high_odds_ratio
            ) %>%
            dplyr::select(Variable, `Coefficient (Log-Odds)`, `Std. Error`, `z value`, `P-value`, 
                          `CI Low (Log-Odds)`, `CI High (Log-Odds)`, 
                          `Odds Ratio`, `CI Low (Odds Ratio)`, `CI High (Odds Ratio)`)
          DT::datatable(coeffs_table, rownames = FALSE,
                        options = list(pageLength = nrow(coeffs_table), dom = 't', scrollX = TRUE, autoWidth = TRUE),
                        caption = htmltools::tags$caption(style = "caption-side: bottom; text-align: left;",
                                                          "Coefficients are on the log-odds scale. Odds Ratios are exponentiated coefficients and represent the multiplicative change in odds for a one-unit increase in the predictor (or for the level shown vs. the reference level for factors).")) %>%
            DT::formatSignif(columns = which(sapply(coeffs_table, is.numeric)), digits = 4)
        })
        
        output$ModelFit <- renderUI({
          tagList(h4("Model Fit Statistics"), verbatimTextOutput(ns("fitPrint")))
        })
        output$fitPrint <- renderPrint({
          cat("AIC:", AIC(fit), "\n"); cat("BIC:", BIC(fit), "\n\n")
          pseudo_r2s <- tryCatch({ DescTools::PseudoR2(fit, which = "all") }, error = function(e) { paste("Error calculating Pseudo R-squared:", e$message) })
          cat("Pseudo R-squared values:\n"); if (is.character(pseudo_r2s)) { print(pseudo_r2s) } else { print(pseudo_r2s) }; cat("\n")
          
          if (requireNamespace("ResourceSelection", quietly = TRUE)) {
            # Ensure 'y' in hoslem.test is the 0/1 numeric response
            y_for_hl <- df_model_data[[response_var_name]] # Already 0/1 numeric
            
            if (length(unique(fitted(fit))) >= 10 && nrow(df_model_data) > 10) {
              hl_test <- tryCatch({ ResourceSelection::hoslem.test(y_for_hl, fitted(fit), g = 10) }, 
                                  error = function(e) list(error_message = paste("Hosmer-Lemeshow test could not be computed:", e$message)))
              cat("Hosmer-Lemeshow Goodness of Fit Test:\n")
              if (!is.null(hl_test$error_message)) { print(hl_test$error_message) } else { print(hl_test); cat(sprintf(" (A non-significant p-value (e.g., > 0.05) suggests the model fits the data well.)\n")) }
            } else { cat("Hosmer-Lemeshow test not computed: Not enough distinct predicted probabilities or observations for 10 groups.\n") }
          } else { cat("ResourceSelection package not available for Hosmer-Lemeshow test (install if needed).\n") }
        })
      }) 
      
      shinyjs::show(id = "logrResultsSection")
      updateNavbarPage(session, inputId = ns("logrResultsTabs"), selected = "Model Output")
    }) 
  }) 
}