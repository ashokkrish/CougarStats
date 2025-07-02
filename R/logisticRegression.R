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
library(DescTools)
library(ResourceSelection)
library(shinyalert)
library(tibble)

# --- UI Function for Logistic Regression Sidebar ---
LogisticRegressionSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("LOGRSidebar"),
      withMathJax(
        helpText("Select a binary response variable (must have exactly two unique values: 0 or 1)."),
        pickerInput(
          ns("responseVariable"),
          "Response Variable (\\(y\\)) – Binary",
          choices = NULL,
          multiple = FALSE, # This ensures only one selection is allowed
          options = list(
            `live-search` = TRUE,
            title = "Nothing selected"
          )
        ),
        uiOutput(ns("responseVariableWarning")),
        helpText("Select one or more explanatory variables (numeric)."),
        pickerInput(
          ns("explanatoryVariables"),
          "Explanatory Variables (x₁, x₂, …, xₙ)",
          choices  = NULL,
          multiple = TRUE,
          options = list(
            `actions-box`       = TRUE,
            `live-search`       = TRUE,
            title = "Nothing selected",
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
  navbarPage(title = NULL,
             tabPanel(
               title = "Data Import",
               div (id = ns("importContainer")),
               uiOutput(ns("fileImportUserMessage")),
               import_file_ui(
                 id = ns("dataImport"),
                 title = "")),
             tabPanel(title = "LR", uiOutput(ns("Equations"))),
             id = ns("mainPanel"),
             theme = bs_theme(version = 4)
  )
}

# --- Server logic for Logistic Regression ---
LogisticRegressionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs()
    
    imported <- import_file_server(
      id            = "dataImport",
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class  = "tbl_df"
    )
    
    noFileCalculate <- reactiveVal(FALSE)
    
    bindEvent(observe({
      shinyjs::hide("mainPanel")
      updateNavbarPage(session, "mainPanel", selected = "Data Import")
    }),
    input$responseVariable,
    input$explanatoryVariables)
    
    observeEvent(imported$data(), {
      df <- imported$data()
      if (!is.null(df) && ncol(df) > 0) {
        vars <- names(df)
        updatePickerInput(session, "responseVariable", choices = vars, selected = character(0))
        updatePickerInput(session, "explanatoryVariables", choices = vars, selected = character(0))
      } else {
        updatePickerInput(session, "responseVariable", choices = c(""), selected = character(0))
        updatePickerInput(session, "explanatoryVariables", choices = c(""), selected = character(0))
      }
      output$responseVariableWarning <- renderUI(NULL)
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
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$reset, {
      updatePickerInput(session, "responseVariable", selected = character(0))
      updatePickerInput(session, "explanatoryVariables", selected = character(0))
      output$responseVariableWarning <- renderUI(NULL)
      noFileCalculate(FALSE)
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
        return()
      } else {
        noFileCalculate(FALSE)
      }
      
      df_orig <- imported$data()
      response_var_name <- input$responseVariable
      explanatory_vars_names <- isolate(input$explanatoryVariables)
      
      if (!isTruthy(response_var_name) || !isTruthy(explanatory_vars_names) || length(explanatory_vars_names) == 0) {
        shinyalert::shinyalert("Input Error", "Please select a response variable and at least one explanatory variable.", type = "error", confirmButtonCol = "#18536F")
        return()
      }
      
      selected_vars_for_model <- unique(c(response_var_name, explanatory_vars_names))
      df_model_data <- df_orig[, selected_vars_for_model, drop = FALSE]
      df_model_data <- na.omit(df_model_data)
      
      if(nrow(df_model_data) < (length(explanatory_vars_names) + 2) || nrow(df_model_data) == 0) {
        shinyalert::shinyalert("Error", "Not enough observations after removing NAs for model fitting, or no data selected.", type = "error", confirmButtonCol = "#18536F")
        return()
      }
      
      response_column_data <- df_model_data[[response_var_name]]
      if (!is.factor(response_column_data)) { response_column_data <- as.factor(response_column_data) }
      
      if (nlevels(response_column_data) != 2) {
        shinyalert::shinyalert("Error", "Response variable must resolve to exactly two distinct values after NA removal for logistic regression.", type = "error", confirmButtonCol = "#18536F")
        return()
      }
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
        shinyalert::shinyalert("Model Fitting Error", paste("Error in glm:", e$message), type = "error", confirmButtonCol = "#18536F")
        return(NULL)
      })
      
      if (!isTruthy(fit)) { return() }
      
      isolate({
        
        # --- 1. Render the Equations UI ---
        output$logisticModelEquations <- renderUI(withMathJax({
          variable_list <- paste(
            r"{\(}",
            sprintf(r"[y    = \text{%s} \\]", response_var_name),
            paste(
              sprintf(
                r"[x_{%d} = \text{%s}]",
                seq_along(explanatory_vars_names),
                explanatory_vars_names
              ),
              collapse = r"[\\]"
            ),
            r"{\)}"
          )
          
          symbolic_terms <- paste(
            sprintf("\\hat{\\beta}_{%d}x_{%d}", 1:length(explanatory_vars_names), 1:length(explanatory_vars_names)),
            collapse = " + "
          )
          log_odds_general <- paste0("\\text{logit}(\\hat{p}) = \\hat{\\beta}_0 + ", symbolic_terms)
          
          model_coeffs <- coefficients(fit)
          explanatory_coeffs <- model_coeffs[-1]
          
          value_terms <- paste(
            sprintf("%+.3f x_{%d}", explanatory_coeffs, seq_along(explanatory_coeffs)),
            collapse = " "
          )
          
          log_odds_specific <- gsub(
            "\\+ -", "- ",
            sprintf("\\text{logit}(\\hat{p}) = %.3f %s", model_coeffs[1], value_terms)
          )
          
          combined_latex <- sprintf("\\(%s \\\\ %s\\)", log_odds_general, log_odds_specific)
          
          div(
            p("The variables in the model are"),
            p(variable_list),
            p("The estimated binary logistic regression equation is"),
            p(combined_latex)
          )
        }))
        
        # --- 2. Render the Coefficients and CIs Table with requested columns ---
        output$logrCoefConfintTable <- renderTable({
          
          summary_coeffs <- as.data.frame(summary(fit)$coefficients)
          
          or_and_ci <- exp(cbind(OR = coef(fit), confint(fit)))
          colnames(or_and_ci) <- c("OR", "Lower_CI_OR", "Upper_CI_OR")
          or_and_ci <- as.data.frame(or_and_ci)
          
          final_table <- summary_coeffs %>%
            mutate(
              Wald = (Estimate / `Std. Error`)^2 ,
              df = 1
            ) %>%
            rownames_to_column("Term") %>%
            left_join(rownames_to_column(or_and_ci, "Term"), by = "Term") %>%
            dplyr::select(
              Term,
              Coefficient = Estimate,
              SE = `Std. Error`,
              Wald,
              df,
              P = `Pr(>|z|)`,
              OR,
              `Lower 95% CI for OR` = Lower_CI_OR,
              `Upper 95% CI for OR` = Upper_CI_OR
            )
          
          # Convert the 'Term' column back to row names for display
          tibble::column_to_rownames(final_table, var = "Term")
          
        }, rownames = TRUE, striped = TRUE, na = "", align = "c", digits = 3)
        
        output$logisticModelCoefficientsAndConfidenceIntervals <- renderUI({
          column(
            12,
            p(strong("Coefficients and Confidence Intervals")),
            p("Coefficients are the log-odds. The Wald statistic tests the hypothesis that a given coefficient is zero. Odds Ratios (OR) and their 95% confidence intervals are also provided."),
            tableOutput(ns("logrCoefConfintTable"))
          )
        })
        
        # --- 3. Render the Main "Equations" Tab UI (parent container) ---
        output$Equations <- renderUI({
          validate(
            need(imported$name(), "Upload some data."),
            need(isTruthy(input$responseVariable), "A response variable is required."),
            need(isTruthy(input$explanatoryVariables), "Explanatory variables are required.")
          )
          fluidPage(
            fluidRow(uiOutput(ns("logisticModelEquations"))),
            fluidRow(uiOutput(ns("logisticModelCoefficientsAndConfidenceIntervals")))
          )
        })
        
      }) # End isolate
      
      shinyjs::show("mainPanel")
      updateNavbarPage(session, "mainPanel", selected = "LR")
    })
  })
}