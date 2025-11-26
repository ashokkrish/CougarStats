# CougarStats-Darren/R/logisticRegression.R

# Source the plot options menu
source("R/plotOptionsMenu.R")

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
          strong("Response Variable (\\(y\\))"),
          choices = NULL,
          multiple = FALSE, # This ensures only one selection is allowed
          options = list(
            `live-search` = TRUE,
            title = "Nothing selected"
          )
        ),
        uiOutput(ns("responseVariableWarning")),
        helpText("Select one or more explanatory variables."),
        pickerInput(
          ns("explanatoryVariables"),
          strong("Explanatory Variables (x₁, x₂, …, xₙ)"),
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
  tagList(
    useShinyjs(),
    navbarPage(title = NULL,
               tabPanel(
                 title = "Data Import",
                 value = "data_import_tab",
                 div (id = ns("importContainer")),
                 uiOutput(ns("fileImportUserMessage")),
                 import_file_ui(
                   id = ns("dataImport"),
                   title = "")),
               tabPanel(title = "Model",
                        uiOutput(ns("Equations"))
               ),
               tabPanel(title = "Analysis of Deviance",
                        uiOutput(ns("anovaOutput"))
               ),
               tabPanel(title = "Diagnostic Plot",
                        value = "diagnostic_plot_tab",
                        h3("Logistic Regression Plot"),
                        plotOptionsMenuUI(ns("logrPlotOptions"), "Scatterplot", xlab = "x", ylab = "y"),
                        plotOutput(ns("logrScatterplot"))
               ),
               tabPanel(title = "Uploaded Data",
                        DTOutput(ns("uploadedDataTable"))
               ),
               id = ns("mainPanel"),
               theme = bs_theme(version = 4)
    )
  )
}

# --- Server logic for Logistic Regression ---
LogisticRegressionServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call the plot options module server
    plotOptionsMenuServer("logrPlotOptions")
    
    imported <- import_file_server(
      id            = "dataImport",
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class  = "tbl_df"
    )
    
    noFileCalculate <- reactiveVal(FALSE)
    calculation_done <- reactiveVal(FALSE)
    
    observeEvent(input$explanatoryVariables, {
      if (!isTruthy(input$explanatoryVariables)) {
        output$anovaOutput <- renderUI({})
      }
    }, ignoreNULL = FALSE)
    
    perform_logr_analysis <- function(silent_validation = FALSE) {
      if (!isTruthy(imported$data())) {
        if (!silent_validation) shinyalert::shinyalert("Input Error", "Please upload a data file.", type = "error", confirmButtonCol = "#18536F")
        return(NULL)
      }
      
      df_orig <- imported$data()
      response_var_name <- input$responseVariable
      explanatory_vars_names <- input$explanatoryVariables
      
      if (!isTruthy(response_var_name) || !isTruthy(explanatory_vars_names) || length(explanatory_vars_names) == 0) {
        if (!silent_validation) shinyalert::shinyalert("Input Error", "Please select a response variable and at least one explanatory variable.", type = "error", confirmButtonCol = "#18536F")
        return(NULL)
      }
      
      selected_vars_for_model <- unique(c(response_var_name, explanatory_vars_names))
      df_model_data <- df_orig[, selected_vars_for_model, drop = FALSE]
      df_model_data <- na.omit(df_model_data)
      
      if(nrow(df_model_data) < (length(explanatory_vars_names) + 2) || nrow(df_model_data) == 0) {
        if (!silent_validation) shinyalert::shinyalert("Error", "Not enough observations after removing NAs for model fitting, or no data selected.", type = "error", confirmButtonCol = "#18536F")
        return(NULL)
      }
      
      response_column_data <- df_model_data[[response_var_name]]
      if (!is.factor(response_column_data)) { response_column_data <- as.factor(response_column_data) }
      
      if (nlevels(response_column_data) != 2) {
        if (!silent_validation) shinyalert::shinyalert("Error", "Response variable must resolve to exactly two distinct values after NA removal for logistic regression.", type = "error", confirmButtonCol = "#18536F")
        return(NULL)
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
        if (!silent_validation) shinyalert::shinyalert("Model Fitting Error", paste("Error in glm:", e$message), type = "error", confirmButtonCol = "#18536F")
        return(NULL)
      })
      
      if (!isTruthy(fit)) { return(NULL) }
      
      return(list(fit = fit, data = df_model_data, response = response_var_name, explanatory = explanatory_vars_names))
    }
    
    observeEvent(input$calculate, {
      results <- perform_logr_analysis(silent_validation = FALSE)
      if (!is.null(results)) {
        render_analysis_results(results)
        calculation_done(TRUE)
      }
    })
    
    observe({
      req(calculation_done())
      results <- perform_logr_analysis(silent_validation = TRUE)
      if (!is.null(results)) {
        render_analysis_results(results)
      } else {
        # Clear outputs if inputs become invalid reactively
        output$Equations <- renderUI({})
        output$anovaOutput <- renderUI({})
      }
    }) |> bindEvent(input$responseVariable, input$explanatoryVariables, ignoreInit = TRUE)
    
    render_analysis_results <- function(results) {
      fit <- results$fit
      df_model_data <- results$data
      response_var_name <- results$response
      explanatory_vars_names <- results$explanatory
      
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
            `Wald 𝑍` = Estimate / `Std. Error`,
            `Wald 𝜒²` = (Estimate / `Std. Error`)^2,
            df = 1
          ) %>%
          rownames_to_column("Term") %>%
          left_join(rownames_to_column(or_and_ci, "Term"), by = "Term") %>%
          dplyr::select(
            Term,
            Coefficient = Estimate,
            SE = `Std. Error`,
            `Wald 𝑍`,
            `Wald 𝜒²`,
            df,
            `P-value` = `Pr(>|z|)`,
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
        tagList(
          h3("Calculations and Formulas"),
          uiOutput(ns("logisticModelEquations")),
          uiOutput(ns("logisticModelCoefficientsAndConfidenceIntervals"))
        )
      })
      
      # --- ANOVA-style table for Logistic Regression ---
      # Likelihood Ratio Test
      anova_table <- anova(fit, test = "LRT")
      
      output$anovaOutput <- renderUI({
        fluidPage(
          h3("Analysis of Deviance Table (Likelihood Ratio Test)"),
          withMathJax(),
          h4("Calculations and Formulas"),
          p("The deviance is a measure of the goodness of fit of a logistic regression model. A smaller deviance indicates a better fit. The deviance for a model is calculated as:"),
          p("$$ D = -2 \\log(\\mathcal{L}) $$"),
          p("where \\(\\mathcal{L}\\) is the likelihood of the model."),
          p("The Likelihood Ratio Test (LRT) statistic is used to compare nested models. It is the difference in deviance between the two models:"),
          p("$$ \\text{LRT} = D_0 - D_1 = -2 \\log\\left(\\frac{\\mathcal{L}_0}{\\mathcal{L}_1}\\right) $$"),
          p("where \\(D_0\\) and \\(\\mathcal{L}_0\\) are the deviance and likelihood of the null model (the simpler model), and \\(D_1\\) and \\(\\mathcal{L}_1\\) are for the alternative model. This LRT statistic follows a \\(\\chi^2\\) distribution with degrees of freedom equal to the difference in the number of parameters between the two models."),
          p("The residual deviance is the deviance of the model with all predictors included:"),
          p("$$ D_{residual} = -2 \\sum_{i=1}^{n} [y_i \\log(\\hat{p}_i) + (1 - y_i) \\log(1 - \\hat{p}_i)] $$"),
          p("The residual degrees of freedom is the number of observations minus the number of parameters in the model:"),
          p("$$ df_{residual} = n - (k + 1) $$"),
          p("where \\(n\\) is the number of observations and \\(k\\) is the number of explanatory variables."),
          br(),
          DTOutput(ns("lrAnovaTable")),
          br(),
          h4("Interpretation"),
          p("The 'Df' column shows the degrees of freedom for each term. The 'Deviance' column shows the change in deviance when the term is added to the model. The 'p-value' column gives the p-value for the likelihood ratio test. A small p-value (typically < 0.05) indicates that the variable is statistically significant and contributes to the model's explanatory power.")
        )
      })
      
      # Display the table using DT package for better formatting
      output$lrAnovaTable <- renderDT({
        # Rename the p-value column
        anova_df <- as.data.frame(anova_table)
        colnames(anova_df)[colnames(anova_df) == "Pr(>Chi)"] <- "p-value"
        
        datatable(
          anova_df,
          options = list(
            dom = 't', # Display table only
            searching = FALSE,
            paging = FALSE,
            ordering = FALSE
          ),
          caption = 'Analysis of Deviance Table (Likelihood Ratio Test)',
          rownames = TRUE
        ) %>% formatRound(columns = c('Deviance', 'p-value', 'Resid. Dev'), digits = 3)
      })
      
      # Scatterplot with logistic curve
      output$logrScatterplot <- renderPlot({
        req(df_model_data, input$responseVariable, input$explanatoryVariables)
        
        ggplot(df_model_data, aes(x = .data[[explanatory_vars_names[1]]], y = .data[[response_var_name]])) +
          geom_point(alpha = 0.5, color = input[["logrPlotOptions-PointsColour"]]) +
          geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = input[["logrPlotOptions-Colour"]], linewidth = input[["logrPlotOptions-LineWidth"]]) +
          labs(
            title = input[["logrPlotOptions-Title"]],
            x = "x",
            y = "y"
          ) +
          theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5),
                axis.title.y = element_text(size = 16, face = "bold"),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y.left = element_text(size = 12),
                panel.background = element_rect(fill = "white", colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      })
      
      output$uploadedDataTable <- renderDT({
        req(imported$data())
        datatable(imported$data(),
                  options = list(pageLength = -1,
                                 lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All"))))
      })
      
      showTab(inputId = "mainPanel", target = "Model")
      showTab(inputId = "mainPanel", target = "Analysis of Deviance")
      showTab(inputId = "mainPanel", target = "Uploaded Data")
      
      if (length(explanatory_vars_names) == 1) {
        showTab(inputId = "mainPanel", target = "diagnostic_plot_tab")
      } else {
        hideTab(inputId = "mainPanel", target = "diagnostic_plot_tab")
      }
      
      updateNavbarPage(session, "mainPanel", selected = "Model")
    }
    
    observeEvent(TRUE, {
      shinyjs::delay(0, {
        hideTab(inputId = "mainPanel", target = "Model")
        hideTab(inputId = "mainPanel", target = "Analysis of Deviance")
        hideTab(inputId = "mainPanel", target = "diagnostic_plot_tab")
        hideTab(inputId = "mainPanel", target = "Uploaded Data")
      })
    }, once = TRUE)
    
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
                   "Warning: Response variable must have exactly two unique values for binary logistic regression.")
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
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$reset, {
      hideTab(inputId = "mainPanel", target = "Model")
      hideTab(inputId = "mainPanel", target = "Analysis of Deviance")
      hideTab(inputId = "mainPanel", target = "diagnostic_plot_tab")
      hideTab(inputId = "mainPanel", target = "Uploaded Data")
      
      calculation_done(FALSE)
      
      updatePickerInput(session, "responseVariable", selected = character(0))
      updatePickerInput(session, "explanatoryVariables", selected = character(0))
      
      output$responseVariableWarning <- renderUI(NULL)
      noFileCalculate(FALSE)
      
      updateNavbarPage(session, "mainPanel", selected = "data_import_tab")
    })
    
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(class = "shiny-output-error-validation",
                 "Required: Cannot calculate without a data file.")
      } else { NULL }
    })
  })
}