# R/decisionTrees.R

# With a response of 3 or more classes, rpart tries every way of splitting a
# categorical predictor's categories into two groups, which grows as
# 2^(categories - 1): about 0.4 s at 26 categories, over 40 s at 33, and no end
# in sight for an ID-like column. Predictors above this limit are refused.
cart_max_categories <- 25

# Upper limit on the number of response classes (see the check in Calculate).
cart_max_classes <- 100

# rpart finds its model-frame columns again from the printed form of the term
# labels, which only gives back the column name when deparse() writes it
# unchanged between backticks. A name with a backtick, a backslash or a control
# character cannot be used that way, and neither can ..1 or ... (R reads them as
# arguments). Calculate fits those columns under placeholder names and puts the
# real names back into the fit afterwards (cart_relabel).
cart_name_ok <- function(x) {
  !grepl("[`\\\\[:cntrl:]]", x) & !grepl("^\\.\\.(\\.|[0-9]+)$", x)
}

# Renames the variables of an rpart fit from `from` to `to`, everywhere the
# tree, its plot and its importance read them.
cart_relabel <- function(fit, from, to) {
  rename <- function(x) {
    i <- match(x, from)
    ifelse(is.na(i), x, to[i])
  }
  rename_classes <- function(terms) {
    classes <- attr(terms, "dataClasses")
    if (!is.null(classes)) names(attr(terms, "dataClasses")) <- rename(names(classes))
    terms
  }

  var <- fit$frame$var
  if (is.factor(var)) levels(var) <- rename(levels(var)) else var <- rename(var)
  fit$frame$var <- var
  if (!is.null(fit$splits))
    rownames(fit$splits) <- rename(rownames(fit$splits))
  if (!is.null(fit$variable.importance))
    names(fit$variable.importance) <- rename(names(fit$variable.importance))
  if (!is.null(fit$ordered))
    names(fit$ordered) <- rename(names(fit$ordered))
  if (length(attr(fit, "xlevels")) > 0)
    names(attr(fit, "xlevels")) <- rename(names(attr(fit, "xlevels")))
  if (!is.null(fit$model)) {
    model_terms <- rename_classes(attr(fit$model, "terms"))
    names(fit$model) <- rename(names(fit$model))
    attr(fit$model, "terms") <- model_terms
  }
  fit$terms <- rename_classes(fit$terms)
  fit
}

# Bar labels and left margin (in lines of text) for a horizontal Variable
# Importance bar chart about to be drawn on the current device, with `panels`
# charts side by side (also used by randomForest.R and xgboost.R). The margin
# grows with the longest name, as it always has. Only when that would leave less
# than two lines of width for the bars (at worst the plot fails with "figure
# margins too large") is the margin half the panel instead, with the longer names
# shortened in the middle ("Average_mo...in_2024").
ml_importance_labels <- function(labels, panels = 1) {
  labels    <- as.character(labels)
  max_chars <- max(nchar(labels), na.rm = TRUE)
  left_mar  <- max(4, ceiling(max_chars * 0.6))

  panel_lines <- par("din")[1] / panels / (par("csi") * par("mex"))
  if (left_mar + 2 + 2 > panel_lines) {
    left_mar <- max(4, floor(panel_lines / 2))
    fit      <- max(5, floor(left_mar / 0.6))
    long     <- !is.na(labels) & nchar(labels) > fit
    head_n   <- ceiling((fit - 3) / 2)
    tail_n   <- fit - 3 - head_n
    labels[long] <- paste0(substr(labels[long], 1, head_n), "...",
                           substring(labels[long], nchar(labels[long]) - tail_n + 1))
  }

  list(labels = labels, left_mar = left_mar)
}

# Hides the named tabs of a navbarPage as soon as the page is parsed, so they
# never flash into view when the module is first opened. showTab() reveals
# them again (it clears this inline style), hideTab() hides them again.
cartHideTabsOnLoad <- function(tabsetId, values) {
  tags$script(HTML(sprintf(
    "(function() {
       var nav = document.getElementById('%s');
       if (!nav) return;
       [%s].forEach(function(v) {
         var a = nav.querySelector('a[data-value=\"' + v + '\"]');
         if (a && a.parentNode) a.parentNode.style.display = 'none';
       });
     })();",
    tabsetId,
    paste0("'", values, "'", collapse = ", ")
  )))
}

CARTSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    numericInput(
      ns("max_depth"),
      strong("Maximum Tree Depth"),
      value = 10,
      min = 1
    ),

    numericInput(
      ns("min_split"),
      strong("Minimum Split Size"),
      value = 20,
      min = 1
    ),

    numericInput(
      ns("cp"),
      strong("Complexity Parameter"),
      value = 0.01,
      min = 0.01,
      step = 0.01
    ),
    
    div(
      style = "font-size: 15px; color: #6c757d; margin-top: 8px; margin-bottom: 6px; ",
      "Select a categorical variable, must have 2 or more unique categories."
    ),
    
    div(
      id = ns("responseWrapper"),
      pickerInput(
        ns("response"),
        strong(HTML("Response Variable (Class)")),
        choices = NULL,
        multiple = TRUE,
        options = list(`live-search` = TRUE, title = "Nothing selected", `max-options` = 1)
      ),
      uiOutput(ns("responseError")),
      uiOutput(ns("responseContinuousWarning"))
    ),

    div(
      id = ns("predictorsWrapper"),
      pickerInput(
        ns("predictors"),
        strong(HTML("Explanatory Variables (<em>x</em><sub>1</sub>, <em>x</em><sub>2</sub>, ..., <em>x</em><sub>k</sub>)")),
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Nothing selected")
      ),
      uiOutput(ns("predictorsError"))
    ),
    
    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

CARTMainPanelUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    suppressWarnings(tippy::use_tippy()),
    navbarPage(
      title = NULL,
      
      tabPanel(
        title = "Results",
        value = "results_tab",
        uiOutput(ns("resultsContainer"))
      ),
      
      tabPanel(
        title = "Plots",
        value = "plots_tab",
        uiOutput(ns("plotsContainer"))
      ),
      
      tabPanel(
        title = "Uploaded Data",
        value = "uploaded_data_tab",
        uiOutput(ns("uploadedDataContainer"))
      ),
      
      id = ns("cartMainPanel"),
      selected = "uploaded_data_tab",
      theme = bs_theme(version = 4)
    ),
    cartHideTabsOnLoad(ns("cartMainPanel"), c("results_tab", "plots_tab"))
  )
}

CARTServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    results_ready <- reactiveVal(FALSE)
    plots_ready <- reactiveVal(FALSE)
    
    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)
    
    calc_results <- reactiveVal(NULL)
    plot_results <- reactiveVal(NULL)
    cart_message <- reactiveVal(NULL)
    
    noFileCalculate <- reactiveVal(FALSE)
    responseError <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    responseContinuous <- reactiveVal(FALSE)

    cart_iv <- shinyvalidate::InputValidator$new()
    cart_iv$add_rule("max_depth", shinyvalidate::sv_required())
    cart_iv$add_rule("max_depth", shinyvalidate::sv_gte(1, message = "Must be at least 1."))
    cart_iv$add_rule("min_split", shinyvalidate::sv_required())
    cart_iv$add_rule("min_split", shinyvalidate::sv_gte(1, message = "Must be at least 1."))
    cart_iv$add_rule("cp", shinyvalidate::sv_required())
    cart_iv$add_rule("cp", shinyvalidate::sv_gt(0, message = "Must be greater than 0."))
    cart_iv$enable()
    
    # The Results and Plots tabs start hidden (cartHideTabsOnLoad in the UI),
    # so nothing needs hiding here when the module server starts.
    
    # Selections this module pushes into its own pickers after an upload. The
    # change event the client echoes back for them is not a user edit and must
    # not overwrite the selections shared with the other methods.
    pushed <- new.env(parent = emptyenv())
    is_echo <- function(key, value) {
      expected <- pushed[[key]]
      pushed[[key]] <- NULL
      !is.null(expected) &&
        identical(sort(as.character(value)), sort(as.character(expected)))
    }
    # The browser only sends a change event when a picker's value really
    # changes, so expect an echo only when the pushed value differs from the
    # current one. Otherwise the marker would linger and swallow the user's next
    # identical selection (e.g. Deselect All, then the same variables again).
    expect_echo <- function(key, value) {
      current <- isolate(input[[key]])
      same <- identical(sort(as.character(value)), sort(as.character(current)))
      pushed[[key]] <- if (same) NULL else value
    }
    
    # Uploaded Data tab
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DTOutput(session$ns("cartUploadTable"))
      }
    })
    
    output$cartUploadTable <- renderDT({
      req(data())
      
      datatable(
        data(),
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX = TRUE
        )
      )
    })
    
    # Populate response/predictor choices after upload
    observeEvent(data(), {
      noFileCalculate(FALSE)
      req(data())
      
      df <- data()
      cols <- colnames(df)
      
      shared_resp    <- shared_response()

      n_rows         <- nrow(df)
      # vapply (not sapply) so a table with no columns gives logical(0), not list()
      valid_response <- cols[vapply(cols, function(col) {
        n_uniq <- length(unique(na.omit(df[[col]])))
        n_uniq >= 2 && n_uniq <= floor(n_rows / 2)
      }, logical(1))]
      pre_response   <- if (length(shared_resp) == 1 && isTruthy(shared_resp) && shared_resp %in% valid_response) shared_resp else character(0)

      # The response is never offered as a predictor (the response observer
      # below does not run when a new file keeps the same response selected)
      predictor_cols <- setdiff(cols, pre_response)
      pre_predictors <- intersect(shared_explanatory(), predictor_cols)

      expect_echo("response",   pre_response)
      expect_echo("predictors", pre_predictors)
      updatePickerInput(session, "response",   choices = valid_response, selected = pre_response)
      updatePickerInput(session, "predictors", choices = predictor_cols, selected = pre_predictors)

      results_ready(FALSE)
      plots_ready(FALSE)
      calc_results(NULL)
      plot_results(NULL)
      cart_message(NULL)
    })
    
    # Keep response out of predictors (also when the response is cleared, so
    # its column becomes selectable as a predictor again)
    observeEvent(input$response, {
      req(data())
      
      df <- data()
      cols <- colnames(df)
      
      available_predictors <- setdiff(cols, input$response)
      selected_predictors <- intersect(input$predictors, available_predictors)
      
      updatePickerInput(
        session,
        "predictors",
        choices = available_predictors,
        selected = selected_predictors
      )
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # Clear outputs if settings change after calculate
    observeEvent(
      list(
        data(),
        input$response,
        input$predictors,
        input$max_depth,
        input$min_split,
        input$cp
      ),
      {
        if (isTRUE(results_ready())) {
          results_ready(FALSE)
          calc_results(NULL)
        }
        
        if (isTRUE(plots_ready())) {
          plots_ready(FALSE)
          plot_results(NULL)
        }
        
        hideTab(inputId = "cartMainPanel", target = "results_tab")
        hideTab(inputId = "cartMainPanel", target = "plots_tab")
        updateNavbarPage(session, "cartMainPanel", selected = "uploaded_data_tab")
      },
      ignoreInit = TRUE
    )
    
    # Results tab container
    output$resultsContainer <- renderUI({
      if (!isTRUE(results_ready())) {
        
        if (!isTRUE(results_ever_calculated())) {
          return(tagList(
            helpText("No results yet. Upload a dataset, choose variables, then click Calculate.")
          ))
        }
        
        return(tagList(
          helpText("Settings changed. Click Calculate to update results.")
        ))
      }
      
      uiOutput(session$ns("resultsUI"))
    })
    
    # Plots tab container
    output$plotsContainer <- renderUI({
      if (!isTRUE(plots_ready())) {
        
        if (!isTRUE(plots_ever_calculated())) {
          return(tagList(
            helpText("No plots yet. Upload a dataset, choose variables, then click Calculate.")
          ))
        }
        
        return(tagList(
          helpText("Settings changed. Click Calculate to update plots.")
        ))
      }
      
      tagList(
        tags$h4("Decision Tree Diagram"),
        plotOutput(session$ns("treePlot"), height = "550px"),
        
        tags$h4("Variable Importance"),
        plotOutput(session$ns("varImportancePlot"), height = "450px")
      )
    })
    
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(
          class = "shiny-output-error-validation",
          "Required: Cannot calculate without a data file."
        )
      } else {
        msg <- cart_message()
        
        if (is.null(msg)) return(NULL)
        
        div(
          style = "margin-top:10px;",
          div(
            class = "alert alert-danger",
            msg
          )
        )
      }
    })
    
    output$responseError <- renderUI({
      if (responseError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Please select a response variable."
        )
      }
    })
    
    output$predictorsError <- renderUI({
      if (predictorsError()) {
        tags$div(
          class = "text-danger",
          style = "font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          icon("exclamation-circle"),
          "Please select at least one explanatory variable."
        )
      }
    })
    
    observeEvent(input$response, {
      if (!is_echo("response", input$response)) shared_response(input$response)
      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      if (isTruthy(data()) && isTruthy(input$response)) {
        responseContinuous(ml_is_continuous_response(data()[[input$response[1]]]))
      } else {
        responseContinuous(FALSE)
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    output$responseContinuousWarning <- renderUI({
      if (responseContinuous()) {
        div(
          class = "alert alert-warning",
          style = "font-size: 12px; margin-top: 4px; margin-bottom: 10px;",
          icon("triangle-exclamation"),
          ml_continuous_response_message
        )
      }
    })

    observeEvent(input$predictors, {
      if (!is_echo("predictors", input$predictors)) shared_explanatory(input$predictors)
      if (length(input$predictors) >= 1) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ---- Results outputs ----
    # Defined once here, reading only from calc_results()/plot_results(), so
    # nothing holds on to the data copies of an earlier Calculate and the
    # settings shown are the ones the model was fitted with.
    output$resultsUI <- renderUI({
      r <- calc_results()
      req(r)
      
      correct <- sum(diag(r$confusion))
      total   <- sum(r$confusion)

      tagList(
        tags$h4("Model Summary"),
        tableOutput(session$ns("cartModelInfo")),
        tags$hr(),

        tags$h4("Class Distribution (Full Dataset)"),
        tableOutput(session$ns("cartClassDist")),
        tags$hr(),

        tags$h4("Classification Report"),
        tableOutput(session$ns("cartClassReport")),
        tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),
        tags$hr(),

        tags$h4("Confusion Matrix"),
        tableOutput(session$ns("confusionMatrixResults")),
        tags$h5(tags$strong("Accuracy Calculation"),
                style = "margin-top: 14px; margin-bottom: 2px;"),
        withMathJax(
          tags$p(HTML(sprintf(
            "\\( \\text{Accuracy} = \\dfrac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\dfrac{%d}{%d} = %.2f\\%% \\)",
            correct, total, r$accuracy * 100
          )))
        )
      )
    })
    
    output$cartModelInfo <- renderTable({
      r <- calc_results()
      req(r)

      data.frame(
        Item = c(
          "Number of Classes",
          "Number of Predictors",
          "Number of Complete Cases",
          "Maximum Tree Depth",
          "Minimum Split Size",
          "Complexity Parameter",
          "Accuracy"
        ),
        Value = c(
          as.character(r$n_classes),
          as.character(length(r$predictors)),
          as.character(r$n),
          as.character(r$max_depth),
          as.character(r$min_split),
          as.character(r$cp),
          as.character(round(r$accuracy, 4))
        ),
        check.names = FALSE
      )
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$cartClassDist <- renderTable({
      r <- calc_results()
      req(r)
      r$class_dist
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$cartClassReport <- renderTable({
      r <- calc_results()
      req(r)
      r$class_report
    }, rownames = FALSE, striped = TRUE, bordered = TRUE,
       sanitize.colnames.function = function(x) {
         tips <- c(
           Precision = "Of all instances predicted as this class, the fraction that are truly this class. High precision means few false positives.",
           Recall    = "Of all actual instances of this class, the fraction correctly predicted. High recall means few false negatives.",
           F1        = "Harmonic mean of Precision and Recall — balances both into a single score.",
           Support   = "Number of actual instances of this class in the dataset."
         )
         sapply(x, function(col) {
           if (col %in% names(tips)) {
             paste0('<b><span data-tippy-content="', tips[[col]],
                    '" style="cursor:help;border-bottom:1px dotted #555;">', col, '</span></b>')
           } else {
             paste0("<b>", col, "</b>")
           }
         }, USE.NAMES = FALSE)
       })

    output$confusionMatrixResults <- renderTable({
      r <- calc_results()
      req(r)

      cm <- as.data.frame.matrix(r$confusion)
      cm$Actual <- paste0("<b>", rownames(cm), "</b>")
      cm <- cm[, c("Actual", setdiff(names(cm), "Actual"))]
      rownames(cm) <- NULL
      cm
    }, rownames = FALSE, striped = TRUE, bordered = TRUE,
       sanitize.text.function = identity,
       sanitize.colnames.function = function(x) {
         sapply(x, function(col) {
           if (col == "Actual") "<b>Actual \\ Predicted</b>" else paste0("<b>", col, "</b>")
         }, USE.NAMES = FALSE)
       })
    
    output$treePlot <- renderPlot({
      r <- plot_results()
      req(r)
      
      par(mar = c(1, 1, 3, 1))
      
      rpart.plot::rpart.plot(
        r$fit,
        main = "Decision Tree Diagram",
        extra = 104,
        fallen.leaves = TRUE,
        tweak = 1.05,
        under = TRUE,
        faclen = 0,
        varlen = 0,
        shadow.col = 0,
        box.palette = "Blues",
        col        = "black",
        split.col  = "black",
        under.col  = "black",
        branch.col = "black",
        border.col = "black"
      )
    }, res = 96)
    
    output$varImportancePlot <- renderPlot({
      r <- plot_results()
      req(r)

      if (nrow(r$importance) == 0) {
        plot.new()
        text(0.5, 0.5, "No variable importance available for this model.",
             cex = 0.9, col = "#555555")
        return()
      }

      imp_df    <- r$importance[order(r$importance$ImportancePct, decreasing = FALSE), ]
      lab       <- ml_importance_labels(imp_df$Variable)

      par(mar = c(5, lab$left_mar, 4, 2))

      barplot(
        imp_df$ImportancePct,
        names.arg = lab$labels,
        horiz     = TRUE,
        las       = 1,
        col       = "#18536F",
        main      = "Variable Importance",
        xlab      = "Importance (%)",
        cex.main  = 1.3,
        font.main = 2,
        cex.lab   = 1.1,
        font.lab  = 2,
        cex.names = 0.95
      )
    })

    cart_calculate <- function() {
      cart_message(NULL)
      
      if (!isTruthy(data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }
      
      # The response is never also used as a predictor
      predictors <- setdiff(input$predictors, input$response)

      # input validation
      if (!isTruthy(input$response)) {
        responseError(TRUE)
        shinyjs::addClass(id = "responseWrapper", class = "has-error")
      } else {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }
      
      if (length(predictors) < 1) {
        predictorsError(TRUE)
        shinyjs::addClass(id = "predictorsWrapper", class = "has-error")
      } else {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
      
      req(cart_iv$is_valid())

      if (!isTruthy(input$response) ||
          length(predictors) < 1) {
        return()
      }
      
      resp_col <- input$response[1]
      df <- data()

      # Selections can be stale for an instant after a new upload
      missing_cols <- setdiff(c(predictors, resp_col), colnames(df))
      if (length(missing_cols) > 0) {
        cart_message(paste0(
          "The following selected variable(s) are not in the current dataset: ",
          paste(missing_cols, collapse = ", "),
          ". Please reselect your variables."
        ))
        return()
      }

      # Continuous response guard — block classification on a continuous variable
      if (ml_is_continuous_response(df[[resp_col]])) {
        responseContinuous(TRUE)
        return()
      }

      analysis_df <- df[, c(predictors, resp_col), drop = FALSE]

      na_cols <- names(which(sapply(analysis_df, function(x) any(is.na(x)))))
      if (length(na_cols) > 0) {
        cart_message(paste0(
          "The following column(s) contain missing values (NA): ",
          paste(na_cols, collapse = ", "),
          ". Please remove or impute missing values before calculating."
        ))
        return()
      }

      analysis_df <- na.omit(analysis_df)

      if (nrow(analysis_df) == 0) {
        showNotification("No complete cases remain after removing missing values.", type = "error", duration = 8)
        return()
      }

      analysis_df[[resp_col]] <- as.factor(analysis_df[[resp_col]])

      if (nlevels(analysis_df[[resp_col]]) < 2) {
        showNotification("Response variable must have at least 2 classes.", type = "error", duration = 8)
        return()
      }

      if (nlevels(analysis_df[[resp_col]]) > floor(nrow(analysis_df) / 2)) {
        showNotification(
          "The selected response variable has too many unique values to be treated as a categorical class variable for CART.",
          type = "error",
          duration = 8
        )
        return()
      }

      # rpart's work and memory grow with rows x classes (20,000 rows and 10,000
      # classes took over a minute and about 1 GB), so unbounded class counts
      # could freeze or crash the shared R process.
      if (nlevels(analysis_df[[resp_col]]) > cart_max_classes) {
        showNotification(
          paste0("The selected response variable has more than ", cart_max_classes,
                 " classes, which is too many for CART. Please choose a response with fewer classes."),
          type = "error",
          duration = 8
        )
        return()
      }
      
      predictor_df      <- analysis_df[, predictors, drop = FALSE]
      numeric_predictors <- names(predictor_df)[sapply(predictor_df, is.numeric)]

      if (length(numeric_predictors) > 0) {
        sds <- sapply(predictor_df[, numeric_predictors, drop = FALSE], sd, na.rm = TRUE)
        zero_var_cols <- names(sds)[is.na(sds) | sds == 0]
        if (length(zero_var_cols) > 0) {
          cart_message(paste0(
            "These selected variable(s) have zero variance and cannot be used in CART: ",
            paste(zero_var_cols, collapse = ", "), "."
          ))
          return()
        }
      }

      # A categorical predictor with very many categories (an ID or free-text
      # column) makes rpart's split search for a 3+ class response effectively
      # endless (see cart_max_categories). With 2 classes rpart orders the
      # categories instead, which is fast, so that case is left alone.
      if (nlevels(analysis_df[[resp_col]]) > 2) {
        n_categories <- vapply(predictor_df, function(x) {
          if (is.factor(x) || is.character(x)) length(unique(x)) else 0L
        }, integer(1))
        many_category_cols <- names(n_categories)[n_categories > cart_max_categories]
        if (length(many_category_cols) > 0) {
          cart_message(paste0(
            "These selected categorical variable(s) have more than ", cart_max_categories,
            " categories, which is too many to search for splits when the response has 3 or more classes: ",
            paste(many_category_cols, collapse = ", "),
            ". Please deselect them or group their categories."
          ))
          return()
        }
      }
      
      # The formula is built from symbols, so any column name works in it. Names
      # rpart could not look up again (see cart_name_ok) are fitted under
      # placeholders; the columns are the predictors followed by the response.
      fit_df    <- analysis_df
      fit_preds <- predictors
      fit_resp  <- resp_col
      placeholders <- !all(cart_name_ok(c(fit_preds, fit_resp)))
      if (placeholders) {
        fit_preds <- paste0("x", seq_along(predictors))
        fit_resp  <- "y"
        names(fit_df) <- c(fit_preds, fit_resp)
      }
      cart_formula <- as.formula(
        call("~", as.name(fit_resp),
             Reduce(function(a, b) call("+", a, b), lapply(fit_preds, as.name))),
        env = baseenv()
      )
      
      cart_message(NULL)
      
      # xval = 0: rpart's default 10-fold cross-validation refits the tree 10
      # more times to fill the cp table's xerror column, which is never used.
      # model = TRUE keeps the model frame with the fit, which rpart.plot needs
      # to label split values of integer predictors (it would otherwise look
      # for the data in the plot's environment, where it no longer is).
      cart_fit <- tryCatch(
        rpart::rpart(
          formula = cart_formula,
          data = fit_df,
          method = "class",
          model = TRUE,
          control = rpart::rpart.control(
            maxdepth = as.integer(input$max_depth),
            minsplit = as.integer(input$min_split),
            cp = as.numeric(input$cp),
            xval = 0
          )
        ),
        error = function(e) {
          showNotification(
            paste("Decision tree could not be computed:", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      )
      
      req(cart_fit)
      
      cart_pred <- tryCatch(
        predict(cart_fit, fit_df[, fit_preds, drop = FALSE], type = "class"),
        error = function(e) {
          showNotification(
            paste("Predictions could not be generated:", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      )
      
      req(cart_pred)
      
      if (placeholders) {
        cart_fit <- cart_relabel(cart_fit, c(fit_preds, fit_resp), c(predictors, resp_col))
      }
      
      confusion_mat <- table(
        Actual = analysis_df[[resp_col]],
        Predicted = cart_pred
      )
      
      accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
      
      class_dist_df     <- as.data.frame(table(analysis_df[[resp_col]]))
      colnames(class_dist_df) <- c("Class", "Count")

      cart_class_report <- knn_classification_report(analysis_df[[resp_col]], cart_pred)$report

      importance <- cart_fit$variable.importance
      if (is.null(importance)) {
        importance <- numeric(0)
      }
      
      importance_df <- data.frame(
        Variable = names(importance),
        Importance = as.numeric(importance),
        stringsAsFactors = FALSE
      )
      
      if (nrow(importance_df) > 0) {
        importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), , drop = FALSE]
        importance_df$ImportancePct <- (importance_df$Importance / sum(importance_df$Importance)) * 100
      }
      
      res <- list(
        fit = cart_fit,
        confusion = confusion_mat,
        accuracy = accuracy,
        response = resp_col,
        predictors = predictors,
        n = nrow(analysis_df),
        n_classes = length(unique(analysis_df[[resp_col]])),
        max_depth = as.integer(input$max_depth),
        min_split = as.integer(input$min_split),
        cp = input$cp,
        importance = importance_df,
        class_dist = class_dist_df,
        class_report = cart_class_report
      )
      
      calc_results(res)
      plot_results(res)
      
      results_ready(TRUE)
      plots_ready(TRUE)
      results_ever_calculated(TRUE)
      plots_ever_calculated(TRUE)
      
      showTab(inputId = "cartMainPanel", target = "results_tab")
      showTab(inputId = "cartMainPanel", target = "plots_tab")
      
      shinyjs::delay(100, {
        updateNavbarPage(session, "cartMainPanel", selected = "results_tab")
      })
    }

    observeEvent(input$calculate, {
      # Backstop: an uncaught error inside an observer ends the whole session,
      # so anything the checks above missed is reported instead.
      tryCatch(
        cart_calculate(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)   # req()/validate(): stay silent
          showNotification(
            paste("Decision tree could not be computed:", conditionMessage(e)),
            type = "error",
            duration = 8
          )
        }
      )
    }, ignoreInit = TRUE)
    
    observeEvent(input$reset, {
      hideTab(inputId = "cartMainPanel", target = "results_tab")
      hideTab(inputId = "cartMainPanel", target = "plots_tab")
      
      results_ready(FALSE)
      plots_ready(FALSE)
      calc_results(NULL)
      plot_results(NULL)
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)
      
      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      responseContinuous(FALSE)
      cart_message(NULL)

      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      
      updateNumericInput(session, "max_depth", value = 10)
      updateNumericInput(session, "min_split", value = 20)
      updateNumericInput(session, "cp", value = 0.01)
      
      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      
      updateNavbarPage(session, "cartMainPanel", selected = "uploaded_data_tab")
    })
  })
}