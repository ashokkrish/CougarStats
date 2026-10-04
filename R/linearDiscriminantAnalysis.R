# R/linearDiscriminantAnalysis.R

LDASidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    div(
      style = "font-size: 15px; color: #6c757d; margin-top: 8px; margin-bottom: 6px; ",
      "Select a categorical variable, must have 2 or more unique categories."
    ),

    div(
      id = ns("responseWrapper"),
      pickerInput(
        ns("response"),
        strong("Response Variable (Class)"),
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
    
    checkboxInput(
      ns("useCV"),
      "Use Leave-One-Out Cross-Validation",
      value = FALSE
    ),
    
    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

LDAMainPanelUI <- function(id) {
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

      id = ns("ldaMainPanel"),
      selected = "uploaded_data_tab",
      theme = bs_theme(version = 4)
    ),

    # Results and Plots only appear after a successful Calculate. Their initial
    # hidden state lives in the markup, so they are never visible before the
    # module server has started (the server also hides them, as before).
    tags$script(HTML(sprintf(
      "$(document.getElementById('%s')).find('a[data-value=\"results_tab\"], a[data-value=\"plots_tab\"]').parent().hide();",
      ns("ldaMainPanel")
    )))
  )
}

LDAServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    prepare_lda_response <- function(x) {
      x_no_na <- x[!is.na(x)]
      
      if (length(x_no_na) == 0) {
        return(NULL)
      }

      if (is.factor(x) || is.character(x) || is.logical(x)) {
        return(as.factor(x))
      }

      if (is.numeric(x) || is.integer(x)) {
        unique_vals <- unique(x_no_na)
        
        if (length(unique_vals) < 2) {
          return(as.factor(x))
        }
        
        return(factor(x, levels = sort(unique_vals)))
      }
      as.factor(x)
    }

    # Names of the numeric columns. vapply (not sapply) so a table with no
    # columns gives character(0) instead of a list that cannot be used to subset.
    numeric_columns <- function(df) {
      names(df)[vapply(df, is.numeric, logical(1))]
    }

    results_ready <- reactiveVal(FALSE)
    plots_ready <- reactiveVal(FALSE)

    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)

    calc_results <- reactiveVal(NULL)
    plot_results <- reactiveVal(NULL)
    lda_message <- reactiveVal(NULL)
    
    noFileCalculate <- reactiveVal(FALSE)
    responseError <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    responseContinuous <- reactiveVal(FALSE)

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

    # Called directly (not deferred via shinyjs::delay/session$onFlushed) so
    # it applies immediately: the module is only ever created once the
    # client has already bound this tab's markup, so the tabs being hidden
    # already exist in the DOM by this point. (They also start hidden in
    # LDAMainPanelUI, so this is only a backstop.)
    hideTab(inputId = "ldaMainPanel", target = "results_tab")
    hideTab(inputId = "ldaMainPanel", target = "plots_tab")

    # Uploaded Data tab
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else if (ncol(data()) == 0) {
        tagList(
          helpText("The uploaded file has no columns, so there is no data to show.")
        )
      } else {
        DTOutput(session$ns("ldaUploadTable"))
      }
    })

    output$ldaUploadTable <- renderDT({
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

    # Populate response/predictor choices
    observeEvent(data(), {
      noFileCalculate(FALSE)
      req(data())

      df <- data()

      # An empty upload gives a table with no columns: nothing can be chosen,
      # so clear the choices and say why.
      if (ncol(df) == 0) {
        updatePickerInput(session, "response",   choices = character(0), selected = character(0))
        updatePickerInput(session, "predictors", choices = character(0), selected = character(0))

        results_ready(FALSE)
        plots_ready(FALSE)
        calc_results(NULL)
        plot_results(NULL)
        lda_message("The uploaded file has no columns. Please upload a file with a header row and data.")
        return()
      }

      cols <- colnames(df)
      numeric_cols <- numeric_columns(df)

      shared_resp    <- shared_response()

      n_rows         <- nrow(df)
      valid_response <- cols[vapply(cols, function(col) {
        n_uniq <- length(unique(na.omit(df[[col]])))
        n_uniq >= 2 && n_uniq <= floor(n_rows / 2)
      }, logical(1))]
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% valid_response) shared_resp else character(0)

      # The response is never offered as a predictor (the response observer
      # below does not run when a new file keeps the same response selected)
      predictor_cols <- setdiff(numeric_cols, pre_response)
      pre_predictors <- intersect(shared_explanatory(), predictor_cols)

      expect_echo("response",   pre_response)
      expect_echo("predictors", pre_predictors)
      updatePickerInput(session, "response",   choices = valid_response, selected = pre_response)
      updatePickerInput(session, "predictors", choices = predictor_cols, selected = pre_predictors)

      results_ready(FALSE)
      plots_ready(FALSE)
      calc_results(NULL)
      plot_results(NULL)
      lda_message(NULL)
    })
    
    observeEvent(input$response, {
      req(data())
      
      df <- data()
      numeric_cols <- numeric_columns(df)

      available_predictors <- setdiff(numeric_cols, input$response)
      selected_predictors <- intersect(input$predictors, available_predictors)
      
      updatePickerInput(
        session,
        "predictors",
        choices = available_predictors,
        selected = selected_predictors
      )
    }, ignoreInit = TRUE, ignoreNULL = FALSE)   # also when cleared: its column is a predictor choice again

    # Clear outputs if settings change after calculate
    observeEvent(
      list(
        data(),
        input$response,
        input$predictors,
        input$useCV
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
        
        hideTab(inputId = "ldaMainPanel", target = "results_tab")
        hideTab(inputId = "ldaMainPanel", target = "plots_tab")

        # The active tab may be one of the two just hidden; move to a tab
        # that is still shown.
        updateNavbarPage(session, "ldaMainPanel", selected = "uploaded_data_tab")
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
        tags$h4("LDA Plot"),
        plotOutput(session$ns("ldaPlot"), height = "500px")
      )
    })
    
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(
          class = "shiny-output-error-validation",
          "Required: Cannot calculate without a data file."
        )
      } else {
        msg <- lda_message()
        
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

    # Also runs when the response is cleared, so the continuous-response warning
    # goes away (a cleared response is not written to the shared selection)
    observeEvent(input$response, {
      if (!is_echo("response", input$response) && !is.null(input$response)) shared_response(input$response)
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

    observeEvent(input$predictors, {
      if (!is_echo("predictors", input$predictors)) shared_explanatory(input$predictors)
      if (length(input$predictors) >= 1) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    })
    
    # Results outputs. Defined once here rather than inside the Calculate
    # observer; each waits for calc_results()/plot_results(), and the
    # containers above only put them on screen once a calculation exists.
    output$resultsUI <- renderUI({
      r <- calc_results()
      req(r)

      conf_used <- if (isTRUE(r$use_cv) && !is.null(r$cv_confusion)) r$cv_confusion else r$confusion
      acc_used  <- if (isTRUE(r$use_cv) && !is.null(r$cv_accuracy))  r$cv_accuracy  else r$accuracy
      correct   <- sum(diag(conf_used))
      total     <- sum(conf_used)

      tagList(
        tags$h4("Model Summary"),
        tableOutput(session$ns("ldaModelInfo")),
        tags$hr(),

        tags$h4("Class Distribution (Full Dataset)"),
        tableOutput(session$ns("ldaClassDist")),
        tags$hr(),

        tags$h4("Classification Report"),
        tableOutput(session$ns("ldaClassReport")),
        tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),
        tags$hr(),

        tags$h4("Confusion Matrix"),
        tableOutput(session$ns("confusionMatrix")),
        tags$h5(tags$strong("Accuracy Calculation"),
                style = "margin-top: 14px; margin-bottom: 2px;"),
        withMathJax(
          tags$p(HTML(sprintf(
            "\\( \\text{Accuracy} = \\dfrac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\dfrac{%d}{%d} = %.2f\\%% \\)",
            correct, total, acc_used * 100
          )))
        ),
        tags$hr(),

        tags$h4("Prior Probabilities"),
        tableOutput(session$ns("ldaPriors")),

        tags$h4("Group Means"),
        tableOutput(session$ns("groupMeans")),

        tags$h4("Coefficients of Linear Discriminants"),
        tableOutput(session$ns("coefficients")),

        tags$h4("Proportion of Trace"),
        tableOutput(session$ns("propTrace"))
      )
    })

    output$ldaModelInfo <- renderTable({
      r <- calc_results()
      req(r)

      acc <- if (isTRUE(r$use_cv) && !is.null(r$cv_accuracy)) {
        round(r$cv_accuracy, 4)
      } else {
        round(r$accuracy, 4)
      }

      data.frame(
        Item = c(
          "Number of Classes",
          "Number of Predictors",
          "Number of Complete Cases",
          "Cross Validation",
          "Accuracy"
        ),
        Value = c(
          length(r$fit$prior),
          length(r$predictors),
          r$n,
          if (isTRUE(r$use_cv)) "Leave-One-Out" else "None",
          acc
        ),
        check.names = FALSE
      )
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$ldaClassDist <- renderTable({
      r <- calc_results()
      req(r)
      r$class_dist
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$ldaClassReport <- renderTable({
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

    output$ldaPriors <- renderTable({
      r <- calc_results()
      req(r)

      data.frame(
        Class = names(r$fit$prior),
        Prior_Probability = round(as.numeric(r$fit$prior), 4),
        check.names = FALSE
      )
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$groupMeans <- renderTable({
      r <- calc_results()
      req(r)
      round(r$fit$means, 4)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)

    output$coefficients <- renderTable({
      r <- calc_results()
      req(r)
      round(r$fit$scaling, 4)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)

    output$propTrace <- renderTable({
      r <- calc_results()
      req(r)

      data.frame(
        Discriminant = paste0("LD", seq_along(r$prop_trace)),
        Proportion = round(r$prop_trace, 4),
        check.names = FALSE
      )
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$confusionMatrix <- renderTable({
      r <- calc_results()
      req(r)

      cm <- if (isTRUE(r$use_cv) && !is.null(r$cv_confusion)) {
        as.data.frame.matrix(r$cv_confusion)
      } else {
        as.data.frame.matrix(r$confusion)
      }

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

    output$ldaPlot <- renderPlot({
      r <- plot_results()
      req(r)

      scores   <- r$scores
      classes  <- as.character(levels(scores$Class))
      n_cls    <- length(classes)
      base_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")
      # The five fixed colours cover up to five classes. For more classes the
      # extra ones get further colours rather than an NA colour, which would
      # leave their points undrawn.
      cls_cols <- if (n_cls <= 5) {
        base_cols[seq_len(n_cls)]
      } else {
        c(base_cols, grDevices::hcl.colors(n_cls - 5, "Dark 3"))
      }
      col_vec  <- cls_cols[match(as.character(scores$Class), classes)]

      par(mar = c(9, 4, 4, 2))

      plot(
        scores$LD1,
        scores$LD2,
        col       = col_vec,
        pch       = 19,
        cex       = 1.1,
        main      = "LDA Plot",
        xlab      = "LD1",
        ylab      = "LD2",
        cex.main  = 1.3,
        font.main = 2,
        cex.lab   = 1.1,
        font.lab  = 2,
        cex.axis  = 1.0,
        bty       = "l"
      )

      # y is anchored via NDC (device-relative, resolution-independent) so the
      # legend reliably lands just above the bottom edge regardless of the
      # plot's rendered aspect ratio/resolution — a margin-relative inset was
      # getting clipped off-device at some sizes. x is centered on the plot
      # box (not the device) so it lines up with the "LD1" axis label, since
      # unequal left/right margins shift the plot box off device-center.
      legend_x <- mean(par("usr")[1:2])
      legend_y <- grconvertY(0.02, from = "ndc", to = "user")

      legend(
        x      = legend_x,
        y      = legend_y,
        xjust  = 0.5,
        yjust  = 0,
        legend = classes,
        col    = cls_cols[seq_along(classes)],
        pch    = 19,
        pt.cex = 1.1,
        bty    = "n",
        cex    = 0.95,
        title  = "Class",
        horiz  = TRUE,
        xpd    = NA
      )
    })

    # Main calculation
    observeEvent(input$calculate, {
      # Any unexpected error (bad column names, a degenerate class, an
      # unreadable value, ...) is reported in the message area instead of
      # ending the session.
      tryCatch({
        if (!isTruthy(data())) {
          noFileCalculate(TRUE)
          return()
        } else {
          noFileCalculate(FALSE)
        }

        # The response is never also used as a predictor
        predictors <- setdiff(input$predictors, input$response)

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

        if (!isTruthy(input$response) || length(predictors) < 1) {
          return()
        }

        resp_col <- input$response[1]
        df <- data()

        # Continuous response guard — block classification on a continuous variable
        if (ml_is_continuous_response(df[[resp_col]])) {
          responseContinuous(TRUE)
          return()
        }

        analysis_df <- df[, c(predictors, resp_col), drop = FALSE]

        na_cols <- names(which(sapply(analysis_df, function(x) any(is.na(x)))))
        if (length(na_cols) > 0) {
          lda_message(paste0(
            "The following column(s) contain missing values (NA): ",
            paste(na_cols, collapse = ", "),
            ". Please remove or impute missing values before calculating."
          ))
          return()
        }

        analysis_df <- na.omit(analysis_df)

        analysis_df[[resp_col]] <- prepare_lda_response(analysis_df[[resp_col]])

        if (is.null(analysis_df[[resp_col]])) {
          showNotification("Response variable could not be prepared for LDA.", type = "error", duration = 8)
          return()
        }

        predictor_df <- analysis_df[, predictors, drop = FALSE]
        numeric_check <- sapply(predictor_df, is.numeric)

        class_counts    <- table(analysis_df[[resp_col]])
        class_dist_df   <- as.data.frame(class_counts)
        colnames(class_dist_df) <- c("Class", "Count")

        if (nlevels(analysis_df[[resp_col]]) > floor(nrow(analysis_df) / 2)) {
          showNotification(
            "The selected response variable has too many unique values to be treated as a categorical class variable for LDA.",
            type = "error",
            duration = 8
          )
          return()
        }

        if (!all(numeric_check)) {
          showNotification("All explanatory variables must be numeric for LDA.", type = "error", duration = 8)
          return()
        }

        if (nlevels(analysis_df[[resp_col]]) < 2) {
          showNotification("Response variable must have at least 2 classes.", type = "error", duration = 8)
          return()
        }

        if (length(predictors) < 1) {
          showNotification("Select at least one explanatory variables", type = "error", duration = 8)
          return()
        }

        if (any(class_counts < 2)) {
          lda_message(
            "Each class must have at least 2 observations for LDA. One or more classes in your selected response variable have fewer than 2 rows."
          )
          return()
        }

        # Zero variance check
        vars <- sapply(predictor_df, var, na.rm = TRUE)
        zero_var_cols <- names(vars)[is.na(vars) | vars < .Machine$double.eps]
        if (length(zero_var_cols) > 0) {
          lda_message(paste0(
            "These selected variable(s) have zero variance and cannot be used in LDA: ",
            paste(zero_var_cols, collapse = ", "), "."
          ))
          return()
        }

        # Build the formula from symbols rather than pasted text, so column
        # names containing backticks or other special characters cannot break
        # parsing. The environment is baseenv() so the fit does not keep this
        # observer's environment (and the data in it) alive.
        lda_formula <- as.formula(
          call("~", as.name(resp_col),
               Reduce(function(a, b) call("+", a, b), lapply(predictors, as.name))),
          env = baseenv()
        )
        lda_message(NULL)
        lda_fit <- MASS::lda(lda_formula, data = analysis_df)

        lda_pred <- predict(lda_fit, analysis_df[, predictors, drop = FALSE])

        confusion_mat <- table(
          Actual = analysis_df[[resp_col]],
          Predicted = lda_pred$class
        )

        accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)

        singular_vals_sq <- lda_fit$svd^2
        prop_trace <- singular_vals_sq / sum(singular_vals_sq)

        scores_mat <- as.data.frame(lda_pred$x)

        if (!"LD1" %in% names(scores_mat)) {
          scores_mat$LD1 <- 0
        }
        if (!"LD2" %in% names(scores_mat)) {
          scores_mat$LD2 <- 0
        }

        scores_df <- data.frame(
          LD1 = scores_mat$LD1,
          LD2 = scores_mat$LD2,
          Class = analysis_df[[resp_col]]
        )

        cv_confusion <- NULL
        cv_accuracy <- NULL

        if (isTRUE(input$useCV)) {
          lda_cv <- MASS::lda(lda_formula, data = analysis_df, CV = TRUE)

          cv_confusion <- table(
            Actual = analysis_df[[resp_col]],
            Predicted = lda_cv$class
          )

          cv_accuracy <- sum(diag(cv_confusion)) / sum(cv_confusion)
        }

        class_report_preds <- if (isTRUE(input$useCV)) lda_cv$class else lda_pred$class
        lda_class_report   <- knn_classification_report(analysis_df[[resp_col]], class_report_preds)$report

        res <- list(
          fit = lda_fit,
          confusion = confusion_mat,
          accuracy = accuracy,
          prop_trace = prop_trace,
          scores = scores_df,
          cv_confusion = cv_confusion,
          cv_accuracy = cv_accuracy,
          use_cv = isTRUE(input$useCV),
          response = resp_col,
          predictors = predictors,
          n = nrow(analysis_df),
          class_dist = class_dist_df,
          class_report = lda_class_report
        )

        calc_results(res)
        plot_results(res)

        results_ready(TRUE)
        plots_ready(TRUE)
        results_ever_calculated(TRUE)
        plots_ever_calculated(TRUE)

        showTab(inputId = "ldaMainPanel", target = "results_tab")
        showTab(inputId = "ldaMainPanel", target = "plots_tab")

        updateNavbarPage(session, "ldaMainPanel", selected = "results_tab")
      }, error = function(e) {
        lda_message(paste("LDA could not be computed:", conditionMessage(e)))
      })
    }, ignoreInit = TRUE)

    # Reset
    observeEvent(input$reset, {
      hideTab(inputId = "ldaMainPanel", target = "results_tab")
      hideTab(inputId = "ldaMainPanel", target = "plots_tab")
      
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
      lda_message(NULL)
      
      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")

      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNavbarPage(session, "ldaMainPanel", selected = "uploaded_data_tab")
    })
  })
}