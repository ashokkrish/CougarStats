# R/kNearestNeighbors.R

knn_classification_report <- function(actual, predicted) {
  #Convert to factors
  actual <- factor(actual)
  predicted <- factor(predicted, levels = levels(actual))
  
  #confusion matrix
  cm <- table(actual, predicted)
  
  classes <- rownames(cm)
  support <- rowSums(cm)
  
  precision <- recall <- f1 <- numeric(length(classes))
  
  #calculate true positive, false positive & false negative for each class
  for (i in seq_along(classes)) {
    tp <- cm[i, i]
    fp <- sum(cm[, i]) - tp
    fn <- sum(cm[i, ]) - tp
    
    #formulas
    precision[i] <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall[i]    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    
    #f1 score
    f1[i]        <- if (is.na(precision[i]) || is.na(recall[i]) || (precision[i] + recall[i]) == 0) {
      NA_real_
    } else {
      2 * precision[i] * recall[i] / (precision[i] + recall[i])
    }
  }
  
  #accuracy
  accuracy <- sum(diag(cm)) / sum(cm)
  
  report <- data.frame(
    Class = classes,
    Precision = round(precision, 4),
    Recall = round(recall, 4),
    F1 = round(f1, 4),
    Support = support,
    row.names = NULL,
    check.names = FALSE
  )
  
  list(cm = cm, report = report, accuracy = accuracy)
}

# class::knn keeps its candidate neighbours in fixed C arrays of MAX_TIES = 1000
# entries. While it fills its first k slots every insertion is also counted as a
# "tie", and it stops with "too many ties in knn" once 2k - 1 (plus any real ties)
# reaches 998: that is every k >= 500, even when no distances tie. For k >= 999
# that check is never reached, the arrays overflow and the whole R process (every
# user's session) dies with a segfault. 499 is therefore the largest k that can
# ever work; the validator also limits k to the number of training rows.
knn_max_k <- 499L

# The response may have up to n/2 distinct values, so an integer-valued measurement
# (a salary, an ID) can be picked as the "class". The confusion matrix has one row
# and one column per class, so thousands of classes means tens or hundreds of MB of
# HTML for one table (and a browser that cannot show it). 500 classes is far more
# than a classification needs and costs about 7 MB.
knn_max_classes <- 500L

# Approximate cost of one class::knn() call. Each of the n_query rows is compared
# with every training row (p coordinates plus a fixed cost per row), and the k best
# candidates are kept in a sorted list, which costs about k^2 (1 + log(n_train / k))
# per query row. Measured for 1,000 to 232,000 training rows, 1 to 30 predictors and
# k = 1 to 499: about 3e9 units a second, within about 30%.
knn_fit_work <- function(n_train, n_query, p, k) {
  n_train <- as.numeric(n_train)
  k       <- as.numeric(k)
  as.numeric(n_query) * (n_train * (p + 3.5) + k^2 * (1 + log(pmax(1, n_train / k))))
}

# Calculate runs on the R process shared by every session, so a fit larger than
# this (about 30 seconds where it was measured) is refused with a message instead
# of being run.
knn_max_fit_work <- 1e11

# The decision boundary is drawn on an 80 x 80 grid of points; for a fit on a very
# large training set a coarser grid keeps that plot to about 3 seconds.
knn_max_boundary_work <- 1e10

# Points per axis of the decision-boundary grid: 80, or fewer when classifying
# 80 x 80 points against this training set would exceed knn_max_boundary_work
knn_boundary_grid_size <- function(n_train, k, n_grid = 80) {
  per_point <- knn_fit_work(n_train, 1, 2, k)
  if (per_point * n_grid^2 <= knn_max_boundary_work) return(n_grid)
  max(20, floor(sqrt(knn_max_boundary_work / per_point)))
}

# Default k for a training set: sqrt(n_train), but never more than class::knn can run
knn_default_k <- function(n_train) {
  max(1, min(knn_max_k, round(sqrt(n_train))))
}

# Number of training rows for a data set and a train/test split (in %)
knn_n_train <- function(df, split) {
  if (is.null(df) || is.null(split) || length(split) != 1 || is.na(split)) return(NA_real_)
  floor(NROW(df) * (split / 100))
}

# Columns that can be a class label: 2 or more, and at most n/2, distinct non-missing values
knn_valid_response_cols <- function(df) {
  n_rows <- nrow(df)
  keep <- vapply(df, function(x) {
    n_uniq <- sum(!is.na(unique(x)))
    n_uniq >= 2 && n_uniq <= floor(n_rows / 2)
  }, logical(1))
  colnames(df)[keep]
}

# Remember the shared R process's RNG state and return a function that puts it
# back, so Calculate can seed its split (set.seed(123)) without re-seeding the
# random numbers every other session draws from.
knn_save_rng <- function() {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  function() {
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }
}

# The RNG state as it is now (NULL if the process has not drawn any random numbers)
knn_current_rng <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
}

# Run `expr` with the RNG set to a saved state (as returned by knn_current_rng()),
# then hand the shared RNG back. class::knn breaks tied votes at random, so the
# plots made after Calculate continue the stream that Calculate's own fit used:
# they come out the same every time, whatever else the R process has been doing.
knn_with_rng <- function(state, expr) {
  if (is.null(state)) return(expr)
  restore_rng <- knn_save_rng()
  on.exit(restore_rng(), add = TRUE)
  assign(".Random.seed", state, envir = globalenv())
  expr
}

# Text for an error raised while calculating, shown in the sidebar message area
knn_error_message <- function(msg) {
  if (grepl("too many ties", msg, fixed = TRUE)) {
    paste0(
      "Too many training observations are tied (identical or equidistant) for the ",
      "k nearest neighbours to be found. Try a smaller k, or remove duplicate rows."
    )
  } else {
    paste0("k-Nearest Neighbors could not be calculated: ", msg)
  }
}

# Test-set accuracy over a grid of k, for the "Accuracy vs k" plot. The k range is
# the one the plot always used (1 .. max(k + 5, min(50, n_train - 1))), limited to
# what class::knn can run. A range of more than `max_points` values is sampled on
# an even grid (the selected k is always included). For very large data the test
# rows are thinned evenly so the whole curve costs at most `work_cap` units of work
# (about ten seconds), however big the upload is; the result says when that
# happened so the plot can say so. When even `min_test` test rows would cost more
# than `work_cap`, fewer values of k are evaluated first (down to `min_points`), and
# only then are the test rows thinned below `min_test`. One unit is one
# training-point coordinate compared for one test row in a fit at small k; a fit
# at larger k costs more (class::knn keeps its k best candidates in a sorted list),
# about 1 + (k/130)^2 times as much, measured at k = 100 to 499.
knn_accuracy_curve <- function(X_train, X_test, y_train, y_test, k,
                               max_points = 100L, work_cap = 1.5e10, min_test = 200L,
                               min_points = 10L) {
  n_train <- nrow(X_train)
  n_test  <- nrow(X_test)

  max_k  <- min(max(k + 5, min(50, n_train - 1)), n_train, knn_max_k)
  k_grid <- function(n_points) {
    if (max_k <= n_points) {
      seq_len(max_k)
    } else {
      sort(unique(c(round(seq(1, max_k, length.out = n_points)), min(k, max_k))))
    }
  }
  k_vals <- k_grid(max_points)

  per_test_row <- function(kv) as.numeric(n_train) * max(ncol(X_train), 1) * sum(1 + (kv / 130)^2)
  n_use <- n_test
  if (per_test_row(k_vals) * n_test > work_cap) {
    n_min <- min(n_test, as.integer(min_test))
    n_pts <- length(k_vals)
    while (n_pts > min_points && per_test_row(k_vals) * n_min > work_cap) {
      n_pts  <- max(min_points, min(n_pts - 1, floor(n_pts * work_cap / (per_test_row(k_vals) * n_min))))
      k_vals <- k_grid(n_pts)
    }
    n_use <- min(n_test, max(1, floor(work_cap / per_test_row(k_vals))))
  }
  idx <- if (n_use < n_test) unique(round(seq(1, n_test, length.out = n_use))) else seq_len(n_test)
  X_eval <- X_test[idx, , drop = FALSE]
  y_eval <- y_test[idx]

  fit_error <- NULL
  acc_vals <- vapply(k_vals, function(ki) {
    pred <- tryCatch(
      suppressWarnings(class::knn(train = X_train, test = X_eval, cl = y_train, k = ki)),
      error = function(e) { fit_error <<- conditionMessage(e); NULL }
    )
    if (is.null(pred)) NA_real_ else mean(pred == y_eval)
  }, numeric(1))

  list(k_vals = k_vals, acc_vals = acc_vals,
       n_eval = length(idx), n_test = n_test, error = fit_error)
}


KNNSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    # radioButtons(
    #   ns("task"),
    #   strong("Task"),
    #   choices = c("Classification"),
    #   selected = "Classification"
    # ),

    # Let user choose any k
    sliderInput(
      ns("split"),
      label = strong("Train/Test split (%)"),
      min = 50, max = 90, value = 80, step = 1
    ),
    
    numericInput(
      ns("k"),
      label = strong(htmltools::HTML("Number of Neighbors (<em>k</em>)")),
      value = 10,
      min = 1,
      max = knn_max_k,
      step = 1
    ),
    
    checkboxInput(ns("standardize"), "Standardize predictors", value = TRUE),
    
    div(
      style = "font-size: 15px; color: #6c757d; margin-top: 8px; margin-bottom: 6px; ",
      "Select a categorical variable, must have 2 or more unique categories."
    ),

    div(
      id = ns("responseWrapper"),
      pickerInput(
        ns("response"),
        strong(htmltools::HTML("Response Variable (Class)")),
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
        strong(htmltools::HTML("Explanatory Variables (<em>x₁, x₂, ..., xₖ</em>)")),
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Nothing selected")
      ),
      uiOutput(ns("predictorsError"))
    ),
    
    br(),
    p(strong("Graph Options")),
    hr(),
    checkboxInput(
      ns("showBoundary"),
      label = "Decision Boundary Plot",
      value = FALSE
    ),
    uiOutput(ns("boundaryVarUI")),

    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

KNNMainPanelUI <- function(id) {
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
      
      id = ns("knnMainPanel"),
      selected = "uploaded_data_tab",
      theme = bs_theme(version = 4)
    ),

    # Results and Plots stay out of the nav until the first Calculate. They are
    # hidden here, in the markup, rather than by hideTab() when the server starts,
    # so they never flash when this pane is first opened.
    tags$script(HTML(sprintf(
      "$(function() { ['results_tab', 'plots_tab'].forEach(function(v) { $('#%s a[data-value=\"' + v + '\"]').parent().hide(); }); });",
      ns("knnMainPanel")
    )))
  )
}

KNNServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    # ---- Uploaded Data container (message until dataset exists) ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DT::DTOutput(session$ns("knnUploadTable"))
      }
    })
    
    # ---- Uploaded Data Table ----
    output$knnUploadTable <- DT::renderDT({
      req(data())
      df <- data()
      validate(need(ncol(df) > 0, "The uploaded file has no columns."))

      DT::datatable(
        df,
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX = TRUE
        )
      )
    })
    
    # ---- Plots Tab ----
    plot_data <- reactive({
      req(isTRUE(plots_ready()))
      req(data())
      s <- plot_settings()
      req(s)
      
      df <- data()
      
      x <- as.data.frame(df[, s$predictors, drop = FALSE])
      y <- as.factor(df[[s$response]])
      
      list(x = x, y = y)
    })
    
    # 1) Class distribution
    output$knnPlotClass <- renderPlot({
      pd <- plot_data()
      y  <- pd$y

      validate(
        need(nlevels(y) >= 2, "Choose a categorical response variable to plot class distribution.")
      )

      n_cls    <- nlevels(y)
      cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")[seq_len(min(n_cls, 5))]

      plot(
        y,
        col       = cls_cols,
        main      = "Class Distribution",
        xlab      = "Class",
        ylab      = "Frequency",
        cex.main  = 1.3,
        font.main = 2,
        cex.lab   = 1.1,
        font.lab  = 2,
        cex.axis  = 1.0,
        xaxt      = "n"
      )

      axis(
        side      = 1,
        at        = seq_along(levels(y)),
        labels    = levels(y),
        cex.axis  = 1.0,
        font      = 1,
        lwd       = 1,
        lwd.ticks = 1
      )

      box(bty = "l")
    })

    
    # ---- Validation (k required, a whole number, > 0, and no more than class::knn can run) ----
    knn_iv <- shinyvalidate::InputValidator$new()
    
    knn_iv$add_rule("k", shinyvalidate::sv_required())
    knn_iv$add_rule("k", shinyvalidate::sv_integer(message = "Must be a whole number."))
    knn_iv$add_rule("k", shinyvalidate::sv_gt(0, message = "Must be greater than 0."))
    knn_iv$add_rule("k", shinyvalidate::sv_lte(knn_max_k, message_fmt = "Must be at most {rhs}."))
    knn_iv$add_rule("k", function(value) {
      n_train <- knn_n_train(data(), input$split)
      if (!is.na(n_train) && n_train >= 1 && value > n_train) {
        paste0("Must not exceed the number of training observations (", n_train, ").")
      }
    })
    
    knn_iv$enable()
    
    results_ready <- reactiveVal(FALSE)
    calc_settings <- reactiveVal(NULL)
    
    plots_ready <- reactiveVal(FALSE)
    plot_settings <- reactiveVal(NULL)
    
    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)
    
    # The Results and Plots tabs start hidden: that initial state is set in the
    # markup (KNNMainPanelUI), not here, so nothing flashes when the pane opens.
    
    responseError <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    responseContinuous <- reactiveVal(FALSE)

    fileImportError <- reactiveVal(FALSE)
    knn_message     <- reactiveVal(NULL)

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

    output$fileImportUserMessage <- renderUI({
      if (isTRUE(fileImportError())) {
        tags$p(
          style = "color: red; font-weight: bold; margin-bottom: 10px;",
          "Required: Cannot calculate without a data file."
        )
      } else {
        msg <- knn_message()
        if (is.null(msg)) return(NULL)
        div(
          style = "margin-top:10px;",
          div(class = "alert alert-danger", msg)
        )
      }
    })
    
    # ---- Clear old outputs when any input changes after Calculate ----
    observeEvent(
      list(
        data(),
        input$split,
        input$k,
        input$standardize,
        input$response,
        input$predictors
      ),
      {
        was_shown <- isTRUE(results_ready()) || isTRUE(plots_ready())

        # A message from the last Calculate is about the previous data/settings
        knn_message(NULL)
        
        if (isTRUE(results_ready())) {
          results_ready(FALSE)
          calc_settings(NULL)
        }
        
        if (isTRUE(plots_ready())) {
          plots_ready(FALSE)
          plot_settings(NULL)
        }
        
        # Results/Plots are only in the nav while they are current, so there is
        # nothing to hide or navigate away from unless they were showing.
        if (was_shown) {
          hideTab(inputId = "knnMainPanel", target = "results_tab")
          hideTab(inputId = "knnMainPanel", target = "plots_tab")
          updateNavbarPage(session, "knnMainPanel", selected = "uploaded_data_tab")
        }
      },
      ignoreInit = TRUE
    )
    
    output$resultsContainer <- renderUI({
      if (!isTRUE(results_ready())) {
        
        # First time ever (before any Calculate)
        if (!isTRUE(results_ever_calculated())) {
          return(tagList(
            helpText("No results yet. Upload a dataset, choose variables, then click Calculate.")
          ))
        }
        
        # User calculated before, but changed something
        return(tagList(
          helpText("Settings changed. Click Calculate to update results.")
        ))
      }
      
      uiOutput(session$ns("resultsUI"))
    })
    
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
      
      {
        s <- plot_settings()
        show_boundary <- !is.null(s) && isTRUE(s$showBoundary) && !is.null(s$boundaryVars)

        tagList(
          tags$h4("Class Distribution"),
          plotOutput(session$ns("knnPlotClass"), height = "350px"),

          if (show_boundary) tagList(
            tags$hr(),
            tags$h4("Decision Boundary"),
            plotOutput(session$ns("knnPlotBoundary"), height = "500px"),
            uiOutput(session$ns("knnBoundaryNote"))
          ),

          tags$hr(),

          tags$h4("Accuracy vs k"),
          plotOutput(session$ns("knnPlotAccVsK"), height = "400px"),
          uiOutput(session$ns("knnAccVsKNote"))
        )
      }
    })
    
    
    output$boundaryVarUI <- renderUI({
      req(isTRUE(input$showBoundary))
      preds <- input$predictors

      if (length(preds) < 2) {
        return(tags$p(
          style = "font-size: 13px; color: #6c757d; margin-top: 4px;",
          "Select at least 2 explanatory variables to use this plot."
        ))
      }

      pickerInput(
        session$ns("boundaryVars"),
        label   = strong("Select 2 Variables for Axes"),
        choices  = preds,
        selected = head(preds, 2),
        multiple = TRUE,
        options  = list(
          `max-options`      = 2,
          `max-options-text` = "Select exactly 2 variables",
          `live-search`      = TRUE,
          title              = "Select exactly 2 variables"
        )
      )
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


    observeEvent(data(), {
      req(data())

      fileImportError(FALSE)

      df0 <- data()
      cols <- colnames(df0)
      numeric_cols <- cols[vapply(df0, is.numeric, logical(1))]

      shared_resp    <- shared_response()

      valid_response <- knn_valid_response_cols(df0)
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% valid_response) shared_resp else character(0)

      # The response is never offered as a predictor (the response observer
      # below does not run when a new file keeps the same response selected)
      predictor_cols <- setdiff(numeric_cols, pre_response)
      pre_predictors <- intersect(shared_explanatory(), predictor_cols)

      expect_echo("response",   pre_response)
      expect_echo("predictors", pre_predictors)
      updatePickerInput(session, "response",   choices = valid_response, selected = pre_response)
      updatePickerInput(session, "predictors", choices = predictor_cols, selected = pre_predictors)

      if (isTruthy(input$split)) {
        n <- nrow(df0)
        n_train <- floor(n * (input$split / 100))
        default_k <- knn_default_k(n_train)
        updateNumericInput(session, "k", value = default_k, max = max(1, min(knn_max_k, n_train)))
      }
    }, ignoreNULL = TRUE)
    
    #reacts to train/test split changes by recalculating a default k based on training size
    #(ignoreInit: when this pane is first opened with data already loaded, the data() observer above has just done this)
    observeEvent(input$split, {
      req(data())
      
      n <- nrow(data())
      n_train <- floor(n * (input$split / 100))
      default_k <- knn_default_k(n_train)
      
      updateNumericInput(session, "k", value = default_k, max = max(1, min(knn_max_k, n_train)))
    }, ignoreInit = TRUE)
    
    # Also runs when the response is cleared, so its column becomes selectable as
    # a predictor again and the continuous-response warning goes away (a cleared
    # response is not written to the shared selection)
    observeEvent(input$response, {
      if (!is_echo("response", input$response) && !is.null(input$response)) shared_response(input$response)
      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      if (isTruthy(data())) {
        df <- data()
        cols <- colnames(df)
        numeric_cols <- cols[vapply(df, is.numeric, logical(1))]
        available_predictors <- setdiff(numeric_cols, input$response)
        selected_predictors  <- intersect(input$predictors, available_predictors)
        updatePickerInput(session, "predictors", choices = available_predictors, selected = selected_predictors)

        responseContinuous(
          isTruthy(input$response) && ml_is_continuous_response(df[[input$response[1]]])
        )
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
    
    #reset button
    observeEvent(input$reset, {
      
      hideTab(inputId = "knnMainPanel", target = "results_tab")
      hideTab(inputId = "knnMainPanel", target = "plots_tab")
      
      results_ready(FALSE)
      plots_ready(FALSE)
      
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)
      
      calc_settings(NULL)
      plot_settings(NULL)
      
      responseError(FALSE)
      predictorsError(FALSE)
      responseContinuous(FALSE)
      fileImportError(FALSE)
      knn_message(NULL)

      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      
      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNavbarPage(session, "knnMainPanel", selected = "uploaded_data_tab")
    })


    # ---- Results and plot outputs ----
    # Created once, here, rather than inside the Calculate observer (which redefined
    # them, and kept everything they captured alive, on every click). Each one reads
    # calc_settings() / plot_settings(), which are NULL until Calculate has run and
    # again as soon as an input changes, and shows nothing then.

    output$resultsUI <- renderUI({
      s       <- calc_settings()
      req(s)
      correct <- sum(diag(s$metrics$cm))
      total   <- sum(s$metrics$cm)

      tagList(
        tags$h4("Model Summary"),
        tableOutput(session$ns("knnModelInfo")),

        tags$hr(),

        tags$h5(tags$strong(HTML("Your Selected <em>k</em>"))),
        tags$p(HTML(paste0(
          "You selected <em>k</em> = <strong>", s$k, "</strong>"
        ))),

        tags$h5(tags$strong(HTML("Recommended <em>k</em>"))),
        tags$p(HTML(paste0(
          "Based on your training set size, the recommended starting <em>k</em> is calculated as: ",
          "&radic;<em>n</em><sub>train</sub>",
          " = &radic;", s$n_train,
          " = ", sprintf("%.2f", sqrt(s$n_train)),
          if (s$k_recommended < s$k_sqrt_odd) {
            paste0(" &asymp; ", s$k_sqrt_odd, ", more than the largest <em>k</em> available (",
                   knn_max_k, "), so <strong>", s$k_recommended, "</strong>")
          } else {
            paste0(" &asymp; <strong>", s$k_recommended, "</strong>")
          }
        ))),

        tags$p(
          style = "color: #6c757d; font-size: 14px; margin-top: 4px;",
          HTML(paste0(
            "The recommended <em>k</em> is a starting point. Your chosen <em>k</em> may perform ",
            "better depending on your dataset. Always validate using the confusion matrix results."
          ))
        ),

        tags$hr(),

        tags$h4("Class Distribution (Full Dataset)"),
        tableOutput(session$ns("classDist")),
        tags$hr(),

        tags$h4("Classification Report"),
        tableOutput(session$ns("classReport")),
        tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),

        tags$h4("Confusion Matrix"),
        tableOutput(session$ns("confMat")),
        tags$h5(tags$strong("Accuracy Calculation"),
                style = "margin-top: 14px; margin-bottom: 2px;"),
        withMathJax(
          tags$p(HTML(sprintf(
            "\\( \\text{Accuracy} = \\dfrac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\dfrac{%d}{%d} = %.2f\\%% \\)",
            correct, total, s$metrics$accuracy * 100
          )))
        )
      )
    })
    
    #send the data into the UI
    output$knnModelInfo <- renderTable({
      s <- calc_settings()
      req(s)

      data.frame(
        Item = c(
          "Number of Observations",
          "Training Observations",
          "Test Observations",
          "Train/Test Split",
          "Selected k",
          "Recommended k",
          "Accuracy"
        ),
        Value = c(
          as.character(s$n_total),
          as.character(s$n_train),
          as.character(s$n_total - s$n_train),
          paste0(s$split, "%"),
          as.character(s$k),
          as.character(s$k_recommended),
          sprintf("%.4f", s$metrics$accuracy)
        ),
        check.names = FALSE
      )
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$classDist    <- renderTable({
      s <- calc_settings()
      req(s)
      s$class_dist
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)
    output$classReport <- renderTable({
      s <- calc_settings()
      req(s)
      s$metrics$report
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
    output$confMat <- renderTable({
      s <- calc_settings()
      req(s)
      cm <- as.data.frame.matrix(s$metrics$cm)
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

    output$knnPlotBoundary <- renderPlot({
      s <- plot_settings()
      req(s, !is.null(s$boundaryVars), length(s$boundaryVars) == 2)

      p1 <- s$boundaryVars[1]
      p2 <- s$boundaryVars[2]

      X_tr <- s$X_train[, c(p1, p2), drop = FALSE]
      X_te <- s$X_test[,  c(p1, p2), drop = FALSE]

      x1_rng <- range(c(X_tr[, 1], X_te[, 1]), na.rm = TRUE)
      x2_rng <- range(c(X_tr[, 2], X_te[, 2]), na.rm = TRUE)
      pad    <- 0.1
      n_grid <- knn_boundary_grid_size(nrow(X_tr), s$k)

      x1_seq <- seq(x1_rng[1] - pad * diff(x1_rng),
                    x1_rng[2] + pad * diff(x1_rng), length.out = n_grid)
      x2_seq <- seq(x2_rng[1] - pad * diff(x2_rng),
                    x2_rng[2] + pad * diff(x2_rng), length.out = n_grid)

      grid_mat           <- as.matrix(expand.grid(x1_seq, x2_seq))
      colnames(grid_mat) <- c(p1, p2)

      grid_pred <- knn_with_rng(s$rng, tryCatch(
        suppressWarnings(class::knn(train = X_tr, test = grid_mat, cl = s$y_train, k = s$k)),
        error = function(e) NULL
      ))
      validate(need(!is.null(grid_pred),
                    "The decision boundary could not be computed for this k (too many tied distances). Try a smaller k."))

      classes  <- levels(s$y_train)
      n_cls    <- length(classes)
      cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")[seq_len(min(n_cls, 5))]

      # Lighten region colours toward white so they remain visible on both
      # light and dark backgrounds without relying on alpha blending
      lighten <- function(col, f = 0.55) {
        v <- col2rgb(col) / 255
        v <- v + (1 - v) * f
        rgb(v[1, ], v[2, ], v[3, ])
      }
      bg_cols <- lighten(cls_cols)

      class_idx <- match(as.character(grid_pred), classes)
      z_mat     <- matrix(class_idx, nrow = length(x1_seq), ncol = length(x2_seq))
      breaks    <- seq(0.5, n_cls + 0.5, by = 1)

      par(mar = c(5, 4, 4, 2), cex.main = 1.3, font.main = 2,
          cex.lab = 1.1, font.lab = 2, cex.axis = 1.0)

      image(
        x1_seq, x2_seq, z_mat,
        col    = bg_cols,
        breaks = breaks,
        main   = paste0("Decision Boundary (", p1, " vs ", p2, ")"),
        xlab   = p1,
        ylab   = p2
      )

      box(bty = "l")

      all_X2  <- rbind(X_tr, X_te)
      all_y   <- c(as.character(s$y_train), as.character(s$y_test))
      all_col <- cls_cols[match(all_y, classes)]
      points(all_X2[, 1], all_X2[, 2], col = all_col, pch = 19, cex = 0.9)

      legend(
        "right",
        legend = classes,
        col    = cls_cols[seq_along(classes)],
        pch    = 19,
        pt.cex = 0.9,
        bty    = "n",
        cex    = 0.95,
        title  = "Class"
      )
    })

    # Said only when the boundary grid had to be coarser to keep the plot quick
    output$knnBoundaryNote <- renderUI({
      s <- plot_settings()
      req(s, !is.null(s$boundaryVars), length(s$boundaryVars) == 2)
      n_grid <- knn_boundary_grid_size(nrow(s$X_train), s$k)
      if (n_grid >= 80) return(NULL)
      tags$p(style = "color: #6c757d; font-size: 14px; margin-top: 4px;", sprintf(
        "The shaded regions are computed on a %d x %d grid of points instead of 80 x 80, to keep the plot quick on a large data set.",
        n_grid, n_grid
      ))
    })

    # Accuracy vs k: worked out once per Calculate (when the plot is first drawn),
    # not again on every redraw.
    acc_curve <- reactive({
      s <- plot_settings()
      req(s)
      knn_with_rng(s$rng, knn_accuracy_curve(s$X_train, s$X_test, s$y_train, s$y_test, s$k))
    })

    output$knnPlotAccVsK <- renderPlot({
      s <- plot_settings()
      req(s)

      cur      <- acc_curve()
      k_vals   <- cur$k_vals
      acc_vals <- cur$acc_vals
      ok       <- !is.na(acc_vals)

      validate(need(any(ok), paste0("Accuracy could not be computed: ", cur$error)))

      par(mar = c(7, 4, 4, 2))

      plot(
        k_vals[ok], acc_vals[ok],
        type      = "l",
        col       = "#4472C4",
        lwd       = 2,
        main      = "Accuracy vs k",
        xlab      = "k (Number of Neighbors)",
        ylab      = "Test Set Accuracy",
        ylim      = c(max(0, min(acc_vals[ok]) - 0.05), min(1, max(acc_vals[ok]) + 0.05)),
        cex.main  = 1.3,
        font.main = 2,
        cex.lab   = 1.1,
        font.lab  = 2,
        cex.axis  = 1.0,
        bty       = "l"
      )

      points(k_vals[ok], acc_vals[ok], pch = 19, col = "#4472C4", cex = 0.6)

      sel <- match(s$k, k_vals)
      if (!is.na(sel) && ok[sel]) {
        points(s$k, acc_vals[sel], col = "#ED7D31", pch = 19, cex = 1.8)
        abline(v = s$k, col = "#ED7D31", lty = 2, lwd = 1.5)
      }

      legend(
        "bottom",
        legend = c("Accuracy", paste0("Selected k = ", s$k)),
        col    = c("#4472C4", "#ED7D31"),
        lty    = c(1, 2),
        pch    = c(19, 19),
        pt.cex = c(0.6, 1.5),
        lwd    = c(2, 1.5),
        bty    = "n",
        cex    = 0.95,
        horiz  = TRUE,
        xpd    = TRUE,
        inset  = c(0, -0.35)
      )
    })

    # Said only when the curve had to be thinned to keep it quick on a large data set
    output$knnAccVsKNote <- renderUI({
      cur <- acc_curve()
      notes <- character(0)
      if (length(cur$k_vals) < max(cur$k_vals)) {
        notes <- c(notes, sprintf("Accuracy is evaluated at %d evenly spaced values of k.", length(cur$k_vals)))
      }
      if (cur$n_eval < cur$n_test) {
        notes <- c(notes, sprintf(
          "It is estimated on an evenly spaced subsample of %s of the %s test observations, to keep the plot quick on a large data set.",
          format(cur$n_eval, big.mark = ","), format(cur$n_test, big.mark = ",")
        ))
      }
      if (length(notes) == 0) return(NULL)
      tags$p(style = "color: #6c757d; font-size: 14px; margin-top: 4px;", paste(notes, collapse = " "))
    })

    # ---- Calculate ----
    # The work done by the Calculate button. Whatever goes wrong in here, including
    # an error from class::knn itself, is caught by the observer below and shown in
    # the sidebar message area, instead of ending the user's session.
    knn_calculate <- function() {
      knn_message(NULL)

      # validation
      if (is.null(data()) || NROW(data()) == 0) {
        fileImportError(TRUE)
        return()
      }
      
      fileImportError(FALSE)
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

      req(knn_iv$is_valid())
      resp_col <- input$response[1]

      #split dataset for training & testing
      df <- data()

      # Continuous response guard — block classification on a continuous variable
      if (ml_is_continuous_response(df[[resp_col]])) {
        responseContinuous(TRUE)
        return()
      }

      # NA check — class::knn cannot handle missing values, in the predictors or in the class labels
      na_cols <- names(which(sapply(df[, predictors, drop = FALSE], function(x) any(is.na(x)))))
      resp_has_na <- anyNA(df[[resp_col]])
      if (length(na_cols) > 0 || resp_has_na) {
        na_msgs <- character(0)
        if (length(na_cols) > 0) {
          na_msgs <- c(na_msgs, paste0(
            "The following predictor column(s) contain missing values (NA): ",
            paste(na_cols, collapse = ", "), "."
          ))
        }
        if (resp_has_na) {
          na_msgs <- c(na_msgs, paste0("The response variable (", resp_col, ") contains missing values (NA)."))
        }
        knn_message(paste(c(na_msgs, "Please remove or impute missing values before calculating."), collapse = " "))
        return()
      }

      # Class count check — a "response" with this many distinct values is a measurement, not a class label
      n_classes <- length(unique(df[[resp_col]]))
      if (n_classes > knn_max_classes) {
        knn_message(paste0(
          "The response variable (", resp_col, ") has ", n_classes, " distinct values, but k-Nearest Neighbors ",
          "supports at most ", knn_max_classes, " classes. Choose a categorical response variable (class label)."
        ))
        return()
      }

      # Zero variance check
      vars <- sapply(df[, predictors, drop = FALSE], var, na.rm = TRUE)
      zero_var_cols <- names(vars)[is.na(vars) | vars < .Machine$double.eps]
      if (length(zero_var_cols) > 0) {
        knn_message(paste0(
          "These selected variable(s) have zero variance and cannot be used in KNN: ",
          paste(zero_var_cols, collapse = ", "), "."
        ))
        return()
      }

      n <- nrow(df)
      n_train <- floor(n * (input$split / 100))
      if (n_train < 1 || n - n_train < 1) {
        knn_message("There are too few observations to split into training and test sets. Upload more data or change the Train/Test split.")
        return()
      }

      # Size guard: the fit runs on the R process shared by every session
      k <- as.integer(input$k) #Ensures k is an integer
      if (knn_fit_work(n_train, n - n_train, length(predictors), k) > knn_max_fit_work) {
        k_ok <- seq_len(min(knn_max_k, n_train))
        k_ok <- k_ok[knn_fit_work(n_train, n - n_train, length(predictors), k_ok) <= knn_max_fit_work]
        knn_message(paste0(
          "This k-Nearest Neighbors model is too large to calculate here: comparing ",
          format(n - n_train, big.mark = ",", scientific = FALSE), " test observations with ",
          format(n_train, big.mark = ",", scientific = FALSE), " training observations (",
          length(predictors), " explanatory variable", if (length(predictors) == 1) "" else "s",
          ", k = ", k, ") would take too long. ",
          if (length(k_ok) > 0) {
            paste0("With this data, use k of at most ", max(k_ok),
                   ", fewer explanatory variables or a data set with fewer rows.")
          } else {
            "Use fewer explanatory variables or a data set with fewer rows."
          }
        ))
        return()
      }

      # Same seeded split as ever, but hand the shared R process's RNG state back
      # afterwards so other sessions' random numbers are not re-seeded
      restore_rng <- knn_save_rng()
      on.exit(restore_rng(), add = TRUE)
      set.seed(123)
      
      train_idx <- sample(seq_len(n), size = n_train)
      
      train <- df[train_idx, , drop = FALSE]
      test  <- df[-train_idx, , drop = FALSE]
      
      # predictors matrix
      X_train <- train[, predictors, drop = FALSE]
      X_test  <- test[, predictors, drop = FALSE]
      X_train <- as.matrix(X_train)
      X_test  <- as.matrix(X_test)
      
      
      #make all predictors comparable by putting them on the same scale
      #in the case that one predictor column has big numbers and another has small numbers
      if (isTRUE(input$standardize)) {
        mu  <- colMeans(X_train, na.rm = TRUE)
        sdv <- apply(X_train, 2, sd, na.rm = TRUE)
        
        #prevents divide-by-zero
        sdv[is.na(sdv) | sdv == 0] <- 1
        
        X_train <- scale(X_train, center = mu, scale = sdv)
        X_test  <- scale(X_test, center = mu, scale = sdv)
      }
      
      # CLASSIFICATION (the only task offered)
        
      # Shared levels so a class missing from one split doesn't make
      # y_train/y_test/pred disagree on levels (breaks == comparisons).
      resp_levels <- sort(unique(as.character(na.omit(df[[resp_col]]))))
      y_train <- factor(train[[resp_col]], levels = resp_levels)
      y_test  <- factor(test[[resp_col]],  levels = resp_levels)
      
      #Run kNN classification algorithm
      pred <- class::knn(
        train = X_train,
        test  = X_test,
        cl    = y_train,
        k     = k
      )
      
      # where the RNG stands after the fit: the plots carry on from here
      rng_after_fit <- knn_current_rng()
      
      #evaluation metrics 
      metrics <- knn_classification_report(actual = y_test, predicted = pred)

      class_dist_df <- as.data.frame(table(as.factor(df[[resp_col]])))
      colnames(class_dist_df) <- c("Class", "Count")

      # nearest odd integer of sqrt(n_train) — recommended starting k
      k_round_val   <- round(sqrt(n_train))
      k_sqrt_odd    <- if (k_round_val %% 2 == 0) k_round_val + 1L else as.integer(k_round_val)
      # ... but never more than class::knn can run (knn_max_k is odd)
      k_recommended <- min(k_sqrt_odd, knn_max_k)

      # to keep train/split static until user clicks calculate
      calc_settings(list(
        split         = input$split,
        k             = k,
        n_total       = n,
        n_train       = n_train,
        k_recommended = k_recommended,
        k_sqrt_odd    = k_sqrt_odd,
        metrics       = metrics,
        class_dist    = class_dist_df
      ))

      boundary_vars <- if (isTRUE(input$showBoundary) &&
                           length(input$boundaryVars) == 2) input$boundaryVars else NULL

      plot_settings(list(
        response     = resp_col,
        predictors   = predictors,
        X_train      = X_train,
        X_test       = X_test,
        y_train      = y_train,
        y_test       = y_test,
        k            = k,
        showBoundary = isTRUE(input$showBoundary),
        boundaryVars = boundary_vars,
        rng          = rng_after_fit
      ))

      results_ready(TRUE)
      plots_ready(TRUE)
      results_ever_calculated(TRUE)
      plots_ever_calculated(TRUE)

      showTab(inputId = "knnMainPanel", target = "results_tab")
      showTab(inputId = "knnMainPanel", target = "plots_tab")
      
      shinyjs::delay(100, {
        updateNavbarPage(session, "knnMainPanel", selected = "results_tab")
      })
    }

    observeEvent(input$calculate, {
      tryCatch(
        knn_calculate(),
        # req()/validate() stopping quietly, as Shiny itself treats it
        shiny.silent.error = function(e) invisible(NULL),
        error = function(e) knn_message(knn_error_message(conditionMessage(e)))
      )
    }, ignoreInit = TRUE)
    
  })
}


