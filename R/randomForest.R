# R/randomForest.R

# ============== LIMITS AND HELPERS ==============

# Largest (training rows x trees) product fitted by one Calculate. Growing the
# forest takes about 90 s on one core and peaks at roughly 4 GB of memory at the
# limit (measured: about 135 bytes per training row and tree, because
# randomForest pre-allocates (2 * rows + 1) x trees node arrays and copies them
# around its C call). Beyond that the fit blocks the shared R process for
# minutes or can run it out of memory.
RF_MAX_TRAIN_TREES <- 3e7

# PDP / ICE / ALE are computed on at most this many training rows (all rows when
# there are fewer), and on fewer when many predictors x trees would make the
# forest predictions too slow: PDP and ICE predict rows x grid points per
# predictor through every tree.
RF_EFFECT_MAX_ROWS  <- 500L
RF_EFFECT_WORK      <- 1e8
RF_EFFECT_GRID_SIZE <- 20L

# Runs expr with the global RNG seeded, then puts the caller's RNG state back
# (and removes .Random.seed again if there was none), so a seeded model fit in
# one session does not reset the random stream of every other session.
rf_with_seed <- function(seed, expr) {
  env      <- globalenv()
  had_seed <- exists(".Random.seed", envir = env, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = env, inherits = FALSE) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = env)
    } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
      rm(".Random.seed", envir = env)
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

# Columns that can serve as a class label: 2 to floor(n / 2) distinct non-missing values.
rf_response_choices <- function(df) {
  n_rows <- nrow(df)
  ok <- vapply(df, function(x) {
    n_uniq <- length(unique(x[!is.na(x)]))
    n_uniq >= 2 && n_uniq <= floor(n_rows / 2)
  }, logical(1))
  colnames(df)[ok]
}

# Number of training rows PDP / ICE / ALE are computed on.
rf_effect_sample_size <- function(n_train, n_pred, ntree) {
  by_work <- floor(RF_EFFECT_WORK / (RF_EFFECT_GRID_SIZE * max(1, n_pred) * max(1, ntree)))
  as.integer(min(n_train, RF_EFFECT_MAX_ROWS, max(100L, by_work)))
}

# Rows PDP / ICE / ALE are computed on: all rows, or a fixed-seed random sample
# that always includes the row holding each predictor's minimum and maximum so
# the plotted ranges match those of the full training set.
rf_effect_rows <- function(X, eff_n) {
  n <- nrow(X)
  if (eff_n >= n) return(seq_len(n))

  extremes <- unique(unlist(lapply(X, function(v) {
    if (is.numeric(v)) c(which.min(v), which.max(v)) else integer(0)
  })))
  extremes <- extremes[seq_len(min(length(extremes), eff_n))]
  rest     <- setdiff(seq_len(n), extremes)
  picked   <- rest[rf_with_seed(123, sample.int(length(rest), eff_n - length(extremes)))]
  sort(c(extremes, picked))
}

# Split, fit and evaluate the forest. Returns list(message = "...") when the
# model cannot be built, otherwise list(result = <slim results list>). The
# fitted forest and the rows used for the effect plots are kept in
# result$model (an environment) only until the effect plots have been computed.
rf_fit_model <- function(analysis_df, resp_col, predictors, ntree, mtry_val, n_train,
                         step = function(value, detail = NULL) invisible(NULL)) {
  n <- nrow(analysis_df)

  # Class distribution from full (cleaned) dataset
  class_dist_df <- as.data.frame(table(analysis_df[[resp_col]]))
  colnames(class_dist_df) <- c("Class", "Count")

  # The fit uses randomForest's x / y interface. Its formula interface rebuilds
  # the data with data.frame() and reformulate(), which fails for any column
  # name that is not a syntactic R name (a space, a backtick, a $ ...).

  # Split, fit and predict the test set with the fixed seed (the test-set
  # prediction breaks vote ties at random, so it belongs to the seeded stream
  # too). The RNG state of the process is restored afterwards.
  step(0.05, "Splitting the data")
  train_df <- test_df <- rf_fit <- test_pred <- NULL
  rf_with_seed(123, {
    train_idx <- sample(seq_len(n), size = n_train)
    train_df  <- analysis_df[ train_idx, , drop = FALSE]
    test_df   <- analysis_df[-train_idx, , drop = FALSE]

    rf_args <- list(
      x          = train_df[, predictors, drop = FALSE],
      y          = train_df[[resp_col]],
      ntree      = ntree,
      importance = TRUE
    )
    if (!is.null(mtry_val)) rf_args$mtry <- mtry_val

    step(0.10, paste0("Growing ", ntree, " trees"))
    rf_fit <- tryCatch(do.call(randomForest::randomForest, rf_args), error = function(e) e)

    if (!inherits(rf_fit, "error")) {
      step(0.85, "Evaluating on the test set")
      test_pred <- tryCatch(
        predict(rf_fit, test_df[, predictors, drop = FALSE]),
        error = function(e) e
      )
    }
  })
  if (inherits(rf_fit, "error")) {
    return(list(message = paste("Random Forest could not be computed:", conditionMessage(rf_fit))))
  }
  if (inherits(test_pred, "error")) {
    return(list(message = paste("Predictions could not be generated:", conditionMessage(test_pred))))
  }
  # The call holds a copy of the training data and of the randomForest function
  rf_fit$call <- quote(randomForest::randomForest(x = x, y = y, ntree = ntree, importance = TRUE))

  # Confusion matrix
  confusion_mat <- table(
    Actual    = test_df[[resp_col]],
    Predicted = test_pred
  )
  accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)

  # Per-class sensitivity and specificity
  classes <- rownames(confusion_mat)
  class_metrics_df <- do.call(rbind, lapply(classes, function(cls) {
    tp  <- confusion_mat[cls, cls]
    fn  <- sum(confusion_mat[cls, ]) - tp
    fp  <- sum(confusion_mat[, cls]) - tp
    tn  <- sum(confusion_mat) - tp - fn - fp
    sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
    data.frame(
      Class       = cls,
      Sensitivity = round(sens, 4),
      Specificity = round(spec, 4),
      stringsAsFactors = FALSE
    )
  }))

  # OOB error rate (final tree)
  oob_error <- rf_fit$err.rate[nrow(rf_fit$err.rate), "OOB"]

  # Classification report (test set)
  rf_class_report <- knn_classification_report(test_df[[resp_col]], test_pred)$report

  # Variable importance (small matrices; the variable importance plot only needs these)
  imp_acc  <- randomForest::importance(rf_fit, type = 1)
  imp_gini <- randomForest::importance(rf_fit, type = 2)

  # What the effect plots (PDP / ICE / ALE) need later: the forest and a row sample.
  eff_n   <- rf_effect_sample_size(n_train, length(predictors), ntree)
  eff_idx <- rf_effect_rows(train_df[, predictors, drop = FALSE], eff_n)
  model <- new.env(parent = emptyenv())
  model$fit          <- rf_fit
  model$X            <- train_df[eff_idx, predictors, drop = FALSE]
  model$y            <- train_df[[resp_col]][eff_idx]
  model$effects      <- NULL
  model$effects_done <- FALSE

  list(result = list(
    model         = model,
    response      = resp_col,
    predictors    = predictors,
    class_dist    = class_dist_df,
    n_total       = n,
    n_train       = n_train,
    n_test        = n - n_train,
    ntree         = ntree,
    mtry_used     = rf_fit$mtry,
    oob_error     = oob_error,
    confusion     = confusion_mat,
    accuracy      = accuracy,
    class_metrics = class_metrics_df,
    class_report  = rf_class_report,
    importance    = list(acc = imp_acc, gini = imp_gini),
    effect_n      = eff_n
  ))
}

# PDP + ICE (one iml pass; the PDP is the mean of the ICE curves) and ALE for each
# predictor. A predictor that fails gets NULL entries; NULL is returned when iml
# itself cannot be used.
rf_compute_effects <- function(fit, X, y, predictors, step = function(i, pvar) invisible(NULL)) {
  tryCatch({
    # iml rebuilds the data with data.frame(), which renames columns whose names
    # are not syntactic R names (a space, a backtick, ...). It therefore gets
    # syntactic stand-ins (make.names() leaves ordinary names as they are); the
    # forest is given its own column names back, and so are the result tables.
    safe <- make.names(predictors, unique = TRUE)
    names(X) <- safe
    predict_prob <- function(model, newdata) {
      if (!identical(safe, predictors)) names(newdata) <- predictors[match(names(newdata), safe)]
      predict(model, newdata, type = "prob")
    }
    restore_name <- function(res, from, to) {
      if (!is.null(res) && from != to) names(res)[names(res) == from] <- to
      res
    }

    # One predict() call per predictor and effect (iml's default of 1000 rows per
    # call copies the whole forest on every call).
    predictor_iml <- suppressWarnings(
      iml::Predictor$new(
        model            = fit,
        data             = X,
        y                = y,
        predict.function = predict_prob,
        batch.size       = max(1000L, RF_EFFECT_GRID_SIZE * nrow(X))
      )
    )

    pdp_list <- ice_list <- ale_list <- stats::setNames(vector("list", length(predictors)), predictors)

    for (i in seq_along(predictors)) {
      pvar <- predictors[i]
      svar <- safe[i]
      step(i, pvar)

      pdp_ice <- restore_name(suppressWarnings(tryCatch(
        iml::FeatureEffect$new(predictor_iml, feature = svar, method = "pdp+ice")$results,
        error = function(e) NULL
      )), svar, pvar)
      if (!is.null(pdp_ice)) {
        is_pdp <- pdp_ice$.type == "pdp"
        pdp <- pdp_ice[is_pdp,  setdiff(names(pdp_ice), ".id"), drop = FALSE]
        ice <- pdp_ice[!is_pdp, , drop = FALSE]
        rownames(pdp) <- NULL
        rownames(ice) <- NULL
        pdp_list[[pvar]] <- pdp
        ice_list[[pvar]] <- ice
      }

      ale_list[pvar] <- list(restore_name(suppressWarnings(tryCatch(
        iml::FeatureEffect$new(predictor_iml, feature = svar, method = "ale")$results,
        error = function(e) NULL
      )), svar, pvar))
    }

    list(pdp = pdp_list, ice = ice_list, ale = ale_list)
  }, error = function(e) NULL)
}

# One class slice of an ICE table as plain vectors: grid values and a
# (grid x observation) matrix, built in a single pass.
rf_ice_matrix <- function(cls_ice, pred_var) {
  x_vals  <- sort(unique(cls_ice[[pred_var]]))
  obs_ids <- sort(unique(cls_ice$.id))

  y_mat <- matrix(NA_real_, nrow = length(x_vals), ncol = length(obs_ids))
  y_mat[cbind(match(cls_ice[[pred_var]], x_vals), match(cls_ice$.id, obs_ids))] <- cls_ice$.value

  list(x = x_vals, y = y_mat)
}


# ============== UI ==============

RFSidebarUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    sliderInput(
      ns("split"),
      label = strong("Train/Test split (%)"),
      min = 50, max = 90, value = 80, step = 1
    ),

    numericInput(
      ns("ntree"),
      strong("Number of Trees"),
      value = 500,
      min = 100,
      max = 2000,
      step = 50
    ),

    numericInput(
      ns("mtry"),
      strong("Variables per Split (leave blank for auto)"),
      value = NA,
      min = 1,
      step = 1
    ),

    div(
      style = "font-size: 15px; color: #6c757d; margin-top: 8px; margin-bottom: 6px;",
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

    uiOutput(ns("fileImportUserMessage")),
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"),     "Reset Values", class = "act-btn")
  )
}

RFMainPanelUI <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    suppressWarnings(tippy::use_tippy()),
    navbarPage(
      title = NULL,

      tabPanel(
        title = "Results",
        value = "model_summary_tab",
        uiOutput(ns("modelSummaryContainer"))
      ),

      tabPanel(
        title = "Plots",
        value = "plots_tab",
        uiOutput(ns("rfVarImpContainer")),
        uiOutput(ns("rfPDPContainer")),
        uiOutput(ns("rfICEContainer")),
        uiOutput(ns("rfALEContainer"))
      ),

      tabPanel(
        title = "Uploaded Data",
        value = "uploaded_data_tab",
        uiOutput(ns("uploadedDataContainer"))
      ),

      id       = ns("rfMainPanel"),
      selected = "uploaded_data_tab",
      theme    = bs_theme(version = 4)
    ),

    # Results and Plots start hidden and appear after Calculate. Hiding them here
    # (inline display:none, which is what showTab() clears) instead of with
    # hideTab() on the server means they never flash when the module starts late.
    tags$script(HTML(sprintf(
      "(function() {
         var nav = document.getElementById('%s');
         if (!nav) return;
         ['model_summary_tab', 'plots_tab'].forEach(function(v) {
           var a = nav.querySelector('a[data-value=\"' + v + '\"]');
           if (a && a.parentNode) a.parentNode.style.display = 'none';
         });
       })();",
      ns("rfMainPanel")
    )))
  )
}


# ============== SERVER ==============

RFServer <- function(id, data, shared_explanatory, shared_response) {
  moduleServer(id, function(input, output, session) {

    # ---- Reactive state ----
    summary_ready <- reactiveVal(FALSE)

    summary_ever_calculated <- reactiveVal(FALSE)

    calc_results <- reactiveVal(NULL)
    rf_message   <- reactiveVal(NULL)

    noFileCalculate <- reactiveVal(FALSE)
    responseError   <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    responseContinuous <- reactiveVal(FALSE)

    # ---- Input validation (ntree) ----
    rf_iv <- shinyvalidate::InputValidator$new()
    rf_iv$add_rule("ntree", shinyvalidate::sv_required())
    rf_iv$add_rule("ntree", shinyvalidate::sv_gte(100, message = "Must be at least 100."))
    rf_iv$add_rule("ntree", function(value) {
      if (!is.na(value) && isTruthy(value) && value > 2000) "Must be at most 2000."
    })
    rf_iv$enable()

    # The Results and Plots tabs start hidden (see RFMainPanelUI) until Calculate succeeds.

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

    # Free the forest if the session ends while results are still held.
    session$onSessionEnded(function() {
      r <- isolate(calc_results())
      if (!is.null(r) && is.environment(r$model)) {
        rm(list = ls(r$model, all.names = TRUE), envir = r$model)
        gc()
      }
    })

    # ---- Uploaded Data tab ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(data())) {
        tagList(helpText("No data yet. Upload a dataset in the Data Import tab to view it here."))
      } else if (ncol(data()) == 0) {
        tagList(helpText("The uploaded file has no columns. Upload a dataset with a header row and at least one column."))
      } else {
        DT::DTOutput(session$ns("rfUploadTable"))
      }
    })

    output$rfUploadTable <- DT::renderDT({
      req(data(), ncol(data()) > 0)
      DT::datatable(
        data(),
        options = list(
          pageLength  = 25,
          lengthMenu  = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX     = TRUE
        )
      )
    })

    # ---- Populate dropdowns after upload ----
    observeEvent(data(), {
      noFileCalculate(FALSE)
      req(data())

      tryCatch({
        df           <- data()
        cols         <- colnames(df)
        numeric_cols <- cols[vapply(df, is.numeric, logical(1))]

        shared_resp    <- shared_response()

        valid_response <- rf_response_choices(df)
        pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% valid_response) shared_resp else character(0)

        # The response is never offered as a predictor (the response observer
        # below does not run when a new file keeps the same response selected)
        predictor_cols <- setdiff(numeric_cols, pre_response)
        pre_predictors <- intersect(shared_explanatory(), predictor_cols)

        expect_echo("response",   pre_response)
        expect_echo("predictors", pre_predictors)
        updatePickerInput(session, "response",   choices = valid_response, selected = pre_response)
        updatePickerInput(session, "predictors", choices = predictor_cols, selected = pre_predictors)
      }, error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        rf_message(paste("The uploaded data could not be prepared for Random Forest:", conditionMessage(e)))
      })
    }, ignoreNULL = TRUE)

    # ---- Keep response out of predictors list (also when the response is
    # cleared, so its column becomes selectable as a predictor again and the
    # continuous-response warning goes away; a cleared response is not shared) ----
    observeEvent(input$response, {
      if (!is_echo("response", input$response) && !is.null(input$response)) shared_response(input$response)
      req(data())

      df           <- data()
      cols         <- colnames(df)
      numeric_cols <- cols[vapply(df, is.numeric, logical(1))]

      available_predictors <- setdiff(numeric_cols, input$response)
      selected_predictors  <- intersect(input$predictors, available_predictors)
      updatePickerInput(session, "predictors",
                        choices  = available_predictors,
                        selected = selected_predictors)

      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      responseContinuous(isTruthy(input$response) && ml_is_continuous_response(df[[input$response[1]]]))
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
    })

    # ---- Clear outputs when any setting changes after Calculate ----
    observeEvent(
      list(data(), input$response, input$predictors, input$split, input$ntree, input$mtry),
      {
        had_results <- isTRUE(summary_ready()) || !is.null(calc_results())

        if (isTRUE(summary_ready())) summary_ready(FALSE)

        calc_results(NULL)
        rf_message(NULL)

        # The result tabs are only visible while results exist, so there is
        # nothing to hide or navigate away from otherwise.
        if (had_results) {
          hideTab(inputId = "rfMainPanel", target = "model_summary_tab")
          hideTab(inputId = "rfMainPanel", target = "plots_tab")
          updateNavbarPage(session, "rfMainPanel", selected = "uploaded_data_tab")
        }
      },
      ignoreInit = TRUE
    )

    # ---- Tab containers (show placeholder until calculate succeeds) ----
    output$modelSummaryContainer <- renderUI({
      if (!isTRUE(summary_ready())) {
        if (!isTRUE(summary_ever_calculated())) {
          return(tagList(helpText("No results yet. Upload a dataset, choose variables, then click Calculate.")))
        }
        return(tagList(helpText("Settings changed. Click Calculate to update results.")))
      }
      uiOutput(session$ns("modelSummaryUI"))
    })


    # ---- Inline error outputs ----
    output$fileImportUserMessage <- renderUI({
      if (noFileCalculate()) {
        tags$div(class = "shiny-output-error-validation",
                 "Required: Cannot calculate without a data file.")
      } else {
        msg <- rf_message()
        if (is.null(msg)) return(NULL)
        div(
          style = "margin-top:10px;",
          div(class = "alert alert-danger", msg)
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

    # ---- Calculate ----
    show_results <- function() {
      summary_ready(TRUE)
      summary_ever_calculated(TRUE)

      showTab(inputId = "rfMainPanel", target = "model_summary_tab")
      showTab(inputId = "rfMainPanel", target = "plots_tab")

      shinyjs::delay(100, {
        updateNavbarPage(session, "rfMainPanel", selected = "model_summary_tab")
      })
    }

    rf_calculate <- function() {

      rf_message(NULL)

      # 1. Data must be loaded
      if (!isTruthy(data())) {
        noFileCalculate(TRUE)
        return()
      } else {
        noFileCalculate(FALSE)
      }

      # The response is never also used as a predictor
      predictors <- setdiff(input$predictors, input$response)

      # 2. Response variable
      if (!isTruthy(input$response)) {
        responseError(TRUE)
        shinyjs::addClass(id = "responseWrapper", class = "has-error")
      } else {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      # 3. Predictors
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

      # 4. Numeric inputs (ntree)
      if (!isTRUE(rf_iv$is_valid())) return()

      resp_col   <- input$response[1]
      df         <- data()

      # The picker values can briefly point at columns of a previously loaded file
      if (!all(c(predictors, resp_col) %in% colnames(df))) {
        rf_message("The selected variables are not in the loaded data. Please select the variables again.")
        return()
      }

      # Continuous response guard — block classification on a continuous variable
      if (ml_is_continuous_response(df[[resp_col]])) {
        responseContinuous(TRUE)
        return()
      }

      # 5. Check for missing values before fitting
      analysis_df <- df[, c(predictors, resp_col), drop = FALSE]

      na_cols <- names(which(sapply(analysis_df, function(x) any(is.na(x)))))
      if (length(na_cols) > 0) {
        rf_message(paste0(
          "The following column(s) contain missing values (NA): ",
          paste(na_cols, collapse = ", "),
          ". Please remove or impute missing values before calculating."
        ))
        return()
      }

      analysis_df <- na.omit(analysis_df)

      if (nrow(analysis_df) == 0) {
        rf_message("No complete cases remain after removing missing values.")
        return()
      }

      analysis_df[[resp_col]] <- as.factor(analysis_df[[resp_col]])
      n_classes <- nlevels(analysis_df[[resp_col]])

      # 6. At least 2 classes
      if (n_classes < 2) {
        rf_message("Response variable must have at least 2 unique classes.")
        return()
      }

      # 7. Not too many classes (same upper-limit check as CART)
      if (n_classes > floor(nrow(analysis_df) / 2)) {
        rf_message("The selected response variable has too many unique values to be treated as a categorical class variable for Random Forest.")
        return()
      }

      # 8. Zero variance check
      vars <- sapply(analysis_df[, predictors, drop = FALSE], var, na.rm = TRUE)
      zero_var_cols <- names(vars)[is.na(vars) | vars < .Machine$double.eps]
      if (length(zero_var_cols) > 0) {
        rf_message(paste0(
          "These selected variable(s) have zero variance and cannot be used in Random Forest: ",
          paste(zero_var_cols, collapse = ", "), "."
        ))
        return()
      }

      # 9. Coerce all predictor columns to canonical types before model fitting and iml.
      #    is.numeric() covers both double and integer in R, so every numeric-type column
      #    is unconditionally cast to double. Character columns become factors.
      for (col in predictors) {
        v <- analysis_df[[col]]
        if (is.numeric(v)) {
          analysis_df[[col]] <- as.double(v)
        } else if (is.character(v)) {
          analysis_df[[col]] <- as.factor(v)
        }
      }

      # 10. Resolve mtry — NA / blank means auto (NULL → randomForest uses floor(sqrt(p)))
      mtry_val <- if (!is.numeric(input$mtry) || is.na(input$mtry)) {
        NULL
      } else {
        suppressWarnings(as.integer(input$mtry))
      }

      if (!is.null(mtry_val) && is.na(mtry_val)) {
        rf_message("mtry must be a whole number between 1 and the number of selected predictors.")
        return()
      }

      if (!is.null(mtry_val) && mtry_val > length(predictors)) {
        rf_message(paste0(
          "mtry (", mtry_val, ") cannot exceed the number of selected predictors (",
          length(predictors), ")."
        ))
        return()
      }

      # Same data and settings as the results already shown: they are identical
      # (the split and the forest use a fixed seed), so reuse them. This also
      # absorbs repeated clicks on Calculate.
      ntree_val <- as.integer(input$ntree)
      calc_key  <- list(response = resp_col, predictors = predictors, split = input$split,
                        ntree = ntree_val, mtry = mtry_val)
      prev <- calc_results()
      if (!is.null(prev) && identical(prev$key, calc_key)) {
        show_results()
        return()
      }

      # 11. Train / test split
      n       <- nrow(analysis_df)
      n_train <- floor(n * (input$split / 100))

      if (n_train < 10) {
        rf_message("Training set is too small. Try increasing the split percentage or using a larger dataset.")
        return()
      }

      # 12. Size guard: the fit runs on the process shared by all sessions
      if (n_train * ntree_val > RF_MAX_TRAIN_TREES) {
        max_trees <- floor(RF_MAX_TRAIN_TREES / n_train)
        rf_message(paste0(
          "This Random Forest is too large to run here: ",
          format(n_train, big.mark = ","), " training observations x ",
          format(ntree_val, big.mark = ","), " trees (the limit is ",
          format(RF_MAX_TRAIN_TREES, big.mark = ",", scientific = FALSE),
          " observation-trees). ",
          if (max_trees >= 100) {
            paste0("With this many training observations, use at most ",
                   format(max_trees, big.mark = ","), " trees, or lower the Train/Test split.")
          } else {
            "Use fewer rows or lower the Train/Test split."
          }
        ))
        return()
      }

      # 13. Split, fit and evaluate
      fitted <- withProgress(
        message = "Random Forest", value = 0,
        rf_fit_model(
          analysis_df, resp_col, predictors, ntree_val, mtry_val, n_train,
          step = function(value, detail = NULL) setProgress(value = value, detail = detail)
        )
      )
      if (!is.null(fitted$message)) {
        rf_message(fitted$message)
        return()
      }

      res     <- fitted$result
      res$key <- calc_key
      calc_results(res)

      # Release the node arrays randomForest pre-allocates while growing the trees
      rm(fitted, analysis_df)
      invisible(gc())

      # ---- Show tabs and navigate ----
      show_results()
    }

    observeEvent(input$calculate, {
      # The model runs on the one R process shared by every session: block
      # repeat clicks until it is done.
      shinyjs::disable("calculate")
      on.exit(shinyjs::enable("calculate"), add = TRUE)

      tryCatch(
        rf_calculate(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          rf_message(paste("Random Forest could not be completed:", conditionMessage(e)))
        }
      )
    }, ignoreInit = TRUE)

    # ---- Model Summary UI ----
    output$modelSummaryUI <- renderUI({
      r <- calc_results()
      req(r)

      oob_pct        <- round(r$oob_error * 100, 2)
      acc_pct        <- round(r$accuracy  * 100, 2)
      accuracy_label <- if (r$oob_error < 0.10) "high" else if (r$oob_error <= 0.25) "moderate" else "low"
      rf_correct     <- sum(diag(r$confusion))
      rf_total       <- sum(r$confusion)

      tagList(
        tags$h4("Model Summary"),
        uiOutput(session$ns("rfSummaryTable")),
        tags$hr(),
        tags$h4("Class Distribution (Full Dataset)"),
        tableOutput(session$ns("rfClassDistTable")),
        tags$hr(),
        tags$h4("Classification Report (Test Set)"),
        tableOutput(session$ns("rfClassReport")),
        tags$script(HTML("setTimeout(function(){ if(typeof tippy!=='undefined') tippy('[data-tippy-content]'); }, 200);")),
        tags$hr(),
        tags$h4("Confusion Matrix (Test Set)"),
        tableOutput(session$ns("rfConfusionTable")),
        tags$h5(tags$strong("Accuracy Calculation"),
                style = "margin-top: 14px; margin-bottom: 2px;"),
        withMathJax(),
        tags$p(HTML(sprintf(
          "\\( \\text{Accuracy} = \\dfrac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\dfrac{%d}{%d} = %.2f\\%% \\)",
          rf_correct, rf_total, r$accuracy * 100
        ))),
        tags$hr(),
        tags$h4("Per-Class Metrics"),
        tableOutput(session$ns("rfClassMetrics")),
        tags$hr(),
        tags$div(
          style = paste(
            "background-color: #f8f9fa;",
            "border-left: 4px solid #dee2e6;",
            "border-radius: 4px;",
            "padding: 16px 20px;",
            "margin-top: 6px;"
          ),
          tags$h5(tags$strong("Interpretation of Results"),
                  style = "margin-top: 0; margin-bottom: 12px;"),
          tags$p(
            style = "margin-bottom: 8px;",
            paste0(
              "The OOB (Out-of-Bag) Error Rate is an internal estimate of prediction error ",
              "computed using observations not used in building individual trees. ",
              "An OOB error of ", oob_pct, "% indicates ", accuracy_label,
              " classification accuracy."
            )
          ),
          tags$p(
            style = "margin-bottom: 0;",
            paste0(
              "The model correctly classified ", acc_pct,
              "% of observations in the test set."
            )
          )
        )
      )
    })

    output$rfSummaryTable <- renderUI({
      r <- calc_results()
      req(r)

      tip <- function(label, tooltip) {
        tags$span(
          `data-tippy-content` = tooltip,
          style = "cursor: help; border-bottom: 1px dotted #555;",
          label
        )
      }

      rows <- list(
        list("Type",            "Classification"),
        list(
          tip("Number of Trees",
              "Total trees built in the forest. More trees improve stability but increase computation time."),
          as.character(r$ntree)
        ),
        list(
          tip("Variables per Split (mtry)",
              "Number of variables randomly considered at each tree split. Controls model diversity."),
          as.character(r$mtry_used)
        ),
        list("Number of Predictors",   as.character(length(r$predictors))),
        list("Total Observations",     as.character(r$n_total)),
        list("Training Observations",  as.character(r$n_train)),
        list("Test Observations",      as.character(r$n_test)),
        list(
          tip("OOB Error Rate",
              "Out-of-Bag error rate. An internal accuracy estimate using data not seen during training. Lower is better."),
          paste0(round(r$oob_error * 100, 2), "%")
        )
      )

      tagList(
        tags$table(
          class = "table table-sm table-bordered table-striped",
          style = "max-width: 480px;",
          tags$thead(
            tags$tr(
              tags$th("Item",  style = "width: 60%;"),
              tags$th("Value")
            )
          ),
          tags$tbody(
            lapply(rows, function(row) {
              tags$tr(tags$td(row[[1]]), tags$td(row[[2]]))
            })
          )
        ),
        tags$script("if (typeof tippy !== 'undefined') tippy('[data-tippy-content]');")
      )
    })

    output$rfClassDistTable <- renderTable({
      r <- calc_results()
      req(r)
      r$class_dist
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$rfClassReport <- renderTable({
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

    output$rfConfusionTable <- renderTable({
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

    output$rfClassMetrics <- renderTable({
      r <- calc_results()
      req(r)
      r$class_metrics
    }, rownames = FALSE, striped = TRUE, bordered = TRUE)

    output$rfVarImpContainer <- renderUI({
      req(calc_results())

      tagList(
        tags$h4("Variable Importance Plots", style = "margin-top: 10px;"),
        plotOutput(session$ns("rfVarImpPlot"), height = "550px"),
        tags$div(
          style = "margin-top: 12px; font-size: 14px; color: #444;",
          tags$p(
            tags$strong("Mean Decrease in Accuracy: "),
            "Measures how much the model's accuracy drops when the values of a variable are randomly shuffled. ",
            "A large drop means the model relied heavily on that variable — it is highly important."
          ),
          tags$p(
            tags$strong("Mean Decrease in Gini: "),
            "Measures how much each variable contributes to reducing impurity (disorder) across all splits in all trees. ",
            "A higher value means the variable is consistently useful for separating the classes."
          )
        )
      )
    })

    output$rfVarImpPlot <- renderPlot({
      r <- calc_results()
      req(r)

      imp_acc  <- r$importance$acc
      imp_gini <- r$importance$gini

      df_acc <- data.frame(
        Variable = rownames(imp_acc),
        Value    = imp_acc[, 1],
        stringsAsFactors = FALSE
      )
      df_acc <- df_acc[order(df_acc$Value, decreasing = FALSE), ]

      df_gini <- data.frame(
        Variable = rownames(imp_gini),
        Value    = imp_gini[, 1],
        stringsAsFactors = FALSE
      )
      df_gini <- df_gini[order(df_gini$Value, decreasing = FALSE), ]

      # Labels and margin for the two panels: see ml_importance_labels() in decisionTrees.R
      lab         <- ml_importance_labels(c(df_acc$Variable, df_gini$Variable), panels = 2)
      acc_labels  <- lab$labels[seq_len(nrow(df_acc))]
      gini_labels <- lab$labels[nrow(df_acc) + seq_len(nrow(df_gini))]

      par(
        mfrow = c(1, 2),
        oma   = c(0, 0, 0, 0),
        mar   = c(5, lab$left_mar, 4, 2)
      )

      barplot(
        df_acc$Value,
        names.arg = acc_labels,
        horiz     = TRUE,
        las       = 1,
        col       = "#4472C4",
        main      = "Mean Decrease in Accuracy",
        xlab      = "Mean Decrease in Accuracy",
        cex.main  = 1.3,
        font.main = 2,
        cex.lab   = 1.1,
        font.lab  = 2,
        cex.names = 0.95
      )

      barplot(
        df_gini$Value,
        names.arg = gini_labels,
        horiz     = TRUE,
        las       = 1,
        col       = "#ED7D31",
        main      = "Mean Decrease in Gini Impurity",
        xlab      = "Mean Decrease in Gini",
        cex.main  = 1.3,
        font.main = 2,
        cex.lab   = 1.1,
        font.lab  = 2,
        cex.names = 0.95
      )
    })

    # ---- Effect plots (PDP, ICE, ALE) ----
    # Computed once per Calculate, the first time one of the effect plots is
    # needed (when the Plots tab is opened), on a row sample when the training
    # set is large. The forest is released once they are computed.
    rf_effects <- reactive({
      r <- calc_results()
      req(r)
      m <- r$model

      if (!isTRUE(m$effects_done)) {
        preds <- r$predictors
        m$effects <- withProgress(
          message = "Computing PDP, ICE and ALE plots", value = 0,
          rf_compute_effects(
            m$fit, m$X, m$y, preds,
            step = function(i, pvar) {
              setProgress(
                value  = (i - 1) / length(preds),
                detail = paste0("Variable ", i, " of ", length(preds), ": ", pvar)
              )
            }
          )
        )
        m$effects_done <- TRUE
        m$fit <- NULL
        m$X   <- NULL
        m$y   <- NULL
        gc()
      }
      m$effects
    })

    # Note under each effect plot when it was computed on a sample of the training rows
    rf_effect_note <- function(r) {
      if (is.null(r$effect_n) || r$effect_n >= r$n_train) return(NULL)
      tags$p(
        style = "margin-top: 6px; font-size: 13px; color: #6c757d;",
        paste0(
          "Computed on a random sample of ", format(r$effect_n, big.mark = ","),
          " of the ", format(r$n_train, big.mark = ","), " training observations."
        )
      )
    }

    # Dynamic container: sets height based on predictor count then renders plotOutput
    output$rfPDPContainer <- renderUI({
      r <- calc_results()
      req(r, !is.null(rf_effects()))

      n_pred    <- length(r$predictors)
      n_cols    <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
      n_rows    <- ceiling(n_pred / n_cols)
      height_px <- max(380, n_rows * 380)

      tagList(
        tags$hr(),
        tags$h4("Partial Dependence Plots", style = "margin-top: 10px;"),
        plotOutput(session$ns("rfPDPPlot"), height = paste0(height_px, "px")),
        rf_effect_note(r)
      )
    })

    output$rfPDPPlot <- renderPlot({
      r   <- calc_results()
      req(r)
      eff <- rf_effects()
      req(!is.null(eff))

      n_pred  <- length(r$predictors)
      n_cols  <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
      n_rows  <- ceiling(n_pred / n_cols)

      cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")

      par(
        mfrow = c(n_rows, n_cols),
        oma   = c(0, 0, 0, 0),
        mar   = c(8, 4, 5, 2)
      )

      for (pred_var in r$predictors) {
        pdp_df <- eff$pdp[[pred_var]]

        if (is.null(pdp_df)) {
          plot.new()
          title(main = pred_var, cex.main = 1.1, font.main = 2)
          text(0.5, 0.5, "Could not compute PDP", cex = 0.9)
          next
        }

        cls_list <- as.character(unique(pdp_df$.class))

        plot(
          x    = NULL,
          xlim = range(pdp_df[[pred_var]], na.rm = TRUE),
          ylim = c(0, 1),
          main = pred_var,
          xlab = pred_var,
          ylab = "Predicted Probability",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.axis  = 1.0,
          bty       = "l"
        )

        for (j in seq_along(cls_list)) {
          cls_df <- pdp_df[as.character(pdp_df$.class) == cls_list[j], ]
          cls_df <- cls_df[order(cls_df[[pred_var]]), ]
          lines(cls_df[[pred_var]], cls_df$.value,
                col = cls_cols[j], lwd = 2)
        }

        # Below the x-axis label, not just at the bottom of the plot area
        legend("bottom", legend = cls_list,
               col = cls_cols[seq_along(cls_list)],
               lwd = 2, bty = "n", cex = 0.8, horiz = TRUE,
               xpd = TRUE, inset = c(0, -0.4))

        mtext("Shows the marginal effect of this variable on the predicted outcome,",
              side = 3, line = 0.9, cex = 0.65, col = "#555555")
        mtext("holding all other variables constant",
              side = 3, line = 0.2, cex = 0.65, col = "#555555")
      }

      # blank out remaining grid cells for odd predictor counts
      remaining <- n_rows * n_cols - n_pred
      for (k in seq_len(remaining)) plot.new()
    })

    output$rfICEContainer <- renderUI({
      r <- calc_results()
      req(r, !is.null(rf_effects()))

      n_pred    <- length(r$predictors)
      n_cols    <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
      n_rows    <- ceiling(n_pred / n_cols)
      height_px <- max(380, n_rows * 380)

      tagList(
        tags$hr(),
        tags$h4("Individual Conditional Expectation Plots", style = "margin-top: 10px;"),
        plotOutput(session$ns("rfICEPlot"), height = paste0(height_px, "px")),
        rf_effect_note(r)
      )
    })

    output$rfICEPlot <- renderPlot({
      r   <- calc_results()
      req(r)
      eff <- rf_effects()
      req(!is.null(eff))

      n_pred  <- length(r$predictors)
      n_cols  <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
      n_rows  <- ceiling(n_pred / n_cols)

      cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")

      par(
        mfrow = c(n_rows, n_cols),
        oma   = c(0, 0, 0, 0),
        mar   = c(8, 4, 5, 2)
      )

      for (pred_var in r$predictors) {
        ice_df <- eff$ice[[pred_var]]

        if (is.null(ice_df)) {
          plot.new()
          title(main = pred_var, cex.main = 1.1, font.main = 2)
          text(0.5, 0.5, "Could not compute ICE", cex = 0.9)
          next
        }

        cls_list <- as.character(unique(ice_df$.class))

        plot(
          x    = NULL,
          xlim = range(ice_df[[pred_var]], na.rm = TRUE),
          ylim = c(0, 1),
          main = pred_var,
          xlab = pred_var,
          ylab = "Predicted Probability",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.axis  = 1.0,
          bty       = "l"
        )

        for (j in seq_along(cls_list)) {
          cls_ice <- ice_df[as.character(ice_df$.class) == cls_list[j], ]
          curves  <- rf_ice_matrix(cls_ice, pred_var)

          matlines(curves$x, curves$y,
                   col = adjustcolor(cls_cols[j], alpha.f = 0.2),
                   lty = 1, lwd = 0.7)
        }

        legend("bottom", legend = cls_list,
               col = cls_cols[seq_along(cls_list)],
               lwd = 2, bty = "n", cex = 0.8, horiz = TRUE,
               xpd = TRUE, inset = c(0, -0.4))

        mtext("Shows how the prediction changes for each individual observation as this variable changes.",
              side = 3, line = 0.9, cex = 0.65, col = "#555555")
        mtext("Each line represents one observation.",
              side = 3, line = 0.2, cex = 0.65, col = "#555555")
      }

      remaining <- n_rows * n_cols - n_pred
      for (k in seq_len(remaining)) plot.new()
    })

    output$rfALEContainer <- renderUI({
      r <- calc_results()
      req(r, !is.null(rf_effects()))

      n_pred    <- length(r$predictors)
      n_cols    <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
      n_rows    <- ceiling(n_pred / n_cols)
      height_px <- max(380, n_rows * 380)

      tagList(
        tags$hr(),
        tags$h4("Accumulated Local Effects Plots", style = "margin-top: 10px;"),
        plotOutput(session$ns("rfALEPlot"), height = paste0(height_px, "px")),
        rf_effect_note(r)
      )
    })

    output$rfALEPlot <- renderPlot({
      r   <- calc_results()
      req(r)
      eff <- rf_effects()
      req(!is.null(eff))

      n_pred  <- length(r$predictors)
      n_cols  <- if (n_pred <= 1) 1 else if (n_pred <= 4) 2 else 3
      n_rows  <- ceiling(n_pred / n_cols)

      cls_cols <- c("#4472C4", "#ED7D31", "#70AD47", "#9E480E", "#7030A0")

      par(
        mfrow = c(n_rows, n_cols),
        oma   = c(0, 0, 0, 0),
        mar   = c(8, 4, 5, 2)
      )

      for (pred_var in r$predictors) {
        ale_df <- eff$ale[[pred_var]]

        if (is.null(ale_df)) {
          plot.new()
          title(main = pred_var, cex.main = 1.1, font.main = 2)
          text(0.5, 0.5, "Could not compute ALE", cex = 0.9)
          next
        }

        cls_list <- as.character(unique(ale_df$.class))
        y_range  <- range(ale_df$.value, na.rm = TRUE)

        plot(
          x    = NULL,
          xlim = range(ale_df[[pred_var]], na.rm = TRUE),
          ylim = y_range,
          main = pred_var,
          xlab = pred_var,
          ylab = "ALE",
          cex.main  = 1.3,
          font.main = 2,
          cex.lab   = 1.1,
          font.lab  = 2,
          cex.axis  = 1.0,
          bty       = "l"
        )

        abline(h = 0, col = "grey70", lty = 2, lwd = 1)

        for (j in seq_along(cls_list)) {
          cls_df <- ale_df[as.character(ale_df$.class) == cls_list[j], ]
          cls_df <- cls_df[order(cls_df[[pred_var]]), ]
          lines(cls_df[[pred_var]], cls_df$.value,
                col = cls_cols[j], lwd = 2)
        }

        legend("bottom", legend = cls_list,
               col = cls_cols[seq_along(cls_list)],
               lwd = 2, bty = "n", cex = 0.8, horiz = TRUE,
               xpd = TRUE, inset = c(0, -0.4))

        mtext("Shows the accumulated local effect of this variable on the predicted outcome.",
              side = 3, line = 0.9, cex = 0.65, col = "#555555")
        mtext("More reliable than PDP when predictors are correlated.",
              side = 3, line = 0.2, cex = 0.65, col = "#555555")
      }

      remaining <- n_rows * n_cols - n_pred
      for (k in seq_len(remaining)) plot.new()
    })

    # ---- Reset ----
    observeEvent(input$reset, {
      hideTab(inputId = "rfMainPanel", target = "model_summary_tab")
      hideTab(inputId = "rfMainPanel", target = "plots_tab")

      summary_ready(FALSE)
      summary_ever_calculated(FALSE)

      noFileCalculate(FALSE)
      responseError(FALSE)
      predictorsError(FALSE)
      responseContinuous(FALSE)
      rf_message(NULL)
      calc_results(NULL)

      shinyjs::removeClass(id = "responseWrapper",   class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")

      updatePickerInput(session, "response",   selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNumericInput(session, "ntree", value = 500)
      updateNumericInput(session, "mtry",  value = NA)
      updateSliderInput(session,  "split", value = 80)

      updateNavbarPage(session, "rfMainPanel", selected = "uploaded_data_tab")
    })

  })
}
