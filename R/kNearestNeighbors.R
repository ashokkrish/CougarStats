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
      uiOutput(ns("responseError"))
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
    )
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

    
    # ---- Validation (k required, must be > 0) ----
    knn_iv <- shinyvalidate::InputValidator$new()
    
    knn_iv$add_rule("k", shinyvalidate::sv_required())
    knn_iv$add_rule("k", shinyvalidate::sv_gt(0, message = "Must be greater than 0."))
    
    knn_iv$enable()
    
    results_ready <- reactiveVal(FALSE)
    calc_settings <- reactiveVal(NULL)
    
    plots_ready <- reactiveVal(FALSE)
    plot_settings <- reactiveVal(NULL)
    
    results_ever_calculated <- reactiveVal(FALSE)
    plots_ever_calculated <- reactiveVal(FALSE)
    
    session$onFlushed(function() {
      hideTab(inputId = "knnMainPanel", target = "results_tab")
      hideTab(inputId = "knnMainPanel", target = "plots_tab")
    }, once = TRUE)
    
    responseError <- reactiveVal(FALSE)
    predictorsError <- reactiveVal(FALSE)
    
    fileImportError <- reactiveVal(FALSE)
    knn_message     <- reactiveVal(NULL)

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
        if (isTRUE(results_ready())) {
          results_ready(FALSE)
          calc_settings(NULL)
        }
        
        if (isTRUE(plots_ready())) {
          plots_ready(FALSE)
          plot_settings(NULL)
        }
        
        hideTab(inputId = "knnMainPanel", target = "results_tab")
        hideTab(inputId = "knnMainPanel", target = "plots_tab")
        updateNavbarPage(session, "knnMainPanel", selected = "uploaded_data_tab")
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
            plotOutput(session$ns("knnPlotBoundary"), height = "500px")
          ),

          tags$hr(),

          tags$h4("Accuracy vs k"),
          plotOutput(session$ns("knnPlotAccVsK"), height = "400px")
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
    
    
    observeEvent(data(), {
      req(data())

      fileImportError(FALSE)

      df0 <- data()
      cols <- colnames(df0)
      numeric_cols <- cols[sapply(df0, is.numeric)]

      pre_predictors <- intersect(shared_explanatory(), numeric_cols)
      shared_resp    <- shared_response()
      pre_response   <- if (isTruthy(shared_resp) && shared_resp %in% cols) shared_resp else character(0)

      updatePickerInput(session, "response",   choices = cols,         selected = pre_response)
      updatePickerInput(session, "predictors", choices = numeric_cols, selected = pre_predictors)

      if (isTruthy(input$split)) {
        n <- nrow(df0)
        n_train <- floor(n * (input$split / 100))
        default_k <- max(1, round(sqrt(n_train)))
        updateNumericInput(session, "k", value = default_k)
      }
    }, ignoreNULL = TRUE)
    #reacts to changing from regression to classification or vice versa (x & y drop downs) OR new data
    observeEvent(list(input$task, data()), {
      req(data(), input$task)
      df0 <- data()
      cols <- colnames(df0)
      numeric_cols <- cols[sapply(df0, is.numeric)]

      shared_resp <- shared_response()

      if (input$task == "Regression") {
        pre_response <- if (isTruthy(shared_resp) && shared_resp %in% numeric_cols) shared_resp else character(0)
        updatePickerInput(session, "response", choices = numeric_cols, selected = pre_response)
      } else {
        pre_response <- if (isTruthy(shared_resp) && shared_resp %in% cols) shared_resp else character(0)
        updatePickerInput(session, "response", choices = cols, selected = pre_response)
      }
    })
    
    #reacts to train/test split changes by recalculating a default k based on training size
    observeEvent(input$split, {
      req(data())
      
      n <- nrow(data())
      n_train <- floor(n * (input$split / 100))
      default_k <- max(1, round(sqrt(n_train)))
      
      updateNumericInput(session, "k", value = default_k)
    })
    
    observeEvent(input$response, {
      shared_response(input$response)
      if (isTruthy(input$response)) {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }

      if (isTruthy(data())) {
        df <- data()
        cols <- colnames(df)
        numeric_cols <- cols[sapply(df, is.numeric)]
        available_predictors <- setdiff(numeric_cols, input$response)
        selected_predictors  <- intersect(input$predictors, available_predictors)
        updatePickerInput(session, "predictors", choices = available_predictors, selected = selected_predictors)
      }
    })

    observeEvent(input$predictors, {
      shared_explanatory(input$predictors)
      if (length(input$predictors) >= 1) {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
    })
    
    #reset button
    observeEvent(input$reset, {
      
      hideTab(inputId = "knnMainPanel", target = "results_tab")
      hideTab(inputId = "knnMainPanel", target = "plots_tab")
      hideTab(inputId = "knnMainPanel", target = "uploaded_data_tab")
      
      results_ready(FALSE)
      plots_ready(FALSE)
      
      results_ever_calculated(FALSE)
      plots_ever_calculated(FALSE)
      
      calc_settings(NULL)
      plot_settings(NULL)
      
      responseError(FALSE)
      predictorsError(FALSE)
      fileImportError(FALSE)
      knn_message(NULL)

      shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      
      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNavbarPage(session, "knnMainPanel", selected = "uploaded_data_tab")
    })


    observeEvent(input$calculate, {
      # validation
      if (is.null(data()) || NROW(data()) == 0) {
        fileImportError(TRUE)
        return()
      }
      
      fileImportError(FALSE)
      if (!isTruthy(input$response)) {
        responseError(TRUE)
        shinyjs::addClass(id = "responseWrapper", class = "has-error")
      } else {
        responseError(FALSE)
        shinyjs::removeClass(id = "responseWrapper", class = "has-error")
      }
      
      if (!isTruthy(input$predictors) || length(input$predictors) < 1) {
        predictorsError(TRUE)
        shinyjs::addClass(id = "predictorsWrapper", class = "has-error")
      } else {
        predictorsError(FALSE)
        shinyjs::removeClass(id = "predictorsWrapper", class = "has-error")
      }
      
      if (!isTruthy(input$response) || !isTruthy(input$predictors) || length(input$predictors) < 1) {
        return()
      }
      
      req(knn_iv$is_valid())
      resp_col <- input$response[1]

      #split dataset for training & testing
      df <- data()

      # Zero variance check
      sds <- sapply(df[, input$predictors, drop = FALSE], sd, na.rm = TRUE)
      zero_var_cols <- names(sds)[is.na(sds) | sds == 0]
      if (length(zero_var_cols) > 0) {
        knn_message(paste0(
          "These selected variable(s) have zero variance and cannot be used in KNN: ",
          paste(zero_var_cols, collapse = ", "), "."
        ))
        return()
      }
      knn_message(NULL)

      set.seed(123)
      
      n <- nrow(df)
      n_train <- floor(n * (input$split / 100))
      train_idx <- sample(seq_len(n), size = n_train)
      
      train <- df[train_idx, , drop = FALSE]
      test  <- df[-train_idx, , drop = FALSE]
      
      # predictors matrix
      X_train <- train[, input$predictors, drop = FALSE]
      X_test  <- test[, input$predictors, drop = FALSE]
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
      
      k <- as.integer(input$k) #Ensures k is an integer
      
      #CLASSIFICATION vs REGRESSION
      if (is.null(input$task) || input$task == "Classification") {
        
        #Pulls the response column that the user selected & turns it into a factor
        y_train <- as.factor(train[[resp_col]])
        y_test  <- as.factor(test[[resp_col]])
        
        #Run kNN classification algorithm
        pred <- class::knn(
          train = X_train,
          test  = X_test,
          cl    = y_train,
          k     = k
        )
        
        #evaluation metrics 
        metrics <- knn_classification_report(actual = y_test, predicted = pred)

        class_dist_df <- as.data.frame(table(as.factor(df[[resp_col]])))
        colnames(class_dist_df) <- c("Class", "Count")

        # nearest odd integer of sqrt(n_train) — recommended starting k
        k_round_val   <- round(sqrt(n_train))
        k_recommended <- if (k_round_val %% 2 == 0) k_round_val + 1L else as.integer(k_round_val)

        # to keep train/split static until user clicks calculate
        calc_settings(list(
          split         = input$split,
          k             = k,
          n_total       = n,
          n_train       = n_train,
          k_recommended = k_recommended
        ))


        #Results UI layout
        output$resultsUI <- renderUI({
          s       <- calc_settings()
          req(s)
          correct <- sum(diag(metrics$cm))
          total   <- sum(metrics$cm)

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
              " &asymp; <strong>", s$k_recommended, "</strong>"
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
              tags$p(sprintf(
                "\\[ \\text{Accuracy} = \\frac{\\text{Correct Predictions}}{\\text{Total Observations}} = \\frac{%d}{%d} = %.2f\\%% \\]",
                correct, total, metrics$accuracy * 100
              ))
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
              sprintf("%.4f", metrics$accuracy)
            ),
            check.names = FALSE
          )
        }, rownames = FALSE, striped = TRUE, bordered = TRUE)

        output$classDist    <- renderTable({ class_dist_df }, rownames = FALSE, striped = TRUE, bordered = TRUE)
        output$classReport <- renderTable({
          metrics$report
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
          cm <- as.data.frame.matrix(metrics$cm)
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
        
        boundary_vars <- if (isTRUE(input$showBoundary) &&
                             length(input$boundaryVars) == 2) input$boundaryVars else NULL

        plot_settings(list(
          response     = resp_col,
          predictors   = input$predictors,
          X_train      = X_train,
          X_test       = X_test,
          y_train      = y_train,
          y_test       = y_test,
          k            = k,
          showBoundary = isTRUE(input$showBoundary),
          boundaryVars = boundary_vars
        ))
        
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

          x1_seq <- seq(x1_rng[1] - pad * diff(x1_rng),
                        x1_rng[2] + pad * diff(x1_rng), length.out = 80)
          x2_seq <- seq(x2_rng[1] - pad * diff(x2_rng),
                        x2_rng[2] + pad * diff(x2_rng), length.out = 80)

          grid_mat           <- as.matrix(expand.grid(x1_seq, x2_seq))
          colnames(grid_mat) <- c(p1, p2)

          grid_pred <- suppressWarnings(class::knn(
            train = X_tr, test = grid_mat, cl = s$y_train, k = s$k
          ))

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
            "topright",
            legend = classes,
            col    = cls_cols[seq_along(classes)],
            pch    = 19,
            pt.cex = 0.9,
            bty    = "n",
            cex    = 0.95,
            title  = "Class"
          )
        })

        output$knnPlotAccVsK <- renderPlot({
          s <- plot_settings()
          req(s)

          max_k  <- max(s$k + 5, min(50, nrow(s$X_train) - 1))
          k_vals <- seq_len(max_k)

          acc_vals <- sapply(k_vals, function(ki) {
            pred <- suppressWarnings(class::knn(
              train = s$X_train, test = s$X_test, cl = s$y_train, k = ki
            ))
            mean(pred == s$y_test)
          })

          par(mar = c(5, 4, 4, 2))

          plot(
            k_vals, acc_vals,
            type      = "l",
            col       = "#4472C4",
            lwd       = 2,
            main      = "Accuracy vs k",
            xlab      = "k (Number of Neighbors)",
            ylab      = "Test Set Accuracy",
            ylim      = c(max(0, min(acc_vals) - 0.05), min(1, max(acc_vals) + 0.05)),
            cex.main  = 1.3,
            font.main = 2,
            cex.lab   = 1.1,
            font.lab  = 2,
            cex.axis  = 1.0,
            bty       = "l"
          )

          points(k_vals, acc_vals, pch = 19, col = "#4472C4", cex = 0.6)

          if (s$k <= max_k) {
            points(s$k, acc_vals[s$k], col = "#ED7D31", pch = 19, cex = 1.8)
            abline(v = s$k, col = "#ED7D31", lty = 2, lwd = 1.5)
          }

          legend(
            "topright",
            legend = c("Accuracy", paste0("Selected k = ", s$k)),
            col    = c("#4472C4", "#ED7D31"),
            lty    = c(1, 2),
            pch    = c(19, 19),
            pt.cex = c(0.6, 1.5),
            lwd    = c(2, 1.5),
            bty    = "n",
            cex    = 0.95
          )
        })

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
      
      #else for regression
    }, ignoreInit = TRUE)
    
  })
}


