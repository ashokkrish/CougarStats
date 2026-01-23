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
  
  list(cm = as.data.frame(cm), report = report, accuracy = accuracy)
}


KNNSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
      ns("task"),
      strong("Task"),
      choices = c("Classification"),
      selected = "Classification"
    ),
    
    # Let user choose any k
    sliderInput(
      ns("split"),
      label = strong("Train/Test split (%)"),
      min = 50, max = 90, value = 80, step = 1
    ),
    
    numericInput(
      ns("k"),
      label = strong("Number of Neighbors (k)"),
      value = 10,
      min = 1,
      step = 1
    ),
    
    checkboxInput(ns("standardize"), "Standardize predictors", value = TRUE),
    
    # Response + predictors
    pickerInput(
      ns("response"),
      strong("Response Variable (\\( y\\))"),
      choices = NULL,
      multiple = FALSE,
      options = list(`live-search` = TRUE, title = "Nothing selected")
    ),
    
    pickerInput(
      ns("predictors"),
      strong("Explanatory Variables (x₁, x₂, ..., xₖ)"),
      choices = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Nothing selected")
    ),
    
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

KNNMainPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    navbarPage(
      title = NULL,
      
      tabPanel(
        title = "Data Import",
        value = "data_import_tab",
        div(id = ns("importContainer")),
        uiOutput(ns("fileImportUserMessage")),
        import_file_ui(
          id = ns("dataImport"),
          title = ""
        )
      ),
      
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
      theme = bs_theme(version = 4)
    )
  )
}

KNNServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    uploadedTibble <- import_file_server(
      id = "dataImport",
      trigger_return = "change",
      btn_show_data = FALSE,
      return_class = "tbl_df"
    )
    
    # ---- Plots Tab Before Upload ----
    output$plotsContainer <- renderUI({
      df <- uploadedTibble$data()
      
      if (is.null(df)) {
        return(tagList(
          helpText("No plots yet. Upload a dataset in the Data Import tab.")
        ))
      }
      
      if (!isTruthy(input$response) || !isTruthy(input$predictors) || length(input$predictors) < 1) {
        return(tagList(
          helpText("To view plots, select a Response Variable and at least one Explanatory Variable, then return to this tab.")
        ))
      }
      
      tagList(
        tags$h4("Class Distribution"),
        plotOutput(session$ns("knnPlotClass"), height = "350px"),
        
        tags$hr(),
        
        tags$h4("Boxplots by Class"),
        plotOutput(session$ns("knnPlotBox"), height = "600px"),
        
        tags$hr(),
        
        tags$h4("Density Plots by Class"),
        plotOutput(session$ns("knnPlotDensity"), height = "600px")
      )
    })
    
    # ---- Uploaded Data container (message until dataset exists) ----
    output$uploadedDataContainer <- renderUI({
      if (is.null(uploadedTibble$data())) {
        tagList(
          helpText("No data yet. Upload a dataset in the Data Import tab to view it here.")
        )
      } else {
        DT::DTOutput(session$ns("knnUploadTable"))
      }
    })
    
    # ---- Uploaded Data Table ----
    output$knnUploadTable <- DT::renderDT({
      req(uploadedTibble$data())
      df <- uploadedTibble$data()
      
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
      req(uploadedTibble$data())
      df <- uploadedTibble$data()
      
      req(isTruthy(input$response))
      req(isTruthy(input$predictors))
      req(length(input$predictors) >= 1)
      
      x <- as.data.frame(df[, input$predictors, drop = FALSE])
      y <- as.factor(df[[input$response]])
      
      list(x = x, y = y)
    })
    
    # 1) Class distribution (plot(y))
    output$knnPlotClass <- renderPlot({
      pd <- plot_data()
      y <- pd$y
      
      validate(
        need(nlevels(y) >= 2, "Choose a categorical response variable to plot class distribution.")
      )
      
      plot(y, main = "Class Distribution", xlab = "Class", ylab = "Frequency")
    })
    
    # 2) Box plots
    output$knnPlotBox <- renderPlot({
      pd <- plot_data()
      
      validate(
        need(nlevels(pd$y) >= 2, "Choose a categorical response variable for classification plots.")
      )
      
      p <- caret::featurePlot(x = pd$x, y = pd$y, plot = "box")
      
      grid::grid.newpage()
      print(p)
    }, res = 96)
    
    # 3) Density plots (free scales)
    output$knnPlotDensity <- renderPlot({
      pd <- plot_data()
      
      validate(
        need(nlevels(pd$y) >= 2, "Choose a categorical response variable for classification plots.")
      )
      
      scales <- list(x = list(relation = "free"), y = list(relation = "free"))
      p <- caret::featurePlot(x = pd$x, y = pd$y, plot = "density", scales = scales)
      
      grid::grid.newpage()
      print(p)
    }, res = 96)
    
    # ---- Validation (k required, must be > 0) ----
    knn_iv <- shinyvalidate::InputValidator$new()
    
    knn_iv$add_rule("k", shinyvalidate::sv_required())
    knn_iv$add_rule("k", shinyvalidate::sv_gt(0, message = "Must be greater than 0."))
    
    knn_iv$enable()
    
    results_ready <- reactiveVal(FALSE)
    calc_settings <- reactiveVal(NULL)
    
    output$resultsContainer <- renderUI({
      if (!isTRUE(results_ready())) {
        tagList(
          helpText("No results yet. Upload a dataset, choose variables, then click Calculate.")
        )
      } else {
        uiOutput(session$ns("resultsUI"))
      }
    })
    
    
    observeEvent(uploadedTibble$data(), {
      req(uploadedTibble$data())  #Wait until data actually exists
      
      df0 <- uploadedTibble$data()
      cols <- colnames(df0) #Store the dataset and get column names
      
      numeric_cols <- cols[sapply(df0, is.numeric)] #look for numeric cols only
      
      #Populate the dropdowns
      updatePickerInput(session, "response", choices = cols, selected = character(0))
      updatePickerInput(session, "predictors", choices = numeric_cols, selected = character(0))
      
      #Auto-calculate a default k based on dataset size
      n <- nrow(df0)
      n_train <- floor(n * (input$split / 100))
      default_k <- max(1, round(sqrt(n_train)))
      updateNumericInput(session, "k", value = default_k)
    })
    #reacts to changing from regression to classification or vice versa (x & y drop downs) OR new data
    observeEvent(list(input$task, uploadedTibble$data()), {
      req(uploadedTibble$data())
      df0 <- uploadedTibble$data()
      cols <- colnames(df0)
      numeric_cols <- cols[sapply(df0, is.numeric)]
      
      if (input$task == "Regression") {
        updatePickerInput(session, "response", choices = numeric_cols, selected = character(0))
      } else {
        updatePickerInput(session, "response", choices = cols, selected = character(0))
      }
    })

    #reacts to train/test split changes by recalculating a default k based on training size
    observeEvent(input$split, {
      req(uploadedTibble$data())
      
      n <- nrow(uploadedTibble$data())
      n_train <- floor(n * (input$split / 100))
      default_k <- max(1, round(sqrt(n_train)))
      
      updateNumericInput(session, "k", value = default_k)
    })
    
    #reset button
    observeEvent(input$reset, {
      results_ready(FALSE)
      updatePickerInput(session, "response", selected = character(0))
      updatePickerInput(session, "predictors", selected = character(0))
      updateNavbarPage(session, "knnMainPanel", selected = "data_import_tab")
    })
    
    
    observeEvent(input$calculate, {
      #validation
      req(uploadedTibble$data())
      req(isTruthy(input$response))
      req(isTruthy(input$predictors))
      req(length(input$predictors) >= 1)
      req(knn_iv$is_valid())
      
      #split dataset for training & testing
      df <- uploadedTibble$data()
      
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
      if (input$task == "Classification") {
        
        #Pulls the response column that the user selected & turns it into a factor
        y_train <- as.factor(train[[input$response]])
        y_test  <- as.factor(test[[input$response]])
        
        #Run kNN classification algorithm
        pred <- class::knn(
          train = X_train,
          test  = X_test,
          cl    = y_train,
          k     = k
        )
       
        #evaluation metrics 
        metrics <- knn_classification_report(actual = y_test, predicted = pred)
        
        # to keep train/split static until user clicks calculate
        calc_settings(list(
          split = input$split,
          k = k,
          n_total = n,
          n_train = n_train
        ))
        
        
        #Results UI layout
        output$resultsUI <- renderUI({
          s <- calc_settings()
          req(s)
          
          split_prop <- s$split / 100
          k_calc <- sqrt(s$n_train)
          
          tagList(
            shiny::withMathJax(),
            
            tags$p(tags$strong("Task: "), input$task),
            
            tags$p(tags$strong("n = "), s$n_total),
            
            tags$p(tags$strong("n × train split = "),
                   paste0(s$n_total, " × ", sprintf("%.2f", split_prop), " = ", s$n_train)),
            
            tags$p(tags$strong("k calculation: "),
                   sprintf("\\( k = \\sqrt{n_{train}} = \\sqrt{%s} = %.4f \\Rightarrow %s \\)",
                           s$n_train, k_calc, s$k)),
            
            tags$p(tags$strong("k = "), s$k),
            tags$p(tags$strong("Train split = "), paste0(s$split, "%")),
            tags$p(tags$strong("Accuracy = "), sprintf("%.4f", metrics$accuracy)),
            
            tags$h4("Classification Report"),
            tableOutput(session$ns("classReport")),
            
            tags$h4("Confusion Matrix"),
            tableOutput(session$ns("confMat"))
          )
        })
        
        
        
        results_ready(TRUE) #show the output UI now
        updateNavbarPage(session, "knnMainPanel", selected = "results_tab") #switch user to Results tab
        
        #send the data into the UI
        output$classReport  <- renderTable({ metrics$report }, striped = TRUE, bordered = TRUE)
        output$confMat      <- renderTable({ metrics$cm }, striped = TRUE, bordered = TRUE)

      }
      
      #else for regression

    }, ignoreInit = TRUE)
    
  })
}
