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
      choices = c("Classification", "Regression"),
      selected = "Classification"
    ),
    
    # Let user choose any k
    numericInput(
      ns("k"),
      label = strong("k (number of neighbors)"),
      value = 10,
      min = 1,
      step = 1
    ),
    
    sliderInput(
      ns("split"),
      label = strong("Train/Test split (%)"),
      min = 50, max = 90, value = 80, step = 1
    ),
    
    checkboxInput(ns("standardize"), "Standardize predictors", value = TRUE),
    
    # Response + predictors
    pickerInput(
      ns("response"),
      strong("Response Variable (y)"),
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
    
    results_ready <- reactiveVal(FALSE)
    
    output$resultsContainer <- renderUI({
      if (!isTRUE(results_ready())) {
        tagList(
          tags$h3("Results"),
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
      req(input$k >= 1)
      
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
        
        #Results UI layout
        output$resultsUI <- renderUI({
          tagList(
            tags$h3("Results"),
            tags$p(tags$strong("Task: "), input$task),
            tags$p(tags$strong("k = "), k),
            tags$p(tags$strong("Train split = "), paste0(input$split, "%")),
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
