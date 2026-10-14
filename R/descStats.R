descStatsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      #   ---------------------------------------- #  
      ### ---- Descriptive Stats sidebarPanel ---- 
      #   ---------------------------------------- #
      sidebarPanel(
        shinyjs::useShinyjs(),
        div(id = ns("inputPanel"),
            radioButtons(
              inputId      = ns("dataInput"),
              label        = strong("Data"),
              choiceValues = list("Enter Raw Data", 
                                  "Upload Data"),
              choiceNames  = list("Enter Raw Data", 
                                  "Upload Data"),
              selected     = "Enter Raw Data",
              inline       = TRUE),
            
            conditionalPanel(
              ns = ns,
              condition = "input.dataInput == 'Enter Raw Data'",
              
              textAreaInput(
                inputId     = ns("descriptiveStat"), 
                label       = strong("Sample"), 
                value       = "46.5, 47.1, 47.8, 48.4, 48.6, 48.8, 49.2, 49.3, 49.6, 49.8, 49.8, 50.0, 50.1, 50.2, 50.3, 50.4, 50.6, 50.8, 51.0, 51.1, 51.3, 51.5, 51.7, 51.9, 52.2, 52.5, 52.6, 53.1, 53.7", 
                placeholder = "Enter values separated by a comma, space, or tab with decimals as points",
                rows        = 4),
            ),
            
            conditionalPanel(
              ns = ns,
              condition = "input.dataInput == 'Upload Data'",
              
              HTML(uploadDataDisclaimer),
              
              fileInput(
                inputId = ns('dsUserData'),
                label = strong('Upload your data (.csv, .xls, .xlsx, .txt, .sas7bdat, .sav, .dta, .rds, .mtp, .mwx, .mpx)'),
                accept = c('text/csv', 'text/comma-seperated-values',
                          'text/tab-separated-values',
                          'text/plain',
                          '.csv',
                          '.txt',
                          '.xls',
                          '.xlsx',
                          '.sas7bdat',
                          '.sav',
                          '.dta',
                          '.rds',
                          '.mtp',
                          '.mwx',
                          '.mpx')),

              uiOutput(ns("dsUploadStatus")),

              conditionalPanel(
                ns = ns,
                condition = "output.dsShowSheetPicker == true",
                selectizeInput(
                  inputId  = ns("dsSheet"),
                  label    = strong("Choose a Sheet"),
                  choices  = c(""),
                  multiple = FALSE,
                  options  = list(placeholder = 'Select a sheet',
                                  onInitialize = I('function() { this.setValue(""); }')))
              ),

              shinyjs::hidden(
                div(id = ns("dsUploadVarsWrap"),
                  selectizeInput(
                    inputId  = ns("dsUploadVars"),
                    label    = strong("Choose a Variable"),
                    choices  = c(""),
                    multiple = FALSE,
                    options  = list(placeholder = 'Select a variable',
                                    onInitialize = I('function() { this.setValue(""); }')))
                )
              ),
            ),
            br(),
            
            shinyWidgets::pickerInput(
              inputId  = ns("dsTableFilters"),
              label    = strong("Statistics"), 
              choices  = list(
                Descriptives          = c("Observations", 
                                          "Sum", 
                                          "Sum of Squares", 
                                          "Mean", 
                                          "Mode"),
                'Five Number Summary' = c("Minimum", 
                                          "First Quartile (Q1)", 
                                          "Second Quartile or Median (Q2)", 
                                          "Third Quartile (Q3)", 
                                          "Maximum"),
                Outliers              = c("IQR", 
                                          "Potential Outliers"),
                Dispersion            = c("Range", 
                                          "Sample Standard Deviation", 
                                          "Sample Variance", 
                                          "Standard Error of the Mean", 
                                          "Coefficient of Variation"),
                Distribution          = c("Skewness", 
                                          "Kurtosis")),
              selected = c("Observations",
                           "Mean",
                           "Mode",
                           "Minimum", 
                           "First Quartile (Q1)", 
                           "Second Quartile or Median (Q2)", 
                           "Third Quartile (Q3)", 
                           "IQR",
                           "Potential Outliers",
                           "Maximum", 
                           "Sample Standard Deviation",
                           "Sample Variance"),
              options  = pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = 'count',
                style = "btn-outline-primary",
                hideDisabled = TRUE),
              multiple = TRUE),
            br(),
            
            shinyWidgets::pickerInput(
              inputId = ns("dsGraphOptions"),
              label = strong("Graph Options"),
              choices = c("Boxplot", "Histogram"),
              selected = c("Boxplot", "Histogram"),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                selectedTextFormat = "values",
                multipleSeperator = ", ",
                title = "Select graph(s) to display"
              )),

            br(),
            
            actionButton(
              inputId = ns("goDescpStats"),
              label   = "Calculate",
              class   = "act-btn"),
            
            actionButton(
              inputId = ns("resetAll"), 
              label   = "Reset Values",
              class   = "act-btn")
          )
      ), #sidebarPanel
      
      mainPanel(
        #   -------------------------------- #  
        ### ---- Descriptive Stats main ---- 
        #   -------------------------------- #
        hidden(div(id = ns("descriptiveStatsMP"),
            uiOutput(ns("renderDescrStats")),

            navbarPage(
              title = NULL,
              id    = ns("dsTabset"),
              theme = bs_theme(version = 4),

              tabPanel(
                id = ns("dsTable"),
                title = "Descriptive Statistics",
                value = "Descriptive Statistics",
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.dsTableFilters == ''",
                    
                    p("Select one or more items from the Statistics menu.")
                  ),
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.dsTableFilters != ''",

                    uiOutput(ns("dsTableWrap"))
                  ),
                  br(),
                    
                  conditionalPanel(
                    ns = ns,
                    condition = "input.dsTableFilters.indexOf('First Quartile (Q1)') > -1 | 
                                 input.dsTableFilters.indexOf('Third Quartile (Q3)') > -1 |
                                 input.dsTableFilters.indexOf('IQR') > -1 | 
                                 input.dsTableFilters.indexOf('Potential Outliers') > -1",
                      
                    helpText("* Note: Quartiles are calculated by excluding the median on both sides.")
                  ),
                
                conditionalPanel(
                  ns = ns,
                  condition = "input.dsTableFilters.indexOf('Skewness') > -1 | 
                               input.dsTableFilters.indexOf('Kurtosis') > -1",
                  
                  helpText(
                    "** Note: Skewness and kurtosis are calculated using the Type 2 method described by ",
                    tags$a(
                      href = "https://rss.onlinelibrary.wiley.com/doi/10.1111/1467-9884.00122",
                      "Joanes and Gill (1998)",
                      target = "_blank"
                    ),
                    ". Different methods may produce slightly different values."
                  )
                )
                ), # dsTable tabPanel
                  
                  tabPanel(
                    id    = ns("dsCalculations"),
                    title = "Calculations",
                    value = 'Calculations',
                    
                    fluidRow(
                      column(
                        width = 4,
                        br(),
                        reactableOutput(ns("sampleDataTable")),
                        br(),
                        ),
                    
                    column(
                      width = 8,
                      
                      titlePanel(tags$u("Sample Mean")),
                      br(),
                      uiOutput(ns("dsMeanCalc")),

                      titlePanel(tags$u("Sample Standard Deviation")),
                      br(),
                      uiOutput(ns("dsSDCalc")),

                      titlePanel(tags$u("Standard Error of the Mean")),
                      br(),
                      uiOutput(ns("dsSECal")),
                      
                      titlePanel(tags$u("Coefficient of Variation")),
                      br(),
                      uiOutput(ns("dsCVCal")),
                      
                      titlePanel(tags$u("Range")),
                      br(),
                      uiOutput(ns("dsRangeCal")),
                      
                      titlePanel(tags$u("Interquartile Range")),
                      br(),
                      uiOutput(ns("dsIQRCalc")),
                    ), #column
                   ), #fluidRow
                  ),
              
                  tabPanel(
                    id    = ns("dsGraphs"), 
                    title = "Graphs", 
                    value = 'Graphs',
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.dsGraphOptions.indexOf('Boxplot') > -1",
                      
                      h3("Boxplot"),
                      br(),
                      plotOptionsMenuUI(
                        id       = ns("dsBoxplot"), 
                        plotType = "Boxplot",
                        title    = "Boxplot"),
                      
                      plotOutput(ns("dsBoxplot"), height = "400px", width = "auto"),
                      br(),
                      boxplotDisclaimer,
                      br(),
                      hr(),
                      br()
                    ), # Boxplot
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.dsGraphOptions.indexOf('Histogram') > -1",
                      
                      h3("Histogram"),
                      br(),
                      plotOptionsMenuUI(
                        id    = ns("dsHisto"),
                        plotType = "Histogram",
                        title = "Histogram",
                        ylab = "Frequency"),
                      plotOutput(ns("dsHistogram"), height = "400px", width = "auto"),
                      br(),
                      hr(),
                      br()
                    ) # Histogram
                  ), # Graphs tabPanel

              tabPanel(
                id    = ns("dsData"),
                title = "Uploaded Data",
                value = "Uploaded Data",
                uiOutput(ns("renderDSData"))
              )
            ) # navbarPage
        )) # descriptiveStatsMP
      ) # mainPanel
    ) # sidebarLayout
  )
}

descStatsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # ------------------------- #
    # ---- Data Validation ----
    # ------------------------- #
    ds_iv <- InputValidator$new()
    dsraw_iv <- InputValidator$new()
    dsupload_iv <- InputValidator$new()
    dsuploadvars_iv <- InputValidator$new()
    
    # ------------------ #
    #       Rules        #
    # ------------------ #
    dsraw_iv$add_rule("descriptiveStat", sv_required())
    dsraw_iv$add_rule("descriptiveStat", sv_regex("^[[:space:]]*(-)?[0-9]+(\\.[0-9]+)?([,[:space:]]+(-)?[0-9]+(\\.[0-9]+)?)+[[:space:]]*$",
                                                  "Data must be numeric values separated by a comma, space, or tab (ie: 2,3,4 or 2 3 4)"))
    # The pattern above accepts arbitrarily long digit strings, which become Inf.
    dsraw_iv$add_rule("descriptiveStat", ~ if(any(is.infinite(dsRawData()))) "Data contains values that are too large to compute.")
    dsupload_iv$add_rule("dsUserData", sv_required())
    dsupload_iv$add_rule("dsUserData", ~ if(is.null(fileInputs$dsStatus) || fileInputs$dsStatus == 'reset') "Required")
    dsupload_iv$add_rule("dsUserData", ~ if(!(tolower(tools::file_ext(input$dsUserData$name)) %in% c("csv", "txt", "xls", "xlsx", "sas7bdat", "sav", "dta", "rds", "mtp", "mwx", "mpx"))) "File format not accepted.")
    # Why the file reader rejected the file (e.g. too large, .rds is not a data frame)
    dsupload_iv$add_rule("dsUserData", ~ uploadValidationMessage(dsUploadData()))
    dsupload_iv$add_rule("dsUserData", ~ if(ncol(dsUploadData()) < 1) "Data must include one variable")
    dsupload_iv$add_rule("dsUserData", ~ if(nrow(dsUploadData()) < 2) "Samples must include at least two observations")
    
    dsuploadvars_iv$add_rule("dsUploadVars", sv_required())
    dsuploadvars_iv$add_rule("dsUploadVars", ~ {
      if (checkNumeric()) {
        "Selected variable contains non-numeric data."
      }
    })
    dsuploadvars_iv$add_rule("dsUploadVars", ~ {
      col_data <- dsUploadData()[[.x]]      
      if(length(na.omit(col_data)) < 2) {   
        "Selected column must have at least two observations."
      }
    })
    dsuploadvars_iv$add_rule("dsUploadVars", ~ {
      if(any(is.infinite(dsUploadData()[[.x]]))) {
        "Selected variable contains infinite values."
      }
    })
    # ------------------ #
    #     Conditions     #
    # ------------------ #
    dsraw_iv$condition(~ isTRUE(input$dataInput == 'Enter Raw Data'))
    dsupload_iv$condition(~ isTRUE(input$dataInput == 'Upload Data'))
    dsuploadvars_iv$condition(function() { input$dataInput == 'Upload Data' && dsupload_iv$is_valid() })

    # ------------------ #
    #     Dependency     #
    # ------------------ #
    ds_iv$add_validator(dsraw_iv) 
    ds_iv$add_validator(dsupload_iv)
    ds_iv$add_validator(dsuploadvars_iv)
    
    # ------------------ #
    #     Activation     #
    # ------------------ #
    ds_iv$enable()
    dsraw_iv$enable()
    dsupload_iv$enable()
    dsuploadvars_iv$enable()

    ds_data_source <- reactiveVal(NULL)

    output$dsUploadStatus <- renderUI({
      src <- ds_data_source()
      if (is.null(src)) return(NULL)
      div(
        class = "alert alert-success",
        style = "padding: 5px 10px; font-size: 12px; margin-top: 2px; margin-bottom: 10px;",
        icon("circle-check"),
        HTML(paste0(" <strong>File loaded:</strong> ", src$name, " (",
                    src$rows, " rows × ", src$cols, " columns)"))
      )
    })

    #  -------------------------------------------------------------------- #
    ## ------------------- Descriptive Stats functions --------------------
    #  -------------------------------------------------------------------- #
    
    ### Module Server Elements ----
    # --------------------------------------------------------------------- #
    plotOptionsMenuServer("dsBoxplot")
    plotOptionsMenuServer("dsHisto")
    
    ### Non-Reactive Functions ----
    # --------------------------------------------------------------------- #
    
    # https://rdrr.io/github/skgrange/threadr/src/R/decimal_count.R
    DecimalCount <- function(x) {
      
      req(is.numeric(x))
      
      # If contains a period
      if (grepl("\\.", x)) {
        x <- stringr::str_replace(x, "0+$", "")
        x <- stringr::str_replace(x, "^.+[.]", "")
        x <- stringr::str_length(x)
        
      } else {
        # Otherwise return zero
        x <- 0
      }
      
      return(x)
    }
    
    # Function to find the mode(s)
    Modes <- function(x, modes = DescTools::Mode(x)) {
      if (anyNA(modes)) {return("No mode exists.")}
      else if (length(modes) == 1) {return(paste(modes))}
      else if (length(modes) > 1) {
        return(paste(modes, collapse = ", "))
      }
    }
    
    Range <- function(min, max) {
      if(DecimalCount(min) < DecimalCount(max)) {
        numDigits <- DecimalCount(max)
      } else {
        numDigits <- DecimalCount(min)
      }
      
      range <- round((max - min), digits = numDigits)
      
      return(range)
    }
    
    GetQuartiles <- function(dat) {
      dat <- sort(dat)
      quartiles <- list()
      
      # Remove median if length is odd
      if(length(dat) %% 2 != 0) {
        dat <- dat[-ceiling(length(dat)/2)]
      }
      
      mid <- length(dat) / 2
      quartiles$q1 <- median(dat[1:mid])
      quartiles$q3 <- median(dat[(mid+1):length(dat)])
      
      return(quartiles)
    }
    
    GetOutliers <- function(dat, lower, upper) {
      return(sort(dat[dat < lower | dat > upper]))
    }
    
    # Outlier values as numbers, parsed from the outlier text of the statistics table
    getOutlierValues <- function(df) {
      if(df['Outlier Values',3] != "There are no outliers.") {
        createNumLst(df['Outlier Values',3])
      } else {
        data.frame()
      }
    }
    
    # Function for populating the value column of the datatable
    createDSColumn <- function(dat) ({
      # Integer-typed columns (.rds, .dta, ...) are treated as plain numbers.
      dat <- as.numeric(dat)
      sampSize <- length(dat)
      sampSum <- sum(dat)
      sumSquares <- sum(dat^2)
      xbar <- mean(dat)
      modeResult <- DescTools::Mode(dat)
      sampMode <- Modes(dat, modeResult)
      
      if(sampMode == "No mode exists."){
        modeFreq <- paste("")
      } else{
        modeFreq <- paste("Each appears", attr(modeResult, "freq"), "times")
      }
      
      sampMin <- min(dat)
      quartiles <- GetQuartiles(dat)
      quartile1 <-  quartiles$q1
      sampMedian <- median(dat)
      quartile3 <-  quartiles$q3
      sampMax <- max(dat)
      sampIQR <- round(quartile3 - quartile1, 4)
      lowerFence <- round(quartile1 - (1.5*sampIQR), 4)
      upperFence <- round(quartile3 + (1.5*sampIQR), 4)
      numOutliers <- sum(dat < lowerFence) + sum(dat > upperFence)
      
      if(is.na(numOutliers) || numOutliers == 0) {
        numOutliers <- "There are no outliers."
        outliers <- "None"
      } else {
        outliers <- paste(as.character(GetOutliers(dat, lowerFence, upperFence)), collapse=", ")
      }
      
      sampRange <- Range(min(dat), max(dat))
      sampVar <- round(var(dat),4)
      sampStdDev <- sd(dat)
      sampMeanSE <- round(sampStdDev/sqrt(length(dat)), 4)
      
      # sd() is NaN/NA when the data overflow; only apply the small-SD format to real numbers
      if (is.finite(sampStdDev) && sampStdDev < 0.0001) {
        formattedSD <- sprintf("%.4e", sampStdDev)    # use scientific notation if SD is sufficiently small
      } else {
        formattedSD <- sprintf("%.4f", sampStdDev)
      }
      
      coeffVar <- round(sampStdDev/xbar, 4)
      
      if (is.na(coeffVar)) {
        coeffVar <- "Coefficient of Variation is undefined for this data"
      } else if (is.infinite(coeffVar)) {
        coeffVar <- "Coefficient of Variation is undefined for this data"
      }
      
      if(sampSize < 3){
        # Use e1071::skewness to specify the package
        sampSkewness <- round(e1071::skewness(dat, type = 1), 4)
        sampSESkewness <- "Not enough data points in the dataset."
        sampSkewnessRatio <- "Not enough data points in the dataset."
      } else {
        # Use e1071::skewness to specify the package
        sampSkewness <- round(e1071::skewness(dat, type = 2), 4)
        sampSESkewness <- round(sur::se.skew(dat), 4)
        sampSkewnessRatio <- round(sampSkewness/sampSESkewness, 4)
      }
      if(sampSize < 4){
        # Use e1071::kurtosis to specify the package
        sampKurtosis <- round(e1071::kurtosis(dat, type = 1), 4)
        sampSEKurtosis <- "Not enough data points in the dataset."
        sampKurtosisRatio <- "Not enough data points in the dataset."
      } else {
        # Use e1071::kurtosis to specify the package
        sampKurtosis <- round(e1071::kurtosis(dat, type = 2), 4)
        sampSEKurtosis <- round(sqrt(24 / sampSize), 4)
        sampKurtosisRatio <- round(sampKurtosis/sampSEKurtosis, 4)
      }
      
      if(is.nan(sampSkewness)) {
        sampSkewness <- "Not enough variability or data points in the dataset."
        sampSkewnessRatio <- "Not enough variability or data points in the dataset."
      }
      
      if(is.nan(sampKurtosis)) {
        sampKurtosis <- "Not enough variability or data points in the dataset."
        sampKurtosisRatio <- "Not enough variability or data points in the dataset."
      }
      
      format4 <- function(x) {
        if(is.numeric(x)) {
          sprintf("%.4f", x)
        } else {
          x
        }
      }
      
      dfCol <- data.frame(Value = c(sampSize, 
                                    sampSum, 
                                    sumSquares, 
                                    xbar, 
                                    sampMode,
                                    modeFreq,
                                    sampMin, 
                                    quartile1, 
                                    sampMedian, 
                                    quartile3, 
                                    sampMax, 
                                    sampIQR, 
                                    lowerFence, 
                                    upperFence, 
                                    numOutliers,
                                    outliers,
                                    sampRange, 
                                    formattedSD, 
                                    sampVar, 
                                    sampMeanSE, 
                                    coeffVar, 
                                    format4(sampSkewness),
                                    format4(sampSESkewness),
                                    format4(sampSkewnessRatio),
                                    format4(sampKurtosis),
                                    format4(sampSEKurtosis),
                                    format4(sampKurtosisRatio))
      )
    })
    
    # --------------------------------------------------------------------- #
    ### Reactives ----
    # --------------------------------------------------------------------- #
    
    dsReset <- reactiveVal(FALSE)

    # Row-filtered data shown in the Descriptive Statistics table. The table is
    # rendered once (see below) and reacts to this value, so every Calculate /
    # filter change refreshes it -- re-assigning renderDT in the observer did not.
    dsTableDf <- reactiveVal(NULL)

    # Snapshot of the last successful Calculate (data, totals and the full statistics
    # table). The result outputs are created once and read this value.
    dsCalc <- reactiveVal(NULL)

    # Message shown in the results message area when Calculate fails unexpectedly
    dsCalcError <- reactiveVal(NULL)

    # TRUE while the result tabs (Descriptive Statistics, Calculations, Graphs) are
    # available, i.e. from a successful Calculate until the data changes or is reset.
    dsResultsShown <- reactiveVal(FALSE)
    
    fileInputs <- reactiveValues(
      dsStatus = NULL)
    
    # Function to convert the raw data input into a numeric list
    dsRawData <- reactive ({
      cleaned <- gsub("[[:space:]]+", ",", trimws(input$descriptiveStat))
      dat <- createNumLst(cleaned)
      return(dat)
    })
    
    # Function to read the uploaded data file (shared reader in utilityFunctions.R,
    # which also applies the upload size limits). A file that cannot be read
    # stops with the reader's short "Unable to read this file..." message.
    dsUploadData <- eventReactive(list(input$dsUserData, input$dsSheet), {
      req(input$dsUserData)
      ext  <- tolower(tools::file_ext(input$dsUserData$name))
      path <- input$dsUserData$datapath

      if (ext %in% c("xls", "xlsx")) {
        sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
        validate(need(length(sheets) > 0, uploadReadErrorMsg(ext)))
        req(input$dsSheet)
        # Block on stale sheet name (transient between file upload and selectize update)
        req(input$dsSheet %in% sheets)
      }

      tryCatch(readUploadedDataFile(ext, path, input$dsSheet),
               uploadReadError = function(e) validate(conditionMessage(e)))
    })

    # Complete observations of one uploaded column, as plain numbers
    getUploadVector <- function(variable) {
      as.numeric(na.omit(as.data.frame(dsUploadData())[[variable]]))
    }

    getSampleVector <- function() {
      if (input$dataInput == 'Upload Data') {
        getUploadVector(input$dsUploadVars)
      } else {
        dsRawData()
      }
    }

    getDsDataframe <- reactive({
      
      req(ds_iv$is_valid())
      
      df <- data.frame(Category = c("Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", 
                                    "Five Number Summary*", "Five Number Summary*", "Five Number Summary*", "Five Number Summary*", "Five Number Summary*", 
                                    "Check for potential outliers*", "Check for potential outliers*", "Check for potential outliers*", "Check for potential outliers*", "Check for potential outliers*", 
                                    "Dispersion", "Dispersion", "Dispersion", "Dispersion", "Dispersion", 
                                    "Distribution**", "Distribution**", "Distribution**", "Distribution**", "Distribution**", "Distribution**"),
                       Variable = c("Number of Observations", 
                                    "Sum", 
                                    "Sum of Squares", 
                                    "Mean", 
                                    "Mode",
                                    "Mode Frequency",
                                    "Minimum", 
                                    "First Quartile (Q<sub>1</sub>)", 
                                    "Second Quartile or Median (Q<sub>2</sub>)", 
                                    "Third Quartile (Q<sub>3</sub>)", 
                                    "Maximum", 
                                    "Interquartile Range (IQR)", 
                                    "Lower Fence: Q<sub>1</sub> - (1.5 × IQR)", 
                                    "Upper Fence: Q<sub>3</sub> + (1.5 x IQR)", 
                                    "Number of Potential Outliers",
                                    "Outlier Value(s)",
                                    "Range", 
                                    "Sample Standard Deviation", 
                                    "Sample Variance", 
                                    "Standard Error of the Mean", 
                                    "Coefficient of Variation",
                                    "Skewness", 
                                    "SE(Skewness)",
                                    "Skewness / SE(Skewness)",
                                    "Kurtosis",
                                    "SE(Kurtosis)",
                                    "Kurtosis / SE(Kurtosis)"))

      # The value column is built on its own and appended, rather than assigned by
      # name, so a variable called "Variable" or "Category" cannot overwrite a label column.
      if(input$dataInput == 'Upload Data')
      {
        req(dsuploadvars_iv$is_valid())

        valueName <- input$dsUploadVars
        dat <- getUploadVector(valueName)
      }
      else
      {
        valueName <- "Value"
        dat <- dsRawData()
      }
      
      df <- cbind(df, setNames(data.frame(createDSColumn(dat)[[1]]), valueName))
      
      rownames(df) <- c("Observations", 
                        "Sum", 
                        "Sum of Squares", 
                        "Mean", 
                        "Mode",
                        "Mode Frequency",
                        "Minimum", 
                        "First Quartile (Q1)", 
                        "Second Quartile or Median (Q2)", 
                        "Third Quartile (Q3)", 
                        "Maximum", 
                        "IQR", 
                        "Lower Fence", 
                        "Upper Fence", 
                        "Potential Outliers",
                        "Outlier Values",
                        "Range", 
                        "Sample Standard Deviation", 
                        "Sample Variance", 
                        "Standard Error of the Mean", 
                        "Coefficient of Variation", 
                        "Skewness", 
                        "SE(Skewness)",
                        "Skewness / SE(Skewness)",
                        "Kurtosis",
                        "SE(Kurtosis)",
                        "Kurtosis / SE(Kurtosis)")
      
      return(df)
    })
    
    checkNumeric <- eventReactive(input$dsUploadVars, {
      dat <- as.data.frame(dsUploadData())[, input$dsUploadVars, drop = FALSE]
      
      # Check if any selected columns are non-numeric
      invalid <- any(!sapply(dat, is.numeric))
      
      return(invalid)
    })
    
    # --------------------------------------------------------------------- #
    ### Observers ----
    # --------------------------------------------------------------------- #

    buildRowFilter <- function(df) {
      rowFilter <- input$dsTableFilters
      
      if ("Mode" %in% input$dsTableFilters && df['Mode', 3] != "No mode exists.") {
        rowFilter <- c(rowFilter, "Mode Frequency")
      }
      
      if ("Potential Outliers" %in% input$dsTableFilters) {
        rowFilter <- c(rowFilter, "Lower Fence", "Upper Fence")
        
        if (df["Potential Outliers", 3] != "There are no outliers.") {
          rowFilter <- c(rowFilter, "Outlier Values")
        }
      }
      
      if ("Skewness" %in% input$dsTableFilters) {
        rowFilter <- c(rowFilter, "SE(Skewness)", "Skewness / SE(Skewness)")
      }
      
      if ("Kurtosis" %in% input$dsTableFilters) {
        rowFilter <- c(rowFilter, "SE(Kurtosis)", "Kurtosis / SE(Kurtosis)")
      }
      
      # Base subsetting: the value column is named after the uploaded variable, which
      # can duplicate a label column name ("Variable"), and dplyr::filter() rejects that.
      df[rownames(df) %in% rowFilter, , drop = FALSE]
    }

    hideResultTabs <- function() {
      hideTab(inputId = "dsTabset", target = "Descriptive Statistics")
      hideTab(inputId = "dsTabset", target = "Calculations")
      hideTab(inputId = "dsTabset", target = "Graphs")
      dsResultsShown(FALSE)
    }

    showResultTabs <- function() {
      showTab(inputId = "dsTabset", target = "Descriptive Statistics")
      showTab(inputId = "dsTabset", target = "Calculations")
      if (!is.null(input$dsGraphOptions)) {
        showTab(inputId = "dsTabset", target = "Graphs")
      }
      dsResultsShown(TRUE)
      shinyjs::runjs(sprintf(
        "setTimeout(function(){var a=$('#%s a[data-value=\"Descriptive Statistics\"]');a.removeClass('active');a.tab('show');$(window).trigger('resize');},50);",
        session$ns("dsTabset")))
    }

    goToUploadedDataTab <- function() {
      showTab(inputId = "dsTabset", target = "Uploaded Data")
      updateNavbarPage(session, "dsTabset", selected = "Uploaded Data")
    }

    # Tells the UI whether to show the sheet picker (only for xls/xlsx)
    output$dsShowSheetPicker <- reactive({
      if (is.null(input$dsUserData)) return(FALSE)
      tolower(tools::file_ext(input$dsUserData$name)) %in% c("xls", "xlsx")
    })
    outputOptions(output, "dsShowSheetPicker", suspendWhenHidden = FALSE)

    # Populate sheet choices when an Excel file is uploaded
    observeEvent(input$dsUserData, {
      req(input$dsUserData)
      ext <- tolower(tools::file_ext(input$dsUserData$name))
      if (ext %in% c("xls", "xlsx")) {
        sheets <- tryCatch(readxl::excel_sheets(input$dsUserData$datapath),
                           error = function(e) character(0))
        freezeReactiveValue(input, "dsSheet")
        updateSelectizeInput(session, "dsSheet",
                             choices  = sheets,
                             selected = if (length(sheets)) sheets[1] else "")
      } else {
        updateSelectizeInput(session, "dsSheet", choices = character(0), selected = "")
      }
    }, priority = 50)

    # Fills the variable selection options based on data file columns.
    # Depends on BOTH dsUserData and dsSheet so that for Excel files we wait
    # until the sheet has been selected before trying to read columns.
    observeEvent({
      input$dsUserData
      input$dsSheet
    }, {
      req(input$dsUserData)
      fileInputs$dsStatus <- "uploaded"

      ext <- tolower(tools::file_ext(input$dsUserData$name))

      # For Excel files, defer until a sheet is selected (a workbook that
      # cannot be read has no sheets; it is handled below like any unreadable file)
      if (ext %in% c("xls", "xlsx") && (is.null(input$dsSheet) || input$dsSheet == "") &&
          is.null(uploadValidationMessage(dsUploadData()))) {
        shinyjs::hide("dsUploadVarsWrap")
        ds_data_source(NULL)
        return()
      }

      if (dsupload_iv$is_valid()) {
        ds_data_source(list(
          name = input$dsUserData$name,
          rows = nrow(dsUploadData()),
          cols = ncol(dsUploadData())
        ))

        freezeReactiveValue(input, "dsUploadVars")
        updateSelectizeInput(session, "dsUploadVars",
                             choices = colnames(dsUploadData()),
                             selected = "")

        shinyjs::show("dsUploadVarsWrap")
        shinyjs::show("descriptiveStatsMP")
        goToUploadedDataTab()
      } else {
        shinyjs::hide("dsUploadVarsWrap")
        ds_data_source(NULL)
        # The results of an earlier file no longer apply (a valid upload hides
        # them through the variable picker reset).
        dsCalcError(NULL)
        hideResultTabs()
        goToUploadedDataTab()
      }
    })

    # ---- Uploaded Data tab: immediate preview (ML-style) ----
    output$renderDSData <- renderUI({
      if (input$dataInput != "Upload Data") return(NULL)
      if (!dsupload_iv$is_valid()) {
        return(helpText("No data yet. Upload a dataset to view it here."))
      }
      DT::DTOutput(session$ns("dsUploadTable"))
    })

    output$dsUploadTable <- DT::renderDT({
      req(input$dataInput == "Upload Data")
      req(dsupload_iv$is_valid())
      DT::datatable(
        as.data.frame(dsUploadData()),
        options = list(
          pageLength  = 25,
          lengthMenu  = list(c(25, 50, 100, -1), c("25", "50", "100", "all")),
          scrollX     = TRUE
        )
      )
    })

    # Results message area. It stays empty until the first Calculate click.
    output$renderDescrStats <- renderUI({
      req(input$goDescpStats)
      
      if(!dsupload_iv$is_valid())
      {
        if(is.null(input$dsUserData)) {
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$dsStatus) && fileInputs$dsStatus == 'uploaded', "Please upload a file."),
          errorClass = "myClass"
        )
        
        validate(
          need(nrow(dsUploadData()) != 0 && ncol(dsUploadData()) > 0, "File is empty."),
          need(nrow(dsUploadData()) > 1, "Sample Data must include at least two observations."),
          errorClass = "myClass"
        )
      } else if(!dsuploadvars_iv$is_valid()) {
        validate(
          need(input$dsUploadVars != "", "Please select a variable."),
          errorClass = "myClass"
        )
        validate(
          need(!checkNumeric(), "Selected variable contains non-numeric data."),
          errorClass = "myClass"
        )
        validate(
          need(
            length(na.omit(dsUploadData()[[input$dsUploadVars]])) >= 2,
            "Selected column must have at least two observations."
          ),
          errorClass = "myClass"
        )
        validate(
          need(
            !any(is.infinite(dsUploadData()[[input$dsUploadVars]])),
            "Selected variable contains infinite values."
          ),
          errorClass = "myClass"
        )
        
      } else if(!dsraw_iv$is_valid()) {
        validate(
          need(length(dsRawData()) >= 2, "Sample data must contain at least two numeric values."),
          errorClass = "myClass"
        )
        validate(
          need(!any(is.infinite(dsRawData())), "Data contains values that are too large to compute."),
          errorClass = "myClass"
        )
        
        validate("Sample Data must be numeric.")
      } else if(!is.null(dsCalcError())) {
        validate(dsCalcError(), errorClass = "myClass")
      }
    })
    
    observeEvent(input$goDescpStats, {
      dsCalcError(NULL)
      
      # An unexpected error here would otherwise end the session. Silent errors
      # (req/validate) keep their usual meaning and are passed on.
      tryCatch({
        if(ds_iv$is_valid())
        {
          dsReset(FALSE)
          
          df <- getDsDataframe()
          
          # Push the row-filtered data to the table. It is rendered once below and
          # reacts to dsTableDf(), so this refreshes it every time.
          dsTableDf(buildRowFilter(df))
          
          dat <- getSampleVector()
          
          dsCalc(list(
            df      = df,
            dat     = dat,
            sumX    = sum(dat),
            sumX2   = sum(dat^2)
          ))
          
          shinyjs::show("descriptiveStatsMP")
          showResultTabs()
        } else {
          hideResultTabs()
        }
      }, error = function(e) {
        if(inherits(e, "shiny.silent.error")) stop(e)
        
        message("[descStats] Calculate failed: ", conditionMessage(e))
        dsCalcError(paste("The calculation could not be completed:", conditionMessage(e)))
        hideResultTabs()
      })
    })
    
    # The result outputs below are created once. Each reads the snapshot of the
    # last Calculate, so they stay blank until a Calculate has succeeded.
    getCalc <- function() {
      calc <- dsCalc()
      req(calc)
      calc
    }
    
    output$sampleDataTable <- renderReactable({
      calc <- getCalc()
      
      # Numbers are shown without decimals when whole and with 3 decimals otherwise.
      # The text is formatted here, vectorised, and displayed by a JS renderer, so the
      # columns still sort numerically and no R function runs per cell.
      formatCell <- function(v) {
        ifelse(v == floor(v), formatC(v, format = "f", digits = 0), formatC(v, format = "f", digits = 3))
      }
      
      dataRows <- data.frame(
        Observation = seq_along(calc$dat),
        x           = calc$dat,
        x2          = calc$dat^2
      )
      dataRows$x_text  <- formatCell(dataRows$x)
      dataRows$x2_text <- formatCell(dataRows$x2)
      
      # All rows are shown on one page; above 1000 rows they are paged (100 per
      # page, page size menu up to all rows, as in Simple Linear Regression) so
      # the browser does not have to draw every row at once. The footer totals
      # are computed over all rows either way.
      dsReactable <- function(data, ...) {
        n <- nrow(data)
        if (n <= 1000) return(reactable(data, ..., pagination = FALSE))
        reactable(data, ..., pagination = TRUE, defaultPageSize = 100,
                  showPageSizeOptions = TRUE,
                  pageSizeOptions = unique(c(25, 50, 100, 250, 500, 1000, n)))
      }
      
      dsReactable(
        dataRows,
        rownames      = FALSE,
        sortable      = TRUE,
        resizable     = TRUE,
        bordered      = TRUE,
        striped       = TRUE,
        highlight     = TRUE,
        fullWidth     = TRUE,
        defaultColDef = colDef(align = "center"),
        columns = list(
          Observation = colDef(
            html   = TRUE,
            name   = "Observation Number",
            footer = tags$b("Total")
          ),
          x = colDef(
            html   = TRUE,
            name   = "x",
            footer = tags$b(format(round(calc$sumX, 3), nsmall = 0, scientific = FALSE)),
            cell   = reactable::JS("function(cellInfo) { return cellInfo.row['x_text']; }")
          ),
          x2 = colDef(
            html   = TRUE,
            name   = "x<sup>2</sup>",
            footer = tags$b(format(round(calc$sumX2, 3), nsmall = 0, scientific = FALSE)),
            cell   = reactable::JS("function(cellInfo) { return cellInfo.row['x2_text']; }")
          ),
          x_text  = colDef(show = FALSE),
          x2_text = colDef(show = FALSE)
        )
      )
    })
    
    output$dsMeanCalc <- renderUI({
      calc <- getCalc()
      df   <- calc$df
      
      withMathJax(
        sprintf("\\( \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%s} = %s \\)",
                calc$sumX,
                df['Observations', 3],
                df['Mean', 3]),
        br(),
        br()
      )
    })

    output$dsSDCalc <- renderUI({
      calc <- getCalc()
      df   <- calc$df
      
      withMathJax(
        sprintf("\\( s = \\sqrt{ \\dfrac{\\sum x^{2} - \\dfrac{(\\sum x)^{2}}{n} }{n - 1} } \\)"),
        sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\)",
                calc$sumX2,
                calc$sumX,
                df['Observations', 3],
                df['Observations', 3],
                df['Sample Standard Deviation', 3]),
        br(),
        br()
      )
    })
    
    output$dsIQRCalc <- renderUI({
      df <- getCalc()$df
      
      div(style = "margin: 20px 0;",
      withMathJax(
        sprintf("\\( IQR = Q_{3} - Q_{1} \\)"),
        sprintf("\\( =  %s - (%s) = %s \\)",
                df['Third Quartile (Q3)', 3],
                df['First Quartile (Q1)', 3],
                df['IQR', 3]),
        br(),
        br()
        )
      )
    })
    
    output$dsRangeCal <- renderUI({
      df <- getCalc()$df
      
      div(style = "margin: 20px 0;",
      withMathJax(
        sprintf("\\( \\text{Range} = \\text{Maximum} - \\text{Minimum} \\)"),
        sprintf("\\( = %s - %s = %s \\)",
                df['Maximum', 3],
                df['Minimum', 3],
                df['Range', 3]),
        br(),
        br()
        )
      )
    })
    
    output$dsSECal <- renderUI({
      df <- getCalc()$df
      
      div(style = "margin: 20px 0;",
      withMathJax(
        sprintf("\\( SE_{\\bar{x}} = \\dfrac{s}{\\sqrt{n}} \\)"),
        sprintf("\\( = \\dfrac{%s}{\\sqrt{%s}} = %s \\)",
                df['Sample Standard Deviation', 3],
                df['Observations', 3],
                df['Standard Error of the Mean', 3]),
        br(),
        br()
        )
      )
    })
    
    output$dsCVCal <- renderUI({
      df <- getCalc()$df
      
      div(style = "margin: 20px 0;",
      withMathJax(
        sprintf("\\( CV = \\dfrac{s}{\\bar{x}} \\)"),
        sprintf("\\( = \\dfrac{%s}{%s} = %s \\)",
                df['Sample Standard Deviation', 3],
                df['Mean', 3],
                df['Coefficient of Variation', 3]),
        br(),
        br()
        )
      )
    })
    
    #---------------- #
    #### Plot sizes ----
    #---------------- #
    # The plot outputs are static. Their container size follows the Height/Width
    # options through the style of the existing element, so changing an option does
    # not rebuild the output (the plots themselves resize via renderPlot below).
    setPlotContainerSize <- function(plotId, menuId) {
      req(input[[paste0(menuId, "-Height")]], input[[paste0(menuId, "-Width")]])
      
      shinyjs::runjs(sprintf(
        "(function(){var e=document.getElementById('%s'); if(e){e.style.height='%s'; e.style.width='%s';}})();",
        session$ns(plotId),
        GetPlotHeight(input[[paste0(menuId, "-Height")]], input[[paste0(menuId, "-HeightPx")]], ui = TRUE),
        GetPlotWidth(input[[paste0(menuId, "-Width")]], input[[paste0(menuId, "-WidthPx")]], ui = TRUE)))
    }
    
    observe(setPlotContainerSize("dsBoxplot", "dsBoxplot"))
    observe(setPlotContainerSize("dsHistogram", "dsHisto"))
    
    #---------------- #
    #### Boxplot ---- 
    #---------------- #
    output$dsBoxplot <- renderPlot({
      calc <- getCalc()
      
      RenderBoxplot(calc$dat,
                    data.frame(x = calc$dat),
                    getOutlierValues(calc$df),
                    input[["dsBoxplot-Colour"]],
                    input[["dsBoxplot-Title"]],
                    input[["dsBoxplot-Xlab"]],
                    input[["dsBoxplot-Ylab"]],
                    input[["dsBoxplot-BoxWidth"]]/10,
                    input[["dsBoxplot-Gridlines"]],
                    input[["dsBoxplot-Flip"]],
                    input[["dsBoxplot-OutlierLabels"]])
      
      
    }, height = function() {GetPlotHeight(input[["dsBoxplot-Height"]], input[["dsBoxplot-HeightPx"]], ui = FALSE)},
       width = function() {GetPlotWidth(input[["dsBoxplot-Width"]], input[["dsBoxplot-WidthPx"]], ui = FALSE)})
    
    #------------------ #
    #### Histogram ----
    #------------------ #
    output$dsHistogram <- renderPlot({
      dat <- getCalc()$dat
      
      hist <- ggplot(data.frame(x = dat)) +
        geom_histogram(
                       aes(x = x, y = if (input[["dsHisto-Density"]]) after_stat(density) else after_stat(count)),
                       bins = 15,
                       boundary = min(dat),
                       closed = "right",
                       fill = input[["dsHisto-Colour"]],
                       color = "black") +
        labs(title = input[["dsHisto-Title"]],
             x = input[["dsHisto-Xlab"]],
             y = input[["dsHisto-Ylab"]]) +
        theme_void() +
        theme(plot.title = element_text(size = 24,
                                        face = "bold",
                                        hjust = 0.5,
                                        margin = margin(0,0,10,0)),
              axis.title.x = element_text(size = 16,
                                          face = "bold",
                                          vjust = -1.5,
                                          margin = margin(8,0,0,0)),
              axis.title.y = element_text(size = 16,
                                          face = "bold",
                                          vjust = 1.5,
                                          margin = margin(0,8,0,0)),
              axis.text.x.bottom = element_text(size = 14,
                                                face = "bold",
                                                margin = margin(8,0,0,0)),
              axis.text.y.left = element_text(size = 14,
                                              face = "bold",
                                              margin = margin(0,8,0,0)),
              plot.margin = unit(c(1, 1, 1, 1),"cm"),
              panel.border = element_rect(fill = NA))
      
      hist <- hist + scale_x_continuous(n.breaks = 10)
      
      if("Major" %in% input[["dsHisto-Gridlines"]]) {
        hist <- hist + theme(panel.grid.major = element_line(colour = "#D9D9D9"))
      }

      if("Minor" %in% input[["dsHisto-Gridlines"]]) {
        hist <- hist + theme(panel.grid.minor = element_line(colour = "#D9D9D9"))
      }
      
      if(input[["dsHisto-Density"]]) {
        hist <- hist +
          geom_density(
            aes(x = x, y = after_stat(density)),
            colour = "orange",
            linewidth = 1.5
          ) +
          scale_y_continuous(
            limits = c(0, NA),
            breaks = scales::breaks_pretty(n = 6)
          )
      }
      
      hist
    }, height = function() {GetPlotHeight(input[["dsHisto-Height"]], input[["dsHisto-HeightPx"]], ui = FALSE)},
       width = function() {GetPlotWidth(input[["dsHisto-Width"]], input[["dsHisto-WidthPx"]], ui = FALSE)})
    
    
    observeEvent(input[["dsHisto-Density"]], {
      updateTextInput(
        session,
        "dsHisto-Ylab",
        value = if (input[["dsHisto-Density"]]) {
          "Density"
        } else {
          "Frequency"
        }
      )
    })
    
    # The table has a value only after the first Calculate (dsTableDf() is NULL
    # until then), so nothing is built at session start. The wrapper is re-created
    # with every new value and both outputs stay unsuspended: DT postpones rendering
    # into a hidden container and only renders a fresh element reliably when it is
    # shown (the resize in showResultTabs), so a static DTOutput could keep showing
    # the previous table.
    output$dsTableWrap <- renderUI({
      req(dsTableDf())
      DT::DTOutput(session$ns("dsTableData"))
    })
    outputOptions(output, "dsTableWrap", suspendWhenHidden = FALSE)

    output$dsTableData <- renderDT({
      req(dsTableDf())
      datatable(dsTableDf(),
                extensions = 'RowGroup',
                options = list(
                  rowGroup = list(dataSrc = 0),
                  columnDefs = list(list(visible = FALSE, targets = c(0)),
                                    list(width = "250px", targets = 1)),
                  dom = 't',
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = TRUE,
                  scrollX = TRUE
                ),
                escape = FALSE,
                rownames = FALSE,
                filter = "none")
    }, server = FALSE)
    outputOptions(output, "dsTableData", suspendWhenHidden = FALSE)

    # A Statistics change refilters the statistics of the last Calculate. Nothing is
    # computed before the first Calculate.
    observeEvent(input$dsTableFilters, {
      req(dsReset() == FALSE)
      calc <- getCalc()

      dsTableDf(buildRowFilter(calc$df))
    }, ignoreInit = TRUE)
    
    #  -------------------------------------------------------------------- #
    #  ------------------------ Component Display -------------------------
    #  -------------------------------------------------------------------- #
    
    observeEvent({input$descriptiveStat
      input$dsUploadVars}, {
        dsCalcError(NULL)
        hideResultTabs()
      }, ignoreInit = TRUE)

    # Initial tab state: no data tab and no result tabs until they are needed. This
    # runs in the module's own reactive context, so the tabset id is namespaced
    # (inside session$onFlushed it was not, and the tabs were never hidden).
    observeEvent(TRUE, {
      hideTab(inputId = "dsTabset", target = "Uploaded Data")
      hideResultTabs()
    }, once = TRUE)

    observeEvent(input$dataInput, {
      dsCalcError(NULL)
      hideResultTabs()

      if (input$dataInput == "Upload Data") {
        shinyjs::show("descriptiveStatsMP")
        goToUploadedDataTab()
        # A file uploaded before is still selected and used by Calculate: show
        # its "File loaded" banner and variable picker again, as after the upload.
        if (!is.null(input$dsUserData) && dsupload_iv$is_valid()) {
          ds_data_source(list(
            name = input$dsUserData$name,
            rows = nrow(dsUploadData()),
            cols = ncol(dsUploadData())
          ))
          shinyjs::show("dsUploadVarsWrap")
        } else {
          shinyjs::hide("dsUploadVarsWrap")
        }
      } else {
        hideTab(inputId = "dsTabset", target = "Uploaded Data")
        updateNavbarPage(session, "dsTabset", selected = "Descriptive Statistics")
        shinyjs::hide("descriptiveStatsMP")
        ds_data_source(NULL)
      }
    }, ignoreInit = TRUE)

    # Graph Options: with nothing selected the Graphs tab is hidden (and left if it
    # is the open tab); selecting a graph again brings the tab back while results exist.
    observeEvent(input$dsGraphOptions, {
      if (is.null(input$dsGraphOptions)) {
        if (identical(input$dsTabset, "Graphs")) {
          updateNavbarPage(session, "dsTabset", selected = "Descriptive Statistics")
        }
        hideTab(inputId = "dsTabset", target = "Graphs")
      } else if (dsResultsShown()) {
        showTab(inputId = "dsTabset", target = "Graphs")
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(input$goDescpStats, {
      if (input$dataInput == "Enter Raw Data") {
        shinyjs::show(id = "descriptiveStatsMP")
      }
    })
    
    observeEvent(input$goDescpStats, {
      if (input$dataInput != "Upload Data") {
        hideTab(inputId = "dsTabset", target = "Uploaded Data")
      } else {
        showTab(inputId = "dsTabset", target = "Uploaded Data")
      }
    })

    # ----- Clear boxplot/histogram axis labels when the data set changes -----
    resetPlotLabels <- function() {
      updateTextInput(session, "dsBoxplot-Xlab", value = "")
      updateTextInput(session, "dsBoxplot-Ylab", value = "")
      updateTextInput(session, "dsHisto-Xlab",   value = "")
    }

    observeEvent(input$dataInput,    resetPlotLabels(), ignoreInit = TRUE)
    observeEvent(input$dsUserData,   resetPlotLabels(), ignoreInit = TRUE)
    observeEvent(input$dsUploadVars, resetPlotLabels(), ignoreInit = TRUE)
    observeEvent(input$dsSheet,      resetPlotLabels(), ignoreInit = TRUE)

    # Clear labels when Calculate is pressed on a different raw dataset
    lastRawCalc <- reactiveVal(NULL)
    observeEvent(input$goDescpStats, {
      if (input$dataInput == "Enter Raw Data") {
        if (!is.null(lastRawCalc()) && !identical(lastRawCalc(), input$descriptiveStat)) {
          resetPlotLabels()
        }
        lastRawCalc(input$descriptiveStat)
      }
    }, priority = 100)

    observeEvent(input$resetAll,{
      dsReset(TRUE)
      dsTableDf(NULL)
      dsCalc(NULL)
      dsCalcError(NULL)
      hideResultTabs()

      resetPlotLabels()
      lastRawCalc(NULL)

      updatePickerInput(session, "dsTableFilters",
                        selected = c("Observations", "Mean", "Mode",
                                     "Minimum", "First Quartile (Q1)",
                                     "Second Quartile or Median (Q2)",
                                     "Third Quartile (Q3)", "IQR",
                                     "Potential Outliers", "Maximum",
                                     "Sample Standard Deviation",
                                     "Sample Variance"))
      updatePickerInput(session, "dsGraphOptions", selected = c("Boxplot", "Histogram"))

      keepUpload <- input$dataInput == "Upload Data" &&
        !is.null(input$dsUserData) && dsupload_iv$is_valid()

      if (keepUpload) {
        freezeReactiveValue(input, "dsUploadVars")
        updateSelectizeInput(session, "dsUploadVars",
                             choices  = colnames(dsUploadData()),
                             selected = "")
        shinyjs::show("dsUploadVarsWrap")
        shinyjs::show("descriptiveStatsMP")
        goToUploadedDataTab()
      } else {
        shinyjs::reset("descriptiveStat")
        shinyjs::reset("dsUserData")
        shinyjs::reset("dsUploadVars")
        ds_data_source(NULL)
        shinyjs::hide("dsUploadVarsWrap")
        fileInputs$dsStatus <- "reset"

        if (input$dataInput == "Upload Data") {
          shinyjs::show("descriptiveStatsMP")
          goToUploadedDataTab()
        } else {
          shinyjs::hide("descriptiveStatsMP")
          updateNavbarPage(session, "dsTabset", selected = "Descriptive Statistics")
        }
      }
    })

    # **************************************************************************** #
  })
}