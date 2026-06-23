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
                value       = "2.14,   2.09,   2.65,   3.56,   5.55,   5.00,   5.55,   8.09,   10.79", 
                placeholder = "Enter values separated by a comma, space, or tab with decimals as points",
                rows        = 3),
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
              selected = c("Boxplot"),
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
                withMathJax(),
                  
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
                      
                      withMathJax(),
                      titlePanel(tags$u("Sample Mean")),
                      br(),
                      uiOutput(ns("dsMeanCalc")),

                      withMathJax(),
                      titlePanel(tags$u("Sample Standard Deviation")),
                      br(),
                      uiOutput(ns("dsSDCalc")),
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
                      
                      uiOutput(ns("renderDSBoxplot"))
                    ), # Boxplot
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.dsGraphOptions.indexOf('Histogram') > -1",
                      
                      h3("Histogram"),
                      br(),
                      plotOptionsMenuUI(
                        id    = ns("dsHisto"),
                        title = "Histogram"),
                      uiOutput(ns("renderDSHistogram"))
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
    dsupload_iv$add_rule("dsUserData", sv_required())
    dsupload_iv$add_rule("dsUserData", ~ if(is.null(fileInputs$dsStatus) || fileInputs$dsStatus == 'reset') "Required")
    dsupload_iv$add_rule("dsUserData", ~ if(!(tolower(tools::file_ext(input$dsUserData$name)) %in% c("csv", "txt", "xls", "xlsx", "sas7bdat", "sav", "dta", "rds", "mtp", "mwx", "mpx"))) "File format not accepted.")
    dsupload_iv$add_rule("dsUserData", ~ if(ncol(dsUploadData()) < 1) "Data must include one variable")
    dsupload_iv$add_rule("dsUserData", ~ if(nrow(dsUploadData()) < 2) "Samples must include at least 2 observations")
    
    dsuploadvars_iv$add_rule("dsUploadVars", sv_required())
    dsuploadvars_iv$add_rule("dsUploadVars", ~ {
      if (checkNumeric()) {
        "Selected variable contains non-numeric data."
      }
    })
    dsuploadvars_iv$add_rule("dsUploadVars", ~ {
      col_data <- dsUploadData()[[.x]]      
      if(length(na.omit(col_data)) < 2) {   
        "Selected column must have at least 2 observations."
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

    # ----------------------------------------------------------- #
    #     Minitab file readers                                    #
    # ----------------------------------------------------------- #
    # Older Minitab Portable Worksheet (.mtp) – text-based.
    read_mtp_helper <- function(path) {
      raw <- foreign::read.mtp(path)
      keep <- raw[vapply(raw, is.numeric, logical(1))]
      validate(need(length(keep) > 0, "No numeric columns found in .mtp file."))
      max_len <- max(vapply(keep, length, integer(1)))
      keep <- lapply(keep, function(v) { length(v) <- max_len; v })
      if (is.null(names(keep)) || any(names(keep) == ""))
        names(keep) <- paste0("V", seq_along(keep))
      as.data.frame(keep, stringsAsFactors = FALSE)
    }

    # Newer Minitab XML formats (.mwx / .mpx) – best-effort, schema varies.
    read_minitab_xml <- function(path) {
      tmp <- tempfile()
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      utils::unzip(path, exdir = tmp)
      xml_files <- list.files(tmp, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
      validate(need(length(xml_files) > 0, "Could not find data inside Minitab file. Try exporting to .xlsx."))

      doc <- NULL
      for (f in xml_files) {
        candidate <- try(xml2::read_xml(f), silent = TRUE)
        if (inherits(candidate, "xml_document") &&
            length(xml2::xml_find_all(candidate, "//*[local-name()='Column']")) > 0) {
          doc <- candidate; break
        }
      }
      validate(need(!is.null(doc), "Could not parse Minitab file. Please export to .xlsx in Minitab."))

      cols <- xml2::xml_find_all(doc, "//*[local-name()='Column']")
      col_data <- lapply(seq_along(cols), function(i) {
        col <- cols[[i]]
        nm  <- xml2::xml_attr(col, "Name")
        if (is.na(nm)) nm <- xml2::xml_attr(col, "name")
        if (is.na(nm)) nm <- paste0("C", i)
        cells <- xml2::xml_find_all(col, ".//*[local-name()='Cell' or local-name()='Value' or local-name()='R']")
        vals  <- xml2::xml_text(cells)
        list(name = nm, values = vals)
      })

      max_len <- max(vapply(col_data, function(x) length(x$values), integer(1)))
      df_cols <- lapply(col_data, function(x) {
        v <- x$values; length(v) <- max_len
        nv <- suppressWarnings(as.numeric(v))
        if (sum(is.na(nv)) <= sum(is.na(v))) nv else v
      })
      names(df_cols) <- vapply(col_data, function(x) x$name, character(1))
      as.data.frame(df_cols, stringsAsFactors = FALSE)
    }

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
      
      req(class(x) == "numeric")
      
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
    Modes <- function(x) {
      modes <- Mode(x)
      if (anyNA(modes)) {return("No mode exists")}
      else if (length(modes) == 1) {return(paste(modes))}
      else if (length(modes) > 1) {
        modesList <- paste(modes[1])
        
        for(index in 2:length(modes)) {
          modesList <- paste0(modesList, ", ", modes[index])
        }
        return(modesList)
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
      outliers <- c()
      
      for(x in dat) {
        if(x < lower | x > upper) {
          outliers <-c(outliers, x)
        }
      }
      
      return(sort(outliers))
    }
    
    # Function for populating the value column of the datatable
    createDSColumn <- function(dat) ({
      sampSize <- length(dat)
      sampSum <- sum(dat)
      sumSquares <- sum(dat^2)
      xbar <- mean(dat)
      sampMode <- Modes(dat)
      
      if(sampMode == "No mode exists"){
        modeFreq <- paste("")
      } else{
        modeFreq <- paste("Each appears", attr(Mode(dat), "freq"), "times")
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
        outliers <- "There are no outliers."
      } else {
        outliers <- paste(as.character(GetOutliers(dat, lowerFence, upperFence)), collapse=", ")
      }
      
      sampRange <- Range(min(dat), max(dat))
      sampVar <- round(var(dat),4)
      sampMeanSE <- round(sd(dat)/sqrt(length(dat)), 4)
      sampStdDev <- sd(dat)
      
      if (sampStdDev < 0.0001) {
        formattedSD <- sprintf("%.4e", sampStdDev)    # use scientific notation if SD is sufficiently small
      } else {
        formattedSD <- sprintf("%.4f", sampStdDev)
      }
      
      coeffVar <- round(sampStdDev/xbar, 4)
      
      if (is.na(coeffVar)) {
        coeffVar <- "Coefficient of Variation is undefined for this data"
      } else if (is.infinite(coeffVar)) {
        coeffVar <- "Infinity"
      }
      
      if(sampSize < 3){
        # Use e1071::skewness to specify the package
        sampSkewness <- round(e1071::skewness(dat, type = 1), 4)
      } else {
        # Use e1071::skewness to specify the package
        sampSkewness <- round(e1071::skewness(dat, type = 2), 4)
      }
      if(sampSize < 4){
        # Use e1071::kurtosis to specify the package
        sampKurtosis <- round(e1071::kurtosis(dat, type = 1), 4)
      } else {
        # Use e1071::kurtosis to specify the package
        sampKurtosis <- round(e1071::kurtosis(dat, type = 2), 4)
      }
      
      if(is.nan(sampSkewness)) {
        sampSkewness <- "Not enough variability or data points in the dataset."
      }
      
      if(is.nan(sampKurtosis)) {
        sampKurtosis <- "Not enough variability or data points in the dataset."
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
                                    sampSkewness, 
                                    sampKurtosis)
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
    
    fileInputs <- reactiveValues(
      dsStatus = NULL)
    
    # Function to convert the raw data input into a numeric list
    dsRawData <- reactive ({
      cleaned <- gsub("[[:space:]]+", ",", trimws(input$descriptiveStat))
      dat <- createNumLst(cleaned)
      return(dat)
    })
    
    # Silence noisy but harmless readxl warnings (boolean-to-numeric coercions).
    quietExcelRead <- function(reader, path, sheet) {
      withCallingHandlers(
        reader(path, sheet = sheet),
        warning = function(w) {
          if (grepl("Coercing boolean to numeric", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }
      )
    }

    # Function to read the uploaded data file
    dsUploadData <- eventReactive(list(input$dsUserData, input$dsSheet), {
      req(input$dsUserData)
      ext  <- tolower(tools::file_ext(input$dsUserData$name))
      path <- input$dsUserData$datapath

      switch(ext,
            csv      = read_csv(path, show_col_types = FALSE),
            xls      = {
              req(input$dsSheet)
              # Block on stale sheet name (transient between file upload and selectize update)
              req(input$dsSheet %in% readxl::excel_sheets(path))
              quietExcelRead(read_xls, path, input$dsSheet)
            },
            xlsx     = {
              req(input$dsSheet)
              req(input$dsSheet %in% readxl::excel_sheets(path))
              quietExcelRead(read_xlsx, path, input$dsSheet)
            },
            txt      = read_tsv(path, show_col_types = FALSE),
            sas7bdat = read_sas(path),
            sav      = read_sav(path),
            dta      = haven::read_dta(path),
            rds      = {
              obj <- readRDS(path)
              validate(need(is.data.frame(obj), ".rds file must contain a data frame."))
              obj
            },
            mtp      = read_mtp_helper(path),
            mwx      = read_minitab_xml(path),
            mpx      = read_minitab_xml(path),

            validate("Improper file format"))
    })

    getSampleVector <- function() {
      if (input$dataInput == 'Upload Data') {
        na.omit(as.data.frame(dsUploadData())[, input$dsUploadVars])
      } else {
        dsRawData()
      }
    }

    getDsDataframe <- reactive({
      
      req(ds_iv$is_valid())
      
      df <- data.frame(Category = c("Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", 
                                    "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", 
                                    "Check for potential outliers", "Check for potential outliers", "Check for potential outliers", "Check for potential outliers", "Check for potential outliers", 
                                    "Dispersion", "Dispersion", "Dispersion", "Dispersion", "Dispersion", 
                                    "Distribution", "Distribution"),
                       Variable = c("Number of Observations", 
                                    "Sum", 
                                    "Sum of Squares", 
                                    "Mean", 
                                    "Mode",
                                    "Mode Frequency",
                                    "Minimum", 
                                    "First Quartile (Q<sub>1</sub>)*", 
                                    "Second Quartile or Median (Q<sub>2</sub>)", 
                                    "Third Quartile (Q<sub>3</sub>)*", 
                                    "Maximum", 
                                    "Interquartile Range (IQR)*", 
                                    "Lower Fence: Q<sub>1</sub> - (1.5 × IQR)", 
                                    "Upper Fence: Q<sub>3</sub> + (1.5 x IQR)", 
                                    "Number of Potential Outliers*",
                                    "Outlier Value(s)*",
                                    "Range", 
                                    "Sample Standard Deviation", 
                                    "Sample Variance", 
                                    "Standard Error of the Mean", 
                                    "Coefficient of Variation",
                                    "Skewness", 
                                    "Kurtosis"))

      if(input$dataInput == 'Upload Data')
      {
        req(dsuploadvars_iv$is_valid())

        for( x in input$dsUploadVars)
        {
          dat <- na.omit(as.data.frame(dsUploadData())[, x])
          newCol <- createDSColumn(dat)
          df[x] <- newCol
        }
        colnames(df) <- c("Category", "Variable", input$dsUploadVars)
      }
      else
      {
        dat <- dsRawData()
        newCol <- createDSColumn(dat)
        df$Value <-newCol
      }
      
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
                        "Kurtosis")
      
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
        rowFilter <- c(rowFilter, "Lower Fence", "Upper Fence", "Outlier Values")
      }

      out <- filter(df, rownames(df) %in% rowFilter)

      # The 'Value' column is a nested data.frame; flatten any data.frame-columns
      # to plain vectors so the table can render client-side (renderDT server = FALSE).
      for (j in seq_along(out)) if (is.data.frame(out[[j]])) out[[j]] <- out[[j]][[1]]
      out
    }

    hideResultTabs <- function() {
      hideTab(inputId = "dsTabset", target = "Descriptive Statistics")
      hideTab(inputId = "dsTabset", target = "Calculations")
      hideTab(inputId = "dsTabset", target = "Graphs")
    }

    showResultTabs <- function() {
      showTab(inputId = "dsTabset", target = "Descriptive Statistics")
      showTab(inputId = "dsTabset", target = "Calculations")
      if (!is.null(input$dsGraphOptions)) {
        showTab(inputId = "dsTabset", target = "Graphs")
      }
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

      # For Excel files, defer until a sheet is selected
      if (ext %in% c("xls", "xlsx") && (is.null(input$dsSheet) || input$dsSheet == "")) {
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

    observeEvent(input$goDescpStats, {
      
      output$renderDescrStats <- renderUI({
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
            need(nrow(dsUploadData()) > 1, "Sample Data must include at least 2 observations."),
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
              "Selected column must have at least 2 observations."
            ),
            errorClass = "myClass"
          )
          
        } else if(!dsraw_iv$is_valid()) {
          validate(
            need(length(dsRawData()) >= 2, "Sample Data must include at least 2 numeric observations."),
            errorClass = "myClass"
          )
          
          validate("Sample Data must be numeric.")
        } 
      })
      
      if(ds_iv$is_valid())
      {
        dsReset(FALSE)
        
        df <- getDsDataframe()
        
        filteredDf <- buildRowFilter(df)

        # Push the row-filtered (flattened) data to the table. It is rendered
        # once below and reacts to dsTableDf(), so this refreshes it every time.
        dsTableDf(filteredDf)

        sampleData <- getSampleVector()
        
        sample_df <- data.frame(sampleData, sampleData^2)
        names(sample_df) <- c("x", "x<sup>2</sup>")
        dfTotaled <- bind_rows(sample_df, summarise(sample_df, across(where(is.numeric), sum)))
        rownames(dfTotaled)[nrow(dfTotaled)] <- "Totals"
        
        output$sampleDataTable <- renderReactable({
          dataRows  <- dfTotaled[1:(nrow(dfTotaled) - 1), , drop = FALSE]
          totalsRow <- dfTotaled[nrow(dfTotaled), , drop = FALSE]

          reactable(
            dataRows,
            sortable      = TRUE,
            resizable     = TRUE,
            bordered      = TRUE,
            striped       = TRUE,
            highlight     = TRUE,
            pagination    = FALSE,
            fullWidth     = TRUE,
            defaultColDef = colDef(align = "center"),
            columns = setNames(
              lapply(names(dataRows), function(col) {
                colDef(
                  html   = TRUE,
                  name   = col,
                  footer = tags$b(format(round(totalsRow[[col]], 3), nsmall = 0, scientific = FALSE)),
                  cell   = function(value) {
                    if (!is.numeric(value)) return(value)
                    if (value == floor(value)) formatC(value, format = "f", digits = 0)
                    else                       formatC(value, format = "f", digits = 3)
                  }
                )
              }),
              names(dataRows)
            )
          )
        })
        
        output$dsMeanCalc <- renderUI({
          withMathJax(
            sprintf("\\( \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%s} = %s \\)",
                    dfTotaled['Totals', 1],
                    df['Observations', 3],
                    df['Mean', 3]),
            br()
          )
        })

        output$dsSDCalc <- renderUI({
          withMathJax(
            sprintf("\\( s = \\sqrt{ \\dfrac{\\sum x^{2} - \\dfrac{(\\sum x)^{2}}{n} }{n - 1} } \\)"),
            sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\)",
                    dfTotaled['Totals', 2],
                    dfTotaled['Totals', 1],
                    df['Observations', 3],
                    df['Observations', 3],
                    df['Sample Standard Deviation', 3])
          )
        })
        
        dat <- getSampleVector()

        df_boxplot <- data.frame(x = dat)
        
        if(df['Outlier Values',3] != "There are no outliers.") {
          df_outliers <- createNumLst(df['Outlier Values',3])
        } else {
          df_outliers <- data.frame()
        }
        
        output$renderDSBoxplot <- renderUI({
          tagList(
            plotOutput(
              session$ns("dsBoxplot"),
              height = GetPlotHeight(input[["dsBoxplot-Height"]], input[["dsBoxplot-HeightPx"]], ui = TRUE),
              width = GetPlotWidth(input[["dsBoxplot-Width"]], input[["dsBoxplot-WidthPx"]], ui = TRUE)),
            br(),
            helpText("* Note: Quartiles are calculated by excluding the median on both sides."),
            br(),
            hr(),
            br(),
          )
        })
        
        output$renderDSHistogram <- renderUI({
          tagList(
            plotOutput(
              session$ns("dsHistogram"),
              height = GetPlotHeight(input[["dsHisto-Height"]], input[["dsHisto-HeightPx"]], ui = TRUE),
              width = GetPlotWidth(input[["dsHisto-Width"]], input[["dsHisto-WidthPx"]], ui = TRUE)),
            br(),
            hr(),
            br(),
          )
        })
        
        #---------------- #
        #### Boxplot ---- 
        #---------------- #
        output$dsBoxplot <- renderPlot({
          RenderBoxplot(dat,
                        df_boxplot,
                        df_outliers,
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
          hist <- ggplot(data.frame(x = dat)) +
            geom_histogram(aes(x = x),
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
          
          if(input[["dsHisto-Flip"]] == 1) {
            hist <- hist + coord_flip() +
              labs(x = input[["dsHisto-Ylab"]],
                   y = input[["dsHisto-Xlab"]]) +
              scale_y_continuous(n.breaks = 10, expand = c(0, 0))
          }
          
          hist
        }, height = function() {GetPlotHeight(input[["dsHisto-Height"]], input[["dsHisto-HeightPx"]], ui = FALSE)},
           width = function() {GetPlotWidth(input[["dsHisto-Width"]], input[["dsHisto-WidthPx"]], ui = FALSE)})
        
        shinyjs::show("descriptiveStatsMP")
        showResultTabs()
      } else {
        hideResultTabs()
      }
    })
    
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
                  columnDefs = list(list(visible = FALSE, targets = c(0))),
                  dom = 't',
                  pageLength = -1,
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

    observeEvent(input$dsTableFilters, {
      req(dsReset() == FALSE)

      dsTableDf(buildRowFilter(getDsDataframe()))
    })
    
    #  -------------------------------------------------------------------- #
    #  ------------------------ Component Display -------------------------
    #  -------------------------------------------------------------------- #
    
    observeEvent({input$descriptiveStat
      input$dsUploadVars}, {
        hideResultTabs()
      })

    session$onFlushed(function() {
      hideTab(inputId = "dsTabset", target = "Uploaded Data")
      hideResultTabs()
      shinyjs::hide("dsUploadVarsWrap")
    }, once = TRUE)

    observeEvent(input$dataInput, {
      hideResultTabs()

      if (input$dataInput == "Upload Data") {
        shinyjs::show("descriptiveStatsMP")
        goToUploadedDataTab()
        shinyjs::hide("dsUploadVarsWrap")
      } else {
        hideTab(inputId = "dsTabset", target = "Uploaded Data")
        updateNavbarPage(session, "dsTabset", selected = "Descriptive Statistics")
        shinyjs::hide("descriptiveStatsMP")
        ds_data_source(NULL)
      }
    }, ignoreInit = TRUE)

    observe({
      if (is.null(input$dsGraphOptions)) {
        hideTab(inputId = "dsTabset", target = "Graphs")
      }
    })
    
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
      updateTextInput(session, "dsHisto-Ylab",   value = "")
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
      updatePickerInput(session, "dsGraphOptions", selected = "Boxplot")

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