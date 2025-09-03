library(htmltools)
library(shiny)
library(shinyWidgets)


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
              selected     = "Enter Raw Data", #character(0), #
              inline       = TRUE),
            
            conditionalPanel(
              ns = ns,
              condition = "input.dataInput == 'Enter Raw Data'",
              
              textAreaInput(
                inputId     = ns("descriptiveStat"), 
                label       = strong("Sample"), 
                value       = "2.14,   2.09,   2.65,   3.56,   5.55,   5.00,   5.55,   8.09,   10.79", 
                placeholder = "Enter values separated by a comma with decimals as points", 
                rows        = 3),
            ),
            
            conditionalPanel(
              ns = ns,
              condition = "input.dataInput == 'Upload Data'",
              
              fileInput(
                inputId = ns('dsUserData'), 
                label   = strong('Upload your data (.csv or .xls or .xlsx or .txt)'),
                accept  = c('text/csv','text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            '.csv',
                            '.txt',
                            '.xls',
                            '.xlsx')),
              
              selectizeInput(
                inputId  = ns("dsUploadVars"),
                label    = strong("Choose a Variable"),
                choices  = c(""),
                multiple = FALSE,
                options  = list(placeholder = 'Select a variable',
                                onInitialize = I('function() { this.setValue(""); }'))),
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
            
            selectizeInput(
              inputId  = ns("dsGraphOptions"),
              label    = strong("Graphs"), 
              choices  = c("Boxplot", 
                           "Histogram", 
                           "Stem and Leaf Plot"),
              selected = c("Boxplot"),
              multiple = TRUE,
              options  = list(hideSelected = FALSE,
                              placeholder = 'Select graph(s) to display')),
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
         #, onclick = "history.go(0)"
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
                    
                    br(),
                    p("Select one or more items from the Statistics menu to see more information.")
                  ),
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.dsTableFilters != ''",
                    
                    DTOutput(ns("dsTableData"))
                  ),  
                  br(),
                    
                  conditionalPanel(
                    ns = ns,
                    condition = "input.dsTableFilters.indexOf('First Quartile (Q1)') > -1 | 
                                 input.dsTableFilters.indexOf('Third Quartile (Q3)') > -1 |
                                 input.dsTableFilters.indexOf('IQR') > -1 | 
                                 input.dsTableFilters.indexOf('Potential Outliers') > -1",
                      
                    helpText("* Note: Quartiles are calculated by excluding the median on both sides."),
                  ),
                  br(),
                  br(),
                    
                  conditionalPanel(
                    ns = ns,
                    condition = "input.dsTableFilters.indexOf('Mean') > -1 | 
                                 input.dsTableFilters.indexOf('Sample Standard Deviation') > -1",
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
                        DTOutput(ns("sampleDataTable")),
                        br(),
                        br()),
                      
                    
                    column(
                      width = 8,
                      
                      withMathJax(),
                      titlePanel(tags$u("Sample Mean")),
                      br(),
                      uiOutput(ns("dsMeanCalc")),
                      br(),
                      
                      withMathJax(),
                      titlePanel(tags$u("Sample Standard Deviation")),
                      br(),
                      uiOutput(ns("dsSDCalc")),
                      br(),
                      br(),
                      br(),
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
                    ), # Histogram
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.dsGraphOptions.indexOf('Stem and Leaf Plot') > -1",
                      
                      h3("Stem and Leaf Plot"),
                      br(),
                      fluidRow(
                        column(
                          width = 2, 
                          div("")),
                        
                        column(
                          width = 8, 
                          verbatimTextOutput(ns("dsStemLeaf")),
                          br(),
                          p("* Note: Outlier values are listed under the HI/LO lists.")),
                        
                        column(
                          width = 2, 
                          div(""))
                      ), # fluidRow
                      br()
                    ), # Stem and Leaf
                  ), # Graphs tabPanel
                  
                  tabPanel(
                    id = ns("dsData"),
                    title = "Uploaded Data",
                    
                    uiOutput(ns("renderDSData"))
                  ),
                
                # dsTabset tabsetPanel
            ) #descrStatsData div
        )) #descriptiveStatsMP
      ) #mainPanel
    ) #sidebarLayout
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
    dsraw_iv$add_rule("descriptiveStat", sv_regex("^( )*(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                                  "Data must be numeric values separated by a comma (ie: 2,3,4)"))
    dsupload_iv$add_rule("dsUserData", sv_required())
    dsupload_iv$add_rule("dsUserData", ~ if(is.null(fileInputs$dsStatus) || fileInputs$dsStatus == 'reset') "Required")
    dsupload_iv$add_rule("dsUserData", ~ if(!(tolower(tools::file_ext(input$dsUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    dsupload_iv$add_rule("dsUserData", ~ if(ncol(dsUploadData()) < 1) "Data must include one variable")
    dsupload_iv$add_rule("dsUserData", ~ if(nrow(dsUploadData()) < 2) "Samples must include at least 2 observations")
    
    dsuploadvars_iv$add_rule("dsUploadVars", sv_required())
    
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
      q <- quantile(dat, probs = c(0.25, 0.75), type = 7, na.rm = TRUE)
      list(q1 = q[1], q3 = q[2])
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
    
    
    # Function to find the population standard deviation
    pop.sd <- function(x) {
      sqrt(sum((x-mean(x))^2)/length(x))
    }
    
    
    # Function for populating the value column of the datatable
    createDSColumn <- function(dat) ({
      sampSize <- length(dat)
      sampSum <- sum(dat)
      sumSquares <- sum(dat^2)
      xbar <- round(mean(dat),4)
      sampMode <- Modes(dat)
      
      if(sampMode == "No mode exists"){
        modeFreq <- paste("")
      } else{
        modeFreq <- paste("Each appears", attr(Mode(dat), "freq"), "times")
      }
      
      sampMin <- min(dat)
      #popuStdDev <- round(pop.sd(dat),4) # round(sqrt((n-1)/n) * sampStdDev(dat), 4)
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
    
    fileInputs <- reactiveValues(
      dsStatus = NULL)
    
    # Function to convert the raw data input into a numeric list
    dsRawData <- reactive ({
      dat <- createNumLst(input$descriptiveStat)
      return(dat)
    })
    
    
    # Function to read the uploaded data file
    dsUploadData <- eventReactive(input$dsUserData, {
      ext <- tools::file_ext(input$dsUserData$name)
      ext <- tolower(ext)
      
      switch(ext, 
             csv = read_csv(input$dsUserData$datapath, show_col_types = FALSE),
             xls = read_xls(input$dsUserData$datapath),
             xlsx = read_xlsx(input$dsUserData$datapath),
             txt = read_tsv(input$dsUserData$datapath, show_col_types = FALSE),
             
             validate("Improper file format.")
      )
    })
    
    
    getDsDataframe <- reactive({
      
      req(ds_iv$is_valid())
      
      df <- data.frame(Category = c("Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", 
                                    "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", 
                                    "Outliers", "Outliers", "Outliers", "Outliers", "Outliers", 
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
                                    "Check for Outliers: Lower Fence*", 
                                    "Check for Outliers: Upper Fence*", 
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
    
    # --------------------------------------------------------------------- #
    
    
    ### Observers ----
    # --------------------------------------------------------------------- #
    
    # Fills the variable selection options based on data file columns
    observeEvent(input$dsUserData, {
      shinyjs::hide(id = "descriptiveStatsMP")
      shinyjs::hide(id = "dsUploadVars")
      fileInputs$dsStatus <- 'uploaded'
      
      if(dsupload_iv$is_valid())
      {
        freezeReactiveValue(input, "dsUploadVars")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "dsUploadVars",
                          choices = c(colnames(dsUploadData()))
        )
        
        shinyjs::show(id = "dsUploadVars")
      }
    })
    
    #### --------------- Render Uploaded Data Table ------------------------
    observeEvent(input$goDescpStats, {
      output$renderDSData <- renderUI({
        tagList(
          titlePanel("Data File"),
          br(),
          br(),
          div(DTOutput(session$ns("dsUploadTable")), style = "width: 75%"),
          br(),
          br()
        )
      })
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
          
        } else if(!dsraw_iv$is_valid()) {
          validate(
            need(length(dsRawData()) >= 2, "Sample Data must include at least 2 numeric observations."),
            errorClass = "myClass"
          )
          
          validate("Sample Data must be numeric.")
        } 
      })
      
#### ------------ Uploaded Data Table --------------------------------------------
      output$dsUploadTable <- renderDT({
        req(dsupload_iv$is_valid())
        datatable(dsUploadData(),
                  options = list(pageLength = 25,
                                 lengthMenu = list(c(25, 50, 100, -1),
                                                   c("25", "50", "100", "all")),
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = 0:ncol(dsUploadData())))),
        )
      })
      
      if(ds_iv$is_valid())
      {
        dsReset(FALSE)
        
        df <- getDsDataframe()
        
        rowFilter <- input$dsTableFilters
        
        if("Mode" %in% input$dsTableFilters && df['Mode',3] != "No mode exists."){
          rowFilter <- c(rowFilter, "Mode Frequency")
        } 
        
        if("Potential Outliers" %in% input$dsTableFilters){
          rowFilter <- c(rowFilter, "Lower Fence", "Upper Fence", "Outlier Values")
        }
        
        filteredDf <- filter(df, rownames(df) %in% rowFilter)
        
        output$dsTableData <- renderDT(datatable(filteredDf,
                                                 extensions = 'RowGroup',
                                                 options = list(
                                                   rowGroup = list(dataSrc = 0),
                                                   columnDefs = list(list(visible=FALSE, targets=c(0))),
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
                                                 filter = "none",
                                                 
        ))
        
        outputOptions(output, "dsTableData", suspendWhenHidden = FALSE)
        
        
        if(input$dataInput == 'Upload Data')
        {
          for( x in input$dsUploadVars)
          {
            sampleData <- na.omit(as.data.frame(dsUploadData())[, x])
          }
        }
        else
        {
          sampleData <- dsRawData()
        }
        
        sample_df <- data.frame(sampleData, sampleData^2)
        names(sample_df) <- c("x", "x<sup>2</sup>")
        dfTotaled <- bind_rows(sample_df, summarise(sample_df, across(where(is.numeric), sum)))
        rownames(dfTotaled)[nrow(dfTotaled)] <- "Totals"
        
        output$sampleDataTable <- renderDT({
          datatable(round(dfTotaled, digits = 3),
                    options = list(dom = 't',
                                   pageLength = -1, 
                                   lengthMenu = list(c(-1, 10, 25, 50, 100), c("All", "10", "25", "50", "100")),
                                   autoWidth = TRUE,
                                   scrollX = TRUE
                    ),
                    rownames = FALSE,
                    escape = FALSE
          ) %>% formatStyle(
            names(dfTotaled),
            target = 'row',
            fontWeight = styleRow(dim(dfTotaled)[1], "bold")
          )
          
        })
        
        output$dsMeanCalc <- renderUI({
          tagList(
            withMathJax(),
            sprintf("\\( \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%s} = %s \\)",
                    dfTotaled['Totals', 1],
                    df['Observations', 3],
                    df['Mean', 3]),
            br(),
            br(),
            br()
          )
        })
        
        output$dsSDCalc <- renderUI({
          tagList(
            withMathJax(),
            sprintf("\\( s = \\sqrt{ \\dfrac{\\sum x^{2} - \\dfrac{(\\sum x)^{2}}{n} }{n - 1} } \\)"),
            sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\)",
                    dfTotaled['Totals', 2],
                    dfTotaled['Totals', 1],
                    df['Observations', 3],
                    df['Observations', 3],
                    df['Sample Standard Deviation', 3])
          )
        })
        
        if(input$dataInput == 'Upload Data')
        {
          for( x in input$dsUploadVars)
          {
            dat <- as.data.frame(dsUploadData())[, x]
          }
          colnames(df) <- c("Category", "Variable", input$dsUploadVars)
        }
        else
        {
          dat <- dsRawData()
        }
        
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
                                              margin = margin(5,0,0,0)),
                  axis.title.y = element_text(size = 16, 
                                              face = "bold", 
                                              vjust = 1.5,
                                              margin = margin(0,5,0,0)),
                  axis.text.x.bottom = element_text(size = 14,
                                                    margin = margin(5,0,0,0)),
                  axis.text.y.left = element_text(size = 14,
                                                  margin = margin(0,5,0,0)),
                  plot.margin = unit(c(1, 1, 1, 1),"cm"),
                  panel.border = element_rect(fill=NA))
          
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
        
        #---------------------- #
        #### Stem and Leaf ----
        #---------------------- #
        
        output$dsStemLeaf <- renderPrint({
          stem.leaf(dat, unit = 1, m = 1, depths = FALSE)
        })
        
        shinyjs::show(id = "outputPanel")
      } else {
        shinyjs::hide(id = "outputPanel")
      }
      # show(id = 'descriptiveStatsMP') 
    })
    
    dsTableProxy <- dataTableProxy('dsTableData')
    
    observeEvent(input$dsTableFilters, {
      req(dsReset() == FALSE)
      
      df <- getDsDataframe()
      
      rowFilter <- input$dsTableFilters
      
      if("Mode" %in% input$dsTableFilters && df['Mode',3] != "No mode exists."){
        rowFilter <- c(rowFilter, "Mode Frequency")
      } 
      
      if("Potential Outliers" %in% input$dsTableFilters){
        rowFilter <- c(rowFilter, "Lower Fence", "Upper Fence", "Outlier Values")
      }
      
      newFilter <- filter(df, rownames(df) %in% rowFilter)
      
      replaceData(dsTableProxy, newFilter, resetPaging = FALSE, rownames = FALSE)
    })
    
    
    #  -------------------------------------------------------------------- #
    #  ------------------------ Component Display -------------------------
    #  -------------------------------------------------------------------- #
    
    observeEvent({input$descriptiveStat
      input$dsUploadVars}, {
        shinyjs::hide(id = 'outputPanel')                
      })
    
    observeEvent(input$dataInput, {
      shinyjs::hide(id = 'outputPanel')
    })
    
    observeEvent(fileInputs$dsStatus, {
      if (fileInputs$dsStatus == "uploaded")
        show(id = "dsUploadVars")
      else
        hide(id = "dsUploadVars")
    })
    
    observe({
      if(is.null(input$dsGraphOptions)) {
        hideTab(inputId = 'dsTabset', target = 'Graphs')
        updateTabsetPanel(inputId = 'dsTabset', selected = 'Descriptive Statistics')
      } else {
        showTab(inputId = 'dsTabset', target = 'Graphs')
      }
    })
    
    observeEvent(input$goDescpStats, {
      shinyjs::show(id = "descriptiveStatsMP")
    })
    
    observeEvent(input$goDescpStats, {
      if(input$dataInput != "Upload Data"){
        hideTab(inputId = "dsTabset", target = "Uploaded Data")
      } else {
        showTab(inputId = "dsTabset", target = "Uploaded Data")
      }
      
    })
    
    observeEvent(input$resetAll,{
      dsReset(TRUE)
      shinyjs::hide(id = 'outputPanel')
      shinyjs::reset("inputPanel")
      fileInputs$dsStatus <- 'reset'
      updateTabsetPanel(session, "dsTabset", selected = "Descriptive Statistics")
    })
    
    #  -------------------------------------------------------------------- #
    
    # --------------------------------------------------------------------- #
    
    
    # **************************************************************************** #
    
  })
}
