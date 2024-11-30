# =========================================================================== #  
# ---- UI Components -------------------------------------------------------- 
# =========================================================================== #

regCorrUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
 #  ========================================================================= #  
 ## -------- Sidebar Panel -------------------------------------------------- 
 #  ========================================================================= #
      sidebarPanel(
        withMathJax(),
        shinyjs::useShinyjs(),
        div(
          id = ns("inputPanel"),
          
          radioButtons(
            inputId      = ns("simple_vs_multiple"),
            label        = strong("Regression Type"),
            choiceValues = list("SLR"#, 
                                #"MLR"
                                ), 
            choiceNames  = list("Simple Linear Regression and Correlation Analysis"#, 
                                #"Multiple Linear Regression"
                                ),
            selected     = "SLR", 
            inline       = TRUE),
          
 ### ------------ Simple Linear Regression ------------------------------------
          conditionalPanel(
            ns = ns,
            condition = "input.simple_vs_multiple == 'SLR'",
            
            radioButtons(
              inputId      = ns("dataRegCor"),
              label        = strong("Data"),
              choiceValues = list("Enter Raw Data", 
                                  "Upload Data"),
              choiceNames  = list("Enter Raw Data", 
                                  "Upload Data"),
              selected     = "Enter Raw Data", #character(0), #
              inline       = TRUE), #,width = '1000px'),
            
            conditionalPanel(
              ns = ns,
              condition = "input.dataRegCor == 'Enter Raw Data'",
              
              textAreaInput(
                inputId     = ns("x"), 
                label       = strong("\\( x\\) (Independent Variable)"), 
                value       = "10, 13, 18, 19, 22, 24, 27, 29, 35, 38", 
                placeholder = "Enter values separated by a comma with decimals as points", 
                rows        = 3),
              
              textAreaInput(
                inputId     = ns("y"), 
                label       = strong("\\( y\\) (Dependent Variable)"), 
                value       = "66, 108, 161, 177, 228, 235, 268, 259, 275, 278", 
                placeholder = "Enter values separated by a comma with decimals as points", 
                rows        = 3),
            ), #dataRegCor == 'Enter Raw Data'
            
            conditionalPanel(
              ns = ns,
              condition = "input.dataRegCor == 'Upload Data'",
              
              fileInput(
                inputId = ns("slrUserData"), 
                label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"), 
                accept  = c("text/csv",
                            "text/comma-separated-values", 
                            "text/plain", 
                            ".csv",
                            ".xls",
                            ".xlsx")),
              
              selectizeInput(
                inputId = ns("slrExplanatory"),
                label   = strong("Choose the Explanatory Variable (x)"),
                choices = c(""),
                options = list(placeholder = 'Select a variable',
                               onInitialize = I('function() { this.setValue(""); }'))),
              
              selectizeInput(
                inputId = ns("slrResponse"),
                label   = strong("Choose the Response Variable (y)"),
                choices = c(""),
                options = list(placeholder = 'Select a variable',
                               onInitialize = I('function() { this.setValue(""); }'))),
            ), #dataRegCor == 'Upload Data'
            
            br(),
            p(strong("Graph Options")),
            hr(),
            checkboxInput(
              inputId = ns("scatterPlot"), 
              label   = "Scatterplot of \\( x\\) versus \\( y\\)", 
              value   = TRUE),
            br(),
          ), #simple_vs_multiple == 'SLR'
          
 ### ------------ Multiple Linear Regression ----------------------------------
          conditionalPanel(
            ns = ns,
            condition = "input.simple_vs_multiple == 'MLR'",
            
            fileInput(
              inputId = ns("mlrUserData"),
              label   = "Upload your data (.csv or .xls or .xlsx or .txt)",
              accept  = c("text/csv",
                          "text/comma-separated-values",
                          "text/tab-separated-values",
                          "text/plain",
                          ".csv",
                          ".txt",
                          ".xls",
                          ".xlsx")),
            
            selectizeInput(
              inputId = ns("mlrExplanatory"),
              label   = strong("Choose the Explanatory Variables (x1, x2, x3, ...)"),
              choices = c(""),
              multiple = TRUE,
              options = list(placeholder = 'Select multiple variables',
                             onInitialize = I('function() { this.setValue(""); }'))),
            
            selectizeInput(
              inputId = ns("mlrResponse"),
              label   = strong("Choose the Response Variable (y)"),
              choices = c(""),
              options = list(placeholder = 'Select a variable',
                             onInitialize = I('function() { this.setValue(""); }')))
          ), #simple_vs_multiple == 'MLR'
          
          actionButton(
            inputId = ns("goRegression"),
            label = "Calculate",
            class = "act-btn"),
          
          actionButton(
            inputId = ns("resetRegCor"),
            label = "Reset Values",
            class = "act-btn")
        ) #inputPanel
      ), #sidebarPanel
    
 #  ========================================================================= #  
 ## -------- Main Panel ----------------------------------------------------- 
 #  ========================================================================= #
      mainPanel(
        div(
          id = ns("regCorrMP"),
         
 ### ------------ Simple Linear Regression ------------------------------------
          conditionalPanel(
            ns = ns,
            condition = "input.simple_vs_multiple == 'SLR'",
            
            uiOutput(ns("slrValidation")),
            
            div(
              id = ns("SLRData"),
              tabsetPanel(
                id       = ns("slrTabset"), 
                selected = "Simple Linear Regression",
                
 #### ---------------- SLR Tab ------------------------------------------------  
                tabPanel(
                  id    = ns("slr"), 
                  title = "Simple Linear Regression",
                    
                  conditionalPanel(
                    ns = ns,
                    condition = "input.scatterPlot == 1",
                      
                    titlePanel("Scatterplot"),
                    br(),
                    plotOptionsMenuUI(
                      id          = ns("slrScatter"),
                      plotType    = "Scatterplot",
                      title       = "Scatterplot",
                      xlab        = "x",
                      ylab        = "y",
                      dim         = "in px",
                      includeFlip = FALSE),
                    uiOutput(ns("renderSLRScatterplot")),
                    br(),
                    hr(),
                  ), #scatterPlot == 1
                    
                  titlePanel("Data"),
                  br(),
                  DTOutput(ns("slrDataTable"), width = "750px"),
                  br(),
                  hr(),
                    
                  titlePanel("Estimated equation of the regression line"),
                  br(),
                  uiOutput(ns('regLineEquation')),
                  
                  # verbatimTextOutput(ns("linearRegression")),
                  # br(),
                  # hr(),
                  #                                                    
                  # titlePanel("95% confidence interval for regression parameters"),
                  # br(),
                  # verbatimTextOutput(ns("confintLinReg")),
                  # br(),
                  # hr(),
                  #                                                    
                  # titlePanel("ANOVA for regression"),
                  # br(),
                  # verbatimTextOutput(ns("anovaLinReg")),
                  #br(),
                ), # slr tabpanel
                  
 #### ---------------- Normality of Residuals Tab -----------------------------
                # tabPanel(
                #   id    = ns("normality"), 
                #   title = "Normality of Residuals",
                #          
                #   #----------------------------------#
                #   # Tests for normality of residuals #
                #   #----------------------------------#
                #   titlePanel("Anderson-Darling test"),
                #   verbatimTextOutput(ns("AndersonDarlingTest")),
                #   br(),
                #          
                #   titlePanel("Kolmogorov-Smirnov test"),
                #   verbatimTextOutput(ns("KolmogorovSmirnovTest")),
                #   br(),
                #          
                #   titlePanel("Shapiro-Wilk test"),
                #   verbatimTextOutput(ns("ShapiroTest")),
                #   br(),
                # ),
                  
 #### ---------------- Residual Plots Tab -------------------------------------
                # tabPanel(
                #   id    = ns("resid"), 
                #   title = "Residual Plots",
                #   #-----------------------------#
                #   # Plots for Residual Analysis #
                #   #-----------------------------#
                #   titlePanel("Q-Q plot"),
                #   plotOutput(ns("qqplot"), width = "500px"),
                #   br(),
                #          
                #   titlePanel("Other diagnostic plots"),
                #   plotOutput(ns("moreplots"), width = "500px"),
                #   br(),
                # ),
                  

 #### ---------------- Correlation Coefficient Analysis Tab -------------------
                tabPanel(
                  id    = ns("correlation"), 
                  title = "Correlation Analysis",
                    
                  titlePanel("Pearson's Product-Moment Correlation"),
                  br(),
                  br(),
                  uiOutput(ns('pearsonCorFormula')),
                  br(),
                  # verbatimTextOutput(ns("PearsonCorTest")),
                  # br(),
                  # verbatimTextOutput(ns("PearsonConfInt")),
                  # br(),
                  hr(),
                    
                  titlePanel("Kendall's Rank Correlation"),
                  br(),
                  uiOutput(ns("kendallEstimate")),
                  br(),
                  hr(),
                    
                  titlePanel("Spearman's Rank Correlation"),
                  br(),
                  uiOutput(ns("spearmanEstimate")),
                  br(),
                  br()
                ), #correlation tabPanel
                  
 #### ---------------- Data File Tab ------------------------------------------
                tabPanel(
                  id    = ns("slrDataFile"),
                  title = "Uploaded Data", 
                  value = "Uploaded Data",
                    
                  titlePanel("Data File"),
                  br(),
                  br(),
                  div(
                    DTOutput(ns("slrViewUpload")), 
                    style = "width: 75%"
                  ),
                  br(),
                  br(),
                ), #slrDataFile tabpanel
              ), #slrTabset tabsetPanel
            ), #SLRData div
          ), #simple_vs_multiple == 'SLR'
 
 ### ------------ Multiple Linear Regression ----------------------------------
          conditionalPanel(
            ns = ns,
            condition = "input.simple_vs_multiple == 'MLR'",
            
            uiOutput(ns("mlrValidation")),
   
          ), #simple_vs_multiple == 'MLR'
        ) #regCorrMP
      ), #mainPanel
    ) #sidebarLayout
  ) # UI tagList
}

# =========================================================================== #  
# ---- Server Components ---------------------------------------------------- 
# =========================================================================== #

regCorrServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
 #  ========================================================================= #
 ## -------- Data Validation ------------------------------------------------
 #  ========================================================================= #
    regcor_iv <- InputValidator$new()
    slrraw_iv <- InputValidator$new()
    slrupload_iv <- InputValidator$new()
    slruploadvars_iv <- InputValidator$new()
    mlrupload_iv <- InputValidator$new()
    mlruploadvars_iv <- InputValidator$new()

 ### ------------ Rules -------------------------------------------------------
    slrraw_iv$add_rule("x", sv_required())
    slrraw_iv$add_rule("x", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                     "Data must be numeric values seperated by a comma (ie: 2,3,4)."))
    slrraw_iv$add_rule("x", ~ if(sampleInfoRaw()$diff != 0) "x and y must have the same number of observations.")
    slrraw_iv$add_rule("x", ~ if(sampleInfoRaw()$xSD == 0) "Not enough variance in Independent Variable.")
    
    slrraw_iv$add_rule("y", sv_required())
    slrraw_iv$add_rule("y", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                     "Data must be numeric values seperated by a comma (ie: 2,3,4)."))
    slrraw_iv$add_rule("y", ~ if(sampleInfoRaw()$diff != 0) "x and y must have the same number of observations.")
    slrraw_iv$add_rule("y", ~ if(sampleInfoRaw()$ySD == 0) "Not enough variance in Dependent Variable.")
    
    slrupload_iv$add_rule("slrUserData", sv_required())
    slrupload_iv$add_rule("slrUserData", ~ if(is.null(fileInputs$slrStatus) || fileInputs$slrStatus == 'reset') "Required")
    slrupload_iv$add_rule("slrUserData", ~ if(!(tolower(tools::file_ext(input$slrUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) == 0) "File is empty.")
    slrupload_iv$add_rule("slrUserData", ~ if(ncol(slrUploadData()) < 2) "Data must include one response and (at least) one explanatory variable.")
    slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) < 3) "Samples must include at least 2 observations.")
    # slrupload_iv$add_rule("slrUserData", ~ if(any(!is.numeric(slrUploadData()))) "File contains non-numeric data.")
    
    slruploadvars_iv$add_rule("slrExplanatory", sv_required())
    slruploadvars_iv$add_rule("slrExplanatory", ~ if(explanatoryInfoUploadSLR()$invalid) "Explanatory variable contains non-numeric data.")
    slruploadvars_iv$add_rule("slrExplanatory", ~ if(explanatoryInfoUploadSLR()$sd == 0) "Not enough variance in Explanatory Variable.")
    
    slruploadvars_iv$add_rule("slrResponse", sv_required())
    slruploadvars_iv$add_rule("slrResponse", ~ if(sampleDiffUpload() != 0) "Missing values detected, x and y must have the same number of observations.")
    slruploadvars_iv$add_rule("slrResponse", ~ if(responseInfoUploadSLR()$invalid) "Response variable contains non-numeric data.")
    slruploadvars_iv$add_rule("slrResponse", ~ if(responseInfoUploadSLR()$sd == 0) "Not enough variance in Response Variable.")

    mlrupload_iv$add_rule("mlrUserData", sv_required())
    mlrupload_iv$add_rule("mlrUserData", ~ if(is.null(fileInputs$mlrStatus) || fileInputs$mlrStatus == 'reset') "Required")
    mlrupload_iv$add_rule("mlrUserData", ~ if(!(tolower(tools::file_ext(input$mlrUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    mlrupload_iv$add_rule("mlrUserData", ~ if(nrow(mlrUploadData()) == 0) "File is empty.")
    mlrupload_iv$add_rule("mlrUserData", ~ if(ncol(mlrUploadData()) < 2) "Data must include one response and (at least) one explanatory variable.")
    
    mlruploadvars_iv$add_rule("mlrExplanatory", sv_required())
    mlruploadvars_iv$add_rule("mlrResponse", sv_required())
    
 ### ------------ Conditions --------------------------------------------------
    slrraw_iv$condition(~ isTRUE(input$dataRegCor == 'Enter Raw Data'))
    slrupload_iv$condition(~ isTRUE(input$dataRegCor == 'Upload Data'))
    slruploadvars_iv$condition(function() {isTRUE(input$dataRegCor == 'Upload Data' && 
                                                    slrupload_iv$is_valid()) })
    mlrupload_iv$condition(~ isTRUE(input$simple_vs_multiple == "MLR"))
    mlruploadvars_iv$condition(~ isTRUE(input$simple_vs_multiple == "MLR" &&
                                        mlrupload_iv$is_valid()))

 ### ------------ Dependencies ------------------------------------------------
    regcor_iv$add_validator(slrraw_iv)
    regcor_iv$add_validator(slrupload_iv)
    regcor_iv$add_validator(slruploadvars_iv)
    regcor_iv$add_validator(mlrupload_iv)
    regcor_iv$add_validator(mlruploadvars_iv)
    
    
 ### ------------ Activation --------------------------------------------------
    regcor_iv$enable()
    slrraw_iv$enable()
    slrupload_iv$enable()
    slruploadvars_iv$enable()
    mlrupload_iv$enable()
    mlruploadvars_iv$enable()
    
    
 #  ========================================================================= #    
 ## -------- Module Server Elements -----------------------------------------
 #  ========================================================================= #
    plotOptionsMenuServer("slrScatter")
    
    
 #  ========================================================================= #
 ## -------- Functions ------------------------------------------------------
 #  ========================================================================= #
    GetPlotHeight  <- function(plotToggle, pxValue, ui) {
      ifelse(plotToggle == 'in px' && !is.na(pxValue), 
             height <- pxValue, 
             height <- 400)
      
      ifelse(ui,
             return(paste0(height, "px")),
             return(height))
    }
    
    GetPlotWidth  <- function(plotToggle, pxValue, ui) {
      if(plotToggle == 'in px' && !is.na(pxValue)) {
        width <- pxValue
        
        if(ui) {
          width <- paste0(width, "px")
        }
      } else {
        width <- "auto"
      }
      return(width)      
    }
    
    
 #  ========================================================================= #
 ## -------- Reactives ------------------------------------------------------
 #  ========================================================================= #
    fileInputs <- reactiveValues(
      slrStatus = NULL,
    )
    
    mlrFileInputs <- reactiveValues(
      mlrStatus = NULL
    )
    
    slrUploadData <- eventReactive(input$slrUserData, {
      ext <- tools::file_ext(input$slrUserData$name)
      ext <- tolower(ext)
      
      switch(ext, 
             csv = read_csv(input$slrUserData$datapath, show_col_types = FALSE),
             xls = read_xls(input$slrUserData$datapath), 
             xlsx = read_xlsx(input$slrUserData$datapath),
             txt = read_tsv(input$slrUserData$datapath, show_col_types = FALSE),
             
             validate("Improper file format"))
    })
    
    mlrUploadData <- eventReactive(input$mlrUserData, {
      ext <- tools::file_ext(input$mlrUserData$name)
      ext <- tolower(ext)
      
      switch(ext, 
             csv = read_csv(input$mlrUserData$datapath, show_col_types = FALSE),
             xls = read_xls(input$mlrUserData$datapath), 
             xlsx = read_xlsx(input$mlrUserData$datapath),
             txt = read_tsv(input$mlrUserData$datapath, show_col_types = FALSE),
             
             validate("Improper file format"))
    })
    
    sampleInfoRaw <- eventReactive({input$x
                                    input$y}, {
        dat <- list()
        datx <- createNumLst(input$x)
        daty <- createNumLst(input$y)
        dat$diff <- length(datx) - length(daty)
        dat$xSD <- sd(datx)
        dat$ySD <- sd(daty)
        return(dat)
      })
    
    explanatoryInfoUploadSLR <- eventReactive(input$slrExplanatory, {
      dat <- list()
      datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
      dat$invalid <- any(!is.numeric(datx))
      dat$sd <- sd(datx, na.rm = TRUE)
      return(dat)
    })
    
    explanatoryInfoUploadMLR <- eventReactive(input$mlrExplanatory, {
      dat <- list()
      datx <- as.data.frame(mlrUploadData())[, input$mlrExplanatory]
      dat$invalid <- any(!is.numeric(datx))
      dat$sd <- sd(datx, na.rm = TRUE)
      return(dat)
    })
    
    responseInfoUploadSLR <- eventReactive(input$slrResponse, {
      dat <- list()
      daty <- as.data.frame(slrUploadData())[, input$slrResponse]
      dat$invalid <- any(!is.numeric(daty))
      dat$sd <- sd(daty, na.rm = TRUE)
      return(dat)
    })
    
    responseInfoUploadMLR <- eventReactive(input$mlrResponse, {
      dat <- list()
      daty <- as.data.frame(mlrUploadData())[, input$mlrResponse]
      dat$invalid <- any(!is.numeric(daty))
      dat$sd <- sd(daty, na.rm = TRUE)
      return(dat)
    })
    
    sampleDiffUpload <- eventReactive (c(input$slrExplanatory, 
                                         input$slrResponse), {
      if(input$slrResponse == "" | input$slrExplanatory == "") {
        return(0)
      } else {
        datx <- na.omit(as.data.frame(slrUploadData())[, input$slrExplanatory])
        daty <- na.omit(as.data.frame(slrUploadData())[, input$slrResponse])
        diff <- length(datx) - length(daty)
        return(diff)
      }
    })
    
    
 #  ========================================================================= #
 ## -------- Observers ------------------------------------------------------
 #  ========================================================================= #
    observeEvent(input$slrUserData, {
      hide(id = "regCorrMP")
      hide(id = "slrResponse")
      hide(id = "slrExplanatory")
      fileInputs$slrStatus <- 'uploaded'
      
      if(slrupload_iv$is_valid())
      {
        freezeReactiveValue(input, "slrExplanatory")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "slrExplanatory",
                          choices = c(colnames(slrUploadData()))
        )
        freezeReactiveValue(input, "slrResponse")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "slrResponse",
                          choices = c(colnames(slrUploadData()))
        )
        show(id = "slrResponse")
        show(id = "slrExplanatory")
      }
    })
    
    observeEvent(input$mlrUserData, {
      hide(id = "mlrResponse")
      hide(id = "mlrExplanatory")
      fileInputs$mlrStatus <- 'uploaded'
      
      if(mlrupload_iv$is_valid())
      {
        freezeReactiveValue(input, "mlrExplanatory")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "mlrExplanatory",
                          choices = c(colnames(mlrUploadData()))
        )
        freezeReactiveValue(input, "mlrResponse")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "mlrResponse",
                          choices = c(colnames(mlrUploadData()))
        )
        show(id = "mlrResponse")
        show(id = "mlrExplanatory")
      }
    })
    
    observeEvent(input$slrExplanatory, {
      updateTextInput(inputId = "xlab", value = input$slrExplanatory)
    })
    
    observeEvent(input$slrResponse, {
      updateTextInput(inputId = "ylab", value = input$slrResponse)
    })
    
    observeEvent(input$goRegression, {
      
      ### SLR Validation messages ----
      if(input$simple_vs_multiple == 'SLR') {
        
        if(regcor_iv$is_valid()) {
          show(id = "SLRData")
        } else {
          hide(id = "SLRData")
        }
        
        output$slrValidation <- renderUI({
          
          if(!slrupload_iv$is_valid()) {
            
            if(is.null(input$slrUserData)) {
              validate("Please upload a file.")
            }
            
            validate(
              need(!is.null(fileInputs$slrStatus) && fileInputs$slrStatus == 'uploaded', "Please upload a file."),
              errorClass = "myClass")
            
            validate(
              need(nrow(slrUploadData()) != 0, "File is empty."),
              need(ncol(slrUploadData()) > 1, "Data must include one response and (at least) one explanatory variable."),
              need(nrow(slrUploadData()) > 2, "Samples must include at least 2 observations."),
              errorClass = "myClass")
          } 
          
          if(!slruploadvars_iv$is_valid()) {
            validate(
              need(input$slrExplanatory != "", "Please select an Explanatory Variable (x)."),
              need(input$slrResponse != "", "Please select a Response Variable (y).") %then%
                need(sampleDiffUpload() == 0, "The Explanatory (x) and Response (y) variables must have the same number of observations."),
              errorClass = "myClass")
            
            validate(
              need(!explanatoryInfoUploadSLR()$invalid, "The Explanatory Variable (x) contains non-numeric data.") %then%
                need(explanatoryInfoUploadSLR()$sd != 0, "The data for the Explanatory Variable (x) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
              need(!responseInfoUploadSLR()$invalid, "The Response Variable (y) contains non-numeric data.") %then%
                need(responseInfoUploadSLR()$sd != 0, "The data for the Response Variable (y) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
              errorClass = "myClass")
          } 
          
          if(input$dataRegCor == 'Upload Data') {
            req(slruploadvars_iv$is_valid())
            datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
            daty <- as.data.frame(slrUploadData())[, input$slrResponse]
          } else {
            validate(
              need(input$x, "Input required for the Independent Variable (x)."),
              need(input$y, "Input required for the Dependent Variable (y)."),
              errorClass = "myClass")
            
            datx <- createNumLst(input$x)
            daty <- createNumLst(input$y)
          }
          
          validate(
            need(length(datx) >= 2, "Must have at least 2 observations for x."),
            need(length(daty) >= 2, "Must have at least 2 observations for y."),
            need(!anyNA(datx), "Data must be numeric."),
            need(!anyNA(daty), "Data must be numeric."),
            need(length(datx) == length(daty), "x and y must have the same number of observations."),
            errorClass = "myclass")
          
          if(!slrraw_iv$is_valid()) {
            validate(
              need(sampleInfoRaw()$xSD != 0, "The data for the Independent Variable (x) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
              need(sampleInfoRaw()$ySD != 0, "The data for the Dependent Variable (y) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
              errorClass = "myClass")
          }
        }) #output$slrValidation

        
        if(regcor_iv$is_valid()) {
          
          if(input$dataRegCor == 'Upload Data') {
            datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
            daty <- as.data.frame(slrUploadData())[, input$slrResponse]
          } else {
            datx <- createNumLst(input$x)
            daty <- createNumLst(input$y)
          }
          
          model <- lm(daty ~ datx)
          
          df <- data.frame(datx, daty, datx*daty, datx^2, daty^2)
          names(df) <- c("x", "y", "xy", "x<sup>2</sup>", "y<sup>2</sup>")
          dfTotaled <- bind_rows(df, summarise(df, across(where(is.numeric), sum)))
          rownames(dfTotaled)[nrow(dfTotaled)] <- "Totals"
          
          sumXSumY <- dfTotaled["Totals", "x"] * dfTotaled["Totals", "y"]
          sumXSqrd <- dfTotaled["Totals", "x"] ^ 2
          sumYSqrd <- dfTotaled["Totals", "y"] ^ 2
          
          output$slrDataTable <- renderDT(
            datatable(round(dfTotaled, digits = 3),
                      options = list(pageLength = -1, 
                                     lengthMenu = list(c(-1, 10, 25, 50, 100), c("All", "10", "25", "50", "100"))
                      ),
                      escape = FALSE
            ) %>% formatStyle(names(dfTotaled),
                              target = 'row',
                              fontWeight = styleRow(dim(dfTotaled)[1], "bold"))
          )
          
          output$renderSLRScatterplot <- renderUI({
            tagList(
              plotOutput(session$ns("slrScatterplot"),
                         height = GetPlotHeight(input[["slrScatter-Height"]], input[["slrScatter-HeightPx"]], ui = TRUE),
                         width = GetPlotWidth(input[["slrScatter-Width"]], input[["slrScatter-WidthPx"]], ui = TRUE)),
            )
          })
          
          output$slrScatterplot <- renderPlot({ # scatterplot ----
            RenderScatterplot(df, 
                              input[["slrScatter-Title"]], 
                              input[["slrScatter-Xlab"]], 
                              input[["slrScatter-Ylab"]], 
                              input[["slrScatter-Colour"]],
                              input[["slrScatter-PointsColour"]],
                              input[["slrScatter-LineWidth"]],
                              input[["slrScatter-PointSize"]],
                              input[["slrScatter-Gridlines"]])
            
          }, height = function() {GetPlotHeight(input[["slrScatter-Height"]], input[["slrScatter-HeightPx"]], ui = FALSE)},
             width = function() {GetPlotWidth(input[["slrScatter-Width"]], input[["slrScatter-WidthPx"]], ui = FALSE)})
          
          if(summary(model)$coefficients["datx", "Estimate"] > 0){
            slopeDirection <- "increase"
            yHatOp <- "+"
            b0HatOp <- "-"
          } else {
            slopeDirection <- "decrease"
            yHatOp <- "-"
            b0HatOp <- "+"
          }
          
          interceptEstimate <- round(summary(model)$coefficients["(Intercept)", "Estimate"], 4)
          slopeEstimate <- round(summary(model)$coefficients["datx", "Estimate"], 4)
          
          output$regLineEquation <- renderUI({
            withMathJax()
            p(
              withMathJax(),
              p("The estimated equation of the regression line is given by "),
              sprintf("\\( \\qquad \\hat{y} = \\hat{\\beta}_{0} + \\hat{\\beta}_{1} x \\)"),
              br(),
              br(),
              p("where"),
              sprintf("\\( \\qquad \\hat{\\beta}_{1} = \\dfrac{ \\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } \\)"),
              sprintf("\\( \\, = \\, \\dfrac{ %g - \\dfrac{ (%g)(%g) }{ %g } }{ %g - \\dfrac{ (%g)^2 }{ %g } } \\)",
                      dfTotaled["Totals", "xy"],
                      dfTotaled["Totals", "x"],
                      dfTotaled["Totals", "y"],
                      length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"],
                      dfTotaled["Totals", "x"],
                      length(datx)),
              sprintf("\\( \\, = \\, \\dfrac{ %g - \\dfrac{ %g }{ %g } }{ %g - \\dfrac{ %g }{ %g } } \\)",
                      dfTotaled["Totals", "xy"],
                      sumXSumY,
                      length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"],
                      sumXSqrd,
                      length(datx)),
              sprintf("\\( \\, = \\, \\dfrac{ %g - %g }{ %g - %g } \\)",
                      dfTotaled["Totals", "xy"],
                      sumXSumY / length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"],
                      sumXSqrd / length(datx)),
              sprintf("\\( \\, = \\, \\dfrac{ %g }{ %g } \\)",
                      dfTotaled["Totals", "xy"] - (sumXSumY) / length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx)),
              sprintf("\\( \\, = \\, %0.4f \\)",
                      slopeEstimate),
              br(),
              br(),
              p("and"),
              sprintf("\\( \\qquad \\hat{\\beta}_{0} = \\bar{y} - \\hat{\\beta}_{1} \\bar{x}\\)"),
              sprintf("\\( \\, = \\, %g - (%0.4f) (%g) \\)",
                      mean(daty),
                      summary(model)$coefficients["datx", "Estimate"],
                      mean(datx)),
              sprintf("\\( \\, = \\, %g %s %0.4f\\)",
                      mean(daty),
                      b0HatOp,
                      abs(slopeEstimate) * mean(datx)),
              sprintf("\\( \\, = \\, %0.4f \\)",
                      interceptEstimate),
              br(),
              br(),
              br(),
              sprintf("\\( \\hat{y} = %0.4f %s %0.4f x \\)",
                      interceptEstimate,
                      yHatOp,
                      abs(slopeEstimate)),
              br(),
              br(),
              br(),
              p(tags$b("Interpretation of regression coefficients:"),
                br(),
                br(),
                "Within the scope of observation, ", interceptEstimate, " is the estimated value of ", 
                em("y"), " when ", em("x"), "= 0. A slope of ", slopeEstimate, 
                " represents the estimated ", slopeDirection, " in ", em("y"), 
                " for a unit increase of ", em("x.")),
              # sprintf("Within the scope of observation, %s is the estimated value of ",
              #         interceptEstimate),
              
              # sprintf("when x = 0. A slope of %s represents the estimated %s in y for a 
              #         unit increase of x.",
              #         
              #         slopeEstimate,
              #         slopeDirection),
              br(),
              br()
            )
          })
          
          output$linearRegression <- renderPrint({ 
            summary(model)
          })
          
          output$confintLinReg <- renderPrint({ 
            confint(model) # Prints the 95% CI for the regression parameters
          })
          
          output$anovaLinReg <- renderPrint({ 
            anova(model) # Prints the ANOVA table
          })
          
          #----------------------------------#
          # Tests for normality of residuals #
          #----------------------------------#
          
          # Anderson-Darling Normality Test 
          # output$AndersonDarlingTest <- renderPrint({ 
          #   ad.test(model$residuals)
          # })
          #   
          # # Kolmogorov-Smirnov Normality Test 
          # output$KolmogorovSmirnovTest <- renderPrint({ 
          #   ks.test(model$residuals, "pnorm")
          # })
          #   
          # # Shapiro-Wilk Normality Test 
          # output$ShapiroTest <- renderPrint({ 
          #   shapiro.test(model$residuals) 
          # })
          #   
          # # Q-Q plot for residuals
          # output$qqplot <- renderPlot({
          #   #qqnorm(model$residuals, ylab = "Residuals", xlab = "Z Scores", main = "Q-Q plot of Standardized Residuals", pch = 19) #+
          #   #qqline(model$residuals)
          #   qqPlot(model$residuals, main = "Q-Q Plot", xlab = "Z Scores",  ylab = "Residuals", pch = 19) 
          # })
          #   
          # output$moreplots <- renderPlot({
          #   par(mfrow = c(2, 2))
          #   plot(model, which = 1:4, pch = 19)
          # })
          
          
          req(length(datx) > 1) ## correlation coefficient ----
          if(length(datx) > 2) {
            
            pearson <- cor.test(datx, daty, method = "pearson")
            
            if(!is.na(pearson$estimate)) {
              if(pearson$estimate < 0) {
                pearsonSign <- "negative"
              } else {
                pearsonSign <- "positive"
              }
              
              if(abs(pearson$estimate) > 0.6) {
                pearsonStrength <- "strong"
              } else if (abs(pearson$estimate) > 0.3) {
                pearsonStrength <- "moderate"
              } else {
                pearsonStrength <- "weak"
              }
              
              output$pearsonCorFormula <- renderUI({
                p(
                  withMathJax(),
                  sprintf("\\( r \\; = \\; \\dfrac
                                      {\\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }
                                      {\\sqrt{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } \\sqrt{ \\sum y^2 - \\dfrac{ (\\sum y) ^2 }{ n } } } \\)"),
                  br(),
                  br(),
                  br(),
                  sprintf("\\( \\quad = \\; \\dfrac
                                      {%g - \\dfrac{ (%g)(%g) }{ %g } }
                                      {\\sqrt{ %g - \\dfrac{ (%g)^2 }{ %g } } \\sqrt{ %g - \\dfrac{ (%g) ^2 }{ %g } } } \\)",
                          dfTotaled["Totals", "xy"],
                          dfTotaled["Totals", "x"],
                          dfTotaled["Totals", "y"],
                          length(datx),
                          dfTotaled["Totals", "x<sup>2</sup>"],
                          dfTotaled["Totals", "x"],
                          length(datx),
                          dfTotaled["Totals", "y<sup>2</sup>"],
                          dfTotaled["Totals", "y"],
                          length(datx)),
                  br(),
                  br(),
                  br(),
                  
                  sprintf("\\( \\quad = \\; \\dfrac
                                      { %g }
                                      {\\sqrt{ %g } \\sqrt{ %g } } \\)",
                          dfTotaled["Totals", "xy"] - sumXSumY / length(datx),
                          dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx),
                          dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx)),
                  
                  sprintf("\\( = \\; \\dfrac
                                      { %g }
                                      { %g } \\)",
                          dfTotaled["Totals", "xy"] - sumXSumY / length(datx),
                          sqrt(dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx)) * sqrt(dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx))),
                  
                  sprintf("\\( = \\; %0.4f \\)",
                          pearson$estimate),
                  br(),
                  br(),
                  br(),
                  p(tags$b("Interpretation:")),
                  sprintf("There is a %s %s linear relationship between \\(x\\) and \\(y\\).",
                          pearsonStrength,
                          pearsonSign),
                  br()
                )
              })
              
              output$PearsonCorTest <- renderPrint({ 
                pearson
              })
              
              if(length(datx) > 3) {
                output$PearsonConfInt <- renderPrint({ 
                  pearson$conf.int
                })
              } else {
                output$PearsonConfInt <- renderPrint ({
                  noquote("Computation of the Confidence Interval requires a minimum sample size of 4.")
                })
              }
            }
            
            
            # output$PearsonEstimate <- renderPrint({
            #   cat(noquote(paste(c("Pearson's r:", round(pearson$estimate[[1]], 4)))))
            # })
          } else {
            output$PearsonCorTest <- renderPrint ({
              noquote("Pearson's Product-Moment Correlation requires a minimum sample size of 3 for computation.")
            })
          }
          
          kendall <- cor.test(datx, daty, method = "kendall")
          spearman <- cor.test(datx, daty, method = "spearman")
          
          output$kendallEstimate <- renderUI({
            sprintf("\\( \\tau \\; = \\; %0.4f \\)", 
                    kendall$estimate)
          })
          
          output$spearmanEstimate <- renderUI({
            sprintf("\\( \\displaystyle r_{s} \\; = \\; 1 - \\dfrac{ 6 \\, \\sum\\limits_{i=1}^n d^2_{i}}{ n(n^2 - 1)} \\; = \\; %0.4f \\)", 
                    spearman$estimate)
          })
          
          output$slrViewUpload <- renderDT({
            req(slrupload_iv$is_valid())
            datatable(slrUploadData(),
                      options = list(pageLength = -1,
                                     lengthMenu = list(c(25, 50, 100, -1),
                                                       c("25", "50", "100", "all"))))
          })
          
        } #if regcor_iv is valid
      }
      
### MLR Validation messages ----      
      else if (input$simple_vs_multiple == "MLR") {
        
        output$mlrValidation <- renderUI({
          
          if(!mlrupload_iv$is_valid()) {
            
            if(is.null(input$mlrUserData)) {
              validate("Please upload a file.")
            }
          }
          
          
          if(!mlruploadvars_iv$is_valid()) {
            validate(
              need(input$mlrExplanatory != "", "Please select at least one Explanatory Variable (x)."),
              need(input$mlrResponse != "", "Please select a Response Variable (y)."),
              
              errorClass = "myClass"
            )
          } 
          
        })
        
      }
      
      show(id = "regCorrMP")
    }) # input$goRegression
    
 ### ------------ Component Display -------------------------------------------
    observeEvent(!regcor_iv$is_valid(), {
      hide(id = "regCorrMP")
      hide(id = "SLRData")
    })
    
    observeEvent(input$dataRegCor, {
      hide(id = "regCorrMP")
      hide(id = "slrResponse")
      hide(id = "slrExplanatory")
      updateTextInput(inputId = "xlab", value = "x")
      updateTextInput(inputId = "ylab", value = "y")
    })
    
    observeEvent(input$simple_vs_multiple, {
      
      if (!mlrupload_iv$is_valid()) {
        hide(id = "mlrResponse")
        hide(id = "mlrExplanatory")
      }
      
    })
    
    observe({
      if(input$dataRegCor == 'Enter Raw Data') {
        hideTab(inputId = "slrTabset", target = "Uploaded Data")
        updateTabsetPanel(inputId = 'slrTabset', selected = 'Simple Linear Regression')
      } else {
        showTab(inputId = "slrTabset", target = "Uploaded Data")
      }
    })
    
    observeEvent(input$resetRegCor, {
      # hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
      # hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
      # hideTab(inputId = 'tabSet', target = 'Residual Plots')
      hide(id = "regCorrMP")
      shinyjs::reset("inputPanel")
      fileInputs$slrStatus <- 'reset'
      fileInputs$mlrStatus <- 'reset'
    })
 
  })
}
