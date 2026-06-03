options(scipen=999)
source("R/RenderBoxplot.R")
source("R/RenderMeanPlot.R")
source("R/RenderQQPlot.R")
source("R/RenderScatterplot.R")
source("R/RenderSideBySideBoxplot.R")
source("R/utilityFunctions.R")
source('R/plotOptionsMenu.R')

# =========================================================================== #
# ---- UI Components --------------------------------------------------------
# =========================================================================== #
SLRMainPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(withMathJax(
    useShinyjs(),
    tags$style(HTML("         # Message for disabling when perfect fit occurs
      .disabled-tab {
        pointer-events: none !important;
        opacity: 0.4 !important;
        cursor: not-allowed !important;
      }
    ")),
    hidden(div(
      id = ns("regCorrMP"), # This div is hidden/shown
      uiOutput(ns("perfectFitWarning")), # added to trap perfect fit
      uiOutput(ns("slrValidation")),
      
      div(
        id = ns("SLRData"), # This div will now contain the navbarPage
        navbarPage(
          title = NULL, # Title for the navbarPage
          id = ns("slrNavbarPage"),
          theme = bs_theme(version = 4),

          #### ---------------- SLR Tab ------------------------------------------------
          tabPanel(
            title = "Model",
            value = "Model",
            
            titlePanel("Estimated equation of the regression line"),
            br(),
            uiOutput(ns('regLineEquation')),
            br()
          ), # slr tabpanel
          
          #### ---------------- Scatterplot Tab --------------------------------------
          tabPanel(
            title = "Scatterplot",
            value = "Scatterplot",
            
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
              
              plotlyOutput(ns("slrScatterplot"),
                           height = "700px",
                           width  = "100%"),
              
              br()
            )
          ), # Scatterplot tabpanel
          
          #### ---------------- Calculations Tab -------------------------------------
          tabPanel(
            title = "Calculations",
            value = "Calculations",
            
            div(
              style = "display: flex; gap: 10px; margin-bottom: 8px;",
              #downloadButton(ns("downloadSLRcsv"),  "Save as CSV"),
              downloadButton(ns("downloadSLRxlsx"), "Save as Excel")
            ),
            
            div(
              style = "overflow-x: auto;",
              reactableOutput(ns("slrDataTable"), width = "100%")
            ),
            br()
          ), # Calculations tabpanel
          
          #### ---------------- Inference Tab -------------------
          tabPanel(
            title = "Inference",
            value = "Inference",
            
            tags$style(HTML("
    .inference-anova-tabs .nav-tabs {
      border-bottom: none;
      background-color: #f8f9fa;
      display: flex;
      padding: 0;
      margin-bottom: 16px;
    }
    .inference-anova-tabs .nav-tabs > li > a {
      color: #18536F;
      font-weight: bold;
      font-size: 15px;
      border: none !important;
      border-radius: 0 !important;
      padding: 10px 24px;
      background-color: #f8f9fa !important;
    }
    .inference-anova-tabs .nav-tabs > li.active > a,
    .inference-anova-tabs .nav-tabs > li.active > a:focus,
    .inference-anova-tabs .nav-tabs > li.active > a:hover,
    .inference-anova-tabs .nav-tabs > li > a.active,
    .inference-anova-tabs .nav-tabs > li > a.active:focus,
    .inference-anova-tabs .nav-tabs > li > a.active:hover {
      background-color: #18536F !important;
      color: white !important;
      border: none !important;
      border-radius: 0 !important;
      font-weight: bold !important;
    }
    .inference-anova-tabs .nav-tabs > li > a:hover {
      background-color: #d0dce8 !important;
      color: #1a3a5c !important;
    }
  ")),
            
            div(
              class = "inference-anova-tabs",
              tabsetPanel(
                type = "tabs",
                id   = ns("inferenceAnovaTabs"),
                
                tabPanel(
                  title = "Parameter Estimates",
                  value = "Parameter Estimates",
                  br(),
                  uiOutput(ns("slrInferenceDetails"))
                ),
                
                tabPanel(
                  title = "ANOVA",
                  value = "ANOVA",
                  br(),
                  uiOutput(ns("anovaHypotheses")),
                  br(),
                  div(tableOutput(ns("anovaTable")), width = "100 px;"),
                  br(),
                  br(),
                  plotOutput(ns("anovaFCurve")),
                  br(),
                  br(),
                  uiOutput(ns("anovaConclusion")),
                  br(),
                  uiOutput(ns("anovaR2")),
                  br(),
                  hr()
                ),
                
                tabPanel(
                  title = "LINE",
                  value = "LINE",
                  br(),
                  uiOutput(ns("lineAssumptions"))
                )
              )
            )
          ), # Inference tabpanel
            
    
          
          #### ---------------- Diagnostic Plots Tab ---------------------------------
          tabPanel(
            title = "Diagnostic Plots",
            value = "Diagnostic Plots",
            fluidPage(
              plotOutput(ns("slrResidualsPanelPlot1")),
              plotOutput(ns("slrResidualsPanelPlot2")),
              plotOutput(ns("slrResidualsPanelPlot3")),
              plotOutput(ns("slrResidualsPanelPlot4")),
              plotOutput(ns("slrResidualsPanelPlot5"))
            )
          ), # Diagnostic Plots tabpanel
          
          #### ---------------- Correlation Coefficient Analysis Tab -------------------
          tabPanel(
            title = "Correlation Analysis",
            value = "Correlation Analysis",
            
            tags$style(HTML("
    .correlation-tabs .nav-tabs {
      border-bottom: none;
      background-color: #f8f9fa;
      display: flex;
      padding: 0;
      margin-bottom: 16px;
    }
    .correlation-tabs .nav-tabs > li > a {
      color: #18536F;
      font-weight: bold;
      font-size: 15px;
      border: none !important;
      border-radius: 0 !important;
      padding: 10px 24px;
      background-color: #f8f9fa !important;
    }
    .correlation-tabs .nav-tabs > li.active > a,
    .correlation-tabs .nav-tabs > li.active > a:focus,
    .correlation-tabs .nav-tabs > li.active > a:hover,
    .correlation-tabs .nav-tabs > li > a.active,
    .correlation-tabs .nav-tabs > li > a.active:focus,
    .correlation-tabs .nav-tabs > li > a.active:hover {
      background-color: #18536F !important;
      color: white !important;
      border: none !important;
      border-radius: 0 !important;
      font-weight: bold !important;
    }
    .correlation-tabs .nav-tabs > li > a:hover {
      background-color: #d0dce8 !important;
      color: #1a3a5c !important;
    }
  ")),
            
            div(
              class = "correlation-tabs",
              tabsetPanel(
                type = "tabs",
                id   = ns("correlationTabs"),
                
                tabPanel(
                  title = "Pearson",
                  value = "Pearson",
                  titlePanel("Pearson's Correlation Coefficient"),
                  br(),
                  br(),
                  uiOutput(ns('pearsonCorFormula')),
                  br(),
                  hr()
                ),
                
                tabPanel(
                  title = "Kendall",
                  value = "Kendall",
                  titlePanel("Kendall's Rank Correlation Coefficient"),
                  br(),
                  uiOutput(ns("kendallFormula")),
                  br(),
                  hr()
                ),
                
                tabPanel(
                  title = "Spearman",
                  value = "Spearman",
                  titlePanel("Spearman's Rank Correlation Coefficient"),
                  br(),
                  uiOutput(ns("spearmanEstimate")),
                  br(),
                  br(),
                  hr()
                )
                
              ) ## Nested tabsetPanel
            )
            
          ), ## Correlation Analysis tabPanel
          #### ---------------- Data File Tab ------------------------------------------
          tabPanel(
            title = "Uploaded Data",
            value = "Uploaded Data",
            
            # titlePanel("Data File"),
            # br(),
            # br(),
            div(
              DTOutput(ns("slrViewUpload")),
              style = "width: 75%"
            ),
            br(),
            br(),
          ) #slrDataFile tabpanel
        ) #slrNavbarPage navbarPage
      ) # SLRData div
    )) # regCorrMP div (hidden)
  )) # tagList withMathJax
}

SLRSidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(withMathJax(div(
    id = ns("inputPanel"),
    
    ### ------------ Simple Linear Regression (SLR) ------------------------------------
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
        inputId     = ns("y"),
        label       = strong("Response Variable (\\( y\\))"),
        value       = "4, 14, 15, 18, 21, 26, 38",
        placeholder = "Enter numeric values separated by a comma with decimals as points. (eg: 1,2,3)",
        rows        = 3),
      
      textAreaInput(
        inputId     = ns("x"),
        label       = strong("Explanatory Variable (\\( x\\))"),
        value       = "61, 111, 125, 134, 169, 173, 244",
        placeholder = "Enter numeric values separated by a comma with decimals as points (eg: 1,2,3).",
        rows        = 3)
    ), #dataRegCor == 'Enter Raw Data'
    
    div(
      id = "userUploadedData",
      conditionalPanel(
        ns = ns,
        condition = "input.dataRegCor == 'Upload Data'",
        
        HTML(uploadDataDisclaimer),
        
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
          inputId = ns("slrResponse"),
          label   = strong("Choose the Response Variable (\\(y\\))"),
          choices = c(""),
          options = list(placeholder = 'Select a variable',
                         onInitialize = I('function() { this.setValue(""); }'))),
        
        selectizeInput(
          inputId = ns("slrExplanatory"),
          label   = strong("Choose the Explanatory Variable (\\(x\\))"),
          choices = c(""),
          options = list(placeholder = 'Select a variable',
                         onInitialize = I('function() { this.setValue(""); }')))
      )), #dataRegCor == 'Upload Data'
    
    br(),
    p(strong("Graph Options")),
    hr(),
    checkboxInput(
      inputId = ns("scatterPlot"),
      label   = "Scatterplot of \\( x\\) versus \\( y\\)",
      value   = TRUE),
    br(),
    
    actionButton(
      inputId = ns("goRegression"),
      label = "Calculate",
      class = "act-btn"),
    
    actionButton(
      inputId = ns("resetRegCor"),
      label = "Reset Values",
      class = "act-btn")
  )))
}

SLRServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ============================================================
    # LINE Assumption Tests Config
    # Add, remove, or reorder tests here freely
    # Each entry: assumption, procedure, test function, min_n
    # ============================================================
    lineTestConfig <- list(
      
      list(
        assumption = "Linearity",
        procedure  = "Residuals vs Fitted Plot",
        min_n      = 3,
        run        = function(model, datx, daty) {
          list(
            statistic = NULL,
            p_value   = NULL,
            note      = "See Residuals vs Fitted plot in Diagnostic Plots tab"
          )
        }
      ),
      
      list(
        assumption = "Linearity",
        procedure  = "Rainbow Test",
        min_n      = 4,
        run        = function(model, datx, daty) {
          rb <- lmtest::raintest(model)
          list(
            statistic = round(rb$statistic, 4),
            p_value   = round(rb$p.value, 4),
            note      = NULL
          )
        }
      ),
      
      list(
        assumption = "Linearity",
        procedure  = "Ramsey RESET Test",
        min_n      = 5,
        run        = function(model, datx, daty) {
          rt <- lmtest::resettest(model)
          list(
            statistic = round(rt$statistic, 4),
            p_value   = round(rt$p.value, 4),
            note      = NULL
          )
        }
      ),
      
      list(
        assumption = "Independence",
        procedure  = "Durbin-Watson Test",
        min_n      = 4,
        run        = function(model, datx, daty) {
          dw <- lmtest::dwtest(model)
          list(
            statistic = round(dw$statistic, 4),
            p_value   = round(dw$p.value, 4),
            note      = NULL
          )
        }
      ),
      
      list(
        assumption = "Normality",
        procedure  = "Shapiro-Wilk Test",
        min_n      = 3,
        run        = function(model, datx, daty) {
          sw <- shapiro.test(residuals(model))
          list(
            statistic = round(sw$statistic, 4),
            p_value   = round(sw$p.value, 4),
            note      = NULL
          )
        }
      ),
      
      list(
        assumption = "Normality",
        procedure  = "Anderson-Darling Test",
        min_n      = 3,
        run        = function(model, datx, daty) {
          ad <- nortest::ad.test(residuals(model))
          list(
            statistic = round(ad$statistic, 4),
            p_value   = round(ad$p.value, 4),
            note      = NULL
          )
        }
      ),
      
      list(
        assumption = "Normality",
        procedure  = "Kolmogorov-Smirnov Test",
        min_n      = 3,
        run        = function(model, datx, daty) {
          ks <- ks.test(residuals(model), "pnorm",
                        mean = mean(residuals(model)),
                        sd   = sd(residuals(model)))
          list(
            statistic = round(ks$statistic, 4),
            p_value   = round(ks$p.value, 4),
            note      = NULL
          )
        }
      ),
      
      list(
        assumption = "Equal Variance (Homoskedasticity)",
        procedure  = "Breusch-Pagan Test",
        min_n      = 4,
        run        = function(model, datx, daty) {
          bp <- lmtest::bptest(model)
          list(
            statistic = round(bp$statistic, 4),
            p_value   = round(bp$p.value, 4),
            note      = NULL
          )
        }
      ),
      
      list(
        assumption = "Equal Variance (Homoskedasticity)",
        procedure  = "White Test",
        min_n      = 6,
        run        = function(model, datx, daty) {
          # White test via auxiliary regression of squared residuals
          e2  <- residuals(model)^2
          x2  <- datx^2
          aux <- lm(e2 ~ datx + x2)
          wt  <- summary(aux)
          f   <- wt$fstatistic
          p   <- pf(f[1], f[2], f[3], lower.tail = FALSE)
          list(
            statistic = round(f[1], 4),
            p_value   = round(p, 4),
            note      = NULL
          )
        }
      )
    )
    
    
    
    slrExportData <- reactiveVal(NULL)
    
    
    # output$downloadSLRcsv <- downloadHandler(
    #   filename    = function() paste0("SLR_Calculations", Sys.Date(), ".csv"),
    #   contentType = "text/csv",
    #   content     = function(file) {
    #     tryCatch({
    #       data <- as.data.frame(slrExportData())
    #       names(data) <- c("x", "y", "xy", "x^2", "y^2", "y_hat",
    #                        "e = (y - y_hat)", "e^2",
    #                        "95% CI Mean Response (Lower)",
    #                        "95% CI Mean Response (Upper)",
    #                        "95% PI (Lower)",
    #                        "95% PI (Upper)")
    #       write.csv(data, file, row.names = TRUE)
    #     }, error = function(e) {
    #       message("Full error: ", conditionMessage(e))
    #     })
    #   }
    # )
    output$downloadSLRxlsx <- downloadHandler(
      filename    = function() paste0("SLR_Calculations", Sys.Date(), ".xlsx"),
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content     = function(file) {
        tryCatch({
          data <- as.data.frame(slrExportData())
          data <- head(data, nrow(data) - 1)  
          names(data) <- c("x", "y", "xy", "x^2", "y^2", "y_hat",
                           "e = (y - y_hat)", "e^2",
                           "95% CI Mean Response (Lower)",
                           "95% CI Mean Response (Upper)",
                           "95% PI (Lower)",
                           "95% PI (Upper)")
          writexl::write_xlsx(data, file)
        }, error = function(e) {
          message("Full error: ", conditionMessage(e))
        })
      }
    )
    
    
    
    
    
    #  ========================================================================= #
    ## -------- Data Validation ------------------------------------------------
    #  ========================================================================= #
    regcor_iv <- InputValidator$new()
    slrraw_iv <- InputValidator$new()
    slrupload_iv <- InputValidator$new()
    slruploadvars_iv <- InputValidator$new()
    
    ### ------------ Rules -------------------------------------------------------
    slrraw_iv$add_rule("x", sv_required())
    slrraw_iv$add_rule("x", sv_regex("^\\s*-?\\d*\\.?\\d+(\\s*,\\s*-?\\d*\\.?\\d+)*\\s*$",
                                     "Data must be numeric values separated by a comma (ie: 2,3,4 or 2, 30, 400)."))
    
    slrraw_iv$add_rule("x", ~ if(length(strsplit(input$x, ",")[[1]]) < 4) "Sample Data must include at least 4 numeric observations.")
    slrraw_iv$add_rule("x", ~ if(sampleInfoRaw()$diff != 0) "x and y must have the same number of observations.")
    slrraw_iv$add_rule("x", ~ if(sampleInfoRaw()$xSD == 0) "Explanatory variable has zero variance (all values are identical). At least two distinct values are required.")
    
    slrraw_iv$add_rule("y", sv_required())
    slrraw_iv$add_rule("y", sv_regex("^\\s*-?\\d*\\.?\\d+(\\s*,\\s*-?\\d*\\.?\\d+)*\\s*$",
                                     "Data must be numeric values separated by a comma (ie: 2,3,4 or 2, 30, 400)."))
    slrraw_iv$add_rule("y", ~ if(length(strsplit(input$x, ",")[[1]]) < 4) "Sample Data must include at least 4 numeric observations.")
    slrraw_iv$add_rule("y", ~ if(sampleInfoRaw()$diff != 0) "x and y must have the same number of observations.")
    slrraw_iv$add_rule("y", ~ if(sampleInfoRaw()$ySD == 0) "Response variable is constant. Correlation is undefined when a variable has zero variance.")
    
    
    
    
    slrupload_iv$add_rule("slrUserData", sv_required())
    slrupload_iv$add_rule("slrUserData", ~ if(is.null(fileInputs$slrStatus) || fileInputs$slrStatus == 'reset') "Required")
    slrupload_iv$add_rule("slrUserData", ~ if(!(tolower(tools::file_ext(input$slrUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) == 0) "File is empty.")
    slrupload_iv$add_rule("slrUserData", ~ if(ncol(slrUploadData()) < 2) "Data must include one response and (at least) one explanatory variable.")
    slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) < 3) "Samples must include at least 2 observations.")
    # slrupload_iv$add_rule("slrUserData", ~ if(any(!is.numeric(slrUploadData()))) "File contains non-numeric data.")
    
    slruploadvars_iv$add_rule("slrExplanatory", sv_required())
    slruploadvars_iv$add_rule("slrExplanatory", ~ if(explanatoryInfoUploadSLR()$invalid) "Explanatory variable contains non-numeric data.")
    slruploadvars_iv$add_rule("slrExplanatory", ~ if(explanatoryInfoUploadSLR()$sd == 0) "Explanatory variable has zero variance (all values are identical). At least two distinct values are required.")
    
    slruploadvars_iv$add_rule("slrResponse", sv_required())
    slruploadvars_iv$add_rule("slrResponse", ~ if(sampleDiffUpload() != 0) "Missing values detected, x and y must have the same number of observations.")
    slruploadvars_iv$add_rule("slrResponse", ~ if(responseInfoUploadSLR()$invalid) "Response variable contains non-numeric data.")
    slruploadvars_iv$add_rule("slrResponse", ~ if(responseInfoUploadSLR()$sd == 0) "Response variable is constant. Correlation is undefined when a variable has zero variance.")
    
    ### ------------ Conditions --------------------------------------------------
    slrraw_iv$condition(~ isTRUE(input$dataRegCor == 'Enter Raw Data'))
    slrupload_iv$condition(~ isTRUE(input$dataRegCor == 'Upload Data'))
    slruploadvars_iv$condition(function() {isTRUE(input$dataRegCor == 'Upload Data' &&
                                                    slrupload_iv$is_valid()) })
    
    ### ------------ Dependencies ------------------------------------------------
    regcor_iv$add_validator(slrraw_iv)
    regcor_iv$add_validator(slrupload_iv)
    regcor_iv$add_validator(slruploadvars_iv)
    
    ### ------------ Activation --------------------------------------------------
    regcor_iv$enable()
    slrraw_iv$enable()
    slrupload_iv$enable()
    slruploadvars_iv$enable()
    
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
    
    slrUploadData <- eventReactive(input$slrUserData, {
      ext <- tools::file_ext(input$slrUserData$name)
      ext <- tolower(ext)
      
      switch(ext,
             csv = read_csv(input$slrUserData$datapath, show_col_types = FALSE),
             xls = read_xls(input$slrUserData$datapath),
             xlsx = read_xlsx(input$slrUserData$datapath),
             txt = read_tsv(input$slrUserData$datapath, show_col_types = FALSE),
             
             validate("Improper file format."))
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
      if (!dat$invalid){
        dat$sd <- sd(datx, na.rm = TRUE)
      }
      else{
        dat$sd <- 0
      }
      return(dat)
    })
    
    responseInfoUploadSLR <- eventReactive(input$slrResponse, {
      dat <- list()
      daty <- as.data.frame(slrUploadData())[, input$slrResponse]
      dat$invalid <- any(!is.numeric(daty))
      # Only calculate SD if numeric
      if (!dat$invalid) {
        dat$sd <- sd(daty, na.rm = TRUE)
      } 
      else {
        dat$sd <- 0
      }
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
      fileInputs$slrStatus <- 'uploaded'
      toggle(id = "regCorrMP", condition = slrupload_iv$is_valid())
      
      if (slrupload_iv$is_valid()) {
        updateSelectInput(inputId = "slrExplanatory",
                          choices = colnames(slrUploadData()))
        updateSelectInput(inputId = "slrResponse",
                          choices = colnames(slrUploadData()))
        show("slrExplanatory")
        show("slrResponse")
      }
    })
    
    ## NOTE: related to the old plot options UI.
    observeEvent(input$slrExplanatory, {
      updateTextInput(inputId = "xlab", value = input$slrExplanatory)
    })
    observeEvent(input$slrResponse, {
      updateTextInput(inputId = "ylab", value = input$slrResponse)
    })
    
    observeEvent(input$goRegression, {
      ## SLR Validation messages ----
      toggle(id = "SLRData", condition = regcor_iv$is_valid())
      
      output$slrValidation <- renderUI({
        
        if(!slrupload_iv$is_valid()){
          if(is.null(input$slrUserData)) {
            validate("Please upload a file.")
          }
        }
        
        # LINE STUFF ==========  
        
        output$lineAssumptions <- renderUI({
          req(model)
          
          alpha <- 0.05
          n     <- length(datx)
          
          # Run each test and collect results
          results <- lapply(lineTestConfig, function(cfg) {
            
            # Skip if not enough observations
            if (n < cfg$min_n) {
              return(data.frame(
                Assumption  = cfg$assumption,
                Procedure   = cfg$procedure,
                #Statistic   = NA_character_,
                `P-Value`   = NA_character_,
                Conclusion  = paste("Requires n \u2265", cfg$min_n),
                check.names = FALSE
              ))
            }
            
            # Run test safely
            result <- tryCatch(cfg$run(model, datx, daty), error = function(e) {
              list(statistic = NULL, p_value = NULL, note = paste("Error:", e$message))
            })
            
            # Format statistic and p-value
           # stat_str <- if (!is.null(result$statistic)) as.character(result$statistic) else "\u2014"
            pval_str <- if (!is.null(result$p_value))   as.character(result$p_value)   else "\u2014"
            
            # Conclusion
            conclusion <- if (!is.null(result$note)) {
              result$note
            } else if (!is.null(result$p_value)) {
              if (result$p_value <= alpha) {
                paste0("Reject H\u2080 (p = ", result$p_value, " \u2264 0.05)")
              } else {
                paste0("Fail to reject H\u2080 (p = ", result$p_value, " > 0.05)")
              }
            } else {
              "\u2014"
            }
            
            data.frame(
              Assumption  = cfg$assumption,
              Procedure   = cfg$procedure,
              #Statistic   = stat_str,
              `P-Value`   = pval_str,
              Conclusion  = conclusion,
              check.names = FALSE
            )
          })
          
          # Combine into one data frame
          tableData <- do.call(rbind, results)
          
          tagList(
            p(strong("Linearity, Independence, Normality and Equal Variance (L.I.N.E) Assumptions"),
              style = "font-size: 16px;"),
            p(paste("Testing at \u03b1 =", alpha, "| n =", n),
              style = "color: #666; font-size: 13px;"),
            br(),
            reactable(
              tableData,
              bordered   = TRUE,
              striped    = FALSE,
              highlight  = TRUE,
              pagination = FALSE,
              fullWidth  = TRUE,
              columns = list(
                Assumption = colDef(
                  name     = "Assumption",
                  minWidth = 200,
                  style    = list(fontWeight = "bold")
                ),
                Procedure  = colDef(name = "Procedure",      minWidth = 180),
                #Statistic  = colDef(name = "Test Statistic", minWidth = 120, align = "center"),
                `P-Value`  = colDef(name = "P-Value",        minWidth = 100, align = "center"),
                Conclusion = colDef(name = "Conclusion",      minWidth = 250)
              )
            ),
            br()
          )
          
        }) # END renderUI — LINE STUFF ==========
        
        
        validate(
          need(slrupload_iv$is_valid(), "Please upload a file."),
          need(nrow(slrUploadData()) != 0, "File is empty."),
          need(ncol(slrUploadData()) > 1,
               "Data must include one response and (at least) one explanatory variable."),
          need(nrow(slrUploadData()) > 2,
               "Samples must include at least 2 observations."),
          errorClass = "myClass"
        )
        
        if(!slruploadvars_iv$is_valid()) {
          validate(
            need(input$slrExplanatory != "", "Please select an Explanatory Variable (x)."),
            need(input$slrResponse != "", "Please select a Response Variable (y).") %then%
              need(sampleDiffUpload() == 0, "The Explanatory (x) and Response (y) variables must have the same number of observations."),
            errorClass = "myClass")
          
          validate(
            need(!explanatoryInfoUploadSLR()$invalid, "The Explanatory Variable (x) contains non-numeric data.") %then%
              need(explanatoryInfoUploadSLR()$sd != 0, "Explanatory Variable (x) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
            need(!responseInfoUploadSLR()$invalid, "The Response Variable (y) contains non-numeric data.") %then%
              need(responseInfoUploadSLR()$sd != 0, "Response Variable (y) must have a standard deviation greater than 0 to perform correlation analysis."),
            errorClass = "myClass")
        }
        
        if(input$dataRegCor == 'Upload Data') {
          req(slruploadvars_iv$is_valid())
          show("slrExplanatory")
          show("slrResponse")
          req(input$slrExplanatory %in% colnames(slrUploadData()))
          req(input$slrResponse %in% colnames(slrUploadData()))
          datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
          daty <- as.data.frame(slrUploadData())[, input$slrResponse]
        } else {
          validate(
            need(input$x, "Input required for the Explanatory variable (x)."),
            need(input$y, "Input required for the Response variable (y)."),
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
            need(sampleInfoRaw()$xSD != 0, "Explanatory variable (x) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
            need(sampleInfoRaw()$ySD != 0, "Response variable (y) must have a standard deviation greater than 0 to perform correlation analysis."),
            errorClass = "myClass")
        }
      }) #output$slrValidation
      
      if(regcor_iv$is_valid()) {
        if(input$dataRegCor == 'Upload Data') {
          req(input$slrExplanatory %in% colnames(slrUploadData()))
          req(input$slrResponse %in% colnames(slrUploadData()))
          datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
          daty <- as.data.frame(slrUploadData())[, input$slrResponse]
        } else {
          datx <- createNumLst(input$x)
          daty <- createNumLst(input$y)
        }
        
        model <- lm(daty ~ datx)
        r_squared <- summary(model)$r.squared
        y_hat <- fitted(model)
        residuals <- residuals(model)
        residuals_sq <- residuals^2
        
        n       <- length(datx)
        x_bar   <- mean(datx)
        mse     <- sum(residuals^2) / (n - 2)
        ssx     <- sum((datx - x_bar)^2)
        t_crit  <- qt(0.975, df = n - 2)
        
        se_mean  <- sqrt(mse * (1/n + (datx - x_bar)^2 / ssx))
        se_pred  <- sqrt(mse * (1 + 1/n + (datx - x_bar)^2 / ssx))
        
        ci_lower <- y_hat - t_crit * se_mean
        ci_upper <- y_hat + t_crit * se_mean
        pi_lower <- y_hat - t_crit * se_pred
        pi_upper <- y_hat + t_crit * se_pred
        
        df <- data.frame(datx, daty, datx*daty, datx^2, daty^2, y_hat, residuals, residuals_sq, ci_lower, ci_upper, pi_lower, pi_upper)
        names(df) <- c("x", "y", "xy", "x<sup>2</sup>", "y<sup>2</sup>", "&ycirc;",
                       "<em>e</em> = (<em>y</em> - <em>&ycirc;</em>)", "e<sup>2</sup>",
                       "95% CI<br>for the mean<br>response<br>(Lower)", 
                       "95% CI<br>for the mean<br>response<br>(Upper)",
                       "95% prediction<br>interval<br>(Lower)", 
                       "95% prediction<br>interval<br>(Upper)")
        
        dfTotaled <- bind_rows(
          df,
          df %>%
            summarise(
              across(c(x, y, xy, 
                       `x<sup>2</sup>`, 
                       `y<sup>2</sup>`,
                       `&ycirc;`, 
                       `<em>e</em> = (<em>y</em> - <em>&ycirc;</em>)`,
                       `e<sup>2</sup>`), sum),
              across(c(`95% CI<br>for the mean<br>response<br>(Lower)`,
                       `95% CI<br>for the mean<br>response<br>(Upper)`,
                       `95% prediction<br>interval<br>(Lower)`,
                       `95% prediction<br>interval<br>(Upper)`), ~ NA_real_))
        )
        
        dfTotaled[nrow(dfTotaled), "<em>e</em> = (<em>y</em> - <em>&ycirc;</em>)"] <- sum(df$`<em>e</em> = (<em>y</em> - <em>&ycirc;</em>)`)
        
        rownames(dfTotaled)[nrow(dfTotaled)] <- "Totals"
        
        sumXSumY <- dfTotaled["Totals", "x"] * dfTotaled["Totals", "y"]
        sumXSqrd <- dfTotaled["Totals", "x"] ^ 2
        sumYSqrd <- dfTotaled["Totals", "y"] ^ 2
        
        dfFormatted <- dfTotaled
        for(col in names(dfFormatted)) {
          if(is.numeric(dfFormatted[[col]])) {
            dfFormatted[[col]] <- sapply(dfFormatted[[col]], function(x) {
              if(is.na(x)) return(NA)
              format(round(x, 3), nsmall = 0, scientific = FALSE)
            })
          }
        }
        slrExportData(dfFormatted)  
        
        
        # Perfect fit detection
        output$perfectFitWarning <- renderUI({
          if (isTRUE(all.equal(r_squared, 1))) {
            
            # Hide the tabs
            hideTab(inputId = "slrNavbarPage", target = "Inference")
            hideTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
            
            div(
              class = "alert alert-warning",
              role  = "alert",
              style = "margin-top: 10px;",
              tags$b("\u26a0\ufe0f Perfect Fit Detected: "),
              "The model has an R\u00b2 of 1 or -1, meaning the regression line fits the data perfectly. Resulting in an MSE equal to zero and a zero set of residuals.",
              "This may indicate that ",
              tags$b("x and y are identical or linearly dependent,"),
              " which can produce unreliable inference results for the Parameters, ANOVA and Diagnostic Plots.",
              tags$b("These tabs are now hidden")
            )
          } else {
            showTab(inputId = "slrNavbarPage", target = "Inference")
            showTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
            NULL
          }
        })
        
          # Disables ANOVA, INFRENCE and DIAGNOSTIC PLOTS if perfect fit is triggered
          isPerfectFit <- isTRUE(all.equal(r_squared, 1))
          
          if (isPerfectFit) {
            hideTab(inputId = "slrNavbarPage", target = "Inference")
            hideTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
          } else {
            showTab(inputId = "slrNavbarPage", target = "Inference")
            showTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
          }
          
          
        
        
        output$slrDataTable <- renderReactable({
          
          dataRows  <- dfFormatted[1:(nrow(dfFormatted) - 1), ]
          totalsRow <- dfFormatted[nrow(dfFormatted), ]
          
          # Use original numeric df for sorting
          numericRows <- df
          
          reactable(
            numericRows,
            sortable   = TRUE,
            resizable  = TRUE,
            bordered   = TRUE,
            striped    = TRUE,
            highlight  = TRUE,
            pagination = FALSE,
            fullWidth  = FALSE,
            rownames   = TRUE,
            columns = c(
              # Row name column — just used to show "Totals" label in footer
              list(".rownames" = colDef(
                name   = "Observation Number",
                align = "center",
                footer = tags$b("Totals"),
                style  = list(color = "#333")
            
              )),
              # Data columns with HTML names and totals footer
              setNames(
                lapply(names(numericRows), function(col) {
                  colDef(
                    html     = TRUE,
                    align    = "center",
                    name     = names(df)[match(col, names(numericRows))],
                    footer   = if (is.na(dfTotaled[nrow(dfTotaled), col])) {
                      ""
                    } else {
                      tags$b(totalsRow[[col]])
                    },
                    cell = function(value) {
                      if (!is.numeric(value)) return(value)
                      if (value == floor(value)) {
                        formatC(value, format = "f", digits = 0)
                      } else {
                        formatC(value, format = "f", digits = 3)
                      }
                    }
                  )
                }),
                names(numericRows)
              )
            )
          )
        })
        
    
        
        
        
        output$slrScatterplot <- renderPlotly({
          RenderScatterplot(
            df,
            model,
            input[["slrScatter-Title"]],
            input[["slrScatter-Xlab"]],
            input[["slrScatter-Ylab"]],
            input[["slrScatter-Colour"]],
            input[["slrScatter-PointsColour"]],
            input[["slrScatter-LineWidth"]],
            input[["slrScatter-PointSize"]],
            input[["slrScatter-Gridlines"]],
            input[["slrScatter-confidenceInterval"]],
            input[["slrScatter-predictionInterval"]],
            input[["slrScatter-showRegressionLine"]]
          )
        })
        
        output$slrResidualsPanelPlot1 <- renderPlot({
          par(font.main = 2, font.lab = 2)
          plot(model, which = 1, pch = 20, main = "", lwd = 2, ann = FALSE, sub.caption = "", caption = "")
          title(main = "Residuals vs Fitted Values", cex.main = 1.2)
          title(xlab = expression(bold(Fitted~Values~(hat(italic(y))))))
          title(ylab = expression(bold(Residuals~plain("(")*italic(e)*plain(")"))))
          abline(h = 0, col = "black", lty = 2, lwd = 1.5)
        })
        
        output$slrResidualsPanelPlot2 <- renderPlot({
          par(font.main = 2, font.lab = 2)
          plot(model, which = 2, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "")
          title(main = "Q-Q Residuals", cex.main = 1.2)
          title(xlab = "Theoretical Quantiles")
        })
        
        output$slrResidualsPanelPlot3 <- renderPlot({
          par(font.main = 2, font.lab = 2)
          plot(model, which = 3, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "")
          title(main = "Scale-Location", cex.main = 1.2)
          title(ylab = "Standardized Residuals")
        })
        
        output$slrResidualsPanelPlot4 <- renderPlot({
          par(font.main = 2, font.lab = 2)
          plot(model, which = 5, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "")
          title(main = "Residuals vs Leverage", cex.main = 1.2)
        })
        
        output$slrResidualsPanelPlot5 <- renderPlot({
          par(font.main = 2, font.lab = 2)
          hist(residuals, main = "", xlab = "",
               col = "darkgreen", border = "white")
          title(main = "Histogram of Residuals", cex.main = 1.2)
          title(xlab = expression(bold(Residuals~plain("(")*italic(e)*plain(")"))))
        })
        
        
        if (summary(model)$coefficients["datx", "Estimate"] > 0) {
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
          withMathJax(
            p("The estimated equation of the regression line is "),
            sprintf("\\( \\qquad \\hat{y} = \\hat{\\beta}_{0} + \\hat{\\beta}_{1} x \\)"),
            br(),
            br(),
            p("where"),
            sprintf("\\( \\qquad \\hat{\\beta}_{1} = \\dfrac{ \\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } \\)"),
            sprintf("\\( \\, = \\, \\dfrac{ %s - \\dfrac{ (%s)(%s) }{ %s } }{ %s - \\dfrac{ (%s)^2 }{ %s } } \\)",
                    format(round(dfTotaled["Totals", "xy"], 3), nsmall = 0, scientific = FALSE),
                    format(round(dfTotaled["Totals", "x"], 3), nsmall = 0, scientific = FALSE),
                    format(round(dfTotaled["Totals", "y"], 3), nsmall = 0, scientific = FALSE),
                    format(round(length(datx), 3), nsmall = 0, scientific = FALSE),
                    format(round(dfTotaled["Totals", "x<sup>2</sup>"], 3), nsmall = 0, scientific = FALSE),
                    format(round(dfTotaled["Totals", "x"], 3), nsmall = 0, scientific = FALSE),
                    format(round(length(datx), 3), nsmall = 0, scientific = FALSE)),
          
            sprintf("\\( \\, = \\, %0.4f \\)",
                    slopeEstimate),
            br(),
            br(),
            p("and"),
            sprintf("\\( \\qquad \\hat{\\beta}_{0} = \\bar{y} - \\hat{\\beta}_{1} \\bar{x}\\)"),
            sprintf("\\( \\, = \\, %s - (%0.4f) (%s) \\)",
                    format(round(mean(daty), 3), nsmall = 0, scientific = FALSE),
                    summary(model)$coefficients["datx", "Estimate"],
                    format(round(mean(datx), 3), nsmall = 0, scientific = FALSE)),
            sprintf("\\( \\, = \\, %s %s %0.4f\\)",
                    format(round(mean(daty), 3), nsmall = 0, scientific = FALSE),
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
            p(tags$b("Interpretation:")),
            p(HTML(paste0("Within the scope of observation, ", interceptEstimate, " is the estimated value of ",
                          "\\(y\\) when \\(x\\) = 0. A slope of ", slopeEstimate,
                          " represents the estimated ", slopeDirection, " in  \\(y\\)",
                          " for a unit increase of \\(x\\).")))
          )
        })
        
        output$slrInferenceDetails <- renderUI({
          req(model)
          
          # Extract Model Statistics
          summ <- summary(model)
          coefs <- summ$coefficients
          
          # Intercept (Beta 0) values
          b0_est <- coefs["(Intercept)", "Estimate"]
          b0_se  <- coefs["(Intercept)", "Std. Error"]
          b0_t   <- coefs["(Intercept)", "t value"]
          b0_p   <- coefs["(Intercept)", "Pr(>|t|)"]
          
          # Slope (Beta 1) values
          b1_est <- coefs["datx", "Estimate"]
          b1_se  <- coefs["datx", "Std. Error"]
          b1_t   <- coefs["datx", "t value"]
          b1_p   <- coefs["datx", "Pr(>|t|)"]
          
          # Data Statistics
          n <- length(datx)
          df <- df.residual(model) # n - 2
          t_crit <- qt(0.975, df)
          
          sum_e2 <- sum(residuals(model)^2)
          x_bar <- mean(datx)
          sum_sq_diff_x <- sum((datx - x_bar)^2)
          
          # Helper for formatting numbers
          fmt <- function(x) format(round(x, 4), nsmall = 4, scientific = FALSE)
          
          # Render the UI with MathJax
          withMathJax(
            tags$style(HTML("
      .left-align-math .MathJax_Display {
        text-align: left !important;
        margin: 0 !important;
      }
    ")),
            fluidRow(style = "display: flex; flex-wrap: wrap;",
                     # --- LEFT COLUMN: Intercept Parameter ---
                     column(6, style = "display: flex;",
                            div(style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; width: 100%;",
                                h4(HTML("Intercept Parameter (\\(\\beta_0\\))")),
                                br(),
                                p(HTML("H<sub>0</sub>: \\(\\beta_0 = 0\\)")),
                                p(HTML("H<sub>a</sub>: \\(\\beta_0 \\neq 0\\)")),
                                p(HTML("\\(\\alpha = 0.05\\)")),
                                
                                # t-statistic equation
                                p(strong("Test Statistic:")),
                                p(class = "left-align-math",
                                  HTML(sprintf("$$\\small{t = \\frac{\\hat{\\beta}_0 - 0}{\\left(\\sqrt{\\frac{\\sum e^2}{n-2}} \\times \\sqrt{\\frac{1}{n} + \\frac{\\bar{x}^2}{\\sum(x-\\bar{x})^2}}\\right)} = \\frac{%s - 0}{%s} = %s}$$",
                                               fmt(b0_est), fmt(b0_se), fmt(b0_t)))),
                                
                                p(strong(sprintf("P-value = %s", fmt(b0_p)))),
                                
                                withMathJax(
                                  p(strong("Conclusion:")),
                                  if (b0_p <= 0.05) {
                                    p(sprintf("Since the p-value is less than \\( \\alpha \\) (%.4f < 0.05), we reject the null hypothesis and conclude there is enough statistical evidence to support the alternative hypothesis.", b0_p))
                                  } else {
                                    p(sprintf("Since the p-value is greater than \\( \\alpha \\) (%.4f >  0.05), we fail to reject the null hypothesis and conclude there isn't enough statistical evidence to support the alternative hypothesis.", b0_p))
                                  }
                                ),
                                
                                # Horizontal Line
                                hr(style = "border-top: 1px solid #ccc;"),
                                
                                # Confidence Interval
                                p("The 95% confidence interval for \\(\\beta_0\\) is"),
                                p(class = "left-align-math",
                                  HTML(sprintf("$$\\scriptsize{\\hat{\\beta}_0 \\pm t_{\\alpha/2,\\,(n-2)} \\left(\\sqrt{\\frac{\\sum e^2}{n-2}} \\times \\sqrt{\\frac{1}{n} + \\frac{\\bar{x}^2}{\\sum(x-\\bar{x})^2}}\\right) \\;=\\; (%s, \\;%s)}$$",
                                               fmt(b0_est - t_crit * b0_se), fmt(b0_est + t_crit * b0_se))))
                            )
                     ),
                     
                     # --- RIGHT COLUMN: Slope Parameter ---
                     column(6, style = "display: flex;",
                            div(style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; width: 100%;",
                                h4(HTML("Slope Parameter (\\(\\beta_1\\))")),
                                br(),
                                p(HTML("H<sub>0</sub>: \\(\\beta_1 = 0\\)")),
                                p(HTML("H<sub>a</sub>: \\(\\beta_1 \\neq 0\\)")),
                                p(HTML("\\(\\alpha = 0.05\\)")),
                                
                                # t-statistic equation
                                p(strong("Test Statistic:")),
                                p(class = "left-align-math",
                                  HTML(sprintf("$$\\small{t = \\frac{\\hat{\\beta}_1 - 0}{\\left(\\frac{\\sqrt{\\frac{\\sum e^2}{n-2}}}{\\sqrt{\\sum(x-\\bar{x})^2}}\\right)} = \\frac{%s - 0}{%s} = %s}$$",
                                               fmt(b1_est), fmt(b1_se), fmt(b1_t)))),
                                
                                p(strong(sprintf("P-value = %s", fmt(b1_p)))),
                                
                                withMathJax(
                                  p(strong("Conclusion:")),
                                  if (b1_p <= 0.05) {
                                    p(sprintf("Since the p-value is less than \\( \\alpha \\) (%.4f < 0.05), we reject the null hypothesis and conclude there is enough statistical evidence to support the alternative hypothesis.", b1_p))
                                  } else {
                                    p(sprintf("Since the p-value is greater than \\( \\alpha \\) (%.4f >  0.05), we fail to reject the null hypothesis and conclude there isn't enough statistical evidence to support the alternative hypothesis.", b1_p))
                                  }
                                ),
                                
                                # Horizontal Line
                                hr(style = "border-top: 1px solid #ccc;"),
                                
                                # Confidence Interval
                                p("The 95% confidence interval for \\(\\beta_1\\) is"),
                                p(class = "left-align-math",
                                  HTML(sprintf("$$\\small{\\hat{\\beta}_1 \\pm t_{\\alpha/2,\\,(n-2)} \\left(\\frac{\\sqrt{\\frac{\\sum e^2}{n-2}}}{\\sqrt{\\sum(x-\\bar{x})^2}}\\right) \\;=\\; (%s, \\;%s)}$$",
                                               fmt(b1_est - t_crit * b1_se), fmt(b1_est + t_crit * b1_se))))
                            )
                     )
            )
          )
        })
        
        output$confintLinReg <- renderPrint({
          confint(model) # Prints the 95% CI for the regression parameters
        })
        
        output$anovaLinReg <- renderPrint({
          anova(model) # Prints the ANOVA table
        })
        
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
              withMathJax(
                
                # Line 1: General formula
                sprintf("\\( \\normalsize{\\quad r = \\dfrac
              {\\left(\\sum xy\\right) - \\dfrac{ \\left(\\sum x\\right) \\times \\left(\\sum y\\right) }{ n } }
              {\\sqrt{ \\left(\\sum x^2\\right) - \\dfrac{ \\left(\\sum x\\right)^2 }{ n } } \\times \\sqrt{ \\left(\\sum y^2\\right) - \\dfrac{ \\left(\\sum y\\right) ^2 }{ n } }} } \\)"),
                
                br(),
                br(),
                
                # Line 2: Values substituted = simplified √ form = final result
                sprintf("\\( \\normalsize{\\quad = \\dfrac
              {%s - \\dfrac{ (%s) \\times (%s) }{ %s } }
              {\\sqrt{ %s - \\dfrac{ (%s)^2 }{ %s } } \\times \\sqrt{ %s - \\dfrac{ (%s)^2 }{ %s } }}
              \\quad = \\dfrac{ %s }{\\sqrt{ %s } \\times \\sqrt{ %s }}
              \\quad = %g} \\)",
                        
                        format(round(dfTotaled["Totals", "xy"], 3), nsmall = 0, scientific = FALSE),
                        format(round(dfTotaled["Totals", "x"], 3), nsmall = 0, scientific = FALSE),
                        format(round(dfTotaled["Totals", "y"], 3), nsmall = 0, scientific = FALSE),
                        format(length(datx), nsmall = 0, scientific = FALSE),
                        
                        format(round(dfTotaled["Totals", "x<sup>2</sup>"], 3), nsmall = 0, scientific = FALSE),
                        format(round(dfTotaled["Totals", "x"], 2), nsmall = 0, scientific = FALSE),
                        format(length(datx), nsmall = 0, scientific = FALSE),
                        
                        format(round(dfTotaled["Totals", "y<sup>2</sup>"], 3), nsmall = 0, scientific = FALSE),
                        format(round(dfTotaled["Totals", "y"], 3), nsmall = 0, scientific = FALSE),
                        format(length(datx), nsmall = 0, scientific = FALSE),
                        
                        # simplified √ form
                        format(round(dfTotaled["Totals", "xy"] - sumXSumY / length(datx), 3),
                               nsmall = 0, scientific = FALSE),
                        
                        format(round(dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx), 3),
                               nsmall = 0, scientific = FALSE),
                        
                        format(round(dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx), 3),
                               nsmall = 0, scientific = FALSE),
                        
                        # final result
                        round(pearson$estimate, 4)
                ),
                
                br(),
                br(),
                br(),
                
                # Interpretation moved to bottom
                p(tags$b("Interpretation:")),
                sprintf(
                  "There exists a %s %s linear relationship between \\(x\\) and \\(y\\).",
                  pearsonStrength,
                  pearsonSign
                ),
                
                br(),
                br(),
                
                # Population Correlation Coefficient
                hr(),
                p(strong("Hypothesis Test for Population Correlation Coefficient")),
                p(HTML("H<sub>0</sub>: \\(\\rho = 0\\)")),
                p(HTML("H<sub>a</sub>: \\(\\rho \\neq 0\\)")),
                p("\\(\\alpha = 0.05\\)"),
                p(sprintf("\\( df = n - 2 = %d \\)", pearson$parameter)),
                
                p(strong("Test Statistic:")),
                p(sprintf(
                  "\\( t = \\dfrac{r\\sqrt{n-2}}{\\sqrt{1-r^2}} = \\dfrac{%0.4f\\sqrt{%d-2}}{\\sqrt{1-%0.4f^2}} = %0.4f \\)",
                  pearson$estimate, n, pearson$estimate, pearson$statistic
                )),
                
                # P-value method
                p(strong("Using P-Value Method:")),
                p(sprintf(
                  "\\( P = 2 \\times P(t > |\\, %0.4f \\,|) = %s \\)",
                  pearson$statistic,
                  format.pval(pearson$p.value, digits = 4, eps = 0.0001)
                )),
                if(pearson$p.value <= 0.05) {
                  p(sprintf(
                    "Since \\( P \\leq 0.05 \\), reject \\( H_0 \\)."
                  ))
                } else {
                  p(sprintf(
                    "Since \\( P > 0.05 \\), fail to reject \\( H_0 \\)."
                  ))
                },
                
                br(),
                
                # Critical value method
                p(strong("Using Critical Value Method:")),
                p(sprintf(
                  "\\( \\text{Critical Value(s)} = \\pm t_{\\alpha/2,\\, n-2} = \\pm t_{0.025,\\, %d} = \\pm %0.4f \\)",
                  pearson$parameter,
                  qt(0.975, df = pearson$parameter)
                )),
                if(abs(pearson$statistic) > qt(0.975, df = pearson$parameter)) {
                  p(sprintf(
                    "Since the test statistic \\( (t = %0.4f) \\) falls within the rejection region, reject \\( H_0 \\).",
                    pearson$statistic
                  ))
                } else {
                  p(sprintf(
                    "Since the test statistic \\( (t = %0.4f) \\) does not fall within the rejection region, fail to reject \\( H_0 \\).",
                    pearson$statistic
                  ))
                },
                
                # Curve
                plotOutput(session$ns("pearsonTCurve")),
                
                
                # Conclusion
                p(strong("Conclusion:")),
                if(pearson$p.value <= 0.05) {
                  p(sprintf(
                    "At \\( \\alpha = 0.05 \\), since the test statistic falls in the rejection region we reject \\( H_0 \\) and conclude that there is enough statistical evidence of a linear relationship between \\( x \\) and \\( y \\) in the population."
                  ))
                } else {
                  p(sprintf(
                    "At \\( \\alpha = 0.05 \\), since the test statistic does not fall in the rejection region we fail to reject \\( H_0 \\) and conclude that there is not enough statistical evidence of a linear relationship between \\( x \\) and \\( y \\) in the population."
                  ))
                },
                
                # Fischer Transform 
                
                hr(),
                p(strong("Confidence Interval for \\(\\rho\\) (Fisher Z-Transformation)")),
                
                p("Since the sampling distribution of Pearson's r is not normal, we use the Fisher Z-Transformation to construct a confidence interval."),
                
                p(strong("Step 1: Transform r")),
                p(sprintf(
                  "\\( z_r = \\dfrac{1}{2} \\ln\\left(\\dfrac{1+r}{1-r}\\right) = \\text{artanh}(r) = \\dfrac{1}{2} \\ln\\left(\\dfrac{1+%0.4f}{1-%0.4f}\\right) = %0.4f \\)",
                  pearson$estimate, pearson$estimate, atanh(pearson$estimate)
                )),
                
                p(strong("Step 2: Standard Error")),
                p(sprintf(
                  "\\( SE_{z_r} = \\dfrac{1}{\\sqrt{n-3}} = \\dfrac{1}{\\sqrt{%d-3}} = %0.4f \\)",
                  n, 1/sqrt(n-3)
                )),
                
                p(strong("Step 3: Confidence Interval on Transformed Scale")),
                p(sprintf(
                  "\\( \\left(z_r - Z_{\\alpha/2} \\cdot SE_{z_r}, \\; z_r + Z_{\\alpha/2} \\cdot SE_{z_r}\\right) = \\left(%0.4f - 1.96 \\times %0.4f, \\; %0.4f + 1.96 \\times %0.4f\\right) = \\left(%0.4f, \\; %0.4f\\right) \\)",
                  atanh(pearson$estimate), 1/sqrt(n-3),
                  atanh(pearson$estimate), 1/sqrt(n-3),
                  atanh(pearson$estimate) - 1.96 * (1/sqrt(n-3)),
                  atanh(pearson$estimate) + 1.96 * (1/sqrt(n-3))
                )),
                
                p(strong("Step 4: Convert Back to Original Scale")),
                {
                  z_lower <- atanh(pearson$estimate) - 1.96 * (1/sqrt(n-3))
                  z_upper <- atanh(pearson$estimate) + 1.96 * (1/sqrt(n-3))
                  ci_lower <- (exp(2*z_lower) - 1) / (exp(2*z_lower) + 1)
                  ci_upper <- (exp(2*z_upper) - 1) / (exp(2*z_upper) + 1)
                  
                  p(sprintf(
                    "\\( \\left(\\dfrac{e^{2Z_{lower}}-1}{e^{2Z_{lower}}+1}, \\; \\dfrac{e^{2Z_{upper}}-1}{e^{2Z_{upper}}+1}\\right) = \\left(%0.4f, \\; %0.4f\\right) \\)",
                    ci_lower, ci_upper
                  ))
                },
                
                br(),
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
            noquote("Pearson's Correlation requires a minimum sample size of 3 for computation.")
          })
        }
        
        kendall <- cor.test(datx, daty, method = "kendall")
        
        spearman <- cor.test(datx, daty, method = "spearman")
        
        output$kendallEstimate <- renderUI({
          sprintf("\\( \\tau \\; = \\; %0.4f \\)",
                  kendall$estimate)
        })
        
        # Kendall's Tau formula
        # output$kendallFormula <- renderUI({
        #   n <- length(datx)
        #   withMathJax(
        #     sprintf("\\( \\displaystyle \\tau = \\dfrac{n_c - n_d}{\\binom{n}{2}} = \\dfrac{n_c - n_d}{\\dfrac{n(n-1)}{2}} = %0.4f \\)",
        #             kendall$estimate)#,
        #     # br(),
        #     # sprintf("\\( \\tau \\; = \\; %0.4f \\)", kendall$estimate)
        #   )
        # })
        
        output$kendallFormula <- renderUI({
          n  <- length(datx)
          n0 <- n * (n - 1) / 2
          n1 <- sum(choose(table(datx), 2))
          n2 <- sum(choose(table(daty), 2))
          
          nc <- 0
          nd <- 0
          for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
              dx <- datx[i] - datx[j]
              dy <- daty[i] - daty[j]
              if (sign(dx) == sign(dy) && dx != 0 && dy != 0) {
                nc <- nc + 1
              } else if (sign(dx) != sign(dy) && dx != 0 && dy != 0) {
                nd <- nd + 1
              }
            }
          }
          
          tau <- kendall$estimate
          
          # Strength
          tauStrength <- if (abs(tau) > 0.6) "strong"
          else if (abs(tau) > 0.3) "moderate"
          else "weak"
          
          # Direction
          tauDirection <- if (tau > 0) "positive" else "negative"
          
          withMathJax(
            HTML(sprintf(
              "\\( \\tau = \\dfrac{n_c - n_d}{\\sqrt{(n_0 - n_1)(n_0 - n_2)}} = \\dfrac{%d - %d}{\\sqrt{(%g - %g)(%g - %g)}} = %.4f \\)",
              nc, nd, n0, n1, n0, n2, tau
            )),
            br(),
            br(),
            p(tags$b("Interpretation:")),
            p(sprintf(
              "There exists a %s %s monotonic relationship between x and y.",
              tauStrength, tauDirection
            ))
          )
        })
        # Spearman's rs formula
        output$spearmanEstimate <- renderUI({
          
          rank_x   <- rank(datx)
          rank_y   <- rank(daty)
          d        <- rank_x - rank_y
          d_sq     <- d^2
          sum_d_sq <- sum(d_sq)
          n        <- length(datx)
          
          spearman_df <- data.frame(
            x      = datx,
            y      = daty,
            rank_x = rank_x,
            rank_y = rank_y,
            d      = d,
            d_sq   = d_sq
          )
          
          rs <- spearman$estimate
          
          # Strength
          rsStrength <- if (abs(rs) > 0.6) "strong"
          else if (abs(rs) > 0.3) "moderate"
          else "weak"
          
          # Direction
          rsDirection <- if (rs > 0) "positive" else "negative"
          
          withMathJax(
            
            div(
              style = "text-align: left; font-size: 18px;",
              HTML(sprintf(
                "\\( r_s = 1 - \\dfrac{6 \\sum_{i=1}^n d_i^2}{n(n^2 - 1)} = 1 - \\dfrac{6 \\times %g}{%d(%d^2 - 1)} = %.4f \\)",
                sum_d_sq, n, n, rs
              ))
            ),
            
            br(),
            
            p(tags$b("Interpretation:")),
            p(sprintf(
              "There exists a %s %s monotonic relationship between \\(\\mathit{x}\\) and \\(\\mathit{y}\\).",
              rsStrength, rsDirection
            )),
            
            
            br(),
            
            reactable(
              spearman_df,
              sortable   = FALSE,
              bordered   = TRUE,
              striped    = TRUE,
              highlight  = TRUE,
              pagination = FALSE,
              fullWidth  = FALSE,
              rownames   = FALSE,
              columns = list(
                x      = colDef(name = "x",      align = "center"),
                y      = colDef(name = "y",      align = "center"),
                rank_x = colDef(name = "Rank x", align = "center"),
                rank_y = colDef(name = "Rank y", align = "center"),
                d      = colDef(name   = "d = (Rank x \u2212 Rank y)", align = "center", footer = tags$b("Total")),
                d_sq   = colDef(
                  name   = HTML("d<sup>2</sup>"),
                  html   = TRUE,
                  align  = "center",
                  footer = tags$b(sum_d_sq),
                  cell   = function(value) formatC(value, format = "f", digits = 0)
                )
              )
            ),
            
            br(),
            
            
            
            br()
          )
        })
        output$slrViewUpload <- renderDT({
          req(slrupload_iv$is_valid())
          datatable(slrUploadData(),
                    options = list(pageLength = -1,
                                   lengthMenu = list(c(25, 50, 100, -1),
                                                     c("25", "50", "100", "all"))))
        })
        
        # ANOVA Output
        output$anovaHypotheses <- renderUI({
          n <- length(datx)
          withMathJax(
            p(strong("Analysis of Variance (ANOVA)")),
            p(
              "\\( H_0: \\beta_1 = 0 \\)",
              br(),
              "\\( H_a: \\beta_1 \\neq 0 \\)"
            ),
            p("\\( \\alpha = 0.05 \\)"),
            p(sprintf("\\( n = %d \\)", n))
          )
        })
        
        output$anovaTable <- renderTable(
          {
            anova_results <- anova(model)
            data.frame(
              Source = c("<strong>Regression (Model)</strong>", "<strong>Error (Residual)</strong>", "<strong>Total</strong>"),
              df = c(anova_results$Df[1], anova_results$Df[2], sum(anova_results$Df)),
              SS = c(anova_results$`Sum Sq`[1], anova_results$`Sum Sq`[2], sum(anova_results$`Sum Sq`)),
              MS = c(anova_results$`Mean Sq`[1], anova_results$`Mean Sq`[2], NA),
              F = c(anova_results$`F value`[1], NA, NA),
              `P-value` = c(anova_results$`Pr(>F)`[1], NA, NA),
              check.names = FALSE
            )
          },
          na = "",
          striped = TRUE,
          align = "c",
          sanitize.text.function = function(x) x
        )
        
        output$anovaConclusion <- renderUI({
          anova_results <- anova(model)
          p_value <- anova_results$`Pr(>F)`[1]
          f_value <- anova_results$`F value`[1]
          msr <- anova_results$`Mean Sq`[1]
          mse <- anova_results$`Mean Sq`[2]
          
          withMathJax(
            p(strong("Test Statistic:")),
            p(sprintf("\\( \\displaystyle F = \\frac{\\mathrm{MSR}}{\\mathrm{MSE}} = \\frac{%.3f}{%.3f} = %.3f \\)", msr, mse, f_value)),
            p(strong("Conclusion:")),
            if (p_value <= 0.05) {
              p(sprintf("Since the p-value is less than \\( \\alpha \\) (%.3f < 0.05), we reject the null hypothesis and conclude there is enough statistical evidence to support the alternative hypothesis. We can conclude the model is statistically significant (There exists a linear relationship between x and y).", p_value))
            } else {
              p(sprintf("Since the p-value is greater than \\( \\alpha \\) (%.3f >  0.05), we fail to reject the null hypothesis and conclude there isn't enough statistical evidence to support the alternative hypothesis. We can conclude the model is not statistically significant (There exists no linear relationship between x and y).", p_value))
            }
          )
        })
        
        output$anovaR2 <- renderUI({
          
          anova_results <- anova(model)
          
          ssr <- anova_results$`Sum Sq`[1]
          sse <- anova_results$`Sum Sq`[2]
          sst <- ssr + sse
          r2  <- ssr / sst
          
          # Percentage explained
          explained_pct <- r2 * 100
          
          withMathJax(
            
            p(strong("Coefficient of Determination (\\( R^2 \\))")),
            
            tags$div(
              style = "text-align: left;",
              HTML(sprintf(
                "\\( R^2 = \\dfrac{\\mathrm{SSR}}{\\mathrm{SSR} + \\mathrm{SSE}} = \\dfrac{\\mathrm{SSR}}{\\mathrm{SST}} = \\dfrac{%.4f}{%.4f + %.4f} = \\dfrac{%.4f}{%.4f} = %.4f \\)",
                ssr, ssr, sse, ssr, sst, r2
              ))
            ),
            
            br(),
            
            tags$p(
              strong("Interpretation:")
            ),
            
            tags$p(sprintf(
              "%.2f%% of the variation in %s can be explained by its linear relationship with %s.",
              explained_pct,
              if (input$dataRegCor == "Upload Data") input$slrResponse else "y",
              if (input$dataRegCor == "Upload Data") input$slrExplanatory else "x"
            ))
            
          )
        })
        
        output$anovaFCurve <- renderPlot({
          anova_results <- anova(model)
          
          df1    <- 1
          df2    <- n - 2
          f_stat <- anova_results$`F value`[1]
          p_val  <- anova_results$`Pr(>F)`[1]
          f_crit <- qf(0.95, df1, df2)
          x_max  <- x_max  <- f_crit * 3
          
          x <- seq(0, x_max, length.out = 1000)
          y <- df(x, df1, df2)
          
          plot_df <- data.frame(x = x, y = y)
          
          ggplot(plot_df, aes(x = x, y = y)) +
            
            # Main curve
            geom_line(lwd = 1) +
            
            # Rejection region shading (red)
            geom_area(
              data = subset(plot_df, x >= f_crit),
              aes(x = x, y = y),
              fill  = "red",
              alpha = 0.3
            ) +
            
            # P-value region shading (blue) - only if f_stat <= x_max
            {if(f_stat <= x_max)
              geom_area(
                data = subset(plot_df, x >= f_stat),
                aes(x = x, y = y),
                fill  = "blue",
                alpha = 0.3
              )
            } +
            
            # Critical value line
            geom_vline(
              xintercept = f_crit,
              colour     = "red",
              linewidth  = 0.8,
              linetype   = "dashed"
            ) +
            
            # F statistic line - only if within plot range
            {if(f_stat <= x_max)
              geom_vline(
                xintercept = f_stat,
                colour     = "blue",
                linewidth  = 0.8,
                linetype   = "dashed"
              )
            } +
            
            # Labels
            labs(
              title = "F Distribution",
              x     = "F",
              y     = "Density"
            ) +
            
            # Annotations
            # Critical value annotation - below the line instead of beside it
            annotate("text",
                     x     = f_crit,
                     y     = max(y) * 0.2,
                     label = sprintf("F critical\n= %.4f", f_crit),
                     hjust = -0.1,
                     color = "red",
                     size  = 3.5) +
            
            # P-value annotation - fixed to top right corner
            annotate("text",
                     x     = x_max * 0.6,
                     y     = max(y) * 0.9,
                     label = sprintf("p-value = %.4f", p_val),
                     color = "black",
                     size  = 4) +
            
            # F statistic annotation - only shown if within plot range
            {if(f_stat <= x_max)
              annotate("text",
                       x     = f_stat,
                       y     = max(y) * 0.4,
                       label = sprintf("F statistic\n= %.4f", f_stat),
                       hjust = -0.1,
                       color = "blue",
                       size  = 3.5)
            } +
            
            # Theme
            theme_classic() +
            theme(
              plot.title   = element_text(hjust = 0.5, face = "bold"),
              axis.title   = element_text(size = 12, face = "bold"),
              axis.text    = element_text(size = 10, face ="bold")
            ) +
            coord_cartesian(clip = "off")
        })
        
        output$pearsonTCurve <- renderPlot({
          hypTTestPlot(
            testStatistic = round(pearson$statistic, 3),
            degfree       = pearson$parameter,
            critValue     = round(qt(0.975, df = pearson$parameter), 3),
            altHypothesis = "two.sided"
          )
        }, height = 300, width = 500)
        
        
        
        
        
      } #if regcor_iv is valid
      
      show(id = "regCorrMP")
    }) # input$goRegression
    

    
    ### ------------ Component Display -------------------------------------------
    observeEvent(!regcor_iv$is_valid(), {
      hide(id = "regCorrMP")
      hide(id = "SLRData")
    })
    
    observeEvent(input$dataRegCor, {
      hide(id = "regCorrMP")
      output$perfectFitWarning <- renderUI({ NULL })
      
      if (is.null(fileInputs$slrStatus) || fileInputs$slrStatus != "uploaded"){
        hide(id = "slrResponse")
        hide(id = "slrExplanatory")
      }
      
      updateTextInput(inputId = "xlab", value = "x")
      updateTextInput(inputId = "ylab", value = "y")
      ## FIXME: the file upload won't reset to its original state simply by
      ## switching back and forth between data sources, which it should.
      #shinyjs::reset("userUploadedData")
      #fileInputs$slrStatus <- 'reset'
    })
    
    observeEvent(input$dataRegCor, {
      req(fileInputs$slrStatus)
      if (fileInputs$slrStatus == "uploaded"){
        show(id = "slrResponse")
        show(id = "slrExplanatory")
      } else {
        hide(id = "slrResponse")
        hide(id = "slrExplanatory")
      }
    })
    
    observe({
      req(isTruthy(input$dataRegCor))
      if(input$dataRegCor == 'Enter Raw Data') {
        if (!is.null(input$slrNavbarPage) && input$slrNavbarPage == "Uploaded Data") { # Check if navbarPage exists and selected
          updateNavbarPage(session, "slrNavbarPage", selected = "Model")
        }
        hideTab(inputId = "slrNavbarPage", target = "Uploaded Data")
      } else {
        showTab(inputId = "slrNavbarPage", target = "Uploaded Data")
      }
    })
    
    observeEvent(input$resetRegCor, {
      output$perfectFitWarning <- renderUI({ NULL })
      # hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
      # hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
      # hideTab(inputId = 'tabSet', target = 'Residual Plots')
      hide(id = "regCorrMP")
      shinyjs::reset("inputPanel")
      fileInputs$slrStatus <- 'reset'
      showTab(inputId = "slrNavbarPage", target = "Inference")
      showTab(inputId = "slrNavbarPage", target = "Diagnostic Plots") 
      if (!is.null(input$slrNavbarPage)) { # Check if navbarPage exists before trying to update
        updateNavbarPage(session, "slrNavbarPage", selected = "Model")
      }
    })
  })
}