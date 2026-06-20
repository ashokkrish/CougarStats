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
    tags$style(HTML("
      .disabled-tab {
        pointer-events: none !important;
        opacity: 0.4 !important;
        cursor: not-allowed !important;
      }
    ")),
    
    hidden(div(
      id = ns("regCorrMP"),
      uiOutput(ns("perfectFitWarning")),
      uiOutput(ns("missingRowsWarning")),
      uiOutput(ns("slrValidation")),
      
      div(
        id = ns("SLRData"),
        
        navbarPage(
          title = NULL,
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
              uiOutput(ns("diagnosticPlotsWarning")),
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
                  title = "Summary",
                  value = "Summary",
                  br(),
                  tableOutput(ns("correlationSummaryTable"))
                ),
                
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
                  title = "Spearman",
                  value = "Spearman",
                  titlePanel("Spearman's Rank Correlation Coefficient"),
                  br(),
                  uiOutput(ns("spearmanFormula")),
                  br(),
                  downloadButton(ns("downloadSpearmanXlsx"), "Save as Excel"),
                  br(),
                  br(),
                  uiOutput(ns("spearmanTable")),
                  br(),
                  hr()
                ),
                
                tabPanel(
                  title = "Kendall",
                  value = "Kendall",
                  titlePanel("Kendall's Rank Correlation Coefficient"),
                  br(),
                  uiOutput(ns("kendallTauComputation")),
                  br(),
                  hr(),
                  uiOutput(ns("kendallHypothesisTest")),
                  br(),
                  hr()
                )
                
                
                
              ) ## Nested tabsetPanel
            )
            
          ), ## Correlation Analysis tabPanel

          #### ---------------- Uploaded Data Tab ------------------------------------------
          tabPanel(
            title = "Uploaded Data",
            value = "Uploaded Data",
            div(
              DTOutput(ns("slrViewUploadTab")),
              style = "width: 75%"
            ),
            br(),
            br()
          ) #slrViewUploadTab tabpanel

        ) #slrNavbarPage navbarPage
      ) # SLRData div
    )), # regCorrMP div (hidden)
    
    # Uploaded Data — always visible, outside the results panel
    div(
      id = ns("uploadedDataPanel"),
      tags$h4(
        "Uploaded Data",
        style = "color: #18536F; font-weight: bold; margin-bottom: 15px; margin-top: 10px;"
      ),
      uiOutput(ns("uploadedDataContent")),
      br()
    )
    
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
        value       = "2.48, 2.26, 2.47, 2.77, 2.99, 3.05, 3.18, 3.46, 3.03, 3.26, 2.67, 2.53",
        placeholder = "Enter numeric values separated by a comma with decimals as points. (eg: 1,2,3)",
        rows        = 3),

      textAreaInput(
        inputId     = ns("x"),
        label       = strong("Explanatory Variable (\\( x\\))"),
        value       = "4.51, 3.58, 4.31, 5.06, 5.64, 4.99, 5.29, 5.83, 4.70, 5.61, 4.90, 4.20",
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
        min_n      = 6,
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
        min_n      = 6,
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
        min_n      = 5,
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
        min_n      = 7,
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
        min_n      = 5,
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
        min_n      = 4,
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

    nDroppedRows <- reactiveVal(0)

    hasHighLeverage <- reactiveVal(FALSE)
    
    hasLeveragePlotIssue <- reactiveVal(FALSE)
    
    output$diagnosticPlotsWarning <- renderUI({
      
      if (hasLeveragePlotIssue()) {
        div(
          class = "alert alert-warning",
          tags$b("⚠ Diagnostic Plot Warning: "),
          "Residuals vs Leverage could not be produced because all leverage values are 0.5."
        )
      }
    })
    
    outputOptions(output, "diagnosticPlotsWarning", suspendWhenHidden = FALSE)
    
    
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
    
    output$slrViewUpload <- renderDT({
      req(input$slrUserData)
      
      dat <- slrUploadData()
      
      datatable(
        dat,
        options = list(
          pageLength  = 25,
          lengthMenu  = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
          scrollX     = TRUE
        )
      )
    })
    
    outputOptions(output, "slrViewUpload", suspendWhenHidden = FALSE)

    output$slrViewUploadTab <- renderDT({
      req(input$slrUserData)
      dat <- slrUploadData()
      datatable(dat, options = list(pageLength = 25, lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")), scrollX = TRUE))
    })
    
    
    
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
    slrraw_iv$add_rule("x", ~ if (length(strsplit(input$x, ",")[[1]]) < 4) "Sample Data must include at least 4 numeric observations.")
    slrraw_iv$add_rule("x", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$diff != 0)) "x and y must have the same number of observations.",
      error = function(e) NULL
    ))
    slrraw_iv$add_rule("x", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$xSD == 0)) "Explanatory variable has a standard deviation equal to zero (all values are identical). At least two distinct values are required.",
      error = function(e) NULL
    ))
    
    slrraw_iv$add_rule("y", sv_required())
    slrraw_iv$add_rule("y", sv_regex("^\\s*-?\\d*\\.?\\d+(\\s*,\\s*-?\\d*\\.?\\d+)*\\s*$",
                                     "Data must be numeric values separated by a comma (ie: 2,3,4 or 2, 30, 400)."))
    slrraw_iv$add_rule("y", ~ if (length(strsplit(input$x, ",")[[1]]) < 4) "Sample Data must include at least 4 numeric observations.")
    slrraw_iv$add_rule("y", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$diff != 0)) "x and y must have the same number of observations.",
      error = function(e) NULL
    ))
    slrraw_iv$add_rule("y", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$ySD == 0)) "Response variable is constant. Correlation is undefined when a variable has a standard deviation equal to zero.",
      error = function(e) NULL
    ))
    
    slrupload_iv$add_rule("slrUserData", sv_required())
    slrupload_iv$add_rule("slrUserData", ~ if (is.null(fileInputs$slrStatus) || fileInputs$slrStatus == 'reset') "Required")
    slrupload_iv$add_rule("slrUserData", ~ if (!(tolower(tools::file_ext(input$slrUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    slrupload_iv$add_rule("slrUserData", ~ tryCatch(
      if (isTRUE(nrow(slrUploadData()) == 0)) "File is empty.",
      error = function(e) NULL
    ))
    slrupload_iv$add_rule("slrUserData", ~ tryCatch(
      if (isTRUE(ncol(slrUploadData()) < 2)) "Data must include one response and (at least) one explanatory variable.",
      error = function(e) NULL
    ))
    slrupload_iv$add_rule("slrUserData", ~ tryCatch(
      if (isTRUE(nrow(slrUploadData()) < 4)) "Samples must include at least 4 numeric observations.",
      error = function(e) NULL
    ))
    
    slruploadvars_iv$add_rule("slrExplanatory", sv_required())
    slruploadvars_iv$add_rule("slrExplanatory", ~ tryCatch(
      if (isTRUE(explanatoryInfoUploadSLR()$invalid)) "Explanatory variable contains non-numeric data.",
      error = function(e) NULL
    ))
    slruploadvars_iv$add_rule("slrExplanatory", ~ tryCatch(
      if (isTRUE(explanatoryInfoUploadSLR()$sd == 0)) "Explanatory variable has a standard deviation equal to zero (all values are identical). At least two distinct values are required.",
      error = function(e) NULL
    ))
    slruploadvars_iv$add_rule("slrExplanatory", ~ tryCatch({
      raw  <- suppressWarnings(as.numeric(as.data.frame(slrUploadData())[, input$slrExplanatory]))
      datx <- na.omit(raw)
      if (length(datx) < 4) "Explanatory variable has fewer than 4 non-missing numeric values."
    }, error = function(e) NULL))
    
    slruploadvars_iv$add_rule("slrResponse", sv_required())
    slruploadvars_iv$add_rule("slrResponse", ~ tryCatch(
      if (isTRUE(responseInfoUploadSLR()$invalid)) "Response variable contains non-numeric data.",
      error = function(e) NULL
    ))
    slruploadvars_iv$add_rule("slrResponse", ~ tryCatch(
      if (isTRUE(sampleDiffUpload() != 0)) "Missing values detected — x and y must have the same number of non-missing observations.",
      error = function(e) NULL
    ))
    slruploadvars_iv$add_rule("slrResponse", ~ tryCatch(
      if (isTRUE(responseInfoUploadSLR()$sd == 0)) "Response variable is constant. Correlation is undefined when a variable has a standard deviation equal to zero.",
      error = function(e) NULL
    ))
    slruploadvars_iv$add_rule("slrResponse", ~ tryCatch({
      raw  <- suppressWarnings(as.numeric(as.data.frame(slrUploadData())[, input$slrResponse]))
      daty <- na.omit(raw)
      if (length(daty) < 4) "Response variable has fewer than 4 non-missing numeric values."
    }, error = function(e) NULL))
    
    
     
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
      ext <- tolower(tools::file_ext(input$slrUserData$name))
      
      dat <- switch(ext,
                    csv  = read_csv(input$slrUserData$datapath, show_col_types = FALSE),
                    xls  = read_xls(input$slrUserData$datapath),
                    xlsx = read_xlsx(input$slrUserData$datapath),
                    txt  = read_tsv(input$slrUserData$datapath, show_col_types = FALSE),
                    validate("Improper file format.")
      )
      
      # Drop columns that are entirely NA (empty phantom columns from Excel)
      dat <- dat[, colSums(!is.na(dat)) > 0, drop = FALSE]
      
      # Drop rows where ALL columns are NA (empty phantom rows from Excel)
      dat <- dat[rowSums(!is.na(dat)) > 0, , drop = FALSE]
      
      dat
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
      req(input$slrExplanatory != "")
      dat <- list()
      tryCatch({
        raw  <- as.data.frame(slrUploadData())[, input$slrExplanatory]
        datx <- suppressWarnings(as.numeric(raw))
        datx <- na.omit(datx)
        dat$invalid <- length(datx) == 0 || any(is.na(suppressWarnings(as.numeric(raw[!is.na(raw)]))))
        dat$sd <- if (!dat$invalid) sd(datx) else 0
      }, error = function(e) {
        dat$invalid <<- TRUE
        dat$sd      <<- 0
      })
      return(dat)
    })
    
    responseInfoUploadSLR <- eventReactive(input$slrResponse, {
      req(input$slrResponse != "")
      dat <- list()
      tryCatch({
        raw  <- as.data.frame(slrUploadData())[, input$slrResponse]
        daty <- suppressWarnings(as.numeric(raw))
        daty <- na.omit(daty)
        dat$invalid <- length(daty) == 0 || any(is.na(suppressWarnings(as.numeric(raw[!is.na(raw)]))))
        dat$sd <- if (!dat$invalid) sd(daty) else 0
      }, error = function(e) {
        dat$invalid <<- TRUE
        dat$sd      <<- 0
      })
      return(dat)
    })
    
    sampleDiffUpload <- eventReactive(c(input$slrExplanatory, input$slrResponse), {
      if (input$slrResponse == "" | input$slrExplanatory == "") {
        return(0)
      } else {
        tryCatch({
          datx <- na.omit(as.numeric(as.data.frame(slrUploadData())[, input$slrExplanatory]))
          daty <- na.omit(as.numeric(as.data.frame(slrUploadData())[, input$slrResponse]))
          if (length(datx) == 0 || length(daty) == 0) return(-1)  # signals invalid
          return(length(datx) - length(daty))
        }, error = function(e) return(-1))
      }
    })
    
    #  ========================================================================= #
    ## -------- Observers ------------------------------------------------------
    #  ========================================================================= #
    
    output$uploadedDataContent <- renderUI({
      if (is.null(input$slrUserData) || 
          is.null(fileInputs$slrStatus) || 
          fileInputs$slrStatus == "reset") {
        div(
          class = "alert alert-info",
          style = "margin-top: 15px;",
          tags$b("No data uploaded. "),
          "Please upload a file using the sidebar to view your data here."
        )
      } else {
        tagList(
          DTOutput(session$ns("slrViewUpload"))
        )
      }
    })
    
    
    observeEvent(input$slrUserData, {
      fileInputs$slrStatus <- "uploaded"

      req(slrUploadData())
      updateSelectInput(
        inputId = "slrExplanatory",
        choices = colnames(slrUploadData())
      )
      updateSelectInput(
        inputId = "slrResponse",
        choices = colnames(slrUploadData())
      )
      show("slrExplanatory")
      show("slrResponse")
      show("uploadedDataPanel")
    })
    
    ## NOTE: related to the old plot options UI.
    observeEvent(input$slrExplanatory, {
      updateTextInput(inputId = "xlab", value = input$slrExplanatory)
      output$perfectFitWarning <- renderUI({ NULL })
    })
    observeEvent(input$slrResponse, {
      updateTextInput(inputId = "ylab", value = input$slrResponse)
      output$perfectFitWarning <- renderUI({ NULL })
    })

    observeEvent(input$goRegression, {
      ## SLR Validation messages ----
      output$perfectFitWarning <- renderUI({ NULL })
      showTab(inputId = "slrNavbarPage", target = "Inference")
      showTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
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
          
          # 
          
          
          
          
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
            need(input$slrResponse != "", "Please select a Response Variable (y)."),
            errorClass = "myClass")

          validate(
            need(!explanatoryInfoUploadSLR()$invalid, "The Explanatory Variable (x) contains non-numeric data.") %then%
              need(explanatoryInfoUploadSLR()$sd != 0, "Explanatory Variable (x) must have a standard deviation greater than zero to perform regression and correlation analysis."),
            need(!responseInfoUploadSLR()$invalid, "The Response Variable (y) contains non-numeric data.") %then%
              need(responseInfoUploadSLR()$sd != 0, "Response Variable (y) must have a standard deviation greater than zero to perform correlation analysis."),
            errorClass = "myClass")

          validate(
            need(sampleDiffUpload() == 0, "The Explanatory (x) and Response (y) variables must have the same number of observations."),
            errorClass = "myClass")
        }
        
        if(input$dataRegCor == 'Upload Data') {
          req(input$slrExplanatory %in% colnames(slrUploadData()))
          req(input$slrResponse %in% colnames(slrUploadData()))
          raw_x <- suppressWarnings(as.numeric(as.data.frame(slrUploadData())[, input$slrExplanatory]))
          raw_y <- suppressWarnings(as.numeric(as.data.frame(slrUploadData())[, input$slrResponse]))
          complete_idx <- !is.na(raw_x) & !is.na(raw_y)
          datx <- raw_x[complete_idx]
          daty <- raw_y[complete_idx]
          if(length(datx) < 4) {
            showNotification("After removing missing values, fewer than 4 complete observations remain. Please choose different variables.", type = "error", duration = 8)
            return()
          }
          nDroppedRows(sum(!complete_idx))
        } else {
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
            need(sampleInfoRaw()$xSD != 0, "Explanatory variable (x) must have a standard deviation greater than zero to perform regression and correlation analysis."),
            need(sampleInfoRaw()$ySD != 0, "Response variable (y) must have a standard deviation greater than zero to perform correlation analysis."),
            errorClass = "myClass")
        }
      }) #output$slrValidation
      
      if(regcor_iv$is_valid()) {
        if (input$dataRegCor == 'Upload Data') {
          hide("uploadedDataPanel")
          showTab(inputId = "slrNavbarPage", target = "Uploaded Data")
        } else {
          hide("uploadedDataPanel")
          hideTab(inputId = "slrNavbarPage", target = "Uploaded Data")
        }

        if(input$dataRegCor == 'Upload Data') {
          req(input$slrExplanatory %in% colnames(slrUploadData()))
          req(input$slrResponse %in% colnames(slrUploadData()))
          raw_x <- suppressWarnings(as.numeric(as.data.frame(slrUploadData())[, input$slrExplanatory]))
          raw_y <- suppressWarnings(as.numeric(as.data.frame(slrUploadData())[, input$slrResponse]))
          complete_idx <- !is.na(raw_x) & !is.na(raw_y)
          datx <- raw_x[complete_idx]
          daty <- raw_y[complete_idx]
        } else {
          datx <- createNumLst(input$x)
          daty <- createNumLst(input$y)
        }

        model <- lm(daty ~ datx)
        
        h <- hatvalues(model)
        hasLeveragePlotIssue(
          all(abs(h - 0.5) < .Machine$double.eps^0.5)
        )
        
        hasHighLeverage(any(hatvalues(model) >= 1))
        
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
        output$missingRowsWarning <- renderUI({
          n <- nDroppedRows()
          if (n > 0) {
            div(
              class = "alert alert-warning",
              role  = "alert",
              style = "margin-top: 10px;",
              tags$b("⚠️ Missing Data Detected: "),
              sprintf("%d row%s with missing values removed before analysis.", n, if (n == 1) "" else "s")
            )
          } else {
            NULL
          }
        })

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
              "This may indicate that",
              tags$b("x and y are identical or linearly dependent,"),
              ("which can produce unreliable inference and diagnostic plots. Standard statistical significance tests cannot run on perfect fits. Please check your data.")
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
          plot(model, which = 3, pch = 20, main = "", lwd = 2, sub.caption = "", caption = "", ann = FALSE)
          title(main = "Scale-Location", cex.main = 1.2)
          title(ylab = "sqrt(|Standardized Residuals|)")
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
            p("The estimated equation of the regression line is"),
            p(sprintf("\\( \\qquad \\hat{y} = \\hat{\\beta}_{0} + \\hat{\\beta}_{1} x \\)")),
            p("where"),
            p(sprintf(
              "\\( \\qquad \\hat{\\beta}_{1} = \\dfrac{ \\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } = \\dfrac{ %s - \\dfrac{ (%s)(%s) }{ %s } }{ %s - \\dfrac{ (%s)^2 }{ %s } } = %0.4f \\)",
              format(round(dfTotaled["Totals", "xy"], 3), nsmall = 0, scientific = FALSE),
              format(round(dfTotaled["Totals", "x"], 3), nsmall = 0, scientific = FALSE),
              format(round(dfTotaled["Totals", "y"], 3), nsmall = 0, scientific = FALSE),
              format(round(length(datx), 3), nsmall = 0, scientific = FALSE),
              format(round(dfTotaled["Totals", "x<sup>2</sup>"], 3), nsmall = 0, scientific = FALSE),
              format(round(dfTotaled["Totals", "x"], 3), nsmall = 0, scientific = FALSE),
              format(round(length(datx), 3), nsmall = 0, scientific = FALSE),
              slopeEstimate
            )),
            p("and"),
            p(sprintf(
              "\\( \\qquad \\hat{\\beta}_{0} = \\bar{y} - \\hat{\\beta}_{1} \\bar{x} = %s - (%0.4f)(%s) = %s %s %0.4f = %0.4f \\)",
              format(round(mean(daty), 3), nsmall = 0, scientific = FALSE),
              summary(model)$coefficients["datx", "Estimate"],
              format(round(mean(datx), 3), nsmall = 0, scientific = FALSE),
              format(round(mean(daty), 3), nsmall = 0, scientific = FALSE),
              b0HatOp,
              abs(slopeEstimate) * mean(datx),
              interceptEstimate
            )),
            br(),
            p(sprintf("\\( \\qquad \\hat{y} = %0.4f %s %0.4f x \\)",
                    interceptEstimate,
                    yHatOp,
                    abs(slopeEstimate))),
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
              = \\dfrac{ %s }{\\sqrt{ %s } \\times \\sqrt{ %s }}
              = %.4f} \\)",
                        
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
                        
                        # simplified √ form — use scientific notation when values would round to 0
                        fmt_sci_latex(dfTotaled["Totals", "xy"] - sumXSumY / length(datx)),

                        fmt_sci_latex(dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx)),

                        fmt_sci_latex(dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx)),
                        
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
                
                if (!isTRUE(all.equal(r_squared, 1))) tagList(
                  
                  # Population Correlation Coefficient
                  hr(),
                  p(HTML(paste0(strong("Hypothesis Test for the Population Correlation Coefficient"), " \\((\\rho)\\)"))),
                  p(HTML("H<sub>0</sub>: \\(\\rho = 0\\)")),
                  p(HTML("H<sub>a</sub>: \\(\\rho \\neq 0\\)")),
                  p("\\(\\alpha = 0.05\\)"),
                  p(sprintf("\\( df = n - 2 = %d \\)", pearson$parameter)),
                  
                  p(strong("Test Statistic:")),
                  p(sprintf(
                    "\\( t = \\dfrac{r\\sqrt{n-2}}{\\sqrt{1-r^2}} = \\dfrac{%0.4f\\sqrt{%d-2}}{\\sqrt{1-%0.4f^2}} = %0.4f \\)",
                    pearson$estimate, n, pearson$estimate, pearson$statistic
                  )),
                  
                  p(strong("Using P-Value Method:")),
                  p(sprintf(
                    "\\( P = 2 \\times P(t > |\\, %0.4f \\,|) %s \\)",
                    pearson$statistic,
                    pval_tex(pearson$p.value)
                  )),
                  if(pearson$p.value <= 0.05) {
                    p(sprintf("Since \\( P \\leq 0.05 \\), reject \\( H_0 \\)."))
                  } else {
                    p(sprintf("Since \\( P > 0.05 \\), fail to reject \\( H_0 \\)."))
                  },
                  
                  br(),
                  
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
                  
                  plotOutput(session$ns("pearsonTCurve")),
                  
                  p(strong("Conclusion:")),
                  if(pearson$p.value <= 0.05) {
                    p("At \\( \\alpha = 0.05 \\), since the test statistic falls in the rejection region we reject \\( H_0 \\) and conclude that there is enough statistical evidence of a linear relationship between \\( x \\) and \\( y \\) in the population.")
                  } else {
                    p("At \\( \\alpha = 0.05 \\), since the test statistic does not fall in the rejection region we fail to reject \\( H_0 \\) and conclude that there is not enough statistical evidence of a linear relationship between \\( x \\) and \\( y \\) in the population.")
                  },
                  
                  # Fischer Transform
                  hr(),
                  p(HTML(paste0(strong("Confidence Interval for the Population Correlation Coefficient \\((\\rho)\\) using Fisher z-Transformation")))),
                  
                  p("Since the sampling distribution of Pearson's \\(r\\) is not normal, we use the Fisher z-Transformation to construct a confidence interval."),

                  p(strong(withMathJax("Step 1: Transform \\(r\\)"))),
                  p(sprintf(
                    "\\( z_r = \\dfrac{1}{2} \\ln\\left(\\dfrac{1+r}{1-r}\\right) = \\text{artanh}(r) = \\dfrac{1}{2} \\ln\\left(\\dfrac{1+(%0.4f)}{1-(%0.4f)}\\right) = %0.4f \\)",
                    pearson$estimate, pearson$estimate, atanh(pearson$estimate)
                  )),
                  
                  p(strong("Step 2: Standard Error")),
                  p(sprintf(
                    "\\( SE_{z_r} = \\dfrac{1}{\\sqrt{n-3}} = \\dfrac{1}{\\sqrt{%d-3}} = %0.4f \\)",
                    n, 1/sqrt(n-3)
                  )),
                  
                  p(sprintf(
                    "\\( \\left(z_r - Z_{\\alpha/2} \\cdot SE_{z_r}, \\; z_r + Z_{\\alpha/2} \\cdot SE_{z_r}\\right) = \\left((%0.4f) - 1.96 \\times %0.4f, \\; (%0.4f) + 1.96 \\times %0.4f\\right) = \\left(%0.4f, \\; %0.4f\\right) \\)",
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
                  
                ) # end if (!isPerfectFit)
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
        
        kendall  <- suppressWarnings(cor.test(datx, daty, method = "kendall"))
        spearman <- suppressWarnings(cor.test(datx, daty, method = "spearman"))
        
        kendallStats <- local({
          n  <- length(datx)
          n0 <- n * (n - 1) / 2
          n1 <- sum(choose(table(datx), 2))
          n2 <- sum(choose(table(daty), 2))
          has_ties <- (n1 > 0 || n2 > 0)
          nc <- 0
          nd <- 0
          for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
              dx <- datx[i] - datx[j]
              dy <- daty[i] - daty[j]
              if (sign(dx) == sign(dy) && dx != 0 && dy != 0) nc <- nc + 1
              else if (sign(dx) != sign(dy) && dx != 0 && dy != 0) nd <- nd + 1
            }
          }
          list(n = n, n0 = n0, n1 = n1, n2 = n2, has_ties = has_ties, nc = nc, nd = nd)
        })

        output$kendallTauComputation <- renderUI({
          ks  <- kendallStats
          tau <- as.numeric(kendall$estimate)

          if (!ks$has_ties) {
            formula_note <- "Since there are no ties in the data, we use Kendall's \\(\\tau_a\\):"
            sym_formula  <- "\\tau_a = \\dfrac{n_c - n_d}{\\dfrac{n(n-1)}{2}}"
            num_formula  <- sprintf(
              "\\tau_a = \\dfrac{%d - %d}{\\dfrac{%d \\cdot (%d - 1)}{2}} = \\dfrac{%d}{%g} = %.4f",
              ks$nc, ks$nd, ks$n, ks$n, ks$nc - ks$nd, ks$n0, tau
            )
          } else {
            formula_note <- "Since there are ties in the data, we use Kendall's \\(\\tau_b\\):"
            sym_formula  <- "\\tau_b = \\dfrac{n_c - n_d}{\\sqrt{(n_0 - n_1)(n_0 - n_2)}}"
            num_formula  <- sprintf(
              "\\tau_b = \\dfrac{%d - %d}{\\sqrt{(%g - %g)(%g - %g)}} = %.4f",
              ks$nc, ks$nd, ks$n0, ks$n1, ks$n0, ks$n2, tau
            )
          }

          tauStrength  <- if (abs(tau) > 0.6) "strong" else if (abs(tau) > 0.3) "moderate" else "weak"
          tauDirection <- if (tau > 0) "positive" else "negative"

          withMathJax(
            p(formula_note),
            p(HTML(sprintf("\\( %s \\)", sym_formula))),
            p(HTML(sprintf("\\( %s \\)", num_formula))),
            br(),
            p(tags$b("Interpretation:")),
            if (tau == 0) {
              p("There exists no monotonic relationship between \\(\\mathit{x}\\) and \\(\\mathit{y}\\).")
            } else {
              p(sprintf(
                "There exists a %s %s monotonic relationship between \\(\\mathit{x}\\) and \\(\\mathit{y}\\).",
                tauStrength, tauDirection
              ))
            }
          )
        })

        output$kendallHypothesisTest <- renderUI({
          ks  <- kendallStats
          tau <- as.numeric(kendall$estimate)

          tauStrength  <- if (isTRUE(abs(tau) > 0.6)) "strong" else if (isTRUE(abs(tau) > 0.3)) "moderate" else "weak"
          tauDirection <- if (isTRUE(tau > 0)) "positive" else "negative"

          header <- tagList(
            p("Kendall's Tau has a formal hypothesis test for whether two variables are monotonically associated."),
            br(),
            HTML("<p>\\(H_0\\): The true Kendall's Tau in the population is <strong>0</strong> (no monotonic association).</p>"),
            HTML("<p>\\(H_a\\): The true Kendall's Tau is <strong>not</strong> equal to 0 (some monotonic association).</p>"),
            br(),
            p("\\( \\alpha = 0.05 \\)"),
            p(sprintf("\\( n = %d \\)", ks$n)),
            br()
          )

          if (!ks$has_ties) {
            sd_tau <- sqrt(2 * (2 * ks$n + 5) / (9 * ks$n * (ks$n - 1)))
            z_stat <- tau / sd_tau
            p_val  <- 2 * pnorm(-abs(z_stat))

            withMathJax(
              header,
              p(tags$b("Mean & Standard Deviation of the sampling distribution of \\( \\hat{\\tau} \\)")),
              p("\\( E(\\hat{\\tau}) = 0 \\)"),
              p("\\( SD(\\hat{\\tau}) = \\sqrt{\\dfrac{2(2n+5)}{9n(n-1)}} \\)"),
              br(),
              p(tags$b("Test Statistic:")),
              p(HTML(sprintf(
                "\\( z = \\dfrac{\\hat{\\tau} - E(\\hat{\\tau})}{SD(\\hat{\\tau})} = \\dfrac{\\hat{\\tau} - 0}{\\sqrt{\\dfrac{2(2n+5)}{9n(n-1)}}} = \\dfrac{%.4f - 0}{\\sqrt{\\dfrac{2(2(%d)+5)}{9(%d)(%d-1)}}} = %.4f \\)",
                tau, ks$n, ks$n, ks$n, z_stat
              ))),
              br(),

              p(strong("Using P-Value Method:")),
              p(sprintf("\\( P = 2 \\times P(Z > |\\, %.4f \\,|) %s \\)", z_stat, pval_tex(p_val))),
              if (isTRUE(p_val <= 0.05)) {
                p("Since \\( P \\leq 0.05 \\), reject \\( H_0 \\).")
              } else {
                p("Since \\( P > 0.05 \\), fail to reject \\( H_0 \\).")
              },
              br(),

              p(strong("Using Critical Value Method:")),
              p("\\( \\text{Critical Value(s)} = \\pm z_{\\alpha/2} = \\pm z_{0.025} = \\pm 1.96 \\)"),
              if (isTRUE(abs(z_stat) > 1.96)) {
                p(sprintf(
                  "Since the test statistic \\( (z = %.4f) \\) falls within the rejection region, reject \\( H_0 \\).",
                  z_stat
                ))
              } else {
                p(sprintf(
                  "Since the test statistic \\( (z = %.4f) \\) does not fall within the rejection region, fail to reject \\( H_0 \\).",
                  z_stat
                ))
              },

              div(style = "margin-bottom: -30px;", plotOutput(session$ns("kendallZCurve"))),

              p(tags$b("Conclusion:")),
              if (isTRUE(p_val <= 0.05)) {
                p(sprintf(
                  "At \\( \\alpha = 0.05 \\), we reject \\( H_0 \\) and conclude that there is enough statistical evidence of a %s %s monotonic relationship between \\( x \\) and \\( y \\) in the population.",
                  tauStrength, tauDirection
                ))
              } else {
                p("At \\( \\alpha = 0.05 \\), we fail to reject \\( H_0 \\) and conclude that there is not enough statistical evidence of a monotonic relationship between \\( x \\) and \\( y \\) in the population.")
              }
            )
          } else {
            # Ties: use cor.test's tie-corrected z statistic and p-value
            z_stat <- as.numeric(kendall$statistic)
            p_val  <- kendall$p.value

            withMathJax(
              header,
              p(em("Note: Ties are present in the data. The test statistic and p-value are computed using R's tie-corrected normal approximation.")),
              br(),
              p(tags$b("Test Statistic:")),
              p(sprintf("\\( z = %.4f \\)", z_stat)),
              br(),

              p(strong("Using P-Value Method:")),
              p(sprintf("\\( P = 2 \\times P(Z > |\\, %.4f \\,|) %s \\)", z_stat, pval_tex(p_val))),
              if (isTRUE(p_val <= 0.05)) {
                p("Since \\( P \\leq 0.05 \\), reject \\( H_0 \\).")
              } else {
                p("Since \\( P > 0.05 \\), fail to reject \\( H_0 \\).")
              },
              br(),

              p(strong("Using Critical Value Method:")),
              p("\\( \\text{Critical Value(s)} = \\pm z_{\\alpha/2} = \\pm z_{0.025} = \\pm 1.96 \\)"),
              if (isTRUE(abs(z_stat) > 1.96)) {
                p(sprintf(
                  "Since the test statistic \\( (z = %.4f) \\) falls within the rejection region, reject \\( H_0 \\).",
                  z_stat
                ))
              } else {
                p(sprintf(
                  "Since the test statistic \\( (z = %.4f) \\) does not fall within the rejection region, fail to reject \\( H_0 \\).",
                  z_stat
                ))
              },

              div(style = "margin-bottom: -30px;", plotOutput(session$ns("kendallZCurve"))),

              p(tags$b("Conclusion:")),
              if (isTRUE(p_val <= 0.05)) {
                p(sprintf(
                  "At \\( \\alpha = 0.05 \\), we reject \\( H_0 \\) and conclude that there is enough statistical evidence of a %s %s monotonic relationship between \\( x \\) and \\( y \\) in the population.",
                  tauStrength, tauDirection
                ))
              } else {
                p("At \\( \\alpha = 0.05 \\), we fail to reject \\( H_0 \\) and conclude that there is not enough statistical evidence of a monotonic relationship between \\( x \\) and \\( y \\) in the population.")
              }
            )
          }
        })

        output$kendallZCurve <- renderPlot({
          ks <- kendallStats
          if (!ks$has_ties) {
            tau    <- as.numeric(kendall$estimate)
            sd_tau <- sqrt(2 * (2 * ks$n + 5) / (9 * ks$n * (ks$n - 1)))
            z_stat <- round(tau / sd_tau, 3)
          } else {
            z_stat <- round(as.numeric(kendall$statistic), 3)
          }
          hypZTestPlot(
            testStatistic = z_stat,
            critValue     = 1.96,
            altHypothesis = "two.sided"
          )
        }, height = 300, width = 500)

        spearman_cf <- function(x) {
          tbl   <- table(x)
          ties  <- as.numeric(tbl[tbl > 1])
          if (length(ties) == 0) return(0)
          sum((ties^3 - ties) / 12)
        }

        spearmanData <- reactive({
          rank_x <- rank(datx)
          rank_y <- rank(daty)
          d      <- rank_x - rank_y
          data.frame(
            x      = datx,
            y      = daty,
            rank_x = rank_x,
            rank_y = rank_y,
            d      = d,
            d_sq   = d^2
          )
        })

        output$downloadSpearmanXlsx <- downloadHandler(
          filename    = function() paste0("Spearman_Rank_Correlation_", Sys.Date(), ".xlsx"),
          contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          content     = function(file) {
            tryCatch({
              data <- spearmanData()
              names(data) <- c("x", "y", "Rank x", "Rank y", "d = (Rank x - Rank y)", "d^2")
              writexl::write_xlsx(data, file)
            }, error = function(e) {
              message("Full error: ", conditionMessage(e))
            })
          }
        )

        # Spearman's rs formula and interpretation
        output$spearmanFormula <- renderUI({

          sp_data  <- spearmanData()
          d_sq     <- sp_data$d_sq
          sum_d_sq <- sum(d_sq)
          n        <- length(datx)
          cf_x     <- spearman_cf(datx)
          cf_y     <- spearman_cf(daty)

          has_ties <- cf_x > 0 || cf_y > 0

          if (has_ties) {
            rank_x   <- sp_data$rank_x
            rank_y   <- sp_data$rank_y
            sum_rxry <- sum(rank_x * rank_y)
            mean_rx  <- mean(rank_x)
            mean_ry  <- mean(rank_y)
            sd_rx    <- sd(rank_x)
            sd_ry    <- sd(rank_y)
            rs       <- (sum_rxry - n * mean_rx * mean_ry) / ((n - 1) * sd_rx * sd_ry)
            formula_latex <- sprintf(
              "\\( r_s = \\dfrac{\\sum_{i=1}^n R_{x_i} R_{y_i} - n\\bar{R}_x\\bar{R}_y}{(n-1)\\,s_{R_x}s_{R_y}} = \\dfrac{%g - %d \\times %g \\times %g}{(%d - 1) \\times %g \\times %g} = %.4f \\)",
              sum_rxry, n, round(mean_rx, 4), round(mean_ry, 4), n, round(sd_rx, 4), round(sd_ry, 4), rs
            )
          } else {
            rs <- 1 - (6 * sum_d_sq) / (n * (n^2 - 1))
            formula_latex <- sprintf(
              "\\( r_s = 1 - \\dfrac{6 \\sum_{i=1}^n d_i^2}{n(n^2 - 1)} = 1 - \\dfrac{6 \\times %g}{%d(%d^2 - 1)} = %.4f \\)",
              sum_d_sq, n, n, rs
            )
          }

          rsStrength  <- if (abs(rs) > 0.6) "strong" else if (abs(rs) > 0.3) "moderate" else "weak"
          rsDirection <- if (rs > 0) "positive" else "negative"

          withMathJax(
            div(
              style = "text-align: left; font-size: 18px;",
              HTML(formula_latex)
            ),
            br(),
            p(tags$b("Interpretation:")),
            if (rs == 0) {
              p("There exists no monotonic relationship between \\(\\mathit{x}\\) and \\(\\mathit{y}\\).")
            } else {
              p(sprintf(
                "There exists a %s %s monotonic relationship between \\(\\mathit{x}\\) and \\(\\mathit{y}\\).",
                rsStrength, rsDirection
              ))
            }
          )
        })

        # Spearman's rank table
        output$spearmanTable <- renderUI({

          spearman_df <- spearmanData()
          sum_d_sq    <- sum(spearman_df$d_sq)

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
              d      = colDef(name = "d = (Rank x \u2212 Rank y)", align = "center", footer = tags$b("Total"), minWidth = 190),
              d_sq   = colDef(
                name   = HTML("d<sup>2</sup>"),
                html   = TRUE,
                align  = "center",
                footer = tags$b(sum_d_sq),
                cell = function(value) {
                  if (value == floor(value)) {
                    formatC(value, format = "d", big.mark = ",")
                  } else {
                    formatC(value, format = "f", digits = 2)
                  }
                }
              )
            )
          )
        })
    
        
        
        output$correlationSummaryTable <- renderTable({
          data.frame(
            `Correlation Coefficient` = c(
              "Pearson's <em>r</em>",
              "Spearman's <em>r</em><sub>s</sub>",
              "Kendall's <em>&tau;</em>"
            ),
            Estimate = c(
              sprintf("%.4f", round(pearson$estimate, 4)),
              sprintf("%.4f", round(spearman$estimate, 4)),
              sprintf("%.4f", round(kendall$estimate, 4))
            ),
            check.names = FALSE
          )
        },
        bordered  = TRUE,
        hover     = TRUE,
        align     = "c",
        sanitize.text.function = function(x) x
        )
        
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
            p(sprintf("\\( \\displaystyle F = \\frac{\\mathrm{MSR}}{\\mathrm{MSE}} = \\frac{%s}{%s} = %.3f \\)", fmt_sci_latex(msr), fmt_sci_latex(mse), f_value)),
            p(strong("Conclusion:")),
            if (p_value <= 0.05) {
              p(sprintf("Since the p-value is less than \\( \\alpha \\) (%.3f < 0.05), we reject the null hypothesis and conclude there is enough statistical evidence to support the alternative hypothesis. We can conclude the model is statistically significant.", p_value))
            } else {
              p(sprintf("Since the p-value is greater than \\( \\alpha \\) (%.3f >  0.05), we fail to reject the null hypothesis and conclude there isn't enough statistical evidence to support the alternative hypothesis. We can conclude the model is not statistically significant.", p_value))
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
                "\\( R^2 = \\dfrac{\\mathrm{SSR}}{\\mathrm{SSR} + \\mathrm{SSE}} = \\dfrac{\\mathrm{SSR}}{\\mathrm{SST}} = \\dfrac{%s}{%s + %s} = \\dfrac{%s}{%s} = %.4f \\)",
                fmt_sci_latex(ssr, 4), fmt_sci_latex(ssr, 4), fmt_sci_latex(sse, 4),
                fmt_sci_latex(ssr, 4), fmt_sci_latex(sst, 4), r2
              ))
            ),
            
            br(),
            
            tags$p(
              strong("Interpretation:")
            ),
            
            tags$p(
              sprintf("Approximately %.2f%% of the variation in ", explained_pct),
              if (input$dataRegCor == "Upload Data") tags$i(input$slrResponse) else withMathJax("\\(y\\)"),
              " can be explained by its linear relationship with ",
              if (input$dataRegCor == "Upload Data") tags$i(input$slrExplanatory) else withMathJax("\\(x\\)"),
              "."
            )
            
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
    observeEvent(regcor_iv$is_valid(), {
      if (!isTRUE(regcor_iv$is_valid())) {
        hide(id = "regCorrMP")
        hide(id = "SLRData")
      }
    })
    
    observeEvent(input$dataRegCor, {
      hide(id = "regCorrMP")
      if (input$dataRegCor == "Upload Data" &&
          !is.null(fileInputs$slrStatus) &&
          fileInputs$slrStatus == "uploaded") {
        show("uploadedDataPanel")
      } else {
        hide("uploadedDataPanel")
      }
      hideTab(inputId = "slrNavbarPage", target = "Uploaded Data")
      output$perfectFitWarning <- renderUI({ NULL })
      nDroppedRows(0)

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
    
    # observe({
    #   req(isTruthy(input$dataRegCor))
    #   if(input$dataRegCor == 'Enter Raw Data') {
    #     if (!is.null(input$slrNavbarPage) && input$slrNavbarPage == "Uploaded Data") { # Check if navbarPage exists and selected
    #       updateNavbarPage(session, "slrNavbarPage", selected = "Model")
    #     }
    #     hideTab(inputId = "slrNavbarPage", target = "Uploaded Data")
    #   } else {
    #     showTab(inputId = "slrNavbarPage", target = "Uploaded Data")
    #   }
    # })
     
    observeEvent(input$resetRegCor, {
      hasHighLeverage(FALSE)
      nDroppedRows(0)
      output$perfectFitWarning <- renderUI({ NULL })
      hide(id = "regCorrMP")
      hide("uploadedDataPanel")
      hideTab(inputId = "slrNavbarPage", target = "Uploaded Data")
      shinyjs::reset("inputPanel")
      fileInputs$slrStatus <- 'reset'
      showTab(inputId = "slrNavbarPage", target = "Inference")
      showTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
      if (!is.null(input$slrNavbarPage)) {
        updateNavbarPage(session, "slrNavbarPage", selected = "Model")
      }
    })
  })
}