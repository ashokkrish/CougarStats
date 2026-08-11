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
      .copy-plot-btn {
        margin-top: 10px;
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
    ")),
    tags$script(HTML("
      function copyPlotToClipboard(plotId) {
        var plotDiv = document.getElementById(plotId);
        if (!plotDiv) return;
        var btn = document.querySelector('[data-copy-plot=\"' + plotId + '\"]');

        Plotly.toImage(plotDiv, {format: 'png', width: plotDiv.offsetWidth, height: plotDiv.offsetHeight})
          .then(function(dataUrl) { return fetch(dataUrl); })
          .then(function(res) { return res.blob(); })
          .then(function(blob) {
            return navigator.clipboard.write([new ClipboardItem({'image/png': blob})]);
          })
          .then(function() {
            if (btn) {
              var orig = btn.innerHTML;
              btn.innerHTML = '<i class=\"fa fa-check\"></i> Copied!';
              btn.disabled = true;
              setTimeout(function() {
                btn.innerHTML = orig;
                btn.disabled = false;
              }, 2000);
            }
          })
          .catch(function(err) {
            alert('Could not copy to clipboard. Your browser may not support this feature, or the page must be served over HTTPS.');
          });
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
                id                = ns("slrScatter"),
                plotType          = "Scatterplot",
                title             = "Scatterplot",
                xlab              = "x",
                ylab              = "y",
                dim               = "in px",
                includeFlip       = FALSE,
                includeMeansOption = TRUE),
              
              plotlyOutput(ns("slrScatterplot"),
                           height = "700px",
                           width  = "100%"),

              tags$button(
                class             = "btn btn-default copy-plot-btn",
                `data-copy-plot`  = ns("slrScatterplot"),
                onclick           = paste0("copyPlotToClipboard('", ns("slrScatterplot"), "')"),
                tags$i(class = "fa fa-clipboard"),
                "Copy to Clipboard"
              ),

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
                  p(strong("ANOVA Table:")),
                  div(DTOutput(ns("anovaTable")), width = "100 px;"),
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
            
    
          
          
          #### ---------------- Prediction Tab ---------------------------------------
          tabPanel(
            title = "Prediction",
            value = "Prediction",

            fluidPage(
              br(),
              div(
                style = "max-width: 500px;",
                numericInput(
                  inputId = ns("slrPredictXTab"),
                  label   = strong("Predict at \\( x_0 \\) ="),
                  value   = NA,
                  step    = 0.1,
                  width   = "220px"
                )
              ),
              uiOutput(ns("slrPrediction")),
              br()
            )
          ), # Prediction tabpanel

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
        placeholder = "Enter numeric values separated by commas or spaces with decimals as points. (eg: 1,2,3 or 1 2 3)",
        rows        = 3),

      textAreaInput(
        inputId     = ns("x"),
        label       = strong("Explanatory Variable (\\( x\\))"),
        value       = "4.51, 3.58, 4.31, 5.06, 5.64, 4.99, 5.29, 5.83, 4.70, 5.61, 4.90, 4.20",
        placeholder = "Enter numeric values separated by commas or spaces with decimals as points (eg: 1,2,3 or 1 2 3).",
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
          label   = strong("Upload your data (.csv, .xls, .xlsx, .txt, .sas7bdat, .sav, .dta, .rds, .mtp, .mwx, .mpx)"),
          accept  = c("text/csv",
                      "text/comma-separated-values",
                      "text/tab-separated-values",
                      "text/plain",
                      ".csv",
                      ".txt",
                      ".xls",
                      ".xlsx",
                      ".sas7bdat",
                      ".sav",
                      ".dta",
                      ".rds",
                      ".mtp",
                      ".mwx",
                      ".mpx")),

        conditionalPanel(
          ns = ns,
          condition = "output.slrShowSheetPicker == true",
          selectizeInput(
            inputId  = ns("slrSheet"),
            label    = strong("Choose a Sheet"),
            choices  = c(""),
            multiple = FALSE,
            options  = list(placeholder = 'Select a sheet',
                            onInitialize = I('function() { this.setValue(""); }')))
        ),

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
          resids <- residuals(model)
          has_ties <- anyDuplicated(resids) > 0
          ks <- suppressWarnings(ks.test(resids, "pnorm",
                                         mean = mean(resids),
                                         sd   = sd(resids)))
          note <- if (has_ties) "Ties detected in residuals; p-value may be unreliable." else NULL
          list(
            statistic = round(ks$statistic, 4),
            p_value   = round(ks$p.value, 4),
            note      = note
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

    # Stored model for the Prediction tab
    slrModel <- reactiveVal(NULL)
    slrDatX  <- reactiveVal(NULL)
    slrDatY  <- reactiveVal(NULL)

    # Shared trigger for Calculate and Predict buttons
    # dest: which tab to navigate to after the calculation
    calcTrigger <- reactiveVal(list(n = 0L, dest = "Model"))
    
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
    slrraw_iv$add_rule("x", ~ if (nzchar(trimws(input$x)) && length(createNumLst(input$x)) == 0)
                                     "Data must be numeric values separated by commas or spaces (ie: 2,3,4 or 2 30 400).")
    slrraw_iv$add_rule("x", ~ if (length(createNumLst(input$x)) < 4) "Sample Data must include at least four numeric observations.")
    slrraw_iv$add_rule("x", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$diff != 0)) "x and y must have the same number of observations.",
      error = function(e) NULL
    ))
    slrraw_iv$add_rule("x", ~ tryCatch(
      if (isTRUE(sampleInfoRaw()$xSD == 0)) "Explanatory variable has a standard deviation equal to zero (all values are identical). At least two distinct values are required.",
      error = function(e) NULL
    ))
    
    slrraw_iv$add_rule("y", sv_required())
    slrraw_iv$add_rule("y", ~ if (nzchar(trimws(input$y)) && length(createNumLst(input$y)) == 0)
                                     "Data must be numeric values separated by commas or spaces (ie: 2,3,4 or 2 30 400).")
    slrraw_iv$add_rule("y", ~ if (length(createNumLst(input$y)) < 4) "Sample Data must include at least four numeric observations.")
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
    slrupload_iv$add_rule("slrUserData", ~ if (!(tolower(tools::file_ext(input$slrUserData$name)) %in% UPLOAD_ACCEPTED_EXTENSIONS)) "File format not accepted.")
    slrupload_iv$add_rule("slrUserData", ~ tryCatch(
      if (isTRUE(nrow(slrUploadData()) == 0)) "File is empty.",
      error = function(e) NULL
    ))
    slrupload_iv$add_rule("slrUserData", ~ tryCatch(
      if (isTRUE(ncol(slrUploadData()) < 2)) "Data must include one response and (at least) one explanatory variable.",
      error = function(e) NULL
    ))
    slrupload_iv$add_rule("slrUserData", ~ tryCatch(
      if (isTRUE(nrow(slrUploadData()) < 4)) "Samples must include at least four numeric observations.",
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
      if (length(datx) < 4) "Explanatory variable has fewer than four non-missing numeric values."
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
      if (length(daty) < 4) "Response variable has fewer than four non-missing numeric values."
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
    
    slrUploadData <- eventReactive(list(input$slrUserData, input$slrSheet), {
      req(input$slrUserData)
      ext  <- tolower(tools::file_ext(input$slrUserData$name))
      path <- input$slrUserData$datapath

      if (ext %in% c("xls", "xlsx")) {
        req(input$slrSheet)
        # Block on stale sheet name (transient between file upload and selectize update)
        req(input$slrSheet %in% readxl::excel_sheets(path))
      }

      dat <- readUploadedDataFile(ext, path, input$slrSheet)

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
    
    
    # Tells the UI whether to show the sheet picker (only for xls/xlsx)
    output$slrShowSheetPicker <- reactive({
      if (is.null(input$slrUserData)) return(FALSE)
      tolower(tools::file_ext(input$slrUserData$name)) %in% c("xls", "xlsx")
    })
    outputOptions(output, "slrShowSheetPicker", suspendWhenHidden = FALSE)

    # Populate sheet choices when an Excel file is uploaded
    observeEvent(input$slrUserData, {
      req(input$slrUserData)
      ext <- tolower(tools::file_ext(input$slrUserData$name))
      if (ext %in% c("xls", "xlsx")) {
        sheets <- tryCatch(readxl::excel_sheets(input$slrUserData$datapath),
                           error = function(e) character(0))
        freezeReactiveValue(input, "slrSheet")
        updateSelectizeInput(session, "slrSheet",
                             choices  = sheets,
                             selected = if (length(sheets)) sheets[1] else "")
      } else {
        updateSelectizeInput(session, "slrSheet", choices = character(0), selected = "")
      }
    }, priority = 50)

    # Fills the variable selection options based on data file columns.
    # Depends on BOTH slrUserData and slrSheet so that for Excel files we wait
    # until the sheet has been selected before trying to read columns.
    observeEvent({
      input$slrUserData
      input$slrSheet
    }, {
      req(input$slrUserData)
      fileInputs$slrStatus <- "uploaded"

      ext <- tolower(tools::file_ext(input$slrUserData$name))

      # For Excel files, defer until a sheet is selected
      if (ext %in% c("xls", "xlsx") && (is.null(input$slrSheet) || input$slrSheet == "")) {
        hide("slrExplanatory")
        hide("slrResponse")
        hide("uploadedDataPanel")
        return()
      }

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
      calcTrigger(list(n = calcTrigger()$n + 1L, dest = "Model"))
    })

    observeEvent(calcTrigger(), {
      req(calcTrigger()$n > 0L)
      dest <- calcTrigger()$dest

      ## SLR Validation messages ----
      output$perfectFitWarning <- renderUI({ NULL })
      showTab(inputId = "slrNavbarPage", target = "Inference")
      showTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
      showTab(inputId = "slrNavbarPage", target = "Prediction")
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
            conclusion <- if (!is.null(result$p_value)) {
              base <- if (result$p_value <= alpha) {
                paste0("Reject H\u2080 (p = ", result$p_value, " \u2264 0.05)")
              } else {
                paste0("Fail to reject H\u2080 (p = ", result$p_value, " > 0.05)")
              }
              if (!is.null(result$note)) paste0(base, " \u2014 ", result$note) else base
            } else if (!is.null(result$note)) {
              result$note
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
            showNotification("After removing missing values, fewer than four complete observations remain. Please choose different variables.", type = "error", duration = 8)
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

        # Store for Prediction tab
        slrModel(model)
        slrDatX(datx)
        slrDatY(daty)
        if (is.na(isolate(input$slrPredictXTab))) {
          updateNumericInput(session, "slrPredictXTab", value = round(mean(datx), 4))
        }

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
              format(round(x, 4), nsmall = 4, scientific = FALSE)
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
                      if (abs(value - round(value)) < 1e-9) {
                        formatC(round(value), format = "f", digits = 0)
                      } else {
                        formatC(value, format = "f", digits = 4)
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
          p <- RenderScatterplot(
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

          if (isTRUE(input[["slrScatter-showMeans"]])) {
            x_bar <- mean(datx)
            y_bar <- mean(daty)
            p <- p %>%
              add_trace(
                inherit = FALSE,
                x      = x_bar,
                y      = y_bar,
                type   = "scatter",
                mode   = "markers",
                name   = "(x̅, y̅)",
                marker = list(
                  color  = "red",
                  size   = 18,
                  symbol = "asterisk-open",
                  line   = list(color = "red", width = 2)
                ),
                hovertemplate = paste0(
                  "<b>x̅:</b> ", round(x_bar, 4), "<br>",
                  "<b>y̅:</b> ", round(y_bar, 4), "<br>",
                  "<extra></extra>"
                )
              )
          }

          p
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
        slopeEstimate     <- round(summary(model)$coefficients["datx", "Estimate"], 4)
        b0_raw            <- summary(model)$coefficients["(Intercept)", "Estimate"]
        b1_raw            <- summary(model)$coefficients["datx", "Estimate"]

        output$regLineEquation <- renderUI({
          withMathJax(
            p("The estimated equation of the regression line is"),
            p(sprintf("\\( \\qquad \\hat{y} = \\hat{\\beta}_{0} + \\hat{\\beta}_{1} x \\)")),
            p("where"),
            p(sprintf(
              "\\( \\qquad \\hat{\\beta}_{1} = \\dfrac{ \\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } = \\dfrac{ %s - \\dfrac{ (%s)(%s) }{ %s } }{ %s - \\dfrac{ (%s)^2 }{ %s } } = %s \\)",
              format(round(dfTotaled["Totals", "xy"], 4), nsmall = 4, scientific = FALSE),
              format(round(dfTotaled["Totals", "x"], 4), nsmall = 4, scientific = FALSE),
              format(round(dfTotaled["Totals", "y"], 4), nsmall = 4, scientific = FALSE),
              format(round(length(datx), 0), nsmall = 0, scientific = FALSE),
              format(round(dfTotaled["Totals", "x<sup>2</sup>"], 4), nsmall = 4, scientific = FALSE),
              format(round(dfTotaled["Totals", "x"], 4), nsmall = 4, scientific = FALSE),
              format(round(length(datx), 0), nsmall = 0, scientific = FALSE),
              fmt_sci_latex(b1_raw, 4)
            )),
            p("and"),
            p(sprintf(
              "\\( \\qquad \\hat{\\beta}_{0} = \\bar{y} - \\hat{\\beta}_{1} \\bar{x} = %s - (%s)(%s) = %s %s %s = %s \\)",
              format(round(mean(daty), 4), nsmall = 4, scientific = FALSE),
              fmt_sci_latex(b1_raw, 4),
              format(round(mean(datx), 4), nsmall = 4, scientific = FALSE),
              format(round(mean(daty), 4), nsmall = 4, scientific = FALSE),
              b0HatOp,
              fmt_sci_latex(abs(b1_raw) * mean(datx), 4),
              fmt_sci_latex(b0_raw, 4)
            )),
            br(),
            p(sprintf("\\( \\qquad \\hat{y} = %s %s %s x \\)",
                    fmt_sci_latex(b0_raw, 4),
                    yHatOp,
                    fmt_sci_latex(abs(b1_raw), 4))),
            br(),
            p(tags$b("Interpretation:")),
            p(HTML(paste0("Within the scope of observation, \\(", fmt_sci_latex(b0_raw, 4), "\\) is the estimated value of ",
                          "\\(y\\) when \\(x\\) = 0. A slope of \\(", fmt_sci_latex(b1_raw, 4), "\\)",
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
                        
                        format(round(dfTotaled["Totals", "xy"], 4), nsmall = 4, scientific = FALSE),
                        format(round(dfTotaled["Totals", "x"], 4), nsmall = 4, scientific = FALSE),
                        format(round(dfTotaled["Totals", "y"], 4), nsmall = 4, scientific = FALSE),
                        format(length(datx), nsmall = 0, scientific = FALSE),

                        format(round(dfTotaled["Totals", "x<sup>2</sup>"], 4), nsmall = 4, scientific = FALSE),
                        format(round(dfTotaled["Totals", "x"], 4), nsmall = 4, scientific = FALSE),
                        format(length(datx), nsmall = 0, scientific = FALSE),

                        format(round(dfTotaled["Totals", "y<sup>2</sup>"], 4), nsmall = 4, scientific = FALSE),
                        format(round(dfTotaled["Totals", "y"], 4), nsmall = 4, scientific = FALSE),
                        format(length(datx), nsmall = 0, scientific = FALSE),

                        # simplified √ form — use scientific notation when values would round to 0
                        fmt_sci_latex(dfTotaled["Totals", "xy"] - sumXSumY / length(datx), 4),

                        fmt_sci_latex(dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx), 4),

                        fmt_sci_latex(dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx), 4),
                        
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
          S      <- nc - nd
          t_vals <- as.numeric(table(datx))
          u_vals <- as.numeric(table(daty))
          v0     <- n * (n - 1) * (2 * n + 5)
          vt     <- sum(t_vals * (t_vals - 1) * (2 * t_vals + 5))
          vu     <- sum(u_vals * (u_vals - 1) * (2 * u_vals + 5))
          s1     <- sum(choose(t_vals, 3))
          s2     <- sum(choose(u_vals, 3))
          s3     <- sum(choose(t_vals, 2)) * sum(choose(u_vals, 2))
          term3  <- if (n >= 3) 9 * s1 * s2 / (n * (n - 1) * (n - 2)) else 0
          varS   <- (v0 - vt - vu) / 18 + term3 + s3 / (2 * n * (n - 1))
          list(n = n, n0 = n0, n1 = n1, n2 = n2, has_ties = has_ties, nc = nc, nd = nd, S = S, varS = varS)
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
            if (!ks$has_ties)
              p("where \\( n_c \\) is the number of concordant pairs and \\( n_d \\) is the number of discordant pairs."),
            if (ks$has_ties)
              p("where \\( n_c \\) is the number of concordant pairs, \\( n_d \\) is the number of discordant pairs, \\( n_0 \\) is the total number of pairs, \\( n_1 \\) is the number of pairs tied on \\( x \\), and \\( n_2 \\) is the number of pairs tied on \\( y \\)."),
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
            # Use cor.test's p-value: exact permutation for n < 50, normal approx for n >= 50
            p_val  <- kendall$p.value

            withMathJax(
              header,
              p(em("Note: There are no ties in the data. R uses the exact permutation distribution of Kendall's score statistic to calculate the p-value. The Kendall's score statistic is calculated as the difference between the number of concordant pairs and the number of discordant pairs. We present the test statistic and p-value using normal approximation.")),
              br(),
              p(tags$b("Test Statistic:")),
              p("\\( E(\\hat{\\tau}) = 0 \\)"),
              p("\\( SD(\\hat{\\tau}) = \\sqrt{\\dfrac{2(2n+5)}{9n(n-1)}} \\)"),
              p(HTML(sprintf(
                "\\( z = \\dfrac{\\hat{\\tau} - E(\\hat{\\tau})}{SD(\\hat{\\tau})} = \\dfrac{\\hat{\\tau} - 0}{\\sqrt{\\dfrac{2(2n+5)}{9n(n-1)}}} = \\dfrac{%.4f - 0}{\\sqrt{\\dfrac{2(2(%d)+5)}{9(%d)(%d-1)}}} = %.4f \\)",
                tau, ks$n, ks$n, ks$n, z_stat
              ))),
              br(),
              p("where \\( E(\\hat{\\tau}) \\) is the expected value of Kendall's \\( \\hat{\\tau} \\) under \\( H_0 \\), and \\( SD(\\hat{\\tau}) \\) is the standard deviation of the sampling distribution of \\( \\hat{\\tau} \\)."),
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

              plotOutput(session$ns("kendallZCurve"), height = "300px", width = "500px"),

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
              p(sprintf(
                "\\( z = \\dfrac{S}{\\sqrt{\\operatorname{Var}(S)}} = %.4f \\)",
                z_stat
              )),
              p("where \\( S \\) is the Kendall score statistic, \\( n_c \\) is the number of concordant pairs, and \\( n_d \\) is the number of discordant pairs."),
              p(HTML(sprintf(
                "\\( S = n_c - n_d = %d - %d = %d \\)",
                ks$nc, ks$nd, ks$nc - ks$nd
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

              plotOutput(session$ns("kendallZCurve"), height = "300px", width = "500px"),

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
            z_stat <- round(tau / sd_tau, 4)
          } else {
            z_stat <- round(as.numeric(kendall$statistic), 4)
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
            rx_ry  = rank_x * rank_y,
            d      = d,
            d_sq   = d^2
          )
        })

        output$downloadSpearmanXlsx <- downloadHandler(
          filename    = function() {
            has_ties <- spearman_cf(datx) > 0 || spearman_cf(daty) > 0
            if (has_ties) {
              paste0("Spearman_Rank_Correlation_with_ties_", Sys.Date(), ".xlsx")
            } else {
              paste0("Spearman_Rank_Correlation_", Sys.Date(), ".xlsx")
            }
          },
          contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          content     = function(file) {
            tryCatch({
              has_ties <- spearman_cf(datx) > 0 || spearman_cf(daty) > 0
              data     <- spearmanData()
              if (has_ties) {
                data <- data[, c("x", "y", "rank_x", "rank_y", "rx_ry")]
                names(data) <- c("x", "y", "Rank x", "Rank y", "Rank x × Rank y")
              } else {
                data <- data[, c("x", "y", "rank_x", "rank_y", "d", "d_sq")]
                names(data) <- c("x", "y", "Rank x", "Rank y", "d = (Rank x - Rank y)", "d^2")
              }
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
            p(if (has_ties)
              "Since there are ties in the data, we use the following formula:"
            else
              "Since there are no ties in the data, we use the following formula:"
            ),
            div(
              style = "text-align: left; font-size: 18px;",
              HTML(formula_latex)
            ),
            if (has_ties) tagList(
              br(),
              p("where \\( R_{x_i} \\) and \\( R_{y_i} \\) are the ranks of the \\( i \\)-th \\( x \\) and \\( y \\) values, \\( \\bar{R}_x \\) and \\( \\bar{R}_y \\) are the mean ranks of \\( x \\) and \\( y \\), and \\( s_{R_x} \\) and \\( s_{R_y} \\) are the standard deviations of the ranks of \\( x \\) and \\( y \\).")
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
          has_ties    <- spearman_cf(datx) > 0 || spearman_cf(daty) > 0

          if (has_ties) {
            sum_rank_x <- sum(spearman_df$rank_x)
            sum_rank_y <- sum(spearman_df$rank_y)
            sum_rxry   <- sum(spearman_df$rx_ry)

            reactable(
              spearman_df[, c("x", "y", "rank_x", "rank_y", "rx_ry")],
              sortable   = FALSE,
              bordered   = TRUE,
              striped    = TRUE,
              highlight  = TRUE,
              pagination = FALSE,
              fullWidth  = FALSE,
              rownames   = FALSE,
              columns = list(
                x      = colDef(name = "x",      align = "center", footer = tags$b("Total")),
                y      = colDef(name = "y",      align = "center"),
                rank_x = colDef(name = "Rank x", align = "center", footer = tags$b(sum_rank_x)),
                rank_y = colDef(name = "Rank y", align = "center", footer = tags$b(sum_rank_y)),
                rx_ry  = colDef(
                  name     = HTML("(Rank x) &times; (Rank y)"),
                  html     = TRUE,
                  align    = "center",
                  minWidth = 200,
                  footer   = tags$b(sum_rxry),
                  cell = function(value) {
                    if (value == floor(value)) {
                      formatC(value, format = "f", digits = 0)
                    } else {
                      formatC(value, format = "f", digits = 2)
                    }
                  }
                )
              )
            )
          } else {
            sum_d_sq <- sum(spearman_df$d_sq)

            reactable(
              spearman_df[, c("x", "y", "rank_x", "rank_y", "d", "d_sq")],
              sortable   = FALSE,
              bordered   = TRUE,
              striped    = TRUE,
              highlight  = TRUE,
              pagination = FALSE,
              fullWidth  = FALSE,
              rownames   = FALSE,
              columns = list(
                x      = colDef(name = "x",      align = "center", footer = tags$b("Total")),
                y      = colDef(name = "y",      align = "center"),
                rank_x = colDef(name = "Rank x", align = "center"),
                rank_y = colDef(name = "Rank y", align = "center"),
                d      = colDef(name = "d = (Rank x \u2212 Rank y)", align = "center", minWidth = 190),
                d_sq   = colDef(
                  name   = HTML("d<sup>2</sup>"),
                  html   = TRUE,
                  align  = "center",
                  footer = tags$b(
                    if (sum_d_sq == floor(sum_d_sq)) {
                      formatC(sum_d_sq, format = "d", big.mark = ",")
                    } else {
                      formatC(sum_d_sq, format = "f", digits = 2)
                    }
                  ),
                  cell   = function(value) {
                    if (value == floor(value)) {
                      formatC(value, format = "d", big.mark = ",")
                    } else {
                      formatC(value, format = "f", digits = 2)
                    }
                  }
                )
              )
            )
          }
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
        
        output$anovaTable <- renderDT({
          anova_results <- anova(model)

          p_val <- anova_results$`Pr(>F)`[1]
          p_val_display <- if (p_val < 0.0001 && p_val > 0) "P < 0.0001" else as.character(round(p_val, 4))

          data <- data.frame(
            df         = c(anova_results$Df[1], anova_results$Df[2], sum(anova_results$Df)),
            SS         = c(anova_results$`Sum Sq`[1], anova_results$`Sum Sq`[2], sum(anova_results$`Sum Sq`)),
            MS         = c(anova_results$`Mean Sq`[1], anova_results$`Mean Sq`[2], NA),
            F          = c(anova_results$`F value`[1], NA, NA),
            `P-Value`  = c(p_val_display, NA_character_, NA_character_),
            check.names = FALSE
          )
          rownames(data) <- c("Regression (Model)", "Error (Residual)", "Total")

          colNames <- c("df", "Sum of Squares (SS)", "Mean Sum of Squares (MS)", "F-ratio", "P-Value")

          headers <- htmltools::withTags(table(
            class = 'display',
            thead(
              tr(
                th("Sources of Variation",
                   style = "border: 1px solid rgba(0, 0, 0, 0.15);
                              border-bottom: 1px solid rgba(0, 0, 0, 0.3);"),
                lapply(colNames, th,
                       style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                                  border-top: 1px solid rgba(0, 0, 0, 0.15);')
              )
            )
          ))

          datatable(
            data,
            class = 'cell-border stripe',
            container = headers,
            options = list(
              dom = 't',
              pageLength = -1,
              ordering = FALSE,
              searching = FALSE,
              paging = FALSE,
              autoWidth = FALSE,
              scrollX = TRUE,
              columnDefs = list(
                list(className = 'dt-center', targets = 0:5),
                list(width = '150px', targets = 2:5)
              )
            ),
            selection = "none",
            escape = FALSE,
            filter = "none"
          ) %>%
            formatRound(columns = 1, digits = 0) %>%
            formatRound(columns = 2:4, digits = 4) %>%
            formatStyle(columns = c(0, 4), fontWeight = 'bold') %>%
            formatStyle(
              columns = 1:5,
              target = 'row',
              fontWeight = styleRow(3, "bold")
            )
        })
        
        output$anovaConclusion <- renderUI({
          anova_results <- anova(model)
          p_value <- anova_results$`Pr(>F)`[1]
          f_value <- anova_results$`F value`[1]
          msr <- anova_results$`Mean Sq`[1]
          mse <- anova_results$`Mean Sq`[2]
          
          withMathJax(
            p(strong("Test Statistic:")),
            p(sprintf("\\( \\displaystyle F = \\frac{\\mathrm{MSR}}{\\mathrm{MSE}} = \\frac{%s}{%s} = %.4f \\)", fmt_sci_latex(msr, 4), fmt_sci_latex(mse, 4), f_value)),
            p(strong("Conclusion:")),
            if (p_value <= 0.05) {
              p(sprintf("Since the p-value is less than \\( \\alpha \\) (%.4f < 0.05), we reject the null hypothesis and conclude there is enough statistical evidence to support the alternative hypothesis. We can conclude the model is statistically significant.", p_value))
            } else {
              p(sprintf("Since the p-value is greater than \\( \\alpha \\) (%.4f >  0.05), we fail to reject the null hypothesis and conclude there isn't enough statistical evidence to support the alternative hypothesis. We can conclude the model is not statistically significant.", p_value))
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
        
        # ── ggplot F-distribution (active) ──────────────────────────────────────
        output$anovaFCurve <- renderPlot({
          anova_results <- anova(model)

          df1    <- 1
          df2    <- n - 2
          f_stat <- round(anova_results$`F value`[1], 4)
          f_crit <- round(qf(0.95, df1, df2), 4)

          x_start <- 0.05
          x_max   <- if (f_stat > x_start && f_stat < f_crit * 10)
                       max(f_crit * 2.5, f_stat * 1.3)
                     else
                       f_crit * 2.5

          x_curve <- seq(x_start, x_max, length.out = 600)
          y_curve <- stats::df(x_curve, df1, df2)
          y_cap   <- stats::df(f_crit * 0.5, df1, df2) * 1.1
          y_disp  <- pmin(y_curve, y_cap)

          plot_df    <- data.frame(x = x_curve, y = y_disp) %>% filter(is.finite(y))
          seg_h      <- y_cap * 0.75
          f_in_range <- f_stat > x_start && f_stat <= x_max

          ggplot(plot_df, aes(x = x, y = y)) +
            geom_ribbon(data = filter(plot_df, x >= f_crit),
                        aes(ymin = 0, ymax = y),
                        fill = "steelblue", alpha = 0.35) +
            geom_line(linewidth = 0.8) +
            annotate("text",
                     x = f_crit, y = y_cap * 1.08,
                     label = "← AR   ", hjust = 1,
                     size = 14 / .pt, fontface = "bold") +
            annotate("text",
                     x = f_crit, y = y_cap * 1.08,
                     label = "   RR →", hjust = 0,
                     size = 14 / .pt, fontface = "bold") +
            annotate("segment",
                     x = f_crit, xend = f_crit, y = 0, yend = seg_h,
                     linewidth = 1.5, color = "#023B70") +
            {if (f_in_range)
              annotate("segment",
                       x = f_stat, xend = f_stat, y = 0, yend = seg_h,
                       linewidth = 1.25, color = "#BD130B")
            } +
            annotate("text",
                     x = f_crit, y = -y_cap * 0.07,
                     label = as.character(f_crit),
                     color = "#023B70", fontface = "bold",
                     size = 14 / .pt) +
            {if (f_in_range)
              annotate("text",
                       x = f_stat, y = -y_cap * 0.07,
                       label = as.character(f_stat),
                       color = "#BD130B", fontface = "bold",
                       size = 14 / .pt)
            } +
            coord_cartesian(xlim = c(0, x_max * 1.02),
                            ylim = c(0, y_cap * 1.18),
                            clip = "off") +
            scale_x_continuous(expand = c(0, 0)) +
            scale_y_continuous(breaks = 0, labels = "0", expand = c(0, 0)) +
            ylab(expression(bold(italic(Density)))) +
            xlab(expression(bold(italic(F)))) +
            theme_classic() +
            theme(
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y  = element_text(size = 13, face = "bold"),
              axis.title.x = element_text(size = 16, face = "bold.italic",
                                          margin = margin(t = 22)),
              axis.title.y = element_text(size = 16, face = "bold.italic"),
              axis.line    = element_line(linewidth = 0.8, color = "black"),
              plot.margin  = margin(t = 20, r = 10, b = 45, l = 5)
            )
        }, height = 400, width = 650)

        # ── plotly F-distribution (commented out — kept for reference) ───────────
        # output$anovaFCurve <- renderPlotly({
        #   anova_results <- anova(model)
        #   df1    <- 1
        #   df2    <- n - 2
        #   f_stat <- round(anova_results$`F value`[1], 4)
        #   f_crit <- round(qf(0.95, df1, df2), 4)
        #   x_start <- 0.05
        #   x_max <- if (f_stat > x_start && f_stat < f_crit * 10)
        #              max(f_crit * 2.5, f_stat * 1.3)
        #            else
        #              f_crit * 2.5
        #   x_curve <- seq(x_start, x_max, length.out = 600)
        #   y_curve <- stats::df(x_curve, df1, df2)
        #   y_cap     <- stats::df(f_crit * 0.5, df1, df2) * 1.1
        #   y_display <- pmin(y_curve, y_cap)
        #   x_fill <- seq(f_crit, x_max, length.out = 300)
        #   y_fill <- stats::df(x_fill, df1, df2)
        #   seg_h      <- y_cap * 0.75
        #   f_in_range <- f_stat > x_start && f_stat <= x_max
        #   y_axis_max <- y_cap * 1.18
        #   annotations <- list(
        #     list(x = f_crit, xref = "x", y = 0.82, yref = "paper",
        #          text = "<b>← AR&nbsp;&nbsp;&nbsp;</b>", showarrow = FALSE,
        #          font = list(size = 14), xanchor = "right"),
        #     list(x = f_crit, xref = "x", y = 0.82, yref = "paper",
        #          text = "<b>&nbsp;&nbsp;&nbsp;RR →</b>", showarrow = FALSE,
        #          font = list(size = 14), xanchor = "left"),
        #     list(x = f_crit, xref = "x", y = -0.09, yref = "paper",
        #          text = paste0("<b>", f_crit, "</b>"), showarrow = FALSE,
        #          font = list(size = 12, color = "#023B70"),
        #          xanchor = "center", yanchor = "top")
        #   )
        #   if (f_in_range) {
        #     annotations <- c(annotations, list(
        #       list(x = f_stat, xref = "x", y = -0.09, yref = "paper",
        #            text = paste0("<b>", f_stat, "</b>"), showarrow = FALSE,
        #            font = list(size = 12, color = "#BD130B"),
        #            xanchor = "center", yanchor = "top")
        #     ))
        #   }
        #   fig <- plot_ly() %>%
        #     add_trace(x = x_fill, y = y_fill,
        #               type = "scatter", mode = "none",
        #               fill = "tozeroy", fillcolor = "rgba(70,130,180,0.35)",
        #               showlegend = FALSE, hoverinfo = "none") %>%
        #     add_trace(x = x_curve, y = y_display,
        #               type = "scatter", mode = "lines",
        #               line = list(color = "black", width = 1.5),
        #               showlegend = FALSE, hoverinfo = "none") %>%
        #     add_segments(x = f_crit, xend = f_crit, y = 0, yend = seg_h,
        #                  line = list(color = "#023B70", width = 2),
        #                  showlegend = FALSE, hoverinfo = "none") %>%
        #     layout(
        #       xaxis = list(title = list(text = "<b><i>F</i></b>", font = list(size = 16)),
        #                    showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE,
        #                    showline = TRUE, linecolor = "black", linewidth = 1.5,
        #                    range = c(0, x_max * 1.02)),
        #       yaxis = list(title = list(text = "<b><i>Density</i></b>", font = list(size = 16)),
        #                    showgrid = FALSE, zeroline = FALSE,
        #                    showline = TRUE, linecolor = "black", linewidth = 1.5,
        #                    range = c(0, y_axis_max)),
        #       annotations = annotations,
        #       margin = list(t = 40, r = 20, b = 55, l = 70),
        #       plot_bgcolor = "white", paper_bgcolor = "white"
        #     )
        #   if (f_in_range) {
        #     fig <- fig %>%
        #       add_segments(x = f_stat, xend = f_stat, y = 0, yend = seg_h,
        #                    line = list(color = "#BD130B", width = 1.5),
        #                    showlegend = FALSE, hoverinfo = "none")
        #   }
        #   fig
        # })

        output$pearsonTCurve <- renderPlot({
          hypTTestPlot(
            testStatistic = round(pearson$statistic, 4),
            degfree       = pearson$parameter,
            critValue     = round(qt(0.975, df = pearson$parameter), 3),
            altHypothesis = "two.sided"
          )
        }, height = 300, width = 500)
        
        
        
        
        
        showTab(inputId = "slrNavbarPage", target = "Prediction")
        updateNavbarPage(session, "slrNavbarPage", selected = dest)

      } #if regcor_iv is valid

      show(id = "regCorrMP")
    }) # calcTrigger


    # =========================================================================== #
    # ---- Prediction Tab Output ------------------------------------------------
    # =========================================================================== #
    output$slrPrediction <- renderUI({
      req(slrModel(), slrDatX())

      x0 <- input$slrPredictXTab
      req(!is.na(x0), is.numeric(x0))

      model  <- slrModel()
      datx   <- slrDatX()
      b0     <- coef(model)[1]
      b1     <- coef(model)[2]
      y_hat  <- b0 + b1 * x0

      n      <- length(datx)
      x_bar  <- mean(datx)
      ssx    <- sum((datx - x_bar)^2)
      mse    <- sum(residuals(model)^2) / (n - 2)
      t_crit <- qt(0.975, df = n - 2)

      se_mean  <- sqrt(mse * (1/n + (x0 - x_bar)^2 / ssx))
      se_pred  <- sqrt(mse * (1 + 1/n + (x0 - x_bar)^2 / ssx))
      ci_lower <- y_hat - t_crit * se_mean
      ci_upper <- y_hat + t_crit * se_mean
      pi_lower <- y_hat - t_crit * se_pred
      pi_upper <- y_hat + t_crit * se_pred

      is_extrap <- x0 < min(datx) || x0 > max(datx)

      b1_sign   <- if (b1 >= 0) "+" else "-"
      x0_tex    <- fmt_sci_latex(x0, 4)
      b0_tex    <- fmt_sci_latex(b0, 4)
      b1_tex    <- fmt_sci_latex(abs(b1), 4)
      yh_tex    <- fmt_sci_latex(y_hat, 4)
      mse_tex   <- fmt_sci_latex(mse, 4)
      xbar_tex  <- fmt_sci_latex(x_bar, 4)
      ssx_tex   <- fmt_sci_latex(ssx, 4)
      tcrit_tex <- fmt_sci_latex(t_crit, 4)

      tagList(
        if (is_extrap) {
          div(
            class = "alert alert-warning",
            style = "max-width: 600px; margin-bottom: 15px;",
            tags$b("⚠️ Extrapolation Warning: "),
            sprintf(
              "x₀ = %s is outside the observed range [%s, %s]. This prediction extends beyond the data and may be unreliable.",
              x0_tex,
              fmt_sci_latex(min(datx), 4),
              fmt_sci_latex(max(datx), 4)
            )
          )
        },
        withMathJax(

          # ---- Point Prediction ----
          p(strong("Point prediction")),
          p("Given"),
          p(sprintf("\\( \\qquad x_0 = %s \\)", x0_tex)),
          br(),
          p("The estimated response is"),
          p(sprintf("\\( \\qquad \\hat{y}_0 = \\hat{\\beta}_0 + \\hat{\\beta}_1 x_0 \\)")),
          br(),
          p("Substitute"),
          p(sprintf("\\( \\qquad = %s %s %s(%s) \\)",
                    b0_tex, b1_sign, b1_tex, x0_tex)),
          p(sprintf("\\( \\qquad = %s \\)", yh_tex)),
          hr(style = "max-width: 600px;"),
          p(strong("Estimated response")),
          p(sprintf("\\( \\qquad \\hat{y}_0 = %s \\)", yh_tex)),
          p(tags$b("Interpretation:")),
          p(HTML(sprintf(
            "When \\( x = %s \\), the estimated mean response is \\( %s \\).",
            x0_tex, yh_tex
          ))),

          hr(style = "max-width: 600px;"),

          # ---- 95% CI for Mean Response ----
          p(strong("95% Confidence Interval for the Mean Response")),
          p("Estimates the true mean value of \\( y \\) across all observations at \\( x_0 \\)."),
          br(),
          p(sprintf(
            "\\( \\qquad \\hat{y}_0 \\pm t_{\\alpha/2,\\, n-2} \\sqrt{MSE \\left( \\dfrac{1}{n} + \\dfrac{(x_0 - \\bar{x})^2}{SS_x} \\right)} \\)"
          )),
          br(),
          p("Substitute"),
          p(sprintf(
            "\\( \\qquad = %s \\pm %s \\sqrt{%s \\left( \\dfrac{1}{%d} + \\dfrac{(%s - %s)^2}{%s} \\right)} \\)",
            yh_tex, tcrit_tex, mse_tex, n, x0_tex, xbar_tex, ssx_tex
          )),
          p(sprintf(
            "\\( \\qquad = %s \\pm %s \\)",
            yh_tex, fmt_sci_latex(t_crit * se_mean, 4)
          )),
          p(sprintf("\\( \\qquad (%s, \\; %s) \\)",
                    fmt_sci_latex(ci_lower, 4), fmt_sci_latex(ci_upper, 4))),
          p(tags$b("Interpretation:")),
          p(HTML(sprintf(
            "We are 95%% confident that the true mean response when \\( x = %s \\) is between \\( %s \\) and \\( %s \\).",
            x0_tex, fmt_sci_latex(ci_lower, 4), fmt_sci_latex(ci_upper, 4)
          ))),

          hr(style = "max-width: 600px;"),

          # ---- 95% Prediction Interval ----
          p(strong("95% Prediction Interval for an Individual Response")),
          p("Estimates the range for a single new observation at \\( x_0 \\)."),
          br(),
          p(sprintf(
            "\\( \\qquad \\hat{y}_0 \\pm t_{\\alpha/2,\\, n-2} \\sqrt{MSE \\left( 1 + \\dfrac{1}{n} + \\dfrac{(x_0 - \\bar{x})^2}{SS_x} \\right)} \\)"
          )),
          br(),
          p("Substitute"),
          p(sprintf(
            "\\( \\qquad = %s \\pm %s \\sqrt{%s \\left( 1 + \\dfrac{1}{%d} + \\dfrac{(%s - %s)^2}{%s} \\right)} \\)",
            yh_tex, tcrit_tex, mse_tex, n, x0_tex, xbar_tex, ssx_tex
          )),
          p(sprintf(
            "\\( \\qquad = %s \\pm %s \\)",
            yh_tex, fmt_sci_latex(t_crit * se_pred, 4)
          )),
          p(sprintf("\\( \\qquad (%s, \\; %s) \\)",
                    fmt_sci_latex(pi_lower, 4), fmt_sci_latex(pi_upper, 4))),
          p(tags$b("Interpretation:")),
          p(HTML(sprintf(
            "We are 95%% confident that a single new observation when \\( x = %s \\) will fall between \\( %s \\) and \\( %s \\).",
            x0_tex, fmt_sci_latex(pi_lower, 4), fmt_sci_latex(pi_upper, 4)
          ))),
          br()
        )
      )
    })



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

    observeEvent(list(input$x, input$y), {
      hide(id = "regCorrMP")
    }, ignoreInit = TRUE)

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
      slrModel(NULL)
      slrDatX(NULL)
      slrDatY(NULL)
      output$perfectFitWarning <- renderUI({ NULL })
      hide(id = "regCorrMP")
      hide("uploadedDataPanel")
      hideTab(inputId = "slrNavbarPage", target = "Uploaded Data")
      shinyjs::reset("inputPanel")
      fileInputs$slrStatus <- 'reset'
      showTab(inputId = "slrNavbarPage", target = "Inference")
      showTab(inputId = "slrNavbarPage", target = "Diagnostic Plots")
      showTab(inputId = "slrNavbarPage", target = "Prediction")
      if (!is.null(input$slrNavbarPage)) {
        updateNavbarPage(session, "slrNavbarPage", selected = "Model")
      }
    })
  })
}