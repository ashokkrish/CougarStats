library(aplpack)
library(base)
library(bslib)
library(car)
library(colourpicker)
library(DescTools)
library(dplyr)
library(DT)
library(generics)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(e1071)
library(nortest)
library(plotly)
library(readr)
library(readxl)
library(rstatix)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyMatrix)
library(shinyvalidate)
library(shinyWidgets)
library(tinytex)
library(tools)
library(writexl)
library(xtable)
library(MASS)

source("R/KruskalWallisUI.R")
source("R/RankedDataTable.R")
# render <- "
# {
#   option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
#   item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
# }"

# =========================================================================== #
# ---- UI Components --------------------------------------------------------
# =========================================================================== #

lessThanInequalGreaterThanChoices123 <-
  c("<" = 1, "≠" = 2, ">" = 3)

statInfrUI <- function(id) {
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
          
          HTML("<label class='si-label'><b>Methodology</b></label>"),
          
          radioButtons(
            inputId      = ns("siMethod"),
            label        = NULL,
            choiceValues = list("1",
                                "2",
                                "Multiple",
                                "Categorical"),
            choiceNames  = list("Inference about 1 sample\\(\\)",
                                "Inference about 2 samples\\(\\)",
                                "Inference about more than 2 samples (e.g. ANOVA or Kruskal-Wallis)\\(\\)",
                                "Inference for Categorical Data (e.g \\( \\chi^2 \\) test)"),
            selected     = "1"),
          
          # radioButtons(inputId = ns("popuDistribution"),
          #              label = strong("Analysis Type"),
          #              choiceValues = list("Parametric analysis", "Non-parametric analysis"),
          #              choiceNames = list("Parametric analysis", "Non-parametric analysis"),
          #              selected = "Parametric analysis", #character(0),
          #              inline = TRUE), #,width = '1000px'),
          #
          # conditionalPanel(
          #   ns = ns,
          #   condition = "input.popuDistribution == 'Non-parametric analysis'",
          #
          # ),
          #
          # conditionalPanel(
          #   ns = ns,
          #   condition = "input.popuDistribution == 'Parametric analysis'",
          #
          # ),
          
          ### ------------ 1 Sample ----------------------------------------------------
          conditionalPanel(
            ns = ns,
            condition = "input.siMethod == '1'",
            
            radioButtons(
              inputId      = ns("popuParameter"),
              label        = strong("Parameter of Interest"),
              choiceValues = list("Population Mean",
                                  "Population Standard Deviation",
                                  "Population Proportion"),
              choiceNames  = list("Population Mean (\\( \\mu \\)) ",
                                  "Population Standard Deviation (\\( \\sigma\\)) ",
                                  "Population Proportion (\\( p\\))"),
              selected     = "Population Mean",
              inline       = FALSE),
            
            #### ---------------- Mean ---------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameter == 'Population Mean'",
              
              radioButtons(
                inputId      = ns("dataAvailability"),
                label        = strong("Data Availability"),
                choiceValues = list("Summarized Data",
                                    "Enter Raw Data",
                                    "Upload Data"),
                choiceNames  = list("Summarized Data",
                                    "Enter Raw Data",
                                    "Upload Data"),
                selected     = "Summarized Data",
                inline       = TRUE),
              
              ##### -------------------- Summarized Data -----------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability == 'Summarized Data'",
                
                numericInput(
                  inputId = ns("sampleSize"),
                  label   = strong("Sample Size (\\( n\\))"),
                  value   = 18,
                  min     = 1,
                  step    = 1),
                
                numericInput(
                  inputId = ns("sampleMean"),
                  label   = strong("Sample Mean (\\( \\bar{x}\\))"),
                  value   = 103.5375,
                  step    = 0.00001),
                
                radioButtons(
                  inputId      = ns("sigmaKnown"),
                  label        = strong("Is Population Standard Deviation (\\( \\sigma\\)) known?"),
                  choiceValues = list("Known",
                                      "Unknown"),
                  choiceNames  = list("Known",
                                      "Unknown"),
                  selected     = "Known",
                  inline       = TRUE),
                
                ###### ------------------------ Sigma Known ----------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.sigmaKnown == 'Known'",
                  
                  numericInput(
                    inputId = ns("popuSD"),
                    label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                    value   = 8.25,
                    min     = 0.00001,
                    step    = 0.00001)
                ), #Sigma Known
                
                ###### ------------------------ Sigma Unknown --------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.sigmaKnown == 'Unknown'",
                  
                  numericInput(
                    inputId = ns("sampSD"),
                    label   = strong("Sample Standard Deviation (\\( s\\)) Value"),
                    value   = 4.78,
                    min     = 0.00001,
                    step    = 0.00001)
                ), # Sigma Unknown
              ), # One Sample Summarized Data
              
              ##### -------------------- Raw Data ------------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability == 'Enter Raw Data'",
                
                textAreaInput(
                  inputId     = ns("sample1"),
                  label       = strong("Sample"),
                  value       = "202, 210, 215, 220, 220, 224, 225, 228, 228, 228",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3),
                
                radioButtons(
                  inputId      = ns("sigmaKnownRaw"),
                  label        = strong("Population Standard Deviation (\\( \\sigma\\))"),
                  choiceValues = list("rawKnown",
                                      "rawUnknown"),
                  choiceNames  = list("Known",
                                      "Unknown"),
                  selected     = "rawUnknown",
                  inline       = TRUE),
                
                ###### ------------------------ Sigma Known ----------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.sigmaKnownRaw == 'rawKnown'",
                  
                  numericInput(
                    inputId = ns("popuSDRaw"),
                    label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                    value   = 8.25,
                    min     = 0.00001,
                    step    = 0.00001)
                ), # Sigma Known
                
                ###### ------------------------ Sigma Unknown --------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.sigmaKnownRaw == 'rawUnknown'"
                ) # Sigma Unknown
              ), # One Sample Raw Data
              
              ##### -------------------- Uploaded Data -------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability == 'Upload Data'",
                
                HTML(uploadDataDisclaimer),
                
                fileInput(
                  inputId = ns("oneMeanUserData"),
                  label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"),
                  accept  = c("text/csv",
                              "text/comma-separated-values",
                              "text/plain",
                              ".csv",
                              ".xls",
                              ".xlsx")),
                
                selectizeInput(
                  inputId = ns("oneMeanVariable"),
                  label   = strong("Choose a Column for Analysis"),
                  choices = c(""),
                  options = list(placeholder = 'Select a column',
                                 onInitialize = I('function() { this.setValue(""); }'))),
                
                radioButtons(
                  inputId      = ns("sigmaKnownUpload"),
                  label        = strong("Population Standard Deviation (\\( \\sigma\\))"),
                  choiceValues = list("Known",
                                      "Unknown"),
                  choiceNames  = list("Known",
                                      "Unknown"),
                  selected     = "Unknown",
                  inline       = TRUE),
                
                ###### ------------------------ Sigma Known ----------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.sigmaKnownUpload == 'Known'",
                  
                  numericInput(
                    inputId = ns("popuSDUpload"),
                    label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                    value   = 5,
                    min     = 0.00001,
                    step    = 0.00001)
                ), # Sigma Known
              ), # One Sample upload data
            ), # One Population Mean
            
            #### ---------------- Proportion ---------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameter == 'Population Proportion'",
              
              numericInput(
                inputId = ns("numSuccesses"),
                label   = strong("Number of Successes (\\( x\\))"),
                value   = 1087,
                min     = 0,
                step    = 1),
              
              numericInput(
                inputId = ns("numTrials"),
                label   = strong("Number of Trials (\\( n\\))"),
                value   = 1430,
                min     = 1,
                step    = 1),
            ), #One Population Proportion
            
            #### ---------------- Standard Deviation -------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameter == 'Population Standard Deviation'",
              
              numericInput(
                inputId = ns("SSDSampleSize"),
                label   = strong("Sample Size (\\( n\\))"),
                value   = 30,
                min     = 2,
                step    = 1
              ),
              
              numericInput(
                inputId = ns("SSDStdDev"),
                label   = strong("Sample Standard Deviation (\\( s\\))"),
                value   = 12.23,
                min     = 0.00001,
                step    = 0.00001),
            ), #One Population Standard Deviation
            
            radioButtons(
              inputId      = ns("inferenceType"),
              label        = strong("Inference Type"),
              choiceValues = list("Confidence Interval",
                                  "Hypothesis Testing"),
              choiceNames  = list("Confidence Interval",
                                  "Hypothesis Testing"),
              selected     = "Confidence Interval",
              inline       = TRUE),
            
            #### ---------------- Confidence Interval ------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.inferenceType == 'Confidence Interval'",
              
              radioButtons(
                inputId  = ns("confidenceLevel"),
                label    = strong("Confidence Level (\\( 1- \\alpha\\))"),
                choices  = c("90%",
                             "95%",
                             "99%"),
                selected = c("95%"),
                inline   = TRUE)
            ), # Confidence Interval
            
            #### ---------------- Hypothesis Testing -------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.inferenceType == 'Hypothesis Testing'",
              
              radioButtons(
                inputId  = ns("significanceLevel"),
                label    = strong("Significance Level (\\( \\alpha\\))"),
                choices  = c("10%",
                             "5%",
                             "1%"),
                selected = c("5%"),
                inline   = TRUE),
              
              conditionalPanel(
                ns = ns,
                condition = "input.popuParameter == 'Population Mean'",
                
                numericInput(
                  inputId = ns("hypMean"),
                  label   = strong("Hypothesized Population Mean (\\( \\mu_{0}\\)) Value"),
                  value   = 99,
                  step    = 0.00001),
              ), # Population Mean
              
              conditionalPanel(
                ns = ns,
                condition = "input.popuParameter == 'Population Proportion'",
                
                numericInput(
                  inputId = ns("hypProportion"),
                  label   = strong("Hypothesized Population Proportion (\\( p_{0}\\)) Value"),
                  value   = 0.73,
                  min     = 0,
                  max     = 1,
                  step    = 0.00001),
              ), # Population Proportion
              
              conditionalPanel(
                ns = ns,
                condition = "input.popuParameter == 'Population Standard Deviation'",
                
                numericInput(
                  inputId = ns("hypStdDeviation"),
                  label   = strong(r"--{Hypothesized Population Standard Deviation (\( \sigma_{0}\)) Value}--"),
                  value   = 16.00,
                  min     = 0.001,
                  step    = 0.001)
              ), # Population standard deviation
              
              selectInput(
                inputId  = ns("altHypothesis"),
                label    = strong("Alternate Hypothesis (\\( H_{a}\\))"),
                choices  = lessThanInequalGreaterThanChoices123,
                selected = 2
                # options  = list(render = I(render))
              ),
            ), # Hypothesis Testing
            
            #### ---------------- Graph Options ------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameter == 'Population Mean' && input.dataAvailability != 'Summarized Data'",
              
              p(strong("Graph Options")),
              
              checkboxInput(
                inputId = ns("oneMeanBoxplot"),
                label   = "Boxplot for Sample Data",
                value   = TRUE),
            ) # Pop Mean ! Summarized
          ), #"input.siMethod == '1'"
          
          ### ------------ 2 Samples ---------------------------------------------------
          conditionalPanel(
            ns = ns,
            condition = "input.siMethod == '2'",
            
            HTML("<label class='si-label'><b>Parameter of Interest</b></label>"),
            
            radioButtons(
              inputId      = ns("popuParameters"),
              label        = NULL,
              choiceValues = list("Independent Population Means",
                                  "Wilcoxon rank sum test",
                                  "Dependent Population Means",
                                  "Population Proportions",
                                  "Two Population Variances"),
              choiceNames  = list("Two Independent Populations (\\( \\mu_{1} - \\mu_{2} \\))",
                                  "Wilcoxon rank sum test (or the Mann-Whitney U test)",
                                  "Dependent (Paired) Populations (\\( \\mu_{d} \\))",
                                  "Two Population Proportions (\\( p_{1} - p_{2}\\))",
                                  "Two Population Variances (\\( \\sigma_{1}^2/\\sigma_{2}^2 \\))"),
              selected     = "Independent Population Means", #character(0), #
              inline       = FALSE), #,width = '1000px'),
            
            #### ---------------- Ind Pop Means ------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Independent Population Means'",
              
              radioButtons(
                inputId      = ns("dataAvailability2"),
                label        = strong("Data Availability"),
                choiceValues = list("Summarized Data",
                                    "Enter Raw Data",
                                    "Upload Data"),
                choiceNames  = list("Summarized Data",
                                    "Enter Raw Data",
                                    "Upload Data"),
                selected     = "Summarized Data", #character(0), #
                inline       = TRUE), #,width = '1000px'),
              
              ##### -------------------- Summarized Data -----------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability2 == 'Summarized Data'",
                
                numericInput(
                  inputId = ns("sampleSize1"),
                  label   = strong("Sample Size 1 (\\( n_{1}\\))"),
                  value   = 21,
                  min     = 2,
                  step    = 1),
                
                numericInput(
                  inputId = ns("sampleMean1"),
                  label   = strong("Sample Mean 1 (\\( \\bar{x}_{1}\\))"),
                  value   = 29.6,
                  step    = 0.00001),
                
                numericInput(
                  inputId = ns("sampleSize2"),
                  label   = strong("Sample Size 2 (\\( n_{2}\\))"),
                  value   = 21,
                  min     = 2,
                  step    = 1),
                
                numericInput(
                  inputId = ns("sampleMean2"),
                  label   = strong("Sample Mean 2 (\\( \\bar{x}_{2}\\))"),
                  value   = 33.9,
                  step    = 0.00001),
                
                radioButtons(
                  inputId      = ns("bothsigmaKnown"),
                  label        = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                  choiceValues = list("bothKnown",
                                      "bothUnknown"),
                  choiceNames  = list("Both Known",
                                      "Both Unknown"),
                  selected     = "bothKnown",
                  inline       = TRUE),
                
                ###### ------------------------ Sigma Both Known -----------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.bothsigmaKnown == 'bothKnown'",
                  
                  numericInput(
                    inputId = ns("popuSD1"),
                    label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                    value   = 5.36,
                    min     = 0.00001,
                    step    = 0.00001),
                  
                  numericInput(
                    inputId = ns("popuSD2"),
                    label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                    value   = 5.97,
                    min     = 0.00001,
                    step    = 0.00001),
                ), # Sigma Both Known
                
                ###### -------------------- Sigma Both Unknown -------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.bothsigmaKnown == 'bothUnknown'",
                  
                  radioButtons(
                    inputId      = ns("bothsigmaEqual"),
                    label        = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                    choiceValues = list("TRUE",
                                        "FALSE"),
                    choiceNames  = list("Yes (Pooled)",
                                        "No (Welch-Satterthwaite df)"),
                    selected     = "TRUE",
                    inline       = TRUE), #,width = '1000px'),
                  
                  numericInput(
                    inputId = ns("sampSD1"),
                    label   = strong("Sample Standard Deviation 1 (\\( s_{1}\\)) Value"),
                    value   = 5.24,
                    min     = 0.00001,
                    step    = 0.00001),
                  
                  numericInput(
                    inputId = ns("sampSD2"),
                    label   = strong("Sample Standard Deviation 2 (\\( s_{2}\\)) Value"),
                    value   = 5.85,
                    min     = 0.00001,
                    step    = 0.00001),
                ), # Sigma Both Unknown
              ), # Summarized Data
              
              ##### -------------------- Raw Data ------------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability2 == 'Enter Raw Data'",
                
                textAreaInput(
                  inputId     = ns("raw_sample1"),
                  label       = strong("Sample 1"),
                  value       = "101.1,  111.1,  107.6,  98.1,  99.5,  98.7,  103.3,  108.9,  109.1,  103.3",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3),
                
                textAreaInput(
                  inputId     = ns("raw_sample2"),
                  label       = strong("Sample 2"),
                  value       = "107.1,  105.0,  98.0,  97.9,  103.3,  104.6,  100.1,  98.2,  97.9",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3),
                
                radioButtons(
                  inputId      = ns("bothsigmaKnownRaw"),
                  label        = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                  choiceValues = list("bothKnown",
                                      "bothUnknown"),
                  choiceNames  = list("Both Known",
                                      "Both Unknown"),
                  selected     = "bothUnknown",
                  inline       = TRUE), #,width = '1000px'),
                
                ###### ------------------------ Sigma Both Unknown ---------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.bothsigmaKnownRaw == 'bothUnknown'",
                  
                  radioButtons(
                    inputId      = ns("bothsigmaEqualRaw"),
                    label        = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                    choiceValues = list("TRUE",
                                        "FALSE"),
                    choiceNames  = list("Yes (Pooled)",
                                        "No (Welch-Satterthwaite df)"),
                    selected     = "TRUE",
                    inline       = TRUE)
                ), # Sigma Both Unknown
                
                ###### ------------------------ Sigma Both Known -----------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.bothsigmaKnownRaw == 'bothKnown'",
                  
                  numericInput(
                    inputId = ns("popuSDRaw1"),
                    label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                    value   = 4.54,
                    min     = 0.00001,
                    step    = 0.00001),
                  
                  numericInput(
                    inputId = ns("popuSDRaw2"),
                    label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                    value   = 3.47,
                    min     = 0.00001,
                    step    = 0.00001),
                ), #Sigma Both Known
              ), # Raw Data
              
              ##### -------------------- Uploaded Data -------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability2 == 'Upload Data'",
                
                HTML(uploadDataDisclaimer),
                
                fileInput(
                  inputId = ns("indMeansUserData"),
                  label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"),
                  accept  = c("text/csv",
                              "text/comma-separated-values",
                              "text/plain",
                              ".csv",
                              ".xls",
                              ".xlsx")),
                
                selectizeInput(
                  inputId = ns("indMeansUplSample1"),
                  label   = strong("Column for Sample 1"),
                  choices = c(""),
                  options = list(placeholder = 'Select a column',
                                 onInitialize = I('function() { this.setValue(""); }'))),
                
                selectizeInput(
                  inputId = ns("indMeansUplSample2"),
                  label   = strong("Column for Sample 2"),
                  choices = c(""),
                  options = list(placeholder = 'Select a column',
                                 onInitialize = I('function() { this.setValue(""); }'))),
                
                radioButtons(
                  inputId      = ns("bothsigmaKnownUpload"),
                  label        = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                  choiceValues = list("bothKnown",
                                      "bothUnknown"),
                  choiceNames  = list("Both Known",
                                      "Both Unknown"),
                  selected     = "bothUnknown",
                  inline       = TRUE),
                
                ###### ------------------------ Sigma Both Unknown ---------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.bothsigmaKnownUpload == 'bothUnknown'",
                  
                  radioButtons(
                    inputId      = ns("bothsigmaEqualUpload"),
                    label        = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                    choiceValues = list("TRUE",
                                        "FALSE"),
                    choiceNames  = list("Yes (Pooled)",
                                        "No (Welch-Satterthwaite df)"),
                    selected     = "TRUE",
                    inline       = TRUE)
                ), # Sigma Both Unknown
                
                ###### ------------------------ Sigma Both Known ---------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.bothsigmaKnownUpload == 'bothKnown'",
                  
                  numericInput(
                    inputId = ns("popuSDUpload1"),
                    label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                    value   = "",
                    min     = 0.00001,
                    step    = 0.00001),
                  
                  numericInput(
                    inputId = ns("popuSDUpload2"),
                    label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                    value   = "",
                    min     = 0.00001,
                    step    = 0.00001)
                ), # Sigma Both Known
              ), # Upload Data
            ), # Two Independent Samples
            
            #### ---------------- Wilcoxon Rank Sum Test ------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Wilcoxon rank sum test'",
              
              radioButtons(
                inputId      = ns("wilcoxonRankSumTestData"),
                label        = strong("Data Availability"),
                choiceValues = list("Enter Raw Data",
                                    "Upload Data"),
                choiceNames  = list("Enter Raw Data",
                                    "Upload Data"),
                selected     = "Enter Raw Data", #character(0), #
                inline       = TRUE), #,width = '1000px'),
              
              ##### -------------------- Raw Data ------------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.wilcoxonRankSumTestData == 'Enter Raw Data'",
                
                textAreaInput(
                  inputId     = ns("rankSumRaw1"),
                  label       = strong("Sample 1"),
                  value       = "2,  1.25,  8.5,  1.1,  1.25,  3.75,  5.5",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3),
                
                textAreaInput(
                  inputId     = ns("rankSumRaw2"),
                  label       = strong("Sample 2"),
                  value       = "1,  1,  0,  3.25,  1,  0.25",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3),
              ), # Wilcoxon Raw Data
              
              ##### -------------------- Uploaded Data -------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.wilcoxonRankSumTestData == 'Upload Data'",
                
                HTML(uploadDataDisclaimer),
                
                fileInput(
                  inputId = ns("wilcoxonUpl"),
                  label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"),
                  accept  = c("text/csv",
                              "text/comma-separated-values",
                              "text/plain",
                              ".csv",
                              ".xls",
                              ".xlsx")),
                
                selectizeInput(
                  inputId = ns("wilcoxonUpl1"),
                  label   = strong("Column for Sample 1"),
                  choices = c(""),
                  options = list(placeholder = 'Select a column',
                                 onInitialize = I('function() { this.setValue(""); }'))),
                
                selectizeInput(
                  inputId = ns("wilcoxonUpl2"),
                  label   = strong("Column for Sample 2"),
                  choices = c(""),
                  options = list(placeholder = 'Select a column',
                                 onInitialize = I('function() { this.setValue(""); }'))),
              ), # Upload Data
            ), # Wilcoxon Rank Sum Test
            
            
            #### ---------------- Dep Pop Means ------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Dependent Population Means'",
              
              radioButtons(
                inputId      = ns("dataTypeDependent"),
                label        = strong("Data Availability"),
                choiceValues = list("Enter Raw Data",
                                    "Upload Data"),
                choiceNames  = list("Enter Raw Data",
                                    "Upload Data"),
                selected     = "Enter Raw Data",
                inline       = TRUE),
              
              ##### -------------------- Raw Data ------------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataTypeDependent == 'Enter Raw Data'",
                
                textAreaInput(
                  inputId     = ns("before"),
                  label       = strong("Sample 1 (e.g. Before, Pre-Treatment, Baseline)"),
                  value       = "484, 478, 492, 444, 436, 398, 464, 476",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3),
                
                textAreaInput(
                  inputId     = ns("after"),
                  label       = strong("Sample 2 (e.g. After, Post-Treatment, Follow-Up)"),
                  value       = "488, 478, 480, 426, 440, 410, 458, 460",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3)
              ), # Raw Data
              
              ##### -------------------- Uploaded Data -------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.dataTypeDependent == 'Upload Data'",
                
                HTML(uploadDataDisclaimer),
                
                fileInput(
                  inputId = ns("depMeansUserData"),
                  label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"),
                  accept  = c("text/csv",
                              "text/comma-separated-values",
                              "text/plain",
                              ".csv",
                              ".xls",
                              ".xlsx")),
                
                selectizeInput(
                  inputId = ns("depMeansUplSample1"),
                  label   = strong("Column for Sample 1 (e.g. Before, Pre-Treatment, Baseline)"),
                  choices = c(""),
                  options = list(placeholder = 'Select a column',
                                 onInitialize = I('function() { this.setValue(""); }'))),
                
                selectizeInput(
                  inputId = ns("depMeansUplSample2"),
                  label   = strong("Column for Sample 2 (e.g. After, Post-Treatment, Follow-Up)"),
                  choices = c(""),
                  options = list(placeholder = 'Select a column',
                                 onInitialize = I('function() { this.setValue(""); }'))),
              ) # Upload Data
            ), # Two Dependent Samples
            
            #### ---------------- 2 Pop Proportions --------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Population Proportions'",
              
              numericInput(
                inputId = ns("numSuccesses1"),
                label   = strong("Number of Successes 1 (\\( x_{1}\\))"),
                value   = 174,
                min     = 0,
                step    = 1),
              
              numericInput(
                inputId = ns("numTrials1"),
                label   = strong("Number of Trials 1 (\\( n_{1}\\))"),
                value   = 300,
                min     = 1,
                step    = 1),
              
              numericInput(
                inputId = ns("numSuccesses2"),
                label   = strong("Number of Successes 2 (\\( x_{2}\\))"),
                value   = 111,
                min     = 0,
                step    = 1),
              
              numericInput(
                inputId = ns("numTrials2"),
                label   = strong("Number of Trials 2 (\\( n_{2}\\))"),
                value   = 300,
                min     = 1,
                step    = 1),
            ), # Two Population Proportions
            
            
            
            
            ### ------------ 2 Pop Standard Deviations ------------------------------------
            
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Two Population Variances'",
              
              radioButtons(
                inputId      = ns("dataAvailability3"),
                label        = strong("Data Availability"),
                choiceValues = list("Summary",
                                    "Variance",
                                    "Enter Raw Data"),
                choiceNames  = list(
                  "\\( n_1,\\ n_2,\\ s_1,\\ s_2 \\)",
                  "\\( n_1,\\ n_2,\\ s_1^2,\\ s_2^2 \\)",
                  "Enter Raw Data"
                ),
                selected     = "Summary",
                inline       = TRUE),
              
              
              ### ------------ Summary (n1, n2, s1, s2) ------------------------------------
              
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability3 == 'Summary'",
                
                withMathJax(
                  tagList(
                    numericInput(
                      inputId = ns("SDSampleSize1"),
                      label   = HTML("<strong>Sample Size 1</strong> \\( (n_1) \\)"),
                      value   = 12,
                      min     = 1,
                      step    = 1
                    ),
                    
                    
                    numericInput(
                      inputId = ns("stdDev1"),
                      label   = HTML("<strong>Sample Standard Deviation 1</strong> \\( (s_1) \\)"),
                      value   = 3,
                      min     = 1,
                      step    = 0.01
                    ),
                    
                    numericInput(
                      inputId = ns("SDSampleSize2"),
                      label   = HTML("<strong>Sample Size 2</strong> \\( (n_2) \\)"),
                      value   = 18,
                      min     = 1,
                      step    = 1
                    ),
                    
                    
                    numericInput(
                      inputId = ns("stdDev2"),
                      label   = HTML("<strong>Sample Standard Deviation 2</strong> \\( (s_2) \\)"),
                      value   = 4.8,
                      min     = 1,
                      step    = 0.01
                    )
                  ))), #summary,
              
              ### ------------ Variance (n1, s1^2, n2, s2^2) ------------------------------------ 
              
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability3 == 'Variance'",
                withMathJax(
                  tagList(
                    numericInput(
                      inputId = ns("n1"),
                      label   = HTML("<strong>Sample Size 1</strong> \\( (n_1) \\)"),
                      value   = 12,
                      min     = 1,
                      step    = 1),
                    
                    numericInput(
                      inputId = ns("s1sq"),
                      label   = HTML("<strong>Sample Variance 1 </strong>\\( (s_1^2) \\)"),
                      value   = 9,
                      min     = 1,
                      step    = 0.01),
                    
                    numericInput(
                      inputId = ns("n2"),
                      label   = HTML("<strong>Sample Size 2</strong> \\( (n_2) \\)"),
                      value   = 18,
                      min     = 1,
                      step    = 1),
                    
                    numericInput(
                      inputId = ns("s2sq"),
                      label   = HTML("<strong>Sample Variance 2</strong> \\( (s_2^2) \\)"),
                      value   = 23.04,
                      min     = 1,
                      step    = 0.01)
                  ))), # variance
              
              ### ------------- Raw Data ---------------------------------------------------------
              
              conditionalPanel(
                ns = ns,
                condition = "input.dataAvailability3 == 'Enter Raw Data'",
                
                textAreaInput(
                  inputId     = ns("rawSamp1SD"),
                  label       = strong("Sample 1 (e.g Class A test scores)"),
                  value       = "80, 54, 97, 76, 66, 87, 83, 91",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3),
                
                textAreaInput(
                  inputId     = ns("rawSamp2SD"),
                  label       = strong("Sample 2 (e.g Class B test scores)"),
                  value       = "45, 54, 67, 95, 100, 82, 83, 74",
                  placeholder = "Enter values separated by a comma with decimals as points",
                  rows        = 3)
              ), # Raw Data
            ), # Two Pop Var
            
            ### ------------ Confidence Level, Inference Type ---------------------------------
            
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters != 'Wilcoxon rank sum test'",
              
              radioButtons(
                inputId      = ns("inferenceType2"),
                label        = strong("Inference Type"),
                choiceValues = list("Confidence Interval",
                                    "Hypothesis Testing"),
                choiceNames  = list("Confidence Interval",
                                    "Hypothesis Testing"),
                selected     = "Confidence Interval",
                inline       = TRUE)
            ),
            
            conditionalPanel(
              ns = ns,
              condition = "input.inferenceType2 == 'Confidence Interval' && input.popuParameters != 'Wilcoxon rank sum test'",
              
              radioButtons(
                inputId  = ns("confidenceLevel2"),
                label    = strong("Confidence Level (\\( 1- \\alpha\\))"),
                choices  = c("90%",
                             "95%",
                             "99%"),
                selected = c("95%"),
                inline   = TRUE)
            ), # Confidence Interval
            
            conditionalPanel(
              ns = ns,
              condition = "input.inferenceType2 == 'Hypothesis Testing' || input.popuParameters == 'Wilcoxon rank sum test'",
              
              radioButtons(
                inputId  = ns("significanceLevel2"),
                label    = strong("Significance Level (\\( \\alpha\\))"),
                choices  = c("10%",
                             "5%",
                             "1%"),
                selected = c("5%"),
                inline   = TRUE),
              
              conditionalPanel(
                ns = ns,
                condition = "input.popuParameters == 'Independent Population Means'",
                
                numericInput(
                  inputId = ns("indMeansMuNaught"),
                  label   = strong(HTML("Hypothesized Population Mean Difference \\( (\\mu_{1} - \\mu_{2})_{0} \\) Value")),
                  value   = 0,
                  step    = 0.00001)
              ), # indMeansMuNaught
              
              conditionalPanel(
                ns = ns,
                condition = "input.popuParameters == 'Dependent Population Means'",
                
                numericInput(
                  inputId = ns("depMeansMuNaught"),
                  label   = strong(HTML("Hypothesized Population Mean Difference \\( (\\mu_{d})_{0} \\) Value")),
                  value   = 0,
                  step    = 0.00001)
              ), # depMeansMuNaught
              
              conditionalPanel(
                ns = ns,
                condition = "input.popuParameters == 'Population Proportions'",
                
                numericInput(
                  inputId = ns("propDiffNaught"),
                  label   = strong(HTML("Hypothesized Population Proportion Difference \\( (p_{1} - p_{2})_{0} \\) Value")),
                  value   = 0,
                  step    = 0.00001)
              ), # propDiffNaught
              
              selectizeInput(
                inputId  = ns("altHypothesis2"),
                label    = strong("Alternate Hypothesis (\\( H_{a}\\))"),
                choices  = lessThanInequalGreaterThanChoices123,
                selected = 2,
                options  = list(render = I(render))),
            ), # Hypothesis Testing
            conditionalPanel(
              ns = ns,
              condition  = "input.popuParameters == 'Wilcoxon rank sum test'", 
              radioButtons(
                inputId  = ns("normaprowrs"),
                label    = strong("Method"), # A more descriptive label
                choices  = c("Exact", "Normal approximation (for large samples)"),
                selected = "Exact",
                inline   = TRUE),
              conditionalPanel(
                ns = ns,
                condition  = "input.normaprowrs == 'Normal approximation (for large samples)'", # This is the inner condition, checking the checkbox
                radioButtons(
                  inputId  = ns("continuityCorrectionOption"),
                  label    = strong("Continuity correction"),
                  choices  = c("True", "False"),
                  selected = "True", # 'selected' argument takes a single value, not a vector
                  inline   = TRUE ))
            ),
            conditionalPanel(
              ns = ns,
              condition = "(input.popuParameters == 'Independent Population Means' && input.dataAvailability2 != 'Summarized Data')",
              
              p(strong("Graph Options")),
              
              checkboxInput(
                inputId = ns("indMeansBoxplot"),
                label   = "Side-by-side Boxplot for Sample Data",
                value   = TRUE),
              
              checkboxInput(
                inputId = ns("indMeansQQPlot"),
                label   = "Q-Q Plots for Sample 1 and Sample 2",
                value   = TRUE)
            ), # Ind Means !Summarized
            
            conditionalPanel(
              ns = ns,
              condition = "(input.popuParameters == 'Dependent Population Means')",
              
              p(strong("Graph Options")),
              
              checkboxInput(
                inputId = ns("depMeansQQPlot"),
                label   = "Q-Q Plot of the Difference (d)",
                value   = TRUE)
            ), # Dep Means Graphs
            
            conditionalPanel(
              ns = ns,
              condition = "input.popuParameters == 'Wilcoxon rank sum test'",
              
              p(strong("Graph Options")),
              
              checkboxInput(
                inputId = ns("sidebysidewRankSum"),
                label   = "Side-by-side Boxplot",
                value   = TRUE),
              
              checkboxInput(
                inputId = ns("sidebysidewRankQQ"),
                label   = "Q-Q plots for Sample 1 and Sample 2",
                value   = TRUE)
            ), # Wilcoxon Rank Sum Graphs
            
            
          ), # "input.siMethod == '2'",
          
          ### ------------ Multiple Samples (ANOVA or Kruskal-Wallis) ------------------------------------
          conditionalPanel(
            ns = ns,
            condition = 'input.siMethod == "Multiple"',
            
            HTML("<label class='si-label'><b>Hypothesis Test</b></label>"),
            
            radioButtons(
              inputId = ns("multipleMethodChoice"),
              label   = NULL,
              choiceNames = c("One-way Analysis of Variance (ANOVA)", "Kruskal-Wallis"),
              choiceValues = c("anova", "kw")
            ),
            
            #### ------------ ANOVA ------------------------------------
            conditionalPanel(
              ns = ns,
              condition = 'input.multipleMethodChoice == "anova"',
              
              HTML(uploadDataDisclaimer),
              
              fileInput(
                inputId = ns("anovaUserData"),
                label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"),
                accept  = c("text/csv",
                            "text/comma-separated-values",
                            "text/plain",
                            ".csv",
                            ".xls",
                            ".xlsx")),
              
              hidden(tagList(
                div(
                  id = ns("anovaUploadInputs"),
                  
                  radioButtons(
                    inputId = ns("anovaFormat"),
                    label   = strong("Data Format"),
                    choiceNames = c("Values in multiple columns",
                                    "Responses and factors stacked in two columns"),
                    choiceValues = c("Multiple",
                                     "Stacked")),
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.anovaFormat == 'Multiple'",
                    
                    selectizeInput(
                      inputId = ns("anovaMultiColumns"),
                      label = strong("Choose columns to conduct analysis"),
                      choices = c(""),
                      multiple = TRUE,
                      selected = NULL,
                      options = list(hideSelected = FALSE,
                                     placeholder = 'Select two or more columns',
                                     onInitialize = I('function() { this.setValue(""); }')))
                  ), #multiple column anova
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.anovaFormat == 'Stacked'",
                    
                    selectizeInput(
                      inputId = ns("anovaResponse"),
                      label = strong("Response Variable"),
                      choices = c(""),
                      selected = NULL,
                      options = list(placeholder = 'Select a variable',
                                     onInitialize = I('function() { this.setValue(""); }'))),
                    
                    selectizeInput(
                      inputId = ns("anovaFactors"),
                      label = strong("Factors"),
                      choices = c(""),
                      selected = NULL,
                      options = list(placeholder = 'Select a factor',
                                     onInitialize = I('function() { this.setValue(""); }')))
                  ) #stacked column anova
                  
                ), #anovaUploadInputs div
              )), #hidden tagList
              
              radioButtons(
                inputId  = ns("anovaSigLvl"),
                label    = strong("Significance Level (\\( \\alpha\\))"),
                choices  = c("10%",
                             "5%",
                             "1%"),
                selected = "5%",
                inline   = TRUE),
              
              checkboxGroupInput(
                inputId = ns("anovaOptions"),
                label = p(strong("Options")),
                choiceNames = c("Include post hoc tests"),
                choiceValues = c("posthoc"),
                selected = NULL),
              
              selectizeInput(
                inputId = ns("anovaGraphs"),
                label = strong("Graphs"),
                choices = c("Side-by-side Boxplot",
                            "Histogram of Residuals",
                            "QQ Plot of Residuals",
                            "Plot Group Means"),
                multiple = TRUE,
                selected = c("Side-by-side Boxplot",
                             "Plot Group Means"),
                options = list(hideSelected = FALSE,
                               placeholder = 'Select graph(s) to display')), 
              
            ), #anova conditionalPanel
            
            #### ------------ Kruskal-Wallis ------------------------------------
            conditionalPanel(
              ns = ns,
              condition = 'input.multipleMethodChoice == "kw"',
              
              HTML(uploadDataDisclaimer),
              
              fileInput(
                inputId = ns("kwUserData"),
                label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"),
                accept  = c("text/csv",
                            "text/comma-separated-values",
                            "text/plain",
                            ".csv",
                            ".xls",
                            ".xlsx")),
              
              hidden(tagList(
                div(
                  id = ns("kwUploadInputs"),
                  
                  radioButtons(
                    inputId = ns("kwFormat"),
                    label   = strong("Data Format"),
                    choiceNames = c("Values in multiple columns",
                                    "Responses and factors stacked in two columns"),
                    choiceValues = c("Multiple",
                                     "Stacked")),
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.kwFormat == 'Multiple'",
                    
                    selectizeInput(
                      inputId = ns("kwMultiColumns"),
                      label = strong("Choose columns to conduct analysis"),
                      choices = c(""),
                      multiple = TRUE,
                      selected = NULL,
                      options = list(hideSelected = FALSE,
                                     placeholder = 'Select two or more columns',
                                     onInitialize = I('function() { this.setValue(""); }')))
                  ), #multiple column kw
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.kwFormat == 'Stacked'",
                    
                    selectizeInput(
                      inputId = ns("kwResponse"),
                      label = strong("Response Variable"),
                      choices = c(""),
                      selected = NULL,
                      options = list(placeholder = 'Select a variable',
                                     onInitialize = I('function() { this.setValue(""); }'))),
                    
                    selectizeInput(
                      inputId = ns("kwFactors"),
                      label = strong("Factors"),
                      choices = c(""),
                      selected = NULL,
                      options = list(placeholder = 'Select a factor',
                                     onInitialize = I('function() { this.setValue(""); }')))
                  ) #stacked column kw
                  
                ), #kwUploadInputs div
              )), #hidden tagList
              
              radioButtons(
                inputId = ns("kwSigLvl"),
                label = strong("Significance Level (\\( \\alpha\\))"),
                choices  = c("10%",
                             "5%",
                             "1%"),
                selected = "5%",
                inline   = TRUE),
              
            ) #Kruskal-Wallis conditionalPanel
          ), #Multiple Samples conditionalPanel
          
          ### ------------ Categorical Samples (Chi-Square) ----------------------------
          conditionalPanel(
            ns = ns,
            condition = 'input.siMethod == "Categorical"',
            
            radioButtons(
              inputId = ns("chisquareDimension"),
              label   = strong("Dimension"),
              choices = c("2 x 2",
                          "2 x 3",
                          "3 x 2",
                          "3 x 3"),
              inline  = TRUE),
            
            conditionalPanel(
              ns = ns,
              condition = "input.chisquareDimension == '2 x 2'",
              
              matrixInput(
                inputId = ns("chiSqInput2x2"),
                inputClass = "cMatrix",
                value = matrix(c(173,599, 160,851),
                               nrow = 2,
                               ncol = 2,
                               dimnames = list(c("R1", "R2"),
                                               c("C1", "C2"))),
                rows = list(editableNames = TRUE),
                cols = list(editableNames = TRUE),
                class = "numeric"),
            ), #2 x 2
            
            conditionalPanel(
              ns = ns,
              condition = "input.chisquareDimension == '2 x 3'",
              
              matrixInput(
                inputId = ns("chiSqInput2x3"),
                inputClass = "cMatrix",
                value = matrix(c(160,40, 140,60, 40,60),
                               nrow = 2,
                               ncol = 3,
                               dimnames = list(c("R1", "R2"),
                                               c("C1", "C2", "C3"))),
                rows = list(editableNames = TRUE),
                cols = list(editableNames = TRUE),
                class = "numeric"),
            ), #2 x 3
            
            conditionalPanel(
              ns = ns,
              condition = "input.chisquareDimension == '3 x 2'",
              
              matrixInput(
                inputId = ns("chiSqInput3x2"),
                inputClass = "cMatrix",
                value = matrix(c(162,106,201, 353,259,332),
                               nrow = 3,
                               ncol = 2,
                               dimnames = list(c("R1", "R2", "R3"),
                                               c("C1", "C2"))),
                rows = list(editableNames = TRUE),
                cols = list(editableNames = TRUE),
                class = "numeric"),
            ), #3 x 2
            
            conditionalPanel(
              ns = ns,
              condition = "input.chisquareDimension == '3 x 3'",
              
              matrixInput(
                inputId = ns("chiSqInput3x3"),
                inputClass = "cMatrix",
                value = matrix(c(6,14,50, 38,31,50, 31,4,5),
                               nrow = 3,
                               ncol = 3,
                               dimnames = list(c("R1", "R2", "R3"),
                                               c("C1", "C2", "C3"))),
                rows = list(editableNames = TRUE),
                cols = list(editableNames = TRUE),
                class = "numeric"),
            ), #3 x 3
            
            textInput(
              inputId = ns("chiSquareRowHeader"),
              label = "Name for Row Variable (optional):",
              value = ""),
            
            textInput(
              inputId = ns("chiSquareColHeader"),
              label = "Name for Column Variable (optional):",
              value = ""),
            
            # tableHTML(mtcars,
            #           rownames = TRUE,
            #           widths = c(150, 100, rep(50, 11)),
            #           row_groups = list(c(10, 10, 12), c('Group 1', 'Group 2', 'Group 3'))) %>%
            #   add_css_column(css = list('background-color', 'lightgray'), columns = 'row_groups') %>%
            #   add_css_column(css = list('text-align', 'right'), columns = 'row_groups') %>%
            #   add_css_header(css = list('background-color', 'lightgray'), headers = 1) %>%
            #   add_editable_column(columns = -1:3),
            # br(),
            
            radioButtons(
              inputId  = ns("chisquareMethod"),
              label    = strong("Hypothesis Test"),
              choiceNames  = c("Chi-Square test for independence",
                               "Fisher's Exact test"),
              choiceValues = c("Chi-Square",
                               "Fisher"),
              selected = c("Chi-Square"),
              inline   = TRUE),
            
            conditionalPanel(
              ns = ns,
              condition = 'input.chisquareMethod == "Chi-Square" && input.chisquareDimension == "2 x 2"',
              
              checkboxInput(
                inputId = ns("chiSquareYates"),
                label   = "with Yates continuity correction",
                value   = FALSE),
              
              # tooltip(
              #   span("Card title ", bsicons::bs_icon("question-circle-fill")),
              #   "Additional info",
              #   placement = "right"
              # ),
            ),
            
            radioButtons(
              inputId  = ns("chisquareSigLvl"),
              label    = strong("Significance Level (\\( \\alpha\\))"),
              choices  = c("10%",
                           "5%",
                           "1%"),
              selected = c("5%"),
              inline   = TRUE),
          ), #Categorical (Chi-Square)
          
          actionButton(
            inputId = ns("goInference"),
            label   = "Calculate",
            class = "act-btn"),
          
          actionButton(
            inputId = ns("resetInference"),
            label   = "Reset Values",
            class = "act-btn")
          
        ) #inputPanel
      ), #sidebarPanel
      
      #  ========================================================================= #
      ## -------- Main Panel -----------------------------------------------------
      #  ========================================================================= #
      mainPanel(
        div(
          id = ns("inferenceMP"),
          
          uiOutput(ns("inferenceValidation")),
          
          div(id = ns("inferenceData"),
              
              ### ------------ 1 Sample ----------------------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.siMethod == '1'",
                
                #### ---------------- 1 Pop Mean ---------------------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.popuParameter == 'Population Mean'",
                  
                  navbarPage(
                    id = ns("onePopMeanTabset"),
                    title = NULL,
                    
                    tabPanel(
                      id = ns("onePopMean"),
                      title = "Analysis",
                      
                      conditionalPanel(
                        ns = ns,
                        condition = "input.inferenceType == 'Confidence Interval'",
                        
                        titlePanel(tags$u("Confidence Interval")),
                        br(),
                        uiOutput(ns('oneMeanCI')),
                        br()
                      ), # Confidence Interval
                      
                      conditionalPanel(
                        ns = ns,
                        condition = "input.inferenceType == 'Hypothesis Testing'",
                        
                        titlePanel(tags$u("Hypothesis Test")),
                        br(),
                        uiOutput(ns('oneMeanHT')),
                        br(),
                      ) # Hypothesis Testing
                    ), # One Population Mean
                    
                    tabPanel(
                      id = ns("onePopMeanData"),
                      title = "Uploaded Data",
                      
                      uiOutput(ns("renderOnePopMeanData")),
                    ), #onePopMeanData Uploaded Data tabPanel
                    
                    tabPanel(
                      id = ns("onePopMeanData"),
                      title = "Graphs",
                      conditionalPanel(
                        ns = ns,
                        condition = "input.dataAvailability != 'Summarized Data' && input.oneMeanBoxplot == 1",
                        br(),
                        titlePanel(tags$u("Boxplot")),
                        br(),
                        plotOptionsMenuUI(
                          id = ns("oneMeanBoxplot"),
                          plotType = "Boxplot",
                          title = "Boxplot"),
                        uiOutput(ns("renderOneMeanBoxplot")),
                        br(),
                      ),
                    ), #onePopMean Graphs tabPanel
                  )), #onePopMean tabsetPanel
                
                #### ---------------- 1 Pop Prop ---------------------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.popuParameter == 'Population Proportion'",
                  
                  navbarPage(
                              id = ns("onePropTabset"),
                              title = NULL,
                              tabPanel(title = "Analysis",
                                       conditionalPanel(
                                         ns = ns,
                                         condition = "input.inferenceType == 'Confidence Interval'",
                                         
                                         titlePanel(tags$u("Confidence Interval")),
                                         br(),
                                         uiOutput(ns('onePropCI')),
                                         br()
                                       ),
                                       
                                       conditionalPanel(
                                         ns = ns,
                                         condition = "input.inferenceType == 'Hypothesis Testing'",
                                         
                                         titlePanel(tags$u("Hypothesis Test")),
                                         br(),
                                         uiOutput(ns('onePropHT')),
                                         br()
                                       )
                              ),
                              
                              tabPanel(
                                title = "Graphs",
                                br(),
                                div(
                                  style = "display: flex; justify-content: flex-start;",
                                  plotOutput(ns("onePropBarGraph"), width = "400px")
                                ),
                                br(),
                                div(
                                  style = "display: flex; justify-content: flex-start;",
                                  plotOutput(ns("onePropPieChart"), width = "400px")
                                )
                              ))), # One Population Proportion
                
                #### ---------------- 1 Pop Standard deviation -------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.popuParameter == 'Population Standard Deviation'",
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.inferenceType == 'Confidence Interval'",
                    
                    titlePanel(tags$u("Confidence Interval")),
                    br(),
                    uiOutput(ns('oneSDCI')),
                    br(),
                  ), # Confidence Interval
                  
                  conditionalPanel(
                    ns = ns,
                    condition = "input.inferenceType == 'Hypothesis Testing'",
                    
                    titlePanel(tags$u("Hypothesis Test")),
                    br(),
                    uiOutput(ns('onePopulationSDHT')),
                    br(),
                  ), # Hypothesis Testing
                ), # One Population Proportion
              ), # "input.siMethod == '1'"
              
              ### ------------ 2 Samples ---------------------------------------------------
              conditionalPanel( #### Two Samp ----
                                ns = ns,
                                condition = "input.siMethod == '2'",
                                
                                #### ---------------- Independent Pop Means ----------------------------------
                                conditionalPanel(
                                  ns = ns,
                                  condition = "input.popuParameters == 'Independent Population Means'",
                                  
                                  navbarPage(
                                    id = ns("indPopMeansTabset"),
                                    selected = "Analysis",
                                    title = NULL,
                                    
                                    tabPanel(
                                      id = ns("indPopMeans"),
                                      title = "Analysis",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Confidence Interval'",
                                        
                                        titlePanel(tags$u("Confidence Interval")),
                                        br(),
                                        uiOutput(ns('indMeansCI')),
                                        br(),
                                      ), # Confidence interval
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                        
                                        titlePanel(tags$u("Hypothesis Test")),
                                        br(),
                                        uiOutput(ns('indMeansHT')),
                                        br()
                                      ) # Hypothesis Testing
                                    ), # indPopMeans Analysis tabPanel
                                    
                                    tabPanel(
                                      id = ns("indPopMeansGraphs"),
                                      title = "Graphs",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.dataAvailability2 != 'Summarized Data' && input.indMeansBoxplot == 1",
                                        br(),
                                        titlePanel(tags$u("Boxplot")),
                                        br(),
                                        plotOptionsMenuUI(
                                          id = ns("indMeansBoxplot"),
                                          plotType = "Boxplot",
                                          title = "Boxplot"),
                                        uiOutput(ns("renderIndMeansBoxplot")),
                                        br(),
                                        br()
                                      ),
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.dataAvailability2 != 'Summarized Data' && input.indMeansQQPlot == 1",
                                        
                                        br(),
                                        hr(),
                                        br(),
                                        titlePanel(tags$u("Q-Q Plots for Sample 1 and Sample 2")),
                                        br(),
                                        plotOptionsMenuUI(
                                          id = ns("indMeansQQPlot"),
                                          plotType = "QQ Plot",
                                          title = "Q-Q Plots"),
                                        uiOutput(ns("renderIndMeansQQPlot")),
                                        br(),
                                        br()
                                      )
                                    ), # indPopMeans Graphs tabPanel
                                    
                                    tabPanel(
                                      id = ns("indPopMeansData"),
                                      title = "Uploaded Data",
                                      
                                      uiOutput(ns("renderIndPopMeansData"))
                                    ), # indPopMeansData Uploaded Data tabPanel
                                    
                                  ), # indPopMeansTabset
                                ), # Two Independent Samples
                                #### ---------------- Dependent Pop Means ----------------------------------
                                conditionalPanel(
                                  ns = ns,
                                  condition = "input.popuParameters == 'Dependent Population Means'",
                                  
                                  navbarPage(
                                    id = ns("depPopMeansTabset"),
                                    selected = "Analysis",
                                    title = NULL,
                                    
                                    tabPanel(
                                      id = ns("depPopMeans"),
                                      title = "Analysis",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Confidence Interval'",
                                        
                                        titlePanel(tags$u("Confidence Interval")),
                                        br(),
                                        uiOutput(ns('depMeansCI')),
                                        br(),
                                      ), # CI
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                        
                                        titlePanel(tags$u("Hypothesis Test")),
                                        br(),
                                        uiOutput(ns('depMeansHT')),
                                        br()
                                      ), # HT
                                    ), #depPopMeans Analysis tabPanel
                                    
                                    tabPanel(
                                      id = ns("depPopMeansData"),
                                      title = "Uploaded Data",
                                      
                                      uiOutput(ns("renderDepPopMeansData")),
                                    ), #depPopMeansData Uploaded Data tabPanel
                                    
                                    tabPanel(
                                      id = ns("depMeansDataCalcs"),
                                      title = "Data with Calculations",
                                      br(),
                                      fluidRow(
                                        column(width = 8,
                                               uiOutput(ns('depMeansTable')),
                                        ),
                                        
                                        column(width = 4,
                                               br(),
                                        )
                                      ),
                                      br(),
                                      br(),
                                    ), # Dep means table with calcs
                                    
                                    tabPanel(
                                      id = ns("depMeansGraphs"),
                                      title = "Graphs",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.depMeansQQPlot == 1",
                                        br(),
                                        titlePanel(tags$u("Q-Q Plot of the Difference (d)")),
                                        br(),
                                        plotOptionsMenuUI(
                                          id = ns("depMeansQQPlot"),
                                          plotType = "QQ Plot",
                                          title = "Q-Q Plot of the Difference (d)"),
                                        uiOutput(ns("renderDepMeansQQPlot")),
                                        br(),
                                        br()
                                      )  
                                    ), # Dep means graphs tab panel
                                  ), # depPopMeansTabset
                                ), # Two Dependent Samples
                                #### ---------------- 2 Pop Proportions --------------------------------------
                                conditionalPanel(
                                  ns = ns,
                                  condition = "input.popuParameters == 'Population Proportions'",
                                  
                                  navbarPage(
                                    id = ns("twoPropTabset"),
                                    selected = "Analysis",
                                    title = NULL,
                                    
                                    tabPanel(
                                      id = ns("twoProp"),
                                      title = "Analysis",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Confidence Interval'",
                                        
                                        titlePanel(tags$u("Confidence Interval")),
                                        br(),
                                        uiOutput(ns('twoPropCI')),
                                        br(),
                                      ), # Confidence Interval
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                        
                                        titlePanel(tags$u("Hypothesis Test")),
                                        br(),
                                        uiOutput(ns('twoPropHT')),
                                        br(),
                                      ), # Hypothesis Testing
                                    ), # Analysis Panel
                                    tabPanel(
                                      id = ns("twoPropGraphs"),
                                      title = "Graphs",
                                      br(),
                                      div(
                                        style = "width: 600px; text-align: left;",
                                        plotOutput(ns("twoPropBarPlot"), height = "400px")),
                                      div(
                                        style = "display: flex; justify-content: flex-start;", 
                                        plotOutput(ns("twoPropPieChart"), 
                                                   width = "600px", height = "500px"))
                                    ), # Graph Panel
                                  )), # Two Population Proportions
                                
                                #### ---------------- Wilcoxon Rank Sum --------------------------------------
                                conditionalPanel(
                                  ns = ns,
                                  condition = "input.popuParameters == 'Wilcoxon rank sum test'",
                                  
                                  navbarPage(
                                    id = ns("wilcoxonRankSumTabset"),
                                    selected = "Analysis",
                                    title = NULL,
                                    
                                    tabPanel(
                                      id = ns("wilcoxonRankSumTab"),
                                      title = "Analysis",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Hypothesis Testing' || input.inferenceType2 == 'Confidence Interval'",
                                        
                                        titlePanel(tags$u("Hypothesis Test")),
                                        br(),
                                        uiOutput(ns('wilcoxonRankSum')),
                                        br(),
                                      ), # Hypothesis Testing
                                    ), # Analysis Tab
                                    tabPanel(
                                      id = ns("wilcoxonRankSumDataRanks"),
                                      title = "Data with Ranks",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Hypothesis Testing' || input.inferenceType2 == 'Confidence Interval'",
                                        
                                        titlePanel("Ranked Results by Group"),
                                        br(),
                                        uiOutput(ns('wilcoxonRankSumDataRanks')),
                                        br(),
                                      ), # Ranked Results by Group
                                    ), # Data with Ranks Tab
                                    tabPanel(
                                      id = ns("wilcoxonRankSumGraphs"),
                                      title = "Graphs",
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.popuParameters == 'Wilcoxon rank sum test' && (input.sidebysidewRankSum == 1 || input.sidebysidewRankQQ == 1)",
                                        
                                        # Side-by-side Boxplot
                                        conditionalPanel(
                                          ns = ns,
                                          condition = "input.popuParameters == 'Wilcoxon rank sum test' && input.sidebysidewRankSum == 1",
                                          titlePanel("Side-by-side Boxplot"),
                                          br(),
                                          plotOptionsMenuUI(
                                            id = ns("sidebysidewRankSum"),
                                            plotType = "Boxplot",
                                            title = "Boxplot"
                                          ),
                                          plotOutput(ns("sidebysidewRankSum")),
                                          br(),br()
                                        ),
                                        
                                        # Q-Q Plots
                                        conditionalPanel(
                                          ns = ns,
                                          condition = "input.popuParameters == 'Wilcoxon rank sum test' && input.sidebysidewRankQQ == 1",
                                          titlePanel("Q-Q Plots for Sample 1 and Sample 2"),
                                          br(),
                                          plotOptionsMenuUI(
                                            id = ns("sidebysidewRankQQ"),
                                            plotType = "QQ Plot",
                                            title = "Q-Q Plots"),
                                          plotOutput(ns("sidebysidewRankQQ")),
                                          br(), br()
                                        ))
                                    ),
                                    tabPanel(
                                      id    = ns("wRankSumData"),
                                      title = "Uploaded Data",
                                      
                                      uiOutput(ns("renderWRankSumMeansData"))
                                    ),
                                  ), # Uploaded Data
                                ), # Wilcoxon rank sum Tabs whole
                                ### ------------ Two Pop Var ------------------------------------------
                                
                                conditionalPanel(
                                  ns = ns,
                                  condition = "input.popuParameters == 'Two Population Variances'",
                                  
                                  navbarPage(
                                    id = ns("twoPopVarTabset"),
                                    selected = "Analysis",
                                    title = NULL,
                                    
                                    tabPanel(
                                      id = ns("twoPopVar"),
                                      title = "Analysis",
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Confidence Interval'",
                                        
                                        titlePanel(tags$u("Confidence Interval")),
                                        br(),
                                        uiOutput(ns('twoPopVarCI')),
                                        br(),
                                        
                                      ), # CI
                                      
                                      conditionalPanel(
                                        ns = ns,
                                        condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                        
                                        titlePanel(tags$u("Hypothesis Test")),
                                        br(),
                                        uiOutput(ns('twoPopVarHT')),
                                        br(),
                                        
                                      ) # HT
                                    ))), # Two Pop Var
              ), # "input.siMethod == '2'"
              
              ### ------------ Multiple Samples ------------------------------------
              #### ----------- ANOVA -----------------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.siMethod == 'Multiple' && input.multipleMethodChoice == 'anova'",
                
                navbarPage(
                  id       = ns("anovaTabset"),
                  selected = "Analysis",
                  title = NULL,
                  
                  tabPanel(
                    id    = ns("anova"),
                    title = "Analysis",
                    
                    titlePanel("One-way Analysis of Variance (ANOVA)"),
                    hr(),
                    br(),
                    uiOutput(ns("anovaOutput")),
                    br(),
                    br(),
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.anovaOptions.indexOf('posthoc') > -1",
                      
                      uiOutput(ns('anovaPosthocAnalysis'))
                    )
                  ),
                  
                  tabPanel(
                    title = "Graphs",
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.anovaGraphs.indexOf('Side-by-side Boxplot') > -1",
                      
                      titlePanel("Side-by-side Boxplot"),
                      br(),
                      br(),
                      plotOptionsMenuUI(
                        id       = ns("anovaBoxplot"),
                        plotType = "Boxplot",
                        title    = "Side-by-Side Boxplot"),
                      uiOutput(ns("renderAnovaBoxplot"))
                    ),
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.anovaGraphs.indexOf('Histogram of Residuals') > -1",
                      
                      titlePanel("Histogram of Residuals"),
                      br(),
                      br(),
                      plotOptionsMenuUI(
                        id    = ns("anovaHistogram"),
                        title = "Histogram of Residuals",
                        xlab  = "Residuals",
                        ylab  = "Frequency"),
                      uiOutput(ns("renderAnovaHistogram"))
                    ),
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.anovaGraphs.indexOf('QQ Plot of Residuals') > -1",
                      
                      titlePanel("QQ Plot of Residuals"),
                      br(),
                      br(),
                      plotOptionsMenuUI(
                        id     = ns("anovaQQplot"),
                        title  = "QQ Plot of Residuals",
                        xlab   = "Normal Quantiles",
                        ylab   = "Residuals",
                        colour = "#0F3345"),
                      uiOutput(ns("renderAnovaQQplot"))
                    ),
                    
                    conditionalPanel(
                      ns = ns,
                      condition = "input.anovaGraphs.indexOf('Plot Group Means') > -1",
                      
                      titlePanel("Group Means"),
                      br(),
                      br(),
                      plotOptionsMenuUI(
                        id     = ns("anovaMeanPlot"),
                        title  = "Group Means",
                        xlab   = "Group",
                        ylab   = "Mean",
                        colour = "#0F3345"),
                      uiOutput(ns("renderAnovaMeanPlot"))
                    )
                  ),
                  
                  tabPanel(
                    id    = ns("anovaData"),
                    title = "Uploaded Data",
                    
                    uiOutput(ns("renderAnovaDataView"))
                  )
                ) #anovaTabset tabsetPanel
              ), #Multiple Samples (ANOVA)
              
              #### ------------ Kruskal-Wallis ------------------------------------
              
              
              conditionalPanel(
                ns = ns,
                condition = "input.siMethod == 'Multiple' && input.multipleMethodChoice == 'kw'",
                
                navbarPage(
                  id = ns("kwTabset"),
                  selected = "Analysis",
                  title = NULL,
                  
                  tabPanel(
                    id    = ns("kw"),
                    title = "Analysis",
                    
                    titlePanel("Hypothesis Test"),
                    br(),
                    uiOutput(ns('kwHT')),
                    br(),
                    plotOutput(ns('kruskalWallisPlot'), width = "50%", height = "400px"),
                    br(),
                    uiOutput(ns('kwConclusionOutput'))
                  ),
                  
                  tabPanel(
                    id    = ns("kwRM"),
                    title = "Data table with Ranks",
                    
                    DTOutput("renderrankedmean"),
                    
                    uiOutput(ns("renderKWRM"))
                  ),
                  
                  tabPanel(
                    id    = ns("kwData"),
                    title = "Uploaded Data",
                    
                    uiOutput(ns("renderKWData"))
                  )
                  
                ), #tabsetPanel 
              ),
              
              ### ------------ Categorical Samples (Chi-Square) ----------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.siMethod == 'Categorical'",
                
                #### ---------------- Chi-Square Test for Independence -----------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.chisquareMethod == 'Chi-Square'",
                  
                  titlePanel("Chi-Square Test for Independence"),
                  hr(),
                  br(),
                  h4("Observed Frequencies"),
                  br(),
                  uiOutput(ns("renderChiSqObs")),
                  br(),
                  br(),
                  h4("Expected Frequencies"),
                  br(),
                  uiOutput(ns("renderChiSqExp")),
                  br(),
                  br(),
                  h4("Calculation of the \\( \\chi^2 \\) statistic value"),
                  br(),
                  uiOutput(ns("renderChiSqResults")),
                  br(),
                  hr(),
                  br(),
                  uiOutput(ns("chiSqTest")),
                  br(),
                  br(),
                ),
                
                #### ---------------- Fisher's Exact Test ------------------------------------
                conditionalPanel(
                  ns = ns,
                  condition = "input.chisquareMethod == 'Fisher'",
                  
                  titlePanel("Fisher's Exact Test"),
                  hr(),
                  br(),
                  h4("Observed Frequencies"),
                  uiOutput(ns("renderFishersObs")),
                  br(),
                  br(),
                  hr(),
                  br(),
                  h4("Hypothesis Test"),
                  uiOutput(ns("fishersTest")),
                  br(),
                  br(),
                ) #Fisher
              ) # input.siMethod == 'Categorical'
          ) #inferenceData
        ), #inferenceMP
        
        uiOutput(ns("kwRawContainer"))
        
      ), #mainPanel
    ) #sidebarLayout
  ) # UI tagList
}

# =========================================================================== #
# ---- Server Components ----------------------------------------------------
# =========================================================================== #

statInfrServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    useShinyjs()
    ## hypothesisTestingRadioButtonCssSelector <-
    ##   "#si-inferenceType > div:nth-child(2) > label:nth-child(2) > input:nth-child(1)"
    ## observe({
    ##   if (input$siMethod == 1 && input$popuParameter == "Population Standard Deviation") {
    ##     updateRadioButtons(session = session,
    ##                        inputId = "inferenceType",
    ##                        selected = "Confidence Interval")
    ##     disable(selector = hypothesisTestingRadioButtonCssSelector)
    ##   } else {
    ##     enable(selector = hypothesisTestingRadioButtonCssSelector)
    ##   }
    ## }) |> bindEvent(input$siMethod, input$popuParameter)
    
    
    #  ========================================================================= #
    ## -------- Data Validation ------------------------------------------------
    #  ========================================================================= #
    si_iv <- InputValidator$new()
    onemean_iv <- InputValidator$new()
    onemeansdknown_iv <- InputValidator$new()
    onemeansdunk_iv <- InputValidator$new()
    onemeanraw_iv <- InputValidator$new()
    onemeanht_iv <- InputValidator$new()
    onemeanupload_iv <- InputValidator$new()
    onemeanuploadvar_iv <- InputValidator$new()
    onemeanuploadsd_iv <- InputValidator$new()
    indmeanssumm_iv <- InputValidator$new()
    indmeansraw_iv <- InputValidator$new()
    indmeansupload_iv <- InputValidator$new()
    indmeansuploadvar_iv <- InputValidator$new()
    indmeanssdknown_iv <- InputValidator$new()
    indmeanssdunk_iv <- InputValidator$new()
    indmeansrawsd_iv <- InputValidator$new()
    indmeansrawsdunk_iv <- InputValidator$new()
    indmeansuploadsd_iv <- InputValidator$new()
    indmeansmunaught_iv <- InputValidator$new()
    wilcoxonUpload_iv <- InputValidator$new()
    wilcoxonraw_iv <- InputValidator$new()
    wilcoxonRanksuploadvars_iv <- InputValidator$new()
    wRankSumrawsd_iv <- InputValidator$new()
    depmeansraw_iv <- InputValidator$new()
    depmeansupload_iv <- InputValidator$new()
    depmeansuploadvars_iv <- InputValidator$new()
    depmeansrawsd_iv <- InputValidator$new()
    depmeansmunaught_iv <- InputValidator$new()
    oneSD_iv <- InputValidator$new()
    oneSDht_iv <- InputValidator$new()
    oneprop_iv <- InputValidator$new()
    onepropht_iv <- InputValidator$new()
    twoprop_iv <- InputValidator$new()
    twopropht_iv <- InputValidator$new()
    twopropdiffnaught_iv <- InputValidator$new()
    twopopvarsum_iv <- InputValidator$new()
    twopopvar_iv <- InputValidator$new()
    twopopvarraw_iv <- InputValidator$new()
    kwupload_iv <- InputValidator$new()
    kwmulti_iv <- InputValidator$new()
    kwstacked_iv <- InputValidator$new()
    anovaupload_iv <- InputValidator$new()
    anovamulti_iv <- InputValidator$new()
    anovastacked_iv <- InputValidator$new()
    chiSq2x2_iv <- InputValidator$new()
    chiSq2x3_iv <- InputValidator$new()
    chiSq3x2_iv <- InputValidator$new()
    chiSq3x3_iv <- InputValidator$new()
    
    ### ------------ Rules -------------------------------------------------------
    
    # sampleSize
    onemean_iv$add_rule("sampleSize", sv_required())
    onemean_iv$add_rule("sampleSize", sv_integer())
    onemean_iv$add_rule("sampleSize", sv_gt(1))
    
    # sampleMean
    onemean_iv$add_rule("sampleMean", sv_required())
    
    # sample1
    onemeanraw_iv$add_rule("sample1", sv_required())
    onemeanraw_iv$add_rule("sample1", sv_regex("^( )*(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be numeric values separated by a comma (ie: 2,3,4)"))
    # raw data, SD unknown
    onemeanraw_iv$add_rule("sample1", ~ {
      if (input$sigmaKnownRaw == "rawUnknown" && input$inferenceType == 'Hypothesis Testing' && (sd(createNumLst(input$sample1)) == 0)) {
        "No variance in sample data"
      }
    })
    
    # One Mean Upload Data
    onemeanupload_iv$add_rule("oneMeanUserData", sv_required())
    onemeanupload_iv$add_rule("oneMeanUserData", ~ if(is.null(fileInputs$oneMeanStatus) || fileInputs$oneMeanStatus == 'reset') "Required")
    onemeanupload_iv$add_rule("oneMeanUserData", ~ if(!(tolower(tools::file_ext(input$oneMeanUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    onemeanupload_iv$add_rule("oneMeanUserData", ~ if(nrow(OneMeanUploadData()) == 0) "File is empty")
    onemeanupload_iv$add_rule("oneMeanUserData", ~ if(nrow(OneMeanUploadData()) < 3) "Samples must include at least 2 observations")

    
    # popuSD
    onemeansdknown_iv$add_rule("popuSD", sv_required())
    onemeansdknown_iv$add_rule("popuSD", sv_gt(0))
    
    # popuSDRaw
    onemeanraw_iv$add_rule("popuSDRaw", ~ {
      if (input$sigmaKnownRaw == "rawKnown" && is.na(input$popuSDRaw)) {
        "Required"
      } else if (input$sigmaKnownRaw == "rawKnown" && input$popuSDRaw <= 0) {
        "Must be greater than 0"
      }
    })
    
    # popuSDUpload
    onemeanuploadsd_iv$add_rule("popuSDUpload", sv_required())
    onemeanuploadsd_iv$add_rule("popuSDUpload", sv_gt(0))
    
    # oneMeanVariable
    onemeanuploadvar_iv$add_rule("oneMeanVariable", sv_required())
    onemeanuploadvar_iv$add_rule("oneMeanVariable", ~ {
      data <- OneMeanUploadData()
      col  <- input$oneMeanVariable
      if (is.null(col) || col == "" || !(col %in% names(data))) return(NULL)
      if (input$sigmaKnownUpload == "Unknown" &&
          sd(data[[col]], na.rm = TRUE) == 0 && input$inferenceType == 'Hypothesis Testing') {
        "No variance in selected column"
      }
    })
    onemeanuploadvar_iv$add_rule("oneMeanVariable", ~ {
      if (checkNumeric(OneMeanUploadData(), input$oneMeanVariable)) {
        "Selected column contains non-numeric data."
      }
    })
    
    # sampSD
    onemeansdunk_iv$add_rule("sampSD", sv_required())
    onemeansdunk_iv$add_rule("sampSD", sv_gt(0))
    
    # sampleSize1
    indmeanssumm_iv$add_rule("sampleSize1", sv_required())
    indmeanssumm_iv$add_rule("sampleSize1", sv_integer())
    indmeanssumm_iv$add_rule("sampleSize1", sv_gt(1))
    
    # sampleMean1
    indmeanssumm_iv$add_rule("sampleMean1", sv_required())
    
    # sampleSize2
    indmeanssumm_iv$add_rule("sampleSize2", sv_required())
    indmeanssumm_iv$add_rule("sampleSize2", sv_integer())
    indmeanssumm_iv$add_rule("sampleSize2", sv_gt(1))
    
    # sampleMean2
    indmeanssumm_iv$add_rule("sampleMean2", sv_required())
    
    # popuSD1
    indmeanssdknown_iv$add_rule("popuSD1", sv_required())
    indmeanssdknown_iv$add_rule("popuSD1", sv_gt(0))
    
    # popuSD2
    indmeanssdknown_iv$add_rule("popuSD2", sv_required())
    indmeanssdknown_iv$add_rule("popuSD2", sv_gt(0))
    
    # sampSD1
    indmeanssdunk_iv$add_rule("sampSD1", sv_required())
    indmeanssdunk_iv$add_rule("sampSD1", sv_gt(0))
    
    # sampSD2
    indmeanssdunk_iv$add_rule("sampSD2", sv_required())
    indmeanssdunk_iv$add_rule("sampSD2", sv_gt(0))
    
    # raw_sample1
    indmeansraw_iv$add_rule("raw_sample1", sv_required())
    indmeansraw_iv$add_rule("raw_sample1", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)"))
    
    # raw_sample2
    indmeansraw_iv$add_rule("raw_sample2", sv_required())
    indmeansraw_iv$add_rule("raw_sample2", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."))
    
    indmeansrawsd_iv$add_rule("popuSDRaw1", sv_required())
    indmeansrawsd_iv$add_rule("popuSDRaw1", sv_gt(0))
    
    indmeansrawsd_iv$add_rule("popuSDRaw2", sv_required())
    indmeansrawsd_iv$add_rule("popuSDRaw2", sv_gt(0))
    
    indmeansrawsdunk_iv$add_rule("raw_sample1", ~ if(sd(createNumLst(input$raw_sample1)) == 0 
                                                     && sd(createNumLst(input$raw_sample2)) == 0 
                                                     && input$inferenceType2 == 'Hypothesis Testing') "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2.")
    indmeansrawsdunk_iv$add_rule("raw_sample2", ~ if(sd(createNumLst(input$raw_sample1)) == 0 
                                                     && sd(createNumLst(input$raw_sample2)) == 0 
                                                     && input$inferenceType2 == 'Hypothesis Testing') "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2.")
    
    #indMeansUserData
    indmeansupload_iv$add_rule("indMeansUserData", sv_required())
    indmeansupload_iv$add_rule("indMeansUserData", ~ if(is.null(fileInputs$indMeansStatus) || fileInputs$indMeansStatus == 'reset') "Required")
    indmeansupload_iv$add_rule("indMeansUserData", ~ if(!(tolower(tools::file_ext(input$indMeansUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    indmeansupload_iv$add_rule("indMeansUserData", ~ if(nrow(IndMeansUploadData()) == 0) "File is empty.")
    indmeansupload_iv$add_rule("indMeansUserData", ~ if(ncol(IndMeansUploadData()) < 2) "File must contain at least 2 distinct samples to choose from for analysis.")
    indmeansupload_iv$add_rule("indMeansUserData", ~ if(nrow(IndMeansUploadData()) < 3) "Samples must include at least 2 observations.")
    
    indmeansuploadsd_iv$add_rule("popuSDUpload1", sv_required())
    indmeansuploadsd_iv$add_rule("popuSDUpload1", sv_gt(0))
    
    indmeansuploadsd_iv$add_rule("popuSDUpload2", sv_required())
    indmeansuploadsd_iv$add_rule("popuSDUpload2", sv_gt(0))
    
    indmeansuploadvar_iv$add_rule("indMeansUplSample1", sv_required())
    indmeansuploadvar_iv$add_rule("indMeansUplSample2", sv_required())
    indmeansuploadvar_iv$add_rule("indMeansUplSample1", ~ {
      d <- IndMeansUploadData()
      c1 <- input$indMeansUplSample1
      c2 <- input$indMeansUplSample2
      
      if (input$bothsigmaKnownUpload == "bothUnknown" && input$inferenceType2 == 'Hypothesis Testing') {
        if (c1 %in% names(d) && c2 %in% names(d)) {
          if (sd(d[[c1]], na.rm = TRUE) == 0 && sd(d[[c2]], na.rm = TRUE) == 0) {
            return("At least 1 of the selected columns must have variance.")
          }
        }
      }
    })
    
    indmeansuploadvar_iv$add_rule("indMeansUplSample2", ~ {
      d <- IndMeansUploadData()
      c1 <- input$indMeansUplSample1
      c2 <- input$indMeansUplSample2
      
      if (input$bothsigmaKnownUpload == "bothUnknown" && input$inferenceType2 == 'Hypothesis Testing') {
        if (c1 %in% names(d) && c2 %in% names(d)) {
          if (sd(d[[c1]], na.rm = TRUE) == 0 && sd(d[[c2]], na.rm = TRUE) == 0) {
            return("At least 1 of the selected columns must have variance.")
          }
        }
      }
    })
    
    indmeansuploadvar_iv$add_rule("indMeansUplSample1", ~ {
      if (checkNumeric(IndMeansUploadData(), input$indMeansUplSample1)) {
        "Selected column contains non-numeric data."
      }
    })
    
    indmeansuploadvar_iv$add_rule("indMeansUplSample2", ~ {
      if (checkNumeric(IndMeansUploadData(), input$indMeansUplSample2)) {
        "Selected column contains non-numeric data."
      }
    })
    
    wilcoxonUpload_iv$add_rule("wilcoxonUpl", sv_required())
    wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if(is.null(fileInputs$rankSumStatus) || fileInputs$rankSumStatus == 'reset') "Required")
    wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if(!(tolower(tools::file_ext(input$wilcoxonUpl$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if(nrow(WilcoxonUploadData()) == 0) "File is empty.")
    wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if(ncol(WilcoxonUploadData()) < 2) "File must contain at least 2 distinct samples to choose from for analysis.")
    wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if(nrow(WilcoxonUploadData()) < 3) "Samples must include at least 2 observations.")
    wilcoxonraw_iv$add_rule("rankSumRaw1", sv_required())
    wilcoxonraw_iv$add_rule("rankSumRaw1", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)"))
    wilcoxonraw_iv$add_rule("rankSumRaw2", sv_required())
    wilcoxonraw_iv$add_rule("rankSumRaw2", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."))
    wilcoxonRanksuploadvars_iv$add_rule("wilcoxonUpl1", sv_required())
    wilcoxonRanksuploadvars_iv$add_rule("wilcoxonUpl2", sv_required())
    wRankSumrawsd_iv$add_rule("rankSumRaw2", ~ {
      data <- GetwRankSumMeansData()
      if(is.null(data) || 
         length(unique(data$samp1)) <= 1 || 
         length(unique(data$samp2)) <= 1) {
        "Variance required in Sample 1 and Sample 2 data for hypothesis testing."
      }
    })
    wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ {
      data <- WilcoxonUploadData()
      if (!is.null(data) && nrow(data) > 0) {
        if (!all(sapply(data, is.numeric))) {
          "Uploaded data contains non-numeric values. Please ensure all columns are numeric."
        }
      }
    })
    # ind means Mu Naught
    indmeansmunaught_iv$add_rule("indMeansMuNaught", sv_required())
    
    # before
    depmeansraw_iv$add_rule("before", sv_required())
    depmeansraw_iv$add_rule("before", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)"))
    
    # after
    depmeansraw_iv$add_rule("after", sv_required())
    depmeansraw_iv$add_rule("after", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                              "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."))
    
    
    depmeansraw_iv$add_rule("before", ~ if(length(createNumLst(input$before)) != length(createNumLst(input$after))) "Sample 1 and Sample 2 must have the same number of observations.")
    depmeansraw_iv$add_rule("after", ~ if(length(createNumLst(input$before)) != length(createNumLst(input$after))) "Sample 1 and Sample 2 must have the same number of observations.")
    
    depmeansupload_iv$add_rule("depMeansUserData", sv_required())
    depmeansupload_iv$add_rule("depMeansUserData", ~ if(is.null(fileInputs$depMeansStatus) || fileInputs$depMeansStatus == 'reset') "Required")
    depmeansupload_iv$add_rule("depMeansUserData", ~ if(!(tolower(tools::file_ext(input$depMeansUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    depmeansupload_iv$add_rule("depMeansUserData", ~ if(nrow(DepMeansUploadData()) == 0) "File is empty.")
    depmeansupload_iv$add_rule("depMeansUserData", ~ if(ncol(DepMeansUploadData()) < 2) "File must contain at least 2 distinct 'Before' and 'After' sets of data to choose from for analysis.")
    depmeansupload_iv$add_rule("depMeansUserData", ~ if(nrow(DepMeansUploadData()) < 4) "Samples must include at least 3 observations.")
    depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ {
      if (input$depMeansUplSample1 != "" &&
          input$depMeansUplSample2 != "" &&
          !checkNumeric(DepMeansUploadData(), input$depMeansUplSample1) &&
          !checkNumeric(DepMeansUploadData(), input$depMeansUplSample2) &&
          (input$depMeansUplSample1 == input$depMeansUplSample2 ||
           GetDepMeansData()$sd == 0)) {
        "'Sample 1' and 'Sample 2' data are the same. Standard deviation of the difference is zero."
      }
    })
    
    depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ {
      if (input$depMeansUplSample1 != "" &&
          input$depMeansUplSample2 != "" &&
          !checkNumeric(DepMeansUploadData(), input$depMeansUplSample1) &&
          !checkNumeric(DepMeansUploadData(), input$depMeansUplSample2) &&
          (input$depMeansUplSample1 == input$depMeansUplSample2 ||
           GetDepMeansData()$sd == 0)) {
        "'Sample 1' and 'Sample 2' data are the same. Standard deviation of the difference is zero."
      }
    })
    
    depmeansuploadvars_iv$add_rule("depMeansUplSample1", sv_required())
    depmeansuploadvars_iv$add_rule("depMeansUplSample2", sv_required())
    depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ if(CheckDepUploadSamples() != 0) "Sample 1 and Sample 2 must have the same number of observations.")
    depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ if(CheckDepUploadSamples() != 0) "Sample 1 and Sample 2 must have the same number of observations.")
    depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ {
      if (checkNumeric(DepMeansUploadData(), input$depMeansUplSample1)) {
        "Selected column contains non-numeric data."
      }
    })
    depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ {
      if (checkNumeric(DepMeansUploadData(), input$depMeansUplSample2)) {
        "Selected column contains non-numeric data."
      }
    })
    
    depmeansraw_iv$add_rule("before", ~ if(GetDepMeansData()$sd == 0) "'Sample 1’ and 'Sample 2' data are the same. Standard deviation of the difference is zero.")
    depmeansraw_iv$add_rule("after", ~ if(GetDepMeansData()$sd == 0) "'Sample 1’ and 'Sample 2' data are the same. Standard deviation of the difference is zero.")
    
    depmeansmunaught_iv$add_rule("depMeansMuNaught", sv_required())
    
    # sample standard deviation
    oneSD_iv$add_rule("SSDSampleSize", sv_required())
    oneSD_iv$add_rule("SSDSampleSize", sv_integer())
    oneSD_iv$add_rule("SSDSampleSize", sv_gt(1))
    oneSD_iv$add_rule("SSDStdDev", sv_required())
    oneSD_iv$add_rule("SSDStdDev", sv_gt(0))
    oneSDht_iv$add_rule("hypStdDeviation", sv_required())
    oneSDht_iv$add_rule("hypStdDeviation", sv_gt(0))
    
    # numSuccessesProportion
    oneprop_iv$add_rule("numSuccesses", sv_required(message = "Numeric value required."))
    oneprop_iv$add_rule("numSuccesses", sv_integer())
    oneprop_iv$add_rule("numSuccesses", sv_gte(0))
    
    # x1
    twoprop_iv$add_rule("numSuccesses1", sv_required())
    twoprop_iv$add_rule("numSuccesses1", sv_integer())
    twoprop_iv$add_rule("numSuccesses1", sv_gte(0))
    twopropht_iv$add_rule("numSuccesses1", ~ if(checkTwoProp() == 0) "At least one of (x1) and (x2) must be greater than 0.")
    
    # x2
    twoprop_iv$add_rule("numSuccesses2", sv_required())
    twoprop_iv$add_rule("numSuccesses2", sv_integer())
    twoprop_iv$add_rule("numSuccesses2", sv_gte(0))
    twopropht_iv$add_rule("numSuccesses2", ~ if(checkTwoProp() == 0) "At least one of (x1) and (x2) must be greater than 0.")
    twopropht_iv$add_rule("numSuccesses1", ~ {
      if (input$numSuccesses1 == input$numTrials1 &&
          input$numSuccesses2 == input$numTrials2) {
        "Both sample proportions are equal to 1."
      }
    })
    twopropht_iv$add_rule("numSuccesses2", ~ {
      if (input$numSuccesses1 == input$numTrials1 &&
          input$numSuccesses2 == input$numTrials2) {
        "Both sample proportions are equal to 1."
      }
    })
    
    # diff naught
    twopropdiffnaught_iv$add_rule("propDiffNaught", sv_required())
    twopropdiffnaught_iv$add_rule("propDiffNaught", sv_gte(-1, message = "Value must be between -1 and 1 (inclusive)."))
    twopropdiffnaught_iv$add_rule("propDiffNaught", sv_lte(1, message = "Value must be between -1 and 1 (inclusive)."))
    
    # SDSampleSize1
    twopopvarsum_iv$add_rule("SDSampleSize1", sv_required())
    twopopvarsum_iv$add_rule("SDSampleSize1", sv_integer())
    twopopvarsum_iv$add_rule("SDSampleSize1", sv_gt(1))
    
    # SDSampleSize2
    twopopvarsum_iv$add_rule("SDSampleSize2", sv_required())
    twopopvarsum_iv$add_rule("SDSampleSize2", sv_integer())
    twopopvarsum_iv$add_rule("SDSampleSize2", sv_gt(1))
    
    # stdDev1
    twopopvarsum_iv$add_rule("stdDev1", sv_required())
    twopopvarsum_iv$add_rule("stdDev1", sv_gt(0))
    
    # stdDev2
    twopopvarsum_iv$add_rule("stdDev2", sv_required())
    twopopvarsum_iv$add_rule("stdDev2", sv_gt(0))
    
    # Two Std Dev n1
    twopopvar_iv$add_rule("n1", sv_required())
    twopopvar_iv$add_rule("n1", sv_integer())
    twopopvar_iv$add_rule("n1", sv_gt(1))
    
    # Two Std Dev n2
    twopopvar_iv$add_rule("n2", sv_required())
    twopopvar_iv$add_rule("n2", sv_integer())
    twopopvar_iv$add_rule("n2", sv_gt(1))
    
    # Two Std Dev s1^2
    twopopvar_iv$add_rule("s1sq", sv_required())
    twopopvar_iv$add_rule("s1sq", sv_gt(0))
    
    # Two Std Dev s2^2
    twopopvar_iv$add_rule("s2sq", sv_required())
    twopopvar_iv$add_rule("s2sq", sv_gt(0))
    
    # raw group 1
    twopopvarraw_iv$add_rule("rawSamp1SD", sv_required())
    twopopvarraw_iv$add_rule("rawSamp1SD", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."))
    twopopvarraw_iv$add_rule("rawSamp1SD", ~ if (sd(createNumLst(input$rawSamp1SD)) == 0) "No variance in sample data")
    
    # raw group 2
    twopopvarraw_iv$add_rule("rawSamp2SD", sv_required())
    twopopvarraw_iv$add_rule("rawSamp2SD", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."))
    twopopvarraw_iv$add_rule("rawSamp2SD", ~ if (sd(createNumLst(input$rawSamp2SD)) == 0) "No variance in sample data")
    
    
    # numTrialsProportion
    oneprop_iv$add_rule("numTrials", sv_required(message = "Numeric value required."))
    oneprop_iv$add_rule("numTrials", sv_integer())
    oneprop_iv$add_rule("numTrials", sv_gt(0))
    
    # n1
    twoprop_iv$add_rule("numTrials1", sv_required())
    twoprop_iv$add_rule("numTrials1", sv_integer())
    twoprop_iv$add_rule("numTrials1", sv_gt(0))
    
    # n2
    twoprop_iv$add_rule("numTrials2", sv_required())
    twoprop_iv$add_rule("numTrials2", sv_integer())
    twoprop_iv$add_rule("numTrials2", sv_gt(0))
    
    # hypMean
    onemeanht_iv$add_rule("hypMean", sv_required())
    
    # hypProportion
    onepropht_iv$add_rule("hypProportion", sv_required())
    onepropht_iv$add_rule("hypProportion", sv_gt(0))
    onepropht_iv$add_rule("hypProportion", sv_lt(1))
    
    # Anova
    anovaupload_iv$add_rule("anovaUserData", sv_required())
    anovaupload_iv$add_rule("anovaUserData", ~ if(is.null(fileInputs$anovaStatus) || fileInputs$anovaStatus == 'reset') "Required")
    anovaupload_iv$add_rule("anovaUserData", ~ if(!(tolower(tools::file_ext(input$anovaUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    anovaupload_iv$add_rule("anovaUserData", ~ if(ncol(anovaUploadData()) < 2) "Data must include at least two columns")
    # anovaupload_iv$add_rule("anovaUserData", ~ if(nrow(anovaUploadData()) < 2) "")
    
    # anovamulti_iv$add_rule("anovaMultiColumns", sv_required())
    anovamulti_iv$add_rule("anovaMultiColumns", ~ if(length(input$anovaMultiColumns) < 2) "Select at least two columns")
    anovamulti_iv$add_rule("anovaMultiColumns", ~ {
      if (checkNumeric(anovaUploadData(), input$anovaMultiColumns)) {
        "Selected column(s) contain non-numeric data."
      }
    })
    
    anovastacked_iv$add_rule("anovaResponse", sv_required())
    anovastacked_iv$add_rule("anovaFactors", sv_required())
    anovastacked_iv$add_rule("anovaResponse", ~ if(anovaStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
    anovastacked_iv$add_rule("anovaFactors", ~ if(anovaStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
    anovastacked_iv$add_rule("anovaResponse", ~ {
      if (checkNumeric(anovaUploadData(), input$anovaResponse)) {
        "Response variable must be numeric."
      }
    })
    
    # Kruskal-Wallis
    kwupload_iv$add_rule("kwUserData", sv_required())
    kwupload_iv$add_rule("kwUserData", ~ if(is.null(fileInputs$kwStatus) || fileInputs$kwStatus == 'reset') "Required")
    kwupload_iv$add_rule("kwUserData", ~ if(!(tolower(tools::file_ext(input$kwUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
    kwupload_iv$add_rule("kwUserData", ~ if(ncol(kwUploadData()) < 2) "Data must include at least two columns")
    
    #kwupload_iv$add_rule("kwUserData", ~ {
    #  df <- kwUploadData()
    #  if (!is.null(df)) {
    #    for (col in names(df)) {
    #      if (length(unique(df[[col]])) == 1) {
    #        return(paste0("Column '", col, "' has the same value for all rows."))
    #      }
    #    }
    #  }
    #})
    
    
    kwmulti_iv$add_rule("kwMultiColumns", ~ if(length(input$kwMultiColumns) < 2) "Select at least two columns")
    kwmulti_iv$add_rule("kwMultiColumns", ~ {
      if (checkNumeric(kwUploadData(), input$kwMultiColumns)) {
        "Selected column(s) contain non-numeric data."
      }
    })
    
    kwstacked_iv$add_rule("kwResponse", sv_required())
    kwstacked_iv$add_rule("kwFactors", sv_required())
    kwstacked_iv$add_rule("kwResponse", ~ if(kwStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
    kwstacked_iv$add_rule("kwFactors", ~ if(kwStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
    kwstacked_iv$add_rule("kwResponse", ~ {
      if (checkNumeric(kwUploadData(), input$kwResponse)) {
        "Response variable must be numeric."
      }
    })
    # Chi-Square
    ChiSqInputRules <- function(iv, inputID) {
      iv$add_rule(inputID, sv_required())
      iv$add_rule(inputID, ~ if(any(is.na(chiSqActiveData()$numeric))) "Fields must be positive integers.")
      iv$add_rule(inputID, ~ if(any(chiSqActiveData()$numeric < 0)) "Fields must be positive integers.")
      iv$add_rule(inputID, ~ if(any(chiSqActiveData()$numeric %% 1 != 0)) "Fields must be positive integers.")
      iv$add_rule(inputID, ~ if(all(chiSqActiveData()$numeric == 0)) "All cell values cannot be equal to zero.")
      iv$add_rule(inputID, ~ if(any(chiSqTotaled()[,"Total"] == 0)) "Row Totals must be greater than zero.")
      iv$add_rule(inputID, ~ if(any(chiSqTotaled()["Total",] == 0)) "Column Totals must be greater than zero.")
    }
    
    # 2 x 2
    ChiSqInputRules(chiSq2x2_iv, "chiSqInput2x2")
    
    # 2 x 3
    ChiSqInputRules(chiSq2x3_iv, "chiSqInput2x3")
    
    # 3 x 2
    ChiSqInputRules(chiSq3x2_iv, "chiSqInput3x2")
    
    # 3 x 3
    ChiSqInputRules(chiSq3x3_iv, "chiSqInput3x3")
    
    ### ------------ Conditions --------------------------------------------------
    onemean_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                    input$popuParameter == 'Population Mean' &&
                                    input$dataAvailability == 'Summarized Data'))
    
    onemeansdknown_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                           input$popuParameter == 'Population Mean' &&
                                           input$dataAvailability == 'Summarized Data' &&
                                           input$sigmaKnown == 'Known'))
    
    onemeansdunk_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                         input$popuParameter == 'Population Mean' &&
                                         input$dataAvailability == 'Summarized Data' &&
                                         input$sigmaKnown == 'Unknown'))
    
    onemeanraw_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                       input$popuParameter == 'Population Mean' &&
                                       input$dataAvailability == 'Enter Raw Data'))
    
    onemeanupload_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                          input$popuParameter == 'Population Mean' &&
                                          input$dataAvailability == 'Upload Data'))
    
    onemeanuploadvar_iv$condition(function() {isTRUE(input$siMethod == '1' &&
                                                       input$popuParameter == 'Population Mean' &&
                                                       input$dataAvailability == 'Upload Data' &&
                                                       onemeanupload_iv$is_valid()) })
    
    onemeanuploadsd_iv$condition(function() {isTRUE(input$siMethod == '1' &&
                                                      input$popuParameter == 'Population Mean' &&
                                                      input$dataAvailability == 'Upload Data' &&
                                                      input$sigmaKnownUpload == 'Known') })
    
    onemeanht_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                      input$popuParameter == 'Population Mean' &&
                                      input$inferenceType == 'Hypothesis Testing'))
    
    indmeanssumm_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                         input$popuParameters == 'Independent Population Means' &&
                                         input$dataAvailability2 == 'Summarized Data'))
    
    indmeansraw_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                        input$popuParameters == 'Independent Population Means' &&
                                        input$dataAvailability2 == 'Enter Raw Data'))
    
    indmeanssdknown_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                            input$popuParameters == 'Independent Population Means' &&
                                            input$dataAvailability2 == 'Summarized Data' &&
                                            input$bothsigmaKnown == 'bothKnown'))
    
    indmeanssdunk_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                          input$popuParameters == 'Independent Population Means' &&
                                          input$dataAvailability2 == 'Summarized Data' &&
                                          input$bothsigmaKnown == 'bothUnknown'))
    
    indmeansrawsd_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                          input$popuParameters == 'Independent Population Means' &&
                                          input$dataAvailability2 == 'Enter Raw Data' &&
                                          input$bothsigmaKnownRaw == 'bothKnown'))
    
    indmeansrawsdunk_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                             input$popuParameters == 'Independent Population Means' &&
                                             input$dataAvailability2 == 'Enter Raw Data' &&
                                             input$bothsigmaKnownRaw == 'bothUnknown' &&
                                             input$inferenceType2 == 'Hypothesis Testing' &&
                                             indmeansraw_iv$is_valid()))
    
    indmeansupload_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                           input$popuParameters == 'Independent Population Means' &&
                                           input$dataAvailability2 == 'Upload Data'))
    
    indmeansuploadvar_iv$condition(function() {isTRUE(input$siMethod == '2' &&
                                                        input$popuParameters == 'Independent Population Means' &&
                                                        input$dataAvailability2 == 'Upload Data' &&
                                                        indmeansupload_iv$is_valid()) })
    
    indmeansuploadsd_iv$condition(function() {isTRUE(input$siMethod == '2' &&
                                                       input$popuParameters == 'Independent Population Means' &&
                                                       input$dataAvailability2 == 'Upload Data' &&
                                                       input$bothsigmaKnownUpload == 'bothKnown') })
    
    
    wilcoxonraw_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                        input$popuParameters == 'Wilcoxon rank sum test' &&
                                        input$wilcoxonRankSumTestData == 'Enter Raw Data'))
    
    wilcoxonUpload_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                           input$popuParameters == 'Wilcoxon rank sum test' &&
                                           input$wilcoxonRankSumTestData == 'Upload Data'))
    
    wilcoxonRanksuploadvars_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                                    input$popuParameters == 'Wilcoxon rank sum test' &&
                                                    input$wilcoxonRankSumTestData == 'Upload Data' &&
                                                    wilcoxonUpload_iv$is_valid()))
    
    wRankSumrawsd_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                          input$popuParameters == 'Wilcoxon rank sum test' &&
                                          input$wilcoxonRankSumTestData == 'Enter Raw Data' &&
                                          wilcoxonraw_iv$is_valid()))
    
    indmeansmunaught_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                             input$popuParameters == 'Independent Population Means' &&
                                             input$inferenceType2 == 'Hypothesis Testing'))
    
    depmeansraw_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                        input$popuParameters == 'Dependent Population Means' &&
                                        input$dataTypeDependent == 'Enter Raw Data'))
    
    depmeansupload_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                           input$popuParameters == 'Dependent Population Means' &&
                                           input$dataTypeDependent == 'Upload Data'))
    
    depmeansuploadvars_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                               input$popuParameters == 'Dependent Population Means' &&
                                               input$dataTypeDependent == 'Upload Data' &&
                                               depmeansupload_iv$is_valid()))
    
    depmeansrawsd_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                          input$popuParameters == 'Dependent Population Means' &&
                                          input$dataTypeDependent == 'Enter Raw Data' &&
                                          depmeansraw_iv$is_valid()))
    
    depmeansmunaught_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                             input$popuParameters == 'Dependent Population Means' &&
                                             input$inferenceType2 == 'Hypothesis Testing'))
    
    oneSD_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                  input$popuParameter == 'Population Standard Deviation'))
    
    ## See: https://rstudio.github.io/shinyvalidate/reference/InputValidator.html#method-InputValidator-condition.
    oneSDht_iv$condition(function() {
      return(all(input$siMethod == '1',
                 input$popuParameter == 'Population Standard Deviation',
                 input$inferenceType == 'Hypothesis Testing'))
    })
    
    oneprop_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                    input$popuParameter == 'Population Proportion'))
    
    onepropht_iv$condition(~ isTRUE(input$siMethod == '1' &&
                                      input$popuParameter == 'Population Proportion' &&
                                      input$inferenceType == 'Hypothesis Testing'))
    
    twoprop_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                    input$popuParameters == 'Population Proportions'))
    
    twopropht_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                      input$popuParameters == 'Population Proportions' &&
                                      input$inferenceType2 == 'Hypothesis Testing'))
    
    twopopvarsum_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                         input$popuParameters == 'Two Population Variances' &&
                                         input$dataAvailability3 == 'Summary'))
    
    twopopvar_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                      input$popuParameters == 'Two Population Variances' &&
                                      input$dataAvailability3 == 'Variance'))
    
    twopopvarraw_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                         input$popuParameters == 'Two Population Variances' &&
                                         input$dataAvailability3 == 'Enter Raw Data'))
    
    twopropdiffnaught_iv$condition(~ isTRUE(input$siMethod == '2' &&
                                              input$popuParameters == 'Population Proportions' &&
                                              input$inferenceType2 == 'Hypothesis Testing'))
    
    kwupload_iv$condition(~ isTRUE(input$siMethod == 'Multiple' &&
                                     input$multipleMethodChoice == 'kw'))
    
    kwmulti_iv$condition(~ isTRUE(input$siMethod == 'Multiple' &&
                                    input$kwFormat == 'Multiple' &&
                                    input$multipleMethodChoice == 'kw' &&
                                    kwupload_iv$is_valid()))
    
    kwstacked_iv$condition(~ isTRUE(input$siMethod == 'Multiple' &&
                                      input$kwFormat == 'Stacked' &&
                                      input$multipleMethodChoice == 'kw' &&
                                      kwupload_iv$is_valid()))
    
    anovaupload_iv$condition(~ isTRUE(input$siMethod == 'Multiple' &&
                                        input$multipleMethodChoice == 'anova'))
    
    anovamulti_iv$condition(~ isTRUE(input$siMethod == 'Multiple' &&
                                       input$anovaFormat == 'Multiple' &&
                                       input$multipleMethodChoice == 'anova' &&
                                       anovaupload_iv$is_valid()))
    
    anovastacked_iv$condition(~ isTRUE(input$siMethod == 'Multiple' &&
                                         input$anovaFormat == 'Stacked' &&
                                         input$multipleMethodChoice == 'anova' &&
                                         anovaupload_iv$is_valid()))
    
    chiSq2x2_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                     input$chisquareDimension == '2 x 2'))
    
    chiSq2x3_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                     input$chisquareDimension == '2 x 3'))
    
    chiSq3x2_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                     input$chisquareDimension == '3 x 2'))
    
    chiSq3x3_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                     input$chisquareDimension == '3 x 3'))
    
    ### ------------ Dependencies ------------------------------------------------
    si_iv$add_validator(onemean_iv)
    si_iv$add_validator(onemeansdknown_iv)
    si_iv$add_validator(onemeansdunk_iv)
    si_iv$add_validator(onemeanraw_iv)
    si_iv$add_validator(onemeanht_iv)
    si_iv$add_validator(onemeanupload_iv)
    si_iv$add_validator(onemeanuploadvar_iv)
    si_iv$add_validator(onemeanuploadsd_iv)
    si_iv$add_validator(indmeanssumm_iv)
    si_iv$add_validator(indmeansraw_iv)
    si_iv$add_validator(indmeanssdknown_iv)
    si_iv$add_validator(indmeanssdunk_iv)
    si_iv$add_validator(indmeansrawsd_iv)
    si_iv$add_validator(indmeansrawsdunk_iv)
    si_iv$add_validator(indmeansupload_iv)
    si_iv$add_validator(indmeansuploadvar_iv)
    si_iv$add_validator(indmeansuploadsd_iv)
    si_iv$add_validator(wilcoxonUpload_iv)
    si_iv$add_validator(wilcoxonraw_iv)
    si_iv$add_validator(wilcoxonRanksuploadvars_iv)
    si_iv$add_validator(indmeansmunaught_iv)
    si_iv$add_validator(depmeansraw_iv)
    si_iv$add_validator(depmeansupload_iv)
    si_iv$add_validator(depmeansuploadvars_iv)
    si_iv$add_validator(depmeansmunaught_iv)
    si_iv$add_validator(oneSD_iv)
    si_iv$add_validator(oneSDht_iv)
    si_iv$add_validator(oneprop_iv)
    si_iv$add_validator(onepropht_iv)
    si_iv$add_validator(twoprop_iv)
    si_iv$add_validator(twopropht_iv)
    si_iv$add_validator(twopropdiffnaught_iv)
    si_iv$add_validator(twopopvarsum_iv)
    si_iv$add_validator(twopopvar_iv)
    si_iv$add_validator(twopopvarraw_iv)
    twoprop_iv$add_validator(twopropht_iv)
    si_iv$add_validator(kwupload_iv)
    si_iv$add_validator(kwmulti_iv)
    si_iv$add_validator(kwstacked_iv)
    si_iv$add_validator(anovaupload_iv)
    si_iv$add_validator(anovamulti_iv)
    si_iv$add_validator(anovastacked_iv)
    si_iv$add_validator(chiSq2x2_iv)
    si_iv$add_validator(chiSq2x3_iv)
    si_iv$add_validator(chiSq3x2_iv)
    si_iv$add_validator(chiSq3x3_iv)
    
    ### ------------ Activation --------------------------------------------------
    
    ## FIXME: If this validator object has been added to another validator
    ## object using InputValidator$add_validator, calls to enable() on this
    ## validator will be ignored. Don't rely on this behaviour, if undefined, or
    ## if out-of-sync with the documenation. When child validators exist they
    ## are enabled or disabled recursively when parent is enabled or disabled,
    ## and actions on the child should be ignored.
    si_iv$enable()
    onemean_iv$enable()
    onemeansdknown_iv$enable()
    onemeansdunk_iv$enable()
    onemeanraw_iv$enable()
    onemeanht_iv$enable()
    onemeanupload_iv$enable()
    onemeanuploadvar_iv$enable()
    onemeanuploadsd_iv$enable()
    indmeanssumm_iv$enable()
    indmeansraw_iv$enable()
    indmeanssdknown_iv$enable()
    indmeanssdunk_iv$enable()
    indmeansrawsd_iv$enable()
    indmeansrawsdunk_iv$enable()
    indmeansupload_iv$enable()
    indmeansuploadvar_iv$enable()
    indmeansuploadsd_iv$enable()
    wilcoxonraw_iv$enable()
    wilcoxonUpload_iv$enable()
    indmeansmunaught_iv$enable()
    depmeansraw_iv$enable
    depmeansupload_iv$enable()
    depmeansuploadvars_iv$enable()
    depmeansrawsd_iv$enable()
    depmeansmunaught_iv$enable()
    oneSD_iv$enable()
    oneSDht_iv$enable()
    oneprop_iv$enable()
    onepropht_iv$enable()
    twoprop_iv$enable()
    twopropht_iv$enable()
    twopropdiffnaught_iv$enable()
    twopopvarsum_iv$enable()
    twopopvar_iv$enable()
    twopopvarraw_iv$enable()
    kwupload_iv$enable()
    kwmulti_iv$enable()
    kwstacked_iv$enable()
    anovaupload_iv$enable()
    anovamulti_iv$enable()
    anovastacked_iv$enable()
    chiSq2x2_iv$enable()
    chiSq2x3_iv$enable()
    chiSq3x2_iv$enable()
    chiSq3x3_iv$enable()
    
    
    #  ========================================================================= #
    ## -------- Module Server Elements -----------------------------------------
    #  ========================================================================= #
    plotOptionsMenuServer("oneMeanBoxplot")
    plotOptionsMenuServer("indMeansBoxplot")
    plotOptionsMenuServer("indMeansQQPlot")
    plotOptionsMenuServer("depMeansQQPlot")
    plotOptionsMenuServer("sidebysidewRankSum")
    plotOptionsMenuServer("anovaBoxplot")
    plotOptionsMenuServer("anovaHistogram")
    plotOptionsMenuServer("anovaQQplot")
    plotOptionsMenuServer("anovaMeanPlot")
    
    
    #  ========================================================================= #
    ## -------- Functions ------------------------------------------------------
    #  ========================================================================= #
    
    checkNumeric <- function(data, cols) {
      if (is.null(cols) || length(cols) == 0 || !all(cols %in% colnames(data))) {
        return(FALSE)  # no invalid columns if nothing selected
      }
      
      dat <- as.data.frame(data)[, cols, drop = FALSE]
      invalid <- any(!sapply(dat, is.numeric))
      
      return(invalid)
    }
    
    getOutliers <- function(sample, sampleName, coef = 1.5) {
      x <- sort(sample)
      
      if(length(x) %% 2 != 0) {
        x_no_median <- x[-ceiling(length(x)/2)]
      } else {
        x_no_median <- x
      }
      
      mid <- length(x_no_median) / 2
      Q1 <- median(x_no_median[1:mid])
      Q2 <- median(x)
      Q3 <- median(x_no_median[(mid+1):length(x_no_median)])
      
      IQR <- Q3 - Q1
      lower_fence <- Q1 - coef * IQR
      upper_fence <- Q3 + coef * IQR
      
      outliers <- x[x < lower_fence | x > upper_fence]
      
      if(length(outliers) == 0) {
        return(data.frame(sample = character(0), data = numeric(0)))
      } else {
        return(data.frame(sample = sampleName, data = outliers))
      }
    }
    
    printHTConclusion <- function(region, reject, suffEvidence, altHyp, altHypValue) {
      conclusion <- tagList(
        withMathJax(),
        p(tags$b("Conclusion:")),
        sprintf("At \\( \\alpha = %s \\), since the test statistic falls in the %s region we %s \\(
               H_{0}\\) and conclude that there %s enough statistical evidence to support that \\(%s %s\\).",
               SigLvl(),
               region,
               reject,
               suffEvidence,
               altHyp,
               altHypValue),
        br(),
        br()
      )
      
      return(conclusion)
    }
    
    getTTestErrorMsg <- function(sampleData) {
      sampleMean <- mean(sampleData, na.rm = TRUE)
      sampleSD   <- sd(sampleData, na.rm = TRUE)
      
      if (sampleSD != 0) return(NULL)
      
      return("When the sample standard deviation is 0, the test statistic (t) is undefined.")
    }
    
    printOneMeanCI <- function() {
      
      oneMeanData <- GetOneMeanCI()
      
      if(OneMeanSigma() == "Known"){
        sdSymbol <- "\\sigma"
        testStat <- "z"
        critVal <- oneMeanData["Z Critical"]
        
      } else {
        sdSymbol <- "s"
        testStat <- "t"
        critVal <- oneMeanData["T Critical"]
      }
      
      oneMeanCIOutput <- tagList()
      
      givenOutput <- printOneMeanGiven()
      
      alphaOutput <- tagList(
        withMathJax(),
        sprintf("For a \\( %s \\)%% Confidence Interval: ",
                ConfLvl()*100),
        br(),
        sprintf("\\( \\alpha = 1 - %s = %s \\)",
                ConfLvl(),
                1 - ConfLvl()),
        br())
      
      cvOutput <- printOneMeanCV()
      formulaOutput <- printOneMeanCIFormula()
      calcOutput <- printOneMeanCICalc()
      intrpOutput <- printOneMeanCIIntrp()
      
      oneMeanCIOutput <- tagAppendChildren(oneMeanCIOutput, givenOutput, alphaOutput, cvOutput, formulaOutput, calcOutput, intrpOutput)
      
      return(oneMeanCIOutput)
    }
    
    printOneMeanCV <- function() {
      
      oneMeanData <- GetOneMeanCI()
      
      if(OneMeanSigma() == "Known"){
        cvOutput <- tagList(
          sprintf("\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
                  1 - ConfLvl(),
                  (1 - ConfLvl()) / 2,
                  oneMeanData["Z Critical"]),
          br(),
          br(),
          br(),
        )
      } else {
        df <- oneMeanData["Sample Size"] - 1
        
        cvOutput <- tagList(
          sprintf("\\( df = n - 1 = %s - 1 = %s\\)",
                  oneMeanData['Sample Size'],
                  oneMeanData['Sample Size'] - 1),
          br(),
          sprintf("\\( t_{\\alpha/2, \\, df} = t_{%s/2, \\, %s} = t_{%s, \\, %s} = %s \\)",
                  1 - ConfLvl(),
                  df,
                  (1 - ConfLvl()) / 2,
                  df,
                  oneMeanData["T Critical"]),
          br(),
          br(),
          br()
        )
      }
      
      return(cvOutput)
    }
    
    printOneMeanGiven <- function() {
      oneMeanData <- GetOneMeanCI()
      
      if(input$dataAvailability == 'Summarized Data') {
        if(OneMeanSigma() == 'Known') {
          sd <- '\\sigma'
        } else {
          sd <- 's'
        }
        
        givenOutput <- tagList(
          sprintf("Given:"),
          br(),
          sprintf("\\( n = %s \\)",
                  oneMeanData['Sample Size']),
          br(),
          sprintf("\\( \\bar{x} = %s \\)",
                  oneMeanData['Sample Mean']),
          br(),
          sprintf("\\( %s = %s \\)",
                  sd,
                  oneMeanData[3]),
          br(),
          br(),
          br()
        )
        
      } else {
        
        if(OneMeanSigma() == 'Known') {
          givenOutput <- tagList(
            sprintf("Given:"),
            br(),
            sprintf("\\( \\sigma = %s \\)",
                    oneMeanData[3]),
            br(),
            br(),
            br()
          )
        } else {
          givenOutput <- br()
          
        }
        
      }
    }
    
    printOneMeanCIFormula <- function() {
      oneMeanData <- GetOneMeanCI()
      
      if(OneMeanSigma() == 'Known') {
        sd <- "\\sigma"
        testStat <- "z_{\\alpha/2}"
      } else {
        sd <- "s"
        testStat <- "t_{\\alpha/2, \\, df}"
      }
      
      formulaOutput <- tagList(
        sprintf("\\( \\displaystyle CI = \\bar{x} \\pm \\left( %s \\dfrac{%s}{\\sqrt{n}} \\right) \\)",
                testStat,
                sd),
        br()
      )
      
      if(input$dataAvailability != "Summarized Data") {
        formulaOutput <- tagAppendChild(formulaOutput, printOneMeanWhere(oneMeanData))
      } else {
        formulaOutput <- tagAppendChildren(formulaOutput, br(), br())
      }
      
      return(formulaOutput)
    }
    
    printOneMeanWhere <- function(oneMeanData) {
      
      if(OneMeanSigma() == 'Known') {
        formulaOutput <- tagList(
          sprintf("where"),
          br(),
          sprintf("\\( \\phantom{CII} n = %s \\; , \\)",
                  oneMeanData["Sample Size"]),
          sprintf("\\( \\phantom{CII} \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%g} = %g \\)",
                  OneMeanTotaledData()[1],
                  oneMeanData["Sample Size"],
                  oneMeanData["Sample Mean"]),
          br(),
          br(),
          br()
        )
      } else {
        formulaOutput <- tagList(
          sprintf("where"),
          br(),
          sprintf("\\( \\phantom{CII} n = %s \\; , \\)",
                  oneMeanData["Sample Size"]),
          sprintf("\\( \\phantom{CII} \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%g} = %g \\; , \\)",
                  OneMeanTotaledData()[1],
                  oneMeanData["Sample Size"],
                  oneMeanData["Sample Mean"]),
          br(),
          sprintf("and"),
          br(),
          sprintf("\\( \\phantom{CII} s  = \\sqrt{ \\dfrac{\\sum x^{2} - \\dfrac{(\\sum x)^{2}}{n} }{n - 1} } \\)"),
          sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%g} }{%g - 1} } = %g \\)",
                  OneMeanTotaledData()[2],
                  OneMeanTotaledData()[1],
                  oneMeanData["Sample Size"],
                  oneMeanData["Sample Size"],
                  oneMeanData[3]),
          br(),
          br(),
          br()
        )
      }
      
    }
    
    printOneMeanCICalc <- function() {
      oneMeanData <- GetOneMeanCI()
      
      if(OneMeanSigma() == "Known"){
        critVal <- oneMeanData["Z Critical"]
      } else {
        critVal <- oneMeanData["T Critical"]
      }
      
      calcOutput <- tagList(
        sprintf("\\( \\displaystyle CI = %s \\pm \\left( %g \\dfrac{%g}{\\sqrt{%g}} \\right) \\)",
                oneMeanData["Sample Mean"],
                critVal,
                oneMeanData[3],
                oneMeanData['Sample Size']),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = %s \\pm \\left( %g \\cdot %g \\right) \\)",
                oneMeanData["Sample Mean"],
                critVal,
                oneMeanData['Std Error']),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = %s \\pm %g \\)",
                oneMeanData["Sample Mean"],
                oneMeanData['ME']),
        br(),
        br(),
        sprintf("\\( \\phantom{CI} = (%g, %g)\\)",
                oneMeanData["LCL"],
                oneMeanData["UCL"]),
        br(),
        br(),
        br()
      )
      
      return(calcOutput)
    }
    
    printOneMeanCIIntrp <- function() {
      oneMeanData <- GetOneMeanCI()
      
      intrpOutput <- tagList(
        p(tags$b("Interpretation:")),
        sprintf("We are %1.0f%% confident that the population mean \\( (\\mu)\\) is between \\( %g \\) and \\( %g \\).",
                ConfLvl()*100,
                oneMeanData["LCL"],
                oneMeanData["UCL"]),
        br()
      )
      
      return(intrpOutput)
    }
    
    printOneMeanHT <- function() {
      
      oneMeanData <- GetOneMeanHT()
      intrpInfo <- OneMeanHypInfo()
      
      if(OneMeanSigma() == 'Known') {
        sdSymbol <- "\\sigma"
        testStat <- "z"
      } else {
        sdSymbol <- "s"
        testStat <- "t"
      }
      
      if(oneMeanData[7] > SigLvl())
      {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      }
      else
      {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      }
      
      oneMeanHTOutput <- tagList(
        withMathJax(),
        sprintf("\\( H_{0}: %s %s\\)",
                intrpInfo$nullHyp,
                input$hypMean),
        br(),
        sprintf("\\( H_{a}: %s %s\\)",
                intrpInfo$altHyp,
                input$hypMean),
        br(),
        br(),
        sprintf("\\( \\alpha = %s \\)",
                SigLvl()),
        br(),
        br(),
        p(tags$b("Test Statistic:")))
      
      givenOutput <- printOneMeanGiven()
      formulaOutput <- printOneMeanHTFormula(sdSymbol, testStat)
      pvalOutput <- printHTPVal(oneMeanData["P-Value"], testStat, intrpInfo$alternative, oneMeanData["Test Statistic"], pvalSymbol, reject)
      cvOutput <- printOneMeanHTCV(testStat, reject, region)
      conclusionOutput <- printHTConclusion(region, reject, suffEvidence, OneMeanHypInfo()$altHyp, input$hypMean)
      
      tagAppendChildren(oneMeanHTOutput, givenOutput, formulaOutput, pvalOutput, cvOutput, conclusionOutput)
    }
    
    printOneMeanHTFormula <- function(sdSymbol, testStat) {
      oneMeanData <- GetOneMeanHT()
      
      formulaOutput <- tagList(
        sprintf("\\(%s = \\dfrac{\\bar{x} - \\mu_{0}}{ \\dfrac{%s}{\\sqrt{n}} } \\)",
                testStat,
                sdSymbol),
        br()
      )
      
      if(input$dataAvailability != "Summarized Data") {
        formulaOutput <- tagAppendChild(formulaOutput, printOneMeanWhere(oneMeanData))
      } else {
        formulaOutput <- tagAppendChildren(formulaOutput, br(), br())
      }
      
      hypPopMean <- input$hypMean
      if (hypPopMean < 0)
        hypPopMean <- paste("(", hypPopMean, ")", sep = "")
      
      calcOutput <- tagList(
        sprintf("\\(%s =  \\dfrac{%g - %g}{ \\dfrac{%g}{\\sqrt{%g}} }\\)",
                testStat,
                oneMeanData[2],
                hypPopMean,
                oneMeanData[3],
                oneMeanData[1]),
        sprintf("\\( = \\dfrac{%g}{%g} \\)",
                oneMeanData[2] - input$hypMean,
                oneMeanData["Std Error"]),
        br(),
        br(),
        sprintf("\\(\\phantom{%s} = %g\\)",
                testStat,
                oneMeanData[6]),
        br(),
        br(),
        br()
      )
      
      formulaOutput <- tagAppendChild(formulaOutput, calcOutput)
      return(formulaOutput)
    }
    
    printOneMeanHTCV <- function(testStat, reject, region) {
      oneMeanData <- GetOneMeanHT()
      
      if(testStat == 'z') {
        critVal <- paste(oneMeanData["Z Critical"])
      } else {
        critVal <- paste(oneMeanData["T Critical"])
      }
      
      if(OneMeanHypInfo()$alternative == "two.sided")
      {
        critVal <- paste("\\pm", critVal)
      }
      
      if(testStat == 'z') {
        
        cvOutput <- tagList(
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                  OneMeanHypInfo()$critSign,
                  OneMeanHypInfo()$critAlph,
                  OneMeanHypInfo()$critSign,
                  OneMeanHypInfo()$alphaVal,
                  critVal),
          br(),
          br(),
        )
        
      } else {
        
        cvOutput <- tagList(
          p(tags$b("Using Critical Value Method:")),
          sprintf("\\( df = n - 1 = %s \\)",
                  oneMeanData["Sample Size"] - 1),
          br(),
          br(),
          sprintf("Critical Value(s) \\( = %s t_{%s, \\, df} = %s t_{%s, \\, %s} = %s \\)",
                  OneMeanHypInfo()$critSign,
                  OneMeanHypInfo()$critAlph,
                  OneMeanHypInfo()$critSign,
                  OneMeanHypInfo()$alphaVal,
                  oneMeanData["Sample Size"] - 1,
                  critVal),
          br(),
          br()
        )
      }
      
      cvEnd <- tagList(
        sprintf("Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
                testStat,
                region,
                reject),
        br(),
        br(),
        plotOutput(session$ns('oneMeanHTPlot'), width = "75%", height = "300px"),
        br()
      )
      
      cvOutput <- tagAppendChild(cvOutput, cvEnd)
      return(cvOutput)
    }
    
    printHTPVal <- function(pValue, testStat, alternative, tsValue, pvalSign, reject) {
      
      if(pValue < 0.0001)
      {
        pValue <- "P \\lt 0.0001"
      }
      
      if(alternative == "two.sided"){
        pvalCalc <- paste("2 \\times P(", testStat, "\\, \\gt \\; \\mid", tsValue, "\\mid)")
      } else if (alternative == "greater"){
        pvalCalc <- paste("P(", testStat, "\\, > \\,", tsValue, ")")
      } else {
        pvalCalc <- paste("P(", testStat, "\\, < \\,", tsValue, ")")
      }
      
      pvalOutput <- tagList(
        p(tags$b("Using P-Value Method:")),
        sprintf("\\(P = %s = %s\\)",
                pvalCalc,
                pValue),
        br(),
        br(),
        sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
                pvalSign,
                SigLvl(),
                reject),
        br(),
        br(),
        br(),
      )
      
      return(pvalOutput)
    }
    
    PrintIndMeansSummaryTable <- function(data) {
      df <- data.frame(
        "Sample Size" = c(data$n1, data$n2),
        "Sample Mean" = c(data$xbar1, data$xbar2),
        "Sample Standard Deviation" = c(data$sd1, data$sd2),
        "Sample Variance" = c(data$sd1^2, data$sd2^2),
        row.names = c("Sample 1", "Sample 2"),
        check.names = FALSE
      )
      
      colNames <- c("Sample Size", "Sample Mean", "Sample Standard Deviation", "Sample Variance")
      
      headers <- htmltools::withTags(table(
        class = 'display',
        style = 'max-width: 600px; table-layout: fixed; width: 100%;',
        thead(
          tr(
            th("",
               style = "border: 1px solid rgba(0, 0, 0, 0.15);
                    border-bottom: 1px solid rgba(0, 0, 0, 0.3);"),
            lapply(colNames, th,
                   style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                        border-top: 1px solid rgba(0, 0, 0, 0.15);')
          )
        )
      ))
      
      datatable(df,
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
                    list(className = 'dt-center', targets = 0:4),
                    list(width = '150px', targets = 0:4)
                  )
                ),
                selection = "none",
                escape = FALSE,
                filter = "none"
      ) %>%
        formatRound(columns = 1, digits = 0) %>%
        formatRound(columns = 2:4, digits = 4) %>%
        formatStyle(columns = 0, fontWeight = 'bold')
    }
    
    showSummaryTable <- function() {
      showTable <-
        (input$dataAvailability2 == "Enter Raw Data" && input$bothsigmaKnownRaw == "bothUnknown") ||
        (input$dataAvailability2 == "Upload Data" && input$bothsigmaKnownUpload == "bothUnknown")
      return (showTable)
    }
    
    GetDepMeansData <- function() {
      ### JB note: this req caused a bunch of errors for input validation on Mu Naught in Dep Means hypothesis testing
      ### leaving it commented out for now
      # req(si_iv$is_valid())
      
      dat <- list()
      
      if(input$dataTypeDependent == 'Upload Data') {
        req(input$depMeansUplSample1, input$depMeansUplSample2)
        sampBefore <- na.omit(unlist(DepMeansUploadData()[,input$depMeansUplSample1]))
        sampAfter <- na.omit(unlist(DepMeansUploadData()[,input$depMeansUplSample2]))
      } else if(input$dataTypeDependent == 'Enter Raw Data') {
        sampBefore <- createNumLst(input$before)
        sampAfter <- createNumLst(input$after)
      }
      
      dat$before <- sampBefore
      dat$after <- sampAfter
      dat$d <- (sampBefore - sampAfter)
      dat$n  <- length(sampBefore)
      dat$dbar <- sum(dat$d) / dat$n
      dat$sd <- sqrt(sum((dat$d - dat$dbar)^2) / (dat$n - 1))
      dat$muNaught <- input$depMeansMuNaught 
      
      return(dat)
    }
    
    #fix this diana  
    GetwRankSumMeansData <- function() {
      req(si_iv$is_valid())
      
      dat <- list()
      
      if(input$wilcoxonRankSumTestData == 'Upload Data') {
        samp1 <- na.omit(as.numeric(unlist(WilcoxonUploadData()[,input$wilcoxonUpl1])))
        samp2 <- na.omit(as.numeric(unlist(WilcoxonUploadData()[,input$wilcoxonUpl2])))
      } else if(input$wilcoxonRankSumTestData == 'Enter Raw Data') {
        samp1 <- createNumLst(input$rankSumRaw1)
        samp2 <- createNumLst(input$rankSumRaw2)
      }
      
      dat$samp1 <- samp1
      dat$samp2 <- samp2
      dat$n1 <- length(samp1)
      dat$n2 <- length(samp2)
      dat$mean1 <- mean(samp1)
      dat$mean2 <- mean(samp2)
      
      return(dat)
    }
    
    TwoPopVarCI <- function(n1, sd1, n2, sd2, conf_level = 0.95, is_variance) {
      df1 <- n1-1
      df2 <- n2-1
      
      alpha <- 1 - conf_level
      
      if (is_variance) {
        var1 <- sd1
        var2 <- sd2
      } else {  # else summary, so sd's need to be ^2
        var1 <- sd1^2
        var2 <- sd2^2
      }
      F_stat <- var1 / var2
      
      F_critical_lower <- qf(alpha/2, df2, df1)
      F_critical_upper <- qf(1 - alpha/2, df2, df1)
      
      CI_lower <- F_stat * F_critical_lower
      CI_upper <- F_stat * F_critical_upper
      
      return(list(
        CI_lower = CI_lower,
        CI_upper = CI_upper,
        F_lower = F_critical_lower,
        F_upper = F_critical_upper,
        F_statistic = F_stat
      ))
      
    }
    
    TwoPopVarHT <- function(n1, sd1, n2, sd2, sig_lvl, alt_hyp = "two.sided", is_variance) {
      df1 <- n1-1
      df2 <- n2-1
      crit_lower <- 0
      crit_upper <- 0
      crit_val <- 0
      
      if (is_variance) {
        var1 <- sd1
        var2 <- sd2
      } else {  # else summary, so sd's need to be ^2
        var1 <- sd1^2
        var2 <- sd2^2
      }
      
      F_stat <- var1/var2
      
      if (alt_hyp == "greater") {
        p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)
        crit_val <- qf(1 - sig_lvl, df1, df2)
        reject <- F_stat > crit_val
        
      } else if (alt_hyp == "less") {
        p_value <- pf(F_stat, df1, df2, lower.tail = TRUE)
        crit_val <- qf(sig_lvl, df1, df2)
        reject <- F_stat < crit_val
        
      } else { # two sided
        if (F_stat > 1) {
          p_value <- 2 * pf(F_stat, df1, df2, lower.tail = FALSE)
        } else {
          p_value <- 2* pf(F_stat, df1, df2, lower.tail = TRUE)
        }
        crit_lower <- qf(sig_lvl / 2, df1, df2)
        crit_upper <- qf(1 - sig_lvl / 2, df1, df2)
        reject <- F_stat < crit_lower || F_stat > crit_upper
      }
      
      return(list(
        F_statistic = F_stat,
        p_value = p_value,
        reject_null = reject,
        crit_upper = crit_upper,
        crit_lower = crit_lower,
        crit_val = crit_val
      ))
    }
    
    printTwoPopVarGivens <- function (data, is_variance) {
      if (is_variance) {
        tagList(
          sprintf("\\(n_1 = %d\\)", data$n1),
          br(),
          sprintf("\\(s_1^2 = %.4f\\)", data$sd1),
          br(),
          sprintf("\\(n_2 = %d\\)", data$n2),
          br(),
          sprintf("\\(s_2^2 = %.4f\\)", data$sd2),
          br(),
          br(),
        )
      } else {
        tagList(
          sprintf("\\(n_1 = %d\\)", data$n1),
          br(),
          sprintf("\\(s_1 = %.4f\\)", data$sd1),
          br(),
          sprintf("\\(n_2 = %d\\)", data$n2),
          br(),
          sprintf("\\(s_2 = %.4f\\)", data$sd2),
          br(),
          br(),
        )
      }
    }
    
    printDegreesFreedom<- function (df1, df2) {
      n1 <- df1+1
      n2 <- df2+1
      
      list(
        p(sprintf("\\(df_1 = n_1 - 1 = %d - 1 = %d\\)", n1, df1)),
        p(sprintf("\\(df_2 = n_2 - 1 = %d - 1 = %d\\)", n2, df2)),
        br())
    }
    
    printFTestPVal <- function(pValue, testStatVal, alternative, pValSign, rejectWord) {
      if (pValue < 0.0001) {
        pValueFormatted <- "P \\lt 0.0001"
      } else {
        pValueFormatted <- sprintf("%.4f", pValue)
      }
      
      if (alternative == "two.sided") {
        pvalCalc <- sprintf("2 \\times P(F \\lt %.4f)", testStatVal)
      } else if (alternative == "greater") {
        pvalCalc <- sprintf("P(F \\gt %.4f)", testStatVal)
      } else {
        pvalCalc <- sprintf("P(F \\lt %.4f)", testStatVal)
      }
      
      tagList(
        p(tags$b("Using P-Value Method:")),
        sprintf("\\(P = %s = %s\\)", pvalCalc, pValueFormatted),
        br(), br(),
        sprintf("Since \\( P %s %.2f \\), %s \\(H_0\\).",
                pValSign,
                SigLvl(),
                rejectWord),
        br(), br(), br(),
      )
    }
    
    twoPopVarOutputText <- function(HT, sig_lvl) {
      if (!HT$reject_null) {
        region <- "acceptance"
        isWord <- "isn't"
      } else {
        region <- "rejection"
        isWord <- "is"
      }
      rejectWord = if (HT$p_value <= sig_lvl) "reject" else "do not reject"
      pValSign = if (HT$p_value <= sig_lvl) "\\leq" else ">"
      
      return(list(
        region = region,
        isWord = isWord,
        rejectWord = rejectWord,
        pValSign = pValSign
      ))
    }
    
    printFStat <- function(sd1, sd2, F_statistic, is_variance, is_HT = FALSE) {
      if (!is_variance) {
        p(sprintf("\\(%s\\dfrac{s_1^2}{s_2^2} = \\dfrac{%.4f^2}{%.4f^2} = %.4f \\)", if (is_HT) "F = " else "", sd1, sd2, F_statistic))
      } else {
        p(sprintf("\\(%s\\dfrac{s_1^2}{s_2^2} = \\dfrac{%.4f}{%.4f} = %.4f \\)", if (is_HT) "F = " else "", sd1, sd2, F_statistic))
      }
    }
    
    shadeHtArea <- function(df, critValue, altHypothesis) {
      
      if(altHypothesis == 'less') {
        geom_area(data = subset(df, x <= critValue),
                  aes(y=y),
                  fill = "#023B70",
                  color = NA,
                  alpha = 0.4)
        
        # } else if (altHypothesis == 'two.sided') {
        #   geom_area(data = subset(df, x <= critValueLeft),
        #             aes(y=y),
        #             fill = "#023B70",
        #             color = NA,
        #             alpha = 0.4) +
        #   geom_area(data = subset(df, x >= critValue),
        #             aes(y=y),
        #             fill = "#023B70",
        #             color = NA,
        #             alpha = 0.4)
        
      } else if (altHypothesis == 'greater') {
        geom_area(data = subset(df, x >= critValue),
                  aes(y=y),
                  fill = "#023B70",
                  color = NA,
                  alpha = 0.4)
      }
    }
    
    #   if(altHypothesis == "less") #less
    #   {
    #     area[x > critValues] <- NA
    #   }
    #   else if(altHypothesis == "two.sided") #twosided
    #   {
    #     area[x > critValues[1] & x < critValues[2]] <- NA
    #   }
    #   else if(altHypothesis == "greater") #greater
    #   {
    #     area[x < critValues] <- NA
    #   }
    #   return(area)
    # }
    
    hypZTestPlot <- function(testStatistic, critValue, altHypothesis){
      # normTail = qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE)
      # normHead = qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE)
      # xSeq = sort(c(normTail, normHead, testStatistic, critValues, 0))
      
      x <- round(seq(from = -3, to = 3, by = 0.1), 2)
      
      if(altHypothesis == "two.sided") {
        CVs <- c(-critValue, critValue)
        RRLabels <- c((-critValue + -3)/2, (critValue + 3)/2)
      } else{
        CVs <- c(critValue)
        if(altHypothesis == 'less') {
          RRLabels <- c((critValue + -3)/2)
        } else {
          RRLabels <- c((critValue + 3)/2)
        }
      }
      
      xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
      
      # if(testStatistic < normTail)
      # {
      #   normTail = testStatistic
      #
      # } else if(testStatistic > normHead)
      # {
      #   normHead = testStatistic
      # }
      
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
      cvDF <- filter(df, x %in% CVs)
      RRLabelsDF <- filter(df, x %in% RRLabels)
      tsDF <- filter(df, x %in% testStatistic)
      centerDF <- filter(df, x %in% c(0))
      
      htPlot <- ggplot(df, aes(x = x, y = y))
      
      if(altHypothesis == 'two.sided') {
        htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
          shadeHtArea(df, critValue, "greater")
      } else {
        htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
      }
      
      htPlot <- htPlot + geom_segment(data = cvDF,
                                      aes(x = x, xend = x, y = 0, yend = y),
                                      linetype = "solid",
                                      lineend = 'butt',
                                      linewidth = 1.5,
                                      color='#023B70') +
        stat_function(fun = dnorm,
                      geom = "density",
                      fill = NA) +
        theme_void() +
        scale_y_continuous(breaks = NULL) +
        ylab("") + xlab("Z") +
        geom_segment(data = filter(df, x %in% c(0)),
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     linewidth = 0.75,
                     color='black') +
        geom_text(data = filter(df, x %in% c(0)),
                  aes(x = x, y = y/2, label = "A R"),
                  size = 16 / .pt,
                  check_overlap = TRUE,
                  fontface = "bold") +
        geom_text(data = filter(df, x %in% c(0)),
                  aes(x = x, y = 0, label = "0"),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03,
                  check_overlap = TRUE) +
        geom_segment(data = tsDF,
                     aes(x = x, xend = x, y = 0, yend = y + .055),
                     linetype = "solid",
                     linewidth = 1.25,
                     color='#BD130B') +
        geom_text(data = tsDF,
                  aes(x = x, y = y, label = x),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = .075,
                  check_overlap = TRUE) +
        geom_text(data = cvDF,
                  aes(x = x, y = 0, label = x),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03,
                  check_overlap = TRUE) +
        geom_text(data = RRLabelsDF,
                  aes(x = x, y = y, label = "RR"),
                  size = 16 / .pt,
                  fontface = "bold",
                  nudge_y = .025,
                  check_overlap = TRUE) +
        theme(axis.title.x = element_text(size = 16, face = "bold.italic")) +
        coord_cartesian(clip="off")
      
      return(htPlot)
    }
    
    hypTTestPlot <- function(testStatistic, degfree, critValue, altHypothesis){
      tTail = qt(0.999, df = degfree, lower.tail = FALSE)
      tHead = qt(0.999, df = degfree, lower.tail = TRUE)
      x <- round(seq(from = tTail, to = tHead, by = 0.1), 2)
      
      if(altHypothesis == "two.sided") {
        CVs <- c(-critValue, critValue)
        RRLabels <- c((-critValue + tTail)/2, (critValue + tHead)/2)
      } else{
        CVs <- c(critValue)
        if(altHypothesis == 'less') {
          RRLabels <- c((critValue + tTail)/2)
        } else {
          RRLabels <- c((critValue + tHead)/2)
        }
      }
      
      xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
      
      df <- data.frame(x = xSeq, y = dt(xSeq, degfree))
      cvDF <- filter(df, x %in% CVs)
      RRLabelsDF <- filter(df, x %in% RRLabels)
      tsDF <- filter(df, x %in% testStatistic)
      centerDF <- filter(df, x %in% c(0))
      
      htPlot <- ggplot(df, aes(x = x, y = y))
      
      if(altHypothesis == 'two.sided') {
        htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
          shadeHtArea(df, critValue, "greater")
      } else {
        htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
      }
      
      htPlot <- htPlot + stat_function(fun = dt,
                                       args = list(df = degfree),
                                       geom = "density",
                                       fill = NA) +
        theme_void()  +
        scale_y_continuous(breaks = NULL) +
        ylab("") +
        xlab("t") +
        geom_segment(data = filter(df, x %in% c(0)),
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     linewidth = 0.75,
                     color='black') +
        geom_text(data = filter(df, x %in% c(0)),
                  aes(x = x, y = y/2, label = "A R"),
                  size = 16 / .pt,
                  fontface = "bold") +
        geom_text(data = filter(df, x %in% c(0)),
                  aes(x = x, y = 0, label = "0"),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03) +
        geom_segment(data = tsDF,
                     aes(x = x, xend = x, y = 0, yend = y + .03),
                     linetype = "solid",
                     linewidth = 1.25,
                     color='#BD130B') +
        geom_text(data = tsDF,
                  aes(x = x, y = y, label = x),
                  size = 16 / .pt,
                  fontface = "bold",
                  nudge_y = .075) +
        geom_segment(data = cvDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'butt',
                     linewidth = 1.5,
                     color='#023B70') +
        geom_text(data = cvDF,
                  aes(x = x, y = 0, label = x),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03) +
        geom_text(data = RRLabelsDF,
                  aes(x = x, y = y, label = "RR"),
                  size = 16 / .pt,
                  fontface = "bold",
                  nudge_y = .03) +
        theme(axis.title.x = element_text(size = 16,
                                          face = "bold.italic"))
      
      return(htPlot)
    }
    
    wilcoxonZTestPlot <- function(testStatistic, critValue, altHypothesis){
      x <- round(seq(from = -3.5, to = 3.5, by = 0.1), 2)
      
      if(altHypothesis == "two.sided") {
        CVs <- c(-critValue, critValue)
        RRLabels <- c(-2.5, 2.5) 
      } else {
        CVs <- c(critValue)
        if(altHypothesis == 'less') {
          RRLabels <- c(-2.5)
        } else {
          RRLabels <- c(2.5)
        }
      }
      
      xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
      
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
      cvDF <- filter(df, x %in% CVs)
      RRLabelsDF <- filter(df, x %in% RRLabels)
      tsDF <- filter(df, x %in% testStatistic)
      centerDF <- filter(df, x %in% c(0))
      
      htPlot <- ggplot(df, aes(x = x, y = y)) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.25)
      
      if(altHypothesis == 'two.sided') {
        htPlot <- htPlot + 
          geom_area(data = subset(df, x <= -critValue),
                    aes(y=y),
                    fill = "#023B70",
                    color = NA,
                    alpha = 0.4) +
          geom_area(data = subset(df, x >= critValue),
                    aes(y=y),
                    fill = "#023B70",
                    color = NA,
                    alpha = 0.4)
      } else if(altHypothesis == 'less') {
        htPlot <- htPlot + 
          geom_area(data = subset(df, x <= critValue),
                    aes(y=y),
                    fill = "#023B70",
                    color = NA,
                    alpha = 0.4)
      } else if(altHypothesis == 'greater') {
        htPlot <- htPlot + 
          geom_area(data = subset(df, x >= critValue),
                    aes(y=y),
                    fill = "#023B70",
                    color = NA,
                    alpha = 0.4)
      }
      
      htPlot <- htPlot + 
        stat_function(fun = dnorm,
                      geom = "line",
                      linewidth = 0.75) +
        theme_void() +
        scale_y_continuous(breaks = NULL) +
        ylab("") + 
        xlab("Z") +
        
        geom_segment(data = centerDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     linewidth = 0.75,
                     color='black') +
        
        geom_text(data = centerDF,
                  aes(x = x, y = y/2, label = "AR"),
                  size = 16 / .pt,
                  fontface = "bold") +
        
        geom_text(data = centerDF,
                  aes(x = x, y = 0, label = "0"),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03) +
        
        geom_segment(data = tsDF,
                     aes(x = x, xend = x, y = 0, yend = y + .055),
                     linetype = "solid",
                     linewidth = 1.25,
                     color='#BD130B') +
        
        geom_text(data = tsDF,
                  aes(x = x, y = y, label = round(x, 3)),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = .075) +
        
        geom_segment(data = cvDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'butt',
                     linewidth = 1.5,
                     color='#023B70') +
        
        geom_text(data = cvDF,
                  aes(x = x, y = 0, label = round(x, 3)),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03) +
        
        geom_text(data = RRLabelsDF,
                  aes(x = x, y = y, label = "RR"),
                  size = 16 / .pt,
                  fontface = "bold",
                  nudge_y = .025) +
        
        theme(axis.title.x = element_text(size = 16, face = "bold.italic")) +
        coord_cartesian(clip="off")
      
      return(htPlot)
    }
    
    getTotaledMatrix <- function(cMatrix, matrixData){
      colnames(cMatrix) <- colnames(matrixData)
      rownames(cMatrix) <- rownames(matrixData)
      cMatrix <- cbind(cMatrix, Total = round(rowSums(cMatrix), 4))
      cMatrix <- rbind(cMatrix, Total = round(colSums(cMatrix), 4))
      
      return(cMatrix)
    }
    
    PrintANOVA <- function() {
      data <- anovaOneWayResults()$test
      
      if(input$anovaSigLvl == "10%") {
        sigLvl <- 0.1
      } else if(input$anovaSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
      critVal <- round(qf(1 - sigLvl, df1 = data[1,"Df"], df2 = data[2,"Df"]), 4)
      
      if(data[1,"Pr(>F)"] < sigLvl) {
        pValSymbol <- "\\leq"
        reject <- "reject"
        region <- "rejection"
        suffEvidence <- "is"
      } else {
        pValSymbol <- "\\gt"
        reject <- "do not reject"
        region <- "acceptance"
        suffEvidence <- "isn't"
      }
      
      hypothesis <- PrintANOVAHyp(sigLvl)
      testStat <- PrintANOVAFormula()
      pValue <- PrintANOVAPValue(pValSymbol, sigLvl, reject)
      anovaCV <- PrintANOVACV(critVal, data[1,"Df"], data[2,"Df"], reject, region, sigLvl)
      conclusion <- PrintANOVAConclusion(sigLvl, reject)
      tagAppendChildren(hypothesis, testStat, pValue, anovaCV, conclusion)
    }
    
    PrintANOVAHyp <- function(sigLvl) {
      anovaData <- anovaOneWayResults()$data
      numGroups <- anovaOneWayResults()$numFactors
      groupCol <- anovaOneWayResults()$factorCol
      groupNames <- anovaOneWayResults()$factorNames
      
      nullHyp <- "H_{0} : "
      groupCounts <- tagList()
      
      for(group in 1:(numGroups - 1)) {
        nullHyp <- paste0(nullHyp, "\\mu_{\\textit{", groupNames[group], "}} = ")
      }
      
      nullHyp <- paste0(nullHyp, "\\mu_{\\textit{", groupNames[numGroups], "}}")
      
      hypothesis <- tagList(
        withMathJax(),
        sprintf("\\( %s \\) ",
                nullHyp),
        br(),
        sprintf("\\( H_{a}: \\) At least two means differ"),
        br(),
        br(),
        sprintf("\\( \\alpha = %s \\)",
                sigLvl),
        br(),
        br(),
        sprintf("\\( n = %s \\)",
                anovaOneWayResults()$count),
        br(),
        sprintf("\\( k = %s \\)",
                numGroups),
        br(),
        br(),
      )
      
      return(hypothesis)
    }
    
    PrintANOVAFormula <- function() {
      tagList(
        br(),
        p(tags$b("Numerical Summaries:")),
        DTOutput(session$ns("oneWayFactorTable"), width = '600px'),
        br(),
        br(),
        p(tags$b("ANOVA Table:")),
        DTOutput(session$ns("oneWayAnovaTable"), width = '900px'),
        br(),
        br(),
        p(tags$b("Test Statistic:")),
        sprintf("\\( F = \\dfrac{MSB}{MSE} = \\dfrac{%0.4f}{%0.4f} = %0.4f \\)",
                anovaOneWayResults()$test[1,"Mean Sq"],
                anovaOneWayResults()$test[2,"Mean Sq"],
                anovaOneWayResults()$test[1,"F value"]),
        br(),
        br(),
        br()
      )
    }
    
    PrintANOVAFactorTable <- function() {
      anovaData <- anovaOneWayResults()$data
      numGroups <- anovaOneWayResults()$numFactors
      groupCol <- anovaOneWayResults()$factorCol
      groupNames <- anovaOneWayResults()$factorNames
      colNames <- c("Factor", "Sample Size", "Sample Mean", "Sample Standard Deviation")
      
      headers = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colNames[1],
               style = "border: 1px solid rgba(0, 0, 0, 0.15);
                      border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"),
            lapply(colNames[2:4], th,
                   style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);')
          )
        )
      ))
      
      factor_df <- data.frame()
      
      for(group in 1:numGroups) {
        groupData <- as.data.frame(anovaData[anovaData$ind == groupNames[group],])
        factor_df <- rbind(factor_df, data.frame("Sample Size" = length(groupData[,groupCol]),
                                                 "Sample Mean" = mean(groupData[,"values"]),
                                                 "Sample Standard Deviation" = sd(groupData[,"values"])))
      }
      
      rownames(factor_df) <- groupNames
      
      ftable <- datatable(factor_df,
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
                            columnDefs = list(list(className = 'dt-center',
                                                   targets = 0:3),
                                              list(width = '150px',
                                                   targets = 0:3))
                          ),
                          selection = "none",
                          escape = FALSE,
                          filter = "none"
      ) %>% formatRound(columns = 1,
                        digits = 0
      ) %>% formatRound(columns = 2:3,
                        digits = 4
      ) %>% formatStyle(columns = c(0),
                        fontWeight = 'bold'
      )
      
      return(ftable)
    }
    
    PrintANOVATable <- function() {
      data <- anovaOneWayResults()$test
      
      if(data[1,"Pr(>F)"] < 0.0001 && data[1,"Pr(>F)"] > 0) {
        data[1,"Pr(>F)"] <- "P < 0.0001"
      } else {
        data[1,"Pr(>F)"] <- paste(round(data[1,"Pr(>F)"], 4))
      }
      
      data <- rbind(data, c(sum(data[,"Df"]), sum(data[,"Sum Sq"]), NA, NA, NA))
      # print(data[,"Df"])
      rownames(data) <- c("Between Groups (Model)", "Within Groups (Error)", "Total")
      colNames <- c("df", "Sum of Squares (SS)", "Mean Sum of Squares (MS)", "F-ratio", "P-Value")
      
      headers = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th("Sources of Variation",
               style = "border: 1px solid rgba(0, 0, 0, 0.15);
                      border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"),
            lapply(colNames, th,
                   style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);')
          )
        )
      ))
      
      datatable(data[,0:5],
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
                  columnDefs = list(list(className = 'dt-center',
                                         targets = 0:5),
                                    list(width = '150px',
                                         targets = 2:5))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none"
      ) %>% formatRound(columns = 1,
                        digits = 0
      ) %>% formatRound(columns = 2:4,
                        digits = 4
      ) %>% formatStyle(columns = c(0,4),
                        fontWeight = 'bold'
      ) %>% formatStyle(columns = 1:5,
                        target = 'row',
                        fontWeight = styleRow(3, "bold"))
    }
    
    PrintANOVAPValue <- function(pValSymbol, sigLvl, reject) {
      tsValue <- anovaOneWayResults()$test[1,"F value"]
      pValue <- anovaOneWayResults()$test[1,"Pr(>F)"]
      pvalCalc <- paste("P(\\, F \\, \\gt \\,", round(tsValue,4), ")")
      
      if(pValue < 0.0001 && pValue > 0) {
        pValue <- "P < 0.0001"
      } else {
        pValue <- paste(round(pValue, 4))
      }
      
      pValOutput <- tagList(
        p(tags$b("Using P-Value Method:")),
        sprintf("\\( P = %s = %s\\)",
                pvalCalc,
                pValue),
        br(),
        br(),
        sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
                pValSymbol,
                sigLvl,
                reject),
        br(),
        br(),
        br()
      )
    }
    
    PrintANOVACV <- function(critVal, df1, df2, reject, region, alpha) {
      
      cvOutput <- tagList(
        p(tags$b("Using Critical Value Method:")),
        sprintf("Critical Value \\( = F_{\\alpha, \\, (k - 1), \\, (n - k)} = F_{%s, \\, %s, \\, %s} = %s \\)",
                alpha,
                df1,
                df2,
                critVal),
        br(),
        br(),
        sprintf("Since the test statistic \\( F \\) falls within the %s region, %s \\( H_{0}\\).",
                region,
                reject),
        br(),
        br(),
        br(),
        plotOutput(session$ns("oneWayAnovaPlot"), width = "50%", height = "400px"),
        br(),
        br()
      )
    }
    
    PrintANOVAConclusion <- function(sigLvl, reject) {
      if(reject == "reject") {
        result <- "there is sufficient statistical evidence in support of the alternative
                 hypothesis \\( (H_{a}) \\) that at least two means differ and post
                 hoc tests are warranted."
      } else {
        result <- "there is not enough statistical evidence in support of the alternative
                  hypothesis \\( (H_{a}) \\) that at least two means differ."
      }
      
      tagList(
        p(tags$b("Conclusion:")),
        p(
          sprintf("At the %1.0f%% significance level, %s",
                  sigLvl*100,
                  result),
          br(),
        )
      )
    }
    
    CreateChiSqObserved <- function(chiSqData) {
      headers <- GetChiSqHeaders(chiSqData)
      
      observedTable <- datatable(chiSqData,
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
                                   columnDefs = list(list(width = '100px',
                                                          targets = 0:ncol(chiSqData)),
                                                     list(className = 'dt-center',
                                                          targets = 0:ncol(chiSqData)))
                                 ),
                                 selection = "none",
                                 escape = FALSE,
                                 filter = "none"
      )
      
      observedTable <- FormatChiSqTable(observedTable, ncol(chiSqData), nrow(chiSqData))
      
      return(observedTable)
    }
    
    CreateChiSqExpected <- function(chiSqData) {
      totaledData <- getTotaledMatrix(round(chiSqData, 4), chiSqActiveData()$data)
      headers <- GetChiSqHeaders(totaledData)
      
      expectedTable <- datatable(totaledData,
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
                                   columnDefs = list(list(width = '100px',
                                                          targets = 0:ncol(totaledData)),
                                                     list(className = 'dt-center',
                                                          targets = 0:ncol(totaledData)))
                                 ),
                                 selection = "none",
                                 escape = FALSE,
                                 filter = "none"
      )
      
      expectedTable <- FormatChiSqTable(expectedTable, ncol(totaledData), nrow(totaledData))
      
      return(expectedTable)
    }
    
    GetChiSqHeaders <- function(chiSqData) {
      rowTitle <- input$chiSquareRowHeader
      colTitle <- input$chiSquareColHeader
      
      if(rowTitle == "" && colTitle == "") {
        headers = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th("",
                 style = "border: 1px solid rgba(0, 0, 0, 0.15);
                        border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"),
              lapply(colnames(chiSqData), th,
                     style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);')
            )
          )
        ))
      } else {
        headers = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 2, colspan = 1, rowTitle,
                 class = 'dt-center',
                 style = 'border: 1px solid rgba(0, 0, 0, 0.15);'),
              th(colspan = ncol(chiSqData), colTitle,
                 class = 'dt-center',
                 style = 'border: 1px solid rgba(0, 0, 0, 0.15);
                      border-left: none;')
            ),
            tr(
              lapply(colnames(chiSqData), th,
                     style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);')
            )
          )
        )
        )
      }
      
      return(headers)
    }
    
    FormatChiSqTable <- function(chiSqTable, numCol, numRow) {
      
      chiSqTable %>%
        formatStyle(columns = c(0,numCol),
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:numCol,
                    target = 'row',
                    fontWeight = styleRow(dim(chiSqTotaled())[1], "bold")) %>%
        formatStyle(columns = c(0,numCol - 1),
                    borderRight = styleRow(c(1:(numRow - 1)),'2px solid #787878')) %>%
        formatStyle(columns = c(1:(numCol - 1)),
                    borderTop = styleRow(c(1),'2px solid #787878'),
                    borderBottom = styleRow(c(numRow - 1),'2px solid #787878'))
    }
    
    PrintChiSqTest <- function() {
      data <- chiSqResults()
      if (input$chiSquareYates && input$chisquareDimension == '2 x 2'){
        chiSqStat <- data$Matrix[nrow(data$Matrix), "(|O - E| - 0.5)<sup>2</sup> / E"]
      } else
        chiSqStat <- data$Matrix[nrow(data$Matrix), "(O - E)<sup>2</sup> / E"]
      
      if(input$chisquareSigLvl == "10%") {
        sigLvl <- 0.1
      } else if(input$chisquareSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
      critVal <- round(qchisq(1 - sigLvl, df = data$Results$parameter), cvDigits)
      
      if(data$Results$p.value < sigLvl) {
        pValSymbol <- "\\leq"
        reject <- "reject"
        region <- "rejection"
        suffEvidence <- "is"
      } else {
        pValSymbol <- "\\gt"
        reject <- "do not reject"
        region <- "acceptance"
        suffEvidence <- "isn't"
      }
      
      chiSqOutput <- tagList(
        withMathJax(),
        titlePanel("5-Step Process"),
        br(),
        br(),
        sprintf("\\( H_{0} \\): The Row variable and Column variable are not associated (independent)"),
        br(),
        sprintf("\\( H_{a} \\): The Row variable and Column variable are associated (dependent)"),
        br(),
        br(),
        sprintf("\\( \\alpha = %s \\)",
                sigLvl),
        br(),
        br()
      )
      
      if(input$chisquareDimension == '2 x 2' && input$chiSquareYates) {
        #Yates correction is only applied when O - E is > 0.5
        chiSqFormula <- PrintChiSqYatesFormula(chiSqStat)
      } else {
        chiSqFormula <- PrintChiSqFormula(chiSqStat)
      }
      
      chiSqPVal <- PrintChiSqPVal(data$Results$p.value, chiSqStat, pValSymbol, sigLvl, reject)
      chiSqCV <- PrintChiSqCV(critVal, reject, region, alpha = sigLvl, df = data$Results$parameter)
      chiSqConclusion <- PrintChiSqConclusion(sigLvl, suffEvidence)
      
      tagAppendChildren(chiSqOutput, chiSqFormula, chiSqPVal, chiSqCV, chiSqConclusion)
    }
    
    PrintChiSqFormula <- function(chiSqStat) {
      data <- chiSqResults()$Matrix
      
      chiSqSum <- ""
      chiSqSmplf <- ""
      
      for(row in 1:(nrow(data) - 2)) {
        chiSqSum <- paste0(chiSqSum, "\\dfrac{(", data[row,"O"], " - ", data[row,"E"], ")^2}{", data[row,"E"], "} + ")
        chiSqSmplf <- paste0(chiSqSmplf, data[row,"(O - E)<sup>2</sup> / E"]," + ")
      }
      
      chiSqSum <- paste0(chiSqSum, "\\dfrac{(", data[nrow(data) - 1,"O"], " - ", data[nrow(data) - 1,"E"], ")^2}{", data[nrow(data) - 1,"E"], "}")
      chiSqSmplf <- paste0(chiSqSmplf, data[nrow(data) - 1,"(O - E)<sup>2</sup> / E"])
      
      formula <- tagList(
        p(tags$b("Test Statistic:")),
        sprintf("\\( \\chi^2 = \\large{\\displaystyle \\sum{ \\dfrac{(O - E)^2}{E} } } \\)"),
        br(),
        br(),
        sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
                chiSqSum),
        br(),
        br(),
        br(),
        sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
                chiSqSmplf),
        br(),
        br(),
        br(),
        sprintf("\\( \\phantom{\\chi^2} = %s \\)",
                chiSqStat),
        br(),
        br(),
        br()
      )
      
      return(formula)
    }
    
    PrintChiSqYatesFormula <- function(chiSqStat) {
      data <- chiSqResults()$Matrix
      yates <- data[,"(O - E)"]
      yates <- round((abs(yates) - 0.5)^2 / data[,"E"], 4)
      yates <- head(yates, -1)
      
      if(all(abs(data[nrow(data) - 1,"(O - E)"]) > 0.5)) {
        
        chiSqStat <- sum(yates)
        chiSqSum <- ""
        chiSqSmplf <- ""
        
        for(row in 1:(nrow(data) - 2)) {
          chiSqSum <- paste0(chiSqSum, "\\dfrac{(|", data[row,"O"], " - ", data[row,"E"], "| - 0.5)^2}{", data[row,"E"], "} + ")
          chiSqSmplf <- paste0(chiSqSmplf, yates[row]," + ")
        }
        
        chiSqSum <- paste0(chiSqSum, "\\dfrac{(|", data[nrow(data) - 1,"O"], " - ", data[nrow(data) - 1,"E"], "| - 0.5)^2}{", data[nrow(data) - 1,"E"], "}")
        chiSqSmplf <- paste0(chiSqSmplf, yates[nrow(data) - 1])
        
        formula <- tagList(
          p(tags$b("Test Statistic:")),
          sprintf("\\( \\chi^2_{Yates} = \\large{ \\sum{ \\dfrac{(|O - E| - 0.5)^2}{E} } } \\)"),
          br(),
          br(),
          sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
                  chiSqSum),
          br(),
          br(),
          br(),
          sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
                  chiSqSmplf),
          br(),
          br(),
          br(),
          sprintf("\\( \\phantom{\\chi^2} = %0.4f \\)",
                  chiSqStat),
          br(),
          br(),
          br()
        )
      } else {
        disclaimer <- tagList(
          p(tags$i("*Note: Yates’ continuity correction is not applied in this
                 case because the correction factor is greater than |O - E| for
                 one or more of the differences.*")
          ),
          br(),
          br()
        )
        formula <- tagAppendChildren(PrintChiSqFormula(chiSqStat), disclaimer)
      }
      
      return(formula)
    }
    
    PrintChiSqPVal <- function(pValue, tsValue, pValSymbol, sigLvl, reject) {
      
      pvalCalc <- paste("P(\\, \\chi^2 \\, \\ge \\,", round(tsValue,4), ")")
      
      if(pValue < 0.0001 && pValue > 0) {
        pValue <- "P < 0.0001"
      } else {
        pValue <- paste(round(pValue, 4))
      }
      
      pValOutput <- tagList(
        p(tags$b("Using P-Value Method:")),
        sprintf("\\( P = %s = %s\\)",
                pvalCalc,
                pValue),
        br(),
        br(),
        sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
                pValSymbol,
                sigLvl,
                reject),
        br(),
        br(),
        br()
      )
      
      return(pValOutput)
    }
    
    PrintChiSqCV <- function(critVal, reject, region, alpha, df) {
      
      cvOutput <- tagList(
        p(tags$b("Using Critical Value Method:")),
        sprintf("Critical Value \\( = \\chi^2_{\\alpha, \\, df} = \\chi^2_{\\alpha, \\, (r - 1) \\times (c - 1)}
              = \\chi^2_{%s, \\, (%s - 1) \\times (%s - 1)} =  \\chi^2_{%s, \\, %s} = %s \\)",
              alpha,
              nrow(chiSqActiveData()$data),
              ncol(chiSqActiveData()$data),
              alpha,
              df,
              critVal),
        br(),
        br(),
        sprintf("Since the test statistic \\( (\\chi^2)\\) falls within the %s region, %s \\( H_{0}\\).",
                region,
                reject),
        br(),
        br(),
        br(),
        plotOutput(session$ns("chiSqPlot"), width = "50%", height = "400px"),
        br(),
        br()
      )
    }
    
    PrintChiSqConclusion <- function(sigLvl, suffEvidence) {
      
      conclusion <- tagList(
        p(tags$b("Conclusion:")),
        p(
          sprintf("At the %1.0f%% significance level, there %s sufficient
                evidence to reject the null hypothesis \\( (H_{0}) \\) that the
                Row variable and Column variable are not associated.",
                sigLvl*100,
                suffEvidence),
          br(),
        )
      )
      
      return(conclusion)
    }
    
    PrintFishersTest <- function() {
      results <- fishersResults()
      
      if(input$chisquareSigLvl == "10%") {
        sigLvl <- 0.1
      } else if(input$chisquareSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
      if(results$p.value > sigLvl) {
        pValSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        pValue <- paste(round(results$p.value, 4))
      } else {
        pValSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        
        if(results$p.value < 0.0001 && results$p.value > 0) {
          pValue <- "p < 0.0001"
        } else {
          pValue <- paste(round(results$p.value, 4))
        }
      }
      
      fishersOutput <- tagList(
        withMathJax(),
        br(),
        br(),
        sprintf("\\( H_{0} \\): The Row variable and Column variable are not associated (independent)"),
        br(),
        sprintf("\\( H_{a} \\): The Row variable and Column variable are associated (dependent)"),
        br(),
        br(),
        sprintf("\\( \\alpha = %s \\)",
                sigLvl),
        br(),
        br()
      )
      
      fishersPVal <- PrintFishersPVal(pValue, pValSymbol, sigLvl, reject)
      fishersConclusion <- PrintChiSqConclusion(sigLvl, suffEvidence)
      
      tagAppendChildren(fishersOutput, fishersPVal, fishersConclusion)
    }
    
    PrintFishersPVal <- function(pValue, pValSymbol, sigLvl, reject) {
      fishersData <- chiSqTotaled()
      
      if(input$chisquareDimension == '2 x 2') {
        tagList(
          p(tags$b("P-Value:")),
          sprintf("\\( p = \\dfrac{(a + b)! \\; (c + d)! \\; (a + c)! \\; (b + d)!}{a! \\; b! \\; c! \\; d! \\; n!} \\)"),
          br(),
          br(),
          sprintf("\\( \\phantom{p} = \\dfrac{(%s + %s)! \\; (%s + %s)! \\; (%s + %s)! \\; (%s + %s)!}{%s! \\; %s! \\; %s! \\; %s! \\; %s!} \\)",
                  fishersData[1,1],
                  fishersData[1,2],
                  fishersData[2,1],
                  fishersData[2,2],
                  fishersData[1,1],
                  fishersData[2,1],
                  fishersData[1,2],
                  fishersData[2,2],
                  fishersData[1,1],
                  fishersData[1,2],
                  fishersData[2,1],
                  fishersData[2,2],
                  fishersData[3,3]),
          br(),
          br(),
          sprintf("\\( \\phantom{p} = %s \\)",
                  pValue),
          br(),
          br(),
          sprintf("Since \\( p %s %0.2f \\), %s \\( H_{0}\\).",
                  pValSymbol,
                  sigLvl,
                  reject),
          br(),
          br()
        )
      } else {
        tagList(
          p(tags$b("P-Value:")),
          sprintf("\\( p = %s \\)",
                  pValue),
          br(),
          br(),
          sprintf("Since \\( p %s %0.2f \\), %s \\( H_{0}\\).",
                  pValSymbol,
                  sigLvl,
                  reject),
          br(),
          br()
        )
      }
    }
    
    #  ========================================================================= #
    ## -------- Reactives ------------------------------------------------------
    #  ========================================================================= #
    fileInputs <- reactiveValues(
      oneMeanStatus = NULL,
      indMeansStatus = NULL,
      rankSumStatus = NULL, 
      depMeansStatus = NULL,
      anovaStatus = NULL,
      kwStatus = NULL
    )
    
    ConfLvl <- reactive({
      
      ## MAYBE FIXME: the inputId siMethod has no choiceValue 'n'... it has
      ## choice values 1, 2, Multiple, and Categorical.
      req(input$siMethod != 'n')
      
      if(input$siMethod == '1') {
        
        if(input$confidenceLevel == '90%') {
          confLvl <- 0.9
        } else if(input$confidenceLevel == '95%') {
          confLvl <- 0.95
        } else {
          confLvl <- 0.99
        }
        
      } else if(input$siMethod == '2') {
        
        if(input$confidenceLevel2 == '90%') {
          confLvl <- 0.9
        } else if(input$confidenceLevel2 == '95%') {
          confLvl <- 0.95
        } else {
          confLvl <- 0.99
        }
      } else {
        confLvl <- 0
      }
      
      ## B.C. NOTE: all of the above could be replaced with the following.
      ## req(input$siMethod != NULL)
      ## if (input$siMethod %in% c(1, 2))
      ##   confLvl <- switch(input$confidenceLevel,
      ##                     "90%" = 0.90,
      ##                     "95%" = 0.95,
      ##                     0.99)
      ## else
      ##   confLvl <- 0
      
      return(confLvl)
    })
    
    
    SigLvl <- reactive({
      
      if(input$siMethod == '1') {
        
        if(input$significanceLevel == "10%") {
          sigLvl <- 0.10
        } else if(input$significanceLevel == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }
        
      } else if (input$siMethod == '2') {
        
        if(input$significanceLevel2 == "10%") {
          sigLvl <- 0.10
        } else if(input$significanceLevel2 == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }
        
      } else {
        sigLvl <- 0.05
      }
      
      return(sigLvl)
    })
    
    ### ------------ One Mean reactives ------------------------------------------
    
    OneMeanUploadData <- eventReactive(input$oneMeanUserData, {
      
      ext <- tools::file_ext(input$oneMeanUserData$name)
      ext <- tolower(ext)
      
      data <- switch(ext,
                     csv = read_csv(input$oneMeanUserData$datapath, show_col_types = FALSE),
                     xls = read_xls(input$oneMeanUserData$datapath),
                     xlsx = read_xlsx(input$oneMeanUserData$datapath),
                     txt = read_tsv(input$oneMeanUserData$datapath, show_col_types = FALSE),
                     
                     validate("Improper file format.")
      )
    })
    
    OneMeanUploadStatus <- reactive({
      if (is.null(fileInputs$oneMeanStatus)) {
        return(NULL)
      } else if (fileInputs$oneMeanStatus == 'uploaded') {
        return(input$file1)
      } else if (fileInputs$oneMeanStatus == 'reset') {
        return(NULL)
      }
    })
    
    OneMeanTotaledData <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability == 'Enter Raw Data'){
        dat <- createNumLst(input$sample1)
        
      } else if (input$dataAvailability == 'Upload Data'){
        dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
      } else{
        dat <- 0
      }
      
      totaled <- list(sum(dat), sum(dat^2))
      return(totaled)
    })
    
    OneMeanHypInfo <- reactive({
      hypTestSymbols <- list()
      
      if(input$altHypothesis == "3"){
        hypTestSymbols$alternative <- "greater"
        hypTestSymbols$nullHyp <- "\\mu \\leq"
        hypTestSymbols$altHyp <- "\\mu \\gt"
        hypTestSymbols$critAlph <- "\\alpha"
        hypTestSymbols$critSign <- ""
        hypTestSymbols$alphaVal <- SigLvl()
      }
      else if(input$altHypothesis == "2"){
        hypTestSymbols$alternative <- "two.sided"
        hypTestSymbols$nullHyp <- "\\mu ="
        hypTestSymbols$altHyp <- "\\mu \\neq"
        hypTestSymbols$critAlph <- "\\alpha/2"
        hypTestSymbols$critSign <- "\\pm"
        hypTestSymbols$alphaVal <- SigLvl()/2
      }
      else{
        hypTestSymbols$alternative <- "less"
        hypTestSymbols$nullHyp <- "\\mu \\geq"
        hypTestSymbols$altHyp <- "\\mu \\lt"
        hypTestSymbols$critAlph <- "\\alpha"
        hypTestSymbols$critSign <- "-"
        hypTestSymbols$alphaVal <- SigLvl()
      }
      
      return(hypTestSymbols)
    })
    
    OneMeanSigma <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability == 'Summarized Data') {
        sigmaKnown <- input$sigmaKnown
      } else if(input$dataAvailability == 'Enter Raw Data') {
        if(input$sigmaKnownRaw == 'rawKnown') {
          sigmaKnown <- "Known"
        } else {
          sigmaKnown <- "Unknown"
        }
      } else if(input$dataAvailability == 'Upload Data') {
        sigmaKnown <- input$sigmaKnownUpload
      }
      
      return(sigmaKnown)
    })
    
    OneMeanZIntSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean
      sigmaSampOne <- input$popuSD
      
      oneMeanZInt <- ZInterval(nSampOne, xbarSampOne, sigmaSampOne, ConfLvl())
      oneMeanZInt["Z Critical"] <- round(oneMeanZInt["Z Critical"], cvDigits)
      
      return(oneMeanZInt)
    })
    
    OneMeanZIntRaw <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability == 'Enter Raw Data') {
        dat <- createNumLst(input$sample1)
        popuSD <- input$popuSDRaw
        
      } else if(input$dataAvailability == 'Upload Data') {
        dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
        popuSD <- input$popuSDUpload
      }
      
      sampleSize <- length(dat)
      sampleMean <- mean(dat)
      
      oneMeanZInt <- ZInterval(sampleSize, sampleMean, popuSD, ConfLvl())
      oneMeanZInt["Z Critical"] <- round(oneMeanZInt["Z Critical"], cvDigits)
      
      return(oneMeanZInt)
    })
    
    OneMeanTIntSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean
      sSampOne <- input$sampSD
      
      oneMeanTInt <- TInterval(nSampOne, xbarSampOne, sSampOne, ConfLvl())
      oneMeanTInt["T Critical"] <- round(oneMeanTInt["T Critical"], cvDigits)
      
      return(oneMeanTInt)
    })
    
    OneMeanTIntRaw <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability == 'Enter Raw Data') {
        dat <- createNumLst(input$sample1)
        
      } else if(input$dataAvailability == 'Upload Data') {
        dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
      }
      
      sampleSize <- length(dat)
      sampleMean <- mean(dat)
      sampleSD <- sd(dat)
      
      oneMeanTInt <- TInterval(sampleSize, sampleMean, sampleSD, ConfLvl())
      oneMeanTInt["T Critical"] <- round(oneMeanTInt["T Critical"], cvDigits)
      
      return(oneMeanTInt)
    })
    
    OneMeanZTestSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean
      hypMeanSampOne <- input$hypMean
      sigmaSampOne <- input$popuSD
      
      oneMeanZTest <- ZTest(nSampOne, xbarSampOne, sigmaSampOne, hypMeanSampOne,
                            OneMeanHypInfo()$alternative, SigLvl())
      oneMeanZTest["Z Critical"] <- round(oneMeanZTest["Z Critical"], cvDigits)
      
      return (oneMeanZTest)
    })
    
    OneMeanZTestRaw <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability == 'Enter Raw Data') {
        dat <- createNumLst(input$sample1)
        popuSD <- input$popuSDRaw
      } else if (input$dataAvailability == 'Upload Data') {
        dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
        popuSD <- input$popuSDUpload
      }
      
      sampleSize <- length(dat)
      sampleMean <- mean(dat)
      hypMeanVal <- input$hypMean
      
      oneMeanZTest <- ZTest(sampleSize, sampleMean, popuSD, hypMeanVal,
                            OneMeanHypInfo()$alternative, SigLvl())
      oneMeanZTest["Z Critical"] <- round(oneMeanZTest["Z Critical"], cvDigits)
      
      return (oneMeanZTest)
    })
    
    OneMeanTTestSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean
      hypMeanSampOne <- input$hypMean
      sSampOne <- input$sampSD
      
      oneMeanTTest <- TTest(nSampOne, xbarSampOne, sSampOne, hypMeanSampOne,
                            OneMeanHypInfo()$alternative, SigLvl())
      oneMeanTTest["T Critical"] <- round(oneMeanTTest["T Critical"], cvDigits)
      
      return(oneMeanTTest)
    })
    
    OneMeanTTestRaw <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability == 'Enter Raw Data') {
        dat <- createNumLst(input$sample1)
        
      } else if (input$dataAvailability == 'Upload Data') {
        dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
      }
      
      sampleSize <- length(dat)
      sampleMean <- mean(dat)
      sampleSD <- sd(dat)
      hypMeanVal <- input$hypMean
      
      oneMeanTTest <- TTest(sampleSize, sampleMean, sampleSD, hypMeanVal,
                            OneMeanHypInfo()$alternative, SigLvl())
      oneMeanTTest["T Critical"] <- round(oneMeanTTest["T Critical"], cvDigits)
      
      return(oneMeanTTest)
    })
    
    GetOneMeanCI <- reactive({
      
      if(OneMeanSigma() == "Known"){
        
        if(input$dataAvailability == 'Summarized Data'){
          oneMeanCI <- OneMeanZIntSumm()
        } else {
          oneMeanCI <- OneMeanZIntRaw()
        }
        
      } else {
        
        if(input$dataAvailability == 'Summarized Data'){
          oneMeanCI <- OneMeanTIntSumm()
        } else {
          oneMeanCI <- OneMeanTIntRaw()
        }
        
      }
      
      return(oneMeanCI)
    })
    
    GetOneMeanHT <- reactive({
      
      if(OneMeanSigma() == "Known"){
        
        if(input$dataAvailability == 'Summarized Data'){
          oneMeanHT <- OneMeanZTestSumm()
        } else {
          oneMeanHT <- OneMeanZTestRaw()
        }
        
      } else {
        
        if(input$dataAvailability == 'Summarized Data'){
          oneMeanHT <- OneMeanTTestSumm()
        } else {
          oneMeanHT <- OneMeanTTestRaw()
        }
        
      }
      
      return(oneMeanHT)
    })
    
    ### ------------ Independent Sample Means reactives --------------------------
    
    IndMeansSummData <- reactive({
      req(si_iv$is_valid())
      
      summData <- list()
      
      summData$n1 <- input$sampleSize1
      summData$xbar1 <- input$sampleMean1
      summData$n2 <- input$sampleSize2
      summData$xbar2 <- input$sampleMean2
      summData$sigmaEqual <- input$bothsigmaEqual
      
      if(input$bothsigmaKnown == 'bothKnown'){
        summData$sd1 <- input$popuSD1
        summData$sd2 <- input$popuSD2
      } else {
        summData$sd1 <- input$sampSD1
        summData$sd2 <- input$sampSD2
      }
      
      return(summData)
    })
    
    IndMeansRawData <- reactive({
      req(si_iv$is_valid())
      
      rawData <- list()
      
      raw_sample1 <- createNumLst(input$raw_sample1)
      raw_sample2 <- createNumLst(input$raw_sample2)
      
      rawData$n1  <- length(raw_sample1)
      rawData$xbar1 <- mean(raw_sample1)
      rawData$n2  <- length(raw_sample2)
      rawData$xbar2 <- mean(raw_sample2)
      rawData$sigmaEqual <- input$bothsigmaEqualRaw
      
      if(input$bothsigmaKnownRaw == 'bothKnown'){
        rawData$sd1 <- input$popuSDRaw1
        rawData$sd2 <- input$popuSDRaw2
      } else {
        rawData$sd1 <- sd(raw_sample1)
        rawData$sd2 <- sd(raw_sample2)
      }
      
      return(rawData)
    })
    
    IndMeansUploadData <- eventReactive(input$indMeansUserData, {
      
      ext <- tools::file_ext(input$indMeansUserData$name)
      ext <- tolower(ext)
      
      switch(ext,
             csv = read_csv(input$indMeansUserData$datapath, show_col_types = FALSE),
             xls = read_xls(input$indMeansUserData$datapath),
             xlsx = read_xlsx(input$indMeansUserData$datapath),
             txt = read_tsv(input$indMeansUserData$datapath, show_col_types = FALSE),
             
             validate("Improper file format")
      )
    })
    
    GetMeansUploadData <- reactive({
      req(input$indMeansUplSample1, input$indMeansUplSample2)
      
      dat <- list()
      
      sample1 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample1]))
      sample2 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample2]))
      
      dat$n1  <- length(sample1)
      dat$xbar1 <- mean(sample1)
      dat$n2  <- length(sample2)
      dat$xbar2 <- mean(sample2)
      dat$sigmaEqual <- input$bothsigmaEqualUpload
      
      if(input$bothsigmaKnownUpload == 'bothKnown'){
        dat$sd1 <- input$popuSDUpload1
        dat$sd2 <- input$popuSDUpload2
      } else {
        dat$sd1 <- sd(sample1)
        dat$sd2 <- sd(sample2)
      }
      
      return(dat)
    })
    
    IndMeansSigmaKnown <- reactive({
      
      if (input$dataAvailability2 == 'Summarized Data') {
        sigmaKnown <- input$bothsigmaKnown
      } else if(input$dataAvailability2 == 'Enter Raw Data'){
        sigmaKnown <- input$bothsigmaKnownRaw
      } else if(input$dataAvailability2 == 'Upload Data'){
        sigmaKnown <- input$bothsigmaKnownUpload
      }
      
      return(sigmaKnown)
    })
    
    IndMeansHypInfo <- reactive({
      hypTestSymbols <- list()
      
      if(input$altHypothesis2 == "3"){
        hypTestSymbols$alternative <- "greater"
        hypTestSymbols$nullHyp <- "\\leq"
        hypTestSymbols$altHyp <- "\\gt"
        hypTestSymbols$critAlph <- "\\alpha"
        hypTestSymbols$critSign <- ""
        hypTestSymbols$alphaVal <- SigLvl()
      }
      else if(input$altHypothesis2 == "2"){
        hypTestSymbols$alternative <- "two.sided"
        hypTestSymbols$nullHyp <- "="
        hypTestSymbols$altHyp <- "\\neq"
        hypTestSymbols$critAlph <- "\\alpha/2"
        hypTestSymbols$critSign <- "\\pm"
        hypTestSymbols$alphaVal <- SigLvl()/2
      }
      else{
        hypTestSymbols$alternative <- "less"
        hypTestSymbols$nullHyp <- "\\geq"
        hypTestSymbols$altHyp <- "\\lt"
        hypTestSymbols$critAlph <- "\\alpha"
        hypTestSymbols$critSign <- "-"
        hypTestSymbols$alphaVal <- SigLvl()
      }
      
      return(hypTestSymbols)
    })
    
    IndMeansZInt <- reactive({
      req(si_iv$is_valid())
      
      if (input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data') {
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      twoSampZInt <- TwoSampZInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, ConfLvl())
      twoSampZInt["Z Critical"] <- round(twoSampZInt["Z Critical"], cvDigits)
      
      return(twoSampZInt)
    })
    
    IndMeansTInt <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data'){
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      twoSampTInt <- TwoSampTInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, data$sigmaEqual, ConfLvl())
      twoSampTInt["T Critical"] <- round(twoSampTInt["T Critical"], cvDigits)
      
      return(twoSampTInt)
    })
    
    IndMeansZTest <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data'){
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      muNaught <- input$indMeansMuNaught
      
      twoSampZTest <- TwoSampZTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, IndMeansHypInfo()$alternative, SigLvl(), muNaught)
      twoSampZTest["Z Critical"] <- round(twoSampZTest["Z Critical"], cvDigits)
      
      return(twoSampZTest)
    })
    
    IndMeansTTest <- reactive({
      req(si_iv$is_valid())
      
      if(input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data'){
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      muNaught <- input$indMeansMuNaught
      
      twoSampTTest <- TwoSampTTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, data$sigmaEqual, IndMeansHypInfo()$alternative, SigLvl(), muNaught)
      twoSampTTest["T Critical"] <- round(twoSampTTest["T Critical"], cvDigits)
      
      return(twoSampTTest)
    })
    
    ### ------------ Wilcoxon Rank Sum Reactives -----------------------------------
    
    WilcoxonUploadData <- eventReactive(input$wilcoxonUpl, {
      
      ext <- tools::file_ext(input$wilcoxonUpl$name)
      ext <- tolower(ext)
      
      switch(ext,
             csv = read_csv(input$wilcoxonUpl$datapath, show_col_types = FALSE),
             xls = read_xls(input$wilcoxonUpl$datapath),
             xlsx = read_xlsx(input$wilcoxonUpl$datapath),
             txt = read_tsv(input$wilcoxonUpl$datapath, show_col_types = FALSE),
             
             validate("Improper file format")
      )
    })
    
    CheckRankSumUploadSamples <- eventReactive (c(input$wilcoxonUpl1,input$wilcoxonUpl2), {
      if(input$wilcoxonUpl1 == "" | input$wilcoxonUpl2 == "") {
        return(0)
      } else {
        before <- unlist(WilcoxonUploadData()[, input$wilcoxonUpl1])
        after <- unlist(WilcoxonUploadData()[, input$wilcoxonUpl2])
        difference <- length(na.omit(before)) - length(na.omit(after))
        return(difference)
      }
    })
    wRankSumTInt <- reactive({
      req(si_iv$is_valid())
      
      data <- GetwRankSumMeansData()
      
      wRankSumTInt <- TInterval(data$n, data$dbar, data$sd, ConfLvl())
      wRankSumTInt["T Critical"] <- round(wRankSumTInt["T Critical"], cvDigits)
      
      return(wRankSumTInt)
    })
    
    wRankSumTTest <- reactive({
      req(si_iv$is_valid())
      
      data <- GetwRankSumMeansData()
      
      n_total <- data$n1 + data$n2
      pooled_mean <- (data$mean1 * data$n1 + data$mean2 * data$n2) / n_total
      combined_data <- c(data$samp1, data$samp2)
      pooled_sd <- sd(combined_data)
      
      wRankSumTTest <- TTest(n_total, pooled_mean, pooled_sd, 0, IndMeansHypInfo()$alternative, SigLvl())
      wRankSumTTest["T Critical"] <- round(wRankSumTTest["T Critical"], cvDigits)
      
      return(wRankSumTTest)
    })
    
    wilcoxonRankedData <- reactive({
      req(input$wilcoxonRankSumTestData)
      
      if (input$wilcoxonRankSumTestData == 'Enter Raw Data') {
        sample1_vals <- as.numeric(unlist(strsplit(input$rankSumRaw1, ",")))
        sample2_vals <- as.numeric(unlist(strsplit(input$rankSumRaw2, ",")))
        
        group1_name <- "Sample 1"
        group2_name <- "Sample 2"
        
      } else if (input$wilcoxonRankSumTestData == 'Upload Data') {
        req(wilcoxonUpload_iv$is_valid())
        uploaded_data <- WilcoxonUploadData()
        req(input$wilcoxonUpl1, input$wilcoxonUpl2)
        
        sample1_vals <- na.omit(uploaded_data[[input$wilcoxonUpl1]])
        sample2_vals <- na.omit(uploaded_data[[input$wilcoxonUpl2]])
        
        group1_name <- input$wilcoxonUpl1
        group2_name <- input$wilcoxonUpl2
        
      } else {
        return(NULL)
      }
      
      combined_data <- data.frame(
        values = c(sample1_vals, sample2_vals),
        ind = c(rep(group1_name, length(sample1_vals)), 
                rep(group2_name, length(sample2_vals)))
      )
      
      ranked_data <- combined_data %>%
        dplyr::mutate(Rank = rank(values, ties.method = "average")) %>%
        dplyr::select(Group = ind, Value = values, Rank) %>%
        dplyr::arrange(Group, Rank)
      
      return(ranked_data)
    })
    
    output$wilcoxonRankSumDataRanks <- renderUI({
      req(wilcoxonRankedData())
      RankedTableOutput(wilcoxonRankedData())
    })
    
    ### ------------ Dependent Means Reactives -----------------------------------
    
    DepMeansUploadData <- eventReactive(input$depMeansUserData, {
      
      ext <- tools::file_ext(input$depMeansUserData$name)
      ext <- tolower(ext)
      
      switch(ext,
             csv = read_csv(input$depMeansUserData$datapath, show_col_types = FALSE),
             xls = read_xls(input$depMeansUserData$datapath),
             xlsx = read_xlsx(input$depMeansUserData$datapath),
             txt = read_tsv(input$depMeansUserData$datapath, show_col_types = FALSE),
             
             validate("Improper file format")
      )
    })
    
    CheckDepUploadSamples <- eventReactive (c(input$depMeansUplSample1,
                                              input$depMeansUplSample2), {
                                                
                                                if(input$depMeansUplSample1 == "" | input$depMeansUplSample2 == "") {
                                                  return(0)
                                                } else {
                                                  before <- unlist(DepMeansUploadData()[, input$depMeansUplSample1])
                                                  after <- unlist(DepMeansUploadData()[, input$depMeansUplSample2])
                                                  difference <- length(na.omit(before)) - length(na.omit(after))
                                                  return(difference)
                                                }
                                              })
    
    DepMeansTInt <- reactive({
      req(si_iv$is_valid())
      
      data <- GetDepMeansData()
      
      depMeansTInt <- TInterval(data$n, data$dbar, data$sd, ConfLvl())
      depMeansTInt["T Critical"] <- round(depMeansTInt["T Critical"], cvDigits)
      
      return(depMeansTInt)
    })
    
    
    DepMeansTTest <- reactive({
      req(si_iv$is_valid() && depmeansrawsd_iv$is_valid())
      
      data <- GetDepMeansData()
      
      depMeansTTest <- TTest(data$n, data$dbar, data$sd, data$muNaught, IndMeansHypInfo()$alternative, SigLvl())
      depMeansTTest["T Critical"] <- round(depMeansTTest["T Critical"], cvDigits)
      
      return(depMeansTTest)
      
    })
    
    ### ------------ Two Prop Reactives ------------------------------------------
    checkTwoProp <- reactive({
      
      if(is.na(input$numSuccesses1) || is.na(input$numSuccesses2)) {
        return(-1)
      } else {
        return(input$numSuccesses1 + input$numSuccesses2)
      }
      
    })
    
    ### ------------ Two Pop Var Reactives --------------------------------------
    GetTwoPopVarData <- reactive({
      req(si_iv$is_valid())
      
      dat <- list()
      
      if (input$dataAvailability3 == "Summary") {
        dat$n1 <- input$SDSampleSize1
        dat$n2 <- input$SDSampleSize2
        dat$sd1 <- input$stdDev1
        dat$sd2 <- input$stdDev2
      } else if (input$dataAvailability3 == "Variance") {
        dat$n1 <- input$n1
        dat$n2 <- input$n2
        dat$sd1 <- input$s1sq
        dat$sd2 <- input$s2sq
      }
      
      return(dat)
    })
    
    GetTwoPopVarRawData <- reactive({
      req(si_iv$is_valid())
      
      dat <- list()
      
      if(input$dataAvailability3 == 'Enter Raw Data') {
        samp1 <- createNumLst(input$rawSamp1SD)
        samp2 <- createNumLst(input$rawSamp2SD)
      } else if(input$dataAvailability3 == 'Upload'){
        # future work, uploading files not implemented currently
      }
      
      dat$sample1 <- samp1
      dat$sample2 <- samp2
      dat$n1 <- length(samp1)
      dat$n2 <- length(samp2)
      dat$sd1 <- sd(dat$sample1)
      dat$sd2 <- sd(dat$sample2)
      
      return(dat)
    })
    
    TwoPopVarHypInfo <- reactive({
      hypTestSymbols <- list()
      
      if (input$altHypothesis2 == "3") {
        hypTestSymbols$alternative <- "greater"
        hypTestSymbols$nullHyp <- "\\sigma^2_1 \\leq \\sigma^2_2"
        hypTestSymbols$altHyp <- "\\sigma^2_1 \\gt \\sigma^2_2"
        hypTestSymbols$critAlph <- "\\alpha"
        hypTestSymbols$critSign <- ""
        hypTestSymbols$alphaVal <- SigLvl()
      } else if (input$altHypothesis2 == "2") {
        hypTestSymbols$alternative <- "two.sided"
        hypTestSymbols$nullHyp <- "\\sigma^2_1 = \\sigma^2_2"
        hypTestSymbols$altHyp <- "\\sigma^2_1 \\neq \\sigma^2_2"
        hypTestSymbols$critAlph <- "\\alpha/2"
        hypTestSymbols$critSign <- "\\pm"
        hypTestSymbols$alphaVal <- SigLvl() / 2
      } else { # less
        hypTestSymbols$alternative <- "less"
        hypTestSymbols$nullHyp <- "\\sigma^2_1 \\geq \\sigma^2_2"
        hypTestSymbols$altHyp <- "\\sigma^2_1 \\lt \\sigma^2_2"
        hypTestSymbols$critAlph <- "\\alpha"
        hypTestSymbols$critSign <- "-"
        hypTestSymbols$alphaVal <- SigLvl()
      }
      
      return(hypTestSymbols)
    })
    
    GetAllTwoPopVarData <- reactive({
      if(input$dataAvailability3 == 'Enter Raw Data') {
        data <- GetTwoPopVarRawData()
      } else if(input$dataAvailability3 == 'Upload Data') {
        # future work, upload not currently implemented
      } else { # Summary or Variance
        data <- GetTwoPopVarData()
      }
      return(data)
    })
    
    ### ------------ ANOVA Reactives ---------------------------------------------
    anovaUploadData <- eventReactive(input$anovaUserData, {
      ext <- tools::file_ext(input$anovaUserData$name)
      ext <- tolower(ext)
      
      switch(ext,
             csv = read_csv(input$anovaUserData$datapath, show_col_types = FALSE),
             xls = read_xls(input$anovaUserData$datapath),
             xlsx = read_xlsx(input$anovaUserData$datapath),
             txt = read_tsv(input$anovaUserData$datapath, show_col_types = FALSE),
             
             validate("Improper file format.")
      )
    })
    
    anovaStackedIsValid <- eventReactive({input$anovaResponse
      input$anovaFactors}, {
        valid <- TRUE
        
        if(!is.null(input$anovaResponse) && !is.null(input$anovaFactors)) {
          if(input$anovaResponse == input$anovaFactors) {
            valid <- FALSE
          }
        }
        
        return(valid)
      })
    
    anovaOneWayResults <- reactive({
      req(si_iv$is_valid)
      
      results <- list()
      
      if (input$anovaFormat == "Multiple") {
        anovaData <- stack(anovaUploadData()[,input$anovaMultiColumns])
        factorCol <- "ind"
        factorNames <- levels(anovaData[,factorCol])
      } else {
        anovaData <- anovaUploadData()
        colnames(anovaData)[colnames(anovaData) == input$anovaFactors] <- "ind"
        colnames(anovaData)[colnames(anovaData) == input$anovaResponse] <- "values"
        anovaData <- anovaData %>% dplyr::mutate(ind = factor(ind))
        factorCol <- "ind"
        factorNames <- levels(anovaData$ind)
      }
      
      anovaData <- na.omit(anovaData)
      totalCount <- nrow(anovaData)
      numFactors <- length(factorNames)
      anovaTest <- aov(formula = values ~ ind, data = anovaData)
      
      results$data <- anovaData
      results$count <- totalCount
      results$factorCol <- factorCol
      results$numFactors <- numFactors
      results$factorNames <- factorNames
      results$fit <- anovaTest
      results$residuals <- anovaTest$residuals
      results$test <- anova(anovaTest)
      
      return(results)
    })
    
    ### ------------ Kruskal-Wallis Reactives ------------------------------------   
    kwUploadData <- eventReactive(input$kwUserData, {
      kwUploadData_func(input$kwUserData)
    })
    
    kwDisplayState <- reactiveVal("none")  # "none", "raw", "analysis
    
    kwStackedIsValid <- eventReactive({input$kwResponse
      input$kwFactors}, {
        kwStackedIsValid_func(input$kwResponse, input$kwFactors)
      })
    
    kwResults <- reactive({
      req(si_iv$is_valid())
      kwResults_func(
        si_iv$is_valid,
        input$kwFormat,
        input$kwMultiColumns,
        kwUploadData(),
        input$kwFactors,
        input$kwResponse
      )
    })
    
    ### Chi-Square Reactives ----
    # chiSqData2x2 <- reactive({
    #   suppressWarnings(as.numeric(input$chiSqInput2x2))
    # })
    #
    # chiSqData2x3 <- reactive({
    #   suppressWarnings(as.numeric(input$chiSqInput2x3))
    # })
    #
    # chiSqData3x2 <- reactive({
    #   suppressWarnings(as.numeric(input$chiSqInput3x2))
    # })
    #
    # chiSqData3x3 <- reactive({
    #   suppressWarnings(as.numeric(input$chiSqInput3x3))
    # })
    
    chiSqActiveData <- reactive({
      if(input$chisquareDimension == "2 x 2") {
        active <- input$chiSqInput2x2
      } else if (input$chisquareDimension == "2 x 3") {
        active <- input$chiSqInput2x3
      } else if (input$chisquareDimension == "3 x 2") {
        active <- input$chiSqInput3x2
      } else if (input$chisquareDimension == "3 x 3") {
        active <- input$chiSqInput3x3
      }
      
      activeData <- list(active, suppressWarnings(as.numeric(active)))
      names(activeData) <- c("data", "numeric")
      
      return(activeData)
    })
    
    chiSqActiveMatrix <- reactive({
      active <- matrix(chiSqActiveData()$numeric, ncol = ncol(chiSqActiveData()$data))
      colnames(active) <- colnames(chiSqActiveData()$data)
      rownames(active) <- rownames(chiSqActiveData()$data)
      
      return(active)
    })
    
    chiSqResults <- reactive({
      req(si_iv$is_valid())
      return(suppressWarnings(ChiSquareTest(chiSqActiveMatrix(), input$chiSquareYates)))
    })
    
    chiSqTotaled <- reactive({
      if(!any(is.na(chiSqActiveData()$numeric))){
        
        chiSqTotaledMatrix <- chiSqActiveMatrix()
        chiSqTotaledMatrix <- cbind(chiSqTotaledMatrix, Total = round(rowSums(chiSqTotaledMatrix), 4))
        chiSqTotaledMatrix <- rbind(chiSqTotaledMatrix, Total = round(colSums(chiSqTotaledMatrix), 4))
        
        return(chiSqTotaledMatrix)
      }
    })
    
    fishersResults <- reactive({
      req(si_iv$is_valid())
      return(fisher.test(chiSqActiveMatrix()))
    })
    
    
    criticalValue <- reactive({
      
      if(input$confLeveln == "90%") {
        critVal <- 1.645
      } else if(input$confLeveln == "95%") {
        critVal <- 1.96
      } else if(input$confLeveln == "99%") {
        critVal <- 2.576
      }
      
      return(critVal)
    })
    
    #  ========================================================================= #
    ## -------- Outputs --------------------------------------------------------
    #  ========================================================================= #
    
    ### ------------ Validation --------------------------------------------------
    
    #### ---------------- One Mean Validation
    output$inferenceValidation <- renderUI({
      
      if(!onemean_iv$is_valid()) {
        validate(
          need(input$sampleSize, "Sample size (n) must be an integer greater than 1.") %then%
            need(input$sampleSize > 1 & input$sampleSize %% 1 == 0, "Sample size (n) must be an integer greater than 1."),
          need(input$sampleMean, "Sample mean required."),
          errorClass = "myClass")
      }
      
      if (!onemeanraw_iv$is_valid()) {
        validate(
          need(input$sample1, "Sample Data required.") %then%
            need(length(createNumLst(input$sample1)) > 1, "Sample Data requires a minimum of 2 data points."),
          if (input$sigmaKnownRaw == "rawKnown") {
            need(input$popuSDRaw,"Population Standard Deviation is required.") %then%
              need(input$popuSDRaw > 0, "Population Standard Deviation must be positive.")
          },
          errorClass = "myClass"
        )
        
        if (input$sigmaKnownRaw == "rawUnknown") {
          sampleData <- createNumLst(input$sample1)
          msg <- getTTestErrorMsg(sampleData)
          if (!is.null(msg)) {
            validate(
              need(FALSE, msg),
              errorClass = "myClass"
            )
          }
        }
      }
      
      if(!onemeansdknown_iv$is_valid()) {
        validate(
          need(input$popuSD & input$popuSD > 0, "Population Standard Deviation must be positive."),
          #need(input$popuSD > 0, "Population Standard Deviation must be greater than 0"),
          errorClass = "myClass")
      }
      
      if(!onemeansdunk_iv$is_valid()) {
        validate(
          need(input$sampSD && input$sampSD > 0, "Sample Standard Deviation (s) must be positive."),
          errorClass = "myClass")
      }
      
      if(!onemeanupload_iv$is_valid()) {
        
        if(is.null(input$oneMeanUserData)) {
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$oneMeanStatus) && fileInputs$oneMeanStatus == 'uploaded', "Please upload a file."),
          errorClass = "myClass")
        
        validate(
          need(nrow(OneMeanUploadData()) != 0, "File is empty."),
          need(nrow(OneMeanUploadData()) > 2, "Samples must include at least 2 observations."),
          errorClass = "myClass")
      }
      
      if(!onemeanuploadvar_iv$is_valid()) {
        validate(
          need(input$oneMeanVariable != "", "Please select a column for analysis."),
          errorClass = "myClass"
        )
        data <- OneMeanUploadData()
        col <- input$oneMeanVariable
        
        if (!is.null(data) && !is.null(col) && input$sigmaKnownUpload == "Unknown") {
          msg <- getTTestErrorMsg(data[[col]])
          if (!is.null(msg)) {
            validate(
              need(FALSE, msg),
              errorClass = "myClass"
            )
          }
        }
      }
      
      
      if(!onemeanuploadsd_iv$is_valid()) {
        validate(
          need(input$popuSDUpload && input$popuSDUpload > 0, "Population Standard Deviation must be positive."),
          errorClass = "myClass")
      }
      
      if(!onemeanht_iv$is_valid()) {
        validate(
          need(input$hypMean, "Hypothesized value of the Population Mean is required."),
          errorClass = "myClass")
      }
      
      #### ---------------- One Standard Deviation Validation
      if(!oneSD_iv$is_valid()) {
        validate(
          need(input$SSDSampleSize, "Sample size (n) is required.") %then%
            need(input$SSDSampleSize > 1 & input$SSDSampleSize %% 1 == 0, "Sample size (n) must be an integer greater than 1."),
          errorClass = "myClass")
      }
      
      if(!oneSD_iv$is_valid()) {
        validate(
          need(input$SSDStdDev, "Sample Standard Deviation (s) is required.") %then%
            need(input$SSDStdDev > 0, "Sample Standard Deviation (s) must be positive."),
          errorClass = "myClass")
      }
      
      ## DONE: these messages are for debugging purposes only.
      ## message(sprintf("Should oneSDht_iv be testing? %s", oneSDht_iv$condition()()))
      if(!oneSDht_iv$is_valid()) {
        ## message("The one sample standard deviation hypothesis test InputValidator object is invalid!")
        validate(
          need(input$hypStdDeviation, "Hypothesized Population Standard Deviation (\u03C3\u2080) is required.") %then%
            need(input$hypStdDeviation > 0 && input$hypStdDeviation < 1, "Hypothesized Population Standard Deviation (\u03C3\u2080) must be positive. (\u03C3\u2080 > 0)."),
          errorClass = "myClass")
      }
      
      #### ---------------- One Prop Validation
      if(!oneprop_iv$is_valid()) {
        validate(
          need(input$numSuccesses, "Numeric value for Number of Successes (x) required"),
          need(input$numTrials, "Numeric value for Number of Trials (n) required"),
          errorClass = "myClass")
        
        validate(
          need(input$numSuccesses %% 1 == 0, "Number of Successes (x) must be an integer"),
          need(input$numSuccesses >= 0, "Number of Successes (x) cannot be negative"),
          need(input$numTrials %% 1 == 0, "Number of Trials (n) must be an integer"),
          need(input$numTrials > 0, "Number of Trials (n) must be greater than 0") %then%
            need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),
          errorClass = "myClass")
      } else if(input$siMethod == '1' && input$popuParameter == 'Population Proportion') {
        req(input$numSuccesses >= 0 && input$numTrials)
        validate(
          need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),
          errorClass = "myClass")
      }
      
      if(!onepropht_iv$is_valid()) {
        validate(
          need(input$hypProportion, "Hypothesized value of the Population Proportion must be between 0 and 1") %then%
            need(input$hypProportion > 0 && input$hypProportion < 1, "Hypothesized value of the Population Proportion must be between 0 and 1"),
          errorClass = "myClass")
      }
      
      #### ---------------- Independent Population Means Validation
      if(!indmeanssumm_iv$is_valid()) {
        validate(
          need(input$sampleSize1, "Sample Size 1 (n1) must be an integer greater than 1.") %then%
            need(input$sampleSize1 > 1 & input$sampleSize1 %% 1 == 0, "Sample Size 1 (n1) must be an integer greater than 1."),
          need(input$sampleMean1, "Sample Mean 1 required."),
          need(input$sampleSize2, "Sample Size 2 (n2) must be an integer greater than 1.") %then%
            need(input$sampleSize2 > 1 & input$sampleSize2 %% 1 == 0, "Sample Size 2 (n2) must be an integer greater than 1."),
          need(input$sampleMean2, "Sample Mean 2 required."),
          errorClass = "myClass")
      }
      
      if(!indmeanssdknown_iv$is_valid())
      {
        validate(
          need(input$popuSD1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
          need(input$popuSD2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
          errorClass = "myClass")
      }
      
      if(!indmeanssdunk_iv$is_valid())
      {
        validate(
          need(input$sampSD1 && input$sampSD1 > 0, "Sample Standard Deviation (s1) must be positive."),
          need(input$sampSD2 && input$sampSD2 > 0, "Sample Standard Deviation (s2) must be positive."),
          errorClass = "myClass")
      }
      
      if(!indmeansraw_iv$is_valid()) {
        validate(
          need(input$raw_sample1, "Sample 1 requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$raw_sample1)) > 2, "Sample Data requires a minimum of 3 data points."),
          need(input$raw_sample2, "Sample 2 requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$raw_sample2)) > 2, "Sample Data requires a minimum of 3 data points."),
          errorClass = "myClass")
        
        validate("Samples require a minimum of 3 data points.")
      }
      
      if(!indmeansrawsd_iv$is_valid()) {
        validate(
          need(input$popuSDRaw1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
          need(input$popuSDRaw2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
          errorClass = "myClass")
      }
      
      if(!indmeansrawsdunk_iv$is_valid()) {
        validate(
          need(sd(createNumLst(input$raw_sample1)) != 0 && sd(createNumLst(input$raw_sample2)) != 0, "The test statistic (t) will be undefined when the sample standard deviation of Sample 1 and Sample 2 are both 0."),
          errorClass = "myClass")
      }
      
      if(!indmeansupload_iv$is_valid()) {
        
        if(is.null(input$indMeansUserData)) {
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$indMeansStatus) && fileInputs$indMeansStatus == 'uploaded', "Please upload a file."),
          errorClass = "myClass")
        
        validate(
          need(nrow(IndMeansUploadData()) != 0, "File is empty."),
          need(ncol(IndMeansUploadData()) > 1, "File must contain at least 2 distinct samples to choose from for analysis."),
          need(nrow(IndMeansUploadData()) > 2, "Samples must include at least 2 observations."),
          errorClass = "myClass")
      }
      
      if(!indmeansuploadvar_iv$is_valid()) {
        validate(
          need(input$indMeansUplSample1, "Please select a column for Sample 1."),
          need(input$indMeansUplSample2, "Please select a column for Sample 2."),
          errorClass = "myClass")
        
        validate(
          need(!checkNumeric(IndMeansUploadData(), input$indMeansUplSample1),
               "Sample 1 must be numeric."),
          errorClass = "myClass"
        )
        
        validate(
          need(!checkNumeric(IndMeansUploadData(), input$indMeansUplSample2),
               "Sample 2 must be numeric."),
          errorClass = "myClass"
        )
        
        if (input$bothsigmaKnownUpload == "bothUnknown") {
          data <- GetMeansUploadData()
          sd1 <- data$sd1
          sd2 <- data$sd2
          
          validate(
            need(!(sd1 == 0 && sd2 == 0),
                 "Both selected columns have a sample standard deviation of 0, so the test statistic (t) is undefined."),
            errorClass = "myClass"
          )
        }
      }
      
      if(!indmeansuploadsd_iv$is_valid()) {
        validate(
          need(input$popuSDUpload1 && input$popuSDUpload1 > 0, "Population Standard Deviation 1 must be positive."),
          need(input$popuSDUpload2 && input$popuSDUpload2 > 0, "Population Standard Deviation 2 must be positive."),
          errorClass = "myClass")
      }
      
      if(!indmeansmunaught_iv$is_valid()) {
        validate(
          need(input$indMeansMuNaught, "Hypothesized value of the Population Mean Difference is required."),
          errorClass = "myClass")
      }
      
      ### ---------------- Wilcoxon Rank Sum Validation
      if(!wilcoxonraw_iv$is_valid()) {
        validate(
          need(input$rankSumRaw1, "Sample 1 data requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$rankSumRaw1)) > 2, "Sample 1 data requires a minimum of 3 data points."),
          need(input$rankSumRaw2, "Sample 2 data requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$rankSumRaw2)) > 2, "Sample 2 data requires a minimum of 3 data points."),
          errorClass = "myClass")
        
        validate(
          need(length(createNumLst(input$rankSumRaw1)) == length(createNumLst(input$rankSumRaw2)), "Same number of data points required for Sample 1 and Sample 2."),
          errorClass = "myClass")
      }
      
      if(!wilcoxonUpload_iv$is_valid()) {
        
        if(is.null(input$wilcoxonUpl)) {
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$rankSumStatus) && fileInputs$rankSumStatus == 'uploaded', "Please upload a file."),
          errorClass = "myClass")
        
        validate(
          need(nrow(WilcoxonUploadData()) > 0, "File is empty."),
          need(ncol(WilcoxonUploadData()) >= 2, "File must contain at least 2 distinct sample 1 and sample 2 sets of data to choose from for analysis."),
          need(nrow(WilcoxonUploadData()) >= 3, "Samples must include at least 3 observations."),
          errorClass = "myClass")
      }
      
      if(!wilcoxonRanksuploadvars_iv$is_valid()) {
        validate(
          need(input$wilcoxonUpl1, "Please select a column for sample 1."),
          need(input$wilcoxonUpl2, "Please select a column for sample 2."),
          need(CheckRankSumUploadSamples() == 0, "Same number of data points required for Sample 1 and Sample 2."),
          errorClass = "myClass")
      }

      #### ---------------- Dependent Population Means Validation
      if(!depmeansraw_iv$is_valid()) {
        validate(
          need(input$before, "Sample 1 data requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$before)) > 2, "Sample 1 data requires a minimum of 3 data points."),
          need(input$after, "Sample 2 data requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$after)) > 2, "Sample 2 data requires a minimum of 3 data points."),
          errorClass = "myClass")
        
        validate(
          need(length(createNumLst(input$before)) == length(createNumLst(input$after)), "Same number of data points required for Sample 1 and Sample 2."),
          errorClass = "myClass")
      }
      
      if(!depmeansupload_iv$is_valid()) {
        
        if(is.null(input$depMeansUserData)) {
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$depMeansStatus) && fileInputs$depMeansStatus == 'uploaded', "Please upload a file."),
          errorClass = "myClass")
        
        validate(
          need(nrow(DepMeansUploadData()) > 0, "File is empty."),
          need(ncol(DepMeansUploadData()) >= 2, "File must contain at least 2 distinct 'Before' and 'After' sets of data to choose from for analysis."),
          need(nrow(DepMeansUploadData()) >= 3, "Samples must include at least 3 observations."),
          errorClass = "myClass")
      }
      
      if(!depmeansuploadvars_iv$is_valid()) {
        validate(
          need(input$depMeansUplSample1, "Please select a column for Sample 1 (e.g. Before, Pre-Treatment, Baseline)."),
          need(input$depMeansUplSample2, "Please select a column for Sample 2 (e.g. After, Post-Treatment, Follow-Up)."),
          need(CheckDepUploadSamples() == 0, "Same number of data points required for Sample 1 and Sample 2."),
          errorClass = "myClass")
       
         validate(
          need(!checkNumeric(DepMeansUploadData(), input$depMeansUplSample1),
               "Sample 1 must be numeric."),
          errorClass = "myClass"
        )
        
        validate(
          need(!checkNumeric(DepMeansUploadData(), input$depMeansUplSample2),
               "Sample 2 must be numeric."),
          errorClass = "myClass"
        )
        
        validate(
          need(
            !(input$depMeansUplSample1 != "" &&
                input$depMeansUplSample2 != "" &&
                (input$depMeansUplSample1 == input$depMeansUplSample2 ||
                   GetDepMeansData()$sd == 0)),
            if (input$inferenceType2 == "Hypothesis Testing") {
              "The test statistic (t) will be undefined for sample data with a sample standard deviation of difference (sd) = 0."
            } else {
              "The confidence interval results in (0,0) when the sample standard deviation of difference (sd) = 0."
            }
          ),
          errorClass = "myClass"
        )
      }
      
      if(!depmeansraw_iv$is_valid()) {
        
        if(input$inferenceType2 == 'Hypothesis Testing'){
          sdValidation <- "The test statistic (t) will be undefined for sample data with a sample standard deviation of difference (sd) = 0."
        } else {
          sdValidation <- paste0("The confidence interval results in (",
                                 GetDepMeansData()$dbar,
                                 ",", GetDepMeansData()$dbar,
                                 ") when the sample standard deviation of difference (sd) = 0.")
        }
        validate(
          need(GetDepMeansData()$sd != 0, sdValidation),
          errorClass = "myClass")
      }
      
      if(!depmeansmunaught_iv$is_valid()) {
        validate(
          need(input$depMeansMuNaught, "Hypothesized value of the Population Mean Difference is required."),
          errorClass = "myClass")
      }
      
      #### ---------------- Two Population Proportion Validation
      if (!twopropht_iv$is_valid()) {
        validate(
          need(checkTwoProp() > 0, "The test statistic (t) will be undefined when the Number of Successes 1 (x1) and Number of Successes 2 (x2) are both 0."),
          need(!(input$numSuccesses1 == input$numTrials1 && input$numSuccesses2 == input$numTrials2),
               "The pooled proportion equals 1, which results in an undefined test statistic (z). This happens when the number of successes equals the number of trials for both samples."),
          errorClass = "myClass"
        )
      }
      
      if(!twoprop_iv$is_valid()) {
        validate(
          need(input$numSuccesses1, "Numeric value for Number of Successes 1 (x1) required"),
          need(input$numTrials1, "Numeric value for Number of Trials 1 (n1) required"),
          need(input$numSuccesses2, "Numeric value for Number of Successes 2 (x2) required"),
          need(input$numTrials2, "Numeric value for Number of Trials 2 (n2) required"),
          errorClass = "myClass")
        
        validate(
          need(input$numSuccesses1 %% 1 == 0, "Number of Successes 1 (x1) must be an integer"),
          need(input$numSuccesses1 >= 0, "Number of Successes 1 (x1) cannot be negative"),
          need(input$numTrials1 %% 1 == 0, "Number of Trials 1 (n1) must be an integer"),
          need(input$numTrials1 > 0, "Number of Trials 1 (n1) must be greater than 0"),
          need(input$numSuccesses2 %% 1 == 0, "Number of Successes 2 (x2) must be an integer"),
          need(input$numSuccesses2 >= 0, "Number of Successes 2 (x2) cannot be negative"),
          need(input$numTrials2 %% 1 == 0, "Number of Trials 2 (n2) must be an integer"),
          need(input$numTrials2 > 0, "Number of Trials 2 (n2) must be greater than 0"),
          errorClass = "myClass")
        
      } else if (input$siMethod == '2' && input$popuParameters == 'Population Proportions') {
        
        validate(
          need(input$numSuccesses1 <= input$numTrials1, "Number of Successes 1 (x1) cannot be greater than Number of Trials 1 (n1)"),
          need(input$numSuccesses2 <= input$numTrials2, "Number of Successes 2 (x2) cannot be greater than Number of Trials 2 (n2)"),
          errorClass = "myClass")
        
      }
      
      if(!twopropdiffnaught_iv$is_valid()) {
        validate(
          need(input$propDiffNaught != "", "Hypothesized value of the Population Proportion Difference is required.") %then%
            need(input$propDiffNaught >= -1, "Hypothesized value of the Population Proportion Difference must be between -1 and +1, inclusive.") %then%
            need(input$propDiffNaught <= 1, "Hypothesized value of the Population Proportion Difference must be between -1 and +1, inclusive."),
          errorClass = "myClass"
        )
      }
      
      #### ---------------- Two Pop Variance Validation
      
      if(!twopopvarsum_iv$is_valid()) {
        validate(
          need(input$SDSampleSize1, "Sample size 1 is required.") %then%
            need(input$SDSampleSize1 %% 1 == 0 && input$SDSampleSize1 > 1, "Sample size 1 must be an integer greater than 1."),
          
          need(input$SDSampleSize2, "Sample size 2 is required.") %then%
            need(input$SDSampleSize2 %% 1 == 0 && input$SDSampleSize2 > 1, "Sample size 2 must be an integer greater than 1."),
          
          need(input$stdDev1, "Sample standard deviation 1 is required.") %then%
            need(input$stdDev1 > 0, "Sample standard deviation 1 must be greater than 0."),
          
          need(input$stdDev2, "Sample standard deviation 2 is required.") %then%
            need(input$stdDev2 > 0, "Sample standard deviation 2 must be greater than 0."),
          
          errorClass = "myClass")
      }
      
      if (!twopopvar_iv$is_valid()) {
        validate(
          need(input$n1, "n1 is required.") %then%
            need(input$n1 %% 1 == 0 && input$n1 > 1,
                 "n1 must be an integer greater than 1."),
          
          need(input$s1sq, "s1^2 is required.") %then%
            need(input$s1sq > 0,
                 "s1^2 must be greater than 0."),
          
          need(input$n2, "n2 is required.") %then%
            need(input$n2 %% 1 == 0 && input$n2 > 1,
                 "n2 must be an integer greater than 1."),
          
          need(input$s2sq, "s2^2 is required.") %then%
            need(input$s2sq > 0,
                 "s2^2 must be greater than 0."),
          
          errorClass = "myClass"
        )
      }
      
      if (!twopopvarraw_iv$is_valid()) {
        validate(
          need(input$rawSamp1SD, "Group 1 data requires a minimum of 3 numeric values.") %then%
            need(length(createNumLst(input$rawSamp1SD)) >= 3, "Group 1 data requires a minimum of 3 numeric values.") %then%
            need(sd(createNumLst(input$rawSamp1SD)) > 0, "Group 1 must have variance."),
          
          need(input$rawSamp2SD, "Group 2 data requires a minimum of 3 numeric values.") %then%
            need(length(createNumLst(input$rawSamp2SD)) >= 3, "Group 2 data requires a minimum of 3 numeric values.") %then%
            need(sd(createNumLst(input$rawSamp2SD)) > 0, "Group 2 must have variance."),
          
          errorClass = "myClass"
        )
      }
      
      #### ---------------- ANOVA Validation
      if(!anovaupload_iv$is_valid()) {
        if(is.null(input$anovaUserData)) {
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$anovaStatus) && fileInputs$anovaStatus == 'uploaded', "Please upload a file."),
          errorClass = "myClass")
        
        validate(
          need(nrow(anovaUploadData()) > 0, "File is empty."),
          need(ncol(anovaUploadData()) >= 2, "File must contain at least 2 distinct columns of data to choose from for analysis."),
          errorClass = "myClass")
      }
      
      if(!anovamulti_iv$is_valid()) {
        validate(
          need(length(input$anovaMultiColumns) >= 2, "Please select two or more columns to conduct analysis."),
          errorClass = "myClass")
        
        validate(
          need(!checkNumeric(anovaUploadData(), input$anovaMultiColumns), 
               "Selected columns must be numeric."),
          errorClass = "myClass"
        )
      }
      
      if(!anovastacked_iv$is_valid()) {
        validate(
          need(!is.null(input$anovaResponse) && input$anovaResponse != '', "Please select a Response Variable."),
          need(!is.null(input$anovaFactors) && input$anovaFactors != '', "Please select a Factors column."),
          errorClass = "myClass")
        
        validate(
          need(anovaStackedIsValid() == TRUE, "Please select distinct columns for the Response Variable and Factors."),
          errorClass = "myClass")
        
        validate(
          need(!checkNumeric(anovaUploadData(), input$anovaResponse), 
               "Response variable must be numeric."),
          errorClass = "myClass"
        )
      }
      
      #### ---------------- Kruskal-Wallis Validation    
      if(!kwupload_iv$is_valid()) {
        if(is.null(input$kwUserData)) {
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$kwStatus)&& fileInputs$kwStatus == 'uploaded', "Please upload a file."),
          errorClass = "myClass"
        )
        
        validate(
          need(nrow(kwUploadData()) > 0, "File is empty."),
          need(ncol(kwUploadData()) >= 2, "File must contain at least 2 distinct columns of data to choose from for analysis."),
          errorClass = "myClass"
        )
      }
      
      if(!kwmulti_iv$is_valid()) {
        validate(
          need(length(input$kwMultiColumns) >= 2, "Please select two or more columns to conduct analysis."),
          errorClass = "myClass"
        )
        
        validate(
          need(!checkNumeric(kwUploadData(), input$kwMultiColumns), 
               "Selected columns must be numeric."),
          errorClass = "myClass"
        )
      }
      
      if(!kwstacked_iv$is_valid()) {
        validate(
          need(!is.null(input$kwResponse) && input$kwResponse != '', "Please select a Response Variable."),
          need(!is.null(input$kwFactors) && input$kwFactors != '', "Please select a Factors column."),
          errorClass = "myClass"
        )
        
        validate(
          need(kwStackedIsValid() == TRUE, "Please select distinct columns for Response Variable and Factors."),
          errorClass = "myClass"
        )
        validate(
          need(!checkNumeric(kwUploadData(), input$kwResponse), 
               "Response variable must be numeric."),
          errorClass = "myClass"
        )
      }
      
      #### ---------------- Chi-Square Validation
      if(!chiSq2x2_iv$is_valid()) {
        validate(
          need(input$chiSqInput2x2, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
            need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(chiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
          errorClass = "myClass")
      }
      
      if(!chiSq2x3_iv$is_valid()) {
        validate(
          need(input$chiSqInput2x3, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
            need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(chiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
          errorClass = "myClass")
      }
      
      if(!chiSq3x2_iv$is_valid()){
        validate(
          need(input$chiSqInput3x2, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
            need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(chiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
          errorClass = "myClass")
      }
      
      if(!chiSq3x3_iv$is_valid()){
        validate(
          need(input$chiSqInput3x3, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
            need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(cchiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        validate(
          need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
          errorClass = "myClass")
      }
    })
    
    ### ------------ One Mean Outputs --------------------------------------------
    
    #### ------------- Uploaded Data Table --------------------------
    output$onePopMeanUploadTable <- renderDT({
      req(onemeanupload_iv$is_valid())
      datatable(OneMeanUploadData(),
                options = list(pageLength = -1,
                               lengthMenu = list(c(25, 50, 100, -1),
                                                 c("25", "50", "100", "all")),
                               columnDefs = list(list(className = 'dt-center',
                                                      targets = 0:ncol(OneMeanUploadData())))),
      )
    })
    
    #### ---------------- CI ----
    output$oneMeanCI <- renderUI({
      printOneMeanCI()
    })
    
    #### ------------------ HT ----
    output$oneMeanHT <- renderUI({
      printOneMeanHT()
    })
    
    #### ---------------- HT Plot ----
    output$oneMeanHTPlot <- renderPlot({
      
      if(input$dataAvailability == 'Summarized Data') {
        
        if(input$sigmaKnown == 'Known') {
          oneMeanData <- OneMeanZTestSumm()
          sigmaKnown <- 'Known'
          
        } else if(input$sigmaKnown == 'Unknown') {
          oneMeanData <- OneMeanTTestSumm()
          sigmaKnown <- 'Unknown'
        }
      } else if(input$dataAvailability == 'Enter Raw Data') {
        
        if(input$sigmaKnownRaw == 'rawKnown'){
          oneMeanData <- OneMeanZTestRaw()
          sigmaKnown <- 'Known'
          
        } else if(input$sigmaKnownRaw == 'rawUnknown') {
          oneMeanData <- OneMeanTTestRaw()
          sigmaKnown <- 'Unknown'
        }
      } else if(input$dataAvailability == 'Upload Data'){
        
        if(input$sigmaKnownUpload == 'Known'){
          oneMeanData <- OneMeanZTestRaw()
          sigmaKnown <- 'Known'
          
        } else if(input$sigmaKnownUpload == 'Unknown'){
          oneMeanData <- OneMeanTTestRaw()
          sigmaKnown <- 'Unknown'
        }
      }
      
      intrpInfo <- OneMeanHypInfo()
      htPlotCritVal <- oneMeanData[4]
      
      if(sigmaKnown== 'Known') {
        oneMeanPlot <- hypZTestPlot(oneMeanData[6], htPlotCritVal, intrpInfo$alternative)
        
      } else {
        oneMeanPlot <- hypTTestPlot(oneMeanData[6], oneMeanData[8], htPlotCritVal, intrpInfo$alternative)
      }
      
      oneMeanPlot
    })
    
    #### ---------------- Boxplot ----
    output$oneMeanBoxplot <- renderPlot({
      req(si_iv$is_valid())
      
      if(input$dataAvailability == 'Enter Raw Data') {
        dat <- createNumLst(input$sample1)
      } else if(input$dataAvailability == 'Upload Data') {
        dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
      } else {
        return(NA)
      }
      
      df_outliers <- getOutliers(dat, "Sample")
      outlier_vals <- df_outliers$data
      
      df_boxplot <- data.frame(x = dat)
      
      RenderBoxplot(dat,
                    df_boxplot,
                    outlier_vals,
                    input[["oneMeanBoxplot-Colour"]],
                    input[["oneMeanBoxplot-Title"]],
                    input[["oneMeanBoxplot-Xlab"]],
                    input[["oneMeanBoxplot-Ylab"]],
                    input[["oneMeanBoxplot-BoxWidth"]]/10,
                    input[["oneMeanBoxplot-Gridlines"]],
                    input[["oneMeanBoxplot-Flip"]],
                    input[["oneMeanBoxplot-OutlierLabels"]])
      
    }, height = function() {GetPlotHeight(input[["oneMeanBoxplot-Height"]], input[["oneMeanBoxplot-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["oneMeanBoxplot-Width"]], input[["oneMeanBoxplot-WidthPx"]], ui = FALSE)}
    )
    
    ### ------------ One Sample Standard Deviation Outputs -----------------------
    #### ---- One population standard deviation confidence interval CI ----
    output$oneSDCI <- renderUI({
      ## Input validation
      req(oneSD_iv$is_valid())
      
      ## Required data
      ## n (sample size), s (sample standard deviation), Confidence Level (1 - α)
      ## ns("SSDSampleSize")
      ## ns("SSDStdDev")
      ## ns("confidenceLevel")
      ## df = n - 1
      oneSDCIalpha <- 1 - ConfLvl() # e.g.: 0.05
      oneSDCIdf <- input[["SSDSampleSize"]] - 1
      
      ## UI
      withMathJax(
        ## Preface
        sprintf("Given:"), br(),
        sprintf("\\( n = %d \\)",
                input$SSDSampleSize),
        br(),
        sprintf("\\( s = %0.2f \\)",
                input$SSDStdDev),
        br(),
        
        br(),
        br(),
        
        sprintf("For a %s Confidence Interval:", input$confidenceLevel),
        br(),
        sprintf("\\( \\alpha = 1 - %0.2f = %0.2f \\)", 1 - oneSDCIalpha, oneSDCIalpha),
        br(),
        sprintf("\\(  df = n - 1 = %d - 1 = %d \\)",
                input$SSDSampleSize,
                input$SSDSampleSize - 1),
        br(),
        sprintf("\\( \\chi^2_{  \\alpha/2, df} = \\chi^2_{    %0.2f / 2 , %d} = \\chi^2_{ %0.3f, %d } = %0.3f \\).",
                oneSDCIalpha,
                oneSDCIdf,
                ## See https://www.easysevens.com/understanding-chi-square-critical-value-a-beginners-tutorial/.
                (critOneSSDLeft <- oneSDCIalpha/2),
                oneSDCIdf,
                (oneSSDLeft <- qchisq(p = 1 - critOneSSDLeft, df = oneSDCIdf))),
        br(),
        sprintf("\\( \\chi^2_{1-\\alpha/2, df} = \\chi^2_{ 1 - %0.2f / 2, %d} = \\chi^2_{ %0.3f, %d} = %0.3f \\)",
                oneSDCIalpha,
                oneSDCIdf,
                (critOneSSDRight <- 1 - oneSDCIalpha/2),
                oneSDCIdf,
                (oneSSDRight <- qchisq(p = 1 - critOneSSDRight, df = oneSDCIdf))),
        br(),
        
        br(),
        br(),
        
        sprintf(r"---{\(
          CI = \displaystyle
          \left(
          \sqrt{\frac{df}{\chi^2_{\alpha/2, df}}} \cdot s, \;\:
          \sqrt{\frac{df}{\chi^2_{1 - \alpha/2, df}}} \cdot s
          \right) \)}---"),
        br(),
        br(),
        br(),
        
        sprintf(r"---(
          \(
          \begin{align}
          CI &= \left( \sqrt{\frac{%d}{%0.3f}} \cdot %0.3f, \;\: \sqrt{\frac{%d}{%0.3f}} \cdot %0.3f \right) \\ \\
             &= \left(%0.2f, %0.2f\right)
          \end{align}
          \)
          )---",
          ## Left/lower
          oneSDCIdf, # df
          oneSSDLeft,
          input$SSDStdDev, # s
          
          ## Right/upper
          oneSDCIdf, # df
          oneSSDRight,
          input$SSDStdDev, #s
          (oneSSDLowerPopStdDev <- sqrt(oneSDCIdf / oneSSDLeft) * input$SSDStdDev),
          (oneSSDUpperPopStdDev <- sqrt(oneSDCIdf / oneSSDRight) * input$SSDStdDev)),
        br(),
        br(),
        br(),
        
        ## Step three
        tags$b("Interpretation:"), br(),
        sprintf("We are %s confident that the population standard deviation (\\( \\sigma \\)) is between \\( %0.2f \\) and \\( %0.2f \\).",
                input$confidenceLevel, oneSSDLowerPopStdDev, oneSSDUpperPopStdDev)
      )
      
    })
    
    #### ---- One population standard deviation hypothesis testing HT ----
    ## See #33.
    chiSqTestData <- function(envir) {
      evalq(expr = {
        ## This paragraph is related to the final interpretation, as used in the
        ## P-value method.
        degreesOfFreedom <- input$SSDSampleSize - 1;
        chiSqTestStatistic <- (degreesOfFreedom * input$SSDStdDev^2) /  input$hypStdDeviation^2;
        ## lower.tail will be false when the alternative hypothesis is >.
        isLeftTailed = input$altHypothesis %in% c(1, 2);
        
        ## Establish the strings to use in MathJax-supported LaTeX for the hypotheses and relations.
        if (input$altHypothesis == 1) {
          nullHypString <- "\\geq";
          altHypString <- "\\lt";
          pValueMethodRelationalOperatorString <- "\\lt";
          chiSqCValue <- qchisq(SigLvl(), degreesOfFreedom);
          chiSqPValue <- pchisq(chiSqTestStatistic, degreesOfFreedom, lower.tail = isLeftTailed);
        } else if (input$altHypothesis == 2) {
          nullHypString <- "=";
          altHypString <- "\\ne";
          pValueMethodRelationalOperatorString <- "\\lt"; 
          chiSqCValueLower <- qchisq(SigLvl()/2, degreesOfFreedom);
          chiSqCValueUpper <- qchisq(1 - SigLvl()/2, degreesOfFreedom);
          chiSqCValue <- c(chiSqCValueLower, chiSqCValueUpper)
          ## NOTE: The correct p-value is P = 2 × min(P(chisquare ≤ TS), P(chisquare ≥ TS))
          ## lower.tail: logical; if TRUE (default), probabilities are P[X <= x],
          ## otherwise, P[X > x].
          chiSqPValue <- 2 * min(pchisq(chiSqTestStatistic, degreesOfFreedom, lower.tail = TRUE),
                                 pchisq(chiSqTestStatistic, degreesOfFreedom, lower.tail = FALSE))
        } else {
          nullHypString <- "\\leq";
          altHypString <- "\\gt";
          pValueMethodRelationalOperatorString <- "\\gt";
          chiSqCValue <- qchisq(1 - SigLvl(), degreesOfFreedom);
          chiSqPValue <- pchisq(chiSqTestStatistic, degreesOfFreedom, lower.tail = isLeftTailed);
        }
      },
      envir = envir)
    }
    
    output$onePopulationSDHTChiSqPlot <- renderPlot({
      chiSqTestData(envir = environment())
      
      ## Clamp the minimum to zero.
      minimumChiSqValue <- min(chiSqTestStatistic, chiSqCValue) - 1
      if (minimumChiSqValue < 0) minimumChiSqValue <- 0
      
      maximumChiSqValue <- max(chiSqTestStatistic, chiSqCValue) + 1
      if (maximumChiSqValue < 20) maximumChiSqValue <- 20
      
      par(font.lab = 2, font.axis = 2)
      
      ## Plot the main curve.
      curve(
        dchisq(x, df = degreesOfFreedom),
        ## from = minimumChiSqValue,
        from = 0,
        to = maximumChiSqValue,
        ## main = sprintf("Chi-Square Distribution (df = %d)", degreesOfFreedom),
        main = NULL,
        lwd = 2, # line width
        font.lab = 2,
        xlab = expression(x = chi^2),
        ylab = expression(y = f(chi^2))
      )
      
      ## Account for two-tailed hypothesis tests.
      if (length(chiSqCValue) == 1) {
        ## applies to lower and upper tailed tests
        lowerRejectionRegion <- sort(seq(0, chiSqCValue, by = 0.00001))
        upperRejectionRegion <- sort(seq(chiSqCValue, maximumChiSqValue, by = 0.00001))
        lowerPVector <- dchisq(lowerRejectionRegion, df = degreesOfFreedom)
        upperPVector <- dchisq(upperRejectionRegion, df = degreesOfFreedom)
        if (input$altHypothesis == 1)
          polygon(c(lowerRejectionRegion, rev(lowerRejectionRegion)),
                  c(lowerPVector, rep(0, length(lowerPVector))),
                  col = adjustcolor("red", alpha = 0.3),
                  border = NA)
        else
          polygon(c(upperRejectionRegion, rev(upperRejectionRegion)),
                  c(upperPVector, rep(0, length(upperPVector))),
                  col = adjustcolor("red", alpha = 0.3),
                  border = NA)
        segments(x0 = chiSqTestStatistic,
                 y0 = 0,
                 y1 = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
                 col = adjustcolor("blue", alpha = 0.8),
                 lwd = 5)
        segments(x0 = chiSqCValue,
                 y0 = 0,
                 y1 = dchisq(chiSqCValue, df = degreesOfFreedom),
                 col = adjustcolor("red", alpha = 0.8),
                 lwd = 5)
        text(
          x = chiSqCValue, y = dchisq(chiSqCValue, df = degreesOfFreedom),
          labels = as.character(round(chiSqCValue, 4)),
          pos = 3, # to the left of the specified (x, y) coordinate.
          offset = 1
        )
        text(
          x = chiSqTestStatistic, y = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
          labels = as.character(round(chiSqTestStatistic, 4)),
          pos = 2, # to the left of the specified (x, y) coordinate.
          offset = 1
        )
      } else {
        ## two-tailed hypothesis tests
        lowerRejectionRegion <- seq(0, chiSqCValue[[1]], by = 0.00001)
        lowerPVector <- dchisq(lowerRejectionRegion, df = degreesOfFreedom)
        polygon(c(lowerRejectionRegion, rev(lowerRejectionRegion)),
                c(lowerPVector, rep(0, length(lowerPVector))),
                col = adjustcolor("red", alpha = 0.3),
                border = NA)
        
        upperRejectionRegion <- seq(chiSqCValue[[2]], max(chiSqTestStatistic, chiSqCValue) + 1)
        upperPVector <- dchisq(upperRejectionRegion, df = degreesOfFreedom)
        polygon(c(upperRejectionRegion, rev(upperRejectionRegion)),
                c(upperPVector, rep(0, length(upperPVector))),
                col = adjustcolor("red", alpha = 0.3),
                border = NA)
        
        segments(x0 = chiSqTestStatistic,
                 y0 = 0,
                 y1 = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
                 col = adjustcolor("blue", alpha = 0.8),
                 lwd = 5)
        segments(x0 = chiSqCValue[[1]],
                 y0 = 0,
                 y1 = dchisq(chiSqCValue[[1]], df = degreesOfFreedom),
                 col = adjustcolor("red", alpha = 0.8),
                 lwd = 5)
        segments(x0 = chiSqCValue[[2]],
                 y0 = 0,
                 y1 = dchisq(chiSqCValue[[2]], df = degreesOfFreedom),
                 col = adjustcolor("red", alpha = 0.8),
                 lwd = 5)
        
        text(
          x = chiSqCValue[[1]], y = dchisq(chiSqCValue[[1]], df = degreesOfFreedom),
          labels = as.character(round(chiSqCValue[[1]], 4)),
          pos = 3, # to the left of the specified (x, y) coordinate.
          offset = 1
        )
        text(
          x = chiSqCValue[[2]], y = dchisq(chiSqCValue[[2]], df = degreesOfFreedom),
          labels = as.character(round(chiSqCValue[[2]], 4)),
          pos = 3, # to the left of the specified (x, y) coordinate.
          offset = 1
        )
        text(
          x = chiSqTestStatistic, y = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
          labels = as.character(round(chiSqTestStatistic, 4)),
          pos = 2, # to the left of the specified (x, y) coordinate.
          offset = 1
        )
      }
      
      segments(x0 = 0, y0 = 0,
               x1 = maximumChiSqValue, y1 = 0,
               col = adjustcolor("black", alpha = 1.0))
    })
    
    relation <- reactiveVal()
    
    output$onePopulationSDHT <- renderUI({
      ## Required data: n, s, alpha, sigma_naught, hypothesis_alternative; ns(x)
      ## doesn't seem to be required here. Review why that might be.
      ##
      ## Inputs: SSDSampleSize (n), SSDStdDev (s), significanceLevel, hypStdDeviation (sigma_naught),
      ## altHypothesis (e.g. "<").
      ##
      ## Useful data: SigLvl() [numeric];
      
      chiSqTestData(envir = environment())
      
      ## "if P <= alpha, reject H0"
      if (chiSqPValue <= SigLvl()) {
        rejectionOrAcceptanceStatement <-
          sprintf("Since \\( P \\leq %0.2f \\), reject \\( H_{0}\\).", SigLvl())
      } else {
        rejectionOrAcceptanceStatement <-
          sprintf("Since \\( P \\gt %0.2f \\), do not reject \\( H_{0}\\).", SigLvl())
      }
      
      hypothesisFormattedString <- function(hypothesis, nullOrAltHypothesisString) {
        sprintf(r"--[\( H_%s: \sigma %s %0.3f \)]--", # σ
                hypothesis, nullOrAltHypothesisString, input$hypStdDeviation);
      }
      
      ## UI
      withMathJax(
        hypothesisFormattedString("0", nullHypString), br(),
        hypothesisFormattedString("a", altHypString), br(),
        br(),
        sprintf("\\( \\alpha = %0.2f \\)", SigLvl()), br(),
        
        br(),
        p(tags$b("Test Statistic:")),
        sprintf("Given:"), br(),
        sprintf(r"--[\( n = %d \)]--", input$SSDSampleSize), br(),
        sprintf(r"--[\( s = %0.4f \)]--", input$SSDStdDev), br(),
        sprintf(r"--[\( \sigma_0 = %.4f \)]--", input$hypStdDeviation), br(),
        
        br(),
        br(),
        p(r"--[
            \(
            \displaystyle \chi^2 = \frac{(n-1)s^2}{\sigma^2_0}
            \)
           ]--"),
        br(),
        sprintf(
          r"--(
           \(
           \displaystyle
           \chi^2 = \frac{(%d - 1)  %0.4f ^2}{%0.4f^2} = %0.4f\\
           \)
           )--",
          input$SSDSampleSize,  input$SSDStdDev,  input$hypStdDeviation, chiSqTestStatistic
        ),
        
        br(),
        br(),
        p(tags$b("Using P-Value Method:")),
        if (input$altHypothesis == 2) {
          sprintf("\\( P = 2 \\times min(P\\left( \\chi^2 \\le %0.4f \\right), P\\left( \\chi^2 \\ge %0.4f \\right)) = %0.4f \\)",
                  chiSqTestStatistic,
                  chiSqTestStatistic,
                  chiSqPValue)
        } else {
          sprintf("\\( P = P\\left( \\chi^2 %s %s \\right) = %0.4f \\)",
                  pValueMethodRelationalOperatorString,
                  sprintf("%0.4f", chiSqTestStatistic),
                  chiSqPValue)
        },
        br(),
        br(),
        rejectionOrAcceptanceStatement, br(),
        
        br(),
        br(),
        p(tags$b("Using Critical Value Method:")),
        sprintf("\\(df = n - 1 = %d - 1 = %d\\)", input$SSDSampleSize, degreesOfFreedom), 
        br(),
        br(),
        if (input$altHypothesis != 2) {
          HTML(sprintf("Critical value(s): \\( \\chi^2_{%0.2f,%d} = %0.4f \\) <br/>",
                       SigLvl(),
                       degreesOfFreedom,
                       chiSqCValue))
        } else {
          HTML(sprintf("Critical value(s): <br/>
                  \\( \\chi^2_{\\alpha/2,df} = \\chi^2_{%0.4f,%d} = %0.4f \\) <br/>
                  \\( \\chi^2_{1 - \\alpha/2,df} = \\chi^2_{%0.4f,%d} = %0.4f \\) <br/>",
                  SigLvl() / 2,
                  degreesOfFreedom,
                  chiSqCValue[[1]],
                  1 - SigLvl() / 2,
                  degreesOfFreedom,
                  chiSqCValue[[2]]))
        },
        
        br(),
        ## Chi square critical value conclusion.
        ## Example from mu: "Since the test statistic (z) falls within the rejection region, reject H0."
        if (input$altHypothesis != 2) {
          HTML(sprintf(
            r"--(\(\begin{align} \displaystyle \chi^2 &%s \chi^2_{%0.2f,%d} \\ %0.4f &%s %0.4f  \\ \end{align} \)<br/>)--",
            ## Both of these are alternative hypothesis-dependent
            {
              if (chiSqTestStatistic < chiSqCValue) { relation("\\leq"); "\\leq" }
              else if (chiSqTestStatistic >= chiSqCValue) { relation("\\geq"); "\\geq" }
            }, SigLvl(), degreesOfFreedom,
            chiSqTestStatistic, {
              if (chiSqTestStatistic < chiSqCValue) { relation("\\leq"); "\\leq" }
              else if (chiSqTestStatistic >= chiSqCValue) { relation("\\geq"); "\\geq" }
            },
            chiSqCValue
          ))
        } else {
          lessThan <- chiSqTestStatistic <= chiSqCValue[[1]]
          greaterThan <- chiSqTestStatistic >= chiSqCValue[[2]]
          between <- !lessThan && !greaterThan
          if (lessThan) { relation("\\leq"); "\\leq" }
          else if (greaterThan) { relation("\\geq"); "\\geq" }
          
          if (!between) {
            HTML(sprintf(
              r"--(\(\begin{align} \displaystyle \chi^2 &%s \chi^2_{%0.4f,%d} \\ %0.4f &%s %0.4f \\ \end{align} \)<br/>)--",
              relation(), {if (lessThan) SigLvl()/2 else 1-SigLvl()/2}, degreesOfFreedom,
              chiSqTestStatistic, relation(), if (lessThan) chiSqCValue[[1]]))
          } else {
            HTML(sprintf(r"--(\(\begin{align} \displaystyle \chi^2_{%0.4f,%d} &< \chi^2 &< \chi^2_{%0.4f,%d} \\ %0.4f &< %0.4f &< %0.4f \\ \end{align} \)<br/>)--",
                         SigLvl()/2, degreesOfFreedom, 1-SigLvl()/2, degreesOfFreedom,
                         chiSqCValue[[1]], chiSqTestStatistic, chiSqCValue[[2]]))
          }
        },
        
        br(),
        {
          conclusionString <-
            function(significanceLevel = SigLvl(),
                     testStatisticValue = chiSqTestStatistic,
                     criticalValue = chiSqCValue,
                     accept = TRUE,
                     lessThan = TRUE) {
              sprintf(paste0("Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
                             " falls in the %s region,",
                             " \\(\\chi^2 = %0.4f\\) which is %s than \\(%0.4f\\), we %sreject \\(H_0\\)",
                             " as there is %ssufficient evidence to accept the",
                             " alternative hypothesis."),
                      if (accept) "acceptance" else "rejection",
                      testStatisticValue,
                      if (lessThan) "less" else "greater",
                      criticalValue,
                      if (accept) "do not " else "",
                      if (accept) "in" else ""
              )
            }
          
          if (input$altHypothesis != 2) {
            ## One-tailed test
            if (input$altHypothesis == 1) {
              if (chiSqTestStatistic < chiSqCValue) {
                conclusionString(accept = (accept <- FALSE), lessThan = TRUE)
              } else {
                conclusionString(accept = (accept <- TRUE), lessThan = FALSE)
              }
            } else {
              if (chiSqTestStatistic < chiSqCValue) {
                conclusionString(accept = (accept <- TRUE), lessThan = TRUE)
              } else {
                conclusionString(accept = (accept <- FALSE), lessThan = FALSE)
              }
            }
          } else {
            ## Two-tailed test
            if (chiSqTestStatistic <= chiSqCValue[[1]]) {
              accept <- FALSE
              sprintf(paste0("Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
                             " falls in the rejection region,",
                             " \\(\\chi^2 = %0.4f\\) which is less than (or equal to) \\(%0.3f\\), we reject \\(H_0\\)",
                             " as there is sufficient evidence to accept the",
                             " alternative hypothesis."),
                      chiSqTestStatistic,
                      chiSqCValue[[1]])
            } else if ((chiSqTestStatistic >= chiSqCValue[[2]])) {
              accept <- FALSE
              sprintf(paste0("Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
                             " falls in the rejection region,",
                             " \\(\\chi^2 = %0.34\\) which is greater than (or equal to) \\(%0.4f\\), we reject \\(H_0\\)",
                             " as there is sufficient evidence to accept the",
                             " alternative hypothesis."),
                      chiSqTestStatistic,
                      chiSqCValue[[2]])
            } else {
              accept <- TRUE
              sprintf(paste0("Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
                             " falls in the acceptance region,",
                             " \\(\\chi^2 = %0.3f\\) which is between \\(%0.4f\\) and \\(%0.4f\\), we do not reject \\(H_0\\)",
                             " as there is insufficient evidence to accept the",
                             " alternative hypothesis."),
                      chiSqTestStatistic,
                      chiSqCValue[[1]],
                      chiSqCValue[[2]])
            }
          }
        },
        
        
        br(),
        
        ### FUTURE WORK: Revisit chi squ plots as they are currently bugged
        #plotOutput(session$ns("onePopulationSDHTChiSqPlot"), width = "50%", height = "400px"),
        
        ## Overall conclusion
        br(),
        p(tags$b("Conclusion:")),
        {
          if (accept) {
            conclusion <- sprintf("At \\(\\alpha = %0.2f\\), since the test statistic falls in the acceptance region we fail to reject \\(H_0\\) and conclude that there is not enough statistical evidence to support that \\(\\sigma %s %s\\).",
                                  SigLvl(),
                                  altHypString,
                                  input$hypStdDeviation)
          } else {
            conclusion <- sprintf("At \\(\\alpha = %0.2f\\), since the test statistic falls in the rejection region we reject \\(H_0\\) and conclude that there is enough statistical evidence to support that \\(\\sigma %s %s\\).",
                                  SigLvl(),
                                  altHypString,
                                  input$hypStdDeviation)
          }
          
          conclusion
        },
        br()) # withMathJax
    }) # renderUI
    
    ### ------------ One Prop Outputs --------------------------------------------
    
    #### ----------------- #3 Graphs! ---------
    output$onePropBarGraph <- renderPlot({
      req(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
      
      df <- tibble(
        Outcome = c("Successes", "Failures"),
        Count = c(input$numSuccesses, input$numTrials - input$numSuccesses)
      )
      
      ggplot(df, aes(x = Outcome, y = Count, fill = Outcome)) +
        geom_col(width = 0.5) +
        labs(
          title = "Bar Chart: Count of Successes vs Failures",
          y = "Count", x = ""
        ) +
        scale_fill_manual(values = c("Successes" = "#4CAF50", "Failures" = "#F44336")) +
        theme_classic() +
        theme(
          axis.text.x = element_text(size = 14, face = "bold", color = "black"),
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 18, face = "bold"),
          legend.position = "none"
        )
    },
    width = 500,
    height = 400)
    
    output$onePropPieChart <- renderPlot({
      req(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
      
      x <- tibble(
        Outcome = c("Successes", "Failures"),
        Count = c(input$numSuccesses, input$numTrials - input$numSuccesses)
      )
      
      ggplot(x, aes(x = "", y = Count, fill = Outcome)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        scale_fill_manual(values = c("Successes" = "#4CAF50", "Failures" = "#F44336")) +
        labs(title = "Success vs Failure Distribution") +
        theme_void() +
        theme(
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # center title
          legend.text = element_text(size = 12),
          plot.margin = margin(0, 0, 0, 0),
          plot.background = element_rect(fill = "white", color = NA)
        )
    })
    
    #### ---------------- CI ----
    output$onePropCI <- renderUI({
      req(si_iv$is_valid() && input$numTrials >= input$numSuccesses);
      
      onePropData <- OnePropZInterval(input$numSuccesses, input$numTrials, ConfLvl());
      critVal <- round(onePropData["Z Critical"], cvDigits);
      
      p(
        withMathJax(
          sprintf("Given:"),
          br(),
          sprintf("\\( n = %s \\)",
                  onePropData["n"]),
          br(),
          sprintf("\\( x = %s \\)",
                  onePropData["x"]),
          br(),
          br(),
          br(),
          sprintf("For a \\( %s \\)%% Confidence Interval: ",
                  ConfLvl()*100),
          br(),
          sprintf("\\( \\alpha = 1 - %s = %s \\)",
                  ConfLvl(),
                  1 - ConfLvl()),
          br(),
          sprintf("\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
                  1 - ConfLvl(),
                  (1 - ConfLvl()) / 2,
                  critVal),
          br(),
          br(),
          br(),
          sprintf("\\( \\displaystyle CI = \\hat{p} \\pm \\left( z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} \\right) \\)"),
          br(),
          p("where"),
          sprintf("\\( \\qquad \\hat{p} = \\dfrac{x}{n} = \\dfrac{%s}{%s} = %0.4f \\)",
                  onePropData["x"],
                  onePropData["n"],
                  onePropData["phat"]),
          br(),
          br(),
          br(),
          sprintf("\\( \\displaystyle CI = %0.4f \\pm \\left( %s \\sqrt{\\dfrac{%0.4f(1 - %0.4f)}{%s}} \\right) \\)",
                  onePropData["phat"],
                  critVal,
                  onePropData["phat"],
                  onePropData["phat"],
                  onePropData["n"]),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\phantom{CI} = %0.4f \\pm \\left( %g \\cdot %0.4f \\right) \\)",
                  onePropData["phat"],
                  critVal,
                  onePropData['Std Error']),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\phantom{CI} = %0.4f \\pm %0.4f \\)",
                  onePropData["phat"],
                  onePropData['ME']),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\phantom{CI} = (%0.4f, %0.4f)\\)",
                  onePropData["LCL"],
                  onePropData["UCL"]),
          br(),
          br(),
          br(),
          p(tags$b("Interpretation:")),
          sprintf("We are %1.0f%% confident that the population proportion \\( (p) \\) is between \\( %0.4f \\) and \\( %0.4f \\).",
                  ConfLvl()*100,
                  onePropData["LCL"],
                  onePropData["UCL"])
        )
      )
    })
    
    
    #### ---------------- HT ----
    output$onePropHT <- renderUI({
      req(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
      
      onePropData <- OnePropZTest(input$numSuccesses, input$numTrials, input$hypProportion, OneMeanHypInfo()$alternative, SigLvl())
      
      if(input$altHypothesis == "2") { #two sided test
        critZVal <- paste("\\pm", round(onePropData["Z Critical"], cvDigits))
        nullHyp <- "p ="
        altHyp <- "p \\neq"
      } else {
        critZVal <- paste(round(onePropData["Z Critical"], cvDigits))
        
        if(input$altHypothesis == "1"){
          nullHyp <- "p \\geq"
          altHyp <- "p \\lt"
        } else {
          nullHyp <- "p \\leq"
          altHyp <- "p \\gt"
        }
      }
      
      if(onePropData["P-Value"] > SigLvl()) {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      } else {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      }
      
      onePropHTHead <- tagList(
        withMathJax(
          sprintf("\\( H_{0}: %s %g\\)",
                  nullHyp,
                  input$hypProportion),
          br(),
          sprintf("\\( H_{a}: %s %g\\)",
                  altHyp,
                  input$hypProportion),
          br(),
          br(),
          sprintf("\\( \\alpha = %g \\)",
                  SigLvl()),
          #br(),
          br(),
          p(tags$b("Test Statistic:")),
          sprintf("Given:"),
          br(),
          sprintf("\\( n = %s \\)",
                  onePropData["n"]),
          br(),
          sprintf("\\( x = %s \\)",
                  onePropData["x"]),
          br(),
          br(),
          br(),
          sprintf("\\(z = \\dfrac{\\hat{p} - p_{0}}{ \\sqrt{ \\dfrac{p_{0}(1 - p_{0})}{n} } }\\)"),
          br(),
          p("where"),
          sprintf("\\( \\qquad \\hat{p} = \\dfrac{x}{n} = \\dfrac{%s}{%s} = %0.4f \\)",
                  onePropData["x"],
                  onePropData["n"],
                  onePropData["phat"]),
          br(),
          br(),
          br(),
          sprintf("\\(z = \\dfrac{%0.4f - %0.4f}{ \\sqrt{ \\dfrac{%0.4f(1 - %0.4f)}{%1.0f} } }\\)",
                  onePropData["phat"],
                  input$hypProportion,
                  input$hypProportion,
                  input$hypProportion,
                  input$numTrials),
          sprintf("\\( = \\dfrac{%0.4f}{%0.4f} \\)",
                  onePropData["phat"] - input$hypProportion,
                  onePropData["Std Error"]),
          br(),
          br(),
          sprintf("\\(\\phantom{z} = %0.4f\\)",
                  onePropData["Test Statistic"]),
          br(),
          br(),
          br()
        )
      )
      
      onePropPVal <- printHTPVal(onePropData["P-Value"],
                                 "z",
                                 OneMeanHypInfo()$alternative,
                                 onePropData["Test Statistic"],
                                 pvalSymbol,
                                 reject)
      # p(tags$b("Using P-Value Method:")),
      # sprintf("\\( %s \\)",
      #         pValue),
      # br(),
      # sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
      #         pvalSymbol,
      #         SigLvl(),
      #         reject),
      # br(),
      # br(),
      # br(),
      
      onePropHTTail <- tagList(
        withMathJax(
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                  OneMeanHypInfo()$critSign,
                  OneMeanHypInfo()$critAlph,
                  OneMeanHypInfo()$critSign,
                  OneMeanHypInfo()$alphaVal,
                  critZVal),
          br(),
          br(),
          sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                  region,
                  reject),
          br(),
          br(),
          plotOutput(session$ns('onePropHTPlot'), width = "75%", height = "300px"),
          br()
        )
      )
      
      onePropHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, input$hypProportion)
      
      tagAppendChildren(onePropHTHead, onePropPVal, onePropHTTail, onePropHTConclusion)
    })
    
    
    #### ---------------- HT Plot ----
    output$onePropHTPlot <- renderPlot({
      
      oneSampPropZTest <- OnePropZTest(input$numSuccesses, input$numTrials, input$hypProportion, OneMeanHypInfo()$alternative, SigLvl())
      htPlotCritVal <- oneSampPropZTest["Z Critical"]
      
      htPlot <- hypZTestPlot(oneSampPropZTest["Test Statistic"], htPlotCritVal, OneMeanHypInfo()$alternative)
      htPlot
    })
    
    ### ------------- Ind Means Outputs ------------------------------------------
    
    #### ------------- Uploaded Data Table --------------------------
    output$indPopMeansUploadTable <- renderDT({
      req(indmeansupload_iv$is_valid())
      datatable(IndMeansUploadData(),
                options = list(pageLength = -1,
                               lengthMenu = list(c(25, 50, 100, -1),
                                                 c("25", "50", "100", "all")),
                               columnDefs = list(list(className = 'dt-center',
                                                      targets = 0:ncol(IndMeansUploadData())))),
      )
    })
    
    #### ------------- Q-Q Plots --------------------------
    
    output$indMeansQQPlot <- renderPlot({
      # ind means qq plot
      req(input$indMeansQQPlot)
      
      if (input$dataAvailability2 == "Enter Raw Data") {
        dat1 <- createNumLst(input$raw_sample1)
        dat2 <- createNumLst(input$raw_sample2)
      } else if (input$dataAvailability2 == "Upload Data") {
        req(input$indMeansUplSample1, input$indMeansUplSample2)
        dat1 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample1]))
        dat2 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample2]))
      }
      
      df1 <- tibble(values = dat1)
      df2 <- tibble(values = dat2)
      
      # QQ plot for sample 1
      qq1 <- RenderQQPlot(
        dat = df1,
        plotColour = input[["indMeansQQPlot-Colour"]],
        plotTitle = "Sample 1 Q-Q Plot",
        plotXlab = input[["indMeansQQPlot-Xlab"]],
        plotYlab = input[["indMeansQQPlot-Ylab"]],
        gridlines = input[["indMeansQQPlot-Gridlines"]],
        flip = input[["indMeansQQPlot-Flip"]]
      )
      
      # QQ plot for sample 2
      qq2 <- RenderQQPlot(
        dat = df2,
        plotColour = input[["indMeansQQPlot-Colour"]],
        plotTitle = "Sample 2 Q-Q Plot",
        plotXlab = input[["indMeansQQPlot-Xlab"]],
        plotYlab = input[["indMeansQQPlot-Ylab"]],
        gridlines = input[["indMeansQQPlot-Gridlines"]],
        flip = input[["indMeansQQPlot-Flip"]]
      )
      
      # pairs the graphs side by side
      plot_pair <- ggpubr::ggarrange(qq1, qq2, ncol = 2)
      
      # title above the 2 graphs
      ggpubr::annotate_figure(
        plot_pair,
        top = ggpubr::text_grob(
          input[["indMeansQQPlot-Title"]],
          face = "bold",
          size = 24
        )
      )
    }, height = function() {
      GetPlotHeight(input[["indMeansQQPlot-Height"]], input[["indMeansQQPlot-HeightPx"]], ui = FALSE)
    }, width = function() {
      GetPlotWidth(input[["indMeansQQPlot-Width"]], input[["indMeansQQPlot-WidthPx"]], ui = FALSE)
    })
    
    #### ---------------- CI ----
    output$indMeansCI <- renderUI({
      
      if(IndMeansSigmaKnown() == 'bothKnown'){
        cInt <- IndMeansZInt()
        sdSymbol <- "\\sigma"
        testStat <- "z"
      }
      else if(IndMeansSigmaKnown() == 'bothUnknown'){
        cInt <- IndMeansTInt()
        sdSymbol <- "s"
        testStat <- "t"
      }
      
      tagList(
        
        p(
          withMathJax(
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothKnown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothKnown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothKnown')",
              
              uiOutput(session$ns('sigmaKnownCIFormula'))
            ),
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown')",
              
              uiOutput(session$ns('sigmaUnknownCIFormula'))
            ),
            br(),
            sprintf("\\( \\quad = (%g, %g)\\)",
                    cInt["LCL"],
                    cInt["UCL"]),
            br(),
            br(),
            br(),
            p(tags$b("Interpretation:")),
            sprintf("We are %1.0f%% confident that the difference in population means \\( (\\mu_{1} - \\mu_{2}) \\) is between \\( %g \\) and \\( %g \\).",
                    ConfLvl()*100,
                    cInt["LCL"],
                    cInt["UCL"]),
            br()
          )
        )
        
      )
      
      
    })
    
    
    #### ---------------- Boxplot ----
    output$indMeansBoxplot <- renderPlot({
      
      if(input$dataAvailability2 == 'Enter Raw Data') {
        sample1 <- createNumLst(input$raw_sample1)
        sample2 <- createNumLst(input$raw_sample2)
      } else if(input$dataAvailability2 == 'Upload Data') {
        req(input$indMeansUplSample1, input$indMeansUplSample2)
        sample1 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample1]))
        sample2 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample2]))
      }
      
      dat <- c(sample1, sample2)
      df_boxplot <- data.frame(sample = c(rep("Sample 1",length(sample1)), rep("Sample 2",length(sample2))),
                               data = c(dat))
      
      RenderSideBySideBoxplot(dat,
                              df_boxplot,
                              input[["indMeansBoxplot-Colour"]],
                              input[["indMeansBoxplot-Title"]],
                              input[["indMeansBoxplot-Xlab"]],
                              input[["indMeansBoxplot-Ylab"]],
                              input[["indMeansBoxplot-BoxWidth"]] / 10,
                              input[["indMeansBoxplot-Gridlines"]],
                              input[["indMeansBoxplot-Flip"]],
                              input[["indMeansBoxplot-OutlierLabels"]])
      
      
    }, height = function() {GetPlotHeight(input[["indMeansBoxplot-Height"]], input[["indMeansBoxplot-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["indMeansBoxplot-Width"]], input[["indMeansBoxplot-WidthPx"]], ui = FALSE)}
    )
    
    output$sigmaKnownCIFormula <- renderUI({
      
      if (input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data') {
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      zInt <- IndMeansZInt()
      
      tagList(
        
        p(
          withMathJax(
            sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( z_{\\alpha/2} \\sqrt{ \\dfrac{\\sigma_{1}^2}{n_{1}} + \\dfrac{\\sigma_{2}^2}{n_{2}} } \\right) \\)"),
            br(),
            br(),
            sprintf("\\( \\displaystyle \\quad = (%.4f - %s) \\pm \\left( %.4f \\sqrt{ \\dfrac{%.4f^2}{%.0f} + \\dfrac{%.4f^2}{%.0f} } \\right) \\)",
                    data$xbar1,
                    if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
                    zInt['Z Critical'],
                    data$sd1,
                    data$n1,
                    data$sd2,
                    data$n2),
            br(),
            br(),
            sprintf("\\( \\displaystyle \\quad = %s \\pm \\left( %g \\cdot %g \\right) \\)",
                    zInt['Difference of means'],
                    zInt['Z Critical'],
                    zInt['Std Error']),
            br(),
            br(),
            sprintf("\\( \\displaystyle \\quad = %s \\pm %g \\)",
                    zInt['Difference of means'],
                    zInt['ME'])
          )
        )
        
      )
    })
    
    
    output$sigmaUnknownCIFormula <- renderUI({
      
      if (input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data') {
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      tInt <- IndMeansTInt()
      
      showTable <- showSummaryTable()
      
      if(data$sigmaEqual) {
        sp <- round(sqrt(((data$n1-1) * data$sd1^2 + (data$n2-1) * data$sd2^2) / (data$n1 + data$n2 - 2)), 4)
        
        tagList(
          withMathJax(
            br(),
            if (showTable) {
              list(
                PrintIndMeansSummaryTable(data),
                br(),
                br())
            },
            sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( t_{\\alpha/2, \\, df} \\cdot s_{p} \\sqrt{ \\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}} } \\right) \\)"),
            br(),
            br(),
            p("where"),
            sprintf("\\( \\qquad df = n_{1} + n_{2} - 2 = %g, \\)",
                    tInt['df']),
            sprintf("\\( \\qquad t_{\\alpha/2, \\, df} = t_{%g, \\, %g} = %g \\)",
                    (1 - ConfLvl()) / 2,
                    tInt['df'],
                    tInt['T Critical']),
            br(),
            p("and"),
            sprintf("\\( \\displaystyle \\qquad s_{p} = \\sqrt{\\dfrac{(n_{1} - 1)s_{1}^2 + (n_{2} - 1)s_{2}^2}{n_{1} + n_{2} - 2}} \\)"),
            sprintf("\\( = \\sqrt{\\dfrac{(%g - 1)%g + (%g - 1)%g}{%g + %g - 2}} = %g \\)",
                    data$n1,
                    data$sd1^2,
                    data$n2,
                    data$sd2^2,
                    data$n1,
                    data$n2,
                    sp),
            br(),
            br(),
            br(),
            br(),
            br(),
            sprintf("\\( \\displaystyle CI = (%.4f - %s) \\pm \\left( %.4f \\cdot %.4f \\sqrt{ \\dfrac{1}{%.0f} + \\dfrac{1}{%.0f} } \\right) \\)",
                    data$xbar1,
                    if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
                    tInt['T Critical'],
                    sp,
                    data$n1,
                    data$n2),
            br(),
            br(),
            sprintf("\\( \\displaystyle \\quad = %g \\pm \\left( %g \\cdot %g \\right) \\)",
                    tInt['Difference of means'],
                    tInt['T Critical'],
                    tInt['Std Error']),
            br(),
            br(),
            sprintf("\\( \\displaystyle \\quad = %g \\pm %g \\)",
                    tInt['Difference of means'],
                    tInt['ME'])
            
          )
        )
      } else {
        
        tagList(
          withMathJax(
            br(),
            if (showTable) {
              list(
                PrintIndMeansSummaryTable(data),
                br(),
                br())
            },
            sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( t_{\\alpha/2, \\, \\nu} \\cdot \\sqrt{ \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} } \\right) \\)"),
            br(),
            br(),
            p("where"),
            sprintf("\\( \\displaystyle \\qquad \\nu = \\: \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }
                    { \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} \\right)^2 }{n_{1} - 1} + \\dfrac{ \\left( \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }{n_{2} - 1} } \\)"),
            sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} \\right)^2 }
                    { \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} + \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} } \\)",
                    data$sd1,
                    data$n1,
                    data$sd2,
                    data$n2,
                    data$sd1,
                    data$n1,
                    data$n1,
                    data$sd2,
                    data$n2,
                    data$n2),
            sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( %g + %g \\right)^2 }
                    { \\dfrac{ %g^2 }{%g} + \\dfrac{ %g^2 }{%g} } \\)",
                    (data$sd1^2) / data$n1,
                    (data$sd2^2) / data$n2,
                    (data$sd1^2) / data$n1,
                    data$n1 - 1,
                    (data$sd2^2) / data$n2,
                    data$n2 - 1),
            sprintf("\\( \\: = \\: %g \\)",
                    tInt['df']),
            br(),
            p("and"),
            sprintf("\\( \\qquad t_{\\alpha/2, \\, \\nu} = t_{%g, \\, %g} = %g \\)",
                    (1- ConfLvl()) / 2,
                    tInt['df'],
                    tInt['T Critical']),
            br(),
            br(),
            br(),
            br(),
            sprintf("\\( CI = (%.4f - %s) \\pm \\left( %.4f \\cdot \\sqrt{ \\dfrac{%.4f^2}{%.0f} + \\dfrac{%.4f^2}{%.0f} } \\right) \\)",
                    data$xbar1,
                    if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
                    tInt['T Critical'],
                    data$sd1,
                    data$n1,
                    data$sd2,
                    data$n2),
            br(),
            br(),
            sprintf("\\( \\displaystyle \\quad = %g \\pm \\left( %g \\cdot %g \\right) \\)",
                    tInt['Difference of means'],
                    tInt['T Critical'],
                    tInt['Std Error']),
            br(),
            br(),
            sprintf("\\( \\displaystyle \\quad = %g \\pm %g \\)",
                    tInt['Difference of means'],
                    tInt['ME'])
          )
        )
      }
      
      
    })
    
    
    #### ----------------- HT ----
    output$indMeansHT <- renderUI({
      
      withMathJax()
      
      intrpInfo <- IndMeansHypInfo()
      muNaught <- input$indMeansMuNaught
      
      if (input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data') {
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      # get test type and results based on sigma known/unknown
      if(IndMeansSigmaKnown() == 'bothKnown'){
        hTest <- IndMeansZTest()
        testStat <- "z"
        critValDF <- paste(intrpInfo$critSign, "z_{", intrpInfo$critAlph, "} = ", intrpInfo$critSign, "z_{", intrpInfo$alphaVal, "}")
      }
      else if(IndMeansSigmaKnown() == 'bothUnknown'){
        hTest <- IndMeansTTest()
        testStat <- "t"
        
        if(data$sigmaEqual) {
          critValDF <- paste(intrpInfo$critSign, "t_{", intrpInfo$critAlph, ", \\, n_{1} + n_{2} - 2} = ", intrpInfo$critSign, "t_{", intrpInfo$alphaVal, ", \\, ", hTest['df'], "}")
        } else {
          critValDF <- paste(intrpInfo$critSign, "t_{", intrpInfo$critAlph, ", \\, \\nu} = ", "\n", intrpInfo$critSign, "t_{", intrpInfo$alphaVal, ", \\, ", hTest['df'], "}")
        }
      }
      
      if(hTest["P-Value"] > SigLvl())
      {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      }
      else
      {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      }
      
      if(intrpInfo$alternative == "two.sided") {
        critVal <- paste("\\pm", hTest[2])
        
      } else {
        critVal <- hTest[2]
      }
      showTable <- showSummaryTable()
      indHTHead <- tagList(
        
        p(
          withMathJax(
            if(!muNaught) {
              list(
                sprintf("\\( H_{0}: \\mu_{1} %s \\mu_{2}\\)",
                        intrpInfo$nullHyp),
                br(),
                sprintf("\\( H_{a}: \\mu_{1} %s \\mu_{2}\\)",
                        intrpInfo$altHyp)
              )
            } else {
              list(
                sprintf("\\( H_{0}: (\\mu_{1} - \\mu_{2}) %s %s\\)",
                        intrpInfo$nullHyp, muNaught),
                br(),
                sprintf("\\( H_{a}: (\\mu_{1} - \\mu_{2}) %s %s\\)",
                        intrpInfo$altHyp, muNaught)
              )
            },
            
            br(),
            br(),
            sprintf("\\( \\alpha = %s \\)",
                    SigLvl()),
            br(),
            br(),
            
            if (showTable) {
              list(
                PrintIndMeansSummaryTable(data),
                br(),
                br())
            },
            
            p(tags$b("Test Statistic:")),
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothKnown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothKnown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothKnown')",
              
              uiOutput(session$ns('sigmaKnownHTFormula'))
            ),
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown')",
              
              uiOutput(session$ns('sigmaUnknownHTFormula'))
            ),
            br(),
            br(),
            br()
          )
        )
      )
      
      indHTPVal <- printHTPVal(hTest["P-Value"],
                               testStat,
                               intrpInfo$alternative,
                               hTest["Test Statistic"],
                               pvalSymbol,
                               reject)
      
      
      indHTTail <- tagList(
        withMathJax(
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s = %s\\)",
                  critValDF,
                  critVal),
          br(),
          
          conditionalPanel(
            ns = session$ns,
            condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown' && input.bothsigmaEqual == 'FALSE') ||
                       (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown' && input.bothsigmaEqualRaw == 'FALSE') ||
                       (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown' && input.bothsigmaEqualUpload == 'FALSE')
                       ",
            br(),
            p("where"),
            sprintf("\\( \\displaystyle \\qquad \\nu = \\: \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }
                  { \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} \\right)^2 }{n_{1} - 1} + \\dfrac{ \\left( \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }{n_{2} - 1} } \\)"),
            sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} \\right)^2 }
                  { \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} + \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} }\\)",
                  data$sd1,
                  data$n1,
                  data$sd2,
                  data$n2,
                  data$sd1,
                  data$n1,
                  data$n1,
                  data$sd2,
                  data$n2,
                  data$n2),
            sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( %0.4f + %0.4f \\right)^2 }
                  { \\dfrac{ %0.4f^2 }{%g} + \\dfrac{ %0.4f^2 }{%g} } = %s\\)",
                  (data$sd1^2) / data$n1,
                  (data$sd2^2) / data$n2,
                  (data$sd1^2) / data$n1,
                  data$n1 - 1,
                  (data$sd2^2) / data$n2,
                  data$n2 - 1,
                  hTest['df']),
            br(),
            br()
          ),
          
          br(),
          sprintf("Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
                  testStat,
                  region,
                  reject),
          br(),
          br()
          
        ),
        plotOutput(session$ns('indMeansHTPlot'), width = "75%", height = "300px"),
        br(),
      )
      
      if (!muNaught) {
        altHypExpr  <- paste0("\\mu_{1} ", intrpInfo$altHyp)
        altHypValue <- "\\mu_{2}"
      } else {
        altHypExpr  <- paste0("(\\mu_{1} - \\mu_{2}) ", intrpInfo$altHyp)
        altHypValue <- muNaught
      }
      
      indHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHypExpr, altHypValue)
      
      tagAppendChildren(indHTHead, indHTPVal, indHTTail, indHTConclusion)
    })
    
    output$sigmaKnownHTFormula <- renderUI({
      
      if (input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data') {
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      zTest <- IndMeansZTest()
      muNaught <- input$indMeansMuNaught
      
      tagList(
        withMathJax(
          sprintf("\\( z = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ \\sqrt{ \\dfrac{\\sigma_{1}^2}{n_{1}} + \\dfrac{\\sigma_{2}^2}{n_{2}} } } \\)"),
          br(),
          br(),
          sprintf("\\( \\phantom{z} = \\dfrac{ (%g - %s) -%s}{ \\sqrt{ \\dfrac{%g^2}{%.0f} + \\dfrac{%g^2}{%.0f} } } = \\dfrac{%g}{%g} = %g \\)",
                  data$xbar1,
                  if (data$xbar2 < 0) sprintf("(%g)", data$xbar2) else sprintf("%g", data$xbar2),
                  if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
                  data$sd1,
                  data$n1,
                  data$sd2,
                  data$n2,
                  zTest['Difference of means'],
                  zTest['Std Error'],
                  zTest['Test Statistic']),
          br()
        )
      )
    })
    
    output$sigmaUnknownHTFormula <- renderUI({
      
      if (input$dataAvailability2 == 'Summarized Data') {
        data <- IndMeansSummData()
      } else if(input$dataAvailability2 == 'Enter Raw Data') {
        data <- IndMeansRawData()
      } else if(input$dataAvailability2 == 'Upload Data') {
        data <- GetMeansUploadData()
      }
      
      sd1Sqrd <- data$sd1^2
      if( sd1Sqrd >= 0.0001) {
        sd1Sqrd <- round(sd1Sqrd, 4)
      } else {
        sd1Sqrd <- signif(sd1Sqrd, 1)
      }
      
      sd2Sqrd <- data$sd2^2
      if( sd2Sqrd >= 0.0001) {
        sd2Sqrd <- round(sd2Sqrd, 4)
      } else {
        sd2Sqrd <- signif(sd2Sqrd, 1)
      }
      
      muNaught <- input$indMeansMuNaught
      tTest <- IndMeansTTest()
      
      if(data$sigmaEqual == TRUE) {
        
        sp <- sqrt(((data$n1 - 1) * data$sd1^2 + (data$n2 - 1) * data$sd2^2) / (data$n1 + data$n2 - 2))
        sp <- if (sp < 0.0001 && sp > -1e-2) {
          signif(sp, 1)
        } else {
          sp
        }
        
        tagList(
          withMathJax(
            sprintf("\\( t = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ s_{p} \\sqrt{ \\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}} } } \\)"),
            br(),
            br(),
            p("where"),
            sprintf("\\( \\displaystyle \\qquad s_{p} = \\sqrt{\\dfrac{(n_{1} - 1)s_{1}^2 + (n_{2} - 1)s_{2}^2}{n_{1} + n_{2} - 2}} \\)"),
            sprintf("\\( = \\sqrt{\\dfrac{(%g - 1)%s + (%g - 1)%s}{%g + %g - 2}} = %g \\)",
                    data$n1,
                    round(sd1Sqrd, 4),
                    data$n2,
                    round(sd2Sqrd, 4),
                    data$n1,
                    data$n2,
                    round(sp, 4)),
            br(),
            br(),
            br(),
            br(),
            sprintf("\\( \\phantom{t} = \\dfrac{ (%s - %s) - %s }{ %g \\sqrt{ \\dfrac{1}{%.0f} + \\dfrac{1}{%.0f} } } \\)",
                    round(data$xbar1, 4),
                    if (data$xbar2 < 0) sprintf("(%s)", round(data$xbar2, 4)) else sprintf("%s", round(data$xbar2, 4)),
                    if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
                    round(sp, 4),
                    data$n1,
                    data$n2),
            sprintf("\\( = \\dfrac{%g}{%g} = %g \\)",
                    tTest['Difference of means'],
                    tTest['Std Error'],
                    tTest['Test Statistic']),
            br()
          )
        )
        
      } else {
        tagList(
          withMathJax(
            sprintf("\\( t = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ \\sqrt{ \\dfrac{s_{1}^2}{n_{1}} + \\dfrac{s_{2}^2}{n_{2}} } } \\)"),
            br(),
            br(),
            sprintf("\\( \\phantom{t} = \\dfrac{ (%g - %s) - %s }{ \\sqrt{ \\dfrac{%.4f^2}{%.0f} + \\dfrac{%.4f^2}{%.0f} } } = \\dfrac{%s}{%s} = %.4f \\)",
                    data$xbar1,
                    if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
                    if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
                    data$sd1,
                    data$n1,
                    data$sd2,
                    data$n2,
                    tTest['Difference of means'],
                    tTest['Std Error'],
                    tTest['Test Statistic']),
            br()
          )
        )
      }
    })
    
    #### ---------------- HT Plot ----
    output$indMeansHTPlot <- renderPlot({
      
      if(IndMeansSigmaKnown() == 'bothKnown'){
        data <- IndMeansZTest()
      }
      else if(IndMeansSigmaKnown() == 'bothUnknown'){
        data <- IndMeansTTest()
      }
      
      intrpInfo <- IndMeansHypInfo()
      htPlotCritVal <- data[2]
      
      if(IndMeansSigmaKnown() == 'bothKnown') {
        indMeansPlot <- hypZTestPlot(data['Test Statistic'], htPlotCritVal, intrpInfo$alternative)
        
      } else if(IndMeansSigmaKnown() == 'bothUnknown'){
        indMeansPlot <- hypTTestPlot(data['Test Statistic'], data['df'], htPlotCritVal, intrpInfo$alternative)
      }
      
      indMeansPlot
    })
    ### ------------ Wilcoxon Rank Sum Outputs -------------------------------------------
    output$wRankSumUploadTable <- renderDT({
      req(wilcoxonUpload_iv$is_valid())
      datatable(WilcoxonUploadData(),
                options = list(pageLength = -1,
                               lengthMenu = list(c(25, 50, 100, -1),
                                                 c("25", "50", "100", "all")),
                               columnDefs = list(list(className = 'dt-center',
                                                      targets = 0:ncol(WilcoxonUploadData())))),
      )
    })
    #### ---------------- Data Table ----
    output$wRankSumMeansData <- renderDT({
      rankSumData <- GetwRankSumMeansData()
      
      df_rankSumData <- data.frame(rankSumData$before, rankSumData$after, rankSumData$d, rankSumData$d^2)
      names(df_rankSumData) <- c("Sample 1", "Sample 2", "<em>d</em> = (Sample 1 - Sample 2)", "<em>d</em><sup>2</sup>")
      df_rankSumData <- bind_rows(df_rankSumData, summarise(df_rankSumData, across(where(is.numeric), sum)))
      rownames(df_rankSumData)[nrow(df_rankSumData)] <- "Totals"
      
      datatable(round(df_rankSumData, digits = 4),
                options = list(dom = 'lftp',
                               pageLength = -1,
                               lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")),
                               ordering = FALSE
                ),
                escape = FALSE
      ) %>% formatStyle(
        names(df_rankSumData),
        target = 'row',
        fontWeight = styleRow(dim(df_rankSumData)[1], "bold")
      )
    })
    
    #### ---------------- HT ----
    calculate_tie_correction <- function(combined_values) {
      tie_counts <- table(combined_values)
      
      tie_correction <- 0
      for (t_j in tie_counts) {
        if (t_j > 1) {  
          tie_correction <- tie_correction + (t_j^3 - t_j)
        }}
      return(tie_correction)
    }
    
    output$wilcoxonRankSum <- renderUI({
      
      req(!is.null(wilcoxonRankedData()))
      req(nrow(wilcoxonRankedData()) > 0)
      
      if (input$wilcoxonRankSumTestData == 'Upload Data') {
        req(input$wilcoxonUpl1, input$wilcoxonUpl2)
        name1 <- input$wilcoxonUpl1
        name2 <- input$wilcoxonUpl2
      } else {
        name1 <- "Sample 1"
        name2 <- "Sample 2"
      }
      data_ranked <- wilcoxonRankedData()
      n1 <- nrow(wilcoxonRankedData() %>% dplyr::filter(Group == name1))
      n2 <- nrow(wilcoxonRankedData() %>% dplyr::filter(Group == name2))
      nAll <- nrow(wilcoxonRankedData())
      mu_w <- (sum(wilcoxonRankedData()$Group == name1) * (nrow(wilcoxonRankedData()) + 1)) / 2
      
      sigma_w <- sqrt((sum(wilcoxonRankedData()$Group == name1) * sum(wilcoxonRankedData()$Group == name2) * (nrow(wilcoxonRankedData()) + 1)) / 12)
      observed_W <- sum(wilcoxonRankedData() %>% dplyr::filter(Group == name1) %>% dplyr::pull(Rank))
      observed_W2 <- sum(wilcoxonRankedData() %>% dplyr::filter(Group == name2) %>% dplyr::pull(Rank))
      significance <- 1 - SigLvl()
      
      u1_statistic <- observed_W-(n1*(n1+1)/2)
      u2_statistic <- observed_W2-(n2*(n2+1)/2)
      u_mean <- (n1*n2)/2
      
      if(input$altHypothesis2 == "2") {
        z_critical <- qnorm(1 - SigLvl()/2)
        critVal <- paste("\\pm", round(qnorm(1 - SigLvl()/2), 3))
        nullHyp <- paste0("\\text{Median}_{\\text{", name1, "}} = \\text{Median}_{\\text{", name2, "}}")
        altHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\neq \\text{Median}_{\\text{", name2, "}}")
        altern <- "two.sided"
        u_test <- u1_statistic
        correction_factor <- 0
        if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrs) &&
            input$continuityCorrectionOption == "True" && input$normaprowrs == "Normal approximation (for large samples)") {
          if (observed_W > mu_w) {
            correction_factor <- -0.5 # Subtract 0.5 if observed_W is in the upper tail
          } else if (observed_W < mu_w) {
            correction_factor <- 0.5  # Add 0.5 if observed_W is in the lower tail
          }
        }
        z_stat <- ((observed_W - mu_w + correction_factor) / sigma_w)
        in_rejection_region <- abs(z_stat) > z_critical
      } else if(input$altHypothesis2 == "1") {
        z_critical <- qnorm(SigLvl())
        critVal <- round(qnorm(SigLvl()), 3)
        nullHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\geq \\text{Median}_{\\text{", name2, "}}")
        altHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\lt \\text{Median}_{\\text{", name2, "}}")
        altern <- "less"
        u_test <- u1_statistic
        correction_factor <- 0
        if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrs) &&
            input$continuityCorrectionOption == "True" && input$normaprowrs == "Normal approximation (for large samples)") {
          correction_factor <- 0.5
        } 
        z_stat <- ((observed_W - mu_w + correction_factor) / sigma_w)
        in_rejection_region <- z_stat < z_critical
      } else {
        z_critical <- qnorm(1 - SigLvl())
        critVal <- round(qnorm(1 - SigLvl()), 3)
        nullHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\leq \\text{Median}_{\\text{", name2, "}}")
        altHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\gt \\text{Median}_{\\text{", name2, "}}")
        altern <- "greater"
        u_test <- u1_statistic
        correction_factor <- 0
        
        if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrs) &&
            input$continuityCorrectionOption == "True" && input$normaprowrs == "Normal approximation (for large samples)") {
          correction_factor <- -0.5
        }
        z_stat <- ((observed_W - mu_w + correction_factor) / sigma_w)
        in_rejection_region <- z_stat > z_critical
      }
      
      if(in_rejection_region) {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      } else {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      }
      group1_data <- data_ranked %>%
        dplyr::filter(Group == name1) %>%
        dplyr::pull(Value)
      
      group2_data <- data_ranked %>%
        dplyr::filter(Group == name2) %>%
        dplyr::pull(Value)
      combined_values <- c(group1_data, group2_data)
      has_ties <- length(unique(combined_values)) < length(combined_values)      
      
      #no ties in data for p value
      if(has_ties){
        if(input$altHypothesis2 == "2") {
          p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
        } else if(input$altHypothesis2 == "1") {
          p_value <- pnorm(z_stat, lower.tail = TRUE)
        } else {
          p_value <- pnorm(z_stat, lower.tail = FALSE)
        }
      }
      else if(!has_ties){
        test_result <- suppressWarnings(
          wilcox.test(group1_data, group2_data, paired = FALSE, alternative = altern, 
                      conf.level = significance, exact = TRUE, conf.int = TRUE)
        )
        p_value <- test_result$p.value
      }
      
      # Confidence Interval for Two sided, Left and Right sided
      if (input$normaprowrs == "Exact"){
        if(input$altHypothesis2 == "2") {
          lower <- qwilcox(SigLvl() / 2, m = n1, n = n2, lower.tail = TRUE)
          upper <- qwilcox(SigLvl() / 2, m = n1, n = n2, lower.tail = FALSE)
        } else if(input$altHypothesis2 == "1") { # alternative = Less than
          lower <- qwilcox(SigLvl(), m = n1, n = n2, lower.tail = TRUE) 
          upper <- Inf 
        } else { # alternative = greater than
          lower <- -Inf 
          upper <- qwilcox(SigLvl(), m = n1, n = n2, lower.tail = FALSE)
        }
      }
      
      if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrs) &&
          input$continuityCorrectionOption == "True" && input$normaprowrs == "Normal approximation (for large samples)"){
        if(has_ties){
          test_result <- suppressWarnings(
            wilcox.test(group1_data, group2_data, paired = FALSE, alternative = altern, 
                        conf.level = significance, exact = TRUE, correct = TRUE)
          )
          p_value <- test_result$p.value
        }
        else{
          test_result <- suppressWarnings(
            wilcox.test(group1_data, group2_data, paired = FALSE, alternative = altern, 
                        conf.level = significance, exact = FALSE, correct = TRUE)
          )
          p_value <- test_result$p.value
        }
      } 
      else if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrs) &&
               input$continuityCorrectionOption == "False" && input$normaprowrs == "Normal approximation (for large samples)"){
        if(has_ties){
          test_result <- suppressWarnings(
            wilcox.test(group1_data, group2_data, paired = FALSE, alternative = altern, 
                        conf.level = significance, exact = TRUE, correct = FALSE)
          )
          p_value <- test_result$p.value
        }
        else{
          test_result <- suppressWarnings(
            wilcox.test(group1_data, group2_data, paired = FALSE, alternative = altern, 
                        conf.level = significance, exact = FALSE, correct = FALSE)
          )
          p_value <- test_result$p.value
        }
        
      }
      
      tie_correction <- calculate_tie_correction(combined_values)
      u_std_dev <- sqrt((n1 * n2 / 12) * ((nAll + 1) - (tie_correction / (nAll * (nAll - 1)))))
      mw_z_stat <- ((u_test - u_mean) / u_std_dev)
      
      
      if (input$normaprowrs == "Exact"){
        z_stat <- mw_z_stat
      }
      rankSumHTHead <- tagList(
        p(
          withMathJax(),
          sprintf("\\( H_{0}:\\ %s\\)", nullHyp),
          br(),
          sprintf("\\( H_{a}:\\ %s\\)", altHyp),
          br(), br(),
          sprintf("\\( \\alpha = %s \\)", SigLvl()),
          br(),br(),
          sprintf("\\(n_{1} = %s\\)", n1),
          br(),
          sprintf("\\(n_{2} = %s\\)", n2),
          br(),
          sprintf("\\( n = n_{1} + n_{2} = %s \\)", nrow(wilcoxonRankedData())),
          
          br(),
          
          p(tags$b("Sum of Ranks:")),
          sprintf("\\(  W_{1} = %s \\)", observed_W),
          br(),
          sprintf("\\(  W_{2} = %s \\)", observed_W2),
          br(),br(),
          
          if (input$normaprowrs == "Exact") {
            tagList(
              p(tags$b("Mann-Whitney ", tags$i("U"), " Statistic:")),
              sprintf("\\(  U_{1} = W_{1} - \\frac{n_{1}(n_{1} + 1)}{2} = %s - \\frac{%s (%s + 1)}{2} = %s \\)", observed_W, n1, n1, u1_statistic),
              br(), br(), 
              sprintf("\\( U_{2} = W_{2} - \\frac{n_{2}(n_{2} + 1)}{2} = %s - \\frac{%s (%s + 1)}{2} = %s \\)", observed_W2, n2, n2, u2_statistic),
              helpText("*Note: By default, U1 is always chosen as a test statistic."),
              br(),
            
          #    p(tags$b("Mann-Whitney U Expected Mean:")),
          #    sprintf("\\( \\qquad \\mu_{U} = \\frac{n_{1}n_{2}}{2} = \\frac{%s(%s)}{2} = %s \\)", n1, n2, u_mean),
          #    br(),br(),
          #  
          #    p(tags$b("Mann-Whitney U Standard Deviation:")),
          #    sprintf("\\( \\qquad \\sigma_U = \\sqrt{\\frac{n_1 n_2}{12}\\left( (n+1) - \\frac{\\sum_{j=1}^{g} (t_j^3 - t_j)}{n(n-1)}\\right)} = 
          #            \\sqrt{\\frac{%s \\times %s}{12}\\left( (%s+1) - \\frac{%s}{%s \\times (%s-1)}\\right)} = %s \\)",
          #            n1, n2, nAll, ifelse(has_ties, tie_correction, 0), nAll, nAll, round(u_std_dev, 4)),
          #    br(), br(),
          #    
          #    p(tags$b("Mann-Whitney U Test Statistic:")),
          #    sprintf("\\( \\qquad z = \\frac{U - \\mu_{U}}{\\sigma_{U}} = \\frac{%s - %s}{%s} = %s \\)",
          #            round(u_test, 4), round(u_mean, 4), round(u_std_dev, 4), round(mw_z_stat, 3)),
              
            )}
          else{
          if (input$normaprowrs == "Normal approximation (for large samples)") {
            tagList(
              p(tags$b("Mean:")),
              sprintf("\\(  \\mu_{W} = \\frac{n_{1}(n + 1)}{2} = \\frac{%s(%s + 1)}{2} = %s \\)",
                      n1, nAll, (sum(wilcoxonRankedData()$Group == name1) * (nrow(wilcoxonRankedData()) + 1)) / 2
              ),
              br(), br(),
              p(tags$b("Standard Deviation:")),
              sprintf("\\( \\sigma_W = \\sqrt{\\frac{n_{1}n_{2}(n + 1)}{12}} = \\sqrt{\\frac{%s \\times %s (%s + 1)}{12}} = %s \\)",
                      n1, n2, nAll, round(sigma_w, 4)),
              br(),br(),
              
              p(tags$b("Test Statistic:")),
              
              if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrs) &&
                  input$continuityCorrectionOption == "True" && input$normaprowrs == "Normal approximation (for large samples)") {
                
                if(input$altHypothesis2 == "1") { # Less than alternative
                  sprintf("\\( z = \\frac{W - \\mu_W + 0.5}{\\sigma_W} = \\frac{%s - %s + %s}{%s} = %s \\)",
                          round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3))
                }
                else if(input$altHypothesis2 == "2") { # Two-sided alternative
                  if (observed_W > mu_w) {
                    sprintf("\\( z = \\frac{W - \\mu_W - 0.5}{\\sigma_W} = \\frac{%s - %s - %s}{%s} = %s \\)",
                            round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3))
                  } else if (observed_W < mu_w) {
                    sprintf("\\( z = \\frac{W - \\mu_W + 0.5}{\\sigma_W} = \\frac{%s - %s + %s}{%s} = %s \\)",
                            round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3))
                  } else { # observed_W == mu_w, no continuity correction applied in formula
                    sprintf("\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                            round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3))
                  }
                }
                else { # Greater than alternative (assuming input$altHypothesis2 corresponds to "greater")
                  sprintf("\\( z = \\frac{W - \\mu_W - 0.5}{\\sigma_W} = \\frac{%s - %s - %s}{%s} = %s \\)",
                          round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3))
                }
                
              } else { # No continuity correction
                if(input$altHypothesis2 == "1") { # Less than alternative
                  sprintf("\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                          round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3))
                }
                else if(input$altHypothesis2 == "2") { # Two-sided alternative
                  sprintf("\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                          round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3))
                }
                else { # Greater than alternative
                  sprintf("\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                          round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3))
                }
              },
              br(), br(),
            )}#,
          },
          
          
          p(tags$b("Using P-value Method:")),
          sprintf("\\( P = %s \\)", ifelse(is.na(p_value), "NA", round(p_value, 4))),
          if (has_ties && input$normaprowrs == "Exact"){
            helpText("*Note: Exact p-values cannot be computed in the presence of ties. Normal approximation was used.")
          },
          
          br(),
          if (p_value <= SigLvl()) {
            tagList(
            sprintf("\\( \\text{Since } P \\leq %s, \\text{reject } H_0. \\)", SigLvl()),
            br(), br())
          } else {
            tagList(
            sprintf("\\( \\text{Since } P > %s, \\text{do not reject } H_0. \\)", SigLvl()),
            br(), br())
          },
          
          if (input$normaprowrs == "Exact"){
            tagList(
              p(tags$b("Using Critical Value Method:")),

              if (input$altHypothesis2 == "2") { # Two-sided
                sprintf("\\( \\text{Rejection Region: reject }H_0 \\text{ whenever } U \\leq %s \\text{ or } U \\geq %s \\)",
                        round(lower, 3), round(upper, 3))
              } else if (input$altHypothesis2 == "1") { # Left-sided
                sprintf("\\(  \\text{Rejection Region: reject }H_0 \\text{ whenever } U \\leq %s \\)", round(lower, 3))
              } else { # Right-sided
                sprintf("\\(  \\text{Rejection Region: reject }H_0 \\text{ whenever } U \\geq %s \\)", round(upper, 3))
              },
              br(),
              
              sprintf("\\( \\text{Observed Test Statistic: } U = %s \\)", round(u1_statistic, 3)),
              br(), br(),

              if (input$altHypothesis2 == "2") { # Two-sided decision
                if (u1_statistic <= lower || u1_statistic >= upper) {
                  sprintf("\\( \\text{Since the test statistic } U_{1} \\text{ falls in the rejection region } (U \\leq %s \\text{ or } U \\geq %s), \\text{reject } H_0. \\)",
                          round(lower, 3), round(upper, 3))
                } else {
                  sprintf("\\( \\text{Since the test statistic } U_{1} \\text{ does not fall in the rejection region } (%s < U < %s), \\text{do not reject } H_0. \\)",
                           round(lower, 3), round(upper, 3))
                }
              } else if (input$altHypothesis2 == "1") { # Left-sided decision
                if (u1_statistic <= lower) {
                  sprintf("\\( \\text{Since the test statistic } U_{1} \\text{ falls in the rejection region } (U \\leq %s), \\text{reject } H_0. \\)",
                           round(lower, 3))
                } else {
                  sprintf("\\( \\text{Since the test statistic } U_{1} \\text{ does not fall in the rejection region } (U > %s), \\text{do not reject } H_0. \\)",
                          round(lower, 3))
                }
              } else { # Right-sided decision
                if (u1_statistic >= upper) {
                  sprintf("\\( \\text{Since the test statistic } U_{1} \\text{ falls in the rejection region } (U \\geq %s), \\text{reject } H_0. \\)",
                           round(upper, 3))
                } else {
                  sprintf("\\( \\text{Since the test statistic } U_{1} \\text{ does not fall in the rejection region } (U < %s), \\text{do not reject } H_0. \\)",
                          round(upper, 3))
                }
              }
          )}
        )
      )
      
      rankSumHTTail <- 
        if (input$normaprowrs == "Normal approximation (for large samples)") {
        tagList(
        p(
          withMathJax(),
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s z_{%s} = %s \\)",
                  if(input$altHypothesis2 == "2") "\\pm" else if(input$altHypothesis2 == "1") "-" else "",
                  if(input$altHypothesis2 == "2") SigLvl()/2 else SigLvl(),
                  critVal),
          br(),
          br(),
          sprintf("Since the test statistic \\( (z = %s)\\) falls within the %s region, %s \\( H_{0}\\).",
                  round(z_stat, 3),
                  region,
                  reject),
          br(),
        ),
        
        plotOutput(session$ns('wilcoxonRankSumPlot'), width = "75%", height = "300px"),
        br()
      )}
      
      depHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, "")
      
      tagAppendChildren(rankSumHTHead, rankSumHTTail, depHTConclusion)
    })
    
    #### ---------------- HT Plot ----
    calculate_z_stat <- function(input, observed_W, mu_w, sigma_w, observed_W2, n1, n2, N, u1_statistic, u2_statistic, u_mean, u_std_dev,
                                 has_ties, tie_correction) {
      z_stat_val <- NA
      correction_factor <- 0
      
      u_test_val <- u1_statistic 
      
      if (input$normaprowrs == "Exact") {
        z_stat_val <- ((u_test_val - u_mean) / u_std_dev)
      } else { 
        if (!is.null(input$continuityCorrectionOption) && input$continuityCorrectionOption == "True") {
          if (input$altHypothesis2 == "2") {
            if (observed_W > mu_w) {
              correction_factor <- -0.5
            } else if (observed_W < mu_w) {
              correction_factor <- 0.5
            }
          } else if (input$altHypothesis2 == "1") { # Less than test
            correction_factor <- 0.5
          } else { 
            correction_factor <- -0.5
          }
        }
        z_stat_val <- ((observed_W - mu_w + correction_factor) / sigma_w)
      }
      return(z_stat_val)
    }
    
    output$wilcoxonRankSumPlot <- renderPlot({
      
      dat <- GetwRankSumMeansData()
      if(length(unique(dat$samp1)) > 1 || length(unique(dat$samp2)) > 1) {
        
        wilcoxonData <- wilcoxonRankedData()
        
        if (input$wilcoxonRankSumTestData == 'Upload Data') {
          name1 <- input$wilcoxonUpl1
          name2 <- input$wilcoxonUpl2
        } else {
          name1 <- "Sample 1"
          name2 <- "Sample 2"
        }
        
        n1 <- sum(wilcoxonData$Group == name1)
        n2 <- sum(wilcoxonData$Group == name2)
        N <- nrow(wilcoxonData)
        
        mu_w <- (n1 * (N + 1)) / 2
        sigma_w <- sqrt((n1 * n2 * (N + 1)) / 12)
        observed_W <- sum(wilcoxonData %>% dplyr::filter(Group == name1) %>% dplyr::pull(Rank))
        observed_W2 <- sum(wilcoxonData %>% dplyr::filter(Group == name2) %>% dplyr::pull(Rank))
        
        u1_statistic <- observed_W - (n1 * (n1 + 1) / 2)
        u2_statistic <- observed_W2 - (n2 * (n2 + 1) / 2)
        u_mean <- (n1 * n2) / 2
        
        group1_data_values <- wilcoxonData %>% dplyr::filter(Group == name1) %>% dplyr::pull(Value)
        group2_data_values <- wilcoxonData %>% dplyr::filter(Group == name2) %>% dplyr::pull(Value)
        combined_values <- c(group1_data_values, group2_data_values)
        has_ties <- length(unique(combined_values)) < length(combined_values)
        
        calculate_tie_correction <- function(x) {
          if (!is.numeric(x) || length(x) == 0) {
            return(0)
          }
          tie_counts <- table(x[duplicated(x) | duplicated(x, fromLast = TRUE)])
          sum(tie_counts^3 - tie_counts)
        }
        
        tie_correction <- calculate_tie_correction(combined_values)
        u_std_dev <- sqrt((n1 * n2 / 12) * ((N + 1) - (tie_correction / (N * (N - 1)))))
        z_stat <- calculate_z_stat(input, observed_W, mu_w, sigma_w, observed_W2, n1, n2, N,
                                   u1_statistic, u2_statistic, u_mean, u_std_dev,
                                   has_ties, tie_correction)
        alternative <- ""
        z_critical <- NA
        if(input$altHypothesis2 == "2") {
          z_critical <- qnorm(1 - SigLvl()/2)
          alternative <- "two.sided"
        } else if(input$altHypothesis2 == "1") {
          z_critical <- qnorm(SigLvl())
          alternative <- "less"
        } else {
          z_critical <- qnorm(1 - SigLvl())
          alternative <- "greater"
        }
        wilcoxonPlot <- wilcoxonZTestPlot(z_stat, z_critical, alternative)
        wilcoxonPlot
      }
    })
    
    ###---- Boxplot Side by Side
    output$sidebysidewRankSum <- renderPlot({
      
      req(input$sidebysidewRankSum)
      
      if(input$wilcoxonRankSumTestData == 'Enter Raw Data') {
        rankSumRaw1 <- createNumLst(input$rankSumRaw1)
        rankSumRaw2 <- createNumLst(input$rankSumRaw2)
      } else if(input$wilcoxonRankSumTestData == 'Upload Data') {
        rankSumRaw1 <- na.omit(unlist(WilcoxonUploadData()[,input$wilcoxonUpl1]))
        rankSumRaw2 <- na.omit(unlist(WilcoxonUploadData()[,input$wilcoxonUpl2]))
      }
      
      dat <- c(rankSumRaw1, rankSumRaw2)
      df_boxplot <- data.frame(sample = c(rep("Sample 1",length(rankSumRaw1)), rep("Sample 2",length(rankSumRaw2))),
                               data = c(dat))
      
      RenderSideBySideBoxplot(dat,
                              df_boxplot,
                              input[["sidebysidewRankSum-Colour"]],
                              input[["sidebysidewRankSum-Title"]],
                              input[["sidebysidewRankSum-Xlab"]],
                              input[["sidebysidewRankSum-Ylab"]],
                              input[["sidebysidewRankSum-BoxWidth"]] / 10,
                              input[["sidebysidewRankSum-Gridlines"]],
                              input[["sidebysidewRankSum-Flip"]],
                              input[["sidebysidewRankSum-OutlierLabels"]])
      
    }, height = function() {GetPlotHeight(input[["sidebysidewRankSum-Height"]], input[["sidebysidewRankSum-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["sidebysidewRankSum-Width"]], input[["sidebysidewRankSum-WidthPx"]], ui = FALSE)}
    )
    
    ###----- QQ Plot
    output$sidebysidewRankQQ <- renderPlot({
      
      req(input$sidebysidewRankQQ)
      
      if(input$wilcoxonRankSumTestData == 'Enter Raw Data') {
        rankSumRaw1 <- createNumLst(input$rankSumRaw1)
        rankSumRaw2 <- createNumLst(input$rankSumRaw2)
      } else if(input$wilcoxonRankSumTestData == 'Upload Data') {
        rankSumRaw1 <- na.omit(unlist(WilcoxonUploadData()[,input$wilcoxonUpl1]))
        rankSumRaw2 <- na.omit(unlist(WilcoxonUploadData()[,input$wilcoxonUpl2]))
      }
      
      RenderWilcoxQQPlots(rankSumRaw1,
                          rankSumRaw2,
                          input[["sidebysidewRankQQ-Colour"]],
                          input[["sidebysidewRankQQ-Title"]],
                          input[["sidebysidewRankQQ-Xlab"]],
                          input[["sidebysidewRankQQ-Ylab"]],
                          input[["sidebysidewRankQQ-Gridlines"]],
                          input[["sidebysidewRankQQ-Flip"]])
      
    }, height = function() {GetPlotHeight(input[["sidebysidewRankQQ-Height"]], input[["sidebysidewRankQQ-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["sidebysidewRankQQ-Width"]], input[["sidebysidewRankQQ-WidthPx"]], ui = FALSE)}
    )
    
    
    ### ------------ Dep Means Outputs -------------------------------------------
    
    #### ------------ Uploaded Data Table (no totals) ----------------------------------
    output$depPopMeansUploadTable <- renderDT({
      req(depmeansupload_iv$is_valid())
      datatable(DepMeansUploadData(),
                options = list(pageLength = -1,
                               lengthMenu = list(c(25, 50, 100, -1),
                                                 c("25", "50", "100", "all")),
                               columnDefs = list(list(className = 'dt-center',
                                                      targets = 0:ncol(DepMeansUploadData())))),
      )
    })
    
    #### ----------- Q-Q Plots ----------------------------------------------------------
    output$depMeansQQPlot <- renderPlot({
      # dep means qq plot
      req(input$depMeansQQPlot)
      
      dat <- GetDepMeansData()
      
      # dat$d is the difference between the samples (i.e before - after)
      df <- tibble(values = dat$d)
      
      RenderQQPlot(
        dat = df,
        plotColour = input[["depMeansQQPlot-Colour"]],
        plotTitle = input[["depMeansQQPlot-Title"]],
        plotXlab = input[["depMeansQQPlot-Xlab"]],
        plotYlab = input[["depMeansQQPlot-Ylab"]],
        gridlines = input[["depMeansQQPlot-Gridlines"]],
        flip = input[["depMeansQQPlot-Flip"]]
      )
    }, height = function() {
      GetPlotHeight(input[["depMeansQQPlot-Height"]], input[["depMeansQQPlot-HeightPx"]], ui = FALSE)
    }, width = function() {
      GetPlotWidth(input[["depMeansQQPlot-Width"]], input[["depMeansQQPlot-WidthPx"]], ui = FALSE)
    })
    
    #### ---------------- Data Table ----
    output$depMeansData <- renderDT({
      depData <- GetDepMeansData()
      
      df_depData <- data.frame(depData$before, depData$after, depData$d, depData$d^2)
      names(df_depData) <- c("Sample 1", "Sample 2", "<em>d</em> = (Sample 1 - Sample 2)", "<em>d</em><sup>2</sup>")
      df_depData <- bind_rows(df_depData, summarise(df_depData, across(where(is.numeric), sum)))
      rownames(df_depData)[nrow(df_depData)] <- "Totals"
      
      datatable(round(df_depData, digits = 4),
                options = list(dom = 'lftp',
                               pageLength = -1,
                               lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")),
                               ordering = FALSE
                ),
                escape = FALSE
      ) %>% formatStyle(
        names(df_depData),
        target = 'row',
        fontWeight = styleRow(dim(df_depData)[1], "bold")
      )
    })
    
    #### ---------------- CI ----
    output$depMeansCI <- renderUI({
      tInt <- DepMeansTInt()
      dSum <- round(sum(GetDepMeansData()$d), 4)
      dSqrdSum <- round(sum(GetDepMeansData()$d^2), 4)
      
      p(
        withMathJax(),
        br(),
        sprintf("\\( \\displaystyle CI = \\bar{d} \\pm \\left( t_{\\alpha/2, \\, df} \\cdot \\dfrac{ s_{d} }{ \\sqrt{n} } \\right) \\)"),
        br(),
        br(),
        p("where"),
        sprintf("\\( \\qquad \\bar{d} = \\dfrac{ \\sum d }{ n } = \\dfrac{%s}{%s} = %s \\; , \\)",
                dSum,
                tInt["Sample Size"],
                tInt["Sample Mean"]),
        sprintf("\\( \\qquad s_{d} = \\sqrt{ \\dfrac{\\sum d^{2} - \\dfrac{(\\sum d)^{2}}{n} }{n - 1} } \\)"),
        sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\; , \\)",
                dSqrdSum,
                dSum,
                tInt["Sample Size"],
                tInt["Sample Size"],
                tInt['Sample SD']),
        sprintf("\\( \\qquad df = n - 1 = %s \\)",
                tInt["Sample Size"] - 1),
        br(),
        br(),
        br(),
        sprintf("\\( \\displaystyle CI = %g \\pm \\left( t_{%g/2, \\, %g} \\cdot \\dfrac{ %g }{ \\sqrt{ %g } } \\right) \\)",
                tInt["Sample Mean"],
                1 - ConfLvl(),
                tInt["Sample Size"] - 1,
                tInt["Sample SD"],
                tInt["Sample Size"]),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = %g \\pm \\left( t_{%g, \\, %g} \\cdot \\dfrac{ %g }{ %g } \\right) \\)",
                tInt["Sample Mean"],
                (1 - ConfLvl()) / 2,
                tInt["Sample Size"] - 1,
                tInt["Sample SD"],
                sqrt(tInt["Sample Size"])),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = %g \\pm ( %g \\cdot %g ) \\)",
                tInt["Sample Mean"],
                tInt["T Critical"],
                tInt["Std Error"]),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = %g \\pm  %g  \\)",
                tInt["Sample Mean"],
                tInt["ME"]),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = (%g, \\, %g)  \\)",
                tInt["LCL"],
                tInt["UCL"]),
        br(),
        br(),
        br(),
        p(tags$b("Interpretation:")),
        sprintf("We are \\( %1.0f \\)%% confident that the population mean difference \\( (\\mu_{d})\\) is between \\( %g \\) and \\( %g \\).",
                ConfLvl()*100,
                tInt["LCL"],
                tInt["UCL"]),
        br(),
        br(),
        br()
      )
    })
    
    #### ---------------- HT ----
    output$depMeansHT <- renderUI({
      req(GetDepMeansData()$sd != 0)
      
      tTest <- DepMeansTTest()
      dSum      <- round(sum(GetDepMeansData()$d), 4)
      dSqrdSum  <- round(sum(GetDepMeansData()$d^2), 4)
      muNaught  <- round(input$depMeansMuNaught, 4)
      
      intrpInfo <- IndMeansHypInfo()
      
      if(tTest["P-Value"] > SigLvl()) {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      } else {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      }
      
      if(input$altHypothesis2 == "2")
      {
        critVal <- paste("\\pm", tTest["T Critical"])
        nullHyp <- "\\mu_{d} ="
        altHyp <- "\\mu_{d} \\neq"
      }
      else
      {
        critVal <- tTest["T Critical"]
        
        if(input$altHypothesis2 == "1"){
          nullHyp <- "\\mu_{d} \\geq"
          altHyp <- "\\mu_{d} \\lt"
        } else {
          nullHyp <- "\\mu_{d} \\leq"
          altHyp <- "\\mu_{d} \\gt"
        }
      }
      
      depHTHead <- tagList(
        p(
          withMathJax(),

          sprintf("\\( H_{0}: %s %s\\)",
                  nullHyp, muNaught),
          br(),
          sprintf("\\( H_{a}: %s %s\\)",
                  altHyp, muNaught),
          br(),
          br(),
          sprintf("\\( \\alpha = %s \\)",
                  SigLvl()),
          br(),
          br(),
          p(tags$b("Test Statistic:")),
          sprintf("\\( \\displaystyle t = \\dfrac{\\bar{d} - (\\mu_{d})_{0}}{ \\left( \\dfrac{ s_{d} }{ \\sqrt{n} } \\right) } \\qquad \\)"),
          br(),
          br(),
          p("where"),
          #HERE
          sprintf("\\( \\qquad \\bar{d} = \\dfrac{ \\sum d }{ n } = \\dfrac{%s}{%s} = %s \\; , \\)",
                  dSum,
                  tTest["Sample Size"],
                  tTest["Sample Mean"]),
          sprintf("\\( \\qquad s_{d} = \\sqrt{ \\dfrac{\\sum d^{2} - \\dfrac{(\\sum d)^{2}}{n} }{n - 1} } \\)"),
          sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\; , \\)",
                  dSqrdSum,
                  dSum,
                  tTest["Sample Size"],
                  tTest["Sample Size"],
                  tTest['Sample SD']),
          br(),
          br(),
          br(),
          sprintf("\\( t = \\dfrac{%g - %s}{ \\left( \\dfrac{ %g }{ \\sqrt{ %g } } \\right) } \\)",
                  tTest["Sample Mean"],
                  if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
                  tTest["Sample SD"],
                  tTest["Sample Size"]),
          sprintf("\\( \\displaystyle \\; = \\; \\dfrac{%g}{ \\left( \\dfrac{ %g }{ %g } \\right) } \\)",
                  tTest["Sample Mean"] - muNaught, 
                  tTest["Sample SD"],
                  sqrt(tTest["Sample Size"])),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\phantom{t} = \\; \\dfrac{ %g }{ %g } \\)",
                  tTest["Sample Mean"] - muNaught,
                  tTest["Std Error"]),
          sprintf("\\( \\displaystyle \\; = \\; %g \\)",
                  tTest["Test Statistic"]),
          br(),
          br(),
          br()
        )
      )
      
      depHTPVal <- printHTPVal(tTest["P-Value"],
                               "t",
                               intrpInfo$alternative,
                               tTest["Test Statistic"],
                               pvalSymbol,
                               reject)
      
      depHTTail <- tagList(
        p(
          withMathJax(),
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s t_{%s, \\, df} = %s t_{%s, \\, %s} = %s \\)",
                  IndMeansHypInfo()$critSign,
                  IndMeansHypInfo()$critAlph,
                  IndMeansHypInfo()$critSign,
                  IndMeansHypInfo()$alphaVal,
                  tTest["Sample Size"] - 1,
                  critVal),
          br(),
          br(),
          p("where"),
          sprintf("\\( \\qquad df = n - 1 = %s \\)",
                  tTest["Sample Size"] - 1),
          br(),
          br(),
          sprintf("Since the test statistic \\( (t)\\) falls within the %s region, %s \\( H_{0}\\).",
                  region,
                  reject),
          br(),
        ),
        
        plotOutput(session$ns('depMeansHTPlot'), width = "75%", height = "300px"),
        br()
      )
      
      depHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, muNaught)
      
      tagAppendChildren(depHTHead, depHTPVal, depHTTail, depHTConclusion)
    })
    
    #### ---------------- HT Plot ----
    output$depMeansHTPlot <- renderPlot({
      
      if(GetDepMeansData()$sd != 0) {
        tTest <- DepMeansTTest()
        intrpInfo <- IndMeansHypInfo()
        
        htPlotCritVal <- tTest["T Critical"]
        
        depMeansPlot <- hypTTestPlot(tTest["Test Statistic"], tTest["df"], htPlotCritVal, intrpInfo$alternative)
        depMeansPlot
      }
      
    })
    
    ### ------------ Two Prop Outputs --------------------------------------------
    
    #### ---------------- CI ----
    output$twoPropCI <- renderUI({
      req(si_iv$is_valid())
      
      twoPropZInt <- TwoPropZInt(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, ConfLvl())
      twoPropZInt["Z Critical"] <- round(twoPropZInt["Z Critical"], cvDigits)
      
      p(
        withMathJax(
          sprintf("Given:"),
          br(),
          sprintf("\\( x_{1} = %s \\)",
                  input$numSuccesses1),
          br(),
          sprintf("\\( n_{1} = %s \\)",
                  input$numTrials1),
          br(),
          sprintf("\\( x_{2} = %s \\)",
                  input$numSuccesses2),
          br(),
          sprintf("\\( n_{2} = %s \\)",
                  input$numTrials2),
          br(),
          br(),
          br(),
          sprintf("For a \\( %s \\)%% Confidence Interval: ",
                  ConfLvl()*100),
          br(),
          sprintf("\\( \\alpha = 1 - %s = %s \\)",
                  ConfLvl(),
                  1 - ConfLvl()),
          br(),
          sprintf("\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
                  1 - ConfLvl(),
                  (1 - ConfLvl()) / 2,
                  twoPropZInt["Z Critical"]),
          br(),
          br(),
          br(),
          sprintf("\\( \\displaystyle CI = (\\hat{p}_{1} - \\hat{p}_{2}) \\pm \\left( z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_{1}(1-\\hat{p}_{1})}{n_{1}} + \\dfrac{\\hat{p}_{2}(1-\\hat{p}_{2})}{n_{2}}} \\right) \\)"),
          br(),
          p("where"),
          sprintf("\\( \\displaystyle \\qquad \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                  input$numSuccesses1,
                  input$numTrials1,
                  twoPropZInt["Sample Proportion 1"]),
          br(),
          p("and"),
          sprintf("\\( \\displaystyle \\qquad \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                  input$numSuccesses2,
                  input$numTrials2,
                  twoPropZInt["Sample Proportion 2"]),
          br(),
          br(),
          br(),
          sprintf("\\( \\displaystyle CI = (%0.4f - %0.4f) \\pm \\left( %s \\sqrt{\\dfrac{%0.4f(1-%0.4f)}{%1.0f} + \\dfrac{%0.4f(1-%0.4f)}{%1.0f}} \\right) \\)",
                  twoPropZInt["Sample Proportion 1"],
                  twoPropZInt["Sample Proportion 2"],
                  twoPropZInt["Z Critical"],
                  twoPropZInt["Sample Proportion 1"],
                  twoPropZInt["Sample Proportion 1"],
                  input$numTrials1,
                  twoPropZInt["Sample Proportion 2"],
                  twoPropZInt["Sample Proportion 2"],
                  input$numTrials2),
          br(),
          br(),
          sprintf("\\( \\phantom{CI} = %0.4f \\pm ( %s \\cdot %0.4f ) \\)",
                  twoPropZInt["Difference of proportions"],
                  twoPropZInt["Z Critical"],
                  twoPropZInt["Std Error"]),
          br(),
          br(),
          sprintf("\\( \\phantom{CI} = %0.4f \\pm %0.4f \\)",
                  twoPropZInt["Difference of proportions"],
                  twoPropZInt["Margin of Error"]),
          br(),
          br(),
          sprintf("\\( \\phantom{CI} = (%0.4f, %0.4f)\\)",
                  twoPropZInt["LCL"],
                  twoPropZInt["UCL"]),
          br(),
          br(),
          br(),
          p(tags$b("Interpretation:")),
          sprintf("We are %1.0f%% confident that the difference in population proportions \\( (p_{1} - p_{2}) \\) is between \\( %0.4f \\) and \\( %0.4f \\).",
                  ConfLvl()*100,
                  twoPropZInt["LCL"],
                  twoPropZInt["UCL"])
        )
      )
    })
    
    #### ---------------- HT ----
    output$twoPropHT <- renderUI({
      req(si_iv$is_valid())
      
      diffNaught <- input$propDiffNaught
      twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, diffNaught, IndMeansHypInfo()$alternative, SigLvl())
      twoPropZTest["Z Critical"] <- round(twoPropZTest["Z Critical"], cvDigits)
      
      
      if(input$altHypothesis2 == "2")
      {
        critZVal <- paste("\\pm", twoPropZTest["Z Critical"])
        htPlotCritVals <- c(-twoPropZTest["Z Critical"], twoPropZTest["Z Critical"])
        nullHyp <- "p_{1} ="
        altHyp <- "p_{1} \\neq"
      }
      else
      {
        critZVal <- paste(twoPropZTest["Z Critical"])
        htPlotCritVals <- twoPropZTest["Z Critical"]
        
        if(input$altHypothesis2 == "1"){
          nullHyp <- "p_{1} \\geq"
          altHyp <- "p_{1} \\lt"
        } else {
          nullHyp <- "p_{1} \\leq"
          altHyp <- "p_{1} \\gt"
        }
      }
      
      propDiff <- twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"]
      
      if(twoPropZTest["P-Value"] > SigLvl())
      {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      }
      else
      {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      }
      
      twoPropHTHead <- tagList(
        withMathJax(
          sprintf("\\( H_{0}: %s p_{2}\\)",
                  nullHyp),
          br(),
          sprintf("\\( H_{a}: %s p_{2}\\)",
                  altHyp),
          br(),
          br(),
          sprintf("\\( \\alpha = %g \\)",
                  SigLvl()),
          #br(),
          br(),
          p(tags$b("Test Statistic:")),
          sprintf("Given:"),
          br(),
          sprintf("\\( x_{1} = %s \\)",
                  input$numSuccesses1),
          br(),
          sprintf("\\( n_{1} = %s \\)",
                  input$numTrials1),
          br(),
          sprintf("\\( x_{2} = %s \\)",
                  input$numSuccesses2),
          br(),
          sprintf("\\( n_{2} = %s \\)",
                  input$numTrials2),
          br(),
          br(),
          br(),
          sprintf("\\(z = \\dfrac{ (\\hat{p}_{1} - \\hat{p}_{2}) - (p_{1} - p_{2})_{0} }{\\sqrt{\\hat{p}(1-\\hat{p})\\left(\\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}}\\right)}}\\)"),
          br(),
          br(),
          p("where"),
          sprintf("\\( \\displaystyle \\qquad \\hat{p} = \\dfrac{x_{1} + x_{2}}{n_{1} + n_{2}} \\)"),
          sprintf("\\( = \\dfrac{%g + %g}{%g + %g} = %0.4f, \\)",
                  input$numSuccesses1,
                  input$numSuccesses2,
                  input$numTrials1,
                  input$numTrials2,
                  twoPropZTest["Pooled Proportion"]),
          br(),
          p("and"),
          sprintf("\\( \\displaystyle \\qquad \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                  input$numSuccesses1,
                  input$numTrials1,
                  twoPropZTest["Sample Proportion 1"]),
          br(),
          p("and"),
          sprintf("\\( \\displaystyle \\qquad \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                  input$numSuccesses2,
                  input$numTrials2,
                  twoPropZTest["Sample Proportion 2"]),
          br(),
          br(),
          br(),
          sprintf("\\( z = \\dfrac{ (%0.4f - %0.4f) - %s}{\\sqrt{%0.4f(1-%0.4f)\\left(\\dfrac{1}{%g} + \\dfrac{1}{%g}\\right)}}\\)",
                  twoPropZTest["Sample Proportion 1"],
                  twoPropZTest["Sample Proportion 2"],
                  if (diffNaught < 0) sprintf("(%.4f)", diffNaught) else sprintf("%.4f", diffNaught),
                  twoPropZTest["Pooled Proportion"],
                  twoPropZTest["Pooled Proportion"],
                  input$numTrials1,
                  input$numTrials2),
          sprintf("\\( = \\dfrac{%0.4f}{%0.4f} \\)",
                  twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"] - diffNaught,
                  twoPropZTest["Std Error"]),
          br(),
          br(),
          sprintf("\\(\\phantom{z} = %0.4f\\)",
                  twoPropZTest["Test Statistic"]),
          br(),
          br(),
          br()
        )
      )
      
      twoPropHTPVal <- printHTPVal(twoPropZTest["P-Value"],
                                   "z",
                                   IndMeansHypInfo()$alternative,
                                   twoPropZTest["Test Statistic"],
                                   pvalSymbol,
                                   reject)
      
      twoPropHTTail <- tagList(
        withMathJax(
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                  IndMeansHypInfo()$critSign,
                  IndMeansHypInfo()$critAlph,
                  IndMeansHypInfo()$critSign,
                  IndMeansHypInfo()$alphaVal,
                  critZVal),
          
          br(),
          br(),
          sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                  region,
                  reject),
          br(),
          br(),
          plotOutput(session$ns('twoPropHTPlot')),
          br()
        )
      )
      
      twoPropHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, "p_{2}")
      
      tagAppendChildren(twoPropHTHead, twoPropHTPVal, twoPropHTTail, twoPropHTConclusion)
    })
    
    #### ---------------- HT Plot ----
    output$twoPropHTPlot <- renderPlot({
      req(si_iv$is_valid())
      
      twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, input$propDiffNaught, IndMeansHypInfo()$alternative, SigLvl())
      htPlotCritVal <- round(twoPropZTest["Z Critical"], cvDigits)
      
      htPlot <- hypZTestPlot(twoPropZTest["Test Statistic"], htPlotCritVal, IndMeansHypInfo()$alternative)
      htPlot
    })
    
    #### --------------- Stacked Bar Plot and Pie Chart ----
    output$twoPropBarPlot <- renderPlot({
      req(input$numTrials1 >= input$numSuccesses1,
          input$numTrials2 >= input$numSuccesses2)
      
      df <- tibble(
        Group = c("Group 1", "Group 1", "Group 2", "Group 2"),
        Outcome = c("Successes", "Failures", "Successes", "Failures"),
        Count = c(input$numSuccesses1, input$numTrials1 - input$numSuccesses1,
                  input$numSuccesses2, input$numTrials2 - input$numSuccesses2)
      )
      
      ggplot(df, aes(x = Group, y = Count, fill = Outcome)) +
        geom_col(position = "fill", width = 0.5) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
          title = "Stacked Bar Chart: Proportion of Successes vs Failures",
          y = "Proportion", x = ""
        ) +
        scale_fill_manual(values = c("Successes" = "#4CAF50", "Failures" = "#F44336")) +
        theme(
          axis.text.x = element_text(size = 14, face = "bold", color = "black"),
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 18, face = "bold"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    })
    
    output$twoPropPieChart <- renderPlot({
      req(input$numTrials1 >= input$numSuccesses1,
          input$numTrials2 >= input$numSuccesses2)
      
      df <- tibble(
        Group = rep(c("Group 1", "Group 2"), each = 2),
        Outcome = c("Successes", "Failures", "Successes", "Failures"),
        Count = c(input$numSuccesses1, input$numTrials1 - input$numSuccesses1,
                  input$numSuccesses2, input$numTrials2 - input$numSuccesses2)
      )
      
      # Calculate percentage for labels
      df <- df %>%
        group_by(Group) %>%
        mutate(Percent = Count / sum(Count),
               Label = paste0(Outcome, " (", scales::percent(Percent), ")"))
      
      ggplot(df, aes(x = "", y = Count, fill = Outcome)) +
        geom_col(color = "white") +
        coord_polar(theta = "y") +
        facet_wrap(~Group) +    # Separate pie charts per group
        scale_fill_manual(values = c("Successes" = "#4CAF50", "Failures" = "#F44336")) +
        labs(title = "Success vs Failure Distribution by Group") +
        theme_void() +
        theme(
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          strip.text = element_text(size = 14, face = "bold")
        )
    })
    
    ### ------------ Two Pop Var Outputs ----------------------------------------------
    #### ----------- CI
    output$twoPopVarCI <- renderUI ({
      req(si_iv$is_valid())
      
      data <- GetAllTwoPopVarData()
      is_variance <- (input$dataAvailability3 == 'Variance')
      
      CI <- TwoPopVarCI(data$n1, data$sd1, data$n2, data$sd2, ConfLvl(), is_variance)
      df1 <- data$n1 - 1
      df2 <- data$n2 - 1
      conf_percent <- ConfLvl() * 100
      alpha <- 1 - ConfLvl()
      
      tagList(
        withMathJax(
          
          if(input$dataAvailability3 != "Enter Raw Data") {
            p("Given:")
          } else {
            p("From the Data:")
          },
          printTwoPopVarGivens(data, is_variance),
          
          p(sprintf("For a \\(%.0f\\%%\\) confidence interval:", conf_percent)),
          p(sprintf("\\(\\alpha = 1 - %.2f = %.2f\\)", ConfLvl(), alpha)),
          br(),
          
          # df
          printDegreesFreedom(df1, df2),
          
          # critical values 
          p(sprintf("\\(F_{\\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)", 
                    alpha / 2, df2, df1, CI$F_lower)),
          p(sprintf("\\(F_{1 - \\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)", 
                    1 - (alpha / 2), df2, df1, CI$F_upper)),
          br(),
          
          # F stat calculation
          printFStat(data$sd1, data$sd2, CI$F_statistic, is_variance),
          
          # formula for CI
          p("\\( \\displaystyle CI = \\left( F_{\\alpha/2,\\ df_2\\,,\\ df_1} \\cdot \\dfrac{s_1^2}{s_2^2},\\ F_{1 - \\alpha/2,\\ df_2\\,,\\ df_1} \\cdot \\dfrac{s_1^2}{s_2^2} \\right) \\)"),
          br(),
          
          # formula with subbed in values
          p(sprintf("\\( \\displaystyle CI = \\left( %.4f \\cdot %.4f,\\ %.4f \\cdot %.4f \\right) \\)",
                    CI$F_lower, CI$F_statistic, CI$F_upper, CI$F_statistic)),
          br(),
          
          # CI result
          p(sprintf("\\( \\displaystyle CI = (%.4f, %.4f) \\)", CI$CI_lower, CI$CI_upper)),
          br(),
          
          # interpretation
          HTML(sprintf("<strong>Interpretation:</strong>")),
          br(),
          br(),
          p(sprintf("We are \\(%.0f\\%%\\) confident that the ratio of the population variances is between \\(%.4f\\) and \\(%.4f\\).", 
                    conf_percent, CI$CI_lower, CI$CI_upper))
        ))
    })
    
    
    #### ------------ HT
    output$twoPopVarHT <- renderUI({
      req(si_iv$is_valid())
      
      data <- GetAllTwoPopVarData()
      hyp_labels <- TwoPopVarHypInfo()
      
      is_variance <- (input$dataAvailability3 == 'Variance')
      sig_lvl <- SigLvl()
      alt_hyp <- hyp_labels$alternative
      
      HT <- TwoPopVarHT(data$n1, data$sd1, data$n2, data$sd2, sig_lvl, alt_hyp, is_variance)
      df1 <- data$n1 - 1
      df2 <- data$n2 - 1
      
      text <- twoPopVarOutputText(HT, sig_lvl)
      
      tagList(
        withMathJax(
          # hypotheses
          p(sprintf("\\(H_0: %s\\)", hyp_labels$nullHyp)),
          p(sprintf("\\(H_a: %s\\)", hyp_labels$altHyp)),
          p(sprintf("\\( \\alpha = %.2f \\)", sig_lvl)),
          
          p(strong("Test Statistic:")), 
          
          if(input$dataAvailability3 != "Enter Raw Data") {
            p("Given:")
          } else {
            p("From the Data")
          },
          
          # print givens
          printTwoPopVarGivens(data, is_variance),
          
          # print F stat calc
          printFStat(data$sd1, data$sd2, HT$F_statistic, is_variance, is_HT = TRUE),
          br(),
          
          # print P value method
          printFTestPVal(
            pValue = HT$p_value,
            testStatVal = HT$F_statistic,
            alternative = alt_hyp,
            pValSign = text$pValSign,
            rejectWord = text$rejectWord
          ),
          
          # print crit value method
          p(tags$b("Using Critical Value Method:")),
          # two sided
          if(alt_hyp == "two.sided") {
            list(
              p(sprintf("Critical Values:")),
              sprintf("\\(F_{\\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)", 
                      sig_lvl/2, df2, df1, HT$crit_lower), br(),
              sprintf("\\(F_{1-\\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)", 
                      1-sig_lvl/2, df1, df2, HT$crit_upper)
            )
            
            # greater than
          } else if (alt_hyp == "greater") {
            sprintf("Critical Value = \\(F_{1-\\alpha,\\ df_1,\\ df_2} = F_{%.2f,\\ %d,\\ %d} = %.4f\\)", 
                    1-sig_lvl, df1, df2, HT$crit_val)
            
            # less than
          } else {
            sprintf("Critical Value = \\(F_{\\alpha,\\ df_2,\\ df_1} = F_{%.2f,\\ %d,\\ %d} = %.4f\\)", 
                    sig_lvl, df2, df1, HT$crit_val)
          },
          br(), br(),
          
          # print degrees freedom for crit val method
          p(sprintf("where")),
          div(style = "margin-left: 20px;",
              printDegreesFreedom(df1, df2)),
          
          sprintf("Since the test statistic \\((F)\\) falls within the %s region, %s \\( H_0 \\).",
                  text$region, text$rejectWord),
          br(), br(), br(),
          
          # conclusion
          p(strong("Conclusion:")),
          sprintf("At \\(\\alpha = %.2f\\), since the test statistic falls within the %s region, we %s \\(H_0\\)
                  and conclude that there %s enough statistical evidence to support that \\(%s\\).",
                  sig_lvl, text$region, text$rejectWord, text$isWord, hyp_labels$altHyp)
        )
      )
    })
    
    ### ------------ ANOVA Outputs -----------------------------------------------
    output$anovaOutput <- renderUI({
      req(si_iv$is_valid())
      PrintANOVA()
    })
    
    #### ---------------- Factor Table ----
    output$oneWayFactorTable <- renderDT({
      req(si_iv$is_valid())
      PrintANOVAFactorTable()
    })
    
    #### ---------------- ANOVA Table ----
    output$oneWayAnovaTable <- renderDT({
      req(si_iv$is_valid())
      PrintANOVATable()
    })
    
    #### ---------------- HT Plot ----
    output$oneWayAnovaPlot <- renderPlot({
      req(si_iv$is_valid())
      
      data <- anovaOneWayResults()$test
      anovaF <- round(data[1,"F value"], 4)
      
      if(input$anovaSigLvl == "10%") {
        sigLvl <- 0.1
      } else if(input$anovaSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
      cv <- round(qf(1 - sigLvl, df1 = data[1,"Df"], df2 = data[2,"Df"]), 4)
      
      xSeq <- c(seq(0, 15, length.out = 75), cv, anovaF)
      rrLabel <- c((cv + max(xSeq))/2)
      x_vector <- sort(c(xSeq, rrLabel))
      p_vector <- df(x_vector, df1 = data[1,"Df"], df2 = data[2,"Df"])
      
      anova_dataframe <- distinct(data.frame(x = x_vector, y = p_vector))
      cv_dataframe <- filter(anova_dataframe, x %in% cv)
      ts_dataframe <- filter(anova_dataframe, x %in% anovaF)
      rrLabelDF <- filter(anova_dataframe, x %in% rrLabel)
      arLabelDF <- filter(anova_dataframe, y %in% max(p_vector))
      
      ggplot(anova_dataframe,
             aes(x = x, y = y)) +
        stat_function(fun = df,
                      args = list(df1 = data[1,"Df"], df2 = data[2,"Df"]),
                      geom = "Density",
                      fill = NA) +
        shadeHtArea(anova_dataframe, cv, "greater") +
        geom_segment(data = filter(anova_dataframe, y %in% max(p_vector)),
                     aes(x = 0, xend = 0, y = 0, yend = y, alpha = 0.5),
                     linetype = "solid",
                     linewidth = 0.75,
                     color='black',
                     show.legend = FALSE) +
        geom_text(data = filter(anova_dataframe, x %in% c(0)),
                  aes(x = x, y = 0, label = "0"),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03,
                  check_overlap = TRUE) +
        geom_segment(data = cv_dataframe,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'butt',
                     linewidth = 1.5,
                     color='#023B70') +
        geom_text(data = cv_dataframe,
                  aes(x = x, y = 0, label = x),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03,
                  check_overlap = TRUE) +
        geom_segment(data = ts_dataframe,
                     aes(x = x, xend = x, y = 0, yend = y + .055),
                     linetype = "solid",
                     linewidth = 1.25,
                     color='#BD130B') +
        geom_text(data = ts_dataframe,
                  aes(x = x, y = y, label = x),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = .075,
                  check_overlap = TRUE) +
        geom_text(data = arLabelDF,
                  aes(x = x, y = 0, label = "A R"),
                  size = 16 / .pt,
                  fontface = "bold",
                  vjust = -4,
                  check_overlap = TRUE) +
        geom_text(data = rrLabelDF,
                  aes(x = x, y = y, label = "RR"),
                  size = 16 / .pt,
                  fontface = "bold",
                  vjust = -4,
                  check_overlap = TRUE) +
        theme_void() +
        ylab("") +
        xlab("F") +
        scale_y_continuous(breaks = NULL) +
        theme(axis.title.x = element_text(size = 20,
                                          family = "serif",
                                          face = "bold.italic"))
    })
    
    #### ---------------- Post hoc analysis ----
    output$anovaPosthocAnalysis <- renderUI({
      
      if(input$anovaSigLvl == "10%") {
        sigLvl <- 0.1
      } else if(input$anovaSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
      numComparisons <- anovaOneWayResults()$numFactors * (anovaOneWayResults()$numFactors - 1) / 2
      
      tagList(
        withMathJax(),
        titlePanel("Pairwise Comparisons"),
        hr(),
        br(),
        sprintf("The total number of independent pairwise comparisons is"),
        sprintf("\\( m = k(k-1)/2 \\), where \\(k\\) is the number of factors."),
        br(),
        sprintf("In this exercise,  \\(m = %s(%s-1)/2 = %s.\\)",
                anovaOneWayResults()$numFactors,
                anovaOneWayResults()$numFactors,
                numComparisons),
        br(),
        br(),
        p(tags$b("Bonferroni adjusted p-values using t tests with pooled SD")),
        DTOutput(session$ns("anovaBonfTable")),
        br(),
        sprintf("Note: The simple Bonferroni correction rejects only null hypotheses
              with a p-value less than"),
        sprintf("\\( \\alpha^{*} = \\alpha / m = %s / %s = %s \\)",
                sigLvl,
                numComparisons,
                round(sigLvl / numComparisons, 4)),
        br()
      )
    })
    
    #### ---------------- Bonf Table ----
    output$anovaBonfTable <- renderDT({
      data <- anovaOneWayResults()$data
      
      bonf_df <- as.data.frame(pairwise.t.test(data$values, data$ind, p.adjust.method = "bonf")$p.value)
      bonf_df <- mutate_if(bonf_df, is.numeric, round, digits=4)
      bonf_df[bonf_df == 0] <- "< 0.0001"
      
      headers = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th("",
               style = "border: 1px solid rgba(0, 0, 0, 0.15);
                        border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"),
            lapply(colnames(bonf_df), th,
                   style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);')
          )
        )
      ))
      
      datatable(bonf_df,
                class = 'cell-border stripe',
                container = headers,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = TRUE,
                  scrollX = TRUE
                ),
                selection = "none",
                escape = FALSE,
                filter = "none") %>% formatStyle(columns = c(0),
                                                 fontWeight = 'bold')
      
    })
    
    #### ----------------- Boxplot ----
    output$anovaBoxplot <- renderPlot({
      req(si_iv$is_valid())
      data <- anovaOneWayResults()$data
      
      df_boxplot <- data.frame(sample = c(data[,"ind"]),
                               data = c(data[,"values"]))
      colnames(df_boxplot) <- c("sample", "data")
      
      RenderSideBySideBoxplot(df_boxplot[,"data"],
                              df_boxplot,
                              input[["anovaBoxplot-Colour"]],
                              input[["anovaBoxplot-Title"]],
                              input[["anovaBoxplot-Xlab"]],
                              input[["anovaBoxplot-Ylab"]],
                              input[["anovaBoxplot-BoxWidth"]] / 10,
                              input[["anovaBoxplot-Gridlines"]],
                              input[["anovaBoxplot-Flip"]],
                              input[["anovaBoxplot-OutlierLabels"]])
      
    }, height = function() {GetPlotHeight(input[["anovaBoxplot-Height"]], input[["anovaBoxplot-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["anovaBoxplot-Width"]], input[["anovaBoxplot-WidthPx"]], ui = FALSE)}
    )
    
    #### ---------------- Histogram of Residuals ----
    output$anovaHistogram <- renderPlot({
      req(si_iv$is_valid())
      data <- anovaOneWayResults()$residuals
      
      RenderHistogram(data,
                      input[["anovaHistogram-Colour"]],
                      input[["anovaHistogram-Title"]],
                      input[["anovaHistogram-Xlab"]],
                      input[["anovaHistogram-Ylab"]],
                      input[["anovaHistogram-Gridlines"]],
                      input[["anovaHistogram-Flip"]])
      
    }, height = function() {GetPlotHeight(input[["anovaHistogram-Height"]], input[["anovaHistogram-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["anovaHistogram-Width"]], input[["anovaHistogram-WidthPx"]], ui = FALSE)}
    )
    
    #### ---------------- QQ Plot of Residuals ----
    output$anovaQQplot <- renderPlot({
      req(si_iv$is_valid())
      data <- anovaOneWayResults()$residuals
      
      qqplot_df <- data.frame(values = data)
      
      RenderQQPlot(qqplot_df,
                   input[["anovaQQplot-Colour"]],
                   input[["anovaQQplot-Title"]],
                   input[["anovaQQplot-Xlab"]],
                   input[["anovaQQplot-Ylab"]],
                   input[["anovaQQplot-Gridlines"]],
                   input[["anovaQQplot-Flip"]])
      
    }, height = function() {GetPlotHeight(input[["anovaQQplot-Height"]], input[["anovaQQplot-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["anovaQQplot-Width"]], input[["anovaQQplot-WidthPx"]], ui = FALSE)}
    )
    
    #### ---------------- Group Means Plot ----
    output$anovaMeanPlot<- renderPlot({
      req(si_iv$is_valid())
      data <- as.data.frame(anovaOneWayResults()$data)
      groups <- anovaOneWayResults()$factornames
      
      RenderMeanPlot(data,
                     groups,
                     input[["anovaMeanPlot-Colour"]],
                     input[["anovaMeanPlot-Title"]],
                     input[["anovaMeanPlot-Xlab"]],
                     input[["anovaMeanPlot-Ylab"]],
                     input[["anovaMeanPlot-Gridlines"]],
                     input[["anovaMeanPlot-Flip"]])
      
    }, height = function() {GetPlotHeight(input[["anovaMeanPlot-Height"]], input[["anovaMeanPlot-HeightPx"]], ui = FALSE)},
    width = function() {GetPlotWidth(input[["anovaMeanPlot-Width"]], input[["anovaMeanPlot-WidthPx"]], ui = FALSE)}
    )
    
    #### ---------------- Uploaded Data Table ----
    output$anovaUploadTable <- renderDT({
      req(anovaupload_iv$is_valid())
      datatable(anovaUploadData(),
                options = list(pageLength = -1,
                               lengthMenu = list(c(25, 50, 100, -1),
                                                 c("25", "50", "100", "all")),
                               columnDefs = list(list(className = 'dt-center',
                                                      targets = 0:ncol(anovaUploadData())))),
      )
    })
    
    ### ------------ Kruskal-Wallis Outputs ------------------------------------------
    output$kwHT <- kruskalWallisHT(kwResults, reactive({input$kwSigLvl}))
    output$kwUploadTable <- kruskalWallisUpload(kwUploadData, reactive({kwupload_iv$is_valid()}))
    output$kwInitialUploadTable <- kruskalWallisUploadInitial(kwUploadData)
    output$renderKWRM <- kwRankedTableOutput(kwResults()$data)
    output$kruskalWallisPlot <- kruskalWallisPlot(kwResults, reactive({input$kwSigLvl}))
    output$kwConclusionOutput <- kwConclusion(kwResults, reactive({input$kwSigLvl}))
    output$debug_kw_state <- renderText({
      paste(
        "multipleMethodChoice:", input$multipleMethodChoice, "|",
        "kwUserData exists:", !is.null(input$kwUserData), "|",
        "kwUserData name:", if(!is.null(input$kwUserData)) input$kwUserData$name else "NULL", "|",
        "siMethod:", input$siMethod
      )
    })
    output$kwRawContainer <- renderUI({
      req(input$multipleMethodChoice == 'kw')
      req(input$kwUserData)
      req(kwDisplayState() == "raw")  # Only show when in "raw" state
      
      tabsetPanel(
        id = session$ns("kwRaw"),
        selected = "Uploaded Data",
        tabPanel(
          title = "Uploaded Data",
          uiOutput(session$ns("renderKWRaw"))
        )
      )
    })
    ### ------------ Chi-Square Outputs ------------------------------------------
    
    output$chiSqObs <- renderDT({
      chiSqData <- chiSqTotaled()
      
      CreateChiSqObserved(chiSqData)
    })
    
    output$chiSqExp <- renderDT({
      CreateChiSqExpected(chiSqResults()$Results$expected)
    })
    
    output$chiSqResultsMatrix <- renderDT({
      req(si_iv$is_valid())
      
      chiSqTest <- suppressWarnings(ChiSquareTest(chiSqActiveMatrix(), input$chiSquareYates))
      
      # choose columns based on whether Yates' correction applied or not
      yates_applied <- input$chiSquareYates
      dimension <- input$chisquareDimension
      if (yates_applied && dimension == '2 x 2'){
        selected_cols <- c("O", "E", "(O - E)", "(|O - E| - 0.5)<sup>2</sup>", "(|O - E| - 0.5)<sup>2</sup> / E", "Standardized Residuals")
      } else {
        selected_cols <- c("O", "E", "(O - E)", "(O - E)<sup>2</sup>", "(O - E)<sup>2</sup> / E", "Standardized Residuals")
      }
      
      display_matrix <- chiSqTest$Matrix[, selected_cols, drop = FALSE]
      
      headers <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(lapply(selected_cols, function(colname) {
            th(HTML(colname),
               class = 'dt-center',
               style = 'border-right: 1px solid rgba(0, 0, 0, 0.15); border-top: 1px solid rgba(0, 0, 0, 0.15);')
          })
          )
        )
      ))
      
      datatable(display_matrix,
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
                    list(width = '130px', targets = c(0, 1, 2, 3, 5)),
                    list(width = '160px', targets = 4),
                    list(className = 'dt-center', targets = c(0, 1, 2, 3, 4, 5)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",
                rownames = FALSE) %>%
        formatStyle(columns = 0:ncol(display_matrix),
                    target = 'row',
                    fontWeight = styleRow(dim(display_matrix)[1], "bold"))
    })
    
    
    output$chiSqTest <- renderUI({
      PrintChiSqTest()
    })
    
    output$chiSqPlot <- renderPlot({ ###### chisq plot ----
      data <- chiSqResults()
      chisq_df <- data$Results$parameter
      chisq_ts <- round(data$Results$statistic, 4)
      ## chisq_ts <- data$Matrix[nrow(data$Matrix), "(O - E)<sup>2</sup> / E"]
      
      if(input$chisquareSigLvl == "10%") {
        sigLvl <- 0.1
      } else if(input$chisquareSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
      cv <- round(qchisq(1 - sigLvl, df = chisq_df), 4)
      # lower95 <- qchisq(.025, chisq_df)
      # upper95 <- qchisq(.975, chisq_df)
      
      xSeq <- c(seq(0, 15, length.out = 75), cv, chisq_ts)
      rrLabel <- c((cv + max(xSeq))/2)
      x_vector <- sort(c(xSeq, rrLabel))
      p_vector <- dchisq(x_vector, df = chisq_df)
      
      df <- distinct(data.frame(x = x_vector, y = p_vector))
      cvDF <- filter(df, x %in% cv)
      tsDF <- filter(df, x %in% chisq_ts)
      rrLabelDF <- filter(df, x %in% rrLabel)
      arLabelDF <- filter(df, y %in% max(p_vector))
      
      ggplot(df,
             aes(x = x, y = y)) +
        stat_function(fun = dchisq,
                      args = list(df = chisq_df),
                      geom = "Density",
                      fill = NA) +
        shadeHtArea(df, cv, "greater") +
        geom_segment(data = filter(df, y %in% max(p_vector)),
                     aes(x = 0, xend = 0, y = 0, yend = y, alpha = 0.5),
                     linetype = "solid",
                     linewidth = 0.75,
                     color='black',
                     show.legend = FALSE) +
        geom_text(data = filter(df, x %in% c(0)),
                  aes(x = x, y = 0, label = "0"),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03,
                  check_overlap = TRUE) +
        geom_segment(data = cvDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'butt',
                     linewidth = 1.5,
                     color='#023B70') +
        geom_text(data = cvDF,
                  aes(x = x, y = 0, label = x),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = -.03,
                  check_overlap = TRUE) +
        geom_segment(data = tsDF,
                     aes(x = x, xend = x, y = 0, yend = y + .055),
                     linetype = "solid",
                     linewidth = 1.25,
                     color='#BD130B') +
        geom_text(data = tsDF,
                  aes(x = x, y = y, label = x),
                  size = 14 / .pt,
                  fontface = "bold",
                  nudge_y = .075,
                  check_overlap = TRUE) +
        geom_text(data = arLabelDF,
                  aes(x = x, y = 0, label = "A R"),
                  size = 16 / .pt,
                  fontface = "bold",
                  vjust = -4,
                  check_overlap = TRUE) +
        geom_text(data = rrLabelDF,
                  aes(x = x, y = y, label = "RR"),
                  size = 16 / .pt,
                  fontface = "bold",
                  vjust = -4,
                  check_overlap = TRUE) +
        theme_void() +
        ylab("") +
        xlab(expression(bold(chi^2))) +
        scale_y_continuous(breaks = NULL) +
        theme(axis.title.x = element_text(size = 20))
      # coord_cartesian(clip="off")
      
    })
    
    output$fishersObs <- renderDT({
      req(si_iv$is_valid())
      
      CreateChiSqObserved(chiSqTotaled())
    })
    
    output$fishersTest <- renderUI({
      PrintFishersTest()
    })
    
    
    #  ========================================================================= #
    ## -------- Observers ------------------------------------------------------
    #  ========================================================================= #
    observeEvent(input$oneMeanUserData, priority = 5, {
      hide(id = "inferenceData")
      hide(id = "oneMeanVariable")
      fileInputs$oneMeanStatus <- 'uploaded'
      # if(onemeanupload_iv$is_valid())
      # {
      freezeReactiveValue(input, "oneMeanVariable")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "oneMeanVariable",
                        choices = c(colnames(OneMeanUploadData()))
      )
      
      shinyjs::show(id = "oneMeanVariable")
      # }
    })
    
    
    observeEvent(input$oneMeanBoxplot, {
      if (input$oneMeanBoxplot && input$dataAvailability != 'Summarized Data') {
        showTab(inputId = "onePopMeanTabset", target = "Graphs")
      } else {
        if (input$onePopMeanTabset == "Graphs") {
          updateTabsetPanel(inputId = 'onePopMeanTabset', selected = "Analysis")
        }
        hideTab(inputId = "onePopMeanTabset", target = "Graphs")
      }
    })
    
    observeEvent(input$goInference, {
      output$renderOnePopMeanData <- renderUI({
        tagList(
          titlePanel("Data File"),
          br(),
          br(),
          div(DTOutput(session$ns("onePopMeanUploadTable")), style = "width: 75%"),
          br(),
          br()
        )
      })
    })
    
    observeEvent(input$indMeansUserData, priority = 5, {
      hide(id = "inferenceData")
      hide(id = "indMeansUplSample1")
      hide(id = "indMeansUplSample2")
      fileInputs$indMeansStatus <- 'uploaded'
      # if(onemeanupload_iv$is_valid())
      # {
      freezeReactiveValue(input, "indMeansUplSample1")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "indMeansUplSample1",
                        choices = c(colnames(IndMeansUploadData()))
      )
      
      freezeReactiveValue(input, "indMeansUplSample2")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "indMeansUplSample2",
                        choices = c(colnames(IndMeansUploadData()))
      )
      shinyjs::show(id = "indMeansUplSample1")
      shinyjs::show(id = "indMeansUplSample2")
      # }
    })
    
    observeEvent(input$goInference, {
      output$renderIndPopMeansData <- renderUI({
        tagList(
          titlePanel("Data File"),
          br(),
          br(),
          div(DTOutput(session$ns("indPopMeansUploadTable")), style = "width: 75%"),
          br(),
          br()
        )
      })
    })
    
    observeEvent(c(input$indMeansBoxplot, input$indMeansQQPlot), {
      if (input$indMeansBoxplot || input$indMeansQQPlot) {
        showTab(inputId = "indPopMeansTabset", target = "Graphs")
      } else {
        if (input$indPopMeansTabset == "Graphs") {
          updateTabsetPanel(inputId = 'indPopMeansTabset', selected = "Analysis")
        }
        hideTab(inputId = "indPopMeansTabset", target = "Graphs")
      }
    })
    
    observeEvent(input$depMeansQQPlot, {
      if (input$depMeansQQPlot) {
        showTab(inputId = "depPopMeansTabset", target = "Graphs")
      } else {
        if (input$depPopMeansTabset == "Graphs") {
          updateTabsetPanel(inputId = 'depPopMeansTabset', selected = "Analysis")
        }
        hideTab(inputId = "depPopMeansTabset", target = "Graphs")
      }
    })
    
    observeEvent(input$depMeansUserData, priority = 5, {
      hide(id = "inferenceData")
      hide(id = "depMeansUplSample1")
      hide(id = "depMeansUplSample2")
      fileInputs$depMeansStatus <- 'uploaded'
      
      if(depmeansupload_iv$is_valid()) {
        freezeReactiveValue(input, "depMeansUplSample1")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "depMeansUplSample1",
                          choices = c(colnames(DepMeansUploadData()))
        )
        
        freezeReactiveValue(input, "depMeansUplSample2")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "depMeansUplSample2",
                          choices = c(colnames(DepMeansUploadData()))
        )
        shinyjs::show(id = "depMeansUplSample1")
        shinyjs::show(id = "depMeansUplSample2")
      }
    })
    
    observeEvent(input$goInference, {
      output$renderDepPopMeansData <- renderUI({
        tagList(
          titlePanel("Data File"),
          br(),
          br(),
          div(DTOutput(session$ns("depPopMeansUploadTable")), style = "width: 75%"),
          br(),
          br()
        )
      })
    })
    
    ### ---------- Wilcoxon Rank Sum Observers --------------------
    observeEvent(input$wilcoxonUpl, priority = 5, {
      hide(id = "inferenceData")
      hide(id = "wilcoxonUpl1")
      hide(id = "wilcoxonUpl2")
      fileInputs$rankSumStatus <- 'uploaded'
      
      if(wilcoxonUpload_iv$is_valid()) {
        
        
        freezeReactiveValue(input, "wilcoxonUpl1")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "wilcoxonUpl1",
                          choices = c(colnames(WilcoxonUploadData()))
        )
        
        freezeReactiveValue(input, "wilcoxonUpl2")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "wilcoxonUpl2",
                          choices = c(colnames(WilcoxonUploadData()))
        )
        shinyjs::show(id = "wilcoxonUpl1")
        shinyjs::show(id = "wilcoxonUpl2")
      }
    })
    
    observeEvent(c(input$sidebysidewRankSum, input$sidebysidewRankQQ), {
      if (input$sidebysidewRankSum || input$sidebysidewRankQQ) {
        showTab(inputId = "wilcoxonRankSumTabset", target = "Graphs")
      } else {
        if (input$wilcoxonRankSumTabset == "Graphs") {
          updateTabsetPanel(inputId = 'wilcoxonRankSumTabset', selected = "Analysis")
        }
        hideTab(inputId = "wilcoxonRankSumTabset", target = "Graphs")
      }
    })
    
    observeEvent(input$goInference, {
      output$renderWRankSumMeansData <- renderUI({
        tagList(
          titlePanel("Data File"),
          br(),
          br(),
          div(DTOutput(session$ns("wRankSumUploadTable")), style = "width: 75%"),
          br(),
          br()
        )
      })
    })
    
    observeEvent(input$anovaUserData, priority = 10, {
      
      hide(id = "inferenceData")
      hide(id = "anovaUploadInputs")
      
      fileInputs$anovaStatus <- 'uploaded'
      
      if(anovaupload_iv$is_valid())
      {
        freezeReactiveValue(input, "anovaMultiColumns")
        updateSelectizeInput(session = getDefaultReactiveDomain(),
                             "anovaMultiColumns",
                             choices = c(colnames(anovaUploadData()))
        )
        
        freezeReactiveValue(input, "anovaResponse")
        updateSelectizeInput(session = getDefaultReactiveDomain(),
                             "anovaResponse",
                             choices = c(colnames(anovaUploadData()))
        )
        
        freezeReactiveValue(input, "anovaFactors")
        updateSelectizeInput(session = getDefaultReactiveDomain(),
                             "anovaFactors",
                             choices = c(colnames(anovaUploadData()))
        )
        
        shinyjs::show(id = "anovaUploadInputs")
      }
    })
    
    observeEvent(input$kwUserData, {
      
      output$analysisContent <- renderUI({ NULL })
      output$renderKWRM <- renderUI({ NULL })
      output$renderKWData <- renderUI({ NULL })
      
      
      output$renderKWRaw <- renderUI({
        tagList(
          titlePanel("Data File"),
          br(), br(),
          div(DTOutput(session$ns("kwInitialUploadTable")), style = "width: 75%"),
          br(), br(),
        )
      })
      
      kwDisplayState("raw")
      
      #hide(id = "inferenceData")
      hide(id = "kwUploadInputs")
      
      fileInputs$kwStatus <- 'uploaded'
      output$kwInitialUploadTable <- kruskalWallisUploadInitial(kwUploadData)
      
      if(kwupload_iv$is_valid())
      {
        updateRadioButtons(session, "kwFormat", selected = "Multiple")
        
        freezeReactiveValue(input, "kwMultiColumns")
        updateSelectizeInput(session = getDefaultReactiveDomain(),
                             "kwMultiColumns",
                             choices = c(colnames(kwUploadData())),
                             selected = character(0)
        )
        
        freezeReactiveValue(input, "kwResponse")
        updateSelectizeInput(session = getDefaultReactiveDomain(),
                             "kwResponse",
                             choices = c(colnames(kwUploadData())),
                             selected = character(0)
        )
        freezeReactiveValue(input, "kwFactors")
        updateSelectizeInput(session = getDefaultReactiveDomain(),
                             "kwFactors",
                             choices = c(colnames(kwUploadData())),
                             selected = character(0)
        )
        
        shinyjs::show(id = "kwUploadInputs")
      }
    })
    
    observeEvent(input$chisquareDimension, {
      if( input$chisquareDimension != '2 x 2') {
        shinyjs::disable(selector = '#chisquareMethod input[value="Fisher"]')
        
        updatePrettyRadioButtons(
          inputId = "chisquareMethod",
          selected = "Chi-Square"
        )
      } else {
        shinyjs::enable(selector = '#chisquareMethod input[value="Fisher"]')
      }
      
      shinyjs::reset(id = "chiSquareRowHeader")
      shinyjs::reset(id = "chiSquareColHeader")
    })
    
    observeEvent(input$goInference, {
      output$renderAnovaDataView <- renderUI({
        tagList(
          titlePanel("Data File"),
          br(),
          br(),
          div(DTOutput(session$ns("anovaUploadTable")), style = "width: 75%"),
          br(),
          br()
        )
      })
      
      output$renderAnovaBoxplot <- renderUI({
        tagList(
          plotOutput(session$ns("anovaBoxplot"),
                     height = GetPlotHeight(input[["anovaBoxplot-Height"]], input[["anovaBoxplot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaBoxplot-Width"]], input[["anovaBoxplot-WidthPx"]], ui = TRUE)),
          br(),
          br(),
          hr()
        )
      })
      
      output$renderAnovaHistogram <- renderUI({
        tagList(
          plotOutput(session$ns("anovaHistogram"),
                     height = GetPlotHeight(input[["anovaBoxplot-Height"]], input[["anovaBoxplot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaBoxplot-Width"]], input[["anovaBoxplot-WidthPx"]], ui = TRUE)),
          br(),
          br(),
          hr()
        )
      })
      
      output$renderAnovaQQplot <- renderUI({
        tagList(
          plotOutput(session$ns("anovaQQplot"),
                     height = GetPlotHeight(input[["anovaQQplot-Height"]], input[["anovaQQplot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaQQplot-Width"]], input[["anovaQQplot-WidthPx"]], ui = TRUE)),
          br(),
          br(),
          hr()
        )
      })
      
      output$renderAnovaMeanPlot <- renderUI({
        tagList(
          plotOutput(session$ns("anovaMeanPlot"),
                     height = GetPlotHeight(input[["anovaMeanPlot-Height"]], input[["anovaMeanPlot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaMeanPlot-Width"]], input[["anovaMeanPlot-WidthPx"]], ui = TRUE)),
          br(),
          br(),
          hr()
        )
      })
      
      observeEvent(input$goInference, {
        kwDisplayState("analysis")
        
        req(kwUploadData())
        req(kwupload_iv$is_valid())
        
        output$renderKWData <- renderUI({
          tagList(
            titlePanel("Data File"),
            br(),
            br(),
            div(DTOutput(session$ns("kwUploadTable")), style = "width: 75%"),
            br(),
            br()
          )
        })
      })
      
      observe({
        req(kwResults())
        
        results <- NULL
        tryCatch({
          results <- kwResults()
        }, error = function(e) {
          cat("Error in kwResults():", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(results)) {
          output$analysisContent <- renderUI({
            tagList(
              p("Unable to calculate results. Please check your data and selections.", 
                style = "color: red; font-weight: bold; font-size: 16px;")
            )
          })
          return()
        }
        
        if (!is.null(results$validation_error)) {
          output$analysisContent <- renderUI({
            tagList(
              p(results$validation_error, style = "color: red; font-weight: bold; font-size: 16px;")
            )
          })
        } else {
          output$analysisContent <- renderUI({
            tagList(
              kruskalWallisHT(kwResults, reactive(input$kwSigLvl))(),
              br(),
              kruskalWallisPlot(kwResults, reactive(input$kwSigLvl))(),
              br(),
              kwConclusion(kwResults, reactive(input$kwSigLvl))()
            )
          })
        }
      })
      
      observe({
        req(kwResults())
        
        results <- NULL
        tryCatch({
          results <- kwResults()
        }, error = function(e) {
          cat("Error in kwResults() for ranking:", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(results)) {
          output$renderKWRM <- renderUI({ 
            tagList(
              p("Unable to generate ranking table.", 
                style = "color: #666; font-style: italic; text-align: center; padding: 20px;")
            )
          })
          return()
        }
        
        if (!is.null(results$data)) {
          output$renderKWRM <- kwRankedTableOutput(results$data)
        } else {
          output$renderKWRM <- renderUI({ 
            tagList(
              p("No data available for ranking.", 
                style = "color: #666; font-style: italic; text-align: center; padding: 20px;")
            )
          })
        }
      })
      
      output$renderChiSqObs <- renderUI({
        DTOutput(session$ns("chiSqObs"), width = '500px')
      })
      
      output$renderChiSqExp <- renderUI({
        DTOutput(session$ns("chiSqExp"), width = '500px')
      })
      
      output$renderChiSqResults <- renderUI({
        if (input$chiSquareYates){
          DTOutput(session$ns("chiSqResultsMatrix"), width = "850px")
        } else
          DTOutput(session$ns("chiSqResultsMatrix"), width = "750px")
      })
      
      output$renderFishersObs <- renderUI({
        DTOutput(session$ns("fishersObs"), width = '500px')
      })
    })
    
    observeEvent(input$goInference, {
      #output$renderInference <- renderDataTable(
      
      if(si_iv$is_valid() && depmeansrawsd_iv$is_valid()) {
        shinyjs::show(id = "inferenceData")
        
      } else {
        hide(id = "inferenceData")
      }
      
      if(input$siMethod == '1'){
        
        if(input$popuParameter == 'Population Mean') {
          req(si_iv$is_valid())
          output$renderOneMeanBoxplot <- renderUI({
            plotOutput(session$ns("oneMeanBoxplot"),
                       height = GetPlotHeight(input[["oneMeanBoxplot-Height"]], input[["oneMeanBoxplot-HeightPx"]], ui = TRUE),
                       width = GetPlotWidth(input[["oneMeanBoxplot-Width"]], input[["oneMeanBoxplot-WidthPx"]], ui = TRUE))
          })
        } else if(input$popuParameter == 'Population Proportion') {
          req(input$numTrials && input$numSuccesses)
          if(input$numTrials < input$numSuccesses) {
            hide(id = "inferenceData")
          }
        } # input$popuParameter == 'Population Proportion'
      } # one sample
      
      else if(input$siMethod == '2') {
        
        if(input$popuParameters == 'Population Proportions') {
          req(!is.na(input$numSuccesses1) && !is.na(input$numTrials1))
          req(!is.na(input$numSuccesses2) && !is.na(input$numTrials2))
          
          if(input$numSuccesses1 > input$numTrials1 || input$numSuccesses2 > input$numTrials2) {
            print("amde it")
            hide(id = 'inferenceData')
          }
          
        } else if (input$popuParameters == "Independent Population Means") {
          
          output$renderIndMeansBoxplot <- renderUI({
            plotOutput(session$ns("indMeansBoxplot"),
                       height = GetPlotHeight(input[["indMeansBoxplot-Height"]], input[["indMeansBoxplot-HeightPx"]], ui = TRUE),
                       width = GetPlotWidth(input[["indMeansBoxplot-Width"]], input[["indMeansBoxplot-WidthPx"]], ui = TRUE))
          })
          output$renderIndMeansQQPlot <- renderUI({
            plotOutput(session$ns("indMeansQQPlot"),
                       height = GetPlotHeight(input[["indMeansQQPlot-Height"]], input[["indMeansQQPlot-HeightPx"]], ui = TRUE),
                       width = GetPlotWidth(input[["indMeansQQPlot-Width"]], input[["indMeansQQPlot-WidthPx"]], ui = TRUE))
          })
        } else if(input$popuParameters == "Wilcoxon rank sum test") {
          output$renderSidebysidewRankSum <- renderUI({
            plotOutput(session$ns("sidebysidewRankSum"),
                       height = GetPlotHeight(input[["sidebysidewRankSum-Height"]], input[["sidebysidewRankSum-HeightPx"]], ui = TRUE),
                       width = GetPlotWidth(input[["sidebysidewRankSum-Width"]], input[["sidebysidewRankSum-WidthPx"]], ui = TRUE))
          })
          
        } else if(input$popuParameters == 'Dependent Population Means') {
          
          output$depMeansTable <- renderUI({
            DTOutput(session$ns("depMeansData"))
          })
          
          output$renderDepMeansQQPlot <- renderUI({
            plotOutput(session$ns("depMeansQQPlot"),
                       height = GetPlotHeight(input[["depMeansQQPlot-Height"]], input[["depMeansQQPlot-HeightPx"]], ui = TRUE),
                       width = GetPlotWidth(input[["depMeansQQPlot-Width"]], input[["depMeansQQPlot-WidthPx"]], ui = TRUE))
          })
        }
      } else if(input$siMethod == 'Categorical') {
        
        # output$render2x2ChiSq <- renderUI({
        #   tagList(
        #
        #     titlePanel("Chi-Square Test for Independence"),
        #     hr(),
        #     br(),
        #     h3("Observed Frequencies"),
        #     DTOutput("chiSq2x2Obs", width = '500px'),
        #     br(),
        #     br(),
        #     h3("Expected Frequencies"),
        #     DTOutput("chiSq2x2", width = '500px'),
        #     br(),
        #   )
        # })
        #
        # output$render2x3ChiSq <- renderUI({
        #   tagList(
        #
        #     titlePanel("Chi-Square Test for Independence"),
        #     hr(),
        #     br(),
        #     DTOutput("chiSq2x3", width = '500px'),
        #     br(),
        #     br(),
        #     br(),
        #   )
        # })
        
        # output$render3x2ChiSq <- renderUI({
        #   tagList(
        #
        #     titlePanel("Chi-Square Test for Independence"),
        #     hr(),
        #     br(),
        #     DTOutput("chiSq3x2", width = '500px'),
        #     br(),
        #     br(),
        #     br(),
        #   )
        # })
        #
        # output$render3x3ChiSq <- renderUI({
        #   tagList(
        #
        #     titlePanel("Chi-Square Test for Independence"),
        #     hr(),
        #     br(),
        #     DTOutput("chiSq3x3", width = '500px'),
        #     br(),
        #     br(),
        #     br(),
        #   )
        # })
      }
      #) # renderInference
    }) # input$goInference
    
    ### ------------ Component Display -------------------------------------------
    observeEvent(!si_iv$is_valid(), {
      hide(id = "inferenceMP")
      hide(id = "inferenceData")
    })
    
    observeEvent(!depmeansrawsd_iv$is_valid(), {
      hide(id = "inferenceMP")
      hide(id = "inferenceData")
    })
    observeEvent(!wRankSumrawsd_iv$is_valid(), {
      hide(id = "inferenceMP")
      hide(id = "inferenceData")
    })
    observeEvent({
      input$siMethod
      input$sampleSize
      input$sampleMean
      input$popuParameter
      input$popuParameters
      input$dataAvailability
      input$dataAvailability2
      input$sigmaKnown
      input$sigmaKnownRaw
      input$popuSD
      input$popuSDRaw
      input$sampSD
      input$inferenceType
      input$inferenceType2
      input$normaprowrs
      input$input$continuityCorrectionOption
    }, {
      hide(id = "inferenceData")
    })
    
    observeEvent(fileInputs$oneMeanStatus, {
      if (fileInputs$oneMeanStatus == 'uploaded')
        show(id = "oneMeanVariable")
      else
        hide(id = "oneMeanVariable")
    })
    
    observeEvent(fileInputs$indMeansStatus, {
      if (fileInputs$indMeansStatus == 'uploaded'){
        show(id = "indMeansUplSample1")
        show(id = "indMeansUplSample2")
      } else {
        hide(id = "indMeansUplSample1")
        hide(id = "indMeansUplSample2")
      }
    })
    
    observeEvent(fileInputs$rankSumStatus, {  
      if (fileInputs$rankSumStatus == 'uploaded'){
        show(id = "wilcoxonUpl1")
        show(id = "wilcoxonUpl2")
      } else {
        hide(id = "wilcoxonUpl1")
        hide(id = "wilcoxonUpl2")
      }
    })
    
    observeEvent(fileInputs$depMeansStatus, {
      if (fileInputs$depMeansStatus == 'uploaded'){
        show(id = "depMeansUplSample1")
        show(id = "depMeansUplSample2")
      } else {
        hide(id = "depMeansUplSample1")
        hide(id = "depMeansUplSample2")
      }
    })
    
    observeEvent(!anovaupload_iv$is_valid(), {
      hide(id = "inferenceMP")
      hide(id = "inferenceData")
    })
    
    observeEvent(input$goInference, {
      shinyjs::show(id = "inferenceMP")
    })
    
    observeEvent(input$goInference, {
      # Hide/show tabs for 1 sample
      if (input$dataAvailability != "Upload Data"){
        updateTabsetPanel(session, "onePopMeanTabset", selected = "Analysis")
        hideTab(inputId = "onePopMeanTabset", target = "Uploaded Data")
      } else {
        showTab(inputId = "onePopMeanTabset", target = "Uploaded Data")
      }
      
      if(input$oneMeanBoxplot && input$dataAvailability != "Summarized Data") {
        showTab(inputId = "onePopMeanTabset", target = "Graphs")
      } else {
        hideTab(inputId = "onePopMeanTabset", target = "Graphs")
      }
      
      # Hide/show tabs for 2 sample independent populations
      if (input$dataAvailability2 != "Upload Data"){
        updateTabsetPanel(session, "indPopMeansTabset", selected = "Analysis")
        hideTab(inputId = "indPopMeansTabset", target = "Uploaded Data")
      } else {
        showTab(inputId = "indPopMeansTabset", target = "Uploaded Data")
      }
      
      if(input$dataAvailability2 != "Summarized Data" && (input$indMeansBoxplot || input$indMeansQQPlot)) {
        showTab(inputId = "indPopMeansTabset", target = "Graphs")
      } else {
        hideTab(inputId = "indPopMeansTabset", target = "Graphs")
      }
      
      # Hide/show tabs for 2 sample dependent populations
      if (input$dataTypeDependent != "Upload Data"){
        updateTabsetPanel(session, "depPopMeansTabset", selected = "Analysis")
        hideTab(inputId = "depPopMeansTabset", target = "Uploaded Data")
      } else {
        showTab(inputId = "depPopMeansTabset", target = "Uploaded Data")
      }
      
      if(input$depMeansQQPlot) {
        showTab(inputId = "depPopMeansTabset", target = "Graphs")
      } else {
        hideTab(inputId = "depPopMeansTabset", target = "Graphs")
      }
      # Hide/show tabs for Wilcoxon Rank Sum Upload
      if (input$wilcoxonRankSumTestData != "Upload Data"){
        updateTabsetPanel(session, "wilcoxonRankSumTabset", selected = "Analysis")
        hideTab(inputId = "wilcoxonRankSumTabset", target = "Uploaded Data")
      } else {
        showTab(inputId = "wilcoxonRankSumTabset", target = "Uploaded Data")
      }
      # Hide/show tabs for Wilcoxon Rank Sum Graph
      if (input$sidebysidewRankSum == 1 || input$sidebysidewRankQQ == 1){
        showTab(inputId = "wilcoxonRankSumTabset", target = "Graphs")
      } else {
        hideTab(inputId = "wilcoxonRankSumTabset", target = "Graphs")
      }
    })
    
    observeEvent(input$resetInference, {
      hide(id = "inferenceMP")
      hide(id = "anovaUploadInputs")
      hide(id = "kwUploadInputs")
      shinyjs::reset("inputPanel")
      fileInputs$oneMeanStatus <- 'reset'
      fileInputs$indMeansStatus <- 'reset'
      fileInputs$rankSumStatus <-'reset'
      fileInputs$depMeansStatus <- 'reset'
      fileInputs$anovaStatus <- 'reset'
      fileInputs$kwStatus <- 'reset'
      
      updateSelectizeInput(session, "kwMultiColumns", 
                           selected = "",
                           choices = c(""),
                           options = list(placeholder = 'Select two or more columns'))
      
      updateSelectizeInput(session, "kwResponse",
                           selected = "",
                           choices = c(""),
                           options = list(placeholder = 'Select a variable'))
      
      updateSelectizeInput(session, "kwFactors",
                           selected = "",
                           choices = c(""),
                           options = list(placeholder = 'Select a factor'))
      
      updateTabsetPanel(session, "onePopMeanTabset", selected = "Analysis")
    })
    
    observe({
      
      if (input$chisquareMethod == "Fisher"){
        updateRadioButtons(session, "chisquareDimension", selected = "2 x 2")
        
        # hide radio buttons
        runjs("$('input[value=\"2 x 3\"]').parent().hide();
               $('input[value=\"3 x 2\"]').parent().hide();
               $('input[value=\"3 x 3\"]').parent().hide();
              ")
        
        # hide matrices
        hide(id = "chiSqInput2x3")
        hide(id = "chiSqInput3x2")
        hide(id = "chiSqInput3x3")
      }
      else {
        
        # show radio buttons
        runjs("$('input[value=\"2 x 3\"]').parent().show();
               $('input[value=\"3 x 2\"]').parent().show();
               $('input[value=\"3 x 3\"]').parent().show();
              ")
        
        # show matrices
        show(id = "chiSqInput2x3")
        show(id = "chiSqInput3x2")
        show(id = "chiSqInput3x3")
      }
      
    })
    
  })
}