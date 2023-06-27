library(bslib)
library(car)
library(dplyr)
library(DT)
library(ggplot2)
library(moments)
library(nortest)
library(readr)
library(readxl)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyvalidate)
library(tinytex)
library(tools)
library(writexl)
library(MASS)

options(scipen = 999) # options(scipen = 0)

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"

# ----------------------- #  
# ---- UI components ---- 
# ----------------------- #
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
  
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
        font-weight: bold;
      }
    "))
  ),
                
  navbarPage(title = div(img(src ="CougarStats.png", height = 100), span("CougarStats", style = "color:#000000; font-weight:bold; font-style: italic; font-size:24pt")),
             
                # --------------------- #  
                # ---- Methods Tab ---- 
                # --------------------- #
                tabPanel(title = "Methods",
                         
                  #  ------------------------- #  
                  ## ---- Methods sidebar ---- 
                  #  ------------------------- #       
                  sidebarLayout(
                    sidebarPanel(
                      withMathJax(),
                      shinyjs::useShinyjs(),
                      id = "sideBar", 

                      selectizeInput(
                        inputId = "dropDownMenu",
                        label = strong("Choose Statistical Topic"),
                        choices = c("Descriptive Statistics", "Probability Distributions", "Statistical Inference", "Regression and Correlation"),
                        multiple = TRUE, 
                        options = list(maxItems = 1)
                        #selected = NULL, #"Descriptive Statistics", # "Statistical Inference", #"Probability Distributions", #"Regression and Correlation", # NULL
                      ),
                      
                      
                      #   ----------------------------------- #  
                      ### ---- Descriptive Stats sidebar ---- 
                      #   ----------------------------------- #
                      conditionalPanel(id = "descriptiveStatsPanel",
                        condition = "input.dropDownMenu == 'Descriptive Statistics'",
                        
                        radioButtons(inputId = "dataInput",
                                     label = strong("Data"),
                                     choiceValues = list("Enter Raw Data", "Upload Data"),
                                     choiceNames = list("Enter Raw Data", "Upload Data"),
                                     selected = "Enter Raw Data", #character(0), #
                                     inline = TRUE), #,width = '1000px'),
                        
                        conditionalPanel(
                          condition = "input.dataInput == 'Enter Raw Data'",
                        
                            textAreaInput("descriptiveStat", label = strong("Sample"), value = "2.14,   2.09,   2.65,   3.56,   5.55,   5.00,   5.55,   8.09,   10.79", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        ),
                        
                        conditionalPanel(
                          condition = "input.dataInput == 'Upload Data'",
                          
                            fileInput('DS_data', 'Upload data',
                                      accept = c('text/csv','text/comma-separated-values','text/tab-separated-values',
                                                 'text/plain','.csv','.txt','.xls','.xlsx'))
                        ),
                        
                        #checkboxInput("boxPlot", strong("Add a Boxplot")),
                        
                        # conditionalPanel(
                        #   condition = "input.boxPlot == 1",
                        #   
                        #   textInput("main", label = strong("Main title and axes labels:"), value = "Box Plot", placeholder = "main title"),
                        #   textInput("xlab", label = NULL, value = "x", placeholder = "x-axis label"),
                        #   textInput("ylab", label = NULL, value = "y", placeholder = "y-axis label"),
                        #   #hr(),
                        # ),
                        
                        actionButton(inputId = "goDescpStats", label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetAll", label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ),
                      
                      #   ------------------------------------------- #  
                      ### ---- Probability Distributions sidebar ---- 
                      #   ------------------------------------------- #
                      conditionalPanel(id = "probPanel",
                        condition = "input.dropDownMenu == 'Probability Distributions'",
                        
                        radioButtons("probability", label = strong("Distribution"), choices = c("Binomial", "Poisson", "Normal"), selected = NULL, inline = TRUE),
                        
                        conditionalPanel(id = "binomialPanel",
                          condition = "input.probability == 'Binomial'",
                          
                          numericInput(inputId = "numTrialsBinom",
                                       label = strong("Number of Trials (\\( n\\))"),
                                       value = 7, min = 1, step = 1),
                          
                          numericInput(inputId = "successProbBinom",
                                       label = strong("Probability of Success (\\( p\\))"),
                                       value = 0.15, min = 0, max = 1, step = 0.00001),
                          
                          radioButtons(inputId = "calcBinom",
                                       label = strong("Probability"),
                                       choiceValues = list("exact", "cumulative", "upperTail", "greaterThan", "lessThan", "between"),
                                       choiceNames = list("\\(P(X = x \\))","\\(P(X \\leq x)\\)","\\(P(X \\ge x)\\)", "\\(P(X \\gt x)\\)", "\\(P(X < x)\\)", "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.calcBinom != 'between'",
                            
                            numericInput(inputId = "numSuccessesBinom",
                                         label = strong("Number of Successes (\\( x\\))"),
                                         value = 2, min = 0, step = 1)
                          ),
                          
                          conditionalPanel(
                            condition = "input.calcBinom == 'between'",
                            
                            numericInput(inputId = "numSuccessesBinomx1",
                                         label = strong("Number of successes (\\( x_{1}\\))"),
                                         value = 2, min = 0, step = 1),
                            
                            numericInput(inputId = "numSuccessesBinomx2",
                                         label = strong("Number of successes (\\( x_{2}\\))"),
                                         value = 4, min = 0, step = 1)
                          ),
                          
                          # checkboxInput(inputId = "probDistTable",
                          #               label = strong("Probability Distribution Table"),
                          #               value = FALSE,
                          #               width = NULL),
                          
                          # checkboxInput(inputId = "mean_and_SD_binom",
                          #               label = strong("Mean (\\( \\mu\\)) and Standard Deviation (\\( \\sigma\\))"),
                          #               value = FALSE,
                          #               width = NULL),
                          
                          actionButton(inputId = "goBinom", label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetBinomial", label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), # , onclick = "history.go(0)"

                          # br(),
                          # downloadButton('downloadBinomResults', 'Download Results'),
                        ),
                        
                        conditionalPanel(id = "poissonPanel", 
                          condition = "input.probability == 'Poisson'",
                          
                          numericInput("muPoisson", label = strong("Average (\\( \\mu\\))"),
                                       value = 4.5),
                          
                          radioButtons(inputId = "calcPoisson",
                                       label = strong("Probability"),
                                       choiceValues = list("exact", "cumulative", "upperTail", "greaterThan", "lessThan", "between"),
                                       choiceNames = list("\\(P(X = x \\))","\\(P(X \\leq x)\\)","\\(P(X \\ge x)\\)", "\\(P(X \\gt x)\\)", "\\(P(X < x)\\)", "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.calcPoisson != 'between'",
                            
                            numericInput("xPoisson", label = strong("Number of Successes (\\( x\\))"),
                                         value = 4, min = 0, step = 1),
                          ),
                          
                          conditionalPanel(
                            condition = "input.calcPoisson == 'between'",
                            
                            numericInput(inputId = "x1Poisson",
                                         label = strong("Number of successes (\\( x_{1}\\))"),
                                         value = 4, min = 0, step = 1),
                            
                            numericInput(inputId = "x2Poisson",
                                         label = strong("Number of successes (\\( x_{2}\\))"),
                                         value = 6, min = 0, step = 1)
                          ),
                          
                          actionButton(inputId = "goPoisson", label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetPoisson", label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                        ),
                        
                        conditionalPanel(id = "normalPanel",
                          condition = "input.probability == 'Normal'",
                          
                          numericInput(inputId = "popMean", 
                                       label = strong("Population Mean (\\( \\mu\\))"), 
                                       value = 0, step = 0.00001),
                          
                          numericInput(inputId = "popSD",
                                       label = strong("Population Standard Deviation (\\( \\sigma\\))"),
                                       value = 1, min = 0, step = 0.00001),

                          radioButtons(inputId = "calcNormal",
                                       label = strong("Probability"), 
                                       choiceValues = list("cumulative", "upperTail", "between"),
                                       choiceNames = list("\\(P(X \\leq x)\\) or \\(P(X < x)\\)", "\\(P(X \\ge x)\\) or \\(P(X \\gt x)\\)", "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.calcNormal != 'between'",
                            
                                numericInput(inputId = "xValue",
                                             label = strong("Normally distributed variable (\\( x\\))"),
                                             value = 0, step = 0.00001),
                          ),
                          
                          conditionalPanel(
                            condition = "input.calcNormal == 'between'",
                            
                            numericInput(inputId = "x1Value",
                                         label = strong("Normally distributed variable (\\( x_{1}\\))"),
                                         value = -1, step = 0.00001),
                            
                            numericInput(inputId = "x2Value",
                                         label = strong("Normally distributed variable (\\( x_{2}\\))"),
                                         value = 1, step = 0.00001),

                          ),
                          
                          actionButton(inputId = "goNormal", label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetNormal", label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                        )
                      ),
                      
                      #   --------------------------------------- #  
                      ### ---- Statistical Inference sidebar ---- 
                      #   --------------------------------------- #
                      conditionalPanel(id = "inferencePanel",
                        condition = "input.dropDownMenu == 'Statistical Inference'",

                        radioButtons(inputId = "samplesSelect",
                                     label = strong("Number of samples"),
                                     choiceValues = list("1", "2"),
                                     choiceNames = list("1", "2"),
                                     selected = "1", #character(0), #
                                     inline = TRUE), #,width = '1000px'),
                        
                        # radioButtons(inputId = "popuDistribution",
                        #              label = strong("Analysis Type"),
                        #              choiceValues = list("Parametric analysis", "Non-parametric analysis"),
                        #              choiceNames = list("Parametric analysis", "Non-parametric analysis"),
                        #              selected = "Parametric analysis", #character(0),
                        #              inline = TRUE), #,width = '1000px'),
                        # 
                        # conditionalPanel(
                        #   condition = "input.popuDistribution == 'Non-parametric analysis'",
                        # 
                        # ),
                        # 
                        # conditionalPanel(
                        #   condition = "input.popuDistribution == 'Parametric analysis'",
                        # 
                        # ),
                        
                        conditionalPanel(
                          condition = "input.samplesSelect == '1'",
                          
                          radioButtons(inputId = "popuParameter",
                                       label = strong("Parameter of Interest"),
                                       choiceValues = list("Population Mean", "Population Proportion"),
                                       choiceNames = list("Population Mean (\\( \\mu\\))", "Population Proportion (\\( p\\))"),
                                       selected = "Population Mean", #character(0), #
                                       inline = TRUE), #,width = '1000px'),
                          
                          # radioButtons(inputId = "popuParameter",
                          #              label = strong("Parameter of Interest"),
                          #              choiceValues = list("Population Mean", "Population Standard Deviation", "Sample Size Estimation"),
                          #              choiceNames = list("Population Mean (\\( \\mu\\))", "Population Standard Deviation (\\( \\sigma\\))", "Sample Size Estimation (n)"),
                          #              selected = "Population Mean",
                          #              #inline = TRUE), #,width = '1000px'),

                          conditionalPanel(
                            condition = "input.popuParameter == 'Population Mean'",
                            
                            radioButtons(inputId = "dataAvailability",
                                         label = strong("Data Availability"),
                                         choiceValues = list("Summarized Data", "Enter Raw Data"),
                                         choiceNames = list("Summarized Data", "Enter Raw Data"),
                                         selected = "Summarized Data", # character(0), # 
                                         inline = TRUE), #,width = '1000px'),
                            
                            conditionalPanel(
                              condition = "input.dataAvailability == 'Summarized Data'",
                              
                              numericInput(inputId = "sampleSize",
                                           label = strong("Sample Size (\\( n\\))"),
                                           value = 18, min = 1, step = 1),
                              
                              numericInput(inputId = "sampleMean",
                                           label = strong("Sample Mean (\\( \\bar{x}\\))"),
                                           value = 103.5375, step = 0.00001),
                              
                              radioButtons(inputId = "sigmaKnown",
                                           label = strong("Is Population Standard Deviation (\\( \\sigma\\)) known?"),
                                           choiceValues = list("Known", "Unknown"),
                                           choiceNames = list("Known", "Unknown"),
                                           selected = "Known", #character(0),
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.sigmaKnown == 'Known'",
                                
                                numericInput(inputId = "popuSD",
                                             label = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                                             value = 8.25, min = 0.00001, step = 0.00001)),
                              
                              conditionalPanel(
                                condition = "input.sigmaKnown == 'Unknown'",
                                
                                numericInput(inputId = "sampSD",
                                             label = strong("Sample Standard Deviation (\\( s\\)) Value"),
                                             value = 4.78, min = 0.00001, step = 0.00001)),
                            ), # One Sample Summarized Data
                            
                            conditionalPanel(
                              condition = "input.dataAvailability == 'Enter Raw Data'",
                              
                              textAreaInput("sample1", strong("Sample"), value = "202, 210, 215, 220, 220, 224, 225, 228, 228, 228", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                              
                              radioButtons(inputId = "sigmaKnownRaw",
                                           label = strong("Population Standard Deviation (\\( \\sigma\\))"),
                                           choiceValues = list("rawKnown", "rawUnknown"),
                                           choiceNames = list("Known", "Unknown"),
                                           selected = "rawKnown",
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.sigmaKnownRaw == 'rawKnown'",
                                
                                numericInput(inputId = "popuSDRaw",
                                             label = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                                             value = 8.25, min = 0.00001, step = 0.00001)
                              ),
                              
                              conditionalPanel(
                                condition = "input.sigmaKnownRaw == 'rawUnknown'"
                              )
                            ), # One Sample Raw Data
                          ), # One Population Mean
                          
                          conditionalPanel(
                            condition = "input.popuParameter == 'Population Proportion'",

                              numericInput(inputId = "numSuccesses",
                                           label = strong("Number of Successes (\\( x\\))"),
                                           value = 1087, min = 0, step = 1),

                              numericInput(inputId = "numTrials",
                                           label = strong("Number of Trials (\\( n\\))"),
                                           value = 1430, min = 1, step = 1),
                          ), #One Population Proportion
                          
                          conditionalPanel(
                            condition = "input.dataAvailability == 'Summarized Data' || input.dataAvailability == 'Enter Raw Data' || input.popuParameter == 'Population Proportion'",
                            
                              radioButtons(inputId = "inferenceType",
                                           label = strong("Inference Type"),
                                           choiceValues = list("Confidence Interval", "Hypothesis Testing"),
                                           choiceNames = list("Confidence Interval", "Hypothesis Testing"),
                                           selected = "Confidence Interval", #character(0), # 
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Confidence Interval'",
                                
                                radioButtons(inputId = "confidenceLevel", 
                                             label = strong("Confidence Level (\\( 1- \\alpha\\))"), 
                                             selected = c("95%"), 
                                             choices = c("90%", "95%","99%"), 
                                             inline = TRUE)
                              ),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Hypothesis Testing'",
                                
                                radioButtons(inputId = "significanceLevel", 
                                             label = strong("Significance Level (\\( \\alpha\\))"), 
                                             selected = c("5%"), 
                                             choices = c("10%", "5%","1%"), 
                                             inline = TRUE),
                                
                                conditionalPanel(
                                  condition = "input.popuParameter == 'Population Mean'",
                                  
                                  conditionalPanel(
                                    condition = "input.dataAvailability == 'Summarized Data' || input.dataAvailability == 'Enter Raw Data'",
                                    
                                    numericInput(inputId = "hypMean",
                                                 label = strong("Hypothesized Population Mean (\\( \\mu_{0}\\)) Value"),
                                                 value = 99, step = 0.00001),
                                  ),
                                ),
                                
                                conditionalPanel(
                                  condition = "input.popuParameter == 'Population Proportion'",
                                  
                                  numericInput(inputId = "hypProportion",
                                               label = strong("Hypothesized Population Proportion (\\( p_{0}\\)) Value"),
                                               value = 0.5, min = 0, max = 1, step = 0.00001),
                                ),
                                
                                selectizeInput(inputId = "altHypothesis",
                                               label = strong("Alternate Hypothesis (\\( H_{a}\\))"),
                                               choices = c(
                                                 "< " = 1,
                                                 "&ne; " = 2,
                                                 "> " = 3
                                               ),
                                               selected = 2,
                                               options = list(
                                                 render = I(render)
                                               ),
                                ),
                              ), # Dropdown for 1-sample HT
                          ), # CI vs HT for 1 sample
                        ), #"input.samplesSelect == '1'"
   
                        conditionalPanel(
                          condition = "input.samplesSelect == '2'",
                          
                          radioButtons(inputId = "popuParameters",
                                       label = strong("Parameter of Interest"),
                                       choiceValues = list("Independent Population Means", "Dependent Population Means", "Population Proportions"),
                                       choiceNames = list("Two Independent Populations (\\( \\mu_{1} - \\mu_{2} \\))", "Dependent (Paired) Populations (\\( \\mu_{d} \\))", "Two Population Proportions (\\( p_{1} - p_{2}\\))"),
                                       selected = "Independent Population Means", #character(0), #
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.popuParameters == 'Independent Population Means'",
                            
                            radioButtons(inputId = "dataAvailability2",
                                         label = strong("Data Availability"),
                                         choiceValues = list("Summarized Data", "Enter Raw Data"),
                                         choiceNames = list("Summarized Data", "Enter Raw Data"),
                                         selected = "Summarized Data", #character(0), # 
                                         inline = TRUE), #,width = '1000px'),
                            
                            conditionalPanel(
                              condition = "input.dataAvailability2 == 'Summarized Data'",
                              
                              numericInput(inputId = "sampleSize1",
                                           label = strong("Sample Size 1 (\\( n_{1}\\))"),
                                           value = 21, min = 1, step = 1),
                              
                              numericInput(inputId = "sampleMean1",
                                           label = strong("Sample Mean 1 (\\( \\bar{x}_{1}\\))"),
                                           value = 29.6, step = 0.00001),
                              
                              numericInput(inputId = "sampleSize2",
                                           label = strong("Sample Size 2 (\\( n_{2}\\))"),
                                           value = 21, min = 1, step = 1),
                              
                              numericInput(inputId = "sampleMean2",
                                           label = strong("Sample Mean 2 (\\( \\bar{x}_{2}\\))"),
                                           value = 33.9, step = 0.00001),
                              
                              radioButtons(inputId = "bothsigmaKnown",
                                           label = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                                           choiceValues = list("bothKnown", "bothUnknown"),
                                           choiceNames = list("Both Known", "Both Unknown (Assumed Equal)"),
                                           selected = "bothKnown",
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnown == 'bothKnown'",
                                
                                numericInput(inputId = "popuSD1",
                                             label = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                                             value = 5.36, min = 0.00001, step = 0.00001),
                                
                                numericInput(inputId = "popuSD2",
                                             label = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                                             value = 5.97, min = 0.00001, step = 0.00001),
                              ),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnown == 'bothUnknown'",
                                
                                numericInput(inputId = "sampSD1",
                                             label = strong("Sample Standard Deviation 1 (\\( s_{1}\\)) Value"),
                                             value = 5.24, min = 0.00001, step = 0.00001),
                                
                                numericInput(inputId = "sampSD2",
                                             label = strong("Sample Standard Deviation 2 (\\( s_{2}\\)) Value"),
                                             value = 5.85, min = 0.00001, step = 0.00001),
                                
                                # radioButtons(inputId = "bothsigmaEqual",
                                #              label = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                                #              choiceValues = list("Yes", "No"),
                                #              choiceNames = list("Yes (Pooled)", "No (Welch-Satterthwaite df)"),
                                #              selected = "Yes",
                                #              inline = TRUE), #,width = '1000px'),
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.dataAvailability2 == 'Enter Raw Data'",
                              
                              textAreaInput("raw_sample1", strong("Sample 1"), value = "101.1,  111.1,  107.6,  98.1,  99.5,  98.7,  103.3,  108.9,  109.1,  103.3", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                              
                              textAreaInput("raw_sample2", strong("Sample 2"), value = "107.1,  105.0,  98.0,  97.9,  103.3,  104.6,  100.1,  98.2,  97.9", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                              
                              radioButtons(inputId = "bothsigmaKnownRaw",
                                           label = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                                           choiceValues = list("bothKnownRaw", "bothUnknownRaw"),
                                           choiceNames = list("Both Known", "Both Unknown (Assumed Equal)"),
                                           selected = "bothKnownRaw",
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnownRaw == 'bothKnownRaw'",
                                
                                numericInput(inputId = "popuSD1",
                                             label = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                                             value = 4.54, min = 0.00001, step = 0.00001),
                                
                                numericInput(inputId = "popuSD2",
                                             label = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                                             value = 3.47, min = 0.00001, step = 0.00001),
                              ),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnownRaw == 'bothUnknownRaw'",
                                
                                #print("Inference about two independent samples when populations variances are unknown but assumed equal")
                              ),
                            ),
                          ), # Two Independent Samples
                          
                          conditionalPanel(
                            condition = "input.popuParameters == 'Dependent Population Means'",
                          
                            textAreaInput("before", strong("Before"), value = "484, 478, 492, 444, 436, 398, 464, 476", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                            
                            textAreaInput("after", strong("After"), value = "488, 478, 480, 426, 440, 410, 458, 460", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                            
                          ), # Two Dependent Samples
                          
                          conditionalPanel(
                            condition = "input.popuParameters == 'Population Proportions'",
                            
                                numericInput(inputId = "numSuccesses1",
                                             label = strong("Number of Successes 1 (\\( x_{1}\\))"),
                                             value = 174, min = 0, step = 1),

                                numericInput(inputId = "numTrials1",
                                             label = strong("Number of Trials 1 (\\( n_{1}\\))"),
                                             value = 300, min = 1, step = 1),

                                numericInput(inputId = "numSuccesses2",
                                             label = strong("Number of Successes 2 (\\( x_{2}\\))"),
                                             value = 111, min = 0, step = 1),

                                numericInput(inputId = "numTrials2",
                                             label = strong("Number of Trials 2 (\\( n_{2}\\))"),
                                             value = 300, min = 1, step = 1),
                          ), # Two Population Proportions
                          
                          conditionalPanel(
                            condition = "input.dataAvailability2 == 'Summarized Data' || input.dataAvailability2 == 'Enter Raw Data' || input.popuParameters == 'Dependent Population Means' || input.popuParameters == 'Population Proportions'",
                            
                                radioButtons(inputId = "inferenceType2",
                                             label = strong("Inference Type"),
                                             choiceValues = list("Confidence Interval", "Hypothesis Testing"),
                                             choiceNames = list("Confidence Interval", "Hypothesis Testing"),
                                             selected = "Confidence Interval", #character(0), # 
                                             inline = TRUE), #,width = '1000px'),
                                
                                conditionalPanel(
                                  condition = "input.inferenceType2 == 'Confidence Interval'",
                                  
                                  radioButtons(inputId = "confidenceLevel2",
                                               label = strong("Confidence Level (\\( 1- \\alpha\\))"),
                                               selected = c("95%"),
                                               choices = c("90%", "95%","99%"),
                                               inline = TRUE)
                                ),
                            
                                conditionalPanel(
                                  condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                  
                                  radioButtons(inputId = "significanceLevel2", 
                                               label = strong("Significance Level (\\( \\alpha\\))"), 
                                               selected = c("5%"), 
                                               choices = c("10%", "5%","1%"), 
                                               inline = TRUE),
                                  
                                  selectizeInput(inputId = "altHypothesis2",
                                                 label = strong("Alternate Hypothesis (\\( H_{a}\\))"),
                                                 choices = c(
                                                   "< " = 1,
                                                   "&ne; " = 2,
                                                   "> " = 3
                                                 ),
                                                 selected = 2,
                                                 options = list(
                                                   render = I(render)
                                                 )
                                  ), 
                                ), # Dropdown for 2-sample HT
                          ) # CI vs HT for 2 samples
                        ), # "input.samplesSelect == '2'",

                        actionButton(inputId = "goInference", label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetInference", label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ),
                      
                      #   -------------------------------------------- #  
                      ### ---- Regression and Correlation sidebar ---- 
                      #   -------------------------------------------- #
                      conditionalPanel(id = "RegCorPanel",
                        condition = "input.dropDownMenu == 'Regression and Correlation'",
 
                        radioButtons(inputId = "simple_vs_multiple",
                                     label = strong("Regression Type"),
                                     choiceValues = list("SLR", "MLR"),
                                     choiceNames = list("Simple Linear Regression (SLR)", "Multiple Linear Regression (MLR)"),
                                     selected = "SLR", #character(0), # 
                                     inline = TRUE), #,width = '1000px'),
                        
                        conditionalPanel(
                          condition = "input.simple_vs_multiple == 'SLR'",
                          
                          radioButtons(inputId = "dataRegCor",
                                       label = strong("Data"),
                                       choiceValues = list("Enter Raw Data", "Upload Data"),
                                       choiceNames = list("Enter Raw Data", "Upload Data"),
                                       selected = "Enter Raw Data", #character(0), #
                                       inline = TRUE), #,width = '1000px'),

                           conditionalPanel(
                             condition = "input.dataRegCor == 'Enter Raw Data'",
  
                             textAreaInput("x", label = strong("\\( x\\) (Independent Variable)"), value = "10, 13, 18, 19, 22, 24, 27, 29, 35, 38", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                             textAreaInput("y", label = strong("\\( y\\) (Dependent Variable)"), value = "66, 108, 161, 177, 228, 235, 268, 259, 275, 278", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                             
                             # textAreaInput("x", label = strong("\\( x\\) (Independent Variable)"), value = "635, 644, 711, 708, 836, 820, 810, 870, 856, 923", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                             # textAreaInput("y", label = strong("\\( y\\) (Dependent Variable)"), value = "100, 93, 88, 84, 77, 75, 74, 63, 57, 55", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
  
                             # textAreaInput("x", label = strong("\\( x\\) (Independent Variable)"), value = "87, 92, 100, 103, 107, 110, 112, 127", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                             # textAreaInput("y", label = strong("\\( y\\) (Dependent Variable)"), value = "39, 47, 60, 50, 60, 65, 115, 118", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
  
                             # textAreaInput("x", label = strong("\\( x\\) (Independent Variable)"), value = "61, 111, 125, 134, 169, 173, 244", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                             # textAreaInput("y", label = strong("\\( y\\) (Dependent Variable)"), value = "4, 14, 15, 18, 21, 26, 38", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                          ),
                          
                          conditionalPanel(
                            condition = "input.dataRegCor == 'Upload Data'",
                            #fileInput('headerfileSLR', 'Upload data',
                            #          accept = c('text/csv','text/comma-separated-values','text/tab-separated-values',
                            #                     'text/plain','.csv','.txt','.xls','.xlsx'))
                            
                            fileInput(inputId = "slrUserData", "Upload your data (.csv or .xls or .xlsx)", 
                                      accept = c("text/csv","text/comma-separated-values", "text/plain", ".csv",".xls",".xlsx")
                            ),
                            
                            uiOutput("slrUploadVars"),
                          ),
                          
                          #radioButtons(inputId = "regressioncorrelation", 
                          #             label = strong("Analyze Data Using"), 
                          #             choices = c("Simple Linear Regression", "Correlation Coefficient"),
                          #             selected = NULL, # c("Simple Linear Regression"), # 
                          #             inline = TRUE),

                          #conditionalPanel(
                          #  condition = "input.regressioncorrelation == 'Simple Linear Regression'",

                          br(),
                          p(strong("Options")),
                          hr(),
                          
                          checkboxInput("scatterPlot", "Scatterplot of \\( x\\) versus \\( y\\)", value = TRUE),
                            
                          conditionalPanel(
                            condition = "input.scatterPlot == 1",
                                
                            textInput("main", label = strong("Main title and axes labels:"), value = "Scatter Plot", placeholder = "main title"),
                            textInput("xlab", label = NULL, value = "Independent Variable, x", placeholder = "x-axis label"),
                            textInput("ylab", label = NULL, value = "Dependent Variable, y", placeholder = "y-axis label"),
                                #hr(),
                          ),
                          #),
                          
                          #conditionalPanel(
                          #  condition = "input.regressioncorrelation == 'Correlation Coefficient'",
                            
                          #  checkboxInput("pearson", "Pearson's Product-Moment Correlation (r)"),
                          checkboxInput("kendall", "Kendall's Rank Correlation (\\( \\tau\\))"),
                          checkboxInput("spearman", "Spearman's Rank Correlation (\\( \\rho\\))"),
                            
                            # br(),
                            # checkboxGroupInput('corcoeff', strong('Correlation Coefficient'), choices = c("Pearson", "Kendall", "Spearman"), selected = "Pearson"),
                          #),
                        ),
                        
                        conditionalPanel(
                          condition = "input.simple_vs_multiple == 'MLR'",
                          
                          fileInput('headerfileMLR', 'Upload data',
                                    accept = c('text/csv','text/comma-separated-values','text/tab-separated-values',
                                               'text/plain','.csv','.txt','.xls','.xlsx'))
                        ),
                        
                        actionButton(inputId = "goRegression", label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetRegCor", label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ),
                      
                      # br(),
                      # downloadButton('describe_download', "Download Report", class="butt" ), br(),
                      # tags$head(tags$style(".butt{background-color:#337ab7;} .butt{color:#fff;}")), br(),
                      # radioButtons('format', 'Document format', c('PDF', 'Word'), inline = TRUE),
                    ),
                    
                    #  --------------------------- #  
                    ## ---- Methods mainPanel ---- 
                    #  --------------------------- #
                    mainPanel(
                      
                      # tags$style(type ="text/css",
                      #            ".shiny-output-error { visibility: hidden; }",
                      #            ".shiny-output-error:before { visibility: hidden; }"
                      # ),
                      
                      #   -------------------------------- #  
                      ### ---- Descriptive Stats main ---- 
                      #   -------------------------------- #
                      div(id = "descriptiveStatsMP",
                        conditionalPanel(
                          condition = "input.dropDownMenu == 'Descriptive Statistics'",
                            
                            conditionalPanel(
                              condition = "input.dataInput == 'Enter Raw Data'",
                              
                                tableOutput("table"),
                                br(),
                                
                                plotOutput("boxplotHorizontal"),
                                br(),
                              
                                #plotOutput("boxplotgg"),
                                #br(), 
                              
                                #downloadButton('downloadDataDS', 'Download Results')
                                
                            ),

                          conditionalPanel(
                            condition = "input.dataInput == 'Upload Data'",
                          
                          ),
                        )
                      ),
                      
                      #   ---------------------------------------- #  
                      ### ---- Probability Distributions main ---- 
                      #   ---------------------------------------- #
                      div(id = "probabilityMP",
                        conditionalPanel(
                          condition = "input.dropDownMenu == 'Probability Distributions'",
                            
                            conditionalPanel(
                              condition = "input.probability == 'Binomial'",
                                
                              br(),
                              uiOutput("renderProbabilityBinom"),
                            ),
                              
                            conditionalPanel(
                              condition = "input.probability == 'Poisson'",
                                
                              br(),
                              uiOutput("renderProbabilityPoisson"),
                            ),
                              
                            conditionalPanel(
                              condition = "input.probability == 'Normal'",
                                
                              br(),
                              uiOutput("renderProbabilityNorm"),
                            )
                        )
                      ), 
                      
                      #   ------------------------------------ #  
                      ### ---- Statistical Inference main ---- 
                      #   ------------------------------------ #
                      div(id = "inferenceMP",
                        conditionalPanel(
                          condition = "input.dropDownMenu == 'Statistical Inference'",
                            
                          uiOutput("renderInference"),
  
                          conditionalPanel(
                            condition = "input.samplesSelect == '1'",
                            
                            conditionalPanel(
                              condition = "input.popuParameter == 'Population Mean'",
                              
                              conditionalPanel(
                                condition = "input.dataAvailability == 'Summarized Data'",
                                
                                  conditionalPanel(
                                    condition = "input.sigmaKnown == 'Known'",
                                    
                                    conditionalPanel(
                                      condition = "input.inferenceType == 'Confidence Interval'",
                                      
                                      uiOutput('oneSampCI'),
                                      br(),
                                      img(src ='OneSampZInt.png', height = '100px')
                                    ), # One Sample CI
                                    
                                    conditionalPanel(
                                      condition = "input.inferenceType == 'Hypothesis Testing'",
                                      
                                      uiOutput('oneSampHT'),
                                      br(),
                                    ), # One Sample HT
                                  ),  # One Sample Sigma known
                                
                                  conditionalPanel(
                                    condition = "input.sigmaKnown == 'Unknown'",
                                    
                                    conditionalPanel(
                                      condition = "input.inferenceType == 'Confidence Interval'",
                                      
                                      uiOutput('oneSampCIUnknown'),
                                      br(),
                                      img(src ='OneSampTInt.png', height = '90px')
                                    ), # One Sample CI
                                    
                                    conditionalPanel(
                                      condition = "input.inferenceType == 'Hypothesis Testing'",
                                      
                                      uiOutput('oneSampHTUnknown'),
                                      br(),
                                    ), # One Sample HT
                                  ), # One Sample Sigma unknown
                                
                              ), # One Sample Summarized Data
                              
                              conditionalPanel(
                                condition = "input.dataAvailability == 'Enter Raw Data'",
                                
                                conditionalPanel(
                                  condition = "input.sigmaKnownRaw == 'rawKnown'",
                                  
                                  conditionalPanel(
                                    condition = "input.inferenceType == 'Confidence Interval'",
                                    
                                    uiOutput('oneSampCIRaw'),
                                    br(),
                                    img(src ='OneSampZInt.png', height = '100px')
                                  ), # One Sample CI Raw
                                  
                                  conditionalPanel(
                                    condition = "input.inferenceType == 'Hypothesis Testing'",
                                    
                                    uiOutput('oneSampHTRaw'),
                                    br(),
                                  ), # One Sample HT Raw
                                ), # One Sample Sigma known Raw
                                
                                conditionalPanel(
                                  condition = "input.sigmaKnownRaw == 'rawUnknown'",
                                  
                                  conditionalPanel(
                                    condition = "input.inferenceType == 'Confidence Interval'",
                                    
                                    uiOutput('oneSampCIRawUnknown'),
                                    br(),
                                    img(src ='OneSampTInt.png', height = '90px')
                                  ), # One Sample CI Raw
                                  
                                  conditionalPanel(
                                    condition = "input.inferenceType == 'Hypothesis Testing'",
                                    
                                    uiOutput('oneSampHTRawUnknown'),
                                    br(),
                                  ),  # One Sample HT Raw
                                ), # One Sample Sigma unknown Raw
                              ), # One Sample Raw Data
                            ), # One Population Mean
                            
                            conditionalPanel(
                              condition = "input.popuParameter == 'Population Proportion'",
                              
                            ), # One Population Proportion
                          ), # "input.samplesSelect == '1'"
  
                          conditionalPanel(
                            condition = "input.samplesSelect == '2'",
                              
                            conditionalPanel(
                              condition = "input.popuParameters == 'Independent Population Means'",
                                
                              conditionalPanel(
                                condition = "input.dataAvailability2 == 'Summarized Data'",
                                  
                                conditionalPanel(
                                  condition = "input.bothsigmaKnown == 'bothKnown'",
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Confidence Interval'",
                                      
                                    uiOutput('twoSampCIbothKnown'),
                                    br(),
                                    img(src ='TwoSampZInt.png', height = '75px')
                                  ), # CI
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                      
                                    uiOutput('twoSampHTbothKnown'),
                                    br(),
                                  ), # HT
                                ),
                                  
                                conditionalPanel(
                                  condition = "input.bothsigmaKnown == 'bothUnknown'",
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Confidence Interval'",
                                      
                                    uiOutput('twoSampCIbothUnknown'),
                                    br(),
                                    img(src ='TwoSampTInt.png', height = '75px')
                                  ), # CI
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                      
                                    uiOutput('twoSampHTbothUnknown'),
                                    br(),
                                  ), # HT
                                ) # both unknown
                              ), #summarized data
                                
                              conditionalPanel(
                                condition = "input.dataAvailability2 == 'Enter Raw Data'",
                                  
                                conditionalPanel(
                                  condition = "input.bothsigmaKnownRaw == 'bothKnownRaw'",
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Confidence Interval'",
                                      
                                    uiOutput('twoSampCIRawbothKnown'),
                                    br(),
                                    img(src ='TwoSampZInt.png', height = '75px')
                                  ), # CI
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                      
                                    uiOutput('twoSampHTRawbothKnown'),
                                    br(),
                                  ), # HT
                                ), #both known raw
                                  
                                conditionalPanel(
                                  condition = "input.bothsigmaKnownRaw == 'bothUnknownRaw'",
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Confidence Interval'",
                                      
                                    uiOutput('twoSampCIRawbothUnknown'),
                                    br(),
                                    img(src ='TwoSampTInt.png', height = '75px')
                                  ), # CI
                                    
                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                      
                                    uiOutput('twoSampHTRawbothUnknown'),
                                    br(),
                                  ), # HT
                                ), #both unknown raw
                              ), #raw data
                            ), # Two Independent Samples
                              
                            #-------------#
                            # PAIRED DATA #
                            #-------------#
                              
                            conditionalPanel(
                              condition = "input.popuParameters == 'Dependent Population Means'",
                                
                              conditionalPanel(
                                condition = "input.inferenceType2 == 'Confidence Interval'",
                                  
                                img(src ='TwoSampTIntPaired.png', height = '100px')
                              ), # CI
                                
                              conditionalPanel(
                                condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                  
                              ), # HT
                            ), # Two Dependent Samples
                              
                            #----------------------------#
                            # TWO POPULATION PROPORTIONS #
                            #----------------------------#
                            conditionalPanel(
                              condition = "input.popuParameters == 'Population Proportions'",
                                
                              conditionalPanel(
                                condition = "input.inferenceType2 == 'Confidence Interval'",
                                  
                              ), # CI
                                
                              conditionalPanel(
                                condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                  
                              ), # HT
                            ), # Two Population Proportions
                          ), # "input.samplesSelect == '2'"
                        ) # input.dropDownMenu == 'Statistical Inference'
                      ), # inferenceMP
                      
                      #   ----------------------------------------- #  
                      ### ---- Regression and Correlation main ---- 
                      #   ----------------------------------------- #
                      div(id = "RegCorMP",
                        conditionalPanel(
                          condition = "input.dropDownMenu == 'Regression and Correlation'",
                            
                          conditionalPanel(
                            condition = "input.simple_vs_multiple == 'SLR'",
                              
                            uiOutput("slrTabs"),
                            
                            #conditionalPanel(
                              #condition = "input.regressioncorrelation == 'Simple Linear Regression'",
                              
                              #tabsetPanel(id = 'tabSet', selected = "SLR",
                              #  tabPanel(id = "SLR", title = "Simple Linear Regression",
                                    
                              #    conditionalPanel(
                               #     condition = "input.scatterPlot == 1",

                                #    titlePanel("Scatterplot"),
                                 #   plotOutput("scatterplot", width = "500px"),
                                  #  br(),
                                  #),
                                     
                                  #titlePanel("Estimated equation of the regression line"),
                                  #verbatimTextOutput("linearRegression"),
                                  #br(),
                                     
                                  #titlePanel("95% confidence interval for regression parameters"),
                                  #verbatimTextOutput("confintLinReg"),
                                  #br(),
                                     
                                  #titlePanel("ANOVA for regression"),
                                  #verbatimTextOutput("anovaLinReg"),
                                  #br(),
                                #), 
                                 
                                #tabPanel(id = "normality", title = "Normality of Residuals",
                                         
                                        #----------------------------------#
                                        # Tests for normality of residuals #
                                        #----------------------------------#
                                 #       titlePanel("Anderson-Darling test"),
                                  #      verbatimTextOutput("AndersonDarlingTest"),
                                   #     br(),
                                               
                                    #    titlePanel("Kolmogorov-Smirnov test"),
                                    #    verbatimTextOutput("KolmogorovSmirnovTest"),
                                     #   br(),
                                               
                                      #  titlePanel("Shapiro-Wilk test"),
                                       # verbatimTextOutput("ShapiroTest"),
                                        #br(),
                                #),
                                
                                #tabPanel(id = "resid", title = "Residual Plots",
                                    
                                        #-----------------------------#
                                        # Plots for Residual Analysis #
                                        #-----------------------------#
                                 #       titlePanel("Q-Q plot"),
                                #        plotOutput("qqplot", width = "500px"),
                                        #br(),
                                          
                                 #       titlePanel("Other diagnostic plots"),
                                #        plotOutput("moreplots", width = "500px"),
                                        #br(),
                                #),
                            
                                # selected = "SLR"
                                # verbatimTextOutput("outlierTest"),
                              #), #tabset
                            #), # Simple Linear Regression
                          
                            #conditionalPanel(
                            #  condition = "input.regressioncorrelation == 'Correlation Coefficient'",

                            #  conditionalPanel(
                            #    condition = "input.pearson == 1",

                            #    verbatimTextOutput("PearsonCorTest"),
                            #    br(),
                              
                            #    verbatimTextOutput("PearsonConfInt"),
                            #    br(),
                              
                                #titlePanel("Pearson's r"),
                            #    verbatimTextOutput("PearsonEstimate"),
                            #  ),
                            
                            #  conditionalPanel(
                            #    condition = "input.kendall == 1",

                                #titlePanel("Kendall's Tau"),
                            #    verbatimTextOutput("Kendall"),
                            #  ),

                            #  conditionalPanel(
                            #    condition = "input.spearman == 1",

                                #titlePanel("Spearman's rs"),
                            #    verbatimTextOutput("Spearman"),
                            #  ),
                            #), # Correlation Coefficient
                          ), # sLR
                          
                          conditionalPanel(
                            condition = "input.simple_vs_multiple == 'MLR'",
                        
                          ), # MLR
                        ), # input.dropdownmenu = regression and correlation 
                      ), # RegCorMP 
                    
                      # checkboxInput(
                      #   inputId = "themeToggle",
                      #   label = icon("cog")
                      # ),
                      # 
                      # tags$script(
                      #                           "
                      #             // define css theme filepaths
                      #             const themes = {
                      #                 dark: 'shinythemes/css/darkly.min.css',
                      #                 light: 'shinythemes/css/flatly.min.css'
                      #             }
                      #     
                      #             // function that creates a new link element
                      #             function newLink(theme) {
                      #                 let el = document.createElement('link');
                      #                 el.setAttribute('rel', 'stylesheet');
                      #                 el.setAttribute('text', 'text/css');
                      #                 el.setAttribute('href', theme);
                      #                 return el;
                      #             }
                      #     
                      #             // function that remove <link> of current theme by href
                      #             function removeLink(theme) {
                      #                 let el = document.querySelector(`link[href='${theme}']`)
                      #                 return el.parentNode.removeChild(el);
                      #             }
                      #     
                      #             // define vars
                      #             const darkTheme = newLink(themes.dark);
                      #             const lightTheme = newLink(themes.light);
                      #             const head = document.getElementsByTagName('head')[0];
                      #             const toggle = document.getElementById('themeToggle');
                      #     
                      #             // define extra css and add as default
                      #             const extraDarkThemeCSS = '.dataTables_length label, .dataTables_filter label, .dataTables_info {       color: white!important;} .paginate_button { background: white!important;} thead { color: white;}'
                      #             const extraDarkThemeElement = document.createElement('style');
                      #             extraDarkThemeElement.appendChild(document.createTextNode(extraDarkThemeCSS));
                      #             head.appendChild(extraDarkThemeElement);
                      #     
                      #     
                      #             // define event - checked === 'light'
                      #             toggle.addEventListener('input', function(event) {
                      #                 // if checked, switch to light theme
                      #                 if (toggle.checked) {
                      #                     removeLink(themes.dark);
                      #                     head.removeChild(extraDarkThemeElement);
                      #                     head.appendChild(lightTheme);
                      #                 }  else {
                      #                     // else add darktheme
                      #                     removeLink(themes.light);
                      #                     head.appendChild(extraDarkThemeElement)
                      #                     head.appendChild(darkTheme);
                      #                 }
                      #             })
                      #             "
                      #   )
                      ) # mainPanel
                    ), # sidebarLayout
                  ), # Methods Panel
                  
                  # -------------------------- #  
                  # ---- Authors Tab ---- 
                  # -------------------------- #
                  tabPanel("Authors",
                           h3("Developement Team", style= "font-weight:bold"),
                           
                           br(),

                           p(span("Ashok Krishnamurthy, PhD", style= "font-weight:bold")),
                           p("Project PI,"),
                           p("Associate Professor, Department of Mathematics and Computing,"),
                           p("Faculty of Science and Technology,"),
                           p("Mount Royal University,"), 
                           p("Calgary, AB, CANADA"),
                           br(),
                           
                           p("Email:",a("akrishnamurthy@mtroyal.ca", href = "mailto:akrishnamurthy@mtroyal.ca")), 
                           p("Website:", a(href = "https://bit.ly/2YKrXjX","https://bit.ly/2YKrXjX", target = "_blank")),
                           p("GitHub:", a(href = "https://github.com/ashokkrish/COMP5690","https://github.com/ashokkrish/COMP5690", target = "_blank")),
                           br(),
                           
                           p(span("Michael Myer,", style= "font-weight:bold")),
                           p("Developer"),
                           #p(span("Lead Developer", style = "font-weight:bold")),
                           p("Undergraduate Student, Mount Royal University,"),
                           p("Calgary, AB, CANADA"),

                           br(),
                           
                           p(span("Crystal Wai,", style= "font-weight:bold")),
                           p("Developer"),
                           #p(span("Lead Developer", style = "font-weight:bold")),
                           p("Undergraduate Student, Mount Royal University,"), 
                           p("Calgary, AB, CANADA"), 
                           
                           br(),

                           p("In Fall 2022 an earlier version of this interactive Shiny app was presented as Crystal Wai's COMP 5690 Senior Computer Science Project. Starting May 2023 this project will be funded by a student research grant awarded by the Faculty of Science and Technology at MRU."), 
                           br(),

                           p("This interactive R Shiny app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback."),
                           
                           hr(),

                           h5("Built with",
                              img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "35px"),
                              "by",
                              img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo.png", height = "35px"),
                              ".")
                  ) # Authors Panel
    ) #navbarPage
  ) #UI 

# --------------------------- #
# ---- Server components ----
# --------------------------- #
server <- function(input, output) {
    
    # ------------------------- #
    # ---- Data Validation ----
    # ------------------------- #

    iv <- InputValidator$new()
    regcor_iv <- InputValidator$new()
    slrraw_iv <- InputValidator$new()
    slrupload_iv <- InputValidator$new()
    slruploadvars_iv <- InputValidator$new()
    
    ## DS rules ----
    
    # descriptiveStat
    
    iv$add_rule("descriptiveStat", sv_required())
    
    ## PD rules ----
    
    # numTrialsBinom 
    
    iv$add_rule("numTrialsBinom", sv_required())
    iv$add_rule("numTrialsBinom", sv_integer())
    iv$add_rule("numTrialsBinom", sv_gt(0))
    
    # successProbBinom (PD)
    
    iv$add_rule("successProbBinom", sv_required())
    iv$add_rule("successProbBinom", sv_gte(0))
    iv$add_rule("successProbBinom", sv_lte(1))
    
    # numSuccessesBinom (PD)
    
    iv$add_rule("numSuccessesBinom", sv_required())
    iv$add_rule("numSuccessesBinom", sv_integer())
    iv$add_rule("numSuccessesBinom", sv_gte(0))
    
    # numSuccessesBinomx1 (PD)
    iv$add_rule("numSuccessesBinomx1", sv_required())
    iv$add_rule("numSuccessesBinomx1", sv_integer())
    iv$add_rule("numSuccessesBinomx1", sv_gte(0))
    
    # numSuccessesBinomx2 (PD)
    iv$add_rule("numSuccessesBinomx2", sv_required())
    iv$add_rule("numSuccessesBinomx2", sv_integer())
    iv$add_rule("numSuccessesBinomx2", sv_gte(0))
    
    # muPoisson (PD)
    
    iv$add_rule("muPoisson", sv_required())
    iv$add_rule("muPoisson", sv_gt(0))
    
    # xPoisson (PD)
    
    iv$add_rule("xPoisson", sv_required())
    iv$add_rule("xPoisson", sv_integer())
    iv$add_rule("xPoisson", sv_gte(0))
    
    # x1Poisson (PD)
    iv$add_rule("x1Poisson", sv_required())
    iv$add_rule("x1Poisson", sv_integer())
    iv$add_rule("x1Poisson", sv_gte(0))
    
    # x2Poisson (PD)
    iv$add_rule("x2Poisson", sv_required())
    iv$add_rule("x2Poisson", sv_integer())
    iv$add_rule("x2Poisson", sv_gte(0))
    
    # popMean (PD)
    
    iv$add_rule("popMean", sv_required())
    
    # popuSD (PD)
    
    iv$add_rule("popSD", sv_required())
    iv$add_rule("popSD", sv_gt(0))
    
    # xValue (PD)
    
    iv$add_rule("xValue", sv_required())
    
    iv$add_rule("x1Value", sv_required())
    iv$add_rule("x2Value", sv_required())
    
    ## SI rules ----
    
    # sampleSize 
    
    iv$add_rule("sampleSize", sv_required())
    iv$add_rule("sampleSize", sv_integer())
    iv$add_rule("sampleSize", sv_gt(0))
    
    # sampleMean 
    
    iv$add_rule("sampleMean", sv_required())
    
    # popuSD 
    
    iv$add_rule("popuSD", sv_required()) 
    iv$add_rule("popuSD", sv_gt(0))
    
    # popuSDRaw 
    
    iv$add_rule("popuSDRaw", sv_required()) 
    iv$add_rule("popuSDRaw", sv_gt(0))
    
    # sampSD 
    
    iv$add_rule("sampSD", sv_required())
    iv$add_rule("sampSD", sv_gt(0))
    
    # sampleSize1 
    
    iv$add_rule("sampleSize1", sv_required())
    iv$add_rule("sampleSize1", sv_integer())
    iv$add_rule("sampleSize1", sv_gt(0))
    
    # sampleMean1 
    
    iv$add_rule("sampleMean1", sv_required())
    
    # sampleSize2 
    
    iv$add_rule("sampleSize2", sv_required())
    iv$add_rule("sampleSize2", sv_integer())
    iv$add_rule("sampleSize2", sv_gt(0))
    
    # sampleMean2 
    
    iv$add_rule("sampleMean2", sv_required()) 
    
    # popuSD1 
    
    iv$add_rule("popuSD1", sv_required()) 
    iv$add_rule("popuSD1", sv_gt(0))
    
    # popuSD2 
    
    iv$add_rule("popuSD2", sv_required()) 
    iv$add_rule("popuSD2", sv_gt(0))
    
    # sampSD1 
    
    iv$add_rule("sampSD1", sv_required())
    iv$add_rule("sampSD1", sv_gt(0))
    
    # sampSD2 
    
    iv$add_rule("sampSD2", sv_required()) 
    iv$add_rule("sampSD2", sv_gt(0))
    
    # sample1 
    
    iv$add_rule("sample1", sv_required()) 
    
    # raw_sample1
    
    iv$add_rule("raw_sample1", sv_required())
    
    # raw_sample2 
    
    iv$add_rule("raw_sample2", sv_required()) 
    
    # numSuccessesProportion
    
    iv$add_rule("numSuccesses", sv_required())
    iv$add_rule("numSuccesses", sv_integer())
    iv$add_rule("numSuccesses", sv_gte(0))
    
    # x1
    iv$add_rule("numSuccesses1", sv_required())
    iv$add_rule("numSuccesses1", sv_integer())
    iv$add_rule("numSuccesses1", sv_gte(0))
    
    # x2
    iv$add_rule("numSuccesses2", sv_required())
    iv$add_rule("numSuccesses2", sv_integer())
    iv$add_rule("numSuccesses2", sv_gte(0))
    
    # numTrialsProportion
    
    iv$add_rule("numTrials", sv_required())
    iv$add_rule("numTrials", sv_integer())
    iv$add_rule("numTrials", sv_gt(0))
    
    # n1
    iv$add_rule("numTrials1", sv_required())
    iv$add_rule("numTrials1", sv_integer())
    iv$add_rule("numTrials1", sv_gt(0))
    
    # n2
    iv$add_rule("numTrials2", sv_required())
    iv$add_rule("numTrials2", sv_integer())
    iv$add_rule("numTrials2", sv_gt(0))
    
    # hypMean 
    
    iv$add_rule("hypMean", sv_required())
    
    # hypProportion 
    
    iv$add_rule("hypProportion", sv_required())
    iv$add_rule("hypProportion", sv_gte(0))
    
    ## RC rules ---- 
    
    slrraw_iv$add_rule("x", sv_required())
    #iv$add_rule("x", sv_regex("^[0-9]+(.[0-9]+)?(, [0-9](.[0-9]+)?)+$", "Data can only be numeric values seperated by commas"))
    slrraw_iv$add_rule("x", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                     "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
    slrraw_iv$add_rule("x", ~ if(sampleDiffRaw() != 0) "x and y must have the same number of observations")
    
    slrraw_iv$add_rule("y", sv_required())
    slrraw_iv$add_rule("y", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                     "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
    slrraw_iv$add_rule("y", ~ if(sampleDiffRaw() != 0) "x and y must have the same number of observations")
    
    slrupload_iv$add_rule("slrUserData", sv_required())
    slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) == 0) "File is empty")
    slrupload_iv$add_rule("slrUserData", ~ if(ncol(slrUploadData()) < 2) "Data must include one response and (at least) one explanatory variable")
    slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) < 3) "Samples must include at least 2 observations")
    
    slruploadvars_iv$add_rule("slrResponse", sv_required())
    slruploadvars_iv$add_rule("slrExplanatory", sv_required())
    slruploadvars_iv$add_rule("slrResponse", ~ if(sampleDiffUpload() != 0) "Missing values detected, x and y must have the same number of observations")
    
    slrraw_iv$condition(~ isTRUE(input$dataRegCor == 'Enter Raw Data'))
    slrupload_iv$condition(~ isTRUE(input$dataRegCor == 'Upload Data'))
    slruploadvars_iv$condition(function() {isTRUE(input$dataRegCor == 'Upload Data' && slrupload_iv$is_valid()) })
    
    
    regcor_iv$add_validator(slrraw_iv)
    regcor_iv$add_validator(slrupload_iv)
    regcor_iv$add_validator(slruploadvars_iv)
    
    iv$enable()
    regcor_iv$enable()
    slrraw_iv$enable()
    slrupload_iv$enable()
    slruploadvars_iv$enable()
    #slruploadvars_iv$disable()
    
    
    #observeEvent(input$dataRegCor, {
      
      #if(input$dataRegCor == 'Enter Raw Data')
      #{
        #regcor_iv$enable()
        #uploaddata_iv$disable()
      #}
      #else
      #{
        #uploaddata_iv$enable()
        #regcor_iv$disable()
      #}
    #})
    
    # -------------------------- #
    # ---- Functions/Output ----
    # -------------------------- #
    
    # String List to Numeric List
    createNumLst <- function(text) {
      text <- gsub("","", text)
      split <- strsplit(text, ",", fixed = FALSE)[[1]]
      as.numeric(split)
    }
    
    # Function to find the mode(s)
    Modes <- function(x) {
      ux <- unique(x)
      tab <- tabulate(match(x, ux))
      ux[tab == max(tab)]
      if (length(ux[tab == max(tab)]) == length(ux)) {return("No mode exists")}
      else if (length(ux[tab == max(tab)]) == 1) {return(ux[tab == max(tab)])}
      else if (length(ux[tab == max(tab)]) > 1 && length(ux[tab == max(tab)]) < length(ux)) {return("There are multiple modes")}
    }
    
    # Function to find the population standard deviation
    pop.sd <- function(x) {
      sqrt(sum((x-mean(x))^2)/length(x))
    }
    
    #  ------------------------------------- #
    ## ---- Descriptive Stats functions ----
    #  ------------------------------------- #
    observeEvent(input$goDescpStats, {
      dat <- createNumLst(input$descriptiveStat)
      
      print(sort(dat))
      
      if(anyNA(dat) | length(dat)<2){
        # validate(
        #   need(dat != "", "Enter a sample data"),
        # 
        #   errorClass = "myClass"
        # )
        print("Invalid input or not enough observations")
      }
      else{
        dat <- createNumLst(input$descriptiveStat)
        
        xbar <- round(mean(dat),4)
        sampStdDev <- round(sd(dat),4)
        #popuStdDev <- round(pop.sd(dat),4) # round(sqrt((n-1)/n) * sampStdDev(dat), 4)
        Quartile1 <- fivenum(dat)[2] #quantile(dat, 0.25, type = 5)
        Quartile3 <- fivenum(dat)[4] #quantile(dat, 0.75, type = 5)
        IQR <- Quartile3 - Quartile1
        Lower_Fence <- Quartile1 - (1.5*IQR)
        Upper_Fence <- Quartile3 + (1.5*IQR)
        Num_Outliers <- sum(dat < Lower_Fence) + sum(dat > Upper_Fence)
        CoeffVar <- round(sampStdDev/xbar,4)
        
        # print(dat)
        # print(Lower_Fence)
        # print(Upper_Fence)
        # print(sum(dat < Lower_Fence))
        # print(sum(dat > Upper_Fence))
        # print(Num_Outliers)

        values <- reactiveValues()
        #dataDS <- reactive(values)
        values$df <- data.frame(Variable = character(), Value = character())
        output$table <- renderTable(values$df)
        row1 <- data.frame(Variable = "Number of observations", Value = paste0(length(dat)))
        row2 <- data.frame(Variable = "Sum", Value = paste0(sum(dat)))
        row3 <- data.frame(Variable = "Sum of squares", Value = paste0(sum(dat^2)))
        row4 <- data.frame(Variable = "Mean", Value = xbar)
        row5 <- data.frame(Variable = "Mode", Value = paste(Modes(dat)))
        row6 <- data.frame(Variable = "Minimum", Value = paste0(min(dat)))
        row7 <- data.frame(Variable = "*First quartile (Q1)", Value = Quartile1)
        row8 <- data.frame(Variable = "Second quartile or median (Q2)", Value = paste0(median(dat)))
        row9 <- data.frame(Variable = "*Third quartile (Q3)", Value = Quartile3)
        row10 <- data.frame(Variable = "Maximum", Value = paste0(max(dat)))
        row11 <- data.frame(Variable = "*Notes", Value = "Q1 and Q3 are calculated by excluding Q2 on both sides")
        row12 <- data.frame(Variable = "Interquartile range (IQR)", Value = IQR)
        row13 <- data.frame(Variable = "Check for Outliers: Lower Fence", Value = Lower_Fence)
        row14 <- data.frame(Variable = "Check for Outliers: Upper Fence", Value = Upper_Fence)
        row15 <- data.frame(Variable = "Number of Potential Outliers", Value = Num_Outliers)
        row16 <- data.frame(Variable = "Range", Value = paste0(range(dat)[2]-range(dat)[1]))
        row17 <- data.frame(Variable = "Sample Standard Deviation", Value = sampStdDev)
        row18 <- data.frame(Variable = "Sample Variance", Value = paste0(round(var(dat),4)))
        row19 <- data.frame(Variable = "Standard Error of the Mean", Value = paste0(round(sd(dat)/sqrt(length(dat)),4)))
        row20 <- data.frame(Variable = "Coefficient of variation", Value = CoeffVar)
        row21 <- data.frame(Variable = "Skewness", Value = paste0(round(skewness(dat),4)))
        row22 <- data.frame(Variable = "Kurtosis", Value = paste0(round(kurtosis(dat),4)))
        #row23 <- data.frame(Variable = "Population Standard Deviation (sigma)", Value = popuStdDev)
        
        values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12, row13, row14, row15, row16, row17, row18, row19, row20, row21, row22)
        
        output$boxplotHorizontal <- renderPlot({
          
          #--------------------#
          # Horizontal boxplot #
          #--------------------#
          
          boxplot(dat, horizontal = TRUE, lty = 1,  pch = 8) #pch = 19)
          
          ## Add mean line
          # segments(x0 = mean(dat), y0 = 0.8,
          #          x1 = mean(dat), y1 = 1.2,
          #          col = "red", lwd = 2)
          # 
          # points(mean(dat), col = 3, pch = 19)
        })
        
        output$boxplotgg <- renderPlot({
          
          #-----------------#
          # ggplot2 boxplot #
          #-----------------#

          ggplot(as.data.frame(dat), aes(x = "", y = dat)) +
          geom_boxplot(show.legend = FALSE)
        })
        
      }
    })
    
    #  -------------------------------------------- #
    ## ---- Probability Distribution functions ----
    #  -------------------------------------------- #
    
    ### Binomial ----
    observeEvent(input$goBinom, {
      
      binom_n <- input$numTrialsBinom
      
      binom_p <- input$successProbBinom
      
      binom_x <- input$numSuccessesBinom
      
      binom_x1 <- input$numSuccessesBinomx1
      
      binom_x2 <- input$numSuccessesBinomx2
      
      output$renderProbabilityBinom <- renderUI({
        
        validate(
          need(binom_n != "", "Enter a value for the number of trials (n)"),
          need(binom_p != "", "Enter a value for the probability of success (p)"),
          
          errorClass = "myClass"
          )
        
        if(input$calcBinom != 'between')
        {
          validate(
            need(binom_x != "", "Enter a value for the number of successes (x)"),
            
            errorClass = "myClass"
            )
          
          if(!is.na(binom_n) && !is.na(binom_p) && !is.na(binom_x)){
            validate(
              need(binom_n > 0, "n must be a positive integer"),
              need(binom_n%%1==0, "n must be a positive integer"),
              
              need(binom_p >= 0, "p must be between 0 and 1"),
              need(binom_p <= 1, "p must be between 0 and 1"),
              
              need(binom_x >= 0, "x must be a positive integer"),
              need(binom_x%%1==0, "x must be a positive integer"),
              need(binom_x <= binom_n, "Number of successes (x) must be less than or equal to the number of trials (n)"),
              
              errorClass = "myClass"
              )
            
            if(binom_x <= binom_n && binom_x >= 0 && binom_p <= 1 && binom_p >= 0 && binom_n > 0){
              if(input$calcBinom == 'exact'){
                withMathJax(paste0("\\(P(X = \\)"," ", binom_x,"\\()\\)"," ","\\( = \\)"," ", round(dbinom(binom_x,binom_n,binom_p), 4)))
              }
              else if(input$calcBinom == 'cumulative'){
                withMathJax(paste0("\\(P(X \\leq \\)"," ", binom_x,"\\()\\)"," ","\\( = \\)"," ", round(pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE), 4)))
              }
              else if(input$calcBinom == 'upperTail'){
                withMathJax(paste0("\\(P(X \\geq \\)"," ", binom_x,"\\()\\)"," ","\\( = \\)"," ", round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE), 4)))
              }
              else if(input$calcBinom == 'greaterThan'){
                withMathJax(paste0("\\(P(X > \\)"," ", binom_x,"\\()\\)"," ","\\( = \\)"," ", round(pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE), 4)))
              }
              else if(input$calcBinom == 'lessThan'){
                withMathJax(paste0("\\(P(X < \\)"," ", binom_x,"\\()\\)"," ","\\( = \\)"," ", round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE), 4)))
              }
              
              # if(input$probDistTable == TRUE){
              #   output$probabilityTable <- DT::renderDataTable(
              #     {
              #       dfBinom <- data.frame("Probability", value = round(dbinom(x = 0:binom_n, size = binom_n, prob = binom_p), 4))
              #     }
              #   )
              # }
            }
          }
        }
        
        else if(input$calcBinom == 'between')
        {
          validate(
            need(binom_x1 != "", "Enter a value for the number of successes (x1)"),
            need(binom_x2 != "", "Enter a value for the number of successes (x2)"),
            
            errorClass = "myClass"
            )
          
          if(!is.na(binom_n) && !is.na(binom_p) && !is.na(binom_x1) && !is.na(binom_x2)){
            validate(
              need(binom_x1 >= 0, "x1 must be a positive integer"),
              need(binom_x1%%1==0, "x1 must be a positive integer"),
              need(binom_x1 <= binom_n, "Number of successes (x1) must be less than or equal to the number of trials (n)"),
              
              need(binom_x2 >= 0, "x2 must be a positive integer"),
              need(binom_x2%%1==0, "x2 must be a positive integer"),
              need(binom_x2 <= binom_n, "Number of successes (x2) must be less than or equal to the number of trials (n)"),
              
              need(binom_x1 <= binom_x2, "x1 must be less than or equal to x2"),
              
              errorClass = "myClass"
              )
            withMathJax(paste0("\\(P(", binom_x1, " ",  " \\leq X \\leq \\)"," ", binom_x2,"\\()\\)"," ","\\( = \\)"," ", round(pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE), 4),"\\( - \\)",round(pbinom(binom_x1-1,binom_n,binom_p,lower.tail = TRUE), 4)," ","\\( = \\)"," ", round(pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE) - pbinom(binom_x1 -1,binom_n,binom_p,lower.tail = TRUE), 4)))
          }
        }
      })
    })

    ### Poisson ----
    observeEvent(input$goPoisson, {
      
      Poisson_mu <- input$muPoisson
      
      Poisson_x <- input$xPoisson
      
      Poisson_x1 <- input$x1Poisson
      
      Poisson_x2 <- input$x2Poisson
      
      output$renderProbabilityPoisson <- renderUI({
        
        validate(
          need(Poisson_mu != "", "Enter a value for the average number of successes (mu)"),
          
          errorClass = "myClass"
          )
        
        if(input$calcPoisson != 'between')
        {
          validate(
            need(Poisson_x != "", "Enter a value for the number of successes (x)"),
            
            errorClass = "myClass"
            )
          
          if(!is.na(Poisson_mu) && !is.na(Poisson_x)){
            validate(
              need(Poisson_mu > 0, "Average must be greater than zero"),
              
              need(Poisson_x >= 0, "x must be a positive integer"),
              need(Poisson_x%%1==0, "x must be a positive integer"),
              
              errorClass = "myClass"
              )
            
            if(Poisson_x >= 0){
              if(input$calcPoisson == 'exact'){
                withMathJax(paste0("\\(P(X = \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(dpois(Poisson_x,Poisson_mu), 4)))
              }
              else if(input$calcPoisson == 'cumulative'){
                withMathJax(paste0("\\(P(X \\leq \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x,Poisson_mu,lower.tail = TRUE), 4)))
              }
              else if(input$calcPoisson == 'upperTail'){
                withMathJax(paste0("\\(P(X \\geq \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x - 1,Poisson_mu,lower.tail = FALSE), 4)))
              }
              else if(input$calcPoisson == 'greaterThan'){
                withMathJax(paste0("\\(P(X > \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x,Poisson_mu,lower.tail = FALSE), 4)))
              }
              else if(input$calcPoisson == 'lessThan'){
                withMathJax(paste0("\\(P(X < \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x - 1,Poisson_mu,lower.tail = TRUE), 4)))
              }
            }
          }
        }
        
        else if(input$calcPoisson == 'between')
        {
          validate(
            need(Poisson_x1 != "", "Enter a value for the number of successes (x1)"),
            need(Poisson_x2 != "", "Enter a value for the number of successes (x2)"),
            
            errorClass = "myClass"
            )
          
          if(!is.na(Poisson_mu) && !is.na(Poisson_x1) && !is.na(Poisson_x2)){
            validate(
              need(Poisson_x1 >= 0, "x1 must be a positive integer"),
              need(Poisson_x1%%1==0, "x1 must be a positive integer"),
              
              need(Poisson_x2 >= 0, "x2 must be a positive integer"),
              need(Poisson_x2%%1==0, "x2 must be a positive integer"),
              
              need(Poisson_x1 <= Poisson_x2, "x1 must be less than or equal to x2"),
              
              errorClass = "myClass"
            )
            withMathJax(paste0("\\(P(", Poisson_x1, " ",  " \\leq X \\leq \\)"," ", Poisson_x2,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x2, Poisson_mu, lower.tail = TRUE), 4), "\\( - \\)", round(ppois(Poisson_x1 - 1, Poisson_mu, lower.tail = TRUE), 4), " ","\\( = \\)"," ", round(ppois(Poisson_x2, Poisson_mu, lower.tail = TRUE) - ppois(Poisson_x1 - 1, Poisson_mu, lower.tail = TRUE), 4)))
          }
        }
      })
    })

       ### Normal ----
    observeEvent(input$goNormal, {
      
      norm_mu <- input$popMean
      
      norm_sigma <- input$popSD
      
      norm_x <- input$xValue
      
      norm_x1 <- input$x1Value
      
      norm_x2 <- input$x2Value
      
      output$renderProbabilityNorm <- renderUI({

        validate(
          need(norm_mu != "", "Enter a value for population mean (mu)"),
          need(norm_sigma != "", "Enter a value for population standard deviation (sigma)"),
          
          errorClass = "myClass"
          )

        if(!is.na(norm_mu) && !is.na(norm_sigma)){
          
            validate(
              need(norm_sigma > 0, "Standard Deviation must be greater than 0"),
              
              errorClass = "myClass"
            )
          
            if(input$calcNormal != 'between')
            {
              if(input$popSD > 0){
                if(input$calcNormal == "cumulative"){
                  withMathJax(paste0("\\(P(X \\leq \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = TRUE),4)))
                }
                else if(input$calcNormal == "upperTail"){
                  withMathJax(paste0("\\(P(X > \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = FALSE),4)))
                }
              }
            }
            else if(input$calcNormal == 'between')
            {
              validate(
                need(norm_x1 != "", "Enter a value for x1"),
                need(norm_x2 != "", "Enter a value for x2"),
                
                errorClass = "myClass"
                )
              
              if(!is.na(norm_mu) && !is.na(norm_x1) && !is.na(norm_x2)){
                validate(
                  need(norm_x1 <= norm_x2, "x1 must be less than or equal to x2"),
                  
                  errorClass = "myClass"
                )

                withMathJax(paste0("\\(P(", norm_x1, " ",  " \\leq X \\leq \\)"," ", norm_x2,"\\()\\)"," ","\\( = \\)"," ", round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE), 4), "\\( - \\)", round(pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4), " ","\\( = \\)"," ",round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE) - pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4)))
              }
            }
        }
      })
    })
    
    #  ----------------------------------------- #
    ## ---- Statistical Inference functions ----
    #  ----------------------------------------- #
    observeEvent(input$goInference, {
      output$renderInference <- renderDataTable(

        if(input$samplesSelect == '1'){
          
          if(input$inferenceType == 'Confidence Interval'){
            
            if(input$confidenceLevel == '90%'){
              ConfLvl <- 0.9
            }
            else if(input$confidenceLevel == '95%'){
              ConfLvl <- 0.95
            }
            else{
              ConfLvl <- 0.99
            }
          }
          
          else if(input$inferenceType == 'Hypothesis Testing'){
            
            if(input$significanceLevel == "10%"){
              sigLvl <- 0.1 
            }
            else if(input$significanceLevel == "5%"){
              sigLvl <- 0.05
            }
            else{
              sigLvl <- 0.01
            }
            
            if(input$altHypothesis == "3"){
              alternative <- "greater"
            }
            else if(input$altHypothesis == "2"){
              alternative <- "two.sided"
            }
            else{
              alternative <- "less"
            }
          }
          
          if(input$popuParameter == 'Population Mean'){
            if(input$dataAvailability == 'Summarized Data'){
              
              nSampOne <- input$sampleSize
              xbarSampOne <- input$sampleMean
              
              if(input$inferenceType == 'Confidence Interval'){
                
                if(input$sigmaKnown == 'Known'){
                  
                  sigmaSampOne <- input$popuSD
                  
                  source('R/OneSampZInt.R')
                  
                  print("Confidence Interval for One Population Mean when Population Standard Deviation is known")
                  
                  zIntPrint <- ZInterval(nSampOne, xbarSampOne, sigmaSampOne, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfKnown <- data.frame(Variable = character(), Value = character())
                  output$oneSampCI <- renderTable(values$dfKnown)
                  
                  row1 <- data.frame(Variable = "Sample Mean", Value = paste(zIntPrint[1]))
                  row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(zIntPrint[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(zIntPrint[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(zIntPrint[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(zIntPrint[5]))
                  
                  values$dfKnown <- rbind(row1, row2, row3, row4, row5)
                }
                
                else if(input$sigmaKnown == 'Unknown'){
                  
                  sSampOne <- input$sampSD
                  
                  source('R/OneSampTInt.R')
                  
                  print("Confidence Interval for One Population Mean when Population Standard Deviation is unknown")
                  
                  tIntPrint <- TInterval(nSampOne, xbarSampOne, sSampOne, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfUnknown <- data.frame(Variable = character(), Value = character())
                  output$oneSampCIUnknown <- renderTable(values$dfUnknown)
                  
                  row1 <- data.frame(Variable = "Sample Mean", Value = paste(tIntPrint[1]))
                  row2 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(tIntPrint[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(tIntPrint[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(tIntPrint[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(tIntPrint[5]))
                  
                  values$dfUnknown <- rbind(row1, row2, row3, row4, row5)
                } # input$sigmaKnown == 'Unknown'
              } # input$inferenceType == 'Confidence Interval'
              
              else if(input$inferenceType == 'Hypothesis Testing'){
                
                hypMeanSampOne <- input$hypMean 
                
                if(input$sigmaKnown == 'Known'){
                  
                  sigmaSampOne <- input$popuSD
                  
                  source("R/OneSampZTest.R")
                  
                  ZTest <- ZTest(nSampOne, xbarSampOne, sigmaSampOne, hypMeanSampOne, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfKnownHyp <- data.frame(Variable = character(), Value = character())
                  output$oneSampHT <- renderTable(values$dfKnownHyp)
                  
                  row1 <- data.frame(Variable = "Sample Size", Value = paste(ZTest[1]))
                  row2 <- data.frame(Variable = "Sample Mean", Value = paste(ZTest[2]))
                  row3 <- data.frame(Variable = "Population Standard Deviation", Value = paste(ZTest[3]))
                  row4 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(ZTest[4]))
                  row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(ZTest[5]))
                  row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(ZTest[6]))
                  row7 <- data.frame(Variable = "P-Value", Value = paste(ZTest[7]))
                  
                  values$dfKnownHyp <- rbind(row1, row2, row3, row4, row5, row6, row7) 
                }
                
                else if(input$sigmaKnown == 'Unknown'){
                  
                  sSampOne <- input$sampSD
                  
                  source("R/OneSampTTest.R")
                  
                  TTest <- TTest(nSampOne, xbarSampOne, sSampOne, hypMeanSampOne, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfUnKnownHyp <- data.frame(Variable = character(), Value = character())
                  output$oneSampHTUnknown <- renderTable(values$dfUnKnownHyp)
                  
                  row1 <- data.frame(Variable = "Sample Size", Value = paste(TTest[1]))
                  row2 <- data.frame(Variable = "Sample Mean", Value = paste(TTest[2]))
                  row3 <- data.frame(Variable = "Sample Standard Deviation", Value = paste(TTest[3]))
                  row4 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TTest[4]))
                  row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TTest[5]))
                  row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TTest[6]))
                  row7 <- data.frame(Variable = "P-Value", Value = paste(TTest[7]))
                  
                  values$dfUnKnownHyp <- rbind(row1, row2, row3, row4, row5, row6, row7) 
                } # input$sigmaKnown == 'Unknown'
              } # input$inferenceType == 'Hypothesis Testing'
            } # input$dataAvailability == 'Summarized Data'
            
            else if(input$dataAvailability == 'Enter Raw Data'){
              
              datRawData <- createNumLst(input$sample1)
              
              rawSampleSize <- length(datRawData)
              rawSampleMean <- mean(datRawData)
              
              if(input$inferenceType == 'Confidence Interval'){
                
                if(input$sigmaKnownRaw == 'rawKnown'){
                  
                  rawPopuSD <- input$popuSDRaw
                  
                  source("R/OneSampZInt.R")
                  
                  ZIntervalRaw <- ZInterval(rawSampleSize, rawSampleMean, rawPopuSD, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfKnownRaw <- data.frame(Variable = character(), Value = character())
                  output$oneSampCIRaw <- renderTable(values$dfKnownRaw)
                  
                  row1 <- data.frame(Variable = "Sample Mean", Value = paste(ZIntervalRaw[1]))
                  row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(ZIntervalRaw[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(ZIntervalRaw[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(ZIntervalRaw[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(ZIntervalRaw[5]))
                  
                  values$dfKnownRaw <- rbind(row1, row2, row3, row4, row5)
                }
                
                else if(input$sigmaKnownRaw == 'rawUnknown'){
                  
                  rawSampleSD <- sd(datRawData)
                  
                  source("R/OneSampTInt.R")
                  
                  TIntervalRaw <- TInterval(rawSampleSize, rawSampleMean, rawSampleSD, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfUnknownRaw <- data.frame(Variable = character(), Value = character())
                  output$oneSampCIRawUnknown <- renderTable(values$dfUnknownRaw)
                  
                  row1 <- data.frame(Variable = "Sample Mean", Value = paste(TIntervalRaw[1]))
                  row2 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TIntervalRaw[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TIntervalRaw[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TIntervalRaw[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TIntervalRaw[5]))
                  
                  values$dfUnknownRaw <- rbind(row1, row2, row3, row4, row5) 
                } # input$sigmaKnownRaw == 'rawUnknown'
              } # input$inferenceType == 'Confidence Interval'
              
              else if(input$inferenceType == 'Hypothesis Testing'){
                
                hypMeanSampOne <- input$hypMean 
                
                if(input$sigmaKnownRaw == 'rawKnown'){
                  
                  rawPopuSD <- input$popuSDRaw
                  
                  source("R/OneSampZTest.R")
                  
                  ZTestRaw <- ZTest(rawSampleSize, rawSampleMean, rawPopuSD, hypMeanSampOne, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfKnownHypRaw <- data.frame(Variable = character(), Value = character())
                  output$oneSampHTRaw <- renderTable(values$dfKnownHypRaw)
                  
                  row1 <- data.frame(Variable = "Sample Size", Value = paste(ZTestRaw[1]))
                  row2 <- data.frame(Variable = "Sample Mean", Value = paste(ZTestRaw[2]))
                  row3 <- data.frame(Variable = "Population Standard Deviation", Value = paste(ZTestRaw[3]))
                  row4 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(ZTestRaw[4]))
                  row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(ZTestRaw[5]))
                  row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(ZTestRaw[6]))
                  row7 <- data.frame(Variable = "P-Value", Value = paste(ZTestRaw[7]))
                  
                  values$dfKnownHypRaw <- rbind(row1, row2, row3, row4, row5, row6, row7) 
                }
                
                else if(input$sigmaKnownRaw == 'rawUnknown'){
                  
                  rawSampleSD <- sd(datRawData)
                  
                  source("R/OneSampTTest.R")
                  
                  TTestRaw <- TTest(rawSampleSize, rawSampleMean, rawSampleSD, hypMeanSampOne, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfUnKnownHypRaw <- data.frame(Variable = character(), Value = character())
                  output$oneSampHTRawUnknown <- renderTable(values$dfUnKnownHypRaw)
                  
                  row1 <- data.frame(Variable = "Sample Size", Value = paste(TTestRaw[1]))
                  row2 <- data.frame(Variable = "Sample Mean", Value = paste(TTestRaw[2]))
                  row3 <- data.frame(Variable = "Sample Standard Deviation", Value = paste(TTestRaw[3]))
                  row4 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TTestRaw[4]))
                  row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TTestRaw[5]))
                  row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TTestRaw[6]))
                  row7 <- data.frame(Variable = "P-Value", Value = paste(TTestRaw[7]))
                  
                  values$dfUnKnownHypRaw <- rbind(row1, row2, row3, row4, row5, row6, row7) 
                } # input$sigmaKnownRaw == 'rawUnknown'
              } # input$inferenceType == 'Hypothesis Testing'
            } # input$dataAvailability == 'Enter Raw Data'
          }
          # else if(input$popuParameter == 'Population Proportion'){
          #   source('R/OnePropZInt.R')
          #   source('R/OnePropZTest.R')
          #   print("Inferences for One Population Proportion are under contruction")
          # }
        }
        
        else if(input$samplesSelect == '2'){
          
          if(input$inferenceType2 == 'Confidence Interval'){
            
            if(input$confidenceLevel2 == '90%'){
              ConfLvl <- 0.9
            }
            else if(input$confidenceLevel2 == '95%'){
              ConfLvl <- 0.95
            }
            else{
              ConfLvl <- 0.99
            }
          }
          
          else if(input$inferenceType2 == 'Hypothesis Testing'){
            
            if(input$significanceLevel2 == "10%"){
              sigLvl <- 0.1 
            }
            else if(input$significanceLevel2 == "5%"){
              sigLvl <- 0.05
            }
            else{
              sigLvl <- 0.01
            }
            
            if(input$altHypothesis2 == "3"){
              alternative <- "greater"
            }
            else if(input$altHypothesis2 == "2"){
              alternative <- "two.sided"
            }
            else{
              alternative <- "less"
            }
          }
          
          if(input$popuParameters == 'Independent Population Means'){
            if(input$dataAvailability2 == 'Summarized Data'){
              n1 <- input$sampleSize1
              xbar1 <- input$sampleMean1
              
              n2 <- input$sampleSize2
              xbar2 <- input$sampleMean2
              
              if(input$inferenceType2 == 'Confidence Interval'){
                if(input$bothsigmaKnown == 'bothKnown'){
                  
                  sigma1 <- input$popuSD1
                  sigma2 <- input$popuSD2
                  
                  source('R/TwoSampZInt.R')
                  
                  TwoSampZInt <- TwoSampZInt(xbar1, sigma1, n1, xbar2, sigma2, n2, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoKnownSum <- data.frame(Variable = character(), Value = character())
                  output$twoSampCIbothKnown <- renderTable(values$dfTwoKnownSum)
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampZInt[1]))
                  row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(TwoSampZInt[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampZInt[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TwoSampZInt[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TwoSampZInt[5]))
                  
                  values$dfTwoKnownSum <- rbind(row1, row2, row3, row4, row5)
                }
                
                else if(input$bothsigmaKnown == 'bothUnknown'){
                  
                  s1 <- input$sampSD1
                  s2 <- input$sampSD2
                  
                  source('R/TwoSampTInt.R')
                  
                  TwoSampTInt <- TwoSampTInt(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoUnknownSum <- data.frame(Variable = character(), Value = character())
                  output$twoSampCIbothUnknown <- renderTable(values$dfTwoUnknownSum )
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampTInt[1]))
                  row2 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TwoSampTInt[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampTInt[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TwoSampTInt[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TwoSampTInt[5]))
                  
                  values$dfTwoUnknownSum  <- rbind(row1, row2, row3, row4, row5)
                }
              }
              else if(input$inferenceType2 == 'Hypothesis Testing'){
                
                if(input$bothsigmaKnown == 'bothKnown'){
                  
                  sigma1 <- input$popuSD1
                  sigma2 <- input$popuSD2
                  
                  source('R/TwoSampZTest.R')
                  
                  TwoSampZTest <- TwoSampZTest(xbar1, sigma1, n1, xbar2, sigma2, n2, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoKnownHyp <- data.frame(Variable = character(), Value = character())
                  output$twoSampHTbothKnown <- renderTable(values$dfTwoKnownHyp )
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampZTest[1]))
                  row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(TwoSampZTest[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampZTest[3]))
                  row4 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TwoSampZTest[4]))
                  row5 <- data.frame(Variable = "P-Value", Value = paste(TwoSampZTest[5]))
                  
                  values$dfTwoKnownHyp  <- rbind(row1, row2, row3, row4, row5)
                }
                
                else if(input$bothsigmaKnown == 'bothUnknown'){
                  
                  s1 <- input$sampSD1
                  s2 <- input$sampSD2
                  
                  source('R/TwoSampTTest.R')
                  
                  TwoSampTTest <- TwoSampTTest(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoUnknownHyp <- data.frame(Variable = character(), Value = character())
                  output$twoSampHTbothUnknown <- renderTable(values$dfTwoUnknownHyp )
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampTTest[1]))
                  row2 <- data.frame(Variable = "Degrees of freedom (df)", Value = paste(TwoSampTTest[2]))
                  row3 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TwoSampTTest[3]))
                  row4 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampTTest[4]))
                  row5 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TwoSampTTest[5]))
                  row6 <- data.frame(Variable = "P-Value", Value = paste(TwoSampTTest[6]))
                  
                  values$dfTwoUnknownHyp  <- rbind(row1, row2, row3, row4, row5, row6)
                }
              }
            }
            else if(input$dataAvailability2 == 'Enter Raw Data'){
              raw_sample1 <- createNumLst(input$raw_sample1)
              
              n1  <- length(raw_sample1)
              xbar1 <- mean(raw_sample1)
              
              raw_sample2 <- createNumLst(input$raw_sample2)
              
              n2  <- length(raw_sample2)
              xbar2 <- mean(raw_sample2)
              
              if(input$inferenceType2 == 'Confidence Interval'){
                
                if(input$bothsigmaKnown == 'bothKnown'){
                  
                  sigma1 <- input$popuSD1
                  sigma2 <- input$popuSD2
                  
                  source('R/TwoSampZInt.R')
                  
                  TwoSampZIntRaw <- TwoSampZInt(xbar1, sigma1, n1, xbar2, sigma2, n2, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoKnownCIRaw <- data.frame(Variable = character(), Value = character())
                  output$twoSampCIRawbothKnown <- renderTable(values$dfTwoKnownCIRaw)
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampZIntRaw[1]))
                  row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(TwoSampZIntRaw[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampZIntRaw[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TwoSampZIntRaw[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TwoSampZIntRaw[5]))
                  
                  values$dfTwoKnownCIRaw <- rbind(row1, row2, row3, row4, row5)
                }
                
                else if(input$bothsigmaKnown == 'bothUnknown'){
                  
                  s1 <- sd(raw_sample1)
                  s2 <- sd(raw_sample2)
                  
                  source('R/TwoSampTInt.R')
                  
                  TwoSampTIntRaw <- TwoSampTInt(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, ConfLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoUnknownCIRaw <- data.frame(Variable = character(), Value = character())
                  output$twoSampCIRawbothUnknown <- renderTable(values$dfTwoUnknownCIRaw)
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampTIntRaw[1]))
                  row2 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TwoSampTIntRaw[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampTIntRaw[3]))
                  row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TwoSampTIntRaw[4]))
                  row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TwoSampTIntRaw[5]))
                  
                  values$dfTwoUnknownCIRaw  <- rbind(row1, row2, row3, row4, row5)
                }
              }
              
              else if(input$inferenceType2 == 'Hypothesis Testing'){
                
                if(input$bothsigmaKnown == 'bothKnown'){
                  
                  sigma1 <- input$popuSD1
                  sigma2 <- input$popuSD2
                  
                  source('R/TwoSampZTest.R')
                  
                  TwoSampZTestRaw <- TwoSampZTest(xbar1, sigma1, n1, xbar2, sigma2, n2, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoKnownHypRaw <- data.frame(Variable = character(), Value = character())
                  output$twoSampHTRawbothKnown <- renderTable(values$dfTwoKnownHypRaw )
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampZTestRaw[1]))
                  row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(TwoSampZTestRaw[2]))
                  row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampZTestRaw[3]))
                  row4 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TwoSampZTestRaw[4]))
                  row5 <- data.frame(Variable = "P-Value", Value = paste(TwoSampZTestRaw[5]))
                  
                  values$dfTwoKnownHypRaw  <- rbind(row1, row2, row3, row4, row5)
                }
                
                else if(input$bothsigmaKnown == 'bothUnknown'){
                  
                  s1 <- sd(raw_sample1)
                  s2 <- sd(raw_sample2)
                  
                  source('R/TwoSampTTest.R')
                  
                  TwoSampTTestRaw <- TwoSampTTest(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, alternative, sigLvl)
                  
                  values <- reactiveValues()
                  values$dfTwoUnknownHypRaw <- data.frame(Variable = character(), Value = character())
                  output$twoSampHTRawbothUnknown <- renderTable(values$dfTwoUnknownHypRaw)
                  
                  row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampTTestRaw[1]))
                  row2 <- data.frame(Variable = "Degrees of freedom (df)", Value = paste(TwoSampTTestRaw[2]))
                  row3 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TwoSampTTestRaw[3]))
                  row4 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampTTestRaw[4]))
                  row5 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TwoSampTTestRaw[5]))
                  row6 <- data.frame(Variable = "P-Value", Value = paste(TwoSampTTestRaw[6]))
                  
                  values$dfTwoUnknownHypRaw <- rbind(row1, row2, row3, row4, row5, row6)
                } # input$bothsigmaKnown == 'bothUnknown'
              } # input$inferenceType2 == 'Hypothesis Testing'
            } # input$dataAvailability2 == 'Enter Raw Data'
          } # input$popuParameters == 'Independent Population Means'
          # else if(input$popuParameters == 'Dependent Population Means'){
          #   print("Inference for the two Dependent Populations")
          # }
          # else if(input$popuParameters == 'Population Proportions'){
          #   # source('R/TwoPropZInt.R')
          #   # source('R/TwoPropZTest.R')
          #   print("Inference for the difference between two Population Proportions")
          # }
        }
       ) # renderInference
    }) # input$goInference
    
    #  ------------------------------------------- #
    ## ---- Linear Regression and Correlation ----
    #  ------------------------------------------- #
    
    slrUploadData <- eventReactive(input$slrUserData, {
      ext <- tools::file_ext(input$slrUserData$name)
      
      switch(ext, 
             csv = read_csv(input$slrUserData$datapath),
             xls = read_excel(input$slrUserData$datapath),
             xlsx = read_excel(input$slrUserData$datapath),
             validate("Improper file format")
             #showModal( modalDialog(
             # title = "Warning",
             # "Improper File Format",
             # easyClose = TRUE
             #))
      )
    })
    
    sampleDiffRaw <- eventReactive({input$x
      input$y}, {
        datx <- createNumLst(input$x)
        daty <- createNumLst(input$y)
        return(length(datx) - length(daty))
      })
    
    sampleDiffUpload <- eventReactive (c(input$slrExplanatory, 
                                         input$slrResponse), {
      if(input$slrResponse == "")
      {
        return()
      }
      else
      {
        datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
        daty <- as.data.frame(slrUploadData())[, input$slrResponse]
        difference <- length(na.omit(datx)) - length(na.omit(daty))
        return(difference)
      }
    })
    
    observeEvent(input$slrUserData, {
      hide(id = "RegCorMP")
      if(slrupload_iv$is_valid())
      {
        output$slrUploadVars <- renderUI({
          
          tagList(
            
            selectizeInput(
              inputId = "slrExplanatory",
              label = strong("Choose the Explanatory Variable (x)"),
              choices = c(colnames(slrUploadData())),
              options = list(
                placeholder = 'Select a variable',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            
            selectizeInput(
              inputId = "slrResponse",
              label = strong("Choose the Response Variable (y)"),
              choices = c(colnames(slrUploadData())),
              options = list(
                placeholder = 'Select a variable',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
          )
        })
        
        #slruploadvars_iv$add_rule("slrResponse", sv_required())
        #slruploadvars_iv$add_rule("slrExplanatory", sv_required())
        #slruploadvars_iv$add_rule("slrResponse", ~ if(sampleDiffUpload() != 0) "Missing values detected, x and y must have the same number of observations")
      
        #slruploadvars_iv$condition(function() {isTRUE(input$dataRegCor == 'Upload Data' && slrupload_iv$is_valid()) })
        
        #regcor_iv$add_validator(slruploadvars_iv)
        
        #slruploadvars_iv$enable()
      }
      else
      {
        output$slrUploadVars <- renderUI({
          ""
        })
      }
    })
    
    observeEvent(input$slrExplanatory, {
      updateTextInput(inputId = "xlab", value = input$slrExplanatory)
    })
    
    observeEvent(input$slrResponse, {
      updateTextInput(inputId = "ylab", value = input$slrResponse)
    })
    
    
    observeEvent(input$goRegression, {
      
      if(input$simple_vs_multiple == 'SLR')
      {
        #if(input$dataRegCor == 'Upload Data')
        #{
        if(!slrupload_iv$is_valid())
        {
          output$slrTabs <- renderUI({
            validate(
              need(input$slrUserData, "Please upload your data to continue"),
              need(nrow(slrUploadData()) != 0, "File is empty"),
              need(ncol(slrUploadData()) > 1, "Data must include one response and (at least) one explanatory variable"),
              need(nrow(slrUploadData()) > 2, "Samples must include at least 2 observations"),
              errorClass = "myClass"
            )
          })
        }
        else if(!slruploadvars_iv$is_valid())
        {
          output$slrTabs <- renderUI({
            validate(
              need(input$slrExplanatory != "", "Please select an explanatory variable (x)"),
              need(input$slrResponse != "", "Please select a response variable (y)"),
              need(sampleDiffUpload() == 0, "x and y must have the same number of observations"),
              
              errorClass = "myClass"
            )
          })
        }
        else
        {
          if(input$dataRegCor == 'Upload Data')
          {
            datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
            daty <- as.data.frame(slrUploadData())[, input$slrResponse]
          }
          else
          {
            datx <- createNumLst(input$x)
            daty <- createNumLst(input$y)
          }
          
          if(regcor_iv$is_valid())
          {
            output$slrTabs <- renderUI({ ###tab generation ----
              
              tagList(
                
                tabsetPanel(id = "slrTabset", selected = "Simple Linear Regression",
                            
                            tabPanel(id = "slr", title = "Simple Linear Regression",
                                     
                                     conditionalPanel(
                                       condition = "input.scatterPlot == 1",
                                       
                                       titlePanel("Scatterplot"),
                                       plotOutput("scatterplot", width = "500px"),
                                       br(),
                                     ),
                                     
                                     titlePanel("Estimated equation of the regression line"),
                                     verbatimTextOutput("linearRegression"),
                                     br(),
                                     
                                     titlePanel("95% confidence interval for regression parameters"),
                                     verbatimTextOutput("confintLinReg"),
                                     br(),
                                     
                                     titlePanel("ANOVA for regression"),
                                     verbatimTextOutput("anovaLinReg"),
                                     #br(),
                            ), 
                            
                            tabPanel(id = "normality", title = "Normality of Residuals",
                                     
                                     #----------------------------------#
                                     # Tests for normality of residuals #
                                     #----------------------------------#
                                     titlePanel("Anderson-Darling test"),
                                     verbatimTextOutput("AndersonDarlingTest"),
                                     br(),
                                     
                                     titlePanel("Kolmogorov-Smirnov test"),
                                     verbatimTextOutput("KolmogorovSmirnovTest"),
                                     br(),
                                     
                                     titlePanel("Shapiro-Wilk test"),
                                     verbatimTextOutput("ShapiroTest"),
                                     #br(),
                            ),
                            
                            tabPanel(id = "resid", title = "Residual Plots",
                                     #-----------------------------#
                                     # Plots for Residual Analysis #
                                     #-----------------------------#
                                     titlePanel("Q-Q plot"),
                                     plotOutput("qqplot", width = "500px"),
                                     #br(),
                                     
                                     titlePanel("Other diagnostic plots"),
                                     plotOutput("moreplots", width = "500px"),
                                     #br(),
                            ),
                            
                            tabPanel(id = "correlation", title = "Correlation Analysis",
                                     
                                     #----------------------------------#
                                     # Correlation Coefficient Analysis #
                                     #----------------------------------#
                                     titlePanel("Pearson's Product-Moment Correlation"),
                                     verbatimTextOutput("PearsonCorTest"),
                                     br(),
                                     verbatimTextOutput("PearsonConfInt"),
                                     br(),
                                     verbatimTextOutput("PearsonEstimate"),
                                     br(),
                                     
                                     conditionalPanel(
                                       condition = "input.kendall == 1",
                                       
                                       titlePanel("Kendall's Rank Correlation"),
                                       verbatimTextOutput("Kendall"),
                                       br(),
                                     ),
                                     
                                     conditionalPanel(
                                       condition = "input.spearman == 1",
                                       
                                       titlePanel("Spearman's Rank Correlation"),
                                       verbatimTextOutput("Spearman"),
                                     ),
                                     #br(),
                            ),
                ),
              )
            })
            
            model <- lm(daty ~ datx)
            
            main <- input$main
            xlab <- input$xlab
            ylab <- input$ylab
            
            output$scatterplot <- renderPlot({
              plot(datx, daty, main = main, xlab = xlab, ylab = ylab, pch = 19) +
                abline(lm(daty ~ datx), col = "blue")
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
            output$AndersonDarlingTest <- renderPrint({ 
              ad.test(model$residuals)
            })
            
            # Kolmogorov-Smirnov Normality Test 
            output$KolmogorovSmirnovTest <- renderPrint({ 
              ks.test(model$residuals, "pnorm")
            })
            
            # Shapiro-Wilk Normality Test 
            output$ShapiroTest <- renderPrint({ 
              shapiro.test(model$residuals) 
            })
            
            # Q-Q plot for residuals
            output$qqplot <- renderPlot({
              #qqnorm(model$residuals, ylab = "Residuals", xlab = "Z Scores", main = "Q-Q plot of Standardized Residuals", pch = 19) #+
              #qqline(model$residuals)
              qqPlot(model$residuals, main = "Q-Q Plot", xlab = "Z Scores",  ylab = "Residuals", pch = 19) 
            })
            
            output$moreplots <- renderPlot({
              par(mfrow = c(2, 2))
              plot(model, which = 1:4, pch = 19)
            })
            
            # output$outlierTest <- renderPrint({ 
            #   outlierTest(model) # Prints the Bonferonni p-value for the most extreme observations
            # })
            
            # output$residversusfittedlot <- renderPlot({
            #   #plot(fitted(reg.model), resid(reg.model), pch = 19, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
            #   #abline(h = 0, col = "red")
            #   #leveragePlots(model) # leverage plots
            # })
            #}
            #else if(input$regressioncorrelation == "Correlation Coefficient") 
            #{
            req(length(datx) > 1) ## correlation coefficient ----
            if(length(datx) > 2)
            {
              Pearson <- cor.test(datx, daty, method = "pearson")
              
              output$PearsonCorTest <- renderPrint({ 
                Pearson
              })
              
              if(length(datx) > 3)
              {
                output$PearsonConfInt <- renderPrint({ 
                  Pearson$conf.int
                })
              }
              else
              {
                output$PearsonConfInt <- renderPrint ({
                  noquote("Computation of the Confidence Interval requires a minimum sample size of 4")
                })
              }
              
              output$PearsonEstimate <- renderPrint({
                cat(noquote(paste(c("Pearson's r:", round(Pearson$estimate[[1]], 4)))))
              })
            }
            else
            {
              #showNotification("Error: sample size must be greater than 2 for Pearson's", duration = NULL, closeButton = TRUE)
              output$PearsonCorTest <- renderPrint ({
                noquote("Pearson's Product-Moment Correlation requires a minimum sample size of 3 for computation")
              })
            }
            
            Kendall <- cor.test(datx, daty, method = "kendall")
            Spearman <- cor.test(datx, daty, method = "spearman")
            
            output$Kendall <- renderPrint({
              cat(noquote(paste(c("Kendall's Tau:", round(Kendall$estimate[[1]], 4)))))
            })
            
            output$Spearman <- renderPrint({
              cat(noquote(paste(c("Spearman's rs:", round(Spearman$estimate[[1]], 4)))))
            })
            #} # Correlation Coefficient
            
            df <- data.frame(datx, daty, datx*daty, datx^2, daty^2)
            names(df) <- c("X", "Y", "XY", "X^2", "Y^2")
            print(df)
            
          } #if regcor_iv is valid
          else
          {
            output$slrTabs <- renderUI({
              
              validate(
                need(length(datx) >= 2, "Must have at least 2 observations for x"),
                need(length(daty) >= 2, "Must have at least 2 observations for y"),
                need(!anyNA(datx), "Data must be numeric"),
                need(!anyNA(daty), "Data must be numeric"),
                need(length(datx) == length(daty), "x and y must have the same number of observations"),
                
                errorClass = "myclass"
              )
            })
          }
        }
      }
      
      #cut here
      show(id = "RegCorMP")
      
    }) # input$goRegression

    # --------------------------- #
    # ---- Component Display ----
    # --------------------------- #
    
    #  -------------------------------- #
    ## ---- Descriptive Statistics ----
    #  -------------------------------- #
    
    observeEvent(input$goDescpStats, {
      show(id = 'descriptiveStatsMP')
    })
    
    observeEvent(input$resetAll,{
      hide(id = 'descriptiveStatsMP')
      shinyjs::reset("descriptiveStatsPanel")
      #shinyjs::reset("sideBar")
    })
    
    #  ----------------------------------- #
    ## ---- Probability Distributions ----
    #  ----------------------------------- #
    
    #-----------------------#
    # Binomial Distribution #
    #-----------------------#
    
    observeEvent(input$goBinom, {
      show(id = 'probabilityMP')
    })
    
    observeEvent(input$resetBinomial, {
      hide(id = 'probabilityMP')
      shinyjs::reset("binomialPanel")
    })

    #----------------------#
    # Poisson Distribution #
    #----------------------#
    
    observeEvent(input$goPoisson, {
      show(id = "probabilityMP")
    })
    
    observeEvent(input$resetPoisson, {
      hide(id = "probabilityMP")
      shinyjs::reset("poissonPanel")
    })
    
    #---------------------#
    # Normal Distribution #
    #---------------------#
    
    observeEvent(input$goNormal, {
      show(id = "probabilityMP")
    })
    
    observeEvent(input$resetNormal, {
      hide(id = "probabilityMP")
      shinyjs::reset("normalPanel")
    })
    
    #  ------------------------------- #
    ## ---- Statistical Inference ----
    #  ------------------------------- #
    
    observeEvent(input$goInference, {
      show(id = "inferenceMP")
    })
    
    observeEvent(input$resetInference, {
      hide(id = "inferenceMP")
      shinyjs::reset("inferencePanel")
    })
    
    #  ------------------------------------ #
    ## ---- Regression and Correlation ----
    #  ------------------------------------ #
    
    observeEvent(input$dataRegCor, {
      hide(id = "RegCorMP")
    })
    
    #observeEvent(input$goRegression, {
      #show(id = "RegCorMP")
      #if(regcor_iv$is_valid())
      #{
        #show(id = "SLR")
        #show(id = "normality")
        #show(id = "resid")
        #showTab(inputId = 'tabSet', target = 'Simple Linear Regression', select = TRUE)
        #showTab(inputId = 'tabSet', target = 'Normality of Residuals')
        #showTab(inputId = 'tabSet', target = 'Residual Plots')
      #}
      #else
      #{
        #hide(id = "SLR")
        #hide(id = "normality")
        #hide(id = "resid")
        #hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
        #hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
        #hideTab(inputId = 'tabSet', target = 'Residual Plots')
      #}
    #})
    
    observeEvent(input$resetRegCor, {
      # hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
      # hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
      # hideTab(inputId = 'tabSet', target = 'Residual Plots')
      hide(id = "RegCorMP")
      shinyjs::reset("RegCorPanel")
    })
    
    #observe(
      #hide(id = "SLR")
      #hideTab(inputId = 'tabSet', target = 'Simple Linear Regression'), 
    #)

    #observe(
      #hide(id = "normality")
      #hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
    #)

    #observe(
      #hide(id = "resid")
      #hideTab(inputId = 'tabSet', target = 'Residual Plots')
    #)
    
    # output$downloadDataDS <- downloadHandler(
    #   filename = function(){"DS_file.xlsx"},
    #   content = function(file){write_xlsx(dataDS, file)}
    # )
}
  
shinyApp(ui = ui, server = server)