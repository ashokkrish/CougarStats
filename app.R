library(bslib)
library(car)
library(dplyr)
library(DT)
library(ggplot2)
library(moments)
library(nortest)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyvalidate)
library(tinytex)

options(scipen = 999) # options(scipen = 0)

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"
  
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
           
  navbarPage(title = div(img(src ="CougarStats.png", height = 100), span("CougarStats", style = "color:#000000; font-weight:bold; font-size:18pt")),

                tabPanel(title = "Methods",
                  sidebarLayout(
                    sidebarPanel(
                      withMathJax(),
                      shinyjs::useShinyjs(),
                      id = "sideBar", 
                      selectInput(
                        inputId = "dropDownMenu",
                        label = strong("Choose Statistical Topic"),
                        choices = c("Descriptive Statistics", "Probability Distributions", "Statistical Inference", "Regression and Correlation"),
                        selected = "Regression and Correlation", #NULL, 
                      ),
                      
                      conditionalPanel(
                        id = "descriptiveStatsPanel",
                        condition = "input.dropDownMenu == 'Descriptive Statistics'",
                        textAreaInput("descriptiveStat", label = strong("Sample"), value = "2.14, 2.09, 2.65, 3.56, 5.55, 5.00, 5.55, 3.09, 6.79", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),

                        actionButton(inputId = "goDescpStats", label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetAll", label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ),
                      
                      conditionalPanel(
                        id = "probPanel",
                        condition = "input.dropDownMenu == 'Probability Distributions'",
                        
                        radioButtons("probability", label = strong("Distribution"), choices = c("Binomial", "Poisson", "Normal"), selected = NULL, inline = TRUE),
                        
                        conditionalPanel(
                          id = "binomialPanel",
                          condition = "input.probability == 'Binomial'",
                          
                          numericInput(inputId = "numTrailsBinom", 
                                       label = strong("Number of Trials (n)"),
                                       value = 15, min = 1, step = 1),
                          
                          numericInput(inputId = "successProbBinom", 
                                       label = strong("Probability of Success (p)"),
                                       value = 0.29, min = 0, max = 1, step = 0.00001),

                          radioButtons(inputId = "calcBinom", 
                                       label = strong("Probability"),
                                       choiceValues = list("exact","cumulative","upperTail"),
                                       choiceNames = list("\\(P(X = x \\))","\\(P(X \\leq x)\\)","\\(P(X \\gt x)\\)"),
                                       inline = TRUE,
                                       width = '1000px'),

                          numericInput(inputId = "numSuccessesBinom", 
                                       label = strong("Number of Successes (x)"),
                                       value = 3, min = 0, step = 1),
                          
                          actionButton(inputId = "goBinom", label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetBinomial", label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") # , onclick = "history.go(0)"
                        ),
                        
                        conditionalPanel(
                          id = "poissonPanel", 
                          condition = "input.probability == 'Poisson'",
                          
                          numericInput("muPoisson", label = strong("Average (\\( \\mu\\))"),
                                       value = 4),

                          radioButtons(inputId = "calcPoisson",
                                       label = strong("Probability"),
                                       choiceValues = list("exact","cumulative","greaterThanEqual", "upperTail", "lessThan", "twoInputprob"),
                                       choiceNames = list("\\(P(X = x \\))","\\(P(X \\leq x)\\)","\\(P(X \\ge x)\\)", "\\(P(X \\gt x)\\)", "\\(P(X < x)\\)", "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = FALSE,
                                       width = '1000px'),
                          
                          numericInput("xPoisson", label = strong("Number of Successes (x)"),
                                       value = 3, min = 0, step = 1),
                          
                          # radioButtons(inputId = "calcPoisson",
                          #              label = strong("Probability"), 
                          #              choiceValues = list("exact","cumulative", "upperTail"),
                          #              choiceNames = list("\\(P(X = x\\))","\\(P(X \\leq x)\\)","\\(P(X \\gt x)\\)"),
                          #              inline = TRUE,
                          #              width = "1000px"
                          # ),
                          
                          # checkboxInput(inputId = "probDistTable",
                          #               label = strong("Probability Distribution Table"),
                          #               value = FALSE,
                          #               width = NULL),
                          
                          # conditionalPanel(
                          #   condition = "input.calcPoisson = 'interval'",
                          #   numericInput("aPoisson", "a",
                          #                value = 6, min = 0, step = 1
                          #   ),
                          #   numericInput("bPoisson", "b \\( (a \\leq b) \\)",
                          #                value = 10, min = 0, step = 1
                          #   )
                          # ),
                          
                          actionButton(inputId = "goPoisson", label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetPoisson", label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                        ),
                        
                        conditionalPanel(
                          id = "normalPanel",
                          condition = "input.probability == 'Normal'",
                          
                          numericInput(inputId = "popMean", 
                                       label = strong("Population Mean (\\( \\mu\\))"), 
                                       value = 0, step = 0.00001),
                          
                          numericInput(inputId = "popSD",
                                       label = strong("Population Standard Deviation (\\( \\sigma\\))"),
                                       value = 1, min = 0, step = 0.00001),

                          radioButtons(inputId = "calcNormal",
                                       label = strong("Probability"), 
                                       choiceValues = list("cumulative", "P(X > x)", "betweenNormal"),
                                       choiceNames = list("\\(P(X \\leq x)\\)", "\\(P(X \\gt x)\\)", "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = TRUE,
                                       width = "1000px"),
                          
                          numericInput(inputId = "xValue",
                                       label = strong("x"),
                                       value = 0, step = 0.00001),
                          
                          actionButton(inputId = "goNormal", label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetNormal", label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "input.dropDownMenu == 'Statistical Inference'",
                        
                        id = "inferencePanel",

                        # radioButtons(inputId = "popuParameter",
                        #              label = strong("Parameter of Interest"),
                        #              choiceValues = list("Population Mean", "Population Standard Deviation", "Sample Size Estimation"),
                        #              choiceNames = list("Population Mean (\\( \\mu\\))", "Population Standard Deviation (\\( \\sigma\\))", "Sample Size Estimation (n)"),
                        #              selected = "Population Mean",
                        #              #inline = TRUE,
                        #              width = "1000px"),
                        
                        radioButtons(inputId = "popuParameter",
                                     label = strong("Parameter of Interest"),
                                     choiceValues = list("Population Mean", "Population Proportion"),
                                     choiceNames = list("Population Mean (\\( \\mu\\))", "Population Proportion (p)"),
                                     selected = character(0), #"Population Mean",
                                     inline = TRUE,
                                     width = "1000px"),

                        conditionalPanel(
                          condition = "input.popuParameter == 'Population Mean' || input.popuParameter == 'Population Proportion'",

                          radioButtons(inputId = "samplesSelect",
                                       label = strong("Number of samples"),
                                       choiceValues = list("1", "2"),
                                       choiceNames = list("1", "2"),
                                       selected = character(0), #"1", #
                                       inline = TRUE,
                                       width = "1000px"),
                        ),
                        
                       conditionalPanel(
                          condition = "input.popuParameter == 'Population Mean'",

                            conditionalPanel(
                              condition = "input.samplesSelect == '1' || input.samplesSelect == '2'",
                              
                              radioButtons(inputId = "dataAvailability",
                                           label = strong("Data Availability"),
                                           choiceValues = list("Summarized Data", "Enter Raw Data"),
                                           choiceNames = list("Summarized Data", "Enter Raw Data"),
                                           selected = character(0), # "Summarized Data",
                                           inline = TRUE,
                                           width = "1000px"),
                            ),
                        
                            conditionalPanel(
                              condition = "input.samplesSelect == '1' && input.dataAvailability == 'Summarized Data'",
                              
                              numericInput(inputId = "sampleSize",
                                           label = strong("Sample Size (n)"),
                                           value = 18, min = 1, step = 1),
                              
                              numericInput(inputId = "sampleMean",
                                           label = strong("Sample Mean (\\( \\bar{x}\\))"),
                                           value = 103.5375, step = 0.00001),
                              
                              radioButtons(inputId = "sigmaKnown",
                                           label = strong("Population Standard Deviation (\\( \\sigma\\))"),
                                           choiceValues = list("Known", "Unknown"),
                                           choiceNames = list("Known", "Unknown"),
                                           selected = "Known",
                                           inline = TRUE,
                                           width = "1000px"),
                              
                              conditionalPanel(
                                condition = "input.sigmaKnown == 'Known'",
                                
                                numericInput(inputId = "popuSD",
                                             label = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                                             value = 8.25, min = 0.00001, step = 0.00001)),
                              
                              conditionalPanel(
                                condition = "input.sigmaKnown == 'Unknown'",
                                
                                numericInput(inputId = "sampSD",
                                             label = strong("Sample Standard Deviation (s) Value"),
                                             value = 4.78, min = 0.00001, step = 0.00001)),
                            ),
                            
                            conditionalPanel(
                              condition = "input.samplesSelect == '1' && input.dataAvailability == 'Enter Raw Data'",
                              
                              textAreaInput("sample1", strong("Sample"), value = "202, 210, 215, 220, 220, 224, 225, 228, 228, 228", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                              
                              radioButtons(inputId = "sigmaKnownRaw",
                                           label = strong("Population Standard Deviation (\\( \\sigma\\))"),
                                           choiceValues = list("rawKnown", "rawUnknown"),
                                           choiceNames = list("Known", "Unknown"),
                                           selected = "rawKnown",
                                           inline = TRUE,
                                           width = "1000px"),
                              
                              conditionalPanel(
                                condition = "input.sigmaKnownRaw == 'rawKnown'",
                                
                                numericInput(inputId = "popuSDRaw",
                                             label = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                                             value = 8.25, min = 0.00001, step = 0.00001)
                                ),
                              
                                conditionalPanel(
                                  condition = "input.sigmaKnownRaw == 'rawUnknown'"
                                )
                            ),
                            
                            conditionalPanel(
                              condition = "input.samplesSelect == '2' && input.dataAvailability == 'Summarized Data'",
                              
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
                                           label = strong("Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\))"),
                                           choiceValues = list("bothKnown", "bothUnknown"),
                                           choiceNames = list("Both Known", "Both Unknown (Assumed Equal)"),
                                           selected = "bothKnown",
                                           inline = TRUE,
                                           width = "1000px"),
                              
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
                                #              label = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))"),
                                #              choiceValues = list("Yes", "No"),
                                #              choiceNames = list("Yes", "No (Welch–Satterthwaite df)"),
                                #              selected = "Yes",
                                #              inline = TRUE,
                                #              width = "1000px")
                              )
                            ),
                            
                            conditionalPanel(
                              condition = "input.samplesSelect == '2' && input.dataAvailability == 'Enter Raw Data'",
                              
                              textAreaInput("raw_sample1", strong("Sample 1"), value = "101.1,  111.1,  107.6,  98.1,  99.5,  98.7,  103.3,  108.9,  109.1,  103.3", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                              
                              textAreaInput("raw_sample2", strong("Sample 2"), value = "107.1,  105.0,  98.0,  97.9,  103.3,  104.6,  100.1,  98.2,  97.9", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
    
                              radioButtons(inputId = "samplesType",
                                           label = strong("Type of Samples"),
                                           choiceValues = list("Independent Samples", "Dependent Samples"),
                                           choiceNames = list("Independent Samples", "Dependent Samples (Paired Data)"),
                                           selected = "Independent Samples",
                                           inline = TRUE,
                                           width = "1000px"),
                              
                              conditionalPanel(
                                condition = "input.samplesType == 'Independent Samples'",
                                
                                  radioButtons(inputId = "bothsigmaKnownRaw",
                                               label = strong("Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\))"),
                                               choiceValues = list("bothKnownRaw", "bothUnknownRaw"),
                                               choiceNames = list("Both Known", "Both Unknown (Assumed Equal)"),
                                               selected = "bothKnownRaw",
                                               inline = TRUE,
                                               width = "1000px"),
                                
                                  conditionalPanel(
                                    condition = "input.bothsigmaKnownRaw == 'bothKnownRaw'",
                                    
                                    numericInput(inputId = "popuSD1",
                                                 label = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                                                 value = 4.54, min = 0.00001, step = 0.00001),
                                    
                                    numericInput(inputId = "popuSD2",
                                                 label = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                                                 value = 3.47, min = 0.00001, step = 0.00001),
                                  )
                                )
                            ),
                            
                            conditionalPanel(
                              condition = "input.dataAvailability == 'Summarized Data' || input.dataAvailability == 'Enter Raw Data'",
                              
                              radioButtons(inputId = "inferenceType",
                                           label = strong("Inference Type"),
                                           choiceValues = list("Confidence Interval", "Hypothesis Testing"),
                                           choiceNames = list("Confidence Interval", "Hypothesis Testing"),
                                           selected = "Confidence Interval", # character(0), # 
                                           inline = TRUE,
                                           width = "1000px"),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Confidence Interval'",
                                
                                radioButtons(inputId = "confidenceLevel", label = strong("Confidence Level"), selected = c("95%"), choices = c("90%", "95%","99%"), inline = TRUE)
                              ),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Hypothesis Testing'",
                                
                                radioButtons(inputId = "significanceLevel", label = strong("Significance Level"), selected = c("5%"), choices = c("10%", "5%","1%"), inline = TRUE),
                              ),
                              
                              # Dropdown for 1-sample HT
                              
                              conditionalPanel(
                                condition = "input.samplesSelect == '1' && input.inferenceType == 'Hypothesis Testing'",
                                
                                numericInput(inputId = "hypMean",
                                             label = strong("Hypothesized Population Mean Value"),
                                             value = 99, step = 0.00001),
                                
                                selectizeInput(
                                  inputId = "altHypothesis",
                                  label = strong("Alternate Hypothesis"),
                                  choices = c(
                                    "< " = 1,
                                    "&ne; " = 2,
                                    "> " = 3
                                  ),
                                  selected = 2,
                                  options = list(
                                    render = I(render)
                                  )
                                )
                              )
                            ),
                            
                            # Dropdown for 2-sample HT
                            
                            conditionalPanel(
                              condition = "input.samplesSelect == '2' && input.inferenceType == 'Hypothesis Testing'",
                              
                              selectizeInput(
                                inputId = "altHypothesis",
                                label = strong("Alternate Hypothesis"),
                                choices = c(
                                  "< " = 1,
                                  "&ne; " = 2,
                                  "> " = 3
                                ),
                                selected = 2,
                                options = list(
                                  render = I(render)
                                )
                              )
                            ),
                        ),
                       
                       conditionalPanel(
                         condition = "input.popuParameter == 'Population Proportion'",

                         conditionalPanel(
                           condition = "input.samplesSelect == '1'",
                           
                           numericInput(inputId = "numSuccesses", 
                                        label = strong("Number of Successes (x)"),
                                        value = 1087, min = 0, step = 1),
                           
                           numericInput(inputId = "numTrails", 
                                        label = strong("Number of Trials (n)"),
                                        value = 1430, min = 1, step = 1),
                          ),
                         
                         conditionalPanel(
                           condition = "input.samplesSelect == '2'",
                         
                           numericInput(inputId = "numSuccesses", 
                                        label = strong("Number of Successes 1 (\\( x_{1}\\))"),
                                        value = 174, min = 0, step = 1),
                           
                           numericInput(inputId = "numTrails", 
                                        label = strong("Number of Trials 1 (\\( n_{1}\\))"),
                                        value = 300, min = 1, step = 1),
                           
                           numericInput(inputId = "numSuccesses", 
                                        label = strong("Number of Successes 2 (\\( x_{2}\\))"),
                                        value = 111, min = 0, step = 1),
                           
                           numericInput(inputId = "numTrails", 
                                        label = strong("Number of Trials 2 (\\( n_{2}\\))"),
                                        value = 300, min = 1, step = 1),
                         ),
                       ),
                       
                        actionButton(inputId = "goInference", label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetInference", label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ),
                      
                      conditionalPanel(
                        condition = "input.dropDownMenu == 'Regression and Correlation'",
                        
                        id = "RegCorPanel",
                        
                        # textAreaInput("x", label = strong("x (Independent Variable)"), value = "87, 92, 100, 103, 107, 110, 112, 127", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        # textAreaInput("y", label = strong("y (Dependent Variable)"), value = "39, 47, 60, 50, 60, 65, 115, 118", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                         
                        textAreaInput("x", label = strong("x (Independent Variable)"), value = "635, 644, 711, 708, 836, 820, 810, 870, 856, 923", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        textAreaInput("y", label = strong("y (Dependent Variable)"), value = "100, 93, 88, 84, 77, 75, 74, 63, 57, 55", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        
                        # textAreaInput("x", label = strong("x (Independent Variable)"), value = "61, 111, 125, 134, 169, 173, 244", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        # textAreaInput("y", label = strong("y (Dependent Variable)"), value = "4, 14, 15, 18, 21, 26, 38", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        
                        radioButtons(inputId = "regressioncorrelation", label = strong("Analyze Data Using"), selected = c("Simple Linear Regression"), choices = c("Simple Linear Regression", "Correlation Coefficient"), inline = TRUE),
                        
                        # conditionalPanel(
                        #   condition = "input.regressioncorrelation == 'Simple Linear Regression'",
                        #   
                        #   checkboxInput("scatterPlot", "Scatterplot of x versus y"),
                        # ),
                        
                        conditionalPanel(
                          condition = "input.regressioncorrelation == 'Correlation Coefficient'",
                          
                          checkboxInput("pearson", "Pearson's Product-Moment Correlation (r)"),
                          # checkboxInput("kendall", "Kendall's Rank Correlation (tau)"),
                          # checkboxInput("spearman", "Spearman's Rank Correlation (rho)"),
                        ),
                        
                        actionButton(inputId = "goRegression", label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetRegCor", label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ),
                      
                      # br(),
                      # br(),
                      # downloadButton('describe_download', "Download Report", class="butt" ), br(),
                      # tags$head(tags$style(".butt{background-color:#337ab7;} .butt{color:#fff;}")), br(),
                      # radioButtons('format', 'Document format', c('PDF', 'Word'), inline = TRUE),
                    ),
                    
                    mainPanel(
                      
                      tags$style(type ="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ), 
                      
                      div(id = "descriptiveStatsMP",
                          conditionalPanel(
                            condition = "input.dropDownMenu == 'Descriptive Statistics'",
                            
                            tableOutput("table"),
                            br(),
                            
                            plotOutput("boxplotHorizontal")
                          )
                      ),
                      
                      div(id = "probabilityMP",
                          conditionalPanel(
                            condition = "input.dropDownMenu == 'Probability Distributions'",
                            
                            conditionalPanel(
                              condition = "input.probability == 'Binomial'",
                              
                              uiOutput("renderProbabilityBinom"),
                              uiOutput("bVal")
                            ),
                            
                            conditionalPanel(
                              condition = "input.probability == 'Poisson'",
                              
                              uiOutput("renderProbabilityPoisson"),
                              uiOutput("pVal")
                            ),
                            
                            conditionalPanel(
                              condition = "input.probability == 'Normal'",
                              
                              uiOutput("renderProbabilityNorm"),
                              uiOutput("nVal")
                            )
                          )
                      ), 
                      
                      div(id = "inferenceMP",
                          conditionalPanel(
                            condition = "input.dropDownMenu == 'Statistical Inference'",
                            
                            uiOutput("renderInference"),
                            
                        conditionalPanel(
                          condition = "input.popuParameter == 'Population Mean'",
                              
                            conditionalPanel(
                              condition = "input.samplesSelect == '1'",
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Confidence Interval'",
                              
                                  conditionalPanel(
                                    condition = "input.dataAvailability == 'Summarized Data'",
                                
                                    uiOutput('oneSampCI'),
                                    
                                    br(),
                                    
                                    conditionalPanel(
                                      condition = "input.sigmaKnown == 'Known'",
                                      
                                      img(src ='OneSampZInt.png', height = '100px')
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.sigmaKnown == 'Unknown'",
                                      
                                      img(src ='OneSampTInt.png', height = '90px')
                                    )
                                  ),
                              
                                  conditionalPanel(
                                    condition = "input.dataAvailability == 'Enter Raw Data'",

                                    uiOutput('oneSampCIRaw'),
                                    
                                    br(),
                                    
                                    conditionalPanel(
                                      condition = "input.sigmaKnownRaw == 'rawKnown'",
                                      
                                      img(src ='OneSampZInt.png', height = '100px')
                                    ),
                                    
                                    conditionalPanel(
                                      condition = "input.sigmaKnownRaw == 'rawUnknown'",
                                      
                                      img(src ='OneSampTInt.png', height = '90px')
                                    )
                                  )
                              ),
                            
                              conditionalPanel(
                                condition = "input.inferenceType == 'Hypothesis Testing'",
                                
                                conditionalPanel(
                                  condition = "input.dataAvailability == 'Summarized Data'",
                                  
                                  uiOutput('oneSampHT')
                                ),
                                
                                conditionalPanel(
                                  condition = "input.dataAvailability == 'Enter Raw Data'",
                                  
                                  uiOutput('oneSampHTRaw')
                                )
                              )
                            ), # condition = "input.samplesSelect == '1'",
                            
                            conditionalPanel(
                              condition = "input.samplesSelect == '2'",

                              conditionalPanel(
                                condition = "input.inferenceType == 'Confidence Interval'",

                                conditionalPanel(
                                  condition = "input.dataAvailability == 'Summarized Data'",

                                  uiOutput('twoSampCI'),
                                  
                                  br(),
                                  
                                  conditionalPanel(
                                    condition = "input.bothsigmaKnown == 'bothKnown'",
                                    
                                    img(src ='TwoSampZInt.png', height = '75px')
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.bothsigmaKnown == 'bothUnknown'",
                                    
                                    img(src ='TwoSampTInt.png', height = '75px')
                                  )
                                ),

                                conditionalPanel(
                                  condition = "input.dataAvailability == 'Enter Raw Data'",

                                  conditionalPanel(
                                    condition = "input.samplesType == 'Independent Samples'",
                                    
                                        uiOutput('twoSampCIRaw'),
                                        
                                        br(),
                                        
                                        conditionalPanel(
                                          condition = "input.bothsigmaKnownRaw == 'bothKnownRaw'",
                                          
                                          img(src ='TwoSampZInt.png', height = '75px')
                                        ),
                                        
                                        conditionalPanel(
                                          condition = "input.bothsigmaKnownRaw == 'bothUnknownRaw'",
                                          
                                          img(src ='TwoSampTInt.png', height = '75px')
                                        )
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.samplesType == 'Dependent Samples'",
                                    
                                      img(src ='TwoSampTIntPaired.png', height = '100px')
                                  )
                                )
                              ),

                              conditionalPanel(
                                condition = "input.inferenceType == 'Hypothesis Testing'",

                                conditionalPanel(
                                  condition = "input.dataAvailability == 'Summarized Data'",

                                  uiOutput('twoSampHT'),
                                  br(),
                                ),

                                conditionalPanel(
                                  condition = "input.dataAvailability == 'Enter Raw Data'",

                                  uiOutput('twoSampHTRaw'),
                                  br(),
                                )
                              )
                            ) # condition = "input.samplesSelect == '2'"
                          ) # condition = "input.popuParameter == 'Population Mean'"
                        ) # input.dropDownMenu == 'Statistical Inference'
                      ), # inferenceMP
                      
                      div(id = "RegCorMP",

                          conditionalPanel(
                            condition = "input.regressioncorrelation == 'Simple Linear Regression'",

                            tabsetPanel(id = 'tabSet',
                                tabPanel(id = "SLR", title = "Simple Linear Regression",
                                     plotOutput("scatterplot", width = "500px"),
                                     br(),
                                     
                                     verbatimTextOutput("linearRegression"),
                                     br(),
                                     
                                     verbatimTextOutput("confintLinReg"),
                                     br(),
                                     
                                     verbatimTextOutput("anovaLinReg"),
                                     #br(),
                                 ),
                                 
                                tabPanel(id = "normality", title = "Normality of Residuals",
                                         #----------------------------------#
                                         # Tests for normality of residuals #
                                         #----------------------------------#
                                         
                                         verbatimTextOutput("AndersonDarlingTest"),
                                         br(),
                                         
                                         verbatimTextOutput("KolmogorovSmirnovTest"),
                                         br(),
                                         
                                         verbatimTextOutput("ShapiroTest"),
                                         #br(),
                                ),
                                
                                 tabPanel(id = "resid", title = "Residual Plots",
                                    #-----------------------------#
                                    # Plots for Residual Analysis #
                                    #-----------------------------#
                                    
                                    plotOutput("qqplot", width = "500px"),
                                    #br(),
                                    
                                    plotOutput("moreplots", width = "500px"),
                                    #br(),
                                ),
                            
                                #selected ="SLR"
                            # verbatimTextOutput("outlierTest"),
                          ),
                          
                          conditionalPanel(
                            condition = "input.regressioncorrelation == 'Correlation Coefficient'",

                            conditionalPanel(
                              condition = "input.pearson == 1",
                              
                              verbatimTextOutput("PearsonEstimate"),
                              br(),
                              
                              verbatimTextOutput("PearsonCorTest"),
                              br(),
                              
                              verbatimTextOutput("PearsonConfInt"),
                            ),
                            
                            # conditionalPanel(
                            #   condition = "input.kendall == 1",
                            #   
                            #   verbatimTextOutput("Kendall"),
                            # ),
                            # 
                            # conditionalPanel(
                            #   condition = "input.spearman == 1",
                            #   
                            #   verbatimTextOutput("Spearman"),
                            # ),
                          ),
                        )
                      ) # RegCorMP
                    ) # mainPanel
                  ), # sidebarLayout
                ), # Methods Panel
                  
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
                           
                           p(span("Lead Developer", style = "font-weight:bold")),
                           p("Crystal Wai,"), 
                           p("Undergraduate Student,"), 
                           p("Mount Royal University,"), 
                           p("Calgary, AB, CANADA"), 
                           
                           br(), 
                           
                           p("Email:",a("akrishnamurthy@mtroyal.ca", href = "mailto:akrishnamurthy@mtroyal.ca")), 
                           p("Website:", a(href = "https://bit.ly/2YKrXjX","https://bit.ly/2YKrXjX", target = "_blank")),
                           p("GitHub:", a(href = "https://github.com/ashokkrish/COMP5690","https://github.com/ashokkrish/COMP5690", target = "_blank")),
                           br(),
                           
                           p("In Fall 2022 an earlier version of this interactive Shiny app was presented as Crystal Wai's COMP 5690 Senior Computer Science Project"), 
                           br(),

                           p("This interactive R Shiny app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback."),
                           
                           hr(),

                           h5("Built with",
                              img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "35px"),
                              "by",
                              img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo.png", height = "35px"),
                              ".")
                  )
        )
  )
  
server <- function(input, output) {
    
    # Data validation
    iv <- InputValidator$new()
    
    #descriptiveStat 
    
    iv$add_rule("descriptiveStat", sv_required())
    
    #numTrailsBinom
    
    iv$add_rule("numTrailsBinom", sv_required())
    iv$add_rule("numTrailsBinom", sv_integer())
    iv$add_rule("numTrailsBinom", sv_gt(0))
    
    # successProbBinom
    
    iv$add_rule("successProbBinom", sv_required())
    iv$add_rule("successProbBinom", sv_gte(0))
    iv$add_rule("successProbBinom", sv_lte(1))
    
    #numSuccessesBinom
    
    iv$add_rule("numSuccessesBinom", sv_required())
    iv$add_rule("numSuccessesBinom", sv_integer())
    iv$add_rule("numSuccessesBinom", sv_gte(0))
    
    #muPoisson
    
    iv$add_rule("muPoisson", sv_required())
    iv$add_rule("muPoisson", sv_gt(0))
    
    #xPoisson 
    
    iv$add_rule("xPoisson", sv_required())
    iv$add_rule("xPoisson", sv_integer())
    iv$add_rule("xPoisson", sv_gte(0))
    
    #popMean 
    
    iv$add_rule("popMean", sv_required())
    
    #popuSD
    
    iv$add_rule("popSD", sv_required())
    iv$add_rule("popSD", sv_gt(0))
    
    #xValue 
    
    iv$add_rule("xValue", sv_required())
    
    #sampleSize
    
    iv$add_rule("sampleSize", sv_required())
    iv$add_rule("sampleSize", sv_gt(0))
    
    #sampleMean 
    
    iv$add_rule("sampleMean", sv_required())
    
    #popuSD
    
    iv$add_rule("popuSD", sv_required()) 
    iv$add_rule("popuSD", sv_gt(0))
    
    #popuSDRaw
    
    iv$add_rule("popuSDRaw", sv_required()) 
    iv$add_rule("popuSDRaw", sv_gt(0))
    
    #sampSD 
    
    iv$add_rule("sampSD", sv_required())
    iv$add_rule("sampSD", sv_gt(0))
    
    #sampleSize1 
    
    iv$add_rule("sampleSize1", sv_required())
    iv$add_rule("sampleSize1", sv_integer())
    iv$add_rule("sampleSize1", sv_gt(0))
    
    #sampleMean1 
    
    iv$add_rule("sampleMean1", sv_required())
    
    #sampleSize2 
    
    iv$add_rule("sampleSize2", sv_required())
    iv$add_rule("sampleSize2", sv_integer())
    iv$add_rule("sampleSize2", sv_gt(0))
    
    #sampleMean2 
    
    iv$add_rule("sampleMean2", sv_required()) 
    
    #popuSD1 
    
    iv$add_rule("popuSD1", sv_required()) 
    iv$add_rule("popuSD1", sv_gt(0))
    
    #popuSD2 
    
    iv$add_rule("popuSD2", sv_required()) 
    iv$add_rule("popuSD2", sv_gt(0))
    
    #sampSD1 
    
    iv$add_rule("sampSD1", sv_required())
    iv$add_rule("sampSD1", sv_gt(0))
    
    #sampSD2 
    
    iv$add_rule("sampSD2", sv_required()) 
    iv$add_rule("sampSD2", sv_gt(0))
    
    #sample1 
    
    iv$add_rule("sample1", sv_required()) 
    
    #raw_sample1
    
    iv$add_rule("raw_sample1", sv_required())
    
    #raw_sample2 
    
    iv$add_rule("raw_sample2", sv_required()) 
    
    #hypMean 
    
    iv$add_rule("hypMean", sv_required())
    
    #Regression and Correlation 
    
    iv$add_rule("x", sv_required())
    iv$add_rule("y", sv_required())
    
    iv$enable()
    
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
    
    observeEvent(input$goDescpStats, {
      dat <- createNumLst(input$descriptiveStat)
      
      if(anyNA(dat) | length(dat)<2){
        "Invalid input or not enough observations"
      }
      else{
        dat <- createNumLst(input$descriptiveStat)
        #print(dat)
        values <- reactiveValues()
        values$df <- data.frame(Variable = character(), Value = character())
        output$table <- renderTable(values$df)
        row1 <- data.frame(Variable = "Sample Size", Value = paste0(length(dat)))
        row2 <- data.frame(Variable = "Sum", Value = paste0(sum(dat)))
        row3 <- data.frame(Variable = "Mean", Value = paste0(round(mean(dat),4)))
        row4 <- data.frame(Variable = "Mode", Value = paste(Modes(dat)))
        row5 <- data.frame(Variable = "Q1", Value = paste0(quantile(dat, 0.25)))
        row6 <- data.frame(Variable = "Median (Q2)", Value = paste0(median(dat)))
        row7 <- data.frame(Variable = "Q3", Value = paste0(quantile(dat, 0.75)))
        row8 <- data.frame(Variable = "Minimum", Value = paste0(min(dat)))
        row9 <- data.frame(Variable = "Maximum", Value = paste0(max(dat)))
        row10 <- data.frame(Variable = "Interquartile range (IQR)", Value = paste0(IQR(dat)))
        row11 <- data.frame(Variable = "Range", Value = paste0(range(dat)[2]-range(dat)[1]))
        row12 <- data.frame(Variable = "Sample Standard Deviation", Value = paste0(round(sd(dat),4)))
        row13 <- data.frame(Variable = "Sample Variance", Value = paste0(round(var(dat),4)))
        row14 <- data.frame(Variable = "Standard Error of the Mean", Value = paste0(round(sd(dat)/sqrt(length(dat)),4)))
        row15 <- data.frame(Variable = "Check for Outliers: Lower Fence", Value = paste(quantile(dat, 0.25) - (1.5*IQR(dat))))
        row16 <- data.frame(Variable = "Check for Outliers: Upper Fence", Value = paste(quantile(dat, 0.75) + (1.5*IQR(dat))))
        row17 <- data.frame(Variable = "Number of Outliers", Value = paste("In progress"))
        row18 <- data.frame(Variable = "Skewness", Value = paste0(round(skewness(dat),4)))
        row19 <- data.frame(Variable = "Kurtosis", Value = paste0(round(kurtosis(dat),4)))
        
        values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12, row13, row14, row15, row16, row17, row18, row19)
        
        output$boxplotHorizontal <- renderPlot({
          
          #-------------------
          # Horizontal boxplot
          #-------------------
          
          boxplot(dat, horizontal = TRUE)
          
          ## Add mean line
          # segments(x0 = mean(dat), y0 = 0.8,
          #          x1 = mean(dat), y1 = 1.2,
          #          col = "red", lwd = 2)
          # 
          # points(mean(dat), col = 3, pch = 19)
          
          #-------------------
          # ggplot2 boxplot
          #-------------------
          
          # ggplot(as.data.frame(dat), aes(x = "", y = dat)) +    
          # geom_boxplot(show.legend = FALSE)
        })
      }
    })
    
    observeEvent(input$goBinom, {
      output$renderProbabilityBinom <- renderUI({
        binom_n <- input$numTrailsBinom
        binom_p <- input$successProbBinom
        binom_x <- input$numSuccessesBinom
        
        binomValues <- reactive({
          
          req(input$numTrailsBinom, input$successProbBinom, input$numSuccessesBinom)
          
          validate(
            need(binom_n != "", "Enter a value for the number of trials (n)"),
            need(binom_p != "", "Enter a value for the probability of successes (p)"),
            need(binom_x != "", "Enter a value for the number of successes (x)"),
            need(binom_n > 0, "n must be a positive integer"),
            need(binom_p > 0, "p must be > 0"),
            need(binom_p < 1, "p must be < 1"),
            need(binom_x >= 0, "x must be a positve integer"),
            need(binom_x <= binom_n, "Number of successes(x) must be less than or equal to the number of trials(n)")
          )
        })
        
        output$bVal <- renderTable({
          if(input$probability == 'Binomial') {head(binomValues())}
        })
        
        if(input$numSuccessesBinom <= input$numTrailsBinom && input$numSuccessesBinom >= 0 && input$successProbBinom < 1 && input$successProbBinom > 0 && input$numTrailsBinom > 0){
            if(input$calcBinom == 'exact'){
              withMathJax(paste0("\\(P(X = \\)"," ",binom_x,"\\()\\)"," ","\\( = \\)"," ", round(dbinom(binom_x,binom_n,binom_p),4)))
            }
            else if(input$calcBinom == 'cumulative'){
              withMathJax(paste0("\\(P(X \\leq \\)"," ",binom_x,"\\()\\)"," ","\\( = \\)"," ", round(pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE),4)))
            }
            else if(input$calcBinom == 'upperTail'){
              withMathJax(paste0("\\(P(X > \\)"," ",binom_x,"\\()\\)"," ","\\( = \\)"," ", round(pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE),4)))
            }
        }
      })
    })
    
    observeEvent(input$goPoisson, {
      output$renderProbabilityPoisson <- renderUI({
        poisson_mu <- input$muPoisson
        poisson_x <- input$xPoisson 
        # aP <- input$aPoisson
        # bP <- input$bPoisson
        
        poissonValues <- reactive({
        req(input$muPoisson, input$xPoisson)

          validate(
            need(poisson_mu > 0, "Average must be a positive value"),
            need(poisson_x != "", "Enter a value for the number of successes (x)")
          )
        })
        
        output$pVal <- renderTable({
          if(input$probability == 'Poisson') {head(poissonValues())}
        })
        
        if(input$muPoisson > 0 && !is.na(input$muPoisson)){
          if(input$calcPoisson == "exact"){
            withMathJax(paste0("\\(P(X = \\)", " ", poisson_x, "\\()\\)", " ", "\\( = \\)", " ", round(dpois(poisson_x,poisson_mu),4)))
          }
          else if(input$calcPoisson == "cumulative"){
            withMathJax(paste0("\\(P(X \\leq \\)", " ", poisson_x, "\\()\\)", " ", "\\( = \\)", " ", round(ppois(poisson_x,poisson_mu,lower.tail = TRUE),4)))
          }
          else if(input$calcPoisson == "upperTail"){
            withMathJax(paste0(paste0("\\(P(X > \\)", " ", poisson_x, "\\()\\)", " " ,"\\(= \\)", " ", round(ppois(poisson_x, poisson_mu, lower.tail = FALSE), 4))))
          }
        }
        # else if(input$calcPoisson == "interval" && input$muPoisson > 0){
        #   withMathJax(
        #     paste0("\\(P(\\)",aP," ", "\\(\\leq X\\leq \\)", " ", bP, "\\()\\)"," ", "\\( = \\)", " ", ifelse(input$aP > input$bP, "a must be less than or equal to b", round(ppois(input$bP, poisson_mu, lower.tail = TRUE) - ppois(input$aP - 1, poisson_mu, lower.tail = TRUE), 4)))   
        #   )
        # }
      })
    })
    
    observeEvent(input$goNormal, {
      output$renderProbabilityNorm <- renderUI({
        normMean <- input$popMean
        normSd <- input$popSD
        normX <- input$xValue
        
        normValues <- reactive({
          validate(
            need(input$popSD > 0, "Standard Deviation must be positive")
          )
        })
        
        output$nVal <- renderTable({
          if(input$probability == 'Normal') {head(normValues())}
        })
        
        if(input$popSD > 0 && !is.na(input$popSD)){
          if(input$calcNormal == "cumulative"){
            withMathJax(paste0("\\(P(X \\leq \\)", " ", normX, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(normX, normMean, normSd, lower.tail = TRUE),4)))
          }
          else if(input$calcNormal == "P(X > x)"){
            withMathJax(paste0("\\(P(X > \\)", " ", normX, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(normX, normMean, normSd, lower.tail = FALSE),4)))
          }
        }
      })
    })
    
    observeEvent(input$goInference, {
      output$renderInference <- renderUI(
        
      if(input$popuParameter == 'Population Mean'){
          
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
            else if(input$altHypothesis == "1"){
              alternative <- "less"
            }
          }
          
          if(input$dataAvailability == 'Summarized Data'){
            
            nSampOne <- input$sampleSize
            xbarSampOne <- input$sampleMean
            
            if(input$inferenceType == 'Confidence Interval'){
              
              if(input$sigmaKnown == 'Known'){

                sigmaSampOne <- input$popuSD
                
                source('R/OneSampZInt.R')
                
                print("Confidence Inrerval for One Population Mean when Population Standard Deviation is known")
                
                zIntPrint <- ZInterval(nSampOne, xbarSampOne, sigmaSampOne, ConfLvl)
                
                values <- reactiveValues()
                values$dfKnown <- data.frame(Variable = character(), Value = character())
                output$oneSampCI <- renderTable(values$dfKnown)
                
                row1 <- data.frame(Variable = "Sample Mean", Value = paste(zIntPrint[1]))
                row2 <- data.frame(Variable = "Z Critical Value", Value = paste(zIntPrint[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(zIntPrint[3]))
                row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(zIntPrint[4]))
                row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(zIntPrint[5]))
                
                values$dfKnown <- rbind(row1, row2, row3, row4, row5)
              }
              
              else if(input$sigmaKnown == 'Unknown'){
                
                sSampOne <- input$sampSD
                
                source('R/OneSampTInt.R')
                
                print("Confidence Inrerval for One Population Mean when Population Standard Deviation is unknown")
                
                tIntPrint <- TInterval(nSampOne, xbarSampOne, sSampOne, ConfLvl)
                
                values <- reactiveValues()
                values$dfUnknown <- data.frame(Variable = character(), Value = character())
                output$oneSampCI <- renderTable(values$dfUnknown)
                
                row1 <- data.frame(Variable = "Sample Mean", Value = paste(tIntPrint[1]))
                row2 <- data.frame(Variable = "T Critical Value", Value = paste(tIntPrint[2]))
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
                
                zTestPrint <- ZTest(nSampOne, xbarSampOne, sigmaSampOne, hypMeanSampOne, alternative, sigLvl)
                
                values <- reactiveValues()
                values$dfKnownHyp <- data.frame(Variable = character(), Value = character())
                output$oneSampHT <- renderTable(values$dfKnownHyp)
                
                row1 <- data.frame(Variable = "Sample Size", Value = paste(zTestPrint[1]))
                row2 <- data.frame(Variable = "Sample Mean", Value = paste(zTestPrint[2]))
                row3 <- data.frame(Variable = "Population SD", Value = paste(zTestPrint[3]))
                row4 <- data.frame(Variable = "Z Critical Value", Value = paste(zTestPrint[4]))
                row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(zTestPrint[5]))
                row6 <- data.frame(Variable = "Test Statistic", Value = paste(zTestPrint[6]))
                row7 <- data.frame(Variable = "P-Value", Value = paste(zTestPrint[7]))
                
                values$dfKnownHyp <- rbind(row1, row2, row3, row4, row5, row6, row7) 
              }
              
              else if(input$sigmaKnown == 'Unknown'){
                
                sSampOne <- input$sampSD
                
                source("R/OneSampTTest.R")
                
                tTestPrint <- TTest(nSampOne, xbarSampOne, sSampOne, hypMeanSampOne, alternative, sigLvl)
                
                values <- reactiveValues()
                values$dfUnKnownHyp <- data.frame(Variable = character(), Value = character())
                output$oneSampHT <- renderTable(values$dfUnKnownHyp)
                
                row1 <- data.frame(Variable = "Sample Size", Value = paste(tTestPrint[1]))
                row2 <- data.frame(Variable = "Sample Mean", Value = paste(tTestPrint[2]))
                row3 <- data.frame(Variable = "Sample SD", Value = paste(tTestPrint[3]))
                row4 <- data.frame(Variable = "T Critical Value", Value = paste(tTestPrint[4]))
                row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(tTestPrint[5]))
                row6 <- data.frame(Variable = "Test Statistic", Value = paste(tTestPrint[6]))
                row7 <- data.frame(Variable = "P-Value", Value = paste(tTestPrint[7]))
                
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
                
                zIntPrintRaw <- ZInterval(rawSampleSize, rawSampleMean, rawPopuSD, ConfLvl)
                
                values <- reactiveValues()
                values$dfKnownRaw <- data.frame(Variable = character(), Value = character())
                output$oneSampCIRaw <- renderTable(values$dfKnownRaw)
                
                row1 <- data.frame(Variable = "Sample Mean", Value = paste(zIntPrintRaw[1]))
                row2 <- data.frame(Variable = "Z Critical Value", Value = paste(zIntPrintRaw[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(zIntPrintRaw[3]))
                row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(zIntPrintRaw[4]))
                row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(zIntPrintRaw[5]))
                
                values$dfKnownRaw <- rbind(row1, row2, row3, row4, row5)
              }
              
              else if(input$sigmaKnownRaw == 'rawUnknown'){
              
                rawSampleSD <- sd(datRawData)
                
                source("R/OneSampTInt.R")
                
                tIntPrintRaw <- TInterval(rawSampleSize, rawSampleMean, rawSampleSD, ConfLvl)
                
                values <- reactiveValues()
                values$dfUnknownRaw <- data.frame(Variable = character(), Value = character())
                output$oneSampCIRaw <- renderTable(values$dfUnknownRaw)
                
                row1 <- data.frame(Variable = "Sample Mean", Value = paste(tIntPrintRaw[1]))
                row2 <- data.frame(Variable = "T Critical Value", Value = paste(tIntPrintRaw[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(tIntPrintRaw[3]))
                row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(tIntPrintRaw[4]))
                row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(tIntPrintRaw[5]))
                
                values$dfUnknownRaw <- rbind(row1, row2, row3, row4, row5) 
              } # input$sigmaKnownRaw == 'rawUnknown'
            } # input$inferenceType == 'Confidence Interval'
            
            else if(input$inferenceType == 'Hypothesis Testing'){
              
              hypMeanSampOne <- input$hypMean 
              
              if(input$sigmaKnownRaw == 'rawKnown'){
                
                rawPopuSD <- input$popuSDRaw
                
                source("R/OneSampZTest.R")
                
                zTestPrint <- ZTest(rawSampleSize, rawSampleMean, rawPopuSD, hypMeanSampOne, alternative, sigLvl)

                values <- reactiveValues()
                values$dfKnownHypRaw <- data.frame(Variable = character(), Value = character())
                output$oneSampHTRaw <- renderTable(values$dfKnownHypRaw)
                
                row1 <- data.frame(Variable = "Sample Size", Value = paste(zTestPrint[1]))
                row2 <- data.frame(Variable = "Sample Mean", Value = paste(zTestPrint[2]))
                row3 <- data.frame(Variable = "Population SD", Value = paste(zTestPrint[3]))
                row4 <- data.frame(Variable = "Z Critical Value", Value = paste(zTestPrint[4]))
                row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(zTestPrint[5]))
                row6 <- data.frame(Variable = "Test Statistic", Value = paste(zTestPrint[6]))
                row7 <- data.frame(Variable = "P-Value", Value = paste(zTestPrint[7]))
  
                values$dfKnownHypRaw <- rbind(row1, row2, row3, row4, row5, row6, row7) 
              }
              
              else if(input$sigmaKnownRaw == 'rawUnknown'){
                
                rawSampleSD <- sd(datRawData)

                source("R/OneSampTTest.R")
                
                tTestPrint <- TTest(rawSampleSize, rawSampleMean, rawSampleSD, hypMeanSampOne, alternative, sigLvl)
                
                values <- reactiveValues()
                values$dfUnKnownHypRaw <- data.frame(Variable = character(), Value = character())
                output$oneSampHTRaw <- renderTable(values$dfUnKnownHypRaw)
                
                row1 <- data.frame(Variable = "Sample Size", Value = paste(tTestPrint[1]))
                row2 <- data.frame(Variable = "Sample Mean", Value = paste(tTestPrint[2]))
                row3 <- data.frame(Variable = "Sample SD", Value = paste(tTestPrint[3]))
                row4 <- data.frame(Variable = "T Critical Value", Value = paste(tTestPrint[4]))
                row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(tTestPrint[5]))
                row6 <- data.frame(Variable = "Test Statistic", Value = paste(tTestPrint[6]))
                row7 <- data.frame(Variable = "P-Value", Value = paste(tTestPrint[7]))
                
                values$dfUnKnownHypRaw <- rbind(row1, row2, row3, row4, row5, row6, row7) 
              } # input$sigmaKnownRaw == 'rawUnknown'
            } # input$inferenceType == 'Hypothesis Testing'
          } # input$dataAvailability == 'Enter Raw Data'
        } # samplesSelect == '1'
        
        else if(input$samplesSelect == '2'){
          
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
            else if(input$altHypothesis == "1"){
              alternative <- "less"
            }
          }
          
          if(input$dataAvailability == 'Summarized Data'){

            n1 <- input$sampleSize1
            xbar1 <- input$sampleMean1

            n2 <- input$sampleSize2
            xbar2 <- input$sampleMean2

            if(input$inferenceType == 'Confidence Interval'){

              if(input$bothsigmaKnown == 'bothKnown'){

                sigma1 <- input$popuSD1
                sigma2 <- input$popuSD2

                source('R/TwoSampZInt.R')

                twoSampKnownConfid <- TwoSampZInt(xbar1, sigma1, n1, xbar2, sigma2, n2, ConfLvl)
                
                values <- reactiveValues()
                values$dfTwoKnownSum <- data.frame(Variable = character(), Value = character())
                output$twoSampCI <- renderTable(values$dfTwoKnownSum)
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampKnownConfid[1]))
                row2 <- data.frame(Variable = "Z Critical Value", Value = paste(twoSampKnownConfid[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampKnownConfid[3]))
                row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(twoSampKnownConfid[4]))
                row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(twoSampKnownConfid[5]))
                
                values$dfTwoKnownSum <- rbind(row1, row2, row3, row4, row5)
              }
              
              else if(input$bothsigmaKnown == 'bothUnknown'){

                s1 <- input$sampSD1
                s2 <- input$sampSD2

                source('R/TwoSampTInt.R')

                twoSampUnKnownConfid <- TwoSampTInt(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, ConfLvl)
                
                values <- reactiveValues()
                values$dfTwoUnknownSum <- data.frame(Variable = character(), Value = character())
                output$twoSampCI <- renderTable(values$dfTwoUnknownSum )
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampUnKnownConfid[1]))
                row2 <- data.frame(Variable = "T Critical Value", Value = paste(twoSampUnKnownConfid[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampUnKnownConfid[3]))
                row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(twoSampUnKnownConfid[4]))
                row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(twoSampUnKnownConfid[5]))
                
                values$dfTwoUnknownSum  <- rbind(row1, row2, row3, row4, row5)
              }
            }

            else if(input$inferenceType == 'Hypothesis Testing'){

              if(input$bothsigmaKnown == 'bothKnown'){

                sigma1 <- input$popuSD1
                sigma2 <- input$popuSD2

                source('R/TwoSampZTest.R')

                twoSampZTestHyp <- TwoSampZTest(xbar1, sigma1, n1, xbar2, sigma2, n2, alternative, sigLvl)

                values <- reactiveValues()
                values$dfTwoKnownHyp <- data.frame(Variable = character(), Value = character())
                output$twoSampHT <- renderTable(values$dfTwoKnownHyp )
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampZTestHyp[1]))
                row2 <- data.frame(Variable = "Z Critical Value", Value = paste(twoSampZTestHyp[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampZTestHyp[3]))
                row4 <- data.frame(Variable = "Test Statistic", Value = paste(twoSampZTestHyp[4]))
                row5 <- data.frame(Variable = "P-Value", Value = paste(twoSampZTestHyp[5]))
                
                values$dfTwoKnownHyp  <- rbind(row1, row2, row3, row4, row5)
              }
              
              else if(input$bothsigmaKnown == 'bothUnknown'){

                s1 <- input$sampSD1
                s2 <- input$sampSD2

                source('R/TwoSampTTest.R')

                twoSampTTestHyp <- TwoSampTTest(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, alternative, sigLvl)

                values <- reactiveValues()
                values$dfTwoUnknownHyp <- data.frame(Variable = character(), Value = character())
                output$twoSampHT <- renderTable(values$dfTwoUnknownHyp )
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampTTestHyp[1]))
                row2 <- data.frame(Variable = "Degrees of freedom (df)", Value = paste(twoSampTTestHyp[2]))
                row3 <- data.frame(Variable = "T Critical Value", Value = paste(twoSampTTestHyp[3]))
                row4 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampTTestHyp[4]))
                row5 <- data.frame(Variable = "Test Statistic", Value = paste(twoSampTTestHyp[5]))
                row6 <- data.frame(Variable = "P-Value", Value = paste(twoSampTTestHyp[6]))
                
                values$dfTwoUnknownHyp  <- rbind(row1, row2, row3, row4, row5, row6)
              }
            }
          }

          else if(input$dataAvailability == 'Enter Raw Data'){

            raw_sample1 <- createNumLst(input$raw_sample1)

            n1  <- length(raw_sample1)
            xbar1 <- mean(raw_sample1)

            raw_sample2 <- createNumLst(input$raw_sample2)

            n2  <- length(raw_sample2)
            xbar2 <- mean(raw_sample2)

            if(input$inferenceType == 'Confidence Interval'){

              if(input$bothsigmaKnown == 'bothKnown'){

                sigma1 <- input$popuSD1
                sigma2 <- input$popuSD2

                source('R/TwoSampZInt.R')

                twoSampZIntRaw <- TwoSampZInt(xbar1, sigma1, n1, xbar2, sigma2, n2, ConfLvl)
                
                values <- reactiveValues()
                values$dfTwoKnownCIRaw <- data.frame(Variable = character(), Value = character())
                output$twoSampCIRaw <- renderTable(values$dfTwoKnownCIRaw)
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampZIntRaw[1]))
                row2 <- data.frame(Variable = "Z Critical Value", Value = paste(twoSampZIntRaw[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampZIntRaw[3]))
                row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(twoSampZIntRaw[4]))
                row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(twoSampZIntRaw[5]))
                
                values$dfTwoKnownCIRaw <- rbind(row1, row2, row3, row4, row5)
              }
              
              else if(input$bothsigmaKnown == 'bothUnknown'){

                s1 <- sd(raw_sample1)
                s2 <- sd(raw_sample2)

                source('R/TwoSampTInt.R')

                twoSampTIntRaw <- TwoSampTInt(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, ConfLvl)
                
                values <- reactiveValues()
                values$dfTwoUnknownCIRaw <- data.frame(Variable = character(), Value = character())
                output$twoSampCIRaw <- renderTable(values$dfTwoUnknownCIRaw)
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampTIntRaw[1]))
                row2 <- data.frame(Variable = "T Critical Value", Value = paste(twoSampTIntRaw[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampTIntRaw[3]))
                row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(twoSampTIntRaw[4]))
                row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(twoSampTIntRaw[5]))
                
                values$dfTwoUnknownCIRaw  <- rbind(row1, row2, row3, row4, row5)
              }
            }

            else if(input$inferenceType == 'Hypothesis Testing'){

              if(input$bothsigmaKnown == 'bothKnown'){

                sigma1 <- input$popuSD1
                sigma2 <- input$popuSD2

                source('R/TwoSampZTest.R')

                twoSampZTestRaw <- TwoSampZTest(xbar1, sigma1, n1, xbar2, sigma2, n2, alternative, sigLvl)
                
                values <- reactiveValues()
                values$dfTwoKnownHypRaw <- data.frame(Variable = character(), Value = character())
                output$twoSampHTRaw <- renderTable(values$dfTwoKnownHypRaw )
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampZTestRaw[1]))
                row2 <- data.frame(Variable = "Z Critical Value", Value = paste(twoSampZTestRaw[2]))
                row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampZTestRaw[3]))
                row4 <- data.frame(Variable = "Test Statistic", Value = paste(twoSampZTestRaw[4]))
                row5 <- data.frame(Variable = "P-Value", Value = paste(twoSampZTestRaw[5]))
                
                values$dfTwoKnownHypRaw  <- rbind(row1, row2, row3, row4, row5)
              }
              
              else if(input$bothsigmaKnown == 'bothUnknown'){

                s1 <- sd(raw_sample1)
                s2 <- sd(raw_sample2)

                source('R/TwoSampTTest.R')

                twoSampTTestRaw <- TwoSampTTest(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, alternative, sigLvl)
                
                values <- reactiveValues()
                values$dfTwoUnknownHypRaw <- data.frame(Variable = character(), Value = character())
                output$twoSampHTRaw <- renderTable(values$dfTwoUnknownHypRaw)
                
                row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(twoSampTTestRaw[1]))
                row2 <- data.frame(Variable = "Degrees of freedom (df)", Value = paste(twoSampTTestRaw[2]))
                row3 <- data.frame(Variable = "T Critical Value", Value = paste(twoSampTTestRaw[3]))
                row4 <- data.frame(Variable = "Standard Error (SE)", Value = paste(twoSampTTestRaw[4]))
                row5 <- data.frame(Variable = "Test Statistic", Value = paste(twoSampTTestRaw[5]))
                row6 <- data.frame(Variable = "P-Value", Value = paste(twoSampTTestRaw[6]))
                
                values$dfTwoUnknownHypRaw <- rbind(row1, row2, row3, row4, row5, row6)
              } # input$bothsigmaKnown == 'bothUnknown'
            } # input$inferenceType == 'Hypothesis Testing'
          } # input$dataAvailability == 'Enter Raw Data'
        } # samplesSelect == '2'
       } # input$popuParameter == 'Population Mean'
      
      else if(input$popuParameter == 'Population Proportion'){
        
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
          else if(input$altHypothesis == "1"){
            alternative <- "less"
          }
        }
        
        if(input$samplesSelect == '1'){
          source('R/OnePropZInt.R')
          source('R/OnePropZTest.R')
          print("Inference for One Population Proportion")
        }
        else if(input$samplesSelect == '2'){
          source('R/TwoPropZInt.R')
          source('R/TwoPropZTest.R')
          print("Inference for the difference between two Population Proportions")
        }
      }
     ) # renderInference
    }) # input$goInference
    
    #------------------------------------------#
    # Simple Linear Regression and Correlation #
    #------------------------------------------#
    
    observeEvent(input$goRegression, {
      
      datx <- createNumLst(input$x)
      daty <- createNumLst(input$y)
      
      if(anyNA(datx) | length(datx)<2 | anyNA(daty) | length(daty)<2){
        # output$linearRegression <- renderPrint({ 
        # "Invalid input or not enough observations"
        #   })
        print("Invalid input or not enough observations")
      }
      else{
          if(input$regressioncorrelation == "Simple Linear Regression")
          {
            model <- lm(daty ~ datx)

          output$scatterplot <- renderPlot({
            plot(datx, daty, main = "Scatter Plot", xlab = "Independent Variable, x", ylab = "Dependent Variable, y", pch = 19) +
              abline(lm(daty ~ datx), col = "blue")
          })
            
          output$linearRegression <- renderPrint({ 
            summary(model)
          })
          
          output$confintLinReg <- renderPrint({ 
            confint(model) # Prints the 95% confidence interval for the regression parameters
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
            par(mfrow = c(2,2))
            plot(model, which=1:4, pch = 19)
          })
          
          # output$outlierTest <- renderPrint({ 
          #   outlierTest(model) # Prints the Bonferonni p-value for the most extreme observations
          # })
          
          # output$residversusfittedlot <- renderPlot({
          #   #plot(fitted(reg.model), resid(reg.model), pch = 19, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
          #   #abline(h = 0, col = "red")
          #   #leveragePlots(model) # leverage plots
          # })
        }

        else if(input$regressioncorrelation == "Correlation Coefficient")
        {
          output$PearsonEstimate <- renderPrint({ 
            cor.test(datx, daty, method = "pearson")$estimate
          })
          
          output$PearsonCorTest <- renderPrint({ 
            cor.test(datx, daty, method = "pearson")
          })

          output$PearsonConfInt <- renderPrint({ 
            cor.test(datx, daty, method = "pearson")$conf.int
          })
          # output$Kendall <- renderPrint({ 
          #   cor.test(datx, daty, method = "kendall")$estimate
          # })
          # 
          # output$Spearman <- renderPrint({ 
          #   cor.test(datx, daty, method = "spearman")$estimate
          # })
        } # Correlation
        
        df <- data.frame(datx, daty, datx*daty, datx^2, daty^2)
        names(df) <- c("X", "Y", "XY", "X^2", "Y^2")
        print(df)
      }
    }) # input$goRegression

    #------------------------#
    # Descriptive Statistics #
    #------------------------#
    
    observeEvent(input$goDescpStats, {
      show(id = 'descriptiveStatsMP')
    })
    
    observeEvent(input$resetAll,{
      hide(id = 'descriptiveStatsMP')
      shinyjs::reset("descriptiveStatsPanel")
      #shinyjs::reset("sideBar")
    })
    
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
    
    #-----------------------#
    # Statistical Inference #
    #-----------------------#
    
    observeEvent(input$goInference, {
      show(id = "inferenceMP")
    })
    
    observeEvent(input$resetInference, {
      hide(id = "inferenceMP")
      shinyjs::reset("inferencePanel")
    })
    
    #----------------------------#
    # Regression and Correlation #
    #----------------------------#
    
    observeEvent(input$goRegression, {
      show(id = "RegCorMP")
      showTab(inputId = 'tabSet', target = 'Simple Linear Regression')
      showTab(inputId = 'tabSet', target = 'Normality of Residuals')
      showTab(inputId = 'tabSet', target = 'Residual Plots')
    })
    
    observeEvent(input$resetRegCor, {
      # hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
      # hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
      # hideTab(inputId = 'tabSet', target = 'Residual Plots')
      hide(id = "RegCorMP")
      shinyjs::reset("RegCorPanel")
    })
    
    observe(
      hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
    )
    
    observe(
      hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
    )
    
    observe(
      hideTab(inputId = 'tabSet', target = 'Residual Plots')
    )
}
  
shinyApp(ui = ui, server = server)