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
library(readr)
library(readxl)
library(rstatix)
library(shiny)
library(shinyjs)
library(shinyMatrix)
library(shinyvalidate)
library(shinyWidgets)
library(tools)
library(writexl)
library(xtable)
library(MASS)

# =========================================================================== #  
# ---- UI Components -------------------------------------------------------- 
# =========================================================================== #

probDistUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
  #  ========================================================================== #  
  ## -------- Sidebar Panel --------------------------------------------------- 
  #  ========================================================================== #
      sidebarPanel(
        shinyjs::useShinyjs(),
        div(
          id = ns("inputPanel"),
          
          HTML("<label class='si-label'><b>Distribution</b></label>"),
          
          radioButtons(
            inputId  = ns("probability"), 
            label    = NULL, #strong("Distribution"), 
            choices  = c("Contingency Table",
                         "Binomial", 
                         "Poisson",
                         "Hypergeometric",
                         "Negative Binomial",
                         "Normal"), 
            selected =  NULL,
            inline   = FALSE),

### ------------ Contingency Tables -------------------------------------------

          conditionalPanel(
            ns = ns,
            id = ns("contingencyPanel"), 
            condition = "input.probability == 'Contingency Table'",
          
            radioButtons(
              inputId = ns("cTableDimension"),
              label   = strong("Dimension"),
              choices = c("2 x 2",
                          "2 x 3",
                          "3 x 2",
                          "3 x 3"),
              inline  = TRUE),
            
            # radioButtons(
            #   inputId = ns("cTableType"),
            #   label   = strong("Data Format"),
            #   choices = c("Frequency Distribution",
            #               "Probability Distribution"),
            #   inline  = TRUE
            # ),
            
            conditionalPanel(
              ns = ns,
              condition = "input.cTableDimension == '2 x 2'",
            
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableType == 'Frequency Distribution'",
            
              matrixInput(
                inputId    = ns("cMatrix2x2"),
                inputClass = "cMatrix",
                value      = matrix(c(18,22, 21,152), 
                                    2, 2, 
                                    dimnames = list(c("R1", "R2"), 
                                                    c("C1", "C2"))),
                rows       = list(editableNames = TRUE),
                cols       = list(editableNames = TRUE)),
              # ), # Frequency Distribution
            
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableType == 'Probability Distribution'",
            
              #   matrixInput(
              #     inputId    = ns("pMatrix2x2"),
              #     inputClass = "cMatrix",
              #     value      = matrix(c(0.18,0.22, 0.41,0.19), 
              #                         2, 2, 
              #                         dimnames = list(c("R1", "R2"), 
              #                                         c("C1", "C2"))),
              #     rows       = list(editableNames = TRUE),
              #     cols       = list(editableNames = TRUE)),
              # ), # Probability Distribution
            ), # 2x2
          
            conditionalPanel(
              ns = ns,
              condition = "input.cTableDimension == '2 x 3'",
            
              # conditionalPanel(
              #   ns = ns
              #   condition = "input.cTableType == 'Frequency Distribution'",
            
              matrixInput(
                inputId    = ns("cMatrix2x3"),
                inputClass = "cMatrix",
                value      = matrix(c(30,210, 26,121, 0,20), 
                                    2, 3, 
                                    dimnames = list(c("R1", "R2"), 
                                                    c("C1", "C2", "C3"))),
                rows       = list(editableNames = TRUE),
                cols       = list(editableNames = TRUE)),
              # ), # Frequency Distribution
            
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableType == 'Probability Distribution'",
              #                                                               
              #   matrixInput(
              #     inputId    = "pMatrix2x3",
              #     inputClass = "cMatrix",
              #     value      = matrix(c(0.28,0.11, 0.08,0.26, 0.13,0.14), 
              #                         2, 3, 
              #                         dimnames = list(c("R1", "R2"), 
              #                                         c("C1", "C2", "C3"))),
              #     rows       = list(editableNames = TRUE),
              #     cols       = list(editableNames = TRUE)
              #   ),
              # ), # Probability Distribution
            ), # 2x3
          
            conditionalPanel(
              ns = ns,
              condition = "input.cTableDimension == '3 x 2'",
            
              # conditionalPanel(
              #   condition = "input.cTableType == 'Frequency Distribution'",
            
              matrixInput(
                inputId    = ns("cMatrix3x2"),
                inputClass = "cMatrix",
                value      = matrix(c(115,75,142, 250,183,235), 
                                    3, 2, 
                                    dimnames = list(c("R1", "R2", "R3"), 
                                                    c("C1", "C2"))),
                rows       = list(editableNames = TRUE),
                cols       = list(editableNames = TRUE)),
              # ), # Frequency Distribution
            
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableType == 'Probability Distribution'",
              #                                                               
              #   matrixInput(
              #     inputId    = "pMatrix3x2",
              #     inputClass = "cMatrix",
              #     value      = matrix(c(0.115,0.075,0.142, 0.250,0.183,0.235), 
              #                         3, 2, 
              #                         dimnames = list(c("R1", "R2", "R3"), 
              #                                         c("C1", "C2"))),
              #     rows       = list(editableNames = TRUE),
              #     cols       = list(editableNames = TRUE)
              #   ),
              # ), # Probability Distribution
            ), # 3x2
          
            conditionalPanel(
              ns = ns,
              condition = "input.cTableDimension == '3 x 3'",
            
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableType == 'Frequency Distribution'",
            
              matrixInput(
                inputId    = ns("cMatrix3x3"),
                inputClass = "cMatrix",
                value      = matrix(c(6,14,50, 38,31,50, 31,4,5), 
                                    3, 3, 
                                    dimnames = list(c("R1", "R2", "R3"), 
                                                    c("C1", "C2", "C3"))),
                rows       = list(editableNames = TRUE),
                cols       = list(editableNames = TRUE)),
              # ), # Frequency Distribution
            
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableType == 'Probability Distribution'",
              #                                                                 
              #   matrixInput(
              #     inputId    = ns("pMatrix3x3"),
              #     inputClass = "cMatrix",
              #     value      = matrix(c(0.0262,0.0611,0.2183, 0.1659,0.1354,0.2184, 0.1354,0.0175,0.0218), 
              #                         3, 3, 
              #                         dimnames = list(c("R1", "R2", "R3"), 
              #                                         c("C1", "C2", "C3"))),
              #     rows       = list(editableNames = TRUE),
              #     cols       = list(editableNames = TRUE)
              #   ),
              # ), # Probability Distribution            
            ), # 3x3
          
            radioButtons(
              inputId = ns("cTableProb"),
              label   = strong("Probabilities"),
              choices = c("Marginal",
                          "Joint",
                          "Union",
                          "Conditional"),
              inline  = TRUE),
          
            actionButton(
              inputId = ns("gocTable"), 
              label   = "Calculate",
              class   = "act-btn"),
          
            actionButton(
              inputId = ns("resetcTable"), 
              label   = "Reset Values",
              class   = "act-btn"),
          ), # Contingency Tables
          
### ------------ Binomial -----------------------------------------------------

          conditionalPanel(
            ns = ns,
            id = ns("binomialPanel"), 
            condition = "input.probability == 'Binomial'",
          
            numericInput(
              inputId = ns("numTrialsBinom"),
              label   = strong("Number of Trials (\\( n\\))"),
              value   = 7, 
              min     = 1, 
              step    = 1),
          
            numericInput(
              inputId = ns("successProbBinom"),
              label   = strong("Probability of Success (\\( p\\))"),
              value   = 0.15, 
              min     = 0, 
              max     = 1, 
              step    = 0.00001),
          
            HTML("<label class='si-label'><b>Probability</b></label>"),                                                            
            radioButtons(
              inputId      = ns("calcBinom"),
              label        = NULL,
              choiceValues = list("exact", 
                                  "cumulative", 
                                  "upperTail", 
                                  "greaterThan", 
                                  "lessThan", 
                                  "between"),
              choiceNames  = list("\\(P(X = x \\))",
                                  "\\(P(X \\leq x)\\)",
                                  "\\(P(X \\ge x)\\)", 
                                  "\\(P(X \\gt x)\\)", 
                                  "\\(P(X < x)\\)", 
                                  "\\(P(x_1 \\leq X \\leq x_2)\\)"),
              inline       = FALSE), 
          
            conditionalPanel(
              ns = ns,
              condition = "input.calcBinom != 'between'",
            
              numericInput(
                inputId = ns("numSuccessesBinom"),
                label   = strong("Number of Successes (\\( x\\))"),
                value   = 2, 
                min     = 0, 
                step    = 1)
            ), # !between
          
            conditionalPanel(
              ns = ns,
              condition = "input.calcBinom == 'between'",
            
              numericInput(
                inputId = ns("numSuccessesBinomx1"),
                label   = strong("Number of Successes (\\( x_{1}\\))"),
                value   = 2, 
                min     = 0, 
                step    = 1),
            
              numericInput(
                inputId = ns("numSuccessesBinomx2"),
                label   = strong("Number of Successes (\\( x_{2}\\))"),
                value   = 4, 
                min     = 0, 
                step    = 1)
            ), # between
            br(),
            p(strong("Options")),
            hr(),
          
            checkboxInput(
              inputId = ns("showBinomTable"), 
              label   = "Display Probability Distribution Table", 
              value   = TRUE),
          
            actionButton(
              inputId = ns("goBinom"), 
              label   = "Calculate",
              class   = "act-btn"),
            
            actionButton(
              inputId = ns("resetBinomial"), 
              label   = "Reset Values",
              class   = "act-btn") 
          ), # Binomial
          
### ------------ Poisson ------------------------------------------------------

          conditionalPanel(
            ns = ns,
            id = ns("poissonPanel"), 
            condition = "input.probability == 'Poisson'",
          
            numericInput(
              inputId = ns("muPoisson"), 
              label   = strong("Average (\\( \\mu\\))"),
              value   = 4.5),
          
            HTML("<label class='si-label'><b>Probability</b></label>"),                                                            
            radioButtons(
              inputId      = ns("calcPoisson"),
              label        = NULL,
              choiceValues = list("exact", 
                                  "cumulative", 
                                  "upperTail", 
                                  "greaterThan", 
                                  "lessThan", 
                                  "between"),
              choiceNames  = list("\\(P(X = x \\))",
                                  "\\(P(X \\leq x)\\)",
                                  "\\(P(X \\ge x)\\)", 
                                  "\\(P(X \\gt x)\\)", 
                                  "\\(P(X < x)\\)", 
                                  "\\(P(x_1 \\leq X \\leq x_2)\\)"),
              inline       = FALSE),
          
            conditionalPanel(
              ns = ns,
              condition = "input.calcPoisson != 'between'",
              
              numericInput(
                inputId = ns("xPoisson"), 
                label   = strong("Number of Successes (\\( x\\))"),
                value   = 4, 
                min     = 0, 
                step    = 1)
            ), # !between
          
            conditionalPanel(
              ns = ns,
              condition = "input.calcPoisson == 'between'",
              
              numericInput(
                inputId = ns("x1Poisson"),
                label   = strong("Number of Successes (\\( x_{1}\\))"),
                value   = 4, 
                min     = 0, 
                step    = 1),
            
              numericInput(
                inputId = ns("x2Poisson"),
                label   = strong("Number of Successes (\\( x_{2}\\))"),
                value   = 6,
                min     = 0, 
                step    = 1)
            ), # between
            br(),
            p(strong("Options")),
            hr(),
          
            checkboxInput(
              inputId = ns("showPoissTable"), 
              label   = "Display Probability Distribution Table", 
              value   = TRUE),
            
            actionButton(
              inputId = ns("goPoisson"), 
              label   = "Calculate",
              class = "act-btn"),
          
            actionButton(
              inputId = ns("resetPoisson"), 
              label   = "Reset Values",
              class = "act-btn") 
          ), # Poisson
        
### ------------ Hypergeometric --------------------------------------------

          conditionalPanel(
            ns = ns,
            id = ns("HypGeoPanel"),
            condition = "input.probability == 'Hypergeometric'",

            numericInput(
              inputId = ns("popSizeHypGeo"),
              label   = strong("Population Size (\\( N\\))"),
              value   = 12,
              min     = 1,
              step    = 1),
            
            numericInput(
              inputId = ns("popSuccessesHypGeo"),
              label   = strong("Number of Successes in the Population (\\( M\\))"),
              value   = 5,
              min     = 1,
              step    = 1),
            
            numericInput(
              inputId = ns("sampSizeHypGeo"),
              label   = strong("Sample Size (\\( n\\))"),
              value   = 6,
              min     = 1,
              step    = 1),

            HTML("<label class='si-label'><b>Probability</b></label>"),

            radioButtons(
              inputId      = ns("calcHypGeo"),
              label        = NULL,
              choiceValues = list("exact",
                                  "cumulative",
                                  "upperTail",
                                  "greaterThan",
                                  "lessThan",
                                  "between"),
              choiceNames  = list("\\(P(X = x \\))",
                                  "\\(P(X \\leq x)\\)",
                                  "\\(P(X \\ge x)\\)",
                                  "\\(P(X \\gt x)\\)",
                                  "\\(P(X < x)\\)",
                                  "\\(P(x_1 \\leq X \\leq x_2)\\)"),
              inline       = FALSE),

            conditionalPanel(
              ns = ns,
              condition = "input.calcHypGeo != 'between'",

              numericInput(
                inputId = ns("xHypGeo"),
                label   = strong("Number of Successes in the Sample (\\( x\\))"),
                value   = 1,
                min     = 0,
                step    = 1)
            ),

            conditionalPanel(
              ns = ns,
              condition = "input.calcHypGeo == 'between'",

              numericInput(
                inputId = ns("x1HypGeo"),
                label   = strong("Number of Successes in the Sample (\\( x_{1}\\))"),
                value   = 3,
                min     = 0,
                step    = 1),

              numericInput(
                inputId = ns("x2HypGeo"),
                label   = strong("Number of Successes in the Sample (\\( x_{2}\\))"),
                value   = 5,
                min     = 0,
                step    = 1)
            ),

            br(),
            p(strong("Options")),
            hr(),

            checkboxInput(
              inputId = ns("showHypGeoTable"),
              label   = "Display Probability Distribution Table",
              value   = TRUE),

            actionButton(
              inputId = ns("goHypGeo"),
              label   = "Calculate",
              class   = "act-btn"),

            actionButton(
              inputId = ns("resetHypGeo"),
              label   = "Reset Values",
              class   = "act-btn")
          ), # Hypergeometric

### ------------ Negative Binomial --------------------------------------------

          conditionalPanel(
            ns = ns,
            id = ns("NegBinPanel"),
            condition = "input.probability == 'Negative Binomial'",
            
            numericInput(
              inputId = ns("successNegBin"),
              label   = strong("Number of Successes (\\( r\\))"),
              value   = 3,
              min     = 0,
              step    = 1),
            
            numericInput(
              inputId = ns("successProbNegBin"),
              label   = strong("Probability of Successes (\\( p\\))"),
              value   = 0.14,
              min     = 0,
              max     = 1,
              step    = 0.00001),
            
            HTML("<label class='si-label'><b>Probability</b></label>"),
            
            radioButtons(
              inputId      = ns("calcNegBin"),
              label        = NULL,
              choiceValues = list("exact",
                                  # "cumulative",
                                  # "upperTail",
                                  # "greaterThan",
                                  # "lessThan",
                                  "between"),
              choiceNames  = list("\\(P(X = x \\))",
                                  # "\\(P(X \\leq x)\\)",
                                  # "\\(P(X \\ge x)\\)",
                                  # "\\(P(X \\gt x)\\)",
                                  # "\\(P(X < x)\\)",
                                  "\\(P(x_1 \\leq X \\leq x_2)\\)"),
              inline       = FALSE),
            
            radioButtons(
              inputId = ns("trialsNegBin"),
              label = strong("Trials"),
              choiceValues = list("failures",
                                  "trials"),
              choiceNames = list("Failures prior to the \\(r^{th}\\) success",
                                 "Trials until (and including) the \\(r^{th}\\) success"),
              inline = TRUE,
            ),
            
            conditionalPanel(
              ns = ns,
              condition = "input.calcNegBin != 'between'",
              
              
              conditionalPanel(
                ns = ns,
                condition = "input.trialsNegBin == 'failures'",
              
              numericInput(
                inputId = ns("xNegBin"),
                label   = strong("Number of Failures prior to the \\(r^{th}\\) success (\\( x\\))"),
                value   = 4,
                min     = 0,
                step    = 1)
              
              ),
              
              conditionalPanel(
                ns = ns,
                condition = "input.trialsNegBin == 'trials'",
                
                numericInput(
                  inputId = ns("xNegBin"),
                  label   = strong("Trials until (and including) the \\(r^{th}\\) success (\\( x\\))"),
                  value   = 4,
                  min     = 0,
                  step    = 1)
              ),
            ),
            
            # conditionalPanel(
            #   ns = ns,
            #   condition = "input.calcNegBin == 'between'",
            #   
            #   numericInput(
            #     inputId = ns("x1NegBin"),
            #     label   = strong("Number of Failures (\\( x_{1}\\))"),
            #     value   = 2,
            #     min     = 0,
            #     step    = 1),
            #   
            #   numericInput(
            #     inputId = ns("x2NegBin"),
            #     label   = strong("Number of Failures (\\( x_{2}\\))"),
            #     value   = 4,
            #     min     = 0,
            #     step    = 1)
            # ),
            
            # br(),
            # p(strong("Options")),
            # hr(),
            # 
            # checkboxInput(
            #   inputId = ns("showNegBinTable"),
            #   label   = "Display Probability Distribution Table",
            #   value   = TRUE),
            
            actionButton(
              inputId = ns("goNegBin"),
              label   = "Calculate",
              class   = "act-btn"),
            
            actionButton(
              inputId = ns("resetNegBin"),
              label   = "Reset Values",
              class   = "act-btn")
          ), # Negative Binomial

### ------------ Normal -------------------------------------------------------

          conditionalPanel(
            ns = ns,
            id = ns("normalPanel"), 
            condition = "input.probability == 'Normal'",
            
            numericInput(
              inputId = ns("popMean"), 
              label   = strong("Population Mean (\\( \\mu\\))"), 
              value   = 0, 
              step    = 0.00001),
          
            numericInput(
              inputId = ns("popSD"),
              label   = strong("Population Standard Deviation (\\( \\sigma\\))"),
              value   = 1, 
              min     = 0, 
              step    = 0.00001),
          
            radioButtons(
              inputId      = ns("calcQuantiles"),
              label        = strong("Type of Calculation"),
              choiceValues = list("Probability", "Quantile"),
              choiceNames  = list("Probability", "Quantile"),
              inline       = TRUE),
          
      #### ---------------- Probability ---------------------------------------------
            conditionalPanel( 
              ns = ns,
              condition = "input.calcQuantiles == 'Probability'",
            
              checkboxInput(
                inputId = ns("sampMeanDistr"),
                label   = strong("Sampling Distribution of the Sample Mean"),
                value   = 0),
                
              conditionalPanel(
                ns = ns,
                condition = "input.sampMeanDistr == 0",
                
                HTML("<label class='si-label'><b>Probability</b></label>"),
                radioButtons(
                  inputId      = ns("calcNormal"),
                  label        = NULL, 
                  choiceValues = list("cumulative", 
                                      "upperTail", 
                                      "between"),
                  choiceNames  = list("\\(P(X \\leq x)\\) or \\(P(X < x)\\)", 
                                      "\\(P(X \\ge x)\\) or \\(P(X \\gt x)\\)", 
                                      "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                  inline       = FALSE),
                
                conditionalPanel(
                  ns = ns,
                  condition = "input.calcNormal != 'between'",
                
                  numericInput(
                    inputId = ns("xValue"),
                    label   = strong("Normally Distributed Variable (\\( x\\))"),
                    value   = 0, 
                    step    = 0.00001),
                ), # !between
              
                conditionalPanel(
                  ns = ns,
                  condition = "input.calcNormal == 'between'",
                
                  numericInput(
                    inputId = ns("x1Value"),
                    label   = strong("Normally Distributed Variable (\\( x_{1}\\))"),
                    value   = -1, 
                    step    = 0.00001),
                  
                  numericInput(
                    inputId = ns("x2Value"),
                    label   = strong("Normally Distributed Variable (\\( x_{2}\\))"),
                    value   = 1, 
                    step    = 0.00001),
                ), # between
              ), # !sampMeanDistr
                
              conditionalPanel(
                ns = ns,
                condition = "input.sampMeanDistr == 1",
                
                HTML("<label class='si-label'><b>Probability</b></label>"),                                                  
                radioButtons(
                  inputId      = ns("calcNormSampDistr"),
                  label        = NULL, 
                  choiceValues = list("cumulative", 
                                      "upperTail", 
                                      "between"),
                  choiceNames  = list("\\(P(\\bar{X} \\leq x)\\) or \\(P(\\bar{X} < x)\\)", 
                                      "\\(P(\\bar{X} \\ge x)\\) or \\(P(\\bar{X} \\gt x)\\)", 
                                      "\\(P(x_1 \\leq \\bar{X} \\leq x_2)\\)"),
                  inline       = FALSE), #,width = '1000px'),
                
                conditionalPanel(
                  ns = ns,
                  condition = "input.calcNormSampDistr != 'between'",
                  
                  numericInput(
                    inputId = ns("sampDistrxValue"),
                    label   = strong("Normally Distributed Variable (\\( \\bar{x}\\))"),
                    value   = 0, 
                    step    = 0.00001)
                ), # !between
                
                conditionalPanel(
                  ns = ns,
                  condition = "input.calcNormSampDistr == 'between'",
                  
                  numericInput(
                    inputId = ns("sampDistrx1Value"),
                    label   = strong("Normally Distributed Variable (\\( \\bar{x}_{1}\\))"),
                    value   = -1, 
                    step    = 0.00001),
                  
                  numericInput(
                    inputId = ns("sampDistrx2Value"),
                    label   = strong("Normally Distributed Variable (\\( \\bar{x}_{2}\\))"),
                    value   = 1, 
                    step    = 0.00001)
                ), # between
                
                numericInput(
                  inputId = ns("sampDistrSize"),
                  label   = strong("Sample Size (\\( n\\))"),
                  value   = 10, 
                  step    = 1),
              ), # sampMeanDistr
            
              actionButton(
                inputId = ns("goNormalProb"), 
                label   = "Calculate",
                class   = "act-btn"),
              
              actionButton(
                inputId = ns("resetNormalProb"), 
                label   = "Reset Values",
                class   = "act-btn")
            ), # Normal Probability
          
      #### ---------------- Quantile ------------------------------------------------
            conditionalPanel( 
              ns = ns,
              condition = "input.calcQuantiles == 'Quantile'",
              
              radioButtons(
                inputId      = ns("calcQuartiles"),
                label        = NULL,
                choiceValues = list("Quartiles", 
                                    "Percentile"),
                choiceNames  = list("Quartiles \\( (Q_{1}, Q_{2}, Q_{3}) \\)", 
                                    "Percentile"),
                inline       = TRUE),
              
              conditionalPanel(
                ns = ns,
                condition = "input.calcQuartiles == 'Percentile'",
                
                autonumericInput(
                  inputId       = ns("percentileValue"),
                  label         = strong("Percentile Value"),
                  value         = 25,
                  minimumValue  = 0,
                  maximumValue  = 100,
                  decimalPlaces = 0,
                  align         = 'left',
                  suffixText    = '%',
                  wheelOn       = 'hover',
                  wheelStep     = 1),
              ), # Percentile
                
              actionButton(
                inputId = ns("goNormalQuan"), 
                label   = "Calculate",
                class   = "act-btn"),
            
              actionButton(
                inputId = ns("resetNormalQuan"), 
                label   = "Reset Values",
                class = "act-btn") 
            ), # Quantile
          ) # Normal
        ), #inputPanel
      ), # sidebarPanel
    
#  ========================================================================== #  
## -------- Main Panel ------------------------------------------------------ 
#  ========================================================================== #
     mainPanel(
        div(id = ns("probabilityMP"),
            
### ------------ Contingency Tables -------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.probability == 'Contingency Table'",
                
              conditionalPanel(
                ns = ns,
                condition = "input.cTableDimension == '2 x 2'",
                
                # conditionalPanel(
                #   ns = ns,
                #   condition = "input.cTableType == 'Frequency Distribution'",
                
                uiOutput(ns("render2x2cTable")),
                br(),
                # ), # 2x2 Frequency Distribution
                  
                # conditionalPanel(
                #   ns = ns,
                #   condition = "input.cTableType == 'Probability Distribution'",
                #                                                  
                #   uiOutput(ns("render2x2pTable")),
                #   br(),
                # ), # 2x2 Probability Distribution
              ), # cTableDimension == 2 x 2
                
              conditionalPanel(
                ns = ns,
                condition = "input.cTableDimension == '2 x 3'",
                  
                # conditionalPanel(
                #   ns = ns,
                #   condition = "input.cTableType == 'Frequency Distribution'",
                  
                uiOutput(ns("render2x3cTable")),
                br(),
                # ), # 2x3 Frequency Distribution
                #                                                
                # conditionalPanel(
                #   ns = ns,
                #   condition = "input.cTableType == 'Probability Distribution'",
                #                                                  
                #   uiOutput(ns("render2x3pTable")),
                #   br(),
                # ), # 2x3 Probability Distribution
              ), # cTableDimension == 2 x 3
                
              conditionalPanel(
                ns = ns,
                condition = "input.cTableDimension == '3 x 2'",
                  
                # conditionalPanel(
                #   ns = ns,
                #   condition = "input.cTableType == 'Frequency Distribution'",
                  
                uiOutput(ns("render3x2cTable")),
                br(),
                # ), # 3x2 Frequency Distribution
                  
                # conditionalPanel(
                #   ns = ns
                #   condition = "input.cTableType == 'Probability Distribution'",
                #                                                  
                #   uiOutput(ns("render3x2pTable")),
                #   br(),
                # ), # 3x2 Probability Distribution
              ), # cTableDimension == 3 x 2
                
              conditionalPanel(
                ns = ns,
                condition = "input.cTableDimension == '3 x 3'",
                
                # conditionalPanel(
                #   ns = ns 
                #   condition = "input.cTableType == 'Frequency Distribution'",
                
                uiOutput(ns("render3x3cTable")),
                br(),
                # ), # 3x3 Frequency Distribution
                  
                # conditionalPanel(
                #   ns = ns,
                #   condition = "input.cTableType == 'Probability Distribution'",
                #                                                  
                #   uiOutput(ns("render3x3pTable")),
                #   br(),
                # ), # 3x3 Probability Distribution
              ), # cTableDimension == 3 x 3
              
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableDimension == 'Other'",
              # 
              #   uiOutput(ns("renderOthercTable")),
              #   br(),
              # )
              
              # conditionalPanel(
              #   ns = ns,
              #   condition = "input.cTableType == 'Frequency Distribution'",
              
              conditionalPanel(
                ns = ns,
                condition = "input.cTableProb == 'Marginal'",
                
                uiOutput(ns('renderMarginalProbs'))
              ), # Marginal
                
              conditionalPanel(
                ns = ns,
                condition = "input.cTableProb == 'Joint'",
                
                uiOutput(ns('renderJointProbs'))
              ), # Joint
              
              conditionalPanel(
                ns = ns,
                condition = "input.cTableProb == 'Union'",
                
                uiOutput(ns('renderUnionProbs'), width = '960px')
              ), # Union
                
              conditionalPanel(
                ns = ns,
                condition = "input.cTableProb == 'Conditional'",
                  
                uiOutput(ns('renderConditionalProbs'))
              ) # Conditional
              # ),
            ), # Contingency Table
            
          ### ------------ Binomial -----------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.probability == 'Binomial'",
              
              br(),
              uiOutput(ns("renderProbabilityBinom")),
              br(),
            ), # Binomial
            
        ### ------------ Poisson ------------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.probability == 'Poisson'",
                
              br(),
              uiOutput(ns("renderProbabilityPoisson")),
              br(),
            ), # Poisson

      ### ------------ Hypergeometric --------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.probability == 'Hypergeometric'",

              br(),
              uiOutput(ns("renderProbabilityHypGeo")),
              br(),
            ), # Hypergeometric

      ### ------------ Negative Binomial --------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.probability == 'Negative Binomial'",

              br(),
              uiOutput(ns("renderProbabilityNegBin")),
              br(),
            ), # Negative Binomial

      ### ------------ Normal -------------------------------------------------------
           conditionalPanel(
              ns = ns,
              condition = "input.probability == 'Normal'",
                
              br(),
              
      #### ---------------- Probability ---------------------------------------------
              conditionalPanel(
                ns = ns,
                condition = "input.calcQuantiles == 'Probability'",
                  
                conditionalPanel(
                  ns = ns,
                  condition = "input.sampMeanDistr == 0",
                    
                  uiOutput(ns("renderProbabilityNorm"))),
                  
                conditionalPanel(
                  ns = ns,
                  condition = "input.sampMeanDistr == 1",
                    
                  uiOutput(ns("renderSampMeanDistr"))),
              ), # Probability
            
      #### ---------------- Quantile ------------------------------------------------
             conditionalPanel(
               ns = ns,
               condition = "input.calcQuantiles == 'Quantile'",
                
               conditionalPanel(
                 ns = ns,
                 condition = "input.calcQuartiles == 'Quartiles'",
                  
                 uiOutput(ns("renderNormQuartiles"))), # Quartiles
                  
               conditionalPanel(
                 ns = ns,
                 condition = "input.calcQuartiles == 'Percentile'",
                  
                 uiOutput(ns("renderNormPercentile"))) # Percentile
               ), # Quantile
             br(),
            ) # Normal
        ), # probabilityMP
      ) # mainPanel
    ) # sidebarLayout
  ) # tagList
}

# =========================================================================== #  
# ---- Server Components ---------------------------------------------------- 
# =========================================================================== #

probDistServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
 # ========================================================================== #
 ## -------- Data Validation ------------------------------------------------
 # ========================================================================== #
    pd_iv <- InputValidator$new()

    ctable_iv <- InputValidator$new()
    ctableconditional_iv <- InputValidator$new()
    ctable2x2_iv <- InputValidator$new()
    ctable2x2conditional_iv <- InputValidator$new()
    ctable2x3_iv <- InputValidator$new()
    ctable2x3conditional_iv <- InputValidator$new()
    ctable3x2_iv <- InputValidator$new()
    ctable3x2conditional_iv <- InputValidator$new()
    ctable3x3_iv <- InputValidator$new()
    ctable3x3conditional_iv <- InputValidator$new()

    ptable_iv <- InputValidator$new()
    ptableconditional_iv <- InputValidator$new()
    ptable2x2_iv <- InputValidator$new()
    ptable2x2conditional_iv <- InputValidator$new()
    ptable2x3_iv <- InputValidator$new()
    ptable2x3conditional_iv <- InputValidator$new()
    ptable3x2_iv <- InputValidator$new()
    ptable3x2conditional_iv <- InputValidator$new()
    ptable3x3_iv <- InputValidator$new()
    ptable3x3conditional_iv <- InputValidator$new()

    binom_iv <- InputValidator$new()
    binomprob_iv <- InputValidator$new()
    binombetween_iv <- InputValidator$new()

    poiss_iv <- InputValidator$new()
    poissprob_iv <- InputValidator$new()
    poissbetween_iv <- InputValidator$new()

    HypGeo_iv <- InputValidator$new()
    HypGeoprob_iv <- InputValidator$new()
    HypGeobetween_iv <- InputValidator$new()
    
    NegBin_iv <- InputValidator$new()
    NegBinprob_iv <- InputValidator$new()
    NegBinbetween_iv <- InputValidator$new()

    norm_iv <- InputValidator$new()
    normprob_iv <- InputValidator$new()
    normbetween_iv <- InputValidator$new()

    sampdistrprob_iv <- InputValidator$new()
    sampdistrbetween_iv <- InputValidator$new()
    sampdistrsize_iv <- InputValidator$new()
    percentile_iv <- InputValidator$new()
    
 ### ------------ Rules -------------------------------------------------------
    
    ctable2x2_iv$add_rule("cMatrix2x2", sv_required())
    ctable2x2_iv$add_rule("cMatrix2x2", ~ if(any(is.na(cMatrixData2x2()))) "Fields must be positive integers.")
    ctable2x2_iv$add_rule("cMatrix2x2", ~ if(any(cMatrixData2x2() < 0)) "Fields must be positive integers.")
    ctable2x2_iv$add_rule("cMatrix2x2", ~ if(any(cMatrixData2x2() %% 1 != 0)) "Fields must be positive integers.")
    ctable2x2_iv$add_rule("cMatrix2x2", ~ if(all(cMatrixData2x2() == 0)) "All cell values cannot be equal to zero.")
    
    ctable2x2conditional_iv$add_rule("cMatrix2x2", ~ if(any(cMatrix2x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ctable2x2conditional_iv$add_rule("cMatrix2x2", ~ if(any(cMatrix2x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    ctable2x3_iv$add_rule("cMatrix2x3", sv_required())
    ctable2x3_iv$add_rule("cMatrix2x3", ~ if(any(is.na(cMatrixData2x3()))) "Fields must be positive integers.")
    ctable2x3_iv$add_rule("cMatrix2x3", ~ if(any(cMatrixData2x3() < 0)) "Fields must be positive integers.")
    ctable2x3_iv$add_rule("cMatrix2x3", ~ if(any(cMatrixData2x3() %% 1 != 0)) "Fields must be positive integers.")
    ctable2x3_iv$add_rule("cMatrix2x3", ~ if(all(cMatrixData2x3() == 0)) "All cell values cannot be equal to zero.")
    
    ctable2x3conditional_iv$add_rule("cMatrix2x3", ~ if(any(cMatrix2x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ctable2x3conditional_iv$add_rule("cMatrix2x3", ~ if(any(cMatrix2x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    ctable3x2_iv$add_rule("cMatrix3x2", sv_required())
    ctable3x2_iv$add_rule("cMatrix3x2", ~ if(any(is.na(cMatrixData3x2()))) "Fields must be positive integers.")
    ctable3x2_iv$add_rule("cMatrix3x2", ~ if(any(cMatrixData3x2() < 0)) "Fields must be positive integers.")
    ctable3x2_iv$add_rule("cMatrix3x2", ~ if(any(cMatrixData3x2() %% 1 != 0)) "Fields must be positive integers.")
    ctable3x2_iv$add_rule("cMatrix3x2", ~ if(all(cMatrixData3x2() == 0)) "All cell values cannot be equal to zero.")
    
    ctable3x2conditional_iv$add_rule("cMatrix3x2", ~ if(any(cMatrix3x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ctable3x2conditional_iv$add_rule("cMatrix3x2", ~ if(any(cMatrix3x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    ctable3x3_iv$add_rule("cMatrix3x3", sv_required())
    ctable3x3_iv$add_rule("cMatrix3x3", ~ if(any(is.na(cMatrixData3x3()))) "Fields must be positive integers.")
    ctable3x3_iv$add_rule("cMatrix3x3", ~ if(any(cMatrixData3x3() < 0)) "Fields must be positive integers.")
    ctable3x3_iv$add_rule("cMatrix3x3", ~ if(any(cMatrixData3x3() %% 1 != 0)) "Fields must be positive integers.")
    ctable3x3_iv$add_rule("cMatrix3x3", ~ if(all(cMatrixData3x3() == 0)) "All cell values cannot be equal to zero.")
    
    ctable3x3conditional_iv$add_rule("cMatrix3x3", ~ if(any(cMatrix3x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ctable3x3conditional_iv$add_rule("cMatrix3x3", ~ if(any(cMatrix3x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    ptable2x2_iv$add_rule("pMatrix2x2", sv_required())
    ptable2x2_iv$add_rule("pMatrix2x2", ~ if(any(is.na(pMatrixData2x2()))) "Probabilities must be between 0 and 1.")
    ptable2x2_iv$add_rule("pMatrix2x2", ~ if(any(pMatrixData2x2() < 0)) "Probabilities must be between 0 and 1.")
    ptable2x2_iv$add_rule("pMatrix2x2", ~ if(any(pMatrixData2x2() >= 1)) "Probabilities must be between 0 and 1.")
    
    ptable2x2conditional_iv$add_rule("pMatrix2x2", ~ if(any(pMatrix2x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ptable2x2conditional_iv$add_rule("pMatrix2x2", ~ if(any(pMatrix2x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    ptable2x3_iv$add_rule("pMatrix2x3", sv_required())
    ptable2x3_iv$add_rule("pMatrix2x3", ~ if(any(is.na(pMatrixData2x3()))) "Probabilities must be between 0 and 1.")
    ptable2x3_iv$add_rule("pMatrix2x3", ~ if(any(pMatrixData2x3() < 0)) "Probabilities must be between 0 and 1.")
    ptable2x3_iv$add_rule("pMatrix2x3", ~ if(any(pMatrixData2x3() >= 1)) "Probabilities must be between 0 and 1.")
    
    ptable2x3conditional_iv$add_rule("pMatrix2x3", ~ if(any(pMatrix2x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ptable2x3conditional_iv$add_rule("pMatrix2x3", ~ if(any(pMatrix2x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    ptable3x2_iv$add_rule("pMatrix3x2", sv_required())
    ptable3x2_iv$add_rule("pMatrix3x2", ~ if(any(is.na(pMatrixData3x2()))) "Probabilities must be between 0 and 1.")
    ptable3x2_iv$add_rule("pMatrix3x2", ~ if(any(pMatrixData3x2() < 0)) "Probabilities must be between 0 and 1.")
    ptable3x2_iv$add_rule("pMatrix3x2", ~ if(any(pMatrixData3x2() >= 1)) "Probabilities must be between 0 and 1.")
    
    ptable3x2conditional_iv$add_rule("pMatrix3x2", ~ if(any(pMatrix3x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ptable3x2conditional_iv$add_rule("pMatrix3x2", ~ if(any(pMatrix3x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    ptable3x3_iv$add_rule("pMatrix3x3", sv_required())
    ptable3x3_iv$add_rule("pMatrix3x3", ~ if(any(is.na(pMatrixData3x3()))) "Probabilities must be between 0 and 1.")
    ptable3x3_iv$add_rule("pMatrix3x3", ~ if(any(pMatrixData3x3() < 0)) "Probabilities must be between 0 and 1.")
    ptable3x3_iv$add_rule("pMatrix3x3", ~ if(any(pMatrixData3x3() >= 1)) "Probabilities must be between 0 and 1.")
    
    ptable3x3conditional_iv$add_rule("pMatrix3x3", ~ if(any(pMatrix3x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
    ptable3x3conditional_iv$add_rule("pMatrix3x3", ~ if(any(pMatrix3x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
    
    # Binomial
    
    binom_iv$add_rule("numTrialsBinom", sv_required())
    binom_iv$add_rule("numTrialsBinom", sv_integer())
    binom_iv$add_rule("numTrialsBinom", sv_gt(0))
    
    binom_iv$add_rule("successProbBinom", sv_required())
    binom_iv$add_rule("successProbBinom", sv_gte(0))
    binom_iv$add_rule("successProbBinom", sv_lte(1))
    
    binomprob_iv$add_rule("numSuccessesBinom", sv_required())
    binomprob_iv$add_rule("numSuccessesBinom", sv_integer())
    binomprob_iv$add_rule("numSuccessesBinom", sv_gte(0))
    
    binombetween_iv$add_rule("numSuccessesBinomx1", sv_required())
    binombetween_iv$add_rule("numSuccessesBinomx1", sv_integer())
    binombetween_iv$add_rule("numSuccessesBinomx1", sv_gte(0))

    binombetween_iv$add_rule("numSuccessesBinomx2", sv_required())
    binombetween_iv$add_rule("numSuccessesBinomx2", sv_integer())
    binombetween_iv$add_rule("numSuccessesBinomx2", sv_gte(0))
    
    # Poisson
    
    poiss_iv$add_rule("muPoisson", sv_required())
    poiss_iv$add_rule("muPoisson", sv_gt(0))
    
    poissprob_iv$add_rule("xPoisson", sv_required())
    poissprob_iv$add_rule("xPoisson", sv_integer())
    poissprob_iv$add_rule("xPoisson", sv_gte(0))
    
    poissbetween_iv$add_rule("x1Poisson", sv_required())
    poissbetween_iv$add_rule("x1Poisson", sv_integer())
    poissbetween_iv$add_rule("x1Poisson", sv_gte(0))
    
    poissbetween_iv$add_rule("x2Poisson", sv_required())
    poissbetween_iv$add_rule("x2Poisson", sv_integer())
    poissbetween_iv$add_rule("x2Poisson", sv_gte(0))

    # Hypergeometric 
    
    HypGeo_iv$add_rule("popSizeHypGeo", sv_required())
    HypGeo_iv$add_rule("popSizeHypGeo", sv_integer())
    HypGeo_iv$add_rule("popSizeHypGeo", sv_gt(0))

    HypGeo_iv$add_rule("popSuccessesHypGeo", sv_required())
    HypGeo_iv$add_rule("popSuccessesHypGeo", sv_integer())
    HypGeo_iv$add_rule("popSuccessesHypGeo", sv_gt(0))

    HypGeo_iv$add_rule("sampSizeHypGeo", sv_required())
    HypGeo_iv$add_rule("sampSizeHypGeo", sv_integer())
    HypGeo_iv$add_rule("sampSizeHypGeo", sv_gt(0))

    HypGeoprob_iv$add_rule("xHypGeo", sv_required())
    HypGeoprob_iv$add_rule("xHypGeo", sv_integer())
    HypGeoprob_iv$add_rule("xHypGeo", sv_gte(0))
    
    HypGeobetween_iv$add_rule("x1HypGeo", sv_required())
    HypGeobetween_iv$add_rule("x1HypGeo", sv_integer())
    HypGeobetween_iv$add_rule("x1HypGeo", sv_gte(0))
    
    HypGeobetween_iv$add_rule("x2HypGeo", sv_required())
    HypGeobetween_iv$add_rule("x2HypGeo", sv_integer())
    HypGeobetween_iv$add_rule("x2HypGeo", sv_gte(0))

    # Negative Binomial
    
    NegBin_iv$add_rule("successNegBin", sv_required())
    NegBin_iv$add_rule("successNegBin", sv_integer())
    NegBin_iv$add_rule("successNegBin", sv_gte(0))
    
    NegBin_iv$add_rule("successProbNegBin", sv_required())
    NegBin_iv$add_rule("successProbNegBin", sv_gt(0))
    NegBin_iv$add_rule("successProbNegBin", sv_lte(1))
    
    NegBinprob_iv$add_rule("xNegBin", sv_required())
    NegBinprob_iv$add_rule("xNegBin", sv_integer())
    NegBinprob_iv$add_rule("xNegBin", sv_gte(0))
    
    NegBinbetween_iv$add_rule("x1NegBin", sv_required())
    NegBinbetween_iv$add_rule("x1NegBin", sv_integer())
    NegBinbetween_iv$add_rule("x1NegBin", sv_gte(0))
    
    NegBinbetween_iv$add_rule("x2NegBin", sv_required())
    NegBinbetween_iv$add_rule("x2NegBin", sv_integer())
    NegBinbetween_iv$add_rule("x2NegBin", sv_gte(0))
    
    # Normal

    norm_iv$add_rule("popMean", sv_required())
    
    norm_iv$add_rule("popSD", sv_required())
    norm_iv$add_rule("popSD", sv_gt(0))
    
    normprob_iv$add_rule("xValue", sv_required())
    
    normbetween_iv$add_rule("x1Value", sv_required())
    normbetween_iv$add_rule("x2Value", sv_required())
    
    sampdistrprob_iv$add_rule("sampDistrxValue", sv_required())
    
    sampdistrbetween_iv$add_rule("sampDistrx1Value", sv_required())
    sampdistrbetween_iv$add_rule("sampDistrx2Value", sv_required())
    
    sampdistrsize_iv$add_rule("sampDistrSize", sv_required())
    sampdistrsize_iv$add_rule("sampDistrSize", sv_integer())
    sampdistrsize_iv$add_rule("sampDistrSize", sv_gt(0))
    
    percentile_iv$add_rule("percentileValue", sv_required())
    percentile_iv$add_rule("percentileValue", sv_gt(0))
    percentile_iv$add_rule("percentileValue", sv_lt(100))
    
 ### ------------ Conditions --------------------------------------------------

    ctable2x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '2 x 2'))
    
    ctable2x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '2 x 2' &&
                                                 # input$cTableType == 'Frequency Distribution' &&
                                                 input$cTableProb == 'Conditional'))
    
    ctable2x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '2 x 3'))
    
    ctable2x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '2 x 3' &&
                                                 # input$cTableType == 'Frequency Distribution'&&
                                                 input$cTableProb == 'Conditional'))
    
    ctable3x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '3 x 2'))
    
    ctable3x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '3 x 2' &&
                                                 # input$cTableType == 'Frequency Distribution'&&
                                                 input$cTableProb == 'Conditional'))
    
    ctable3x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '3 x 3'))
    
    ctable3x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '3 x 3' &&
                                                 # input$cTableType == 'Frequency Distribution'&&
                                                 input$cTableProb == 'Conditional'))
    
    ptable2x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '2 x 2' &&
                                      input$cTableType == 'Probability Distribution'))
    
    ptable2x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '2 x 2' &&
                                                 input$cTableType == 'Probability Distribution'&&
                                                 input$cTableProb == 'Conditional'))
    
    ptable2x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '2 x 3' &&
                                      input$cTableType == 'Probability Distribution'))
    
    ptable2x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '2 x 3' &&
                                                 input$cTableType == 'Probability Distribution'&&
                                                 input$cTableProb == 'Conditional'))
    
    ptable3x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '3 x 2' &&
                                      input$cTableType == 'Probability Distribution'))
    
    ptable3x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '3 x 2' &&
                                                 input$cTableType == 'Probability Distribution'&&
                                                 input$cTableProb == 'Conditional'))
    
    ptable3x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                      input$cTableDimension == '3 x 3' &&
                                      input$cTableType == 'Probability Distribution'))
    
    ptable3x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                                 input$cTableDimension == '3 x 3' &&
                                                 input$cTableType == 'Probability Distribution'&&
                                                 input$cTableProb == 'Conditional'))
    # Binomial
    
    binom_iv$condition(~ isTRUE(input$probability == 'Binomial'))
    
    binomprob_iv$condition(~ isTRUE(input$probability == 'Binomial' && 
                                      input$calcBinom != 'between'))
    
    binombetween_iv$condition(~ isTRUE(input$probability == 'Binomial' && 
                                         input$calcBinom == 'between'))
    # Poisson
    
    poiss_iv$condition(~ isTRUE(input$probability == 'Poisson'))
    
    poissprob_iv$condition(~ isTRUE(input$probability == 'Poisson' && 
                                      input$calcPoisson != 'between'))
    
    poissbetween_iv$condition(~ isTRUE(input$probability == 'Poisson' && 
                                         input$calcPoisson == 'between'))
    
    # Hypergeometric
    
    HypGeo_iv$condition(~ isTRUE(input$probability == 'Hypergeometric'))
    
    HypGeoprob_iv$condition(~ isTRUE(input$probability == 'Hypergeometric' && 
                                      input$calcHypGeo != 'between'))
    
    HypGeobetween_iv$condition(~ isTRUE(input$probability == 'Hypergeometric' && 
                                         input$calcHypGeo == 'between'))
    # Negative Binomial
    
    NegBin_iv$condition(~ isTRUE(input$probability == 'Negative Binomial'))
    
    NegBinprob_iv$condition(~ isTRUE(input$probability == 'Negative Binomial' && 
                                       input$calcNegBin != 'between'))
    
    NegBinbetween_iv$condition(~ isTRUE(input$probability == 'Negative Binomial' && 
                                          input$calcNegBin == 'between'))
    
    # Normal
    
    norm_iv$condition(~ isTRUE(input$probability == 'Normal'))
    
    normprob_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                     input$calcQuantiles == 'Probability' &&
                                     input$sampMeanDistr == 0 && 
                                     input$calcNormal != 'between'))
    
    normbetween_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                        input$calcQuantiles == 'Probability' &&
                                        input$sampMeanDistr == 0 &&
                                        input$calcNormal == 'between'))
    
    sampdistrprob_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                          input$calcQuantiles == 'Probability' && 
                                          input$sampMeanDistr == 1 && 
                                          input$calcNormSampDistr != 'between'))
    
    sampdistrbetween_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                             input$calcQuantiles == 'Probability' &&
                                             input$sampMeanDistr == 1 &&
                                             input$calcNormSampDistr == 'between'))
    
    sampdistrsize_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                          input$calcQuantiles == 'Probability' && 
                                          input$sampMeanDistr == 1))
    
    percentile_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                       input$calcQuantiles == 'Quantile' &&
                                       input$calcQuartiles == 'Percentile'))

 ### ------------ Dependencies ------------------------------------------------

    ctable_iv$add_validator(ctable2x2_iv)
    ctable_iv$add_validator(ctable2x3_iv)
    ctable_iv$add_validator(ctable3x2_iv)
    ctable_iv$add_validator(ctable3x3_iv)
    
    ctableconditional_iv$add_validator(ctable2x2conditional_iv)
    ctableconditional_iv$add_validator(ctable2x3conditional_iv)
    ctableconditional_iv$add_validator(ctable3x2conditional_iv)
    ctableconditional_iv$add_validator(ctable3x3conditional_iv)
    
    ptable_iv$add_validator(ptable2x2_iv)
    ptable_iv$add_validator(ptable2x3_iv)
    ptable_iv$add_validator(ptable3x2_iv)
    ptable_iv$add_validator(ptable3x3_iv)
    
    ptableconditional_iv$add_validator(ptable2x2conditional_iv)
    ptableconditional_iv$add_validator(ptable2x3conditional_iv)
    ptableconditional_iv$add_validator(ptable3x2conditional_iv)
    ptableconditional_iv$add_validator(ptable3x3conditional_iv)
    
    binom_iv$add_validator(binomprob_iv)
    binom_iv$add_validator(binombetween_iv)
    
    poiss_iv$add_validator(poissprob_iv)
    poiss_iv$add_validator(poissbetween_iv)
    
    HypGeo_iv$add_validator(HypGeoprob_iv)
    HypGeo_iv$add_validator(HypGeobetween_iv)
    
    NegBin_iv$add_validator(NegBinprob_iv)
    NegBin_iv$add_validator(NegBinbetween_iv)

    norm_iv$add_validator(normprob_iv)
    norm_iv$add_validator(normbetween_iv)
    norm_iv$add_validator(sampdistrprob_iv)
    norm_iv$add_validator(sampdistrbetween_iv)
    norm_iv$add_validator(sampdistrsize_iv)
    norm_iv$add_validator(percentile_iv)
    
    pd_iv$add_validator(ctable_iv)
    pd_iv$add_validator(ptable_iv)
    pd_iv$add_validator(binom_iv)
    pd_iv$add_validator(poiss_iv)
    pd_iv$add_validator(HypGeo_iv)
    pd_iv$add_validator(NegBin_iv)
    pd_iv$add_validator(norm_iv)

 ### ------------ Activation --------------------------------------------------   

    pd_iv$enable()

    ctable_iv$enable()
    ctableconditional_iv$enable()
    ctable2x2_iv$enable()
    ctable2x2conditional_iv$enable()
    ctable2x3_iv$enable()
    ctable2x3conditional_iv$enable()
    ctable3x2_iv$enable()
    ctable3x2conditional_iv$enable()
    ctable3x3_iv$enable()
    ctable3x3conditional_iv$enable()

    ptable_iv$enable()
    ptableconditional_iv$enable()
    ptable2x2_iv$enable()
    ptable2x2conditional_iv$enable()
    ptable2x3_iv$enable()
    ptable2x3conditional_iv$enable()
    ptable3x2_iv$enable()
    ptable3x2conditional_iv$enable()
    ptable3x3_iv$enable()
    ptable3x3conditional_iv$enable()

    binom_iv$enable()
    binomprob_iv$enable()
    binombetween_iv$enable()

    poiss_iv$enable()
    poissprob_iv$enable()
    poissbetween_iv$enable()

    HypGeo_iv$enable()
    HypGeoprob_iv$enable()
    HypGeobetween_iv$enable()
    
    NegBin_iv$enable()
    NegBinprob_iv$enable()
    NegBinbetween_iv$enable()

    norm_iv$enable()
    normprob_iv$enable()
    normbetween_iv$enable()

    sampdistrprob_iv$enable()
    sampdistrbetween_iv$enable()
    sampdistrsize_iv$enable()
    percentile_iv$enable()
    
 # ========================================================================== #
 ## -------- Functions ------------------------------------------------------
 # ========================================================================== #
    
    ResetCTable <- function(tableID, numRows, numCols, rowNames, colNames){
      newMatrix <- matrix("", numRows, numCols)
      colnames(newMatrix) <- colNames
      rownames(newMatrix) <- rowNames
      updateMatrixInput(session, tableID, newMatrix)
    }
    
    
    getProbabilities <- function(x, t){
      return(round((x/t), 4))
    }
    
    
    getTotaledMatrix <- function(cMatrix, matrixData){
      colnames(cMatrix) <- colnames(matrixData)
      rownames(cMatrix) <- rownames(matrixData)
      cMatrix <- cbind(cMatrix, Total = round(rowSums(cMatrix), 4))
      cMatrix <- rbind(cMatrix, Total = round(colSums(cMatrix), 4))
      
      return(cMatrix)
    }
    
    
    printMarginalProbs <- function(probMatrix, cMatrix){
      outputTagList <- tagList(
        withMathJax(),
        titlePanel('Marginal Probabilities'),
        hr(),
        br()
      )
      for(row in 1:(nrow(probMatrix)-1)){
        newLine <- sprintf("\\( P( \\text{%s} ) = \\dfrac{%s}{%s} = %s \\)",
                           rownames(probMatrix)[row],
                           cMatrix[row,'Total'],
                           cMatrix['Total','Total'],
                           probMatrix[row,'Total'])
        outputTagList <- tagAppendChildren(outputTagList, newLine, br(), br(), br())
      }
      for(col in 1:(ncol(probMatrix)-1)){
        newLine <- sprintf("\\( P( \\text{%s} ) = \\dfrac{%s}{%s} = %s \\)",
                           colnames(probMatrix)[col],
                           cMatrix['Total', col],
                           cMatrix['Total','Total'],
                           probMatrix['Total',col])
        outputTagList <- tagAppendChildren(outputTagList, newLine, br(), br(), br())
      }
      return(outputTagList)
    }
    
    
    printJointProbs <- function(probMatrix, cMatrix){
      outputTagList <- tagList(
        withMathJax(),
        titlePanel('Joint Probabilities'),
        hr(),
        br()
      )
      for(row in 1:(nrow(probMatrix) - 1)){
        for(col in 1:(ncol(probMatrix) - 1)){
          newLine <- sprintf("\\( P( \\text{%s} \\cap \\text{%s} ) \\; = 
                           \\dfrac{%s}{%s} = %s \\)",
                             rownames(probMatrix)[row],
                             colnames(probMatrix)[col],
                             cMatrix[row,col],
                             cMatrix['Total','Total'],
                             probMatrix[row,col])
          outputTagList <- tagAppendChildren(outputTagList, newLine, br(), br())
        }
        outputTagList <- tagAppendChildren(outputTagList, br(), br())
      }
      return(outputTagList)
    }

    printUnionProbs <- function(probMatrix, cMatrix){
      outputTagList <- tagList(
        withMathJax(),
        titlePanel('Union Probabilities'),
        hr(),
        br()
      )
      for(row in 1:(nrow(probMatrix) - 1)){
        for(col in 1:(ncol(probMatrix) - 1)){
          newLine <- sprintf("\\( P( \\text{%s} \\cup \\text{%s} ) \\; =
                           \\; P( \\text{%s} ) + P( \\text{%s} ) - P(\\text{%s} \\cap \\text{%s}) \\; =
                           \\; \\dfrac{%s}{%s} + \\dfrac{%s}{%s} - \\dfrac{%s}{%s} \\; =
                           \\; \\dfrac{%s}{%s} = %s \\quad \\)",
                             rownames(probMatrix)[row],
                             colnames(probMatrix)[col],
                             rownames(probMatrix)[row],
                             colnames(probMatrix)[col],
                             rownames(probMatrix)[row],
                             colnames(probMatrix)[col],
                             cMatrix[row,'Total'],
                             cMatrix['Total','Total'],
                             cMatrix['Total',col],
                             cMatrix['Total','Total'],
                             cMatrix[row,col],
                             cMatrix['Total','Total'],
                             cMatrix[row,'Total'] + cMatrix['Total',col] - cMatrix[row,col],
                             cMatrix['Total','Total'],
                             probMatrix[row,'Total'] + probMatrix['Total',col] - probMatrix[row,col])
          outputTagList <- tagAppendChildren(outputTagList, newLine, br(), br())
        }
        outputTagList <- tagAppendChildren(outputTagList, br(), br())
      }
      return(outputTagList)
    }

    printConditionalProbs <- function(cMatrix){
      outputTagList <- tagList(
        withMathJax(),
        titlePanel('Conditional Probabilities'),
        hr(),
        br(),
        sprintf("\\( P(\\text{A} \\, | \\, \\text{B}) \\; = 
                                     \\; \\dfrac{P(\\text{A} \\cap \\text{B})}{P(\\text{B})}, 
                                     \\quad P(\\text{B}) \\gt 0 \\)"),
        br(),
        br(),
        br(),
        br()
      )
      for(row in 1:(nrow(cMatrix) - 1)){
        for(col in 1:(ncol(cMatrix) - 1)){
          newLine <- sprintf("\\( P( \\text{%s} \\, | \\, \\text{%s} ) \\; = 
                           \\; \\dfrac{P( \\text{%s} \\cap \\text{%s} )}{P( \\text{%s} )} \\; = 
                           \\; \\dfrac{%s}{%s} \\bigg/ \\dfrac{%s}{%s} \\; =
                           \\; \\dfrac{%s}{%s} = %s \\)",
                             rownames(cMatrix)[row],
                             colnames(cMatrix)[col],
                             rownames(cMatrix)[row],
                             colnames(cMatrix)[col],
                             colnames(cMatrix)[col],
                             cMatrix[row,col],
                             cMatrix['Total','Total'],
                             cMatrix['Total',col],
                             cMatrix['Total','Total'],
                             cMatrix[row,col],
                             cMatrix['Total',col],
                             round( (cMatrix[row,col] / cMatrix['Total',col]), 4))
          outputTagList <- tagAppendChildren(outputTagList, newLine, br(), br())
        }
        outputTagList <- tagAppendChildren(outputTagList, br(), br())
      }
      for(col in 1:(ncol(cMatrix) - 1)){
        for(row in 1:(nrow(cMatrix) - 1)){
          newLine <- sprintf("\\( P( \\text{%s} \\, | \\, \\text{%s} ) \\; = 
                           \\; \\dfrac{P( \\text{%s} \\cap \\text{%s} )}{P( \\text{%s} )} \\; = 
                           \\; \\dfrac{%s}{%s} \\bigg/ \\dfrac{%s}{%s} \\; =
                           \\; \\dfrac{%s}{%s} = %s \\)",
                             colnames(cMatrix)[col],
                             rownames(cMatrix)[row],
                             colnames(cMatrix)[col],
                             rownames(cMatrix)[row],
                             rownames(cMatrix)[row],
                             cMatrix[row,col],
                             cMatrix['Total','Total'],
                             cMatrix[row,'Total'],
                             cMatrix['Total','Total'],
                             cMatrix[row,col],
                             cMatrix[row,'Total'],
                             round( (cMatrix[row,col] / cMatrix[row,'Total']), 4))
          outputTagList <- tagAppendChildren(outputTagList, newLine, br(), br())
        }
        outputTagList <- tagAppendChildren(outputTagList, br(), br())
      }
      return(outputTagList)
    }

    shadeNormArea <- function(df, normValue, normLines, probType){
      if(normValue > 0){
        if(probType == 'cumulative') {
          geom_area(data = subset(df, x <= normLines),
                    aes(y=y), 
                    fill = "#023B70", 
                    color = NA, 
                    alpha = 0.4)
        } else if (probType == 'upperTail') {
          geom_area(data = subset(df, x >= normLines),
                    aes(y=y), 
                    fill = "#023B70", 
                    color = NA, 
                    alpha = 0.4)
        } else {
          geom_area(data = subset(df, x >= normLines[1] & x <= normLines[2]),
                    aes(y=y), 
                    fill = "#023B70", 
                    color = NA, 
                    alpha = 0.4)
        }
      }
    }

    labelNormZArea <- function(probVal, probType, normLines){
      req(pd_iv$is_valid())
      if(probType == 'cumulative') {
        centerPoint <- normLines - 0.5
      } else if (probType == 'upperTail') {
        centerPoint <- normLines + 0.5
      } else {
        centerPoint <- normLines[1] + (normLines[2] - normLines[1])/2
      }
      geom_label(data = NULL,
                 aes(x = centerPoint, y = 0.15, label = probVal),
                 fill = "#265381",
                 color = "white",
                 size = 24 / .pt,
                 fontface = "bold",
                 label.size = NA)
    }

    plotTitle <- function(variance){
      if(input$sampMeanDistr == 0){
        ggtitle(bquote(bolditalic(X %~% N(.(input$popMean),.(variance)))))
      } else {
        ggtitle(bquote(bolditalic(bar(X) %~% N(.(input$popMean),.(variance)))))
      }
    }

    plotXLab <- function(){
      if(input$sampMeanDistr == 0){
        xlab(expression(bolditalic( X )))
      } else {
        xlab(expression(bolditalic( bar(X) )))
      }
    }

    normPlot <- function(normValue, normLines, popmean, variance, standDev, lineLabels, probType, plotLab){
      req(pd_iv$is_valid())
      withMathJax()
      
      x <- round(seq(from = -3, to = 3, by = 0.1), 2)
      xSeq <- unique(sort(c(x, normLines)))
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
      lineDF <- filter(df, x %in% normLines)
      lineDF['z'] <- lineLabels
      meanDF <- filter(df, x %in% c(0))
      
      nPlot <- ggplot(df, aes(x = x, y = y)) +
        geom_line(linetype = "solid",
                  linewidth = 0.75,
                  color='#021C38') +
        geom_area(data = df,
                  aes(y=y), 
                  fill = NA, 
                  color = NA) +
        shadeNormArea(df, normValue, normLines, probType) +
        geom_segment(data = lineDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'round',
                     linewidth = 1.25,
                     color='#021C38') +
        geom_text(data = lineDF, 
                  aes(x = x, y = 0, label = z), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = meanDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     lineend = 'round',
                     linewidth = 1,
                     color='#021C38',
                     alpha = 0.5) +
        geom_text(data = meanDF, 
                  aes(x = x, y = 0, label = popmean), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = data.frame(x1 = -3, x2 = 3, y1 = 0, y2 = 0),
                     aes(x = x1, xend = x2, y = y1, yend = y2),
                     linetype = "solid",
                     linewidth = 1,
                     color='#021C38') +
        coord_cartesian(clip="off") +
        plotTitle(standDev) +
        theme_minimal()  +
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
              axis.text.x.bottom = element_text(size = 14)) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        ylab("") + 
        plotXLab() 
      
      return(nPlot)
    }
 
    normZPlot <- function(normValue, normLines, probType){
      req(pd_iv$is_valid())
      
      x <- round(seq(from = -3, to = 3, by = 0.1), 2)
      xSeq <- unique(sort(c(x, normLines)))
      
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
      
      
      lineDF <- filter(df, x %in% normLines)
      meanDF <- filter(df, x %in% c(0))
      
      nPlot <- ggplot(df, aes(x = x, y = y)) +
        geom_line(linetype = "solid",
                  linewidth = 0.75,
                  color='#021C38') +
        geom_area(data = df,
                  aes(y=y), 
                  fill = NA, 
                  color = NA) +
        shadeNormArea(df, normValue, normLines, probType) +
        geom_segment(data = lineDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'round',
                     linewidth = 1.25,
                     color='#021C38') +
        geom_text(data = lineDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = meanDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     lineend = 'round',
                     linewidth = 1,
                     color='#021C38',
                     alpha = 0.5) +
        geom_text(data = meanDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = data.frame(x1 = -3, x2 = 3, y1 = 0, y2 = 0),
                     aes(x = x1, xend = x2, y = y1, yend = y2),
                     linetype = "solid",
                     linewidth = 1,
                     color='#021C38') +
        coord_cartesian(clip="off") +
        ggtitle(bquote(bolditalic( Z %~% N(0,1) ))) +
        theme_minimal()  +
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
              axis.text.x.bottom = element_text(size = 14)) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        ylab("") +
        xlab("Z") 
      
      return(nPlot)
    }
    
 # ========================================================================== #
 ## -------- Reactives ------------------------------------------------------
 # ========================================================================== #
    
 ### ------------ cMatrixData Reactives ---------------------------------------
 #
 # Purpose:
 #   converts matrix data user input into numeric values
    
    cMatrixData2x2 <- reactive({
      suppressWarnings(as.numeric(input$cMatrix2x2))
    })
    
    cMatrixData2x3 <- reactive({
      suppressWarnings(as.numeric(input$cMatrix2x3))
    })
    
    cMatrixData3x2 <- reactive({
      suppressWarnings(as.numeric(input$cMatrix3x2))
    })
    
    cMatrixData3x3 <- reactive({
      suppressWarnings(as.numeric(input$cMatrix3x3))
    })
    
 ## ---- pMatrixData Reactives ---------------------------------------------- #
 # 
 # Purpose:
 #   converts matrix data input by the user into numeric values
    
    # pMatrixData2x2 <- reactive({
    #   suppressWarnings(as.numeric(input$pMatrix2x2))
    # })
    # 
    # pMatrixData2x3 <- reactive({
    #   suppressWarnings(as.numeric(input$pMatrix2x3))
    # })
    # 
    # pMatrixData3x2 <- reactive({
    #   suppressWarnings(as.numeric(input$pMatrix3x2))
    # })
    # 
    # pMatrixData3x3 <- reactive({
    #   suppressWarnings(as.numeric(input$pMatrix3x3))
    # })
    
 ### ------------ cMatrixTotaled Reactives ------------------------------------
 # 
 # Purpose:
 #   uses the numeric user data to create a matrix with a 'Total' row and
 #   column using the GetCMatrix function.
    
    cMatrix2x2Totaled <- reactive({
      if(!any(is.na(cMatrixData2x2()))){
        cData2x2 <- matrix(cMatrixData2x2(), ncol = ncol(input$cMatrix2x2))
        cData2x2 <- getTotaledMatrix(cData2x2, input$cMatrix2x2)
        return(cData2x2)
      }
    })
    
    cMatrix2x3Totaled <- reactive({
      if(!any(is.na(cMatrixData2x3()))){
        cData2x3 <- matrix(cMatrixData2x3(), ncol = ncol(input$cMatrix2x3))
        cData2x3 <- getTotaledMatrix(cData2x3, input$cMatrix2x3)
        return(cData2x3)
      }
    })
    
    cMatrix3x2Totaled <- reactive({
      if(!any(is.na(cMatrixData3x2()))){
        cData3x2 <- matrix(cMatrixData3x2(), ncol = ncol(input$cMatrix3x2))
        cData3x2 <- getTotaledMatrix(cData3x2, input$cMatrix3x2)
        return(cData3x2)
      }
    })
    
    cMatrix3x3Totaled <- reactive({
      if(!any(is.na(cMatrixData3x3()))){
        cData3x3 <- matrix(cMatrixData3x3(), ncol = ncol(input$cMatrix3x3))
        cData3x3 <- getTotaledMatrix(cData3x3, input$cMatrix3x3)
        return(cData3x3)
      }
    })
    
 ## ---- pMatrixTotaled Reactives ------------------------------------------- #
 #
 # Purpose:
 #   uses the numeric user data to create a matrix with a 'Total' row and
 #   column using the GetCMatrix function.
    
    # pMatrix2x2Totaled <- reactive({
    #   if(!any(is.na(pMatrixData2x2()))){
    #     pData2x2 <- matrix(pMatrixData2x2(), ncol = ncol(input$pMatrix2x2))
    #     pData2x2 <- getTotaledMatrix(pData2x2, input$pMatrix2x2)
    # 
    #     return(pData2x2)
    #   }
    # })
    # 
    # pMatrix2x3Totaled <- reactive({
    #   if(!any(is.na(cMatrixData2x3()))){
    #     pData2x3 <- matrix(pMatrixData2x3(), ncol = ncol(input$pMatrix2x3))
    #     pData2x3 <- getTotaledMatrix(pData2x3, input$pMatrix2x3)
    # 
    #     return(pData2x3)
    #   }
    # })
    # 
    # pMatrix3x2Totaled <- reactive({
    #   if(!any(is.na(pMatrixData3x2()))){
    #     pData3x2 <- matrix(pMatrixData3x2(), ncol = ncol(input$pMatrix3x2))
    #     pData3x2 <- getTotaledMatrix(pData3x2, input$pMatrix3x2)
    # 
    #     return(pData3x2)
    #   }
    # })
    # 
    # pMatrix3x3Totaled <- reactive({
    #   if(!any(is.na(pMatrixData3x3()))){
    #     pData3x3 <- matrix(pMatrixData3x3(), ncol = ncol(input$pMatrix3x3))
    #     pData3x3 <- getTotaledMatrix(pData3x3, input$pMatrix3x3)
    # 
    #     return(pData3x3)
    #   }
    # })
    
    getNormValue <- reactive({
      req(pd_iv$is_valid())
      
      if(input$calcNormal == "cumulative")
      {
        normValue <- round(pnorm(input$xValue, input$popMean, input$popSD, lower.tail = TRUE),4)
        #paste("\\(P(X \\leq \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = TRUE),4))
      }
      else if(input$calcNormal == "upperTail")
      {
        normValue <- round(pnorm(input$xValue, input$popMean, input$popSD, lower.tail = FALSE),4)
        #paste("\\(P(X > \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = FALSE),4))
      }
      else if(input$calcNormal == 'between')
      {
        req(input$x1Value <= input$x2Value)
        normValue <- round(pnorm(input$x2Value, input$popMean, input$popSD, lower.tail = TRUE) - pnorm(input$x1Value, input$popMean, input$popSD, lower.tail = TRUE), 4)
      }
    })
    
    getMeanNormValue <- reactive({
      req(pd_iv$is_valid())
      
      sampSE <- input$popSD / sqrt(input$sampDistrSize)
      
      if(input$calcNormSampDistr == "cumulative")
      {
        normValue <- round(pnorm(input$sampDistrxValue, input$popMean, sampSE, lower.tail = TRUE),4)
        #paste("\\(P(X \\leq \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = TRUE),4))
      }
      else if(input$calcNormSampDistr == "upperTail")
      {
        normValue <- round(pnorm(input$sampDistrxValue, input$popMean, sampSE, lower.tail = FALSE),4)
        #paste("\\(P(X > \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = FALSE),4))
      }
      else if(input$calcNormSampDistr == 'between')
      {
        req(input$sampDistrx1Value <= input$sampDistrx2Value)
        normValue <- round(pnorm(input$sampDistrx2Value, input$popMean, sampSE, lower.tail = TRUE) - pnorm(input$sampDistrx1Value, input$popMean, sampSE, lower.tail = TRUE), 4)
      }
    })
 
 # ========================================================================== #
 ## -------- Observers ------------------------------------------------------
 # ========================================================================== #
    
 ### ------------ Contingency Table -------------------------------------------
    
    observeEvent(input$gocTable, {
      
      output$render2x2cTable <- renderUI({
        
        validate(
          need(input$cMatrix2x2, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(cMatrixData2x2())), "Fields must be positive integers.") %then%
            need(all(cMatrixData2x2() %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(cMatrixData2x2() >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(cMatrixData2x2() != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        tagList(
          titlePanel("Frequency Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("cTable2x2"), width = '500px'),
          br(),
          br(),
          br(),
          titlePanel("Probability Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("probTable2x2"), width = '500px'),
          br(),
          br(),
          br()
          )# tagList
      })
      
      output$render2x3cTable <- renderUI({
        
        validate(
          need(input$cMatrix2x3, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(cMatrixData2x3())), "Fields must be positive integers.") %then%
            need(all(cMatrixData2x3() %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(cMatrixData2x3() >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(cMatrixData2x3() != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        tagList(
          titlePanel("Frequency Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("cTable2x3"), width = '625px'),
          br(),
          br(),
          br(),
          titlePanel("Probability Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("probTable2x3"), width = '625px'),
          br(),
          br(),
          br()
          )# tagList
      })
      
      output$render3x2cTable <- renderUI({
        
        validate(
          need(input$cMatrix3x2, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(cMatrixData3x2())), "Fields must be positive integers.") %then%
            need(all(cMatrixData3x2() %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(cMatrixData3x2() >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(cMatrixData3x2() != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        tagList(
          titlePanel("Frequency Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("cTable3x2"), width = '500px'),
          br(),
          br(),
          br(),
          titlePanel("Probability Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("probTable3x2"), width = '500px'),
          br(),
          br(),
          br()
          )# tagList
      })
      
      output$render3x3cTable <- renderUI({
        
        validate(
          need(input$cMatrix3x3, "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(!is.na(cMatrixData3x3())), "Fields must be positive integers.") %then%
            need(all(cMatrixData3x3() %% 1 == 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(all(cMatrixData3x3() >= 0), "Fields must be positive integers."),
          errorClass = "myClass")
        
        validate(
          need(any(cMatrixData3x3() != 0), "All cell values cannot be equal to zero."),
          errorClass = "myClass")
        
        tagList(
          titlePanel("Frequency Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("cTable3x3"), width = '625px'),
          br(),
          br(),
          br(),
          titlePanel("Probability Distribution Table"),
          hr(),
          br(),
          DTOutput(session$ns("probTable3x3"), width = '625px'),
          br(),
          br(),
          br()
          )# tagList
      })
      
      cData2x2 <- matrix(cMatrixData2x2(), ncol = ncol(input$cMatrix2x2))
      cData2x2 <- getTotaledMatrix(cData2x2, input$cMatrix2x2)
      
      output$cTable2x2 <- renderDT({
        datatable(cData2x2,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>%
          formatStyle(columns = c(0,3),
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:3,
                      target = 'row',
                      fontWeight = styleRow(dim(cData2x2)[1], "bold"))
      })
      
      probData2x2 <- apply(cData2x2, 2, getProbabilities, t=cData2x2['Total',3])
      
      output$probTable2x2 <- renderDT({
        datatable(probData2x2,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>% 
          formatStyle(columns = c(0,3), 
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:3,
                      target = 'row',
                      fontWeight = styleRow(dim(probData2x2)[1], "bold"))
      })
      
      cData2x3 <- matrix(cMatrixData2x3(), ncol = ncol(input$cMatrix2x3))
      cData2x3 <- getTotaledMatrix(cData2x3, input$cMatrix2x3)
      
      output$cTable2x3 <- renderDT({
        datatable(cData2x3,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>% 
          formatStyle(columns = c(0,4), #specify columns to format
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:4,
                      target = 'row',
                      fontWeight = styleRow(dim(cData2x3)[1], "bold"))
      })
      
      probData2x3 <- apply(cData2x3, 2, getProbabilities, t=cData2x3['Total',4])
      
      output$probTable2x3 <- renderDT({
        datatable(probData2x3,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>% 
          formatStyle(columns = c(0,4), 
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:4,
                      target = 'row',
                      fontWeight = styleRow(dim(probData2x3)[1], "bold"))
      })
      
      cData3x2 <- matrix(cMatrixData3x2(), ncol = ncol(input$cMatrix3x2))
      cData3x2 <- getTotaledMatrix(cData3x2, input$cMatrix3x2)
      
      output$cTable3x2 <- renderDT({
        datatable(cData3x2,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(1, 2, 3)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>% 
          formatStyle(columns = c(0,3), #specify columns to format
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:3,
                      target = 'row',
                      fontWeight = styleRow(dim(cData3x2)[1], "bold"))
      })
      
      probData3x2 <- apply(cData3x2, 2, getProbabilities, t=cData3x2['Total',3])
      
      output$probTable3x2 <- renderDT({
        datatable(probData3x2,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(1, 2, 3)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>% 
          formatStyle(columns = c(0,3), 
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:3,
                      target = 'row',
                      fontWeight = styleRow(dim(probData3x2)[1], "bold"))
      })
      
      cData3x3 <- matrix(cMatrixData3x3(), ncol = ncol(input$cMatrix3x3))
      cData3x3 <- getTotaledMatrix(cData3x3, input$cMatrix3x3)
      
      output$cTable3x3 <- renderDT({
        datatable(cData3x3,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>% 
          formatStyle(columns = c(0,4), #specify columns to format
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:4,
                      target = 'row',
                      fontWeight = styleRow(dim(cData3x3)[1], "bold"))
      })
      
      probData3x3 <- apply(cData3x3, 2, getProbabilities, t=cData3x3['Total',4])
      
      output$probTable3x3 <- renderDT({
        datatable(probData3x3,
                  class = 'cell-border stripe',
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                      list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",) %>% 
          formatStyle(columns = c(0,4), 
                      fontWeight = 'bold') %>%
          formatStyle(columns = 1:4,
                      target = 'row',
                      fontWeight = styleRow(dim(probData3x3)[1], "bold"))
      })
      
      if(input$cTableDimension == '2 x 2') {
        activeProbMatrix <- probData2x2
        activeCMatrix <- cData2x2
      } else if(input$cTableDimension == '2 x 3') {
        activeProbMatrix <- probData2x3
        activeCMatrix <- cData2x3
      } else if(input$cTableDimension == '3 x 2') {
        activeProbMatrix <- probData3x2
        activeCMatrix <- cData3x2
      } else if(input$cTableDimension == '3 x 3') {
        activeProbMatrix <- probData3x3
        activeCMatrix <- cData3x3
      }
      
      output$renderMarginalProbs <- renderUI({
        req(pd_iv$is_valid())
        printMarginalProbs(activeProbMatrix, activeCMatrix)
        
      })
      
      output$renderJointProbs <- renderUI({
        req(pd_iv$is_valid())
        printJointProbs(activeProbMatrix, activeCMatrix)
      })
      
      output$renderUnionProbs <- renderUI({
        req(pd_iv$is_valid())
        printUnionProbs(activeProbMatrix, activeCMatrix)
      })
      
      output$renderConditionalProbs <- renderUI({
        req(pd_iv$is_valid())
        if(ctableconditional_iv$is_valid()) {
          printConditionalProbs(activeCMatrix)
        } else {
          validate("Row and Column totals must be greater than 0 to calculate conditional probabilities.",
                   errorClass = "myClass")
        }
      })
    })
    
    observeEvent(input$resetcTable, {
      ResetCTable("cMatrix2x2", 2, 2, c("R1", "R2"), c("C1", "C2"))
      ResetCTable("cMatrix2x3", 2, 3, c("R1", "R2"), c("C1", "C2", "C3"))
      ResetCTable("cMatrix3x2", 3, 2, c("R1", "R2", "R3"), c("C1", "C2"))
      ResetCTable("cMatrix3x3", 3, 3, c("R1", "R2", "R3"), c("C1", "C2", "C3"))
    })
    
 ### ------------ Binomial ----------------------------------------------------
    
    observeEvent(input$goBinom, {
      
      output$renderProbabilityBinom <- renderUI({
        withMathJax(
          if(!pd_iv$is_valid())
          {
            if(!binomprob_iv$is_valid())
            {
              validate(
                need(input$numTrialsBinom, "Number of Trials (n) must be a positive integer") %then%
                  need(input$numTrialsBinom > 0 && input$numTrialsBinom %% 1 == 0, "Number of Trials (n) must be a positive integer"),
                need(input$successProbBinom, "Probability of Success (p) must be between 0 and 1") %then%
                  need(input$successProbBinom >= 0 && input$successProbBinom <= 1, "Probability of Success (p) must be between 0 and 1"),
                need(input$numSuccessesBinom != "", "Enter a value for the Number of Successes (x)") %then%
                  need(input$numSuccessesBinom >= 0 && input$numSuccessesBinom %% 1 == 0, "Number of Successes (x) must be a positive integer"),
                errorClass = "myClass")
            }
            
            if(!binombetween_iv$is_valid())
            {
              validate(
                need(input$numTrialsBinom, "Number of Trials (n) must be a positive integer") %then%
                  need(input$numTrialsBinom > 0 && input$numTrialsBinom %% 1 == 0, "Number of Trials (n) must be a positive integer"),
                need(input$successProbBinom, "Probability of Success (p) must be between 0 and 1") %then%
                  need(input$successProbBinom >= 0 && input$successProbBinom <= 1, "Probability of Success (p) must be between 0 and 1"),
                need(input$numSuccessesBinomx1, "Number of Successes (x1) must be a positive integer") %then%
                  need(input$numSuccessesBinomx1 >= 0 && input$numSuccessesBinomx1 %% 1 == 0, "Number of Successes (x1) must be a positive integer"),
                need(input$numSuccessesBinomx2, "Enter a value for the Number of Successes (x2)") %then%
                  need(input$numSuccessesBinomx2 >= 0 && input$numSuccessesBinomx2 %% 1 == 0, "Number of Successes (x2) must be a positive integer"),
                errorClass = "myClass")
            }
            
            validate(
              need(input$numTrialsBinom, "Number of Trials (n) must be a positive integer") %then%
                need(input$numTrialsBinom > 0 && input$numTrialsBinom %% 1 == 0, "Number of Trials (n) must be a positive integer"),
              need(input$successProbBinom, "Probability of Success (p) must be between 0 and 1") %then%
                need(input$successProbBinom >= 0 && input$successProbBinom <= 1, "Probability of Success (p) must be between 0 and 1"),
              errorClass = "myClass")
          }
          else
          {
            binom_n <- input$numTrialsBinom
            binom_p <- input$successProbBinom
            binom_mu <- round(binom_n * binom_p, 4)
            binom_var <- round(binom_mu * (1 - binom_p), 4)
            binom_sd <- round(sqrt(binom_var), 4)
            
            if(input$calcBinom != 'between')
            {
              binom_x <- input$numSuccessesBinom
              
              validate(
                need(binom_x <= binom_n, "Number of Successes (x) must be less than or equal to the Number of Trials (n)"),
                errorClass = "myClass")
              
              if(input$calcBinom == 'exact'){
                binomProb <- paste("P(X = ", binom_x, ")") 
                binomForm <- paste("\\binom{", binom_n, "}{", binom_x, "}", binom_p, "^{", binom_x, "}(1-", binom_p, ")^{", binom_n, "-", binom_x, "}")
                binomVal <- round(dbinom(binom_x,binom_n,binom_p), 4)
              }
              else if(input$calcBinom == 'cumulative'){
                binomProb <- paste("P(X \\leq ", binom_x, ")") 
                binomForm <- paste("\\sum_{x = 0}^{", binom_x, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE), 4)
              }
              else if(input$calcBinom == 'upperTail'){
                binomProb <- paste("P(X \\geq ", binom_x, ")") 
                binomForm <- paste("\\sum_{x = ", binom_x, "}^{", binom_n, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE), 4)
              }
              else if(input$calcBinom == 'greaterThan'){
                binomProb <- paste("P(X \\gt ", binom_x, ")") 
                binomForm <- paste("\\sum_{x = ", binom_x + 1, "}^{", binom_n, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE), 4)
              }
              else if(input$calcBinom == 'lessThan'){
                binomProb <- paste("P(X \\lt ", binom_x, ")") 
                binomForm <- paste("\\sum_{x = 0}^{", binom_x - 1, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE), 4)
              }
            }
            else if(input$calcBinom == 'between')
            {
              binom_x1 <- input$numSuccessesBinomx1
              binom_x2 <- input$numSuccessesBinomx2
              
              validate(
                need(binom_x1 <= binom_n, "Number of Successes (x1) must be less than or equal to the Number of Trials (n)"),
                need(binom_x2 <= binom_n, "Number of Successes (x2) must be less than or equal to the Number of Trials (n)"),
                need(binom_x1 <= binom_x2, "Number of Successes (x1) must be less than or equal to Number of Successes (x2)"),
                errorClass = "myClass")
              
              binomProb <- paste("P(", binom_x1, " \\leq X \\leq ", binom_x2, ")")
              binomForm <- paste("\\sum_{x = ", binom_x1, "}^{", binom_x2, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE) - pbinom(binom_x1-1,binom_n,binom_p,lower.tail = TRUE), 4)
            }
            
            tagList(
              withMathJax(
                div(
                  h3(
                    sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Bin(n = %1.0f, p = %g): \\)",
                            binomProb,
                            binom_n,
                            binom_p)),
                  hr(),
                  br(),
                  p(tags$b("Using the Probability Mass Function: ")),
                  sprintf("\\( P(X = x) = \\binom{n}{x} p^x (1-p)^{n-x} \\)"),
                  sprintf("\\( \\qquad \\) for \\( x = 0, 1, 2, ..., n\\)"),
                  br(),
                  br(),
                  br(),
                  sprintf("\\( \\displaystyle %s = %s\\)",
                          binomProb,
                          binomForm),
                  br(),
                  br(),
                  sprintf("\\( %s = %0.4f\\)",
                          binomProb,
                          binomVal),
                  br(),
                  br(),
                  br(),
                  sprintf("Population Mean \\( (\\mu) = np = %g\\)",
                          binom_mu),
                  br(),
                  br(),
                  sprintf("Population Standard Deviation \\( (\\sigma) = \\sqrt{np(1 - p)} = %g\\)",
                          binom_sd),
                  br(),
                  br(),
                  sprintf("Population Variance \\( (\\sigma^{2}) = np(1 - p) = %g\\)",
                          binom_var)
                ),
                br(),
                conditionalPanel(
                  ns = session$ns,
                  condition = "input.showBinomTable == 1",
                  
                  br(),
                  titlePanel("Probability Distribution Table"),
                  hr(),
                  DTOutput(session$ns("binomDistrTable"), width = "25%"),
                  br(),
                  plotOutput(session$ns("binomDistrBarPlot"), width = "50%")
                  )
                ) # withMathJax
              ) # tagList
          }) # withMathJax
      }) # renderProbabilityBinom
      
      output$binomDistrTable <- DT::renderDT({
        req(pd_iv$is_valid())
        
        if(input$numTrialsBinom < 50)
        {
          dfBinom <- data.frame(value = seq(0, input$numTrialsBinom), 
                                value = round(dbinom(x = 0:input$numTrialsBinom, 
                                                     size = input$numTrialsBinom, 
                                                     prob = input$successProbBinom), 4))
          colnames(dfBinom) <- c("X", "P(X = x)")
          
          datatable(dfBinom,
                    options = list(
                      dom = 't',
                      pageLength = -1,
                      ordering = FALSE,
                      searching = FALSE,
                      paging = FALSE
                    ),
                    rownames = FALSE,
                    filter = "none"
          ) %>% formatRound(2, digits = 4)
        }
        else
        {
          dfBinom <- data.frame(value = "Probability distribution table limited to sample sizes less than 50")
          colnames(dfBinom) <- c("Sample Size Too Large")
          datatable(dfBinom,
                    options = list(
                      dom = '',
                      pageLength = -1,
                      ordering = FALSE,
                      searching = FALSE,
                      paging = FALSE
                    ),
                    rownames = FALSE,
                    filter = "none")
        }
      }) # binomDistrTable
    })
    
    output$binomDistrBarPlot <- renderPlot({
      
      req(input$numTrialsBinom < 50)
      
      dfBinom <- data.frame(X = seq(0, input$numTrialsBinom), 
                            P = round(dbinom(x = 0:input$numTrialsBinom, 
                                             size = input$numTrialsBinom, 
                                             prob = input$successProbBinom), 4))
      
      n <- input$numTrialsBinom
      p <- input$successProbBinom
      
      # render as ggplot
      ggplot(dfBinom, aes(x = X, y = P)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(x = bquote(bold("Number of Successes (" * bolditalic(x) * ")")),
             y = bquote(bold("P(" * bolditalic(X == x) * ")")),
             title = bquote(bold("Binomial Distribution: " * bolditalic(X) * " ~ Bin(" * bolditalic(n) * " = " * bold(.(n)) * ", " * bolditalic(p) * " = " * bold(.(p)) * ")"))
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 18, hjust = 0.5),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
              plot.background = element_rect(color = "black", fill = NA, linewidth = 1),
              plot.margin = margin(10, 10, 10, 5, unit = "mm")
        )
      
    })
    
 ### ------------ Poisson -----------------------------------------------------
    observeEvent(input$goPoisson, {
      
      output$renderProbabilityPoisson <- renderUI({
        withMathJax(
          if(!pd_iv$is_valid())
          {
            if(!poissprob_iv$is_valid())
            {
              validate(
                need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
                need(input$xPoisson , "Number of Successes (x) must be a positive integer") %then%
                  need(input$xPoisson >= 0 && input$xPoisson %% 1 == 0, "Number of Successes (x) must be a positive integer"),
                errorClass = "myClass")
            }
            
            if(!poissbetween_iv$is_valid())
            {
              validate(
                need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
                need(input$x1Poisson, "Enter a value for the Number of Successes (x1)") %then%
                  need(input$x1Poisson >= 0 && input$x1Poisson %% 1 == 0, "Number of Successes (x1) must be a positive integer"),
                need(input$x2Poisson, "Enter a value for the Number of Successes (x2)") %then%
                  need(input$x2Poisson >= 0 && input$x2Poisson %% 1 == 0, "Number of Successes (x2) must be a positive integer"),
                errorClass = "myClass")
            }
            
            validate(
              need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
              errorClass = "myClass")
          }
          else
          {
            poisson_mu <- input$muPoisson
            poisson_sd <- round(sqrt(input$muPoisson), 4)
            
            if(input$calcPoisson != 'between')
            {
              poisson_x <- input$xPoisson
              
              if(input$calcPoisson == 'exact'){
                poissProb <- paste("P(X = ", poisson_x, ")") 
                poissForm <- paste("\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^{", poisson_x, "}}{", poisson_x, "!}")
                poissVal <- round(dpois(poisson_x,poisson_mu), 4)
              }
              else if(input$calcPoisson == 'cumulative'){
                poissProb <- paste("P(X \\leq ", poisson_x, ")") 
                poissForm <- paste("\\sum_{x = 0}^{", poisson_x, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = TRUE), 4)
              }
              else if(input$calcPoisson == 'upperTail'){
                poissProb <- paste("P(X \\geq ", poisson_x, ")") 
                poissForm <- paste("1 - \\sum_{x = 0}^{", poisson_x - 1, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x - 1,poisson_mu,lower.tail = FALSE), 4)
              }
              else if(input$calcPoisson == 'greaterThan'){
                poissProb <- paste("P(X \\gt ", poisson_x, ")") 
                poissForm <- paste("1 - \\sum_{x = 0}^{", poisson_x, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = FALSE), 4)
              }
              else if(input$calcPoisson == 'lessThan'){
                poissProb <- paste("P(X \\lt ", poisson_x, ")") 
                poissForm <- paste("\\sum_{x = 0}^{", poisson_x - 1, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x - 1,poisson_mu,lower.tail = TRUE), 4)
              }
            }
            else if(input$calcPoisson == 'between')
            {
              validate(
                need(input$x1Poisson <= input$x2Poisson, "Number of Successes (x1) must be less than or equal to Number of Successes (x2)"),
                errorClass = "myClass")
              
              poisson_x1 <- input$x1Poisson
              poisson_x2 <- input$x2Poisson
              
              poissProb <- paste("P(", poisson_x1, " \\leq X \\leq ", poisson_x2, ")")
              poissForm <- paste("\\sum_{x = ", poisson_x1, "}^{", poisson_x2, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x2, poisson_mu, lower.tail = TRUE) - ppois(poisson_x1 - 1, poisson_mu, lower.tail = TRUE), 4)
            }
            
            tagList(
              withMathJax(
                div(
                  h3(
                    sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Pois(\\mu = %g): \\)",
                            poissProb,
                            poisson_mu)),
                  hr(),
                  br(),
                  p(tags$b("Using the Probability Mass Function: ")),
                  sprintf("\\( P(X = x) = \\dfrac{e^{-\\mu} \\mu^x}{x!} \\)"),
                  sprintf("\\( \\qquad \\) for \\( x = 0, 1, 2, ... \\)"),
                  br(),
                  br(),
                  br(),
                  sprintf("\\( \\displaystyle %s = %s\\)",
                          poissProb,
                          poissForm),
                  br(),
                  br(),
                  sprintf("\\( %s = %0.4f\\)",
                          poissProb,
                          poissVal),
                  br(),
                  br(),
                  br(),
                  sprintf("Population Mean \\( (\\mu) = \\mu = %g\\)",
                          poisson_mu),
                  br(),
                  br(),
                  sprintf("Population Standard Deviation \\( (\\sigma) = \\sqrt{\\mu} = %g\\)",
                          poisson_sd),
                  br(),
                  br(),
                  sprintf("Population Variance \\( (\\sigma^{2}) = \\mu = %g\\)",
                          poisson_mu)
                ),
                br(),
                conditionalPanel(
                  condition = "input.showPoissTable == 1",
                  
                  br(),
                  titlePanel("Probability Distribution Table"),
                  hr(),
                  DTOutput(session$ns("poissDistrTable"), width = "25%"),
                  br()
                  )
                ) # withMathJax
              ) # tagList
          }) # withMathJax
      }) # renderProbabilityPoisson
      
      output$poissDistrTable <- DT::renderDT({
        req(pd_iv$is_valid())
        
        dfPoiss <- data.frame(value = seq(qpois(0.0001, input$muPoisson), qpois(0.9999, input$muPoisson)), value = round(dpois(x = qpois(0.0001, input$muPoisson):qpois(0.9999, input$muPoisson), lambda = input$muPoisson), 4))
        colnames(dfPoiss) <- c("X", "P(X = x)")
        datatable(dfPoiss,
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE
                  ),
                  rownames = FALSE,
                  filter = "none"
        ) %>% formatRound(2, digits = 4)
      }) # poissDistrTable
    }) # goPoisson
 
 ### ------------ Hypergeometric ------------------------------------------

    # IMPORTANT: If (n-x) > (N-M) then P(X = x) = 0, P(X < x) = 0, P(X ≤ x) = 0, P(X > x) = 1, P(X ≥ x) = 1
    
    observeEvent(input$goHypGeo, {
      
      output$renderProbabilityHypGeo <- renderUI({
        withMathJax(
          if(!pd_iv$is_valid())
          {
            if(!HypGeoprob_iv$is_valid())
            {
              validate(
                need(input$popSizeHypGeo , "Population Size (N) must be a positive integer")%then%
                  need(input$popSizeHypGeo > 0 && input$popSizeHypGeo %% 1 == 0, "Population Size (N) must be a positive integer"),
                need(input$popSuccessesHypGeo && input$popSuccessesHypGeo > 0, "Number of Successes in the Population (M) must be a positive integer")%then%
                  need(input$popSuccessesHypGeo %% 1 == 0, "Number of Successes in the Population (M) must be a positive integer"),
                need(input$sampSizeHypGeo && input$sampSizeHypGeo > 0, "Sample Size (n) must be a positive integer")%then%
                  need(input$sampSizeHypGeo %% 1 == 0, "Sample Size (n) must be a positive integer"),
                need(input$xHypGeo , "Number of Successes in the Sample (x) must be a positive integer") %then%
                  need(input$xHypGeo >= 0 && input$xHypGeo %% 1 == 0, "Number of Successes in the Sample (x) must be a positive integer"),
                errorClass = "myClass")
            }

            if(!HypGeobetween_iv$is_valid())
            {
              validate(
                need(input$popSizeHypGeo , "Population Size (N) must be a positive integer")%then%
                  need(input$popSizeHypGeo > 0 && input$popSizeHypGeo %% 1 == 0, "Population Size (N) must be a positive integer"),
                need(input$popSuccessesHypGeo && input$popSuccessesHypGeo > 0, "Number of Successes in the Population (M) must be a positive integer")%then%
                  need(input$popSuccessesHypGeo %% 1 == 0, "Number of Successes in the Population (M) must be a positive integer"),
                need(input$sampSizeHypGeo && input$sampSizeHypGeo > 0, "Sample Size (n) must be a positive integer")%then%
                  need(input$sampSizeHypGeo %% 1 == 0, "Sample Size (n) must be a positive integer"),
                need(input$x1HypGeo , "Number of Successes in the Sample (x1) must be a positive integer") %then%
                  need(input$x1HypGeo >= 0 && input$x1HypGeo %% 1 == 0, "Number of Successes in the Sample (x1) must be a positive integer"),
                need(input$x2HypGeo , "Number of Successes in the Sample (x2) must be a positive integer") %then%
                  need(input$x2HypGeo >= 0 && input$x2HypGeo %% 1 == 0, "Number of Successes in the Sample (x2) must be a positive integer"),
                errorClass = "myClass")
            }

              validate(
                need(input$popSizeHypGeo , "Population Size (N) must be a positive integer")%then%
                  need(input$popSizeHypGeo > 0 && input$popSizeHypGeo %% 1 == 0, "Population Size (N) must be a positive integer"),
                need(input$popSuccessesHypGeo && input$popSuccessesHypGeo > 0, "Number of Successes in the Population (M) must be a positive integer")%then%
                  need(input$popSuccessesHypGeo %% 1 == 0, "Number of Successes in the Population (M) must be a positive integer"),
                need(input$sampSizeHypGeo && input$sampSizeHypGeo > 0, "Sample Size (n) must be a positive integer")%then%
                  need(input$sampSizeHypGeo %% 1 == 0, "Sample Size (n) must be a positive integer"),
                errorClass = "myClass")
          }
          else
          {
            popSizeHypGeo <- input$popSizeHypGeo
            popSuccessesHypGeo <- input$popSuccessesHypGeo
            sampSizeHypGeo <- input$sampSizeHypGeo

            validate(
              need(popSizeHypGeo >= popSuccessesHypGeo, "Number of Successes in the Population (M) must be less than or equal to Population Size (N)"),
              need(popSizeHypGeo >= sampSizeHypGeo, "Sample Size (n) must be less than or equal to Population Size (N)"),
            errorClass = "myClass")

            HypGeo_mu <- round(sampSizeHypGeo*popSuccessesHypGeo/popSizeHypGeo, 4)
            HypGeo_var <- round(sampSizeHypGeo*(popSuccessesHypGeo/popSizeHypGeo)*((popSizeHypGeo - popSuccessesHypGeo)/popSizeHypGeo)*((popSizeHypGeo - sampSizeHypGeo)/(popSizeHypGeo - 1)), 4)
            HypGeo_sd <- round(sqrt(HypGeo_var), 4)

            if(input$calcHypGeo != 'between')
            {
              xHypGeo <- input$xHypGeo

              validate(
                need(xHypGeo <= sampSizeHypGeo, "Number of Successes in the Sample (x) must be less than or equal to the Sample Size (n)"),
                need(xHypGeo <= popSuccessesHypGeo, "Number of Successes in the Sample (x) must be less than or equal to the Number of Successes in the Population (M)"),
                need((sampSizeHypGeo - xHypGeo) <= (popSizeHypGeo - popSuccessesHypGeo), "Since (n - x) > (N - M) the following are true P(X = x) = 0, P(X < x) = 0, P(X ≤ x) = 0, P(X > x) = 1, P(X ≥ x) = 1"),
                errorClass = "myClass")
              
              if(input$calcHypGeo == 'exact'){
                HypGeoProb <- paste("P(X = ", xHypGeo, ")") 
                HypGeoForm <- paste("\\dfrac{\\binom{", popSuccessesHypGeo, "}{", xHypGeo, "}", "\\binom{", (popSizeHypGeo - popSuccessesHypGeo), "}{", (sampSizeHypGeo - xHypGeo), "}}{\\binom{", popSizeHypGeo, "}{", sampSizeHypGeo, "}}")
                HypGeoVal <- round(dhyper(xHypGeo, popSuccessesHypGeo, (popSizeHypGeo - popSuccessesHypGeo), sampSizeHypGeo), 4) # dhyper(x, m, n, k, log = FALSE)
              }
              else if(input$calcHypGeo == 'cumulative'){
                HypGeoProb <- paste("P(X \\leq ", xHypGeo, ")")
                HypGeoForm <- paste("\\sum_{x = ", max(0, sampSizeHypGeo + popSuccessesHypGeo - popSizeHypGeo), "}^{", xHypGeo, "} \\dfrac{\\binom{", popSuccessesHypGeo, "}{x} \\binom{", (popSizeHypGeo - popSuccessesHypGeo), "}{", sampSizeHypGeo,  "- x}}{\\binom{", popSizeHypGeo, "}{", sampSizeHypGeo, "}}")
                HypGeoVal <- round(phyper(xHypGeo, popSuccessesHypGeo, (popSizeHypGeo - popSuccessesHypGeo), sampSizeHypGeo, lower.tail = TRUE), 4)
              }
              else if(input$calcHypGeo == 'upperTail'){
                HypGeoProb <- paste("P(X \\geq ", xHypGeo, ")")
                HypGeoForm <- paste("\\sum_{x =", xHypGeo,"}^{", min(popSuccessesHypGeo, sampSizeHypGeo), "} \\dfrac{\\binom{", popSuccessesHypGeo, "}{x} \\binom{", (popSizeHypGeo - popSuccessesHypGeo), "}{", sampSizeHypGeo,  "- x}}{\\binom{", popSizeHypGeo, "}{", sampSizeHypGeo, "}}")
                HypGeoVal <- round(phyper(xHypGeo - 1, popSuccessesHypGeo, (popSizeHypGeo - popSuccessesHypGeo), sampSizeHypGeo, lower.tail = FALSE), 4)
              }
              else if(input$calcHypGeo == 'greaterThan'){
                HypGeoProb <- paste("P(X \\gt ", xHypGeo, ")")
                HypGeoForm <- paste("\\sum_{x =", xHypGeo + 1,"}^{", min(popSuccessesHypGeo, sampSizeHypGeo), "} \\dfrac{\\binom{", popSuccessesHypGeo, "}{x} \\binom{", (popSizeHypGeo - popSuccessesHypGeo), "}{", sampSizeHypGeo,  "- x}}{\\binom{", popSizeHypGeo, "}{", sampSizeHypGeo, "}}")
                HypGeoVal <- round(phyper(xHypGeo, popSuccessesHypGeo, (popSizeHypGeo - popSuccessesHypGeo), sampSizeHypGeo, lower.tail = FALSE), 4)
              }
              else if(input$calcHypGeo == 'lessThan'){
                HypGeoProb <- paste("P(X \\lt ", xHypGeo, ")")
                HypGeoForm <- paste("\\sum_{x ", max(0, sampSizeHypGeo + popSuccessesHypGeo - popSizeHypGeo), "}^{", xHypGeo - 1, "} \\dfrac{\\binom{", popSuccessesHypGeo, "}{x} \\binom{", (popSizeHypGeo - popSuccessesHypGeo), "}{", sampSizeHypGeo,  "- x}}{\\binom{", popSizeHypGeo, "}{", sampSizeHypGeo, "}}")
                HypGeoVal <- round(phyper(xHypGeo - 1, popSuccessesHypGeo, (popSizeHypGeo - popSuccessesHypGeo), sampSizeHypGeo, lower.tail = TRUE), 4)
              }
            }
            else if(input$calcHypGeo == 'between')
            {
              x1HypGeo <- input$x1HypGeo
              x2HypGeo <- input$x2HypGeo
              
              validate(
                need(x1HypGeo <= sampSizeHypGeo, "Number of Successes in the Sample (x1) must be less than or equal to the Sample Size (n)"),
                need(x2HypGeo <= sampSizeHypGeo, "Number of Successes in the Sample (x2) must be less than or equal to the Sample Size (n)"),
                need(x1HypGeo <= popSuccessesHypGeo, "Number of Successes in the Sample (x1) must be less than or equal to the Number of Successes in the Population (M)"),
                need(x2HypGeo <= popSuccessesHypGeo, "Number of Successes in the Sample (x2) must be less than or equal to the Number of Successes in the Population (M)"),
                need(x1HypGeo <= x2HypGeo, "Number of Successes in the Sample (x1) must be less than or equal to Number of Successes in the Sample (x2)"),
                errorClass = "myClass")
              
                HypGeoProb <- paste("P(", x1HypGeo, " \\leq X \\leq ", x2HypGeo, ")")
                HypGeoForm <- paste("\\sum_{x = ", x1HypGeo, "}^{", x2HypGeo, "} \\dfrac{\\binom{", popSuccessesHypGeo, "}{x} \\binom{", (popSizeHypGeo - popSuccessesHypGeo), "}{", sampSizeHypGeo,  "- x}}{\\binom{", popSizeHypGeo, "}{", sampSizeHypGeo, "}}")
                HypGeoVal <- round(phyper(x2HypGeo, popSuccessesHypGeo, (popSizeHypGeo - popSuccessesHypGeo), sampSizeHypGeo, lower.tail = TRUE) - phyper(x1HypGeo - 1, popSuccessesHypGeo, (popSizeHypGeo - popSuccessesHypGeo), sampSizeHypGeo, lower.tail = TRUE), 4)
              }
            
            tagList(
              withMathJax(
                div(
                  h3(
                    sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Hyper(N = %1.0f, M = %1.0f, n = %1.0f): \\)",
                            HypGeoProb,
                            popSizeHypGeo,
                            popSuccessesHypGeo,
                            sampSizeHypGeo)),
                  hr(),
                  br(),
                  p(tags$b("Using the Probability Mass Function: ")),
                  sprintf("\\( P(X = x) = \\dfrac{\\binom{M}{x} \\binom{N - M}{n - x}}{\\binom{N}{n}} \\)"),
                  sprintf("\\( \\qquad \\) for \\( x = max(0, n + M - N), ..., min(n, M) \\)"),
                  br(),
                  br(),
                  br(),
                  sprintf("\\( \\displaystyle %s = %s\\)",
                          HypGeoProb,
                          HypGeoForm),
                  br(),
                  br(),
                  sprintf("\\( %s = %0.4f\\)",
                          HypGeoProb,
                          HypGeoVal),
                  br(),
                  br(),
                  br(),
                  sprintf("Population Mean \\( (\\mu) = n\\left(\\dfrac{M}{N}\\right) = %g\\)",
                          HypGeo_mu),
                  br(),
                  br(),
                  sprintf("Population Standard Deviation \\( (\\sigma) = \\sqrt{n\\left(\\dfrac{M}{N}\\right)\\left(\\dfrac{N-M}{N}\\right)\\left(\\dfrac{N-n}{N-1}\\right)} = %g\\)",
                          HypGeo_sd),
                  br(),
                  br(),
                  sprintf("Population Variance \\( (\\sigma^{2}) = n\\left(\\dfrac{M}{N}\\right)\\left(\\dfrac{N-M}{N}\\right)\\left(\\dfrac{N-n}{N-1}\\right) = %g\\)",
                          HypGeo_var)
                )
                ,
                br(),
                conditionalPanel(
                  ns = session$ns,
                  condition = "input.showHypGeoTable == 1",

                  br(),
                  titlePanel("Probability Distribution Table"),
                  hr(),
                  DTOutput(session$ns("HypGeoDistrTable"), width = "25%"),
                  br(),
                  plotOutput(session$ns("HypGeoDistrBarPlot"), width = "60%")
                )
              ) # withMathJax
            ) # tagList
          }) # withMathJax
      }) # renderProbabilityHypGeo
      
      output$HypGeoDistrTable <- DT::renderDT({
        req(pd_iv$is_valid())
        
        if(input$sampSizeHypGeo < 50)
        {
          dfHypGeo <- data.frame(value = seq(max(0, input$sampSizeHypGeo + input$popSuccessesHypGeo - input$popSizeHypGeo), min(input$popSuccessesHypGeo, input$sampSizeHypGeo)), 
                                 value = round(dhyper(x = max(0, input$sampSizeHypGeo + input$popSuccessesHypGeo - input$popSizeHypGeo):min(input$popSuccessesHypGeo, input$sampSizeHypGeo), input$popSuccessesHypGeo, (input$popSizeHypGeo - input$popSuccessesHypGeo), input$sampSizeHypGeo), 4))
          colnames(dfHypGeo) <- c("X", "P(X = x)")
          
          datatable(dfHypGeo,
                    options = list(
                      dom = 't',
                      pageLength = -1,
                      ordering = FALSE,
                      searching = FALSE,
                      paging = FALSE
                    ),
                    rownames = FALSE,
                    filter = "none"
          ) %>% formatRound(2, digits = 4)
        }
        else
        {
          dfHypGeo <- data.frame(value = "Probability distribution table limited to sample sizes less than 50")
          colnames(dfHypGeo) <- c("Sample Size Too Large")
          datatable(dfHypGeo,
                    options = list(
                      dom = '',
                      pageLength = -1,
                      ordering = FALSE,
                      searching = FALSE,
                      paging = FALSE
                    ),
                    rownames = FALSE,
                    filter = "none")
        }
      }) # HypGeoDistrTable
    }) # goHypGeo
    
    output$HypGeoDistrBarPlot <- renderPlot({
      
      req(input$sampSizeHypGeo < 50)
      
      dfHypGeo <- data.frame(value = seq(max(0, input$sampSizeHypGeo + input$popSuccessesHypGeo - input$popSizeHypGeo), min(input$popSuccessesHypGeo, input$sampSizeHypGeo)), 
                             prob = round(dhyper(x = max(0, input$sampSizeHypGeo + input$popSuccessesHypGeo - input$popSizeHypGeo):min(input$popSuccessesHypGeo, input$sampSizeHypGeo), input$popSuccessesHypGeo, (input$popSizeHypGeo - input$popSuccessesHypGeo), input$sampSizeHypGeo), 4))
      
      ggplot(dfHypGeo, aes(x = value, y = prob)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(x = bquote(bold("Number of Successes (" * bolditalic(x) * ")")),
             y = bquote(bold("P(" * bolditalic(X == x) * ")")),
             title = bquote(bold("Hypergeometric Distribution: " * bolditalic(X) * " ~ HypGeo(" * bolditalic(N) * " = " * .(input$popSizeHypGeo) * ", " * bolditalic(M) * " = " * .(input$popSuccessesHypGeo) * ", " * bolditalic(n) * " = " * .(input$sampSizeHypGeo) * ")"))
            ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        scale_x_continuous(breaks = seq(min(dfHypGeo$value), max(dfHypGeo$value), by = 1)) +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 18, hjust = 0.5),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
              plot.background = element_rect(color = "black", fill = NA, linewidth = 1),
              plot.margin = margin(10, 10, 10, 5, unit = "mm")
        )
      
    }) #HypGeoDistrBarPlot
    
 ### ------------ Negative Binomial ------------------------------------------

    observeEvent(input$goNegBin, {
      
      output$renderProbabilityNegBin <- renderUI({
        withMathJax(
          if(!pd_iv$is_valid())
          {
            if(!NegBinprob_iv$is_valid())
            {
              validate(
                need(input$successNegBin , "Number of Successes (r) must be a positive integer")%then%
                  need(input$successNegBin >= 0 && input$successNegBin %% 1 == 0, "Number of Successes (r) must be a positive integer"),
                need(input$successProbNegBin, "Probability of Success (p) must be between 0 and 1") %then%
                  need(input$successProbNegBin > 0 && input$successProbNegBin <= 1, "Probability of Success (p) must be 0 < p  ≤ 1"),
                need(input$xNegBin , "Number of Failures (x) must be a positive integer") %then%
                  need(input$xNegBin >= 0 && input$xNegBin %% 1 == 0, "Number of Failures (x) must be a positive integer"),
                errorClass = "myClass")
            }
            
            if(!NegBinbetween_iv$is_valid())
            {
              validate(
                need(input$successNegBin , "Number of Successes (r) must be a positive integer")%then%
                  need(input$successNegBin >= 0 && input$successNegBin %% 1 == 0, "Number of Successes (r) must be a positive integer"),
                need(input$successProbNegBin, "Probability of Success (p) must be between 0 and 1") %then%
                  need(input$successProbNegBin > 0 && input$successProbNegBin <= 1, "Probability of Success (p) must be 0 < p  ≤ 1"),
                need(input$x1NegBin , "Number of Failures (x1) must be a positive integer") %then%
                  need(input$x1NegBin >= 0 && input$x1NegBin %% 1 == 0, "Number of Failures (x1) must be a positive integer"),
                need(input$x2NegBin , "Number of Failures (x2) must be a positive integer") %then%
                  need(input$x2NegBin >= 0 && input$x2NegBin %% 1 == 0, "Number of Failures (x2) must be a positive integer"),
                errorClass = "myClass")
            }
            
            validate(
              need(input$successNegBin , "Number of Successes (r) must be a positive integer")%then%
                need(input$successNegBin > 0 && input$successNegBin %% 1 == 0, "Number of Successes (r) must be a positive integer"),
              need(input$successProbNegBin, "Probability of Success (p) must be between 0 and 1") %then%
                need(input$successProbNegBin >= 0 && input$successProbNegBin <= 1, "Probability of Success (p) must be between 0 and 1"),
              errorClass = "myClass")
          }
          else
          {
            successNegBin <- input$successNegBin
            successProbNegBin <- input$successProbNegBin
            
            NegBin_mu <- round((successNegBin*(1 - successProbNegBin))/successProbNegBin, 4)
            NegBin_var <- round((successNegBin*(1 - successProbNegBin))/(successProbNegBin^2), 4)
            NegBin_sd <- round(sqrt(NegBin_var), 4)

            if(input$calcNegBin != 'between')
            {
              xNegBin <- input$xNegBin

              if(input$calcNegBin == 'exact'){
                NegBinProb <- paste("P(X = ", xNegBin, ")")
                NegBinForm <- paste("\\binom{", xNegBin, "-1}{", successNegBin, "-1} ", successProbNegBin, "^", successNegBin, " (1-", successProbNegBin, ")^{", xNegBin, "-", successNegBin, "}")
                NegBinVal <- round(dnbinom(xNegBin, successNegBin, successProbNegBin), 4)
              }
              # else if(input$calcNegBin == 'cumulative'){
              #   NegBinProb <- paste("P(X \\leq ", xNegBin, ")")
              #   NegBinForm <- paste()
              #   NegBinVal <- round(pnbinom(xNegBin, successNegBin, successProbNegBin, lower.tail = TRUE), 4)
              # }
              # else if(input$calcNegBin == 'upperTail'){
              #   NegBinProb <- paste("P(X \\geq ", xNegBin, ")")
              #   NegBinForm <- paste()
              #   NegBinVal <- round(pnbinom(xNegBin - 1, successNegBin, successProbNegBin, lower.tail = FALSE), 4)
              # }
              # else if(input$calcNegBin == 'greaterThan'){
              #   NegBinProb <- paste("P(X \\gt ", xNegBin, ")")
              #   NegBinForm <- paste()
              #   NegBinVal <- round(pnbinom(xNegBin, successNegBin, successProbNegBin, lower.tail = FALSE), 4)
              # }
              # else if(input$calcNegBin == 'lessThan'){
              #   NegBinProb <- paste("P(X \\lt ", xNegBin, ")")
              #   NegBinForm <- paste()
              #   NegBinVal <- round(pnbinom(xNegBin - 1, successNegBin, successProbNegBin, lower.tail = TRUE), 4)
              # }
            }
            else if(input$calcNegBin == 'between')
            {
              x1NegBin <- input$x1NegBin
              x2NegBin <- input$x2NegBin

              validate(
                need(x1NegBin <= x2NegBin, "Number of Failures (x1) must be less than or equal to Number of Failures (x2)"),
                errorClass = "myClass")

            }

          tagList(
            withMathJax(
              div(
                h3(
                  sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim NegBin(r = %1.0f, p = %g): \\)",
                          NegBinProb,
                          successNegBin,
                          successProbNegBin
                  )),
                hr(),
                br(),
                p(tags$b("Using the Probability Mass Function: ")),
                sprintf("\\( P(X = x) = \\binom{x-1}{r-1} p^r (1-p)^{x-r} \\)"),
                sprintf("\\( \\qquad \\) for \\( x = r, r+1, ... \\)"),
                br(),
                br(),
                br(),
                sprintf("\\( \\displaystyle %s = %s\\)",
                        NegBinProb,
                        NegBinForm),
                br(),
                br(),
                sprintf("\\( %s = %0.4f\\)",
                        NegBinProb,
                        NegBinVal),
                br(),
                br(),
                br(),
                sprintf("Population Mean \\( (\\mu) = \\dfrac{r(1 - p)}{p} = %g\\)",
                        NegBin_mu),
                br(),
                br(),
                sprintf("Population Standard Deviation \\( (\\sigma) = \\sqrt{\\dfrac{r(1 - p)}{p^2}} = %g\\)",
                        NegBin_sd),
                br(),
                br(),
                sprintf("Population Variance \\( (\\sigma^{2}) = \\dfrac{r(1 - p)}{p^2} = %g\\)",
                        NegBin_var)
              )
              #,
              # br(),
              # conditionalPanel(
              #   ns = session$ns,
              #   condition = "input.showNegBinTable == 1",
              #
              #   br(),
              #   titlePanel("Probability Distribution Table"),
              #   hr(),
              #   DTOutput(session$ns("NegBinDistrTable"), width = "25%"),
              #   br(),
              #   plotOutput(session$ns("NegBinDistrBarPlot"), width = "50%")
              # )
            ) # withMathJax
          ) # tagList
         }) # withMathJax
      }) # renderProbabilityNegBin
    }) # goNegBin

 ### ------------ Normal ------------------------------------------------------
    observeEvent(input$goNormalProb, {
      
      #### ---------------- Normal Probability -------------------------------------
      output$renderProbabilityNorm <- renderUI({
        if(!pd_iv$is_valid())
        {
          if(!normprob_iv$is_valid())
          {
            validate(
              need(input$popMean, "Enter a value for Population Mean (mu)"),
              need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
              need(input$xValue, "Enter a value for Normally Distributed Variable (x)"),
              errorClass = "myClass")
          }
          
          if(!normbetween_iv$is_valid())
          {
            validate(
              need(input$popMean, "Enter a value for Population Mean (mu)"),
              need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
              need(input$x1Value, "Enter a value for Normally Distributed Variable (x1)"),
              need(input$x2Value, "Enter a value for Normally Distributed Variable (x2)"),
              errorClass = "myClass")
          }
          
          validate(
            need(input$popMean, "Enter a value for Population Mean (mu)"),
            need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
            errorClass = "myClass")
        }
        
        norm_mu <- input$popMean
        norm_sigma <- input$popSD
        
        if(input$calcNormal != 'between')
        {
          norm_x <- input$xValue
          probValue <- round((norm_x - norm_mu)/norm_sigma, 4)
          
          if(input$calcNormal == "cumulative"){
            normProb <- paste("P(X \\leq ", norm_x,")")
            normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\leq \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
            normForm <- paste("= P(Z \\leq", probValue, ")")
          }
          else if(input$calcNormal == "upperTail"){
            normProb <- paste("P(X \\gt ", norm_x,")")
            normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\gt \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
            normForm <- paste("= P(Z \\gt", probValue, ")")
          }
        }
        else if(input$calcNormal == 'between')
        {
          norm_x1 <- input$x1Value
          norm_x2 <- input$x2Value
          
          validate(
            need(norm_x1 <= norm_x2, "Normally Distributed Variable (x1) must be less than or equal to Normally Distributed Variable (x2)"),
            errorClass = "myClass")
          
          normProb <- paste("P(", norm_x1, " ",  " \\leq X \\leq"," ", norm_x2,")") 
          normProbTransform <- paste("P \\left( \\dfrac{", norm_x1, " - ", norm_mu, "}{", norm_sigma, "} \\leq \\dfrac{X - \\mu}{\\sigma} \\leq",
                                     "\\dfrac{", norm_x2, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
          normForm <- paste("= P(", round((norm_x1 - norm_mu)/norm_sigma, 4), "\\leq Z \\leq", round((norm_x2 - norm_mu)/norm_sigma, 4), ") = ", 
                            round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE), 4), " - ", round(pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4))
        }
        
        tagList(
          withMathJax(
            div(
              h3(
                sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim N(\\mu = %g, \\sigma = %g): \\)",
                        normProb,
                        norm_mu,
                        norm_sigma)),
              hr(),
              br(),
              sprintf("\\( \\displaystyle %s = %s\\)",
                      normProb,
                      normProbTransform),
              br(),
              br(),
              sprintf("\\( \\displaystyle %s = %g\\)",
                      normForm,
                      getNormValue()),
              br(),
              br(),
              br(),
              sprintf("Population Mean \\( (\\mu) = %g\\)",
                      norm_mu),
              br(),
              br(),
              sprintf("Population Standard Deviation \\( (\\sigma) = %g\\)",
                      norm_sigma),
              br(),
              br(),
              sprintf("Population Variance \\( (\\sigma^{2}) = %g\\)",
                      norm_sigma^2)
            ),
            br(),
            hr(),
            br(),
            fluidRow(
              column(
                width = 6,
                plotOutput(session$ns('normDistrPlot'))
                ),
              column(
                width = 6,
                plotOutput(session$ns('normZPlot'))
                )
              ),# fluidRow
            br(),
            br()
          )
        )
      })
      
      output$normDistrPlot <- renderPlot({
        req(pd_iv$is_valid())
        
        if(input$calcNormal == "between") {
          normLines <- c(round((input$x1Value - input$popMean)/input$popSD, 4), round((input$x2Value - input$popMean)/input$popSD, 4))
          lineLabels <- c(input$x1Value, input$x2Value) 
        } else {
          normLines <- round((input$xValue - input$popMean)/input$popSD, 4)
          lineLabels <- input$xValue
        }
        normPlot(getNormValue(), normLines, input$popMean, input$popSD^2, input$popSD, lineLabels, input$calcNormal)
      })
      
      output$normZPlot <- renderPlot({
        req(pd_iv$is_valid())
        
        if(input$calcNormal == "between") {
          req(input$x1Value <= input$x2Value)
          normLines <- c(round((input$x1Value - input$popMean)/input$popSD, 4), round((input$x2Value - input$popMean)/input$popSD, 4))
        } else {
          normLines <- round((input$xValue - input$popMean)/input$popSD, 4)
        }
        normZPlot(getNormValue(), normLines, input$calcNormal)
      })
      
      #### ---------------- Sampling Distribution of the Sample Mean ---------------
      output$renderSampMeanDistr <- renderUI({
        if(!pd_iv$is_valid()) {
          if(!sampdistrprob_iv$is_valid()) {
            withMathJax()
            validate(
              need(input$popMean, "Enter a value for Population Mean (mu)."),
              need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
              need(input$sampDistrxValue, "Enter a value for Normally Distributed Variable (x)."),
              need(input$sampDistrSize > 0 && input$sampDistrSize %% 1 == 0, "Sample Size (n) must be a positive integer."),
              errorClass = "myClass")
          }
          
          if(!sampdistrbetween_iv$is_valid()) {
            validate(
              need(input$popMean, "Enter a value for Population Mean (mu)."),
              need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
              need(input$sampDistrx1Value, "Enter a value for Normally Distributed Variable (x1)."),
              need(input$sampDistrx2Value, "Enter a value for Normally Distributed Variable (x2)."),
              need(input$sampDistrSize > 0 && input$sampDistrSize %% 1 == 0, "Sample Size (n) must be a positive integer."),
              errorClass = "myClass")
          }
          
          validate(
            need(input$popMean, "Enter a value for Population Mean (mu)."),
            need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
            need(input$sampDistrSize > 0 && input$sampDistrSize %% 1 == 0, "Sample Size (n) must be a positive integer."),
            errorClass = "myClass")
        }
        
        if(input$calcNormSampDistr != 'between') {
          probValue <- round((input$sampDistrxValue - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4)
          
          if(input$calcNormSampDistr == "cumulative"){
            normProb <- paste("P(\\bar{X} \\leq ", input$sampDistrxValue,")")
            normProbTransform <- paste("P \\left( \\dfrac{\\bar{X} - \\mu}{ \\left( \\dfrac{\\sigma}{\\sqrt{n}} \\right) } \\leq \\dfrac{", input$sampDistrxValue, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\right)")
            normForm <- paste("= P(Z \\leq", probValue, ")")
          } else if(input$calcNormSampDistr == "upperTail"){
            normProb <- paste("P(\\bar{X} \\gt ", input$sampDistrxValue,")")
            normProbTransform <- paste("P \\left( \\dfrac{\\bar{X} - \\mu}{ \\left( \\dfrac{\\sigma}{\\sqrt{n}} \\right) } \\gt \\dfrac{", input$sampDistrxValue, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\right)")
            normForm <- paste("= P(Z \\gt", probValue, ")")
          }
        } else if(input$calcNormSampDistr == 'between') {
          norm_x1 <- input$sampDistrx1Value
          norm_x2 <- input$sampDistrx2Value
          sampSE <- input$popSD / sqrt(input$sampDistrSize)
          
          validate(
            need(norm_x1 <= norm_x2, "Normally Distributed Variable (x1) must be less than or equal to Normally Distributed Variable (x2)"),
            errorClass = "myClass")
          
          normProb <- paste("P(", norm_x1, " ",  " \\leq \\bar{X} \\leq"," ", norm_x2,")")
          
          normProbTransform <- paste("P \\left( \\dfrac{", norm_x1, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\leq \\dfrac{\\bar{X} - \\mu}{ \\left( \\dfrac{\\sigma}{\\sqrt{n}} \\right) } \\leq",
                                     "\\dfrac{", norm_x2, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\right)")
          normForm <- paste("= P(", round((norm_x1 - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), "\\leq Z \\leq", round((norm_x2 - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), ") = ", 
                            round(pnorm(norm_x2, input$popMean, sampSE, lower.tail = TRUE), 4), " - ", round(pnorm(norm_x1, input$popMean, sampSE, lower.tail = TRUE), 4))
        }
        
        sampMeanDistrSD <- input$popSD / sqrt(input$sampDistrSize)
        
        tagList(
          withMathJax(
            div(
              h3(
                sprintf("Calculating  \\( %s \\)   when  \\(  \\bar{X} \\sim N(\\mu_{\\bar{X}} = \\mu = %g, \\, \\sigma_{\\bar{X}} = \\dfrac{\\sigma}{\\sqrt{n}} = %0.4f): \\)",
                        normProb,
                        input$popMean,
                        sampMeanDistrSD)
                ),
              hr(),
              br(),
              sprintf("\\( \\displaystyle %s = %s\\)",
                      normProb,
                      normProbTransform),
              br(),
              br(),
              sprintf("\\( \\displaystyle %s = %g\\)",
                      normForm,
                      getMeanNormValue()),
              br(),
              br(),
              br(),
              sprintf("Mean \\( (\\mu_{\\bar{X}}) = \\mu = %g\\)",
                      input$popMean),
              br(),
              br(),
              sprintf("Standard Deviation \\( (\\sigma_{\\bar{X}}) = \\dfrac{\\sigma}{\\sqrt{n}} = %0.4f\\)",
                      sampMeanDistrSD),
              br(),
              sprintf("Variance \\( (\\sigma_{\\bar{X}}^2) = \\dfrac{\\sigma^{2}}{n} = %g\\)",
                      input$popSD^2 / input$sampDistrSize)
              ),
            br(),
            hr(),
            br(),
            fluidRow(
              column(width = 6,
                     plotOutput(session$ns('sampMeanDistrPlot'))),
              column(width = 6,
                     plotOutput(session$ns('sampMeanZPlot')))
              ),
            br(),
            br()
          )
        )# tagList
      })

      output$sampMeanDistrPlot <- renderPlot({
        req(pd_iv$is_valid())
        
        withMathJax()
        sampSE <- round(input$popSD / sqrt(input$sampDistrSize), 4)
        
        if(input$calcNormSampDistr == "between") {
          req(input$sampDistrx1Value <= input$sampDistrx2Value)
          normLines <- c(round((input$sampDistrx1Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), 
                         round((input$sampDistrx2Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4))
          lineLabels <- c(input$sampDistrx1Value, input$sampDistrx2Value)
        } else {
          normLines <- round((input$sampDistrxValue - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4)
          lineLabels <- c(input$sampDistrxValue)
        }
        normPlot(getMeanNormValue(), normLines, input$popMean, round(input$popSD^2 / input$sampDistrSize, 4), sampSE, lineLabels, input$calcNormSampDistr)
      })

      output$sampMeanZPlot <- renderPlot({
        req(pd_iv$is_valid())
        
        if(input$calcNormSampDistr == "between") {
          req(input$sampDistrx1Value <= input$sampDistrx2Value)
          normLines <- c(round((input$sampDistrx1Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), 
                         round((input$sampDistrx2Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4))
        } else {
          normLines <- round((input$sampDistrxValue - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4)
        }
        normZPlot(getMeanNormValue(), normLines, input$calcNormSampDistr)
      })
    })
    
    observeEvent(input$goNormalQuan, {

      #### ---------------- Quartiles ----------------------------------------------
      output$renderNormQuartiles <- renderUI({
        validate(
          need(input$popMean, "Enter a value for Population Mean (mu)"),
          need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
          errorClass = "myClass")
        
        qOne <- round(qnorm(0.25, input$popMean, input$popSD, TRUE), 4)
        qTwo <- round(qnorm(0.5, input$popMean, input$popSD, TRUE), 4)
        qThree <- round(qnorm(0.75, input$popMean, input$popSD, TRUE), 4)
        
        tagList(
          withMathJax(
            div(
              h3(
                sprintf("Given \\( X \\sim N(\\mu  = %g, \\sigma = %g) \\) then",
                        input$popMean,
                        input$popSD)
                ),
              hr(),
              br(),
              br(),
              fluidRow(
                column(width = 5,
                       div(style = "padding-top: 60px;",
                           sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right) \\)",
                                   input$popMean,
                                   input$popSD),
                           br(),
                           br(),
                           sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le -0.6745) = 0.25\\)"),
                           br(),
                           br(),
                           sprintf("Quartile 1 \\( \\displaystyle (Q_{1}) \\) is obtained by solving for \\(x\\)"),
                           br(),
                           br(),
                           sprintf("\\( \\displaystyle x = %s + (-0.6745 \\times %s) = %s\\)",
                                   input$popMean,
                                   input$popSD,
                                   qOne),
                           br(),
                           br(),
                           br())),
                column(width = 7,
                       plotOutput(session$ns("quartile1Plot"), height = "300px"),
                       br())
                ),# fluidRow
              hr(),
              br(),
              fluidRow(
                column(
                  width = 5,
                  div(
                    style = "padding-top: 60px;",
                    sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right) \\)",
                            input$popMean,
                            input$popSD),
                    br(),
                    br(),
                    sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le 0) = 0.50\\)"),
                    br(),
                    br(),
                    sprintf("Quartile 2 \\( \\displaystyle (Q_{2}) \\) is obtained by solving for \\(x\\)"),
                    br(),
                    br(),
                    sprintf("\\( \\displaystyle x = %s + (0 \\times %s) = %s\\)",
                            input$popMean,
                            input$popSD,
                            qTwo),
                    br(),
                    br(),
                    br()
                    )
                  ),
                column(
                  width = 7,
                  plotOutput(session$ns("quartile2Plot"), height = "300px"),
                  br()
                  )
                ),# fluidRow
              hr(),
              br(),
              fluidRow(
                column(
                  width = 5,
                  div(
                    style = "padding-top: 60px;",
                    sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right)\\)",
                            input$popMean,
                            input$popSD),
                    br(),
                    br(),
                    sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le 0.6745) = 0.75\\)"),
                    br(),
                    br(),
                    sprintf("Quartile 3 \\( \\displaystyle (Q_{3}) \\) is obtained by solving for \\(x\\)"),
                    br(),
                    br(),
                    sprintf("\\( \\displaystyle x = %s + (0.6745 \\times %s) = %s\\)",
                            input$popMean,
                            input$popSD,
                            qThree),
                    br(),
                    br())),
                column(
                  width = 7,
                  plotOutput(session$ns("quartile3Plot"), height = "300px"),
                  br(),
                  br())
                )# fluidRow
              ),
            br(),
            br())
          )# tagList
      })
      
      output$quartile1Plot <- renderPlot({
        req(pd_iv$is_valid())
        
        probability <- 0.25
        probLine <- round(qnorm(0.25, input$popMean, input$popSD, TRUE), 4)
        xStart <- input$popMean - (3 * input$popSD)
        xEnd <- input$popMean + (3 * input$popSD)
        
        x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
        xSeq <- unique(sort(c(x, input$popMean, probLine)))
        
        df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
        meanDF <- filter(df, x %in% c(input$popMean))
        lineDF <- filter(df, x %in% c(probLine))
        
        nPlot <- ggplot(df, aes(x = x, y = y)) +
          geom_line(linetype = "solid",
                    linewidth = 0.75,
                    color='#021C38') +
          geom_area(data = df,
                    aes(y=y), 
                    fill = NA, 
                    color = NA) +
          geom_area(data = subset(df, x <= probLine),
                    aes(y=y), 
                    fill = "#023B70", 
                    color = NA, 
                    alpha = 0.4) +
          geom_segment(data = lineDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "solid",
                       lineend = 'round',
                       linewidth = 1.25,
                       color='#021C38') +
          geom_text(data = lineDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = meanDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "dotted",
                       lineend = 'round',
                       linewidth = 1,
                       color='#021C38',
                       alpha = 0.5) +
          geom_text(data = meanDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = df,
                       aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                       linetype = "solid",
                       linewidth = 0.5,
                       color='#021C38') +
          coord_cartesian(clip="off") +
          theme_minimal()  +
          theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
                axis.text.x.bottom = element_text(size = 14)) +
          scale_x_continuous(breaks = NULL) +
          scale_y_continuous(breaks = NULL) +
          ylab("") +
          xlab("X") 
        
        nPlot
      })
      
      output$quartile2Plot <- renderPlot({
        req(pd_iv$is_valid())
        
        probability <- 0.5
        probLine <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
        xStart <- input$popMean - (3 * input$popSD)
        xEnd <- input$popMean + (3 * input$popSD)
        
        x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
        xSeq <- unique(sort(c(x, input$popMean, probLine)))
        
        df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
        meanDF <- filter(df, x %in% c(input$popMean))
        lineDF <- filter(df, x %in% c(probLine))
        
        nPlot <- ggplot(df, aes(x = x, y = y)) +
          geom_line(linetype = "solid",
                    linewidth = 0.75,
                    color='#021C38') +
          geom_area(data = df,
                    aes(y=y), 
                    fill = NA, 
                    color = NA) +
          geom_area(data = subset(df, x <= probLine),
                    aes(y=y), 
                    fill = "#023B70", 
                    color = NA, 
                    alpha = 0.4) +
          geom_segment(data = lineDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "solid",
                       lineend = 'round',
                       linewidth = 1.25,
                       color='#021C38') +
          geom_text(data = lineDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = meanDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "dotted",
                       lineend = 'round',
                       linewidth = 1,
                       color='#021C38',
                       alpha = 0.5) +
          geom_text(data = meanDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = df,
                       aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                       linetype = "solid",
                       linewidth = 0.5,
                       color='#021C38') +
          coord_cartesian(clip="off") +
          theme_minimal()  +
          theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
                axis.text.x.bottom = element_text(size = 14)) +
          scale_x_continuous(breaks = NULL) +
          scale_y_continuous(breaks = NULL) +
          ylab("") +
          xlab("X") 
        
        nPlot
      })
      
      output$quartile3Plot <- renderPlot({
        req(pd_iv$is_valid())
        
        probability <- 0.75
        probLine <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
        xStart <- input$popMean - (3 * input$popSD)
        xEnd <- input$popMean + (3 * input$popSD)
        
        x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
        xSeq <- unique(sort(c(x, input$popMean, probLine)))
        
        df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
        meanDF <- filter(df, x %in% c(input$popMean))
        lineDF <- filter(df, x %in% c(probLine))
        
        nPlot <- ggplot(df, aes(x = x, y = y)) +
          geom_line(linetype = "solid",
                    linewidth = 0.75,
                    color='#021C38') +
          geom_area(data = df,
                    aes(y=y), 
                    fill = NA, 
                    color = NA) +
          geom_area(data = subset(df, x <= probLine),
                    aes(y=y), 
                    fill = "#023B70", 
                    color = NA, 
                    alpha = 0.4) +
          geom_segment(data = lineDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "solid",
                       lineend = 'round',
                       linewidth = 1.25,
                       color='#021C38') +
          geom_text(data = lineDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = meanDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "dotted",
                       lineend = 'round',
                       linewidth = 1,
                       color='#021C38',
                       alpha = 0.5) +
          geom_text(data = meanDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = df,
                       aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                       linetype = "solid",
                       linewidth = 0.5,
                       color='#021C38') +
          coord_cartesian(clip="off") +
          theme_minimal()  +
          theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
                axis.text.x.bottom = element_text(size = 14)) +
          scale_x_continuous(breaks = NULL) +
          scale_y_continuous(breaks = NULL) +
          ylab("") +
          xlab("X") 
        
        nPlot
      })
      
      output$renderNormPercentile <- renderUI({
        validate(
          need(input$popMean, "Enter a value for Population Mean (mu)."),
          need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
          need(input$percentileValue, "Enter a Percentile Value between 0 and 100."),
          errorClass = "myClass")
        
        validate(
          need(input$percentileValue > 0 && input$percentileValue < 100, "Percentile Value must be between 0 and 100"),
          errorClass = "myClass")
        
        if(input$percentileValue %% 10 == 1) {
          ordinal <- 'st'
        } else if(input$percentileValue %% 10 == 2) {
          ordinal <- 'nd'
        } else if(input$percentileValue %% 10 == 3) {
          ordinal <- 'rd'
        } else {
          ordinal <- 'th'
        }
        
        probability <- (input$percentileValue / 100)
        zVal <- round(qnorm(probability, 0, 1, TRUE), 4)
        percentile <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
        
        tagList(
          withMathJax(
            div(
              h3(
                sprintf("Given \\( X \\sim N(\\mu  = %g, \\sigma = %g) \\) then",
                        input$popMean,
                        input$popSD)),
              hr(),
              br(),
              br(),
              fluidRow(
                column(width = 5,
                       div(style = "padding-top: 60px;",
                           sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right)\\)",
                                   input$popMean,
                                   input$popSD),
                           br(),
                           br(),
                           sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le %s) = %s\\)",
                                   zVal,
                                   probability),
                           br(),
                           br(),
                           sprintf("the \\( \\displaystyle %d^{%s} \\) percentile is obtained by solving for \\(x\\)",
                                   input$percentileValue,
                                   ordinal),
                           br(),
                           br(),
                           sprintf("\\( \\displaystyle x = %s + (%s \\times %s) = %s\\)",
                                   input$popMean,
                                   zVal,
                                   input$popSD,
                                   percentile),
                           br(),
                           br(),
                           br(),
                       ),
                ),
                column(width = 7,
                       plotOutput(session$ns("percentilePlot"), height = "300px"),
                       br(),
                       br())
                )# fluidRow
              ),
            br(),
            br())
          )# tagList
      })
      
      output$percentilePlot <- renderPlot({
        req(pd_iv$is_valid())
        
        probability <- input$percentileValue / 100
        percentileLine <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
        xStart <- input$popMean - (3 * input$popSD)
        xEnd <- input$popMean + (3 * input$popSD)
        
        x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
        xSeq <- unique(sort(c(x, input$popMean, percentileLine)))
        
        df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
        meanDF <- filter(df, x %in% c(input$popMean))
        lineDF <- filter(df, x %in% c(percentileLine))
        
        nPlot <- ggplot(df, aes(x = x, y = y)) +
          geom_line(linetype = "solid",
                    linewidth = 0.75,
                    color='#021C38') +
          geom_area(data = df,
                    aes(y=y), 
                    fill = NA, 
                    color = NA) +
          geom_area(data = subset(df, x <= percentileLine),
                    aes(y=y), 
                    fill = "#023B70", 
                    color = NA, 
                    alpha = 0.4) +
          geom_segment(data = lineDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "solid",
                       lineend = 'round',
                       linewidth = 1,
                       color='#021C38') +
          geom_text(data = lineDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = meanDF,
                       aes(x = x, xend = x, y = 0, yend = y),
                       linetype = "dotted",
                       lineend = 'round',
                       linewidth = 1,
                       color='#021C38',
                       alpha = 0.5) +
          geom_text(data = meanDF, 
                    aes(x = x, y = 0, label = x), 
                    size = 16 / .pt,
                    fontface = "bold",
                    check_overlap = TRUE,
                    vjust = 1.5) +
          geom_segment(data = df,
                       aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                       linetype = "solid",
                       linewidth = 0.5,
                       color='#021C38') +
          coord_cartesian(clip="off") +
          theme_minimal()  +
          theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                axis.title.x = element_text(size = 18, face = "bold", vjust = -1),
                axis.text.x.bottom = element_text(size = 14)) +
          scale_x_continuous(breaks = NULL) +
          scale_y_continuous(breaks = NULL) +
          ylab("") +
          xlab("X") 
        
        nPlot
      })
    })
    
 ### ------------ Component Display -------------------------------------------
    
    observeEvent({
      input$cTableDimension
      input$cTableType
      input$cMatrix2x2
      input$cMatrix2x3
      input$cMatrix3x2
      input$cMatrix3x3}, {
        hide('probabilityMP')
      })
    
    observeEvent(input$gocTable, {
      show(id = 'probabilityMP')
    })
    
    #-----------------------#
    # Binomial Distribution #
    #-----------------------#
    
    observeEvent(input$goBinom, {
      show(id = 'probabilityMP')
    })
    
    observeEvent({input$numTrialsBinom
      input$successProbBinom
      input$numSuccessesBinom
      input$numSuccessesBinomx1
      input$numSuccessesBinomx2}, {
        hide(id = 'probabilityMP')
      })
    
    observeEvent(input$calcBinom, {
      if(input$calcBinom == 'between') {
        hide(id = "probabilityMP")
      }
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
    
    observeEvent({input$muPoisson
      input$xPoisson
      input$x1Poisson
      input$x2Poisson}, {
        hide(id = 'probabilityMP')
      })
    
    observeEvent(input$calcPoisson, {
      if(input$calcPoisson == 'between') {
        hide(id = "probabilityMP")
      }
    })
    
    observeEvent(input$resetPoisson, {
      hide(id = "probabilityMP")
      shinyjs::reset("poissonPanel")
    })
    
    #-----------------------------#
    # Hypergeometric Distribution #
    #-----------------------------#
    
    observeEvent(input$goHypGeo, {
      show(id = "probabilityMP")
    })
    
    observeEvent({input$popSizeHypGeo
      input$popSuccessesHypGeo
      input$sampSizeHypGeo
      input$xHypGeo
      input$x1HypGeo
      input$x2HypGeo}, {
        hide(id = 'probabilityMP')
      })
    
    observeEvent(input$calcHypGeo, {
      if(input$calcHypGeo == 'between') {
        hide(id = "probabilityMP")
      }
    })
    
    observeEvent(input$resetHypGeo, {
      hide(id = "probabilityMP")
      shinyjs::reset("HypGeoPanel")
    })
    
    #--------------------------------#
    # Negative Binomial Distribution #
    #--------------------------------#

    observeEvent(input$goNegBin, {
      show(id = "probabilityMP")
    })
    
    observeEvent({input$successNegBin
      input$successProbNegBin
      input$xNegBin
      #input$x1NegBin
      #input$x2NegBin
      }, {
        hide(id = 'probabilityMP')
      })
    
    observeEvent(input$calcNegBin, {
      if(input$calcNegBin == 'between') {
        hide(id = "probabilityMP")
      }
    })

    observeEvent(input$resetNegBin, {
      hide(id = "probabilityMP")
      shinyjs::reset("NegBinPanel")
    })

    #---------------------#
    # Normal Distribution #
    #---------------------#
    
    observeEvent(input$goNormalProb, {
      show(id = "probabilityMP")
    })
    
    observeEvent(input$goNormalQuan, {
      show(id = "probabilityMP")
    })
    
    observeEvent({input$popMean
      input$popSD
      input$xValue
      input$x1Value
      input$x2Value
      input$sampMeanDistr
      input$sampDistrxValue
      input$sampDistrx1Value
      input$sampDistrx2Value
      input$sampDistrSize
      input$calcQuantiles
      input$percentileValue}, {
        hide(id = 'probabilityMP')
      })
    
    observeEvent(input$calcQuartiles, {
      if(input$calcQuartiles == 'Percentile') {
        hide(id = "probabilityMP")
      }
    })
    
    observeEvent(input$calcNormal, {
      if(input$calcNormal == 'between') {
        hide(id = "probabilityMP")
      }
    })
    
    observeEvent(input$calcNormSampDistr, {
      if(input$calcNormSampDistr == 'between') {
        hide(id = "probabilityMP")
      }
    })
    
    observeEvent(input$resetNormalProb, {
      hide(id = "probabilityMP")
      shinyjs::reset("normalPanel")
    })
    
    observeEvent(input$resetNormalQuan, {
      hide(id = "probabilityMP")
      shinyjs::reset("normalPanel")
    })
  })
}