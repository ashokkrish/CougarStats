library(aplpack)
library(base)
library(bslib)
library(car)
library(dplyr)
library(DT)
library(generics)
library(ggplot2)
library(e1071)
library(nortest)
library(readr)
library(readxl)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyvalidate)
library(shinyWidgets)
library(tinytex)
library(tools)
library(writexl)
library(xtable)
library(MASS)

options(scipen = 999) # options(scipen = 0)

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

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
      .dropdown-item:hover,
      .dropdown-menu>li>a:hover,
      .dropdown-item:focus,
      .dropdown-menu>li>a:focus {
        color:#fff;
        text-decoration:none;
        background-color:#428BCA
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
                                     choices = c("", 
                                                 "Descriptive Statistics", 
                                                 "Probability Distributions", 
                                                 "Statistical Inference", 
                                                 "Regression and Correlation"),
                                     options = list(
                                       placeholder = 'Please select a topic below'),
                      ),
                      
                      
                      #   ----------------------------------- #  
                      ### ---- Descriptive Stats sidebar ---- 
                      #   ----------------------------------- #
                      conditionalPanel(id = "descriptiveStatsPanel",
                                       condition = "input.dropDownMenu == 'Descriptive Statistics'",
                                       style = "display: none;",
                        
                        radioButtons(inputId = "dataInput",
                                     label = strong("Data"),
                                     choiceValues = list("Enter Raw Data", 
                                                         "Upload Data"),
                                     choiceNames = list("Enter Raw Data", 
                                                        "Upload Data"),
                                     selected = "Enter Raw Data", #character(0), #
                                     inline = TRUE), #,width = '1000px'),
                        
                        conditionalPanel(
                          condition = "input.dataInput == 'Enter Raw Data'",
                        
                          textAreaInput("descriptiveStat", 
                                        label = strong("Sample"), 
                                        value = "2.14,   2.09,   2.65,   3.56,   5.55,   5.00,   5.55,   8.09,   10.79", 
                                        placeholder = "Enter values separated by a comma with decimals as points", 
                                        rows = 3),
                        ),
                        
                        conditionalPanel(
                          condition = "input.dataInput == 'Upload Data'",
                          
                          fileInput(inputId = 'dsUserData', 
                                    label = 'Upload your data (.csv or .xls or .xlsx)',
                                    accept = c('text/csv','text/comma-separated-values',
                                               'text/tab-separated-values',
                                               'text/plain',
                                               '.csv',
                                               '.txt',
                                               '.xls',
                                               '.xlsx')
                                    ),
                          
                          selectizeInput(
                                         inputId = "dsUploadVars",
                                         label = strong("Choose a Variable"),
                                         choices = c(""),
                                         multiple = TRUE,
                                         options = list(
                                           placeholder = 'Select a variable',
                                           onInitialize = I('function() { this.setValue(""); }')
                                         )
                          ),
                        ),
                        br(),
                        #p(strong("Display Options")),
                        #hr(),

                        
                        
                        pickerInput(
                                    inputId = "dsTableFilters",
                                    label = strong("Options"), 
                                    choices = list(
                                                  Descriptives = c("Observations", 
                                                                   "Sum", 
                                                                   "Sum of Squares", 
                                                                   "Mean", 
                                                                   "Mode"),
                                                  'Five Number Summary' = c("Min", 
                                                                            "First Quartile (Q1)", 
                                                                            "Median", 
                                                                            "Third Quartile (Q3)", 
                                                                            "Max"),
                                                  Outliers = c("IQR", 
                                                               "Lower Fence", 
                                                               "Upper Fence", 
                                                               "Potential Outliers"),
                                                  Dispersion = c("Range", 
                                                                 "Sample Standard Deviation", 
                                                                 "Sample Variance", 
                                                                 "Standard Error of the Mean", 
                                                                 "Coefficient of Variation"),
                                                  Distribution = c("Skewness", 
                                                                   "Kurtosis")
                                    ),
                                    selected = c("Observations", 
                                                 "Min", 
                                                 "First Quartile (Q1)", 
                                                 "Median", 
                                                 "Third Quartile (Q3)", 
                                                 "Max", 
                                                 "Sample Standard Deviation"),
                                    options = list(
                                      `actions-box` = TRUE,
                                      'selected-text-format' = 'count',
                                      style = 'primary'
                                      ),
                                    multiple = TRUE
                        ),
                        
                        br(),
                        
                        pickerInput(
                                    inputId = "dsGraphOptions",
                                    label = strong("Graph Options"), 
                                    choices = c("Boxplot", 
                                                "Histogram", 
                                                "Stem and Leaf Plot"),
                                    selected = c("Boxplot"),
                                    options = list(
                                      `actions-box` = TRUE,
                                      'selected-text-format' = 'count',
                                      style = 'primary'
                                    ),
                                    multiple = TRUE
                        ),
                        
                        br(),
                        
                        # prettyToggle(inputId = "descriptivesToggle",
                        #              label_on = tags$u(strong("Descriptives")),
                        #              label_off = strong("Descriptives"),
                        #              status_on = 'primary',
                        #              status_off = 'default',
                        #              icon_on = icon("chevron-down", lib = "font-awesome"),
                        #              icon_off = icon("chevron-up", lib = "font-awesome"),
                        #              shape = 'curve',
                        #              animation = 'smooth',
                        #              thick = TRUE,
                        #              outline = FALSE,
                        #              fill = FALSE,
                        #              value = TRUE),
                        # 
                        # conditionalPanel(
                        #   condition = 'input.descriptivesToggle',
                        #   
                        #   prettyCheckboxGroup(inputId = "descriptives",
                        #                       label = NULL,
                        #                       choices = c("Observations", "Sum", "Sum of Squares", "Mean", "Mode"),
                        #                       selected = c("Observations", "Mean"),
                        #                       inline = TRUE,
                        #                       icon = icon("check"),
                        #                       status = "primary",
                        #                       shape = 'curve',
                        #                       animation = "smooth"
                        #   )
                        # ),
                        # 
                        # #br(),
                        # 
                        # prettyToggle(inputId = "fiveNumSummaryToggle",
                        #              label_on = tags$u(strong("Five Number Summary")),
                        #              label_off = strong("Five Number Summary"),
                        #              status_on = 'primary',
                        #              status_off = 'default',
                        #              icon_on = icon("chevron-down", lib = "font-awesome"),
                        #              icon_off = icon("chevron-up", lib = "font-awesome"),
                        #              shape = 'curve',
                        #              animation = 'smooth',
                        #              thick = TRUE,
                        #              outline = FALSE,
                        #              fill = FALSE,
                        #              value = TRUE),
                        # 
                        # 
                        # conditionalPanel(
                        #   condition = 'input.fiveNumSummaryToggle',
                        #   
                        #   prettyCheckboxGroup(inputId = "fiveNumSummary",
                        #                       label = NULL,
                        #                       choices = c("Min", "First Quartile (Q1)", "Median", "Third Quartile (Q3)", "Max"),
                        #                       selected = c("Min", "First Quartile (Q1)", "Median", "Third Quartile (Q3)", "Max"),
                        #                       inline = TRUE,
                        #                       icon = icon("check"),
                        #                       status = "primary",
                        #                       shape = 'curve',
                        #                       animation = "smooth"
                        #   )
                        # ),
                        # #br(),
                        # # checkboxGroupInput(inputId = "fiveNumSummary",
                        # #                    label = NULL,
                        # #                    choices = c("Min", "First Quartile (Q1)", "Median", "Third Quartile (Q3)", "Max"),
                        # #                    selected = c("Min", "First Quartile (Q1)", "Median", "Third Quartile (Q3)", "Max"),
                        # #                    inline = TRUE
                        # # ),
                        # 
                        # 
                        # prettyToggle(inputId = "outliersToggle",
                        #              label_on = tags$u(strong("Outliers")),
                        #              label_off = strong("Outliers"),
                        #              status_on = 'primary',
                        #              status_off = 'default',
                        #              icon_on = icon("chevron-down", lib = "font-awesome"),
                        #              icon_off = icon("chevron-up", lib = "font-awesome"),
                        #              shape = 'curve',
                        #              animation = 'smooth',
                        #              thick = TRUE,
                        #              outline = FALSE,
                        #              fill = FALSE,
                        #              value = FALSE),
                        # 
                        # 
                        # conditionalPanel(
                        #   condition = 'input.outliersToggle',
                        #   
                        #   prettyCheckboxGroup(inputId = "outliers",
                        #                       label = NULL,
                        #                       choices = c("IQR", "Lower Fence", "Upper Fence", "Potential Outliers"),
                        #                       selected = c(""),
                        #                       inline = TRUE,
                        #                       icon = icon("check"),
                        #                       status = "primary",
                        #                       shape = 'curve',
                        #                       animation = "smooth"
                        #   )
                        # ),
                        # #br(),
                        # 
                        # # checkboxGroupInput(inputId = "outliers",
                        # #                    label = strong("Outliers:"),
                        # #                    choices = c("IQR", "Lower Fence", "Upper Fence", "Potential Outliers"),
                        # #                    selected = c(""),
                        # #                    inline = TRUE
                        # # ),
                        # 
                        # 
                        # 
                        # prettyToggle(inputId = "dispersionToggle",
                        #              label_on = tags$u(strong("Dispersion")),
                        #              label_off = strong("Dispersion"),
                        #              status_on = 'primary',
                        #              status_off = 'default',
                        #              icon_on = icon("chevron-down", lib = "font-awesome"),
                        #              icon_off = icon("chevron-up", lib = "font-awesome"),
                        #              shape = 'curve',
                        #              animation = 'smooth',
                        #              thick = TRUE,
                        #              outline = FALSE,
                        #              fill = FALSE,
                        #              value = TRUE),
                        # 
                        # 
                        # conditionalPanel(
                        #   condition = 'input.dispersionToggle',
                        #   
                        #   prettyCheckboxGroup(inputId = "dispersion",
                        #                       label = NULL,
                        #                       choices = c("Range", "Sample Standard Deviation", "Sample Variance", "Standard Error of the Mean", "Coefficient of Variation"),
                        #                       selected = c("Sample Standard Deviation"),
                        #                       inline = TRUE,
                        #                       icon = icon("check"),
                        #                       status = "primary",
                        #                       shape = 'curve',
                        #                       animation = "smooth"
                        #   )
                        # ),
                        # #br(),
                        # 
                        # 
                        # 
                        # # checkboxGroupInput(inputId = "dispersion",
                        # #                    label = strong("Dispersion:"),
                        # #                    choices = c("Range", "Sample Standard Deviation", "Sample Variance", "Standard Error of the Mean", "Coefficient of Variation"),
                        # #                    selected = c("Sample Standard Deviation"),
                        # #                    inline = TRUE
                        # # ),
                        # 
                        # 
                        # 
                        # prettyToggle(inputId = "distributionToggle",
                        #              label_on = tags$u(strong("Distribution")),
                        #              label_off = strong("Distribution"),
                        #              status_on = 'primary',
                        #              status_off = 'default',
                        #              icon_on = icon("chevron-down", lib = "font-awesome"),
                        #              icon_off = icon("chevron-up", lib = "font-awesome"),
                        #              shape = 'curve',
                        #              animation = 'smooth',
                        #              thick = TRUE,
                        #              outline = FALSE,
                        #              fill = FALSE,
                        #              value = FALSE),
                        # 
                        # 
                        # conditionalPanel(
                        #   condition = 'input.distributionToggle',
                        #   
                        #   prettyCheckboxGroup(inputId = "distribution",
                        #                       label = NULL,
                        #                       choices = c("Skewness", "Kurtosis"),
                        #                       selected = c(""),
                        #                       inline = TRUE,
                        #                       icon = icon("check"),
                        #                       status = "primary",
                        #                       shape = 'curve',
                        #                       animation = "smooth"
                        #   )
                        # ),
                        # br(),
                        
                        # checkboxGroupInput(inputId = "distribution",
                        #                    label = NULL,
                        #                    choices = c("Skewness", "Kurtosis"),
                        #                    selected = c(""),
                        #                    inline = TRUE
                        # ),
                        
                        
                        #checkboxInput("boxPlot", strong("Add a Boxplot")),
                        
                        # conditionalPanel(
                        #   condition = "input.boxPlot == 1",
                        #   
                        #   textInput("main", label = strong("Main title and axes labels:"), value = "Box Plot", placeholder = "main title"),
                        #   textInput("xlab", label = NULL, value = "x", placeholder = "x-axis label"),
                        #   textInput("ylab", label = NULL, value = "y", placeholder = "y-axis label"),
                        #   #hr(),
                        # ),
                        
                        actionButton(inputId = "goDescpStats", 
                                     label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetAll", 
                                     label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ), #DescriptiveStats Panel
                      
                      #   ------------------------------------------- #  
                      ### ---- Probability Distributions sidebar ---- 
                      #   ------------------------------------------- #
                      conditionalPanel(id = "probPanel",
                                       condition = "input.dropDownMenu == 'Probability Distributions'",
                                       style = "display: none;",
                        
                        radioButtons(inputId = "probability", 
                                     label = strong("Distribution"), 
                                     choices = c("Binomial", 
                                                 "Poisson", 
                                                 "Normal"), 
                                     selected = NULL, 
                                     inline = TRUE),
                        
                        conditionalPanel(id = "binomialPanel",
                          condition = "input.probability == 'Binomial'",
                          
                          numericInput(inputId = "numTrialsBinom",
                                       label = strong("Number of Trials (\\( n\\))"),
                                       value = 7, 
                                       min = 1, 
                                       step = 1),
                          
                          numericInput(inputId = "successProbBinom",
                                       label = strong("Probability of Success (\\( p\\))"),
                                       value = 0.15, 
                                       min = 0, 
                                       max = 1, 
                                       step = 0.00001),
                          
                          radioButtons(inputId = "calcBinom",
                                       label = strong("Probability"),
                                       choiceValues = list("exact", 
                                                           "cumulative", 
                                                           "upperTail", 
                                                           "greaterThan", 
                                                           "lessThan", 
                                                           "between"),
                                       choiceNames = list("\\(P(X = x \\))",
                                                          "\\(P(X \\leq x)\\)",
                                                          "\\(P(X \\ge x)\\)", 
                                                          "\\(P(X \\gt x)\\)", 
                                                          "\\(P(X < x)\\)", 
                                                          "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.calcBinom != 'between'",
                            
                            numericInput(inputId = "numSuccessesBinom",
                                         label = strong("Number of Successes (\\( x\\))"),
                                         value = 2, 
                                         min = 0, 
                                         step = 1)
                          ),
                          
                          conditionalPanel(
                            condition = "input.calcBinom == 'between'",
                            
                            numericInput(inputId = "numSuccessesBinomx1",
                                         label = strong("Number of Successes (\\( x_{1}\\))"),
                                         value = 2, 
                                         min = 0, 
                                         step = 1),
                            
                            numericInput(inputId = "numSuccessesBinomx2",
                                         label = strong("Number of Successes (\\( x_{2}\\))"),
                                         value = 4, 
                                         min = 0, 
                                         step = 1)
                          ),

                          br(),
                          p(strong("Options")),
                          hr(),
                          
                          checkboxInput(inputId = "showBinomTable", 
                                        label = "Display Probability Distribution Table", 
                                        value = TRUE),

                          
                          # checkboxInput(inputId = "probDistTable",
                          #               label = strong("Probability Distribution Table"),
                          #               value = FALSE,
                          #               width = NULL),
                          
                          # checkboxInput(inputId = "mean_and_SD_binom",
                          #               label = strong("Mean (\\( \\mu\\)) and Standard Deviation (\\( \\sigma\\))"),
                          #               value = FALSE,
                          #               width = NULL),
                          
                          actionButton(inputId = "goBinom", 
                                       label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetBinomial", 
                                       label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), # , onclick = "history.go(0)"

                          # br(),
                          # downloadButton('downloadBinomResults', 'Download Results'),
                        ),
                        
                        conditionalPanel(id = "poissonPanel", 
                          condition = "input.probability == 'Poisson'",
                          
                          numericInput(inputId = "muPoisson", 
                                       label = strong("Average (\\( \\mu\\))"),
                                       value = 4.5),
                          
                          radioButtons(inputId = "calcPoisson",
                                       label = strong("Probability"),
                                       choiceValues = list("exact", 
                                                           "cumulative", 
                                                           "upperTail", 
                                                           "greaterThan", 
                                                           "lessThan", 
                                                           "between"),
                                       choiceNames = list("\\(P(X = x \\))",
                                                          "\\(P(X \\leq x)\\)",
                                                          "\\(P(X \\ge x)\\)", 
                                                          "\\(P(X \\gt x)\\)", 
                                                          "\\(P(X < x)\\)", 
                                                          "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.calcPoisson != 'between'",
                            
                            numericInput(inputId = "xPoisson", 
                                         label = strong("Number of Successes (\\( x\\))"),
                                         value = 4, 
                                         min = 0, 
                                         step = 1),
                          ),
                          
                          conditionalPanel(
                            condition = "input.calcPoisson == 'between'",
                            
                            numericInput(inputId = "x1Poisson",
                                         label = strong("Number of Successes (\\( x_{1}\\))"),
                                         value = 4, 
                                         min = 0, 
                                         step = 1),
                            
                            numericInput(inputId = "x2Poisson",
                                         label = strong("Number of Successes (\\( x_{2}\\))"),
                                         value = 6,
                                         min = 0, 
                                         step = 1)
                          ),
                          
                          br(),
                          p(strong("Options")),
                          hr(),
                          
                          checkboxInput(inputId = "showPoissTable", 
                                        label = "Display Probability Distribution Table", 
                                        value = TRUE),
                          
                          actionButton(inputId = "goPoisson", 
                                       label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton("resetPoisson", 
                                       label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                        ),
                        
                        
                        conditionalPanel(id = "normalPanel",
                          condition = "input.probability == 'Normal'",
                          
                          numericInput(inputId = "popMean", 
                                       label = strong("Population Mean (\\( \\mu\\))"), 
                                       value = 0, 
                                       step = 0.00001),
                          
                          numericInput(inputId = "popSD",
                                       label = strong("Population Standard Deviation (\\( \\sigma\\))"),
                                       value = 1, min = 0, 
                                       step = 0.00001),

                          radioButtons(inputId = "calcNormal",
                                       label = strong("Probability"), 
                                       choiceValues = list("cumulative", "upperTail", "between"),
                                       choiceNames = list("\\(P(X \\leq x)\\) or \\(P(X < x)\\)", "\\(P(X \\ge x)\\) or \\(P(X \\gt x)\\)", "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.calcNormal != 'between'",
                            
                                numericInput(inputId = "xValue",
                                             label = strong("Normally Distributed Variable (\\( x\\))"),
                                             value = 0, 
                                             step = 0.00001),
                          ),
                          
                          conditionalPanel(
                            condition = "input.calcNormal == 'between'",
                            
                            numericInput(inputId = "x1Value",
                                         label = strong("Normally Distributed Variable (\\( x_{1}\\))"),
                                         value = -1, 
                                         step = 0.00001),
                            
                            numericInput(inputId = "x2Value",
                                         label = strong("Normally Distributed Variable (\\( x_{2}\\))"),
                                         value = 1, 
                                         step = 0.00001),

                          ),
                          
                          actionButton(inputId = "goNormal", 
                                       label = "Calculate",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          actionButton(inputId = "resetNormal", 
                                       label = "Reset Values",
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                        )
                      ), #ProbPanel
                      
                      #   --------------------------------------- #  
                      ### ---- Statistical Inference sidebar ---- 
                      #   --------------------------------------- #
                      conditionalPanel(id = "inferencePanel",
                                       condition = "input.dropDownMenu == 'Statistical Inference'",
                                       style = "display: none;",

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
                        
                        conditionalPanel( #### 1 Sample ----
                          condition = "input.samplesSelect == '1'",
                          
                          radioButtons(inputId = "popuParameter",
                                       label = strong("Parameter of Interest"),
                                       choiceValues = list("Population Mean", 
                                                           "Population Proportion"),
                                       choiceNames = list("Population Mean (\\( \\mu\\))", 
                                                          "Population Proportion (\\( p\\))"),
                                       selected = "Population Mean", #character(0), #
                                       inline = TRUE), #,width = '1000px'),
                          
                          # radioButtons(inputId = "popuParameter",
                          #              label = strong("Parameter of Interest"),
                          #              choiceValues = list("Population Mean", "Population Standard Deviation", "Sample Size Estimation"),
                          #              choiceNames = list("Population Mean (\\( \\mu\\))", "Population Standard Deviation (\\( \\sigma\\))", "Sample Size Estimation (n)"),
                          #              selected = "Population Mean",
                          #              #inline = TRUE), #,width = '1000px'),

                          conditionalPanel( ##### Mean ----
                            condition = "input.popuParameter == 'Population Mean'",
                            
                            radioButtons(inputId = "dataAvailability",
                                         label = strong("Data Availability"),
                                         choiceValues = list("Summarized Data", 
                                                             "Enter Raw Data"),
                                         choiceNames = list("Summarized Data", 
                                                            "Enter Raw Data"),
                                         selected = "Summarized Data", # character(0), # 
                                         inline = TRUE), #,width = '1000px'),
                            
                            conditionalPanel( ###### Summarized ----
                              condition = "input.dataAvailability == 'Summarized Data'",
                              
                              numericInput(inputId = "sampleSize",
                                           label = strong("Sample Size (\\( n\\))"),
                                           value = 18, 
                                           min = 1, 
                                           step = 1),
                              
                              numericInput(inputId = "sampleMean",
                                           label = strong("Sample Mean (\\( \\bar{x}\\))"),
                                           value = 103.5375, 
                                           step = 0.00001),
                              
                              radioButtons(inputId = "sigmaKnown",
                                           label = strong("Is Population Standard Deviation (\\( \\sigma\\)) known?"),
                                           choiceValues = list("Known", 
                                                               "Unknown"),
                                           choiceNames = list("Known", 
                                                              "Unknown"),
                                           selected = "Known", #character(0),
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel( ####### " Known ----
                                condition = "input.sigmaKnown == 'Known'",
                                
                                numericInput(inputId = "popuSD",
                                             label = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                                             value = 8.25, 
                                             min = 0.00001, 
                                             step = 0.00001)),
                              
                              conditionalPanel( ####### " Unknown ----
                                condition = "input.sigmaKnown == 'Unknown'",
                                
                                numericInput(inputId = "sampSD",
                                             label = strong("Sample Standard Deviation (\\( s\\)) Value"),
                                             value = 4.78, 
                                             min = 0.00001, 
                                             step = 0.00001)),
                            ), # One Sample Summarized Data
                            
                            conditionalPanel( ###### Raw ----
                              condition = "input.dataAvailability == 'Enter Raw Data'",
                              
                              textAreaInput(inputId = "sample1", 
                                            label = strong("Sample"), 
                                            value = "202, 210, 215, 220, 220, 224, 225, 228, 228, 228", 
                                            placeholder = "Enter values separated by a comma with decimals as points", 
                                            rows = 3),
                              
                              radioButtons(inputId = "sigmaKnownRaw",
                                           label = strong("Population Standard Deviation (\\( \\sigma\\))"),
                                           choiceValues = list("rawKnown", 
                                                               "rawUnknown"),
                                           choiceNames = list("Known", 
                                                              "Unknown"),
                                           selected = "rawKnown",
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel( ###### " Known ----
                                condition = "input.sigmaKnownRaw == 'rawKnown'",
                                
                                numericInput(inputId = "popuSDRaw",
                                             label = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                                             value = 8.25, 
                                             min = 0.00001, 
                                             step = 0.00001)
                              ),
                              
                              conditionalPanel( ###### " Unknown
                                condition = "input.sigmaKnownRaw == 'rawUnknown'"
                              )
                            ), # One Sample Raw Data
                          ), # One Population Mean
                          
                          conditionalPanel(##### Proportion ----
                            condition = "input.popuParameter == 'Population Proportion'",

                              numericInput(inputId = "numSuccesses",
                                           label = strong("Number of Successes (\\( x\\))"),
                                           value = 1087, 
                                           min = 0, 
                                           step = 1),

                              numericInput(inputId = "numTrials",
                                           label = strong("Number of Trials (\\( n\\))"),
                                           value = 1430, 
                                           min = 1, 
                                           step = 1),
                          ), #One Population Proportion 
                          
                          #conditionalPanel(
                            #condition = "input.dataAvailability == 'Summarized Data' || input.dataAvailability == 'Enter Raw Data' || input.popuParameter == 'Population Proportion'",
                            
                              radioButtons(inputId = "inferenceType",
                                           label = strong("Inference Type"),
                                           choiceValues = list("Confidence Interval", 
                                                               "Hypothesis Testing"),
                                           choiceNames = list("Confidence Interval", 
                                                              "Hypothesis Testing"),
                                           selected = "Confidence Interval", #character(0), # 
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Confidence Interval'",
                                
                                radioButtons(inputId = "confidenceLevel", 
                                             label = strong("Confidence Level (\\( 1- \\alpha\\))"), 
                                             choices = c("90%", 
                                                         "95%",
                                                         "99%"), 
                                             selected = c("95%"),
                                             inline = TRUE)
                              ),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Hypothesis Testing'",
                                
                                radioButtons(inputId = "significanceLevel", 
                                             label = strong("Significance Level (\\( \\alpha\\))"), 
                                             choices = c("10%", 
                                                         "5%",
                                                         "1%"),
                                             selected = c("5%"),
                                             inline = TRUE),
                                
                                conditionalPanel(
                                  condition = "input.popuParameter == 'Population Mean'",
                                  
                                  conditionalPanel(
                                    condition = "input.dataAvailability == 'Summarized Data' || input.dataAvailability == 'Enter Raw Data'",
                                    
                                    numericInput(inputId = "hypMean",
                                                 label = strong("Hypothesized Population Mean (\\( \\mu_{0}\\)) Value"),
                                                 value = 99, 
                                                 step = 0.00001),
                                  ),
                                ),
                                
                                conditionalPanel(
                                  condition = "input.popuParameter == 'Population Proportion'",
                                  
                                  numericInput(inputId = "hypProportion",
                                               label = strong("Hypothesized Population Proportion (\\( p_{0}\\)) Value"),
                                               value = 0.73, 
                                               min = 0, 
                                               max = 1, 
                                               step = 0.00001),
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
                          #), # CI vs HT for 1 sample
                        ), #"input.samplesSelect == '1'"
   
                        conditionalPanel(
                          condition = "input.samplesSelect == '2'",
                          
                          radioButtons(inputId = "popuParameters",
                                       label = strong("Parameter of Interest"),
                                       choiceValues = list("Independent Population Means", 
                                                           "Dependent Population Means", 
                                                           "Population Proportions"),
                                       choiceNames = list("Two Independent Populations (\\( \\mu_{1} - \\mu_{2} \\))", 
                                                          "Dependent (Paired) Populations (\\( \\mu_{d} \\))", 
                                                          "Two Population Proportions (\\( p_{1} - p_{2}\\))"),
                                       selected = "Independent Population Means", #character(0), #
                                       inline = FALSE), #,width = '1000px'),
                          
                          conditionalPanel(
                            condition = "input.popuParameters == 'Independent Population Means'",
                            
                            radioButtons(inputId = "dataAvailability2",
                                         label = strong("Data Availability"),
                                         choiceValues = list("Summarized Data", 
                                                             "Enter Raw Data"),
                                         choiceNames = list("Summarized Data", 
                                                            "Enter Raw Data"),
                                         selected = "Summarized Data", #character(0), # 
                                         inline = TRUE), #,width = '1000px'),
                            
                            conditionalPanel(
                              condition = "input.dataAvailability2 == 'Summarized Data'",
                              
                              numericInput(inputId = "sampleSize1",
                                           label = strong("Sample Size 1 (\\( n_{1}\\))"),
                                           value = 21, 
                                           min = 1, 
                                           step = 1),
                              
                              numericInput(inputId = "sampleMean1",
                                           label = strong("Sample Mean 1 (\\( \\bar{x}_{1}\\))"),
                                           value = 29.6, 
                                           step = 0.00001),
                              
                              numericInput(inputId = "sampleSize2",
                                           label = strong("Sample Size 2 (\\( n_{2}\\))"),
                                           value = 21, 
                                           min = 1, 
                                           step = 1),
                              
                              numericInput(inputId = "sampleMean2",
                                           label = strong("Sample Mean 2 (\\( \\bar{x}_{2}\\))"),
                                           value = 33.9, 
                                           step = 0.00001),
                              
                              radioButtons(inputId = "bothsigmaKnown",
                                           label = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                                           choiceValues = list("bothKnown", 
                                                               "bothUnknown"),
                                           choiceNames = list("Both Known", 
                                                              "Both Unknown"),
                                           selected = "bothKnown",
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnown == 'bothKnown'",
                                
                                numericInput(inputId = "popuSD1",
                                             label = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                                             value = 5.36, 
                                             min = 0.00001, 
                                             step = 0.00001),
                                
                                numericInput(inputId = "popuSD2",
                                             label = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                                             value = 5.97, 
                                             min = 0.00001, 
                                             step = 0.00001),
                              ),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnown == 'bothUnknown'",
                                
                                numericInput(inputId = "sampSD1",
                                             label = strong("Sample Standard Deviation 1 (\\( s_{1}\\)) Value"),
                                             value = 5.24, 
                                             min = 0.00001, 
                                             step = 0.00001),
                                
                                numericInput(inputId = "sampSD2",
                                             label = strong("Sample Standard Deviation 2 (\\( s_{2}\\)) Value"),
                                             value = 5.85, 
                                             min = 0.00001, 
                                             step = 0.00001),
                                
                                radioButtons(inputId = "bothsigmaEqual",
                                             label = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                                             choiceValues = list("TRUE", "FALSE"),
                                             choiceNames = list("Yes (Pooled)", "No (Welch-Satterthwaite df)"),
                                             selected = "TRUE",
                                             inline = TRUE), #,width = '1000px'),
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.dataAvailability2 == 'Enter Raw Data'",
                              
                              textAreaInput(inputId = "raw_sample1", 
                                            label = strong("Sample 1"), 
                                            value = "101.1,  111.1,  107.6,  98.1,  99.5,  98.7,  103.3,  108.9,  109.1,  103.3", 
                                            placeholder = "Enter values separated by a comma with decimals as points", 
                                            rows = 3),
                              
                              textAreaInput(inputId = "raw_sample2", 
                                            label = strong("Sample 2"), 
                                            value = "107.1,  105.0,  98.0,  97.9,  103.3,  104.6,  100.1,  98.2,  97.9", 
                                            placeholder = "Enter values separated by a comma with decimals as points", 
                                            rows = 3),
                              
                              radioButtons(inputId = "bothsigmaKnownRaw",
                                           label = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                                           choiceValues = list("bothKnown", 
                                                               "bothUnknown"),
                                           choiceNames = list("Both Known", 
                                                              "Both Unknown"),
                                           selected = "bothKnown",
                                           inline = TRUE), #,width = '1000px'),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnownRaw == 'bothKnown'",
                                
                                numericInput(inputId = "popuSDRaw1",
                                             label = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                                             value = 4.54, 
                                             min = 0.00001, 
                                             step = 0.00001),
                                
                                numericInput(inputId = "popuSDRaw2",
                                             label = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                                             value = 3.47, 
                                             min = 0.00001, 
                                             step = 0.00001),
                              ),
                              
                              conditionalPanel(
                                condition = "input.bothsigmaKnownRaw == 'bothUnknown'",
                                
                                radioButtons(inputId = "bothsigmaEqualRaw",
                                             label = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                                             choiceValues = list("TRUE", "FALSE"),
                                             choiceNames = list("Yes (Pooled)", "No (Welch-Satterthwaite df)"),
                                             selected = "TRUE",
                                             inline = TRUE)
                                
                                #print("Inference about two independent samples when populations variances are unknown but assumed equal")
                              ),
                            ),
                          ), # Two Independent Samples
                          
                          conditionalPanel(
                            condition = "input.popuParameters == 'Dependent Population Means'",
                          
                            textAreaInput(inputId = "before", 
                                          label = strong("Before"), 
                                          value = "484, 478, 492, 444, 436, 398, 464, 476", 
                                          placeholder = "Enter values separated by a comma with decimals as points", 
                                          rows = 3),
                            
                            textAreaInput(inputId = "after", 
                                          label = strong("After"), 
                                          value = "488, 478, 480, 426, 440, 410, 458, 460", 
                                          placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                            
                          ), # Two Dependent Samples
                          
                          conditionalPanel(
                            condition = "input.popuParameters == 'Population Proportions'",
                            
                                numericInput(inputId = "numSuccesses1",
                                             label = strong("Number of Successes 1 (\\( x_{1}\\))"),
                                             value = 174, 
                                             min = 0, 
                                             step = 1),

                                numericInput(inputId = "numTrials1",
                                             label = strong("Number of Trials 1 (\\( n_{1}\\))"),
                                             value = 300, 
                                             min = 1, 
                                             step = 1),

                                numericInput(inputId = "numSuccesses2",
                                             label = strong("Number of Successes 2 (\\( x_{2}\\))"),
                                             value = 111, 
                                             min = 0, 
                                             step = 1),

                                numericInput(inputId = "numTrials2",
                                             label = strong("Number of Trials 2 (\\( n_{2}\\))"),
                                             value = 300, 
                                             min = 1, 
                                             step = 1),
                          ), # Two Population Proportions
                          
                          conditionalPanel(
                            condition = "input.dataAvailability2 == 'Summarized Data' || input.dataAvailability2 == 'Enter Raw Data' || input.popuParameters == 'Dependent Population Means' || input.popuParameters == 'Population Proportions'",
                            
                                radioButtons(inputId = "inferenceType2",
                                             label = strong("Inference Type"),
                                             choiceValues = list("Confidence Interval", 
                                                                 "Hypothesis Testing"),
                                             choiceNames = list("Confidence Interval", 
                                                                "Hypothesis Testing"),
                                             selected = "Confidence Interval", #character(0), # 
                                             inline = TRUE), #,width = '1000px'),
                                
                                conditionalPanel(
                                  condition = "input.inferenceType2 == 'Confidence Interval'",
                                  
                                  radioButtons(inputId = "confidenceLevel2",
                                               label = strong("Confidence Level (\\( 1- \\alpha\\))"),
                                               choices = c("90%", 
                                                           "95%",
                                                           "99%"),
                                               selected = c("95%"),
                                               inline = TRUE)
                                ),
                            
                                conditionalPanel(
                                  condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                  
                                  radioButtons(inputId = "significanceLevel2", 
                                               label = strong("Significance Level (\\( \\alpha\\))"), 
                                               choices = c("10%", 
                                                           "5%",
                                                           "1%"),
                                               selected = c("5%"),
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

                        actionButton(inputId = "goInference", 
                                     label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetInference", 
                                     label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ), #inferencePanel
                      
                      #   -------------------------------------------- #  
                      ### ---- Regression and Correlation sidebar ---- 
                      #   -------------------------------------------- #
                      conditionalPanel(id = "RegCorPanel",
                                       condition = "input.dropDownMenu == 'Regression and Correlation'",
                                       style = "display: none;",
 
                        radioButtons(inputId = "simple_vs_multiple",
                                     label = strong("Regression Type"),
                                     choiceValues = list("SLR", 
                                                         "MLR"),
                                     choiceNames = list("Simple Linear Regression and Correlation Analysis", 
                                                        "Multiple Linear Regression"),
                                     selected = "SLR", #character(0), # 
                                     inline = TRUE), #,width = '1000px'),
                        
                        conditionalPanel(
                          condition = "input.simple_vs_multiple == 'SLR'",
                          
                          radioButtons(inputId = "dataRegCor",
                                       label = strong("Data"),
                                       choiceValues = list("Enter Raw Data", 
                                                           "Upload Data"),
                                       choiceNames = list("Enter Raw Data", 
                                                          "Upload Data"),
                                       selected = "Enter Raw Data", #character(0), #
                                       inline = TRUE), #,width = '1000px'),

                           conditionalPanel(
                             condition = "input.dataRegCor == 'Enter Raw Data'",
  
                             textAreaInput(inputId = "x", 
                                           label = strong("\\( x\\) (Independent Variable)"), 
                                           value = "10, 13, 18, 19, 22, 24, 27, 29, 35, 38", 
                                           placeholder = "Enter values separated by a comma with decimals as points", 
                                           rows = 3),
                             textAreaInput(inputId = "y", 
                                           label = strong("\\( y\\) (Dependent Variable)"), 
                                           value = "66, 108, 161, 177, 228, 235, 268, 259, 275, 278", 
                                           placeholder = "Enter values separated by a comma with decimals as points", 
                                           rows = 3),
                             
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
                                      accept = c("text/csv",
                                                 "text/comma-separated-values", 
                                                 "text/plain", 
                                                 ".csv",
                                                 ".xls",
                                                 ".xlsx")
                            ),
                            
                              selectizeInput(
                                             inputId = "slrExplanatory",
                                             label = strong("Choose the Explanatory Variable (x)"),
                                             choices = c(""),
                                             options = list(
                                               placeholder = 'Select a variable',
                                               onInitialize = I('function() { this.setValue(""); }')
                                             )
                              ),
                              
                              selectizeInput(
                                             inputId = "slrResponse",
                                             label = strong("Choose the Response Variable (y)"),
                                             choices = c(""),
                                             options = list(
                                               placeholder = 'Select a variable',
                                               onInitialize = I('function() { this.setValue(""); }')
                                             )
                              ),
                            #uiOutput("slrUploadVars"),
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
                          
                          checkboxInput(inputId = "scatterPlot", 
                                        label = "Scatterplot of \\( x\\) versus \\( y\\)", 
                                        value = TRUE),
                            
                          conditionalPanel(
                            condition = "input.scatterPlot == 1",
                                
                            textInput(inputId = "main", 
                                      label = strong("Main title and axes labels:"), 
                                      value = "Scatter Plot", 
                                      placeholder = "main title"),
                            textInput(inputId = "xlab", 
                                      label = NULL, 
                                      value = "Independent Variable, x", 
                                      placeholder = "x-axis label"),
                            textInput(inputId = "ylab", 
                                      label = NULL, 
                                      value = "Dependent Variable, y", 
                                      placeholder = "y-axis label"),
                                #hr(),
                          ),
                          br(),
                          #),
                          
                          #conditionalPanel(
                          #  condition = "input.regressioncorrelation == 'Correlation Coefficient'",
                            
                          #  checkboxInput("pearson", "Pearson's Product-Moment Correlation (r)"),
                          #checkboxInput("kendall", "Kendall's Rank Correlation (\\( \\tau\\))"),
                          #checkboxInput("spearman", "Spearman's Rank Correlation (\\( \\rho\\))"),
                            
                            # br(),
                            # checkboxGroupInput('corcoeff', strong('Correlation Coefficient'), choices = c("Pearson", "Kendall", "Spearman"), selected = "Pearson"),
                          #),
                        ),
                        
                        conditionalPanel(
                          condition = "input.simple_vs_multiple == 'MLR'",
                          
                          fileInput(inputId = 'headerfileMLR', 
                                    label = 'Upload data',
                                    accept = c('text/csv',
                                               'text/comma-separated-values',
                                               'text/tab-separated-values',
                                               'text/plain',
                                               '.csv',
                                               '.txt',
                                               '.xls',
                                               '.xlsx'))
                        ),
                        
                        actionButton(inputId = "goRegression", 
                                     label = "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton(inputId = "resetRegCor", 
                                     label = "Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4") #, onclick = "history.go(0)"
                      ), #RegCorPanel
                      
                      # br(),
                      # downloadButton('describe_download', "Download Report", class="butt" ), br(),
                      # tags$head(tags$style(".butt{background-color:#337ab7;} .butt{color:#fff;}")), br(),
                      # radioButtons('format', 'Document format', c('PDF', 'Word'), inline = TRUE),
                    ), #SidebarPanel
                    
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
                          style = "display: none;",
                            
                          uiOutput('renderDescrStats'),
                          
                          div(id = "descrStatsData",
                              tabsetPanel(id = "dsTabset", selected = "Table",
                                          
                                          tabPanel(id = "dsTable", title = "Table", value = 'Table',
                                                   
                                                   h3("Descriptive Statistics"),
                                                   fluidRow(
                                                     withMathJax(),
                                                     column(align = "center", width = 12, 
                                                            
                                                            uiOutput('dsDataTable')
                                                            )
                                                   ),
                                                   
                                                   br(),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.dsTableFilters.indexOf('First Quartile (Q1)') > -1 | input.dsTableFilters.indexOf('Third Quartile (Q3)') > -1",
                                                     
                                                     p("* Note: Q1 and Q3 are calculated by excluding Q2 on both sides"),
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.dsTableFilters.indexOf('Skewness') > -1 | input.dsTableFilters.indexOf('Third Quartile (Q3)') > -1",
                                                     
                                                     p("** Note: Skewness calculations requires at least 3 complete observations."),
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.dsTableFilters.indexOf('Kurtosis') > -1 | input.dsTableFilters.indexOf('Third Quartile (Q3)') > -1",
                                                     
                                                     p("*** Note: Kurtosis calculation requires at least 4 complete observations."),
                                                   ),
                                                   
                                                   br()
                                                   
                                          ),# dsTable tabPanel
                                          
                                          tabPanel(id = "dsGraphs", title = "Graphs", value = 'Graphs',
                                                   
                                                   conditionalPanel(
                                                     condition = "input.dsGraphOptions == ''",
                                                     
                                                     br(),
                                                     
                                                     p("Select one or more options from the Graph Options menu to see more information.")
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.dsGraphOptions.indexOf('Boxplot') > -1",
                                                     
                                                     h3("Boxplot"),
                                                     
                                                     fluidRow(
                                                       column(align = "center", width = 12, 
                                                              
                                                              plotOutput("dsBoxplot", width = '75%')
                                                              )
                                                     ),
                                                     
                                                     br(),
                                                     br(),
                                                     hr(),
                                                     br(),
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.dsGraphOptions.indexOf('Histogram') > -1",
                                                     
                                                     h3("Histogram"),
                                                     
                                                     fluidRow(
                                                       column(align = "center", width = 12, 
                                                              
                                                              plotOutput("dsHistogram", width = '75%')
                                                              )
                                                     ),
                                                     
                                                     br(),
                                                     hr(),
                                                     br(),
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.dsGraphOptions.indexOf('Stem and Leaf Plot') > -1",
                                                     
                                                     h3("Stem and Leaf Plot"),
                                                     
                                                     br(),
                                                     
                                                     fluidRow(
                                                       column(width = 2, div("")),
                                                       column(width = 8, 
                                                              
                                                              verbatimTextOutput('dsStemLeaf'),
                                                              
                                                              br(),
                                                              
                                                              p("* Note: Outlier values are listed under the HI/LO lists.")
                                                              ),
                                                       column(width = 2, div(""))
                                                     ),
                                                     
                                                     br(),
                                                   ),
                                                   
                                                   
                                          )# Graphs tabPanel
                              )# dsTabset tabsetPanel
                              
                            )# descrStatsData div
    
                        )
                      ), #DescriptiveStats Main Panel
                      
                      #   ---------------------------------------- #  
                      ### ---- Probability Distributions main ---- 
                      #   ---------------------------------------- #
                      div(id = "probabilityMP",
                          
                        conditionalPanel(
                          condition = "input.dropDownMenu == 'Probability Distributions'",
                          style = "display: none;",
                            
                          conditionalPanel(
                            condition = "input.probability == 'Binomial'",
                                
                            br(),
                            
                            uiOutput("renderProbabilityBinom"),
                            
                            br(),
                          ),
                              
                          conditionalPanel(
                            condition = "input.probability == 'Poisson'",
                                
                            br(),
                            
                            uiOutput("renderProbabilityPoisson"),
                            
                            br(),
                          ),
                              
                          conditionalPanel(
                            condition = "input.probability == 'Normal'",
                                
                            br(),
                            
                            uiOutput("renderProbabilityNorm"),
                            
                            br(),
                          )
                        )
                      ), #Probability MainPanel 
                      
                      #   ------------------------------------ #  
                      ### ---- Statistical Inference main ---- 
                      #   ------------------------------------ #
                      div(id = "inferenceMP",
                          
                        conditionalPanel(
                          condition = "input.dropDownMenu == 'Statistical Inference'",
                          style = "display: none;",
                            
                          uiOutput("renderInference"),
  
                          div(id = "inferenceData",
                              
                          conditionalPanel( #### One samp ----
                            condition = "input.samplesSelect == '1'",
                            
                            conditionalPanel( ##### Pop Mean ----
                              condition = "input.popuParameter == 'Population Mean'",
                              
                              fluidRow(
                                column(width = 4,
                                       
                                       titlePanel("Sample Data Summary"),
                                       hr(),
                                       uiOutput('oneSampMeanTable'),
                                       br(),
                                ),
                                column(width = 8,
                                       
                                       titlePanel('Summary Details'),
                                       hr(),
                                       
                                       conditionalPanel(
                                         condition = "input.oneSampMeanData_rows_selected == 0",
                                         
                                         p("Select one or more variables from the summary table for more information"),
                                       ),
                                       
                                )
                              ),
                              
                              br(),
                              hr(),
                              br(),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Confidence Interval'",
                                
                                titlePanel(tags$u("Confidence Interval")),
                                br(),
                                uiOutput('oneMeanCI'),
                                br(),
                              ),
                              
                              conditionalPanel(
                                condition = "input.inferenceType == 'Hypothesis Testing'",
                                
                                titlePanel(tags$u("Hypothesis Test")),
                                br(),
                                uiOutput('oneMeanHT'),
                                br(),
                              ),
                              
                              # conditionalPanel(
                              #   condition = "input.dataAvailability == 'Summarized Data'",
                              #   
                              #     conditionalPanel(
                              #       condition = "input.sigmaKnown == 'Known'",
                              #       
                              #       conditionalPanel(
                              #         condition = "input.inferenceType == 'Confidence Interval'",
                              #         
                              #         uiOutput('oneSampCI'),
                              #         br(),
                              #         #img(src ='OneSampZInt.png', height = '100px')
                              #       ), # One Sample CI
                              #       
                              #       conditionalPanel(
                              #         condition = "input.inferenceType == 'Hypothesis Testing'",
                              #         
                              #         uiOutput('oneSampHT'),
                              #         br(),
                              #       ), # One Sample HT
                              #     ),  # One Sample Sigma known
                              #   
                              #     conditionalPanel(
                              #       condition = "input.sigmaKnown == 'Unknown'",
                              #       
                              #       conditionalPanel(
                              #         condition = "input.inferenceType == 'Confidence Interval'",
                              #         
                              #         uiOutput('oneSampCIUnknown'),
                              #         br(),
                              #         #img(src ='OneSampTInt.png', height = '90px')
                              #       ), # One Sample CI
                              #       
                              #       conditionalPanel(
                              #         condition = "input.inferenceType == 'Hypothesis Testing'",
                              #         
                              #         uiOutput('oneSampHTUnknown'),
                              #         br(),
                              #       ), # One Sample HT
                              #     ), # One Sample Sigma unknown
                              #   
                              # ), # One Sample Summarized Data
                              # 
                              # conditionalPanel(
                              #   condition = "input.dataAvailability == 'Enter Raw Data'",
                              #   
                              #   conditionalPanel(
                              #     condition = "input.sigmaKnownRaw == 'rawKnown'",
                              #     
                              #     conditionalPanel(
                              #       condition = "input.inferenceType == 'Confidence Interval'",
                              #       
                              #       uiOutput('oneSampCIRaw'),
                              #       br(),
                              #       #img(src ='OneSampZInt.png', height = '100px')
                              #     ), # One Sample CI Raw
                              #     
                              #     conditionalPanel(
                              #       condition = "input.inferenceType == 'Hypothesis Testing'",
                              #       
                              #       uiOutput('oneSampHTRaw'),
                              #       br(),
                              #     ), # One Sample HT Raw
                              #   ), # One Sample Sigma known Raw
                              #   
                              #   conditionalPanel(
                              #     condition = "input.sigmaKnownRaw == 'rawUnknown'",
                              #     
                              #     conditionalPanel(
                              #       condition = "input.inferenceType == 'Confidence Interval'",
                              #       
                              #       uiOutput('oneSampCIRawUnknown'),
                              #       br(),
                              #       #img(src ='OneSampTInt.png', height = '90px')
                              #     ), # One Sample CI Raw
                              #     
                              #     conditionalPanel(
                              #       condition = "input.inferenceType == 'Hypothesis Testing'",
                              #       
                              #       uiOutput('oneSampHTRawUnknown'),
                              #       br(),
                              #     ),  # One Sample HT Raw
                              #   ), # One Sample Sigma unknown Raw
                              # ), # One Sample Raw Data
                            ), # One Population Mean
                            
                            conditionalPanel( ##### Pop Prop ----
                              condition = "input.popuParameter == 'Population Proportion'",
                              
                              #uiOutput('oneSampPropErrors'),
                              
                              #div(id = 'oneSampPropMP',
                                  
                                  #fluidRow(
                                    #column(width = 4,
                                           #titlePanel("Sample Data Summary"),
                                           #br(),
                                           #withMathJax(DTOutput('oneSampPropData')),
                                    #),
                                    #column(width = 8,
                                           #conditionalPanel(
                                             #condition = "input.oneSampPropData_rows_selected != 0",
                                             
                                             #titlePanel('Details'),
                                             #br(),
                                             
                                             #conditionalPanel(
                                               #condition = "1 %in% input$oneSampPropData_rows_selected",
                                               #uiOutput('oneSampPropDataNDetails'),
                                          #),
                                    #),
                                  #),
                                #),
                                  
                              uiOutput('oneSampProportion'),
                    
                              
                              #titlePanel("Sample Data Summary"),
                              #br(),
                              #withMathJax(tableOutput('oneSampPropData')),
                              #br(),
                              #hr(),
                              #br(),
                              
                              #conditionalPanel(
                                #condition = "input.inferenceType == 'Confidence Interval'",
                                
                                #titlePanel("Confidence Interval"),
                                #br(),
                                #uiOutput('oneSampPropCI'),
                                #br(),
                              #),
                              
                              #conditionalPanel(
                                #condition = "input.inferenceType == 'Hypothesis Testing'",
                                
                                #titlePanel("Hypothesis Test"),
                                #br(),
                                #uiOutput('oneSampPropHT'),
                                #br(),
                                #plotOutput('oneSampPropHTPlot')
                                #br(),
                              #),
                              
                            ), # One Population Proportion
                          ), # "input.samplesSelect == '1'"
  
                          conditionalPanel( #### Two Samp ----
                            condition = "input.samplesSelect == '2'",
                              
                            conditionalPanel( ##### Ind Pop Means ----
                              condition = "input.popuParameters == 'Independent Population Means'",
                                
                              # fluidRow(
                              #   column(width = 4,
                              #          titlePanel("Sample Data Summary"),
                              #          hr(),
                              #          uiOutput('indMeansTable'),
                              #          br(),
                              #   ),
                              #   column(width = 8,
                              #          titlePanel('Summary Details'),
                              #          hr(),
                              # 
                              #          conditionalPanel(
                              #            condition = "input.indMeansData_rows_selected == 0",
                              # 
                              #            p("Select one or more variables from the summary table for more information"),
                              #          ),
                              # 
                              #   )
                              # ),
                              # 
                              # br(),
                              # hr(),
                              # br(),
                              # 
                              # conditionalPanel(
                              #   condition = "input.inferenceType2 == 'Confidence Interval'",
                              # 
                              #   titlePanel(tags$u("Confidence Interval")),
                              #   br(),
                              #   uiOutput('indMeansCI'),
                              #   br(),
                              # ),
                              # 
                              # conditionalPanel(
                              #   condition = "input.inferenceType2 == 'Hypothesis Testing'",
                              # 
                              #   titlePanel(tags$u("Hypothesis Test")),
                              #   br(),
                              #   uiOutput('indMeansHT'),
                              #   br(),
                              # ),
                              
                              
                              
                              conditionalPanel(
                                condition = "input.dataAvailability2 == 'Summarized Data'",

                                conditionalPanel(
                                  condition = "input.bothsigmaKnown == 'bothKnown'",

                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Confidence Interval'",

                                    uiOutput('twoSampCIbothKnown'),
                                    br(),
                                    #img(src ='TwoSampZInt.png', height = '75px')
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
                                    #img(src ='TwoSampTInt.png', height = '75px')
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
                                  condition = "input.bothsigmaKnownRaw == 'bothKnown'",

                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Confidence Interval'",

                                    uiOutput('twoSampCIRawbothKnown'),
                                    br(),
                                    #img(src ='TwoSampZInt.png', height = '75px')
                                  ), # CI

                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Hypothesis Testing'",

                                    uiOutput('twoSampHTRawbothKnown'),
                                    br(),
                                  ), # HT
                                ), #both known raw

                                conditionalPanel(
                                  condition = "input.bothsigmaKnownRaw == 'bothUnknown'",

                                  conditionalPanel(
                                    condition = "input.inferenceType2 == 'Confidence Interval'",

                                    uiOutput('twoSampCIRawbothUnknown'),
                                    br(),
                                    #img(src ='TwoSampTInt.png', height = '75px')
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
                              
                            conditionalPanel( ##### Dep Pop Means ----
                              condition = "input.popuParameters == 'Dependent Population Means'",
                                
                              conditionalPanel(
                                condition = "input.inferenceType2 == 'Confidence Interval'",
                                  
                                #img(src ='TwoSampTIntPaired.png', height = '100px')
                              ), # CI
                                
                              conditionalPanel(
                                condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                  
                              ), # HT
                            ), # Two Dependent Samples
                              
                            #----------------------------#
                            # TWO POPULATION PROPORTIONS #
                            #----------------------------#
                            conditionalPanel( ##### Pop Props ----
                              condition = "input.popuParameters == 'Population Proportions'",
                                
                              uiOutput('twoSampProportion'),
                              #conditionalPanel(
                              #  condition = "input.inferenceType2 == 'Confidence Interval'",
                                  
                              #), # CI
                                
                              #conditionalPanel(
                              #  condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                  
                              #), # HT
                            ), # Two Population Proportions
                          ), # "input.samplesSelect == '2'"
                          ) # input.dropDownMenu == 'Statistical Inference'
                        )
                      ), # inferenceMP
                      
                      #   ----------------------------------------- #  
                      ### ---- Regression and Correlation main ---- 
                      #   ----------------------------------------- #
                      div(id = "RegCorMP",
                          
                        conditionalPanel(
                          condition = "input.dropDownMenu == 'Regression and Correlation'",
                          style = "display: none;",
                            
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
                           p("GitHub:", a(href = "https://github.com/ashokkrish/CougarStats","https://github.com/ashokkrish/CougarStats", target = "_blank")),
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
 )#UI 

# --------------------------- #
# ---- Server components ----
# --------------------------- #
server <- function(input, output) {
    
    # ------------------------- #
    # ---- Data Validation ----
    # ------------------------- #

    iv <- InputValidator$new()
    ds_iv <- InputValidator$new()
    dsraw_iv <- InputValidator$new()
    dsupload_iv <- InputValidator$new()
    dsuploadvars_iv <- InputValidator$new()
    
    pd_iv <- InputValidator$new()
    binom_iv <- InputValidator$new()
    binomprob_iv <- InputValidator$new()
    binombetween_iv <- InputValidator$new()
    poiss_iv <- InputValidator$new()
    poissprob_iv <- InputValidator$new()
    poissbetween_iv <- InputValidator$new()
    norm_iv <- InputValidator$new()
    normprob_iv <- InputValidator$new()
    normbetween_iv <- InputValidator$new()
    
    si_iv <- InputValidator$new()
    onemean_iv <- InputValidator$new()
    onemeansdknown_iv <- InputValidator$new()
    onemeansdunk_iv <- InputValidator$new()
    onemeanraw_iv <- InputValidator$new()
    onemeanht_iv <- InputValidator$new()
    indmeanssumm_iv <- InputValidator$new()
    indmeansraw_iv <- InputValidator$new()
    indmeanssdknown_iv <- InputValidator$new()
    indmeanssdunk_iv <- InputValidator$new()
    indmeansrawsd_iv <- InputValidator$new()
    oneprop_iv <- InputValidator$new()
    onepropht_iv <- InputValidator$new()
    twoprop_iv <- InputValidator$new()
    
    regcor_iv <- InputValidator$new()
    slrraw_iv <- InputValidator$new()
    slrupload_iv <- InputValidator$new()
    slruploadvars_iv <- InputValidator$new()
    
    ## DS rules ----
    
    # descriptiveStat
    
    dsraw_iv$add_rule("descriptiveStat", sv_required())
    dsraw_iv$add_rule("descriptiveStat", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                     "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
    
    dsupload_iv$add_rule("dsUserData", sv_required())
    dsupload_iv$add_rule("dsUserData", ~ if(ncol(dsUploadData()) < 1) "Data must include one variable")
    dsupload_iv$add_rule("dsUserData", ~ if(nrow(dsUploadData()) < 2) "Samples must include at least 2 observations")
    
    dsuploadvars_iv$add_rule("dsUploadVars", sv_required())
    
    #ds_iv$add_rule("dsTableFilters", sv_required())
    
    ds_iv$condition(~ isTRUE(input$dropDownMenu == 'Descriptive Statistics'))
    dsraw_iv$condition(~ isTRUE(input$dataInput == 'Enter Raw Data'))
    dsupload_iv$condition(~ isTRUE(input$dataInput == 'Upload Data'))
    dsuploadvars_iv$condition(function() {isTRUE(input$dataInput == 'Upload Data' && dsupload_iv$is_valid()) })
    
    ds_iv$add_validator(dsraw_iv)
    ds_iv$add_validator(dsupload_iv)
    ds_iv$add_validator(dsuploadvars_iv)
    
    ds_iv$enable()
    dsraw_iv$enable()
    dsupload_iv$enable()
    dsuploadvars_iv$enable()
    
    ## PD rules ----
    
    # numTrialsBinom 
    
    binom_iv$add_rule("numTrialsBinom", sv_required())
    binom_iv$add_rule("numTrialsBinom", sv_integer())
    binom_iv$add_rule("numTrialsBinom", sv_gt(0))
    
    # successProbBinom (PD)
    
    binom_iv$add_rule("successProbBinom", sv_required())
    binom_iv$add_rule("successProbBinom", sv_gte(0))
    binom_iv$add_rule("successProbBinom", sv_lte(1))
    
    # numSuccessesBinom (PD)
    
    binomprob_iv$add_rule("numSuccessesBinom", sv_required())
    binomprob_iv$add_rule("numSuccessesBinom", sv_integer())
    binomprob_iv$add_rule("numSuccessesBinom", sv_gte(0))
    
    # numSuccessesBinomx1 (PD)
    binombetween_iv$add_rule("numSuccessesBinomx1", sv_required())
    binombetween_iv$add_rule("numSuccessesBinomx1", sv_integer())
    binombetween_iv$add_rule("numSuccessesBinomx1", sv_gte(0))
    
    # numSuccessesBinomx2 (PD)
    binombetween_iv$add_rule("numSuccessesBinomx2", sv_required())
    binombetween_iv$add_rule("numSuccessesBinomx2", sv_integer())
    binombetween_iv$add_rule("numSuccessesBinomx2", sv_gte(0))
    
    # muPoisson (PD)
    
    poiss_iv$add_rule("muPoisson", sv_required())
    poiss_iv$add_rule("muPoisson", sv_gt(0))
    
    # xPoisson (PD)
    
    poissprob_iv$add_rule("xPoisson", sv_required())
    poissprob_iv$add_rule("xPoisson", sv_integer())
    poissprob_iv$add_rule("xPoisson", sv_gte(0))
    
    # x1Poisson (PD)
    poissbetween_iv$add_rule("x1Poisson", sv_required())
    poissbetween_iv$add_rule("x1Poisson", sv_integer())
    poissbetween_iv$add_rule("x1Poisson", sv_gte(0))
    
    # x2Poisson (PD)
    poissbetween_iv$add_rule("x2Poisson", sv_required())
    poissbetween_iv$add_rule("x2Poisson", sv_integer())
    poissbetween_iv$add_rule("x2Poisson", sv_gte(0))
    
    # popMean (PD)
    
    norm_iv$add_rule("popMean", sv_required())
    
    # popuSD (PD)
    
    norm_iv$add_rule("popSD", sv_required())
    norm_iv$add_rule("popSD", sv_gt(0))
    
    # xValue (PD)
    
    normprob_iv$add_rule("xValue", sv_required())
    
    normbetween_iv$add_rule("x1Value", sv_required())
    normbetween_iv$add_rule("x2Value", sv_required())
    
    # ------------------ #
    #     Conditions     #
    # ------------------ #
    binom_iv$condition(~ isTRUE(input$probability == 'Binomial'))
    binomprob_iv$condition(~ isTRUE(input$probability == 'Binomial' && input$calcBinom != 'between'))
    binombetween_iv$condition(~ isTRUE(input$probability == 'Binomial' && input$calcBinom == 'between'))
    
    poiss_iv$condition(~ isTRUE(input$probability == 'Poisson'))
    poissprob_iv$condition(~ isTRUE(input$probability == 'Poisson' && input$calcPoisson != 'between'))
    poissbetween_iv$condition(~ isTRUE(input$probability == 'Poisson' && input$calcPoisson == 'between'))
    
    norm_iv$condition(~ isTRUE(input$probability == 'Normal'))
    normprob_iv$condition(~ isTRUE(input$probability == 'Normal' && input$calcNormal != 'between'))
    normbetween_iv$condition(~ isTRUE(input$probability == 'Normal' && input$calcNormal == 'between'))
    # ------------------ #
    #     Dependency     #
    # ------------------ #
    binom_iv$add_validator(binomprob_iv)
    binom_iv$add_validator(binombetween_iv)
    
    poiss_iv$add_validator(poissprob_iv)
    poiss_iv$add_validator(poissbetween_iv)
    
    norm_iv$add_validator(normprob_iv)
    norm_iv$add_validator(normbetween_iv)
    
    pd_iv$add_validator(binom_iv)
    pd_iv$add_validator(poiss_iv)
    pd_iv$add_validator(norm_iv)
    
    # ------------------ #
    #     Activation     #
    # ------------------ #
    pd_iv$enable()
    binom_iv$enable()
    binomprob_iv$enable()
    binombetween_iv$enable()
    poiss_iv$enable()
    poissprob_iv$enable()
    poissbetween_iv$enable()
    norm_iv$enable()
    normprob_iv$enable()
    normbetween_iv$enable()
    
    #--------------- #
    ## SI rules ----
    #--------------- #
    
    # sampleSize 
    
    onemean_iv$add_rule("sampleSize", sv_required())
    onemean_iv$add_rule("sampleSize", sv_integer())
    onemean_iv$add_rule("sampleSize", sv_gt(0))
    
    # sampleMean 
    
    onemean_iv$add_rule("sampleMean", sv_required())
    
    # sample1 
    
    onemeanraw_iv$add_rule("sample1", sv_required())
    onemeanraw_iv$add_rule("sample1", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                               "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
    
    # popuSD 
    
    onemeansdknown_iv$add_rule("popuSD", sv_required()) 
    onemeansdknown_iv$add_rule("popuSD", sv_gt(0))
    
    # popuSDRaw 
    
    onemeanraw_iv$add_rule("popuSDRaw", sv_required()) 
    onemeanraw_iv$add_rule("popuSDRaw", sv_gt(0))
    
    
    # sampSD 
    
    onemeansdunk_iv$add_rule("sampSD", sv_required())
    onemeansdunk_iv$add_rule("sampSD", sv_gt(0))
    
    # sampleSize1 
    
    indmeanssumm_iv$add_rule("sampleSize1", sv_required())
    indmeanssumm_iv$add_rule("sampleSize1", sv_integer())
    indmeanssumm_iv$add_rule("sampleSize1", sv_gt(0))
    
    # sampleMean1 
    
    indmeanssumm_iv$add_rule("sampleMean1", sv_required())
    
    # sampleSize2 
    
    indmeanssumm_iv$add_rule("sampleSize2", sv_required())
    indmeanssumm_iv$add_rule("sampleSize2", sv_integer())
    indmeanssumm_iv$add_rule("sampleSize2", sv_gt(0))
    
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
    indmeansraw_iv$add_rule("raw_sample1", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                                    "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)"))
    
    # raw_sample2 
    
    indmeansraw_iv$add_rule("raw_sample2", sv_required())
    indmeansraw_iv$add_rule("raw_sample2", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                                    "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)"))
    
    indmeansrawsd_iv$add_rule("popuSDRaw1", sv_required()) 
    indmeansrawsd_iv$add_rule("popuSDRaw1", sv_gt(0))
    
    
    indmeansrawsd_iv$add_rule("popuSDRaw2", sv_required()) 
    indmeansrawsd_iv$add_rule("popuSDRaw2", sv_gt(0))
    # numSuccessesProportion
    
    oneprop_iv$add_rule("numSuccesses", sv_required(message = "Numeric value required"))
    oneprop_iv$add_rule("numSuccesses", sv_integer())
    oneprop_iv$add_rule("numSuccesses", sv_gte(0))
    
    # x1
    twoprop_iv$add_rule("numSuccesses1", sv_required())
    twoprop_iv$add_rule("numSuccesses1", sv_integer())
    twoprop_iv$add_rule("numSuccesses1", sv_gte(0))
    
    # x2
    twoprop_iv$add_rule("numSuccesses2", sv_required())
    twoprop_iv$add_rule("numSuccesses2", sv_integer())
    twoprop_iv$add_rule("numSuccesses2", sv_gte(0))
    
    # numTrialsProportion
    
    oneprop_iv$add_rule("numTrials", sv_required(message = "Numeric value required"))
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
    onepropht_iv$add_rule("hypProportion", sv_gte(0))
    onepropht_iv$add_rule("hypProportion", sv_lte(1))
    
    onemean_iv$condition(~ isTRUE(input$samplesSelect == '1' && input$popuParameter == 'Population Mean' && input$dataAvailability == 'Summarized Data'))
    onemeansdknown_iv$condition(~ isTRUE(input$samplesSelect == '1' && input$popuParameter == 'Population Mean' && input$dataAvailability == 'Summarized Data' && input$sigmaKnown == 'Known'))
    onemeansdunk_iv$condition(~ isTRUE(input$samplesSelect == '1' && input$popuParameter == 'Population Mean' && input$dataAvailability == 'Summarized Data' && input$sigmaKnown == 'Unknown'))
    onemeanraw_iv$condition(~ isTRUE(input$samplesSelect == '1' && input$popuParameter == 'Population Mean' && input$dataAvailability == 'Enter Raw Data'))
    onemeanht_iv$condition(~ isTRUE(input$samplesSelect == '1' && input$popuParameter == 'Population Mean' && input$inferenceType == 'Hypothesis Testing'))
    indmeanssumm_iv$condition(~ isTRUE(input$samplesSelect == '2' && input$popuParameters == 'Independent Population Means' && input$dataAvailability2 == 'Summarized Data'))
    indmeansraw_iv$condition(~ isTRUE(input$samplesSelect == '2' && input$popuParameters == 'Independent Population Means' && input$dataAvailability2 == 'Enter Raw Data'))
    indmeanssdknown_iv$condition(~ isTRUE(input$samplesSelect == '2' && input$popuParameters == 'Independent Population Means' && input$dataAvailability2 == 'Summarized Data' && input$bothsigmaKnown == 'bothKnown'))
    indmeanssdunk_iv$condition(~ isTRUE(input$samplesSelect == '2' && input$popuParameters == 'Independent Population Means' && input$dataAvailability2 == 'Summarized Data' && input$bothsigmaKnown == 'bothUnknown'))
    indmeansrawsd_iv$condition(~ isTRUE(input$samplesSelect == '2' && input$popuParameters == 'Independent Population Means' && input$dataAvailability2 == 'Enter Raw Data' && input$bothsigmaKnownRaw == 'bothKnown'))
    oneprop_iv$condition(~ isTRUE(input$samplesSelect == '1' && input$popuParameter == 'Population Proportion'))
    onepropht_iv$condition(~ isTRUE(input$samplesSelect == '1' && input$popuParameter == 'Population Proportion' && input$inferenceType == 'Hypothesis Testing'))
    twoprop_iv$condition(~ isTRUE(input$samplesSelect == '2' && input$popuParameters == 'Population Proportions'))
    
    si_iv$add_validator(onemean_iv)
    si_iv$add_validator(onemeansdknown_iv)
    si_iv$add_validator(onemeansdunk_iv)
    si_iv$add_validator(onemeanraw_iv)
    si_iv$add_validator(onemeanht_iv)
    si_iv$add_validator(indmeanssumm_iv)
    si_iv$add_validator(indmeansraw_iv)
    si_iv$add_validator(indmeanssdknown_iv)
    si_iv$add_validator(indmeanssdunk_iv)
    si_iv$add_validator(indmeansrawsd_iv)
    si_iv$add_validator(oneprop_iv)
    si_iv$add_validator(onepropht_iv)
    si_iv$add_validator(twoprop_iv)
    
    si_iv$enable()
    onemean_iv$enable()
    onemeansdknown_iv$enable()
    onemeansdunk_iv$enable()
    onemeanraw_iv$enable()
    onemeanht_iv$enable()
    indmeanssumm_iv$enable()
    indmeansraw_iv$enable()
    indmeanssdknown_iv$enable()
    indmeanssdunk_iv$enable()
    indmeansrawsd_iv$enable()
    oneprop_iv$enable()
    onepropht_iv$enable()
    twoprop_iv$enable()
    
    ## RC rules ---- 
    
    slrraw_iv$add_rule("x", sv_required())
    #iv$add_rule("x", sv_regex("^[0-9]+(.[0-9]+)?(, [0-9](.[0-9]+)?)+$", "Data can only be numeric values separated by commas"))
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
      text <- gsub("[^0-9.,-]","", text) #purge non-numeric characters 
      text <- gsub("^,", "", text)      #purge any leading commas
      text <- gsub(",(,)+", ",", text)  #transform multiple consecutive commas into a single comma
      text <- gsub(",$", "", text)      #purge any trailing commas
      split <- strsplit(text, ",", fixed = FALSE)[[1]]
      as.numeric(split)
    }
    
# **************************************************************************** #
    
    #  -------------------------------------------------------------------- #
    ## ------------------- Descriptive Stats functions --------------------
    #  -------------------------------------------------------------------- #
    
    
    ### Non-Reactive Functions ----
    # --------------------------------------------------------------------- #
    
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

    
    # Function for populating the value column of the datatable
    createDSColumn <- function(dat) ({
      
      sampSize <- length(dat)
      sampSum <- sum(dat)
      sumSquares <- sum(dat^2)
      xbar <- round(mean(dat),4)
      sampMode <- Modes(dat)
      sampMin <- min(dat)
      #popuStdDev <- round(pop.sd(dat),4) # round(sqrt((n-1)/n) * sampStdDev(dat), 4)
      quartile1 <-  quantile(dat, 0.25, type = 6) #fivenum(dat)[2]
      sampMedian <- median(dat)
      quartile3 <-  quantile(dat, 0.75, type = 6) #fivenum(dat)[4]
      sampMax <- max(dat)
      sampIQR <- round(quartile3 - quartile1, 4)
      lowerFence <- round(quartile1 - (1.5*sampIQR), 4)
      upperFence <- round(quartile3 + (1.5*sampIQR), 4)
      numOutliers <- sum(dat < lowerFence) + sum(dat > upperFence)
      sampRange <- range(dat)[2]-range(dat)[1]
      sampStdDev <- round(sd(dat),4)
      sampVar <- round(var(dat),4)
      sampMeanSE <- round(sd(dat)/sqrt(length(dat)), 4)
      coeffVar <- round(sampStdDev/xbar, 4)
      if(sampSize < 3){
        sampSkewness <- "Not enough observations."
      } else {
        sampSkewness <- round(skewness(dat, type = 2), 4)
      }
      if(sampSize < 4){
        sampKurtosis <- "Not enough observations."
      } else {
        sampKurtosis <- round(kurtosis(dat, type = 2), 4)
      }
      
      
      dfCol <- data.frame(Value = c(sampSize, 
                                    sampSum, 
                                    sumSquares, 
                                    xbar, 
                                    sampMode, 
                                    sampMin, 
                                    quartile1, 
                                    sampMedian, 
                                    quartile3, 
                                    sampMax, 
                                    sampIQR, 
                                    lowerFence, 
                                    upperFence, 
                                    numOutliers, 
                                    sampRange, 
                                    sampStdDev, 
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
    
    # Function to convert the raw data input into a numeric list
    dsRawData <- reactive ({
      dat <- createNumLst(input$descriptiveStat)
    })
    
    
    # Function to read the uploaded data file
    dsUploadData <- eventReactive(input$dsUserData, {
      ext <- tools::file_ext(input$dsUserData$name)
      
      switch(ext, 
             csv = read_csv(input$dsUserData$datapath),
             xls = read_excel(input$dsUserData$datapath),
             xlsx = read_excel(input$dsUserData$datapath),
             validate("Improper file format")
             #showModal( modalDialog(
             # title = "Warning",
             # "Improper File Format",
             # easyClose = TRUE
             #))
      )
    })
    
    
    getDsDataframe <- reactive({
      
      req(ds_iv$is_valid())
      
      df <- data.frame(Category = c("Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", 
                                    "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", 
                                    "Outliers", "Outliers", "Outliers", "Outliers", 
                                    "Dispersion", "Dispersion", "Dispersion", "Dispersion", "Dispersion", 
                                    "Distribution", "Distribution"),
                       Variable = c("Number of Observations", 
                                    "Sum", 
                                    "Sum of Squares", 
                                    "Mean", 
                                    "Mode", 
                                    "Minimum", 
                                    "First Quartile \\( (Q_{1}) \\)*", 
                                    "Second Quartile or Median \\( (Q_{2}) \\)", 
                                    "Third Quartile \\( (Q_{3}) \\)*", 
                                    "Maximum", 
                                    "Interquartile Range (IQR)", 
                                    "Check for Outliers: Lower Fence", 
                                    "Check for Outliers: Upper Fence", 
                                    "Number of Potential Outliers", 
                                    "Range", 
                                    "Sample Standard Deviation", 
                                    "Sample Variance", 
                                    "Standard Error of the Mean", 
                                    "Coefficient of Variation",
                                    "Skewness**", 
                                    "Kurtosis***"))
      
      
      if(input$dataInput == 'Upload Data')
      {
        for( x in input$dsUploadVars)
        {
          dat <- as.data.frame(dsUploadData())[, x]
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
                        "Min", 
                        "First Quartile (Q1)", 
                        "Median", 
                        "Third Quartile (Q3)", 
                        "Max", 
                        "IQR", 
                        "Lower Fence", 
                        "Upper Fence", 
                        "Potential Outliers", 
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
      hide(id = "descriptiveStatsMP")
      hide(id = "dsUploadVars")
      
      if(dsupload_iv$is_valid())
      {
        freezeReactiveValue(input, "dsUploadVars")
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "dsUploadVars",
                          choices = c(colnames(dsUploadData()))
                          )

        show(id = "dsUploadVars")
      }
    })
    
    
    # dsTableFilters <- reactive({
    #   filters <- list()
    #   
    #   filters <- c(filters, input$descriptives)
    #   filters <- c(filters, input$fiveNumSummary)
    #   filters <- c(filters, input$outliers)
    #   filters <- c(filters, input$dispersion)
    #   filters <- c(filters, input$distribution)
    #   
    #   return(filters)
    # })
    
    
    observeEvent(input$goDescpStats, {
      
      output$renderDescrStats <- renderUI({
        if(!dsupload_iv$is_valid())
        {
          validate(
            need(input$dsUserData, "Please upload your data to continue"),
            need(nrow(dsUploadData()) != 0 && ncol(dsUploadData()) > 0, "File is empty"),
            need(nrow(dsUploadData()) > 1, "Sample Data must include at least 2 observations"),
          
            errorClass = "myClass"
          )
        }
        else if(!dsuploadvars_iv$is_valid())
        {
          validate(
            need(input$dsUploadVars != "", "Please select a variable"),
              
            errorClass = "myClass"
          )

        }
        else if(!dsraw_iv$is_valid())
        {
          validate(
            need(!anyNA(dsRawData()), "Sample Data must be numeric"),
            need(length(dsRawData()) >= 2, "Sample Data must include at least 2 observations"),
            
            errorClass = "myClass"
          )
        }
        
        # tagList(
        #   # conditionalPanel(
        #   #   condition = "input.dataInput == 'Enter Raw Data'",
        #   tabsetPanel(id = "dsTabset", selected = "Table",
        #               
        #     tabPanel(id = "dsTable", title = "Table", value = 'Table',
        #       h3("Descriptive Statistics"),
        #       fluidRow(
        #         withMathJax(),
        #         column(align = "center", width = 12, withMathJax(DTOutput("dsTable")))
        #       ),
        #       br(),
        #       conditionalPanel(
        #         condition = "input.dsTableFilters.indexOf('First Quartile (Q1)') > -1 | input.dsTableFilters.indexOf('Third Quartile (Q3)') > -1",
        #         
        #         p("* Note: Q1 and Q3 are calculated by excluding Q2 on both sides"),
        #         br(),
        #       ),
        #       
        #     ),
        #   
        #     tabPanel(id = "dsGraphs", title = "Graphs", value = 'Graphs',
        #             conditionalPanel(
        #               condition = "input.dsGraphOptions == ''",
        #               
        #               br(),
        #               p("Select one or more options from the Graph Options menu to see more information.")
        #             ),
        #              
        #             conditionalPanel(
        #               condition = "input.dsGraphOptions.indexOf('Boxplot') > -1",
        #                
        #               h3("Boxplot"),
        #               fluidRow(
        #                column(align = "center", width = 12, plotOutput("dsBoxplot", width = '75%'))
        #               ),
        #               br(),
        #               br(),
        #               hr(),
        #               br(),
        #             ),
        #              
        #             conditionalPanel(
        #               condition = "input.dsGraphOptions.indexOf('Histogram') > -1",
        #              
        #               h3("Histogram"),
        #               fluidRow(
        #                column(align = "center", width = 12, plotOutput("dsHistogram", width = '75%'))
        #               ),
        #               br(),
        #               hr(),
        #               br(),
        #             ),
        #              
        #             conditionalPanel(
        #               condition = "input.dsGraphOptions.indexOf('Stem and Leaf Plot') > -1",
        #               
        #               h3("Stem and Leaf Plot"),
        #               br(),
        #               fluidRow(
        #                 column(width = 2, div("")),
        #                 column(width = 8, 
        #                        verbatimTextOutput('dsStemLeaf'),
        #                        br(),
        #                        p("* Note: Outlier values are listed under the HI/LO lists.")),
        #                 column(width = 2, div(""))
        #               ),
        #               br(),
        #             ),
        # 
        #       
        #     )
        #   )
        #     #plotOutput("boxplotgg"),
        #     #br(), 
        #     
        #     #downloadButton('downloadDataDS', 'Download Results')
        #     
        #   #),
        #   
        #   # conditionalPanel(
        #   #   condition = "input.dataInput == 'Upload Data'",
        #   #   
        #   # ),
        # )
      })
      
      if(ds_iv$is_valid())
      {
        # df <- data.frame(Category = c("Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", "Five Number Summary", "Five Number Summary",
        #                               "Five Number Summary", "Five Number Summary", "Five Number Summary", "Outliers", "Outliers", "Outliers", "Outliers", 
        #                               "Dispersion", "Dispersion", "Dispersion", "Dispersion", "Dispersion", "Distribution", "Distribution"),
        #                  Variable = c("Number of Observations", "Sum", "Sum of Squares", "Mean", "Mode", "Minimum", "First Quartile (Q1)*", 
        #                               "Second Quartile or Median (Q2)", "Third Quartile (Q3)*", "Maximum", "Interquartile Range (IQR)", 
        #                               "Check for Outliers: Lower Fence", "Check for Outliers: Upper Fence", "Number of Potential Outliers", "Range", 
        #                               "Sample Standard Deviation", "Sample Variance", "Standard Error of the Mean", "Coefficient of Variation",
        #                               "Skewness", "Kurtosis"))
        # 
        # 
        # if(input$dataInput == 'Upload Data')
        # {
        #   for( x in input$dsUploadVars)
        #   {
        #     dat <- as.data.frame(dsUploadData())[, x]
        #     print(dat)
        #     newCol <- createDSColumn(dat)
        #     df[x] <- newCol
        #   }
        #   colnames(df) <- c("Category", "Variable", input$dsUploadVars)
        # }
        # else
        # {
        #   dat <- dsRawData()
        #   print(dat)
        #   newCol <- createDSColumn(dat)
        #   df$Value <-newCol
        # }
        
        # xbar <- round(mean(dat),4)
        # sampStdDev <- round(sd(dat),4)
        # #popuStdDev <- round(pop.sd(dat),4) # round(sqrt((n-1)/n) * sampStdDev(dat), 4)
        # Quartile1 <- fivenum(dat)[2] #quantile(dat, 0.25, type = 5)
        # Quartile3 <- fivenum(dat)[4] #quantile(dat, 0.75, type = 5)
        # IQR <- Quartile3 - Quartile1
        # Lower_Fence <- Quartile1 - (1.5*IQR)
        # Upper_Fence <- Quartile3 + (1.5*IQR)
        # Num_Outliers <- sum(dat < Lower_Fence) + sum(dat > Upper_Fence)
        # CoeffVar <- round(sampStdDev/xbar,4)
        
        # print(dat)
        # print(Lower_Fence)
        # print(Upper_Fence)
        # print(sum(dat < Lower_Fence))
        # print(sum(dat > Upper_Fence))
        # print(Num_Outliers)
        
 
        #dataDS <- reactive(values)
        # row1 <- data.frame(Category = "Descriptives", Variable = "Number of observations", Value = paste0(length(dat)))
        # row2 <- data.frame(Category = "Descriptives", Variable = "Sum", Value = paste0(sum(dat)))
        # row3 <- data.frame(Category = "Descriptives", Variable = "Sum of squares", Value = paste0(sum(dat^2)))
        # row4 <- data.frame(Category = "Descriptives", Variable = "Mean", Value = xbar)
        # row5 <- data.frame(Category = "Descriptives", Variable = "Mode", Value = paste(Modes(dat)))
        # row6 <- data.frame(Category = "Five Number Summary", Variable = "Minimum", Value = paste0(min(dat)))
        # row7 <- data.frame(Category = "Five Number Summary", Variable = "First quartile (Q1)*", Value = Quartile1)
        # row8 <- data.frame(Category = "Five Number Summary", Variable = "Second quartile or median (Q2)", Value = paste0(median(dat)))
        # row9 <- data.frame(Category = "Five Number Summary", Variable = "Third quartile (Q3)*", Value = Quartile3)
        # row10 <- data.frame(Category = "Five Number Summary", Variable = "Maximum", Value = paste0(max(dat)))
        # row11 <- data.frame(Category = "Outliers", Variable = "Interquartile range (IQR)", Value = IQR)
        # row12 <- data.frame(Category = "Outliers", Variable = "Check for Outliers: Lower Fence", Value = Lower_Fence)
        # row13 <- data.frame(Category = "Outliers", Variable = "Check for Outliers: Upper Fence", Value = Upper_Fence)
        # row14 <- data.frame(Category = "Outliers", Variable = "Number of Potential Outliers", Value = Num_Outliers)
        # row15 <- data.frame(Category = "Dispersion", Variable = "Range", Value = paste0(range(dat)[2]-range(dat)[1]))
        # row16 <- data.frame(Category = "Dispersion", Variable = "Sample Standard Deviation", Value = sampStdDev)
        # row17 <- data.frame(Category = "Dispersion", Variable = "Sample Variance", Value = paste0(round(var(dat),4)))
        # row18 <- data.frame(Category = "Dispersion", Variable = "Standard Error of the Mean", Value = paste0(round(sd(dat)/sqrt(length(dat)),4)))
        # row19 <- data.frame(Category = "Dispersion", Variable = "Coefficient of variation", Value = CoeffVar)
        # row20 <- data.frame(Category = "Distribution", Variable = "Skewness", Value = paste0(round(skewness(dat),4)))
        # row21 <- data.frame(Category = "Distribution", Variable = "Kurtosis", Value = paste0(round(kurtosis(dat),4)))
        #row23 <- data.frame(Variable = "Population Standard Deviation (sigma)", Value = popuStdDev)
        
        # df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12, row13, row14, row15, row16, row17, row18, row19, row20, row21)
        # rownames(df) <- c("Observations", "Sum", "Sum of Squares", "Mean", "Mode", "Min", "First Quartile (Q1)", "Median", "Third Quartile (Q3)", "Max", "IQR", 
        #                   "Lower Fence", "Upper Fence", "Potential Outliers", "Range", "Sample Standard Deviation", "Sample Variance", "Standard Error of the Mean", 
        #                   "Coefficient of Variation", "Skewness", "Kurtosis")
        
        # if(is.null(input$dsGraphOptions)) {
        #   hideTab(inputId = "dsTabset", target = "Graphs")
        # }
        output$dsDataTable <- renderUI({
          tagList(
            
            conditionalPanel(
              condition = "input.dsTableFilters == ''",

              br(),
              p("Select one or more options from the Options menu to see more information.")
            ),
            
            conditionalPanel(
              condition = "input.dsTableFilters != ''",
              
              withMathJax(),
              withMathJax(DTOutput("dsTableData"))
            ),
            
          )
        })
        
        df <- getDsDataframe()
        filteredDf <- filter(df, rownames(df) %in% input$dsTableFilters)
        
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
                                           rownames = FALSE,
                                           filter = "none",
                                           
        ))
        
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
        
        output$dsBoxplot <- renderPlot({
          
          #--------------------#
          # Horizontal boxplot #
          #--------------------#
          
          #boxplot(dat, horizontal = TRUE, lty = 1,  pch = 8, col = "#819BB6") #pch = 19)
          
          ggplot(data.frame(x = dat), aes(x = x, y = 0)) +
            geom_boxplot(fill = "#03376d",
                         alpha = .5,
                         outlier.size = 3) +
            labs(x = "Values",
                 y = "") +
            theme_minimal() +
            theme(axis.title.x = element_text(size = 16, face = "bold", vjust = -1.5),
                  axis.text.x.bottom = element_text(size = 14),
                  axis.text.y.left = element_blank()) +
            scale_x_continuous(breaks = seq(floor(min(dat)), to = ceiling(max(dat)), by = ceiling( (max(dat) - min(dat))/10 ) )) +
            ylim(-1, 1)
          
          
          ## Add mean line
          # segments(x0 = mean(dat), y0 = 0.8,
          #          x1 = mean(dat), y1 = 1.2,
          #          col = "red", lwd = 2)
          # 
          # points(mean(dat), col = 3, pch = 19)
        })
        
        # output$dsBoxplot <- renderPlot({
        #   
        #   #-----------------#
        #   # ggplot2 boxplot #
        #   #-----------------#
        #   
        #   ggplot(as.data.frame(dat), aes(x = "", y = dat)) +
        #     geom_boxplot(show.legend = FALSE)
        # })
        
        output$dsHistogram <- renderPlot({
          pp <- ggplot(data.frame(x = dat)) +
            geom_histogram(aes(x = x),
                           bins = 15,
                           fill = "#03376d",
                           color = "black",
                           alpha = .5) +
            labs(x = "Values",
                 y = "Frequency") +
            theme_minimal() +
            theme(axis.title.x = element_text(size = 16, 
                                              face = "bold", 
                                              vjust = -1.5),
                  axis.title.y = element_text(size = 16, 
                                              face = "bold", 
                                              vjust = 1.5),
                  axis.text.x.bottom = element_text(size = 14),
                  axis.text.y.left = element_text(size = 14)) +
            scale_x_continuous(breaks = seq(floor(min(dat)), to = ceiling(max(dat)), by = ceiling( (max(dat) - min(dat))/10 ) )) 
          
          pp
        })
        
        output$dsStemLeaf <- renderPrint({
          stem.leaf(dat, m = 1, depths = FALSE)
        })
      
        show(id = 'descrStatsData')
      }
      #print(sort(dat)
      # show(id = 'descriptiveStatsMP') 
    })
    
    dsTableProxy <- dataTableProxy('dsTableData')
    
    observeEvent(input$dsTableFilters, {

      df <- getDsDataframe()
      newFilter <- filter(df, rownames(df) %in% input$dsTableFilters)

      replaceData(dsTableProxy, newFilter, resetPaging = FALSE, rownames = FALSE)
    })
    
    # --------------------------------------------------------------------- #
    
    
# **************************************************************************** #
    
    
    #  -------------------------------------------------------------------- #
    ## --------------- Probability Distribution functions -----------------
    #  -------------------------------------------------------------------- #

    
    ### Non-Reactive Functions ----
    # --------------------------------------------------------------------- #
    
    shadeNormArea <- function(x){
      area <- dnorm(x, input$popMean, input$popSD)
      
      if(input$calcNormal == "cumulative") #less
      {
        area[x > input$xValue] <- NA
      }
      else if(input$calcNormal == "between") #twosided
      {
        area[x <= input$x1Value | x >= input$x2Value] <- NA
      }
      else if(input$calcNormal == "upperTail") #greater
      {
        area[x < input$xValue] <- NA
      }
      return(area)
    }
    
    normPlot <- function(normValue){
      normTail = qnorm(0.999, mean = input$popMean, sd = input$popSD, lower.tail = FALSE)
      normHead = qnorm(0.999, mean = input$popMean, sd = input$popSD, lower.tail = TRUE)
      #xSeq = seq(normTail, normHead, by = 0.005)
      
      if(input$calcNormal == "between")
      {
        normLines <- c(input$x1Value, input$x2Value)
        probXLab <- (input$x2Value + input$x1Value)/2 
      }
      else
      {
        normLines <- input$xValue
        
        if(input$calcNormal == "cumulative")
        {
          probXLab <- (input$xValue - normHead/4)
        }
        else if(input$calcNormal == "upperTail")
        {
          probXLab <- (input$xValue + normHead/4)
        }
      }
      xSeq = sort(c(normTail, normHead, normLines, probXLab))
      
      df <- data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD))
      lineDF <- filter(df, x %in% normLines)
      probDF <- filter(df, x %in% probXLab)
      
      nPlot <- ggplot(df, aes(x = x, y = y)) +
        stat_function(fun = dnorm, 
                      args = list(mean = input$popMean, sd = input$popSD), 
                      geom = "density",
                      #xlim = c(normTail, normHead),
                      fill = "#03376d",
                      alpha = 0.3) + 
        stat_function(fun = shadeNormArea, 
                      geom = "area",
                      #xlim = c(normTail, normHead),
                      fill = "#03376d",
                      alpha = 0.7) +
        theme_minimal()  +
        theme(axis.title.x = element_text(size = 16, 
                                          face = "bold"),
              axis.text.x.bottom = element_text(size = 14)) +
        scale_x_continuous(breaks = waiver()) +
        scale_y_continuous(breaks = NULL) +
        ylab("") + xlab(paste("X")) +
        geom_text(data = lineDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 14 / .pt, 
                  fontface = "bold", 
                  vjust = "outward") 
      #geom_text(data = probDF, aes(x = x, y = y/2, label = normValue), size = 24 / .pt, fontface = "bold")
      
      
      return(nPlot)
    }
    
    # --------------------------------------------------------------------- #
    
    
    ### Reactives ----
    # --------------------------------------------------------------------- #
    
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
    
    # --------------------------------------------------------------------- #
    
    
    ### Observers ----
    # --------------------------------------------------------------------- #
    
    
    #### Binomial ----
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
                
                errorClass = "myClass"
              )
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
                
                errorClass = "myClass"
              )
            }
            
            validate(
              need(input$numTrialsBinom, "Number of Trials (n) must be a positive integer") %then%
                need(input$numTrialsBinom > 0 && input$numTrialsBinom %% 1 == 0, "Number of Trials (n) must be a positive integer"),
              need(input$successProbBinom, "Probability of Success (p) must be between 0 and 1") %then%
                need(input$successProbBinom >= 0 && input$successProbBinom <= 1, "Probability of Success (p) must be between 0 and 1"),
              
              errorClass = "myClass"
            )
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
                
                errorClass = "myClass"
              )
              
              if(input$calcBinom == 'exact'){
                #sprintf("\\(P(X = %1.0f) = %0.4f\\)",
                #        binom_x,
                #        dbinom(binom_x,binom_n,binom_p)
                #        )
                binomProb <- paste("P(X = ", binom_x, ")") #= ", dbinom(binom_x,binom_n,binom_p))
                binomForm <- paste("\\binom{", binom_n, "}{", binom_x, "}", binom_p, "^", binom_x, "(1-", binom_p, ")^{", binom_n, "-", binom_x, "}")
                binomVal <- round(dbinom(binom_x,binom_n,binom_p), 4)
              }
              else if(input$calcBinom == 'cumulative'){
                #sprintf("\\(P(X \\leq %1.0f) = %0.4f\\)",
                #        binom_x,
                #        pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE)
                #        )
                binomProb <- paste("P(X \\leq ", binom_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
                binomForm <- paste("\\sum_{x = 0}^", binom_x, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE), 4)
              }
              else if(input$calcBinom == 'upperTail'){
                #sprintf("\\(P(X \\geq %1.0f) = %0.4f\\)",
                #        binom_x,
                #        pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE)
                #        )
                binomProb <- paste("P(X \\geq ", binom_x, ")") # = ", pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE))
                binomForm <- paste("\\sum_{x = ", binom_x, "}^", binom_n, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE), 4)
                
              }
              else if(input$calcBinom == 'greaterThan'){
                #sprintf("\\(P(X \\gt %1.0f) = %0.4f\\)",
                #        binom_x,
                #        pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE)
                #        )
                binomProb <- paste("P(X \\gt ", binom_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE))
                binomForm <- paste("\\sum_{x = ", binom_x + 1, "}^", binom_n, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE), 4)
              }
              else if(input$calcBinom == 'lessThan'){
                #sprintf("\\(P(X \\lt %1.0f) = %0.4f\\)",
                #        binom_x,
                #        pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE)
                #        )
                binomProb <- paste("P(X \\lt ", binom_x, ")") # = ", pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE))
                binomForm <- paste("\\sum_{x = 0}^", binom_x - 1, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
                binomVal <- round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE), 4)
              }
              
              # if(input$probDistTable == TRUE){
              #   output$probabilityTable <- DT::renderDataTable(
              #     {
              #       dfBinom <- data.frame("Probability", value = round(dbinom(x = 0:binom_n, size = binom_n, prob = binom_p), 4))
              #     }
              #   )
              # }
            }
            else if(input$calcBinom == 'between')
            {
              binom_x1 <- input$numSuccessesBinomx1
              
              binom_x2 <- input$numSuccessesBinomx2
              
              validate(
                need(binom_x1 <= binom_n, "Number of Successes (x1) must be less than or equal to the Number of Trials (n)"),
                need(binom_x2 <= binom_n, "Number of Successes (x2) must be less than or equal to the Number of Trials (n)"),
                need(binom_x1 <= binom_x2, "Number of Successes (x1) must be less than or equal to Number of Successes (x2)"),
                
                errorClass = "myClass"
              )
              
              #sprintf("\\(P(%1.0f \\leq X \\leq %1.0f) = %0.4f - %0.4f = %0.4f\\)",
              #        binom_x1,
              #        binom_x2,
              #        pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE),
              #        pbinom(binom_x1-1,binom_n,binom_p,lower.tail = TRUE),
              #        pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE) - pbinom(binom_x1-1,binom_n,binom_p,lower.tail = TRUE)
              #       )
              binomProb <- paste("P(", binom_x1, " \\leq X \\leq ", binom_x2, ")")
              binomForm <- paste("\\sum_{x = ", binom_x1, "}^", binom_x2, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE) - pbinom(binom_x1-1,binom_n,binom_p,lower.tail = TRUE), 4)
            }
            
            tagList(
              withMathJax(
                div(
                  h3(
                    #sprintf("\\(Calculating \\enspace  %s \\enspace  when \\enspace  X \\sim B(%1.0f,%g): \\)",
                    sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Bin(%1.0f,%g): \\)",
                            binomProb,
                            binom_n,
                            binom_p)
                  ),
                  hr(),
                  br(),
                  p("Using the Probability Mass Function: "),
                  sprintf("\\( \\qquad \\qquad P(X = x) = \\binom{n}{x} p^x (1-p)^{n-x} \\)"),
                  sprintf("\\( \\qquad \\) for \\( x = 0, 1, 2, ... n\\)"),
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
                  sprintf("Mean \\( (\\mu) = np = %g\\)",
                          binom_mu),
                  br(),
                  br(),
                  sprintf("Standard Deviation \\( (\\sigma) = \\sqrt{np(1 - p)} = %g\\)",
                          binom_sd),
                  br(),
                  br(),
                  sprintf("Variance \\( (\\sigma^{2}) = np(1 - p) = %g\\)",
                          binom_var)
                ),
                br(),
                conditionalPanel(
                  condition = "input.showBinomTable == 1",
                  
                  br(),
                  titlePanel("Probability Distribution Table"),
                  hr(),
                  DTOutput("binomDistrTable", width = "25%"),
                  br()
                )
              )
            )
          }
        )
      })
      
      output$binomDistrTable <- DT::renderDT({
        req(pd_iv$is_valid())
        
        if(input$numTrialsBinom < 50)
        {
          dfBinom <- data.frame(value = seq(0, input$numTrialsBinom), value = round(dbinom(x = 0:input$numTrialsBinom, size = input$numTrialsBinom, prob = input$successProbBinom), 4))
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
                    filter = "none"
          )
        }
        
               
      })
    })

    #### Poisson ----
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
                
                errorClass = "myClass"
              )
            }
            
            if(!poissbetween_iv$is_valid())
            {
              validate(
                need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
                need(input$x2Poisson, "Enter a value for the Number of Successes (x1)") %then%
                  need(input$x1Poisson >= 0 && input$x1Poisson %% 1 == 0, "Number of Successes (x1) must be a positive integer"),
                need(input$x2Poisson, "Enter a value for the Number of Successes (x2)") %then%
                  need(input$x2Poisson >= 0 && input$x2Poisson %% 1 == 0, "Number of Successes (x2) must be a positive integer"),
                
                errorClass = "myClass"
              )
            }
            
            validate(
              need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
              
              errorClass = "myClass"
            )
          }
          else
          {
            poisson_mu <- input$muPoisson
            poisson_sd <- round(sqrt(input$muPoisson), 4)
            
            if(input$calcPoisson != 'between')
            {
              poisson_x <- input$xPoisson
              
              if(input$calcPoisson == 'exact'){
                #withMathJax(paste0("\\(P(X = \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(dpois(Poisson_x,Poisson_mu), 4)))
                poissProb <- paste("P(X = ", poisson_x, ")") #= ", dbinom(binom_x,binom_n,binom_p))
                poissForm <- paste("\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^", poisson_x, "}{", poisson_x, "!}")
                poissVal <- round(dpois(poisson_x,poisson_mu), 4)
              }
              else if(input$calcPoisson == 'cumulative'){
                #withMathJax(paste0("\\(P(X \\leq \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x,Poisson_mu,lower.tail = TRUE), 4)))
                poissProb <- paste("P(X \\leq ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
                poissForm <- paste("\\sum_{x = 0}^", poisson_x, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = TRUE), 4)
              }
              else if(input$calcPoisson == 'upperTail'){
                #withMathJax(paste0("\\(P(X \\geq \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x - 1,Poisson_mu,lower.tail = FALSE), 4)))
                poissProb <- paste("P(X \\geq ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
                poissForm <- paste("1 - \\sum_{x = 0}^", poisson_x - 1, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x - 1,poisson_mu,lower.tail = FALSE), 4)
              }
              else if(input$calcPoisson == 'greaterThan'){
                #withMathJax(paste0("\\(P(X > \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x,Poisson_mu,lower.tail = FALSE), 4)))
                poissProb <- paste("P(X \\gt ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
                poissForm <- paste("1 - \\sum_{x = 0}^", poisson_x, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = FALSE), 4)
              }
              else if(input$calcPoisson == 'lessThan'){
                #withMathJax(paste0("\\(P(X < \\)"," ", Poisson_x,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x - 1,Poisson_mu,lower.tail = TRUE), 4)))
                poissProb <- paste("P(X \\lt ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
                poissForm <- paste("\\sum_{x = 0}^", poisson_x - 1, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
                poissVal <- round(ppois(poisson_x - 1,poisson_mu,lower.tail = TRUE), 4)
              }
            }
            else if(input$calcPoisson == 'between')
            {
              validate(
                need(input$x1Poisson <= input$x2Poisson, "Number of Successes (x1) must be less than or equal to Number of Successes (x2)"),
                
                errorClass = "myClass"
              )
              
              poisson_x1 <- input$x1Poisson
              poisson_x2 <- input$x2Poisson
              
              #withMathJax(paste0("\\(P(", Poisson_x1, " ",  " \\leq X \\leq \\)"," ", Poisson_x2,"\\()\\)"," ","\\( = \\)"," ", round(ppois(Poisson_x2, Poisson_mu, lower.tail = TRUE), 4), "\\( - \\)", round(ppois(Poisson_x1 - 1, Poisson_mu, lower.tail = TRUE), 4), " ","\\( = \\)"," ", round(ppois(Poisson_x2, Poisson_mu, lower.tail = TRUE) - ppois(Poisson_x1 - 1, Poisson_mu, lower.tail = TRUE), 4)))
              poissProb <- paste("P(", poisson_x1, " \\leq X \\leq ", poisson_x2, ")")
              poissForm <- paste("\\sum_{x = ", poisson_x1, "}^", poisson_x2, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x2, poisson_mu, lower.tail = TRUE) - ppois(poisson_x1 - 1, poisson_mu, lower.tail = TRUE), 4)
            }
            
            tagList(
              withMathJax(
                div(
                  h4(
                    #sprintf("\\(Calculating \\enspace  %s \\enspace  when \\enspace  X \\sim B(%1.0f,%g): \\)",
                    sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Pois(%g): \\)",
                            poissProb,
                            poisson_mu)
                  ),
                  hr(),
                  br(),
                  p("Using the Probability Mass Function: "),
                  sprintf("\\( \\qquad \\qquad P(X = x) = \\dfrac{e^{-\\mu} \\mu^x}{x!} \\)"),
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
                  sprintf("Mean \\( (\\mu) = \\mu = %g\\)",
                          poisson_mu),
                  br(),
                  br(),
                  sprintf("Standard Deviation \\( (\\sigma) = \\sqrt{\\mu} = %g\\)",
                          poisson_sd),
                  br(),
                  br(),
                  sprintf("Variance \\( (\\sigma^{2}) = \\mu = %g\\)",
                          poisson_mu)
                ),
                br(),
                conditionalPanel(
                  condition = "input.showPoissTable == 1",
                  
                  br(),
                  titlePanel("Probability Distribution Table"),
                  hr(),
                  DTOutput("poissDistrTable", width = "25%"),
                  br()
                )
              )
            )
          }
        )
      })
      
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
        
      })
    })

       ### Normal ----
    observeEvent(input$goNormal, {
      
      output$renderProbabilityNorm <- renderUI({
        
        if(!pd_iv$is_valid())
        {
          if(!normprob_iv$is_valid())
          {
            validate(
              need(input$popMean, "Enter a value for Population Mean (mu)"),
              need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
              need(input$xValue, "Enter a value for Normally Distributed Variable (x)"),
              
              errorClass = "myClass"
            )
          }
          
          if(!normbetween_iv$is_valid())
          {
            validate(
              need(input$popMean, "Enter a value for Population Mean (mu)"),
              need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
              need(input$x1Value, "Enter a value for Normally Distributed Variable (x)"),
              need(input$x2Value, "Enter a value for Normally Distributed Variable (x)"),
              
              errorClass = "myClass"
            )
          }
          
          validate(
            need(input$popMean, "Enter a value for Population Mean (mu)"),
            need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
            
            errorClass = "myClass"
          )
        }
        
        norm_mu <- input$popMean
        norm_sigma <- input$popSD
          
        if(input$calcNormal != 'between')
        {
          norm_x <- input$xValue
              
          if(input$calcNormal == "cumulative"){
            normProb <- paste("P(X \\leq ", norm_x,")")
            normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\leq \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
            normForm <- paste("= P(Z \\leq", round((norm_x - norm_mu)/norm_sigma, 4), ")")
            #normValue <- round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = TRUE),4)
            #paste("\\(P(X \\leq \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = TRUE),4))
          }
          else if(input$calcNormal == "upperTail"){
            normProb <- paste("P(X \\gt ", norm_x,")")
            normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\gt \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
            normForm <- paste("= P(Z \\gt", round((norm_x - norm_mu)/norm_sigma, 4), ")")
            #normValue <- round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = FALSE),4)
            #paste("\\(P(X > \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = FALSE),4))
          }
        }
        else if(input$calcNormal == 'between')
        {
          norm_x1 <- input$x1Value
          norm_x2 <- input$x2Value
            
          validate(
            need(norm_x1 <= norm_x2, "Normally Distributed Variable (x1) must be less than or equal to Normally Distributed Variable (x2)"),
              
            errorClass = "myClass"
          )
  
          normProb <- paste("P(", norm_x1, " ",  " \\leq X \\leq"," ", norm_x2,")") 
          normProbTransform <- paste("P \\left( \\dfrac{", norm_x1, " - ", norm_mu, "}{", norm_sigma, "} \\leq \\dfrac{X - \\mu}{\\sigma} \\leq",
                             "\\dfrac{", norm_x2, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
          normForm <- paste("= P(", round((norm_x1 - norm_mu)/norm_sigma, 4), "\\leq Z \\leq", round((norm_x2 - norm_mu)/norm_sigma, 4), ") = ", 
                            round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE), 4), " - ", round(pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4))
          #normValue <- round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE) - pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4)
          #paste("\\(P(", norm_x1, " ",  " \\leq X \\leq \\)"," ", norm_x2,"\\()\\)"," ","\\( = \\)"," ", round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE), 4), "\\( - \\)", round(pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4), " ","\\( = \\)"," ",round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE) - pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4))
        }
        
        tagList(
          withMathJax(
            div(
              h4(
                #sprintf("\\(Calculating \\enspace  %s \\enspace  when \\enspace  X \\sim B(%1.0f,%g): \\)",
                sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim N(\\mu = %g, \\sigma^2 = %g): \\)",
                        normProb,
                        norm_mu,
                        norm_sigma^2)
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
                      getNormValue()),
              br(),
              br(),
              br(),
              sprintf("Mean \\( (\\mu) = %g\\)",
                      norm_mu),
              br(),
              br(),
              sprintf("Standard Deviation \\( (\\sigma) = %g\\)",
                      norm_sigma),
              br(),
              br(),
              sprintf("Variance \\( (\\sigma^{2}) = %g\\)",
                      norm_sigma^2)
            ),
            br(),
            hr(),
            br(),
            plotOutput('normDistrPlot', width = "75%", height = "300px"),
            br()
          )
        )
      })
    })
    
    output$normDistrPlot <- renderPlot({
      normPlot(getNormValue())
    })
    
    # --------------------------------------------------------------------- #
    
    
# **************************************************************************** #
    
    
    #  -------------------------------------------------------------------- #
    ## ----------------- Statistical Inference functions ------------------
    #  -------------------------------------------------------------------- #
    
    
    ### Non-Reactive Functions ----
    # --------------------------------------------------------------------- #
    
    shadeHtZArea <- function(x, critValues, altHypothesis){
      area <- dnorm(x, 0, 1)
      
      if(altHypothesis == "less") #less
      {
        area[x > critValues] <- NA
      }
      else if(altHypothesis == "two.sided") #twosided
      {
        area[x > critValues[1] & x < critValues[2]] <- NA
      }
      else if(altHypothesis == "greater") #greater
      {
        area[x < critValues] <- NA
      }
      return(area)
    }
    
    hypZTestPlot <- function(testStatistic, critValues, altHypothesis){
      normTail = qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE)
      normHead = qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE)
      #xSeq = seq(normTail, normHead, by = 0.005)
      xSeq = sort(c(normTail, normHead, testStatistic, critValues, 0))
      
      if(testStatistic < normTail)
      {
        normTail = testStatistic
        
      } else if(testStatistic > normHead)
      {
        normHead = testStatistic
      } 
      
      df <- data.frame(x = xSeq, y = dnorm(xSeq, 0, 1))
      cvDF <- filter(df, x %in% critValues)
      tsDF <- filter(df, x %in% testStatistic)
      centerDF <- filter(df, x %in% c(0))
      
      htPlot <- ggplot(df, aes(x = x, y = y)) +
        stat_function(fun = dnorm, 
                      geom = "density",
                      xlim = c(normTail, normHead),
                      fill = "#03376d",
                      alpha = 0.3) + 
        stat_function(fun = shadeHtZArea, 
                      args = list(critValues, altHypothesis), 
                      geom = "area",
                      xlim = c(normTail, normHead),
                      fill = "#03376d",
                      alpha = 0.7) +
        theme_void()  +
        scale_x_continuous(breaks = floor(normTail):ceiling(normHead)) +
        scale_y_continuous(breaks = NULL) +
        ylab("") + xlab("Z") +
        geom_segment(data = filter(df, x %in% c(0)), 
                     aes(x = x, xend = x, y = 0, yend = y), 
                     linetype = "dotted", 
                     linewidth = 0.75, 
                     color='#03376d') +
        geom_text(data = filter(df, x %in% c(0)), 
                  aes(x = x, y = y/2, label = "A R"), 
                  size = 16 / .pt, 
                  fontface = "bold") +
        geom_text(data = filter(df, x %in% c(0)), 
                  aes(x = x, y = 0, label = "0"), 
                  size = 14 / .pt, 
                  fontface = "bold", 
                  nudge_y = -.01) +
        geom_segment(data = tsDF, 
                     aes(x = x, xend = x, y = -0.01, yend = y + .03), 
                     linetype = "solid", 
                     linewidth = 1.25, 
                     color='#03376d') +
        geom_text(data = tsDF, 
                  aes(x = x, y = y, label = "TS"), 
                  size = 16 / .pt, 
                  fontface = "bold", 
                  nudge_y = .045) +
        geom_text(data = tsDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 14 / .pt, 
                  fontface = "bold", 
                  nudge_y = -.02) +
        geom_segment(data = cvDF, 
                     aes(x = x, xend = x, y = 0, yend = y), 
                     linetype = "blank", 
                     lineend = 'round', 
                     linewidth = 1.5, 
                     color='#03376d') +
        geom_text(data = cvDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 14 / .pt, 
                  fontface = "bold", 
                  nudge_y = -.01) +
        geom_text(data = cvDF, 
                  aes(x = x + x/4, y = y, label = "RR"), 
                  size = 16 / .pt, 
                  fontface = "bold", 
                  nudge_y = .03) +
        theme(axis.title.x = element_text(size = 16, face = "bold"))
      
      return(htPlot)
    }
    
    shadeHtTArea <- function(x, testStatistic, critValues, altHypothesis){
      area <- dt(x, testStatistic)
      
      if(altHypothesis == "less") #less
      {
        area[x > critValues] <- NA
      }
      else if(altHypothesis == "two.sided") #twosided
      {
        area[x > critValues[1] & x < critValues[2]] <- NA
      }
      else if(altHypothesis == "greater") #greater
      {
        area[x < critValues] <- NA
      }
      return(area)
    }
    
    
    hypTTestPlot <- function(testStatistic, critValues, altHypothesis){
      tTail = qt(0.999, df = testStatistic, lower.tail = FALSE)
      tHead = qt(0.999, df = testStatistic, lower.tail = TRUE)
      #xSeq = seq(normTail, normHead, by = 0.005)
      xSeq = sort(c(tTail, tHead, testStatistic, critValues, 0))
      
      if(testStatistic < tTail)
      {
        tTail = testStatistic
        
      } else if(testStatistic > tHead)
      {
        tHead = testStatistic
      } 
      
      df <- data.frame(x = xSeq, y = dt(xSeq, testStatistic))
      cvDF <- filter(df, x %in% critValues)
      tsDF <- filter(df, x %in% testStatistic)
      centerDF <- filter(df, x %in% c(0))
      
      htPlot <- ggplot(df, aes(x = x, y = y)) +
        stat_function(fun = dt, 
                      args = list(df = testStatistic), 
                      geom = "density",
                      xlim = c(tTail, tHead),
                      fill = "#03376d",
                      alpha = 0.3) + 
        stat_function(fun = shadeHtTArea, 
                      args = list(testStatistic, critValues, altHypothesis), 
                      geom = "area",
                      xlim = c(tTail, tHead),
                      fill = "#03376d",
                      alpha = 0.7) +
        theme_void()  +
        scale_x_continuous(breaks = floor(tTail):ceiling(tHead) ) +
        scale_y_continuous(breaks = NULL) +
        ylab("") + 
        xlab("t") +
        geom_segment(data = filter(df, x %in% c(0)), 
                     aes(x = x, xend = x, y = 0, yend = y), 
                     linetype = "dotted", 
                     linewidth = 0.75, color='#03376d') +
        geom_text(data = filter(df, x %in% c(0)), 
                  aes(x = x, y = y/2, label = "A R"), 
                  size = 16 / .pt, 
                  fontface = "bold") +
        geom_text(data = filter(df, x %in% c(0)), 
                  aes(x = x, y = 0, label = "0"), 
                  size = 14 / .pt, 
                  fontface = "bold", 
                  nudge_y = -.01) +
        geom_segment(data = tsDF, 
                     aes(x = x, xend = x, y = -0.01, yend = y + .03), 
                     linetype = "solid", 
                     linewidth = 1.25, 
                     color='#03376d') +
        geom_text(data = tsDF, 
                  aes(x = x, y = y, label = "TS"), 
                  size = 16 / .pt, 
                  fontface = "bold", 
                  nudge_y = .045) +
        geom_text(data = tsDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 14 / .pt, 
                  fontface = "bold", 
                  nudge_y = -.02) +
        geom_segment(data = cvDF, 
                     aes(x = x, xend = x, y = 0, yend = y), 
                     linetype = "blank", 
                     lineend = 'round', 
                     linewidth = 1.5, 
                     color='#03376d') +
        geom_text(data = cvDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 14 / .pt, 
                  fontface = "bold", 
                  nudge_y = -.01) +
        geom_text(data = cvDF, 
                  aes(x = x + x/4, y = y, label = "RR"), 
                  size = 16 / .pt, 
                  fontface = "bold", 
                  nudge_y = .03) +
        theme(axis.title.x = element_text(size = 16, face = "bold"))
      
      return(htPlot)
    }
    
    # --------------------------------------------------------------------- #


    ### Reactives ----
    # --------------------------------------------------------------------- #
    
    ConfLvl <- reactive({
      
      if(input$samplesSelect == '1') {
        
        if(input$confidenceLevel == '90%') {
          confLvl <- 0.9
        } else if(input$confidenceLevel == '95%') {
          confLvl <- 0.95
        } else {
          confLvl <- 0.99
        }
        
      } else if(input$samplesSelect == '2') {
        
        if(input$confidenceLevel2 == '90%') {
          confLvl <- 0.9
        } else if(input$confidenceLevel2 == '95%') {
          confLvl <- 0.95
        } else {
          confLvl <- 0.99
        }
      } 

      return(confLvl)
    })
    
    
    SigLvl <- reactive({
      
      if(input$samplesSelect == '1') {
        
        if(input$significanceLevel == "10%") {
          sigLvl <- 0.1 
        } else if(input$significanceLevel == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }
        
      } else if (input$samplesSelect == '2') {
        
        if(input$significanceLevel2 == "10%") {
          sigLvl <- 0.1 
        } else if(input$significanceLevel2 == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }
        
      }
      
      return(sigLvl)
    })
    
    
    OneMeanHypInfo <- reactive({
      hypTestSymbols <- list()
      
      if(input$altHypothesis == "3"){
        hypTestSymbols$alternative <- "greater"
        hypTestSymbols$nullHyp <- "\\leq"
        hypTestSymbols$altHyp <- "\\gt"
        hypTestSymbols$critZAlph <- "z_{\\alpha}"
      }
      else if(input$altHypothesis == "2"){
        hypTestSymbols$alternative <- "two.sided"
        hypTestSymbols$nullHyp <- "="
        hypTestSymbols$altHyp <- "\\neq"
        hypTestSymbols$critZAlph <- "\\pm z_{\\alpha/2}"
      }
      else{
        hypTestSymbols$alternative <- "less"
        hypTestSymbols$nullHyp <- "\\geq"
        hypTestSymbols$altHyp <- "\\lt"
        hypTestSymbols$critZAlph <- "-z_{\\alpha}"
      }
      
      return(hypTestSymbols)
    })
    
    
    OneMeanZIntSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean
      sigmaSampOne <- input$popuSD
      
      source('R/OneSampZInt.R')
      
      oneMeanZInt <- ZInterval(nSampOne, xbarSampOne, sigmaSampOne, ConfLvl())
      
      return(oneMeanZInt)
    })
    
    
    OneMeanZIntRaw <- reactive({
      req(si_iv$is_valid())
      
      datRawData <- createNumLst(input$sample1)
      rawSampleSize <- length(datRawData)
      rawSampleMean <- mean(datRawData)
      rawPopuSD <- input$popuSDRaw
      
      source("R/OneSampZInt.R")
      
      oneMeanZInt <- ZInterval(rawSampleSize, rawSampleMean, rawPopuSD, 
                               ConfLvl())
      
      return(oneMeanZInt)
    })
    
    
    OneMeanTIntSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean  
      sSampOne <- input$sampSD
      
      source('R/OneSampTInt.R')
      
      oneMeanTInt <- TInterval(nSampOne, xbarSampOne, sSampOne, ConfLvl())
      
      return(oneMeanTInt)
    })
    
    
    OneMeanTIntRaw <- reactive({
      req(si_iv$is_valid())
      
      datRawData <- createNumLst(input$sample1)
      rawSampleSize <- length(datRawData)
      rawSampleMean <- mean(datRawData)
      rawSampleSD <- sd(datRawData)
      
      source("R/OneSampTInt.R")
      
      oneMeanTInt <- TInterval(rawSampleSize, rawSampleMean, rawSampleSD, 
                               ConfLvl())
      
      return(oneMeanTInt) 
    })
    
    
    OneMeanZTestSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean 
      hypMeanSampOne <- input$hypMean 
      sigmaSampOne <- input$popuSD
      
      source("R/OneSampZTest.R")
      
      oneMeanZTest <- ZTest(nSampOne, xbarSampOne, sigmaSampOne, hypMeanSampOne,
                            OneMeanHypInfo()$alternative, SigLvl())
      
      return (oneMeanZTest)
    }) 
    
    OneMeanZTestRaw <- reactive({
      req(si_iv$is_valid())
      
      datRawData <- createNumLst(input$sample1)
      rawSampleSize <- length(datRawData)
      rawSampleMean <- mean(datRawData)
      rawPopuSD <- input$popuSDRaw
      hypMeanSampOne <- input$hypMean 
      
      source("R/OneSampZTest.R")
      
      oneMeanZTest <- ZTest(rawSampleSize, rawSampleMean, rawPopuSD, 
                            hypMeanSampOne, OneMeanHypInfo()$alternative, 
                            SigLvl())
      
      return (oneMeanZTest) 
    })
    
    
    OneMeanTTestSumm <- reactive({
      req(si_iv$is_valid())
      
      nSampOne <- input$sampleSize
      xbarSampOne <- input$sampleMean
      hypMeanSampOne <- input$hypMean 
      sSampOne <- input$sampSD
      
      source("R/OneSampTTest.R")
      
      oneMeanTTest <- TTest(nSampOne, xbarSampOne, sSampOne, hypMeanSampOne, 
                            OneMeanHypInfo()$alternative, SigLvl())
      
      return(oneMeanTTest)
    })
    
    
    OneMeanTTestRaw <- reactive({
      req(si_iv$is_valid())
      
      datRawData <- createNumLst(input$sample1)
      rawSampleSize <- length(datRawData)
      rawSampleMean <- mean(datRawData)
      rawSampleSD <- sd(datRawData)
      hypMeanSampOne <- input$hypMean 
      
      
      source("R/OneSampTTest.R")
      
      oneMeanTTest <- TTest(rawSampleSize, rawSampleMean, rawSampleSD, 
                            hypMeanSampOne, OneMeanHypInfo()$alternative, 
                            SigLvl())
      
      return(oneMeanTTest)
    })
    
    
    IndMeansSummData <- reactive({
      req(si_iv$is_valid())
      
      summData <- list()
      
      summData$n1 <- input$sampleSize1
      summData$xbar1 <- input$sampleMean1
      summData$n2 <- input$sampleSize2
      summData$xbar2 <- input$sampleMean2
      
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
      
      if(input$bothsigmaKnownRaw == 'bothKnown'){
        rawData$sd1 <- input$popuSDRaw1
        rawData$sd2 <- input$popuSDRaw2
      } else {
        rawData$sd1 <- sd(raw_sample1)
        rawData$sd2 <- sd(raw_sample2)
      }
      
      return(rawData)
    })
    
    
    IndMeansHypInfo <- reactive({
      hypTestSymbols <- list()
      
      if(input$altHypothesis2 == "3"){
        hypTestSymbols$alternative <- "greater"
        hypTestSymbols$nullHyp <- "\\leq"
        hypTestSymbols$altHyp <- "\\gt"
        hypTestSymbols$critZAlph <- "z_{\\alpha}"
      }
      else if(input$altHypothesis2 == "2"){
        hypTestSymbols$alternative <- "two.sided"
        hypTestSymbols$nullHyp <- "="
        hypTestSymbols$altHyp <- "\\neq"
        hypTestSymbols$critZAlph <- "\\pm z_{\\alpha/2}"
      }
      else{
        hypTestSymbols$alternative <- "less"
        hypTestSymbols$nullHyp <- "\\geq"
        hypTestSymbols$altHyp <- "\\lt"
        hypTestSymbols$critZAlph <- "-z_{\\alpha}"
      }
      
      return(hypTestSymbols)
    })
    
    # --------------------------------------------------------------------- #
    
    

    ### Outputs ----
    # --------------------------------------------------------------------- #
    
    output$renderInference <- renderUI({
      
      if(!onemean_iv$is_valid())
      {
        validate(
          need(input$sampleSize, "Sample size (n) must be a positive integer.") %then%
            need(input$sampleSize > 0 & input$sampleSize %% 1 == 0, "Sample size (n) must be a positive integer."),
          need(input$sampleMean, "Sample mean required."),
          
          errorClass = "myClass"
        )
      }
      
      if(!onemeanraw_iv$is_valid())
      {
        validate(
          need(input$sample1, "Sample Data required.") %then%
            need(length(createNumLst(input$sample1)) > 1, "Sample Data requires a minimum of 2 data points."),
          need(input$popuSDRaw & input$popuSDRaw > 0, "Population Standard Deviation must be positive."),
          #need(input$popuSDRaw > 0, "Population Standard Deviation must be greater than 0"),
          
          errorClass = "myClass"
        )
      }
      
      if(!onemeansdknown_iv$is_valid())
      {
        validate(
          need(input$popuSD & input$popuSD > 0, "Population Standard Deviation must be positive."),
          #need(input$popuSD > 0, "Population Standard Deviation must be greater than 0"),
          
          errorClass = "myClass"
        )
      }
      
      if(!onemeansdunk_iv$is_valid())
      {
        validate(
          need(input$sampSD && input$sampSD > 0, "Sample Standard Deviation (s) must be positive."),
          
          errorClass = "myClass"
        )
      }
      
      if(!onemeanht_iv$is_valid())
      {
        validate(
          need(input$hypMean, "Hypothesized Population Mean value required."),
          
          errorClass = "myClass"
        )
      }
      
      if(!indmeanssumm_iv$is_valid()) {
        
        validate(
          need(input$sampleSize1, "Sample Size 1 (n1) must be a positive integer.") %then%
            need(input$sampleSize1 > 0 & input$sampleSize1 %% 1 == 0, "Sample Size 1 (n1) must be a positive integer."),
          need(input$sampleMean1, "Sample Mean 1 required."),
          
          need(input$sampleSize2, "Sample Size 2 (n2) must be a positive integer.") %then%
            need(input$sampleSize2 > 0 & input$sampleSize2 %% 1 == 0, "Sample Size 2 (n2) must be a positive integer."),
          need(input$sampleMean2, "Sample Mean 2 required."),
          
          errorClass = "myClass"
        )
      }
      
      if(!indmeanssdknown_iv$is_valid())
      {
        validate(
          need(input$popuSD1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
          need(input$popuSD2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
          
          errorClass = "myClass"
        )
      }
      
      if(!indmeanssdunk_iv$is_valid())
      {
        validate(
          need(input$sampSD1 && input$sampSD1 > 0, "Sample Standard Deviation (s1) must be positive."),
          need(input$sampSD2 && input$sampSD2 > 0, "Sample Standard Deviation (s2) must be positive."),
          
          errorClass = "myClass"
        )
      }
      
      if(!indmeansraw_iv$is_valid()) {
        
        validate(
          need(input$raw_sample1, "Sample 1 requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$raw_sample1)) > 2, "Sample Data requires a minimum of 3 data points."),
          need(input$raw_sample2, "Sample 2 requires a minimum of 3 data points.") %then%
            need(length(createNumLst(input$raw_sample2)) > 2, "Sample Data requires a minimum of 3 data points."),
          
          errorClass = "myClass"
        )
        
        validate("Samples require a minimum of 3 data points.")
      }
      
      if(!indmeansrawsd_iv$is_valid()) {
        
        validate(
          need(input$popuSDRaw1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
          need(input$popuSDRaw2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
          
          errorClass = "myClass"
        )
      }
    })
    
    
    
    output$oneSampMeanData <- renderDT({
      withMathJax()
      oneMeanData <- data.frame(Variable = character(), Value = character())
      
      if(input$inferenceType == 'Confidence Interval'){
        
        if(input$dataAvailability == 'Summarized Data'){
          
          if(input$sigmaKnown == 'Known'){
            
            oneMeanZInt <- OneMeanZIntSumm()
            
            row1 <- data.frame(Variable = "Sample Mean \\( (\\bar{x}) \\)", Value = oneMeanZInt["Sample Mean"])
            row2 <- data.frame(Variable = "Z Critical Value \\( (CV) \\)", Value = oneMeanZInt["Z Critical"])
            row3 <- data.frame(Variable = "Standard Error \\( (SE) \\)", Value = oneMeanZInt["Std Error"])
            row4 <- data.frame(Variable = "Lower Confidence Limit \\( (LCL) \\)", Value = oneMeanZInt["LCL"])
            row5 <- data.frame(Variable = "Upper Confidence Limit \\( (UCL) \\)", Value = oneMeanZInt["UCL"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5)
            
          }
          else if(input$sigmaKnown == 'Unknown'){
            
            oneMeanTInt <- OneMeanTIntSumm()
            
            row1 <- data.frame(Variable = "Sample Mean \\( (\\bar{x})\\)", Value = oneMeanTInt["Sample Mean"])
            row2 <- data.frame(Variable = "T Critical Value \\( (CV)\\)", Value = oneMeanTInt["T Critical"])
            row3 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = oneMeanTInt["Std Error"])
            row4 <- data.frame(Variable = "Lower Confidence Limit \\( (LCL)\\)", Value = oneMeanTInt["LCL"])
            row5 <- data.frame(Variable = "Upper Confidence Limit \\( (UCL)\\)", Value = oneMeanTInt["UCL"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5)
            
          } 
        }
        else if(input$dataAvailability == 'Enter Raw Data'){
          
          if(input$sigmaKnownRaw == 'rawKnown'){
            
            oneMeanZInt <- OneMeanZIntRaw()
            
            row1 <- data.frame(Variable = "Sample Mean \\( (\\bar{x})\\)", Value = oneMeanZInt["Sample Mean"])
            row2 <- data.frame(Variable = "Z Critical Value \\( (CV)\\)", Value = oneMeanZInt["Z Critical"])
            row3 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = oneMeanZInt["Std Error"])
            row4 <- data.frame(Variable = "Lower Confidence Limit \\( (LCL)\\)", Value = oneMeanZInt["LCL"])
            row5 <- data.frame(Variable = "Upper Confidence Limit \\( (UCL)\\)", Value = oneMeanZInt["UCL"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5)
            
          }
          else if(input$sigmaKnownRaw == 'rawUnknown'){
            
            oneMeanTInt <- OneMeanTIntRaw()
            
            row1 <- data.frame(Variable = "Sample Mean \\( (\\bar{x})\\)", Value = oneMeanTInt["Sample Mean"])
            row2 <- data.frame(Variable = "T Critical Value \\( (CV)\\)", Value = oneMeanTInt["T Critical"])
            row3 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = oneMeanTInt["Std Error"])
            row4 <- data.frame(Variable = "Lower Confidence Limit \\( (LCL)\\)", Value = oneMeanTInt["LCL"])
            row5 <- data.frame(Variable = "Upper Confidence Limit \\( (UCL)\\)", Value = oneMeanTInt["UCL"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5)
            
          }
        }
      }
      else if(input$inferenceType == 'Hypothesis Testing'){
        
        if(input$dataAvailability == 'Summarized Data'){
          
          if(input$sigmaKnown == 'Known'){
            
            oneMeanZTest <- OneMeanZTestSumm()
            
            row1 <- data.frame(Variable = "Sample Size \\( (n)\\)", Value = oneMeanZTest["Sample Size"])
            row2 <- data.frame(Variable = "Sample Mean \\( (\\bar{x})\\)", Value = oneMeanZTest["Sample Mean"])
            row3 <- data.frame(Variable = "Population Standard Deviation \\( (\\sigma)\\)", Value = oneMeanZTest["Population SD"])
            row4 <- data.frame(Variable = "Z Critical Value \\( (CV)\\)", Value = oneMeanZTest["Z Critical"])
            row5 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = oneMeanZTest["Std Error"])
            row6 <- data.frame(Variable = "Test Statistic \\( (TS)\\)", Value = oneMeanZTest["Test Statistic"])
            row7 <- data.frame(Variable = "P-Value \\( (P)\\)", Value = oneMeanZTest["P-Value"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5, row6, row7) 
            
          }
          else if(input$sigmaKnown == 'Unknown'){
            
            oneMeanTTest <- OneMeanTTestSumm()
            
            row1 <- data.frame(Variable = "Sample Size \\( (n)\\)", Value = oneMeanTTest["Sample Size"])
            row2 <- data.frame(Variable = "Sample Mean \\( (\\bar{x})\\)", Value = oneMeanTTest["Sample Mean"])
            row3 <- data.frame(Variable = "Sample Standard Deviation \\( (s)\\)", Value = oneMeanTTest["Sample SD"])
            row4 <- data.frame(Variable = "T Critical Value \\( (CV)\\)", Value = oneMeanTTest["T Critical"])
            row5 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = oneMeanTTest["Std Error"])
            row6 <- data.frame(Variable = "Test Statistic \\( (TS)\\)", Value = oneMeanTTest["Test Statistic"])
            row7 <- data.frame(Variable = "P-Value \\( (P) \\)", Value = oneMeanTTest["P-Value"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5, row6, row7)
            
          } 
        }
        else if(input$dataAvailability == 'Enter Raw Data'){
          
          if(input$sigmaKnownRaw == 'rawKnown'){
            
            oneMeanZTest <- OneMeanZTestRaw()
            
            row1 <- data.frame(Variable = "Sample Size \\( (n)\\)", Value = oneMeanZTest["Sample Size"])
            row2 <- data.frame(Variable = "Sample Mean \\( (\\bar{x})\\)", Value = oneMeanZTest["Sample Mean"])
            row3 <- data.frame(Variable = "Population Standard Deviation \\( (\\sigma)\\)", Value = oneMeanZTest["Population SD"])
            row4 <- data.frame(Variable = "Z Critical Value \\( (CV)\\)", Value = oneMeanZTest["Z Critical"])
            row5 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = oneMeanZTest["Std Error"])
            row6 <- data.frame(Variable = "Test Statistic \\( (TS)\\)", Value = oneMeanZTest["Test Statistic"])
            row7 <- data.frame(Variable = "P-Value \\( (P)\\)", Value = oneMeanZTest["P-Value"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5, row6, row7) 
            
            
          }
          else if(input$sigmaKnownRaw == 'rawUnknown'){
            
            oneMeanTTest <- OneMeanTTestRaw()
            
            row1 <- data.frame(Variable = "Sample Size \\( n \\)", Value = oneMeanTTest["Sample Size"])
            row2 <- data.frame(Variable = "Sample Mean \\((\\bar{x})\\)", Value = oneMeanTTest["Sample Mean"])
            row3 <- data.frame(Variable = "Sample Standard Deviation \\((s)\\)", Value = oneMeanTTest["Sample SD"])
            row4 <- data.frame(Variable = "T Critical Value \\((CV)\\)", Value = oneMeanTTest["T Critical"])
            row5 <- data.frame(Variable = "Standard Error \\( (SE) \\)", Value = oneMeanTTest["Std Error"])
            row6 <- data.frame(Variable = "Test Statistic \\((TS)\\)", Value = oneMeanTTest["Test Statistic"])
            row7 <- data.frame(Variable = "P-Value \\((P)\\)", Value = oneMeanTTest["P-Value"])
            
            oneMeanData <- rbind(row1, row2, row3, row4, row5, row6, row7)
            
          } 
        }
      }
      withMathJax()
      datatable(oneMeanData,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE
                ),
                rownames = FALSE,
                filter = "none"
      )
      
    })
    
    
    output$oneMeanCI <- renderUI({
      if(input$dataAvailability == 'Summarized Data'){
        
        if(input$sigmaKnown == 'Known'){
          
          oneMeanData <- OneMeanZIntSumm()
          sdSymbol <- "\\sigma"
          testStat <- "z"
          critVal <- oneMeanData["Z Critical"]
          
        } else if(input$sigmaKnown == 'Unknown'){
          
          oneMeanData <- OneMeanTIntSumm()
          sdSymbol <- "s"
          testStat <- "t"
          critVal <- oneMeanData["T Critical"]
          
        } 
      } else if(input$dataAvailability == 'Enter Raw Data'){
        
        if(input$sigmaKnownRaw == 'rawKnown'){
          
          oneMeanData <- OneMeanZIntRaw()
          sdSymbol <- "\\sigma"
          testStat <- "z"
          critVal <- oneMeanData["Z Critical"]
          
        } else if(input$sigmaKnownRaw == 'rawUnknown'){
          
          oneMeanData <- OneMeanTIntRaw()
          sdSymbol <- "s"
          testStat <- "t"
          critVal <- oneMeanData["T Critical"]
        } 
      }
      
      p(
        withMathJax(
          sprintf("CI \\(= \\bar{x} \\pm %s_{\\alpha/2} * \\dfrac{%s}{\\sqrt{n}}\\)",
                  testStat,
                  sdSymbol),
          br(),
          br(),
          sprintf("CI \\( \\displaystyle = %g \\pm \\left( %g \\cdot \\dfrac{%g}{\\sqrt{%g}} \\right) \\)",
                  oneMeanData["Sample Mean"],
                  critVal,
                  oneMeanData[3],
                  oneMeanData['Sample Size']),
          br(),
          br(),
          sprintf("CI \\(= (%g, %g)\\)",
                  oneMeanData["LCL"],
                  oneMeanData["UCL"]),
          br(),
          br(),
          br(),
          p(tags$b("Interpretation:")),
          sprintf("We are %1.0f%% confident that the population mean \\( (\\mu)\\) is between %g and %g.",
                  ConfLvl()*100,
                  oneMeanData["LCL"],
                  oneMeanData["UCL"]),
          br()
        )
      )
    })
    
    
    output$oneMeanHT <- renderUI({
      withMathJax()
      
      if(input$dataAvailability == 'Summarized Data'){
        
        if(input$sigmaKnown == 'Known'){
          oneMeanData <- OneMeanZTestSumm()
          sdSymbol <- "\\sigma"
          testStat <- "z"
        }
        else if(input$sigmaKnown == 'Unknown'){
          oneMeanData <- OneMeanTTestSumm()
          sdSymbol <- "s"
          testStat <- "t"
        } 
      }
      else if(input$dataAvailability == 'Enter Raw Data'){
        
        if(input$sigmaKnownRaw == 'rawKnown'){
          oneMeanData <- OneMeanZTestRaw()
          sdSymbol <- "\\sigma"
          testStat <- "z"
        }
        else if(input$sigmaKnownRaw == 'rawUnknown'){
          oneMeanData <- OneMeanTTestRaw()
          sdSymbol <- "s"
          testStat <- "t"
        } 
      }
      
      intrpInfo <- OneMeanHypInfo()
      
      if(oneMeanData[7] < 0.0001)
      {
        pValue <- "\\lt 0.0001"
      }
      else
      {
        pValue <- paste(oneMeanData[7])
      }
      
      if(oneMeanData[7] > SigLvl())
      {
        pvalSymbol <- "\\( \\gt\\)"
        suffEvidence <- "do not provide"
        reject <- "do not reject"
        region <- "acceptance"
      }
      else
      {
        pvalSymbol <- "\\( \\leq\\)"
        suffEvidence <- "provide"
        reject <- "reject"
        region <- "rejection"
      }
      
      if(intrpInfo$alternative == "two.sided")
      {
        if(testStat == 'z') {
          critVal <- paste("\\( \\pm\\)", oneMeanData["Z Critical"])
        } else {
          critVal <- paste("\\( \\pm\\)", oneMeanData["T Critical"])
        }
        
      }
      else
      {
        if(testStat == 'z') {
          critVal <- paste(oneMeanData["Z Critical"])
        } else {
          critVal <- paste(oneMeanData["T Critical"])
        }
        
      }
      
      tagList(
        
        p(
          withMathJax(
            #h4(tags$u("Performing the Hypothesis Test:")),
            #br(),
            sprintf("\\( H_{0}: \n \\mu %s %s\\)",
                    intrpInfo$nullHyp,
                    input$hypMean),
            br(),
            sprintf("\\( H_{a}: \\mu %s %s\\)",
                    intrpInfo$altHyp,
                    input$hypMean),
            br(),
            br(),
            sprintf("\\( \\alpha = %s \\)",
                    SigLvl()),
            br(),
            br(),
            sprintf("\\(%s = \\dfrac{\\bar{x} - \\mu_{0}}{%s / \\sqrt{n}} = \\dfrac{%s - %s}{%s / \\sqrt{%s}}\\)",
                    testStat,
                    sdSymbol,
                    oneMeanData[2],
                    input$hypMean,
                    oneMeanData[3],
                    oneMeanData[1]),
            br(),
            br(),
            sprintf("\\(%s = %g\\)",
                    testStat,
                    oneMeanData[6]),
            br(),
            br(),
            br(),
            p(tags$b("Using P-Value Method:")),
            sprintf("\\(P = %s\\)",
                    pValue),
            br(),
            sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
                    pvalSymbol,
                    SigLvl(),
                    reject),
            br(),
            br(),
            br(),
            p(tags$b("Using Critical Value Method:")),
            sprintf("Critical Value(s) = %s",
                    critVal),
            br(),
            sprintf("Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
                    testStat,
                    region,
                    reject)
            
          )
        ),
        
        plotOutput('oneMeanHTPlot', width = "75%", height = "300px"),
        br(),
        
        withMathJax(
          p(tags$b("Conclusion:")),
          p(
            sprintf("At the %1.0f%% significance level, the data %s sufficient evidence to reject the null hypothesis \\( (H_{0}) \\) that the population 
                              mean \\( (\\mu) \\) \\( %s \\) %g.",
                    SigLvl()*100,
                    suffEvidence,
                    intrpInfo$nullHyp,
                    input$hypMean),
            br(),
          )
        )
      )
    })
    
    
    output$oneMeanHTPlot <- renderPlot({
      
      if(input$dataAvailability == 'Summarized Data'){
        
        if(input$sigmaKnown == 'Known'){
          oneMeanData <- OneMeanZTestSumm()
        }
        else if(input$sigmaKnown == 'Unknown'){
          oneMeanData <- OneMeanTTestSumm()
        }
      }
      else if(input$dataAvailability == 'Enter Raw Data'){
        
        if(input$sigmaKnownRaw == 'rawKnown'){
          oneMeanData <- OneMeanZTestRaw()
        }
        else if(input$sigmaKnownRaw == 'rawUnknown'){
          oneMeanData <- OneMeanTTestRaw()
        }
      }
      
      intrpInfo <- OneMeanHypInfo()
      
      if(intrpInfo$alternative == "two.sided")
      {
        critZVal <- paste("\\( \\pm\\)", oneMeanData[4])
        htPlotCritVals <- c(-oneMeanData[4], oneMeanData[4])
      }
      else
      {
        critZVal <- paste(oneMeanData[4])
        htPlotCritVals <- oneMeanData[4]
      }
      
      if(input$sigmaKnown == 'Known' || input$sigmaKnownRaw == 'rawKnown')
      {
        oneMeanPlot <- hypZTestPlot(oneMeanData[6], htPlotCritVals, intrpInfo$alternative)
      }
      else
      {
        oneMeanPlot <- hypTTestPlot(oneMeanData[6], htPlotCritVals, intrpInfo$alternative)
      }
      
      oneMeanPlot
    })
    
    
    # --------------------------------------------------------------------- #
    
    
    ### Observers ----
    # --------------------------------------------------------------------- #
    
    observeEvent(input$goInference, {
      #output$renderInference <- renderDataTable(
      
      if(si_iv$is_valid())
      {
        show(id = "inferenceData")
      }

        if(input$samplesSelect == '1'){
          
          if(input$inferenceType == 'Confidence Interval'){
            
            if(input$confidenceLevel == '90%'){
              confLvl <- 0.9
            }
            else if(input$confidenceLevel == '95%'){
              confLvl <- 0.95
            }
            else{
              confLvl <- 0.99
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
              nullHyp <- "\\leq"
              altHyp <- "\\gt"
              critZAlph <- "z_{\\alpha}"
            }
            else if(input$altHypothesis == "2"){
              alternative <- "two.sided"
              nullHyp <- "="
              altHyp <- "\\neq"
              critZAlph <- "\\pm z_{\\alpha/2}"
            }
            else{
              alternative <- "less"
              nullHyp <- "\\geq"
              altHyp <- "\\lt"
              critZAlph <- "-z_{\\alpha}"
            }
          }

          if(input$popuParameter == 'Population Mean'){ 
            if(si_iv$is_valid())
            {
              # output$renderInference <- renderUI({
              #   ""
              # })
              
              output$oneSampMeanTable <- renderUI({
                tagList(
                  withMathJax(),
                  withMathJax(DTOutput('oneSampMeanData', width = "95%"))
                )
              })
              
            #   if(input$dataAvailability == 'Summarized Data'){
            #     
            #     nSampOne <- input$sampleSize
            #     xbarSampOne <- input$sampleMean
            #     
            #     if(input$inferenceType == 'Confidence Interval'){
            #       
            #       if(input$sigmaKnown == 'Known'){
            #         
            #         sigmaSampOne <- input$popuSD
            #         
            #         source('R/OneSampZInt.R')
            #         
            #         print("Confidence Interval for One Population Mean when Population Standard Deviation is known")
            #         
            #         zIntPrint <- ZInterval(nSampOne, xbarSampOne, sigmaSampOne, confLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfKnown <- data.frame(Variable = character(), Value = character())
            #         output$oneSampCI <- renderTable(values$dfKnown)
            #         
            #         row1 <- data.frame(Variable = "Sample Mean", Value = paste(zIntPrint[1]))
            #         row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(zIntPrint[2]))
            #         row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(zIntPrint[3]))
            #         row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(zIntPrint[4]))
            #         row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(zIntPrint[5]))
            #         
            #         values$dfKnown <- rbind(row1, row2, row3, row4, row5)
            #       }
            #       
            #       else if(input$sigmaKnown == 'Unknown'){
            #         
            #         sSampOne <- input$sampSD
            #         
            #         source('R/OneSampTInt.R')
            #         
            #         print("Confidence Interval for One Population Mean when Population Standard Deviation is unknown")
            #         
            #         tIntPrint <- TInterval(nSampOne, xbarSampOne, sSampOne, confLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfUnknown <- data.frame(Variable = character(), Value = character())
            #         output$oneSampCIUnknown <- renderTable(values$dfUnknown)
            #         
            #         row1 <- data.frame(Variable = "Sample Mean", Value = paste(tIntPrint[1]))
            #         row2 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(tIntPrint[2]))
            #         row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(tIntPrint[3]))
            #         row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(tIntPrint[4]))
            #         row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(tIntPrint[5]))
            #         
            #         values$dfUnknown <- rbind(row1, row2, row3, row4, row5)
            #       } # input$sigmaKnown == 'Unknown'
            #     } # input$inferenceType == 'Confidence Interval'
            #     
            #     else if(input$inferenceType == 'Hypothesis Testing'){
            #       
            #       hypMeanSampOne <- input$hypMean 
            #       
            #       if(input$sigmaKnown == 'Known'){
            #         
            #         sigmaSampOne <- input$popuSD
            #         
            #         source("R/OneSampZTest.R")
            #         
            #         ZTest <- ZTest(nSampOne, xbarSampOne, sigmaSampOne, hypMeanSampOne, alternative, sigLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfKnownHyp <- data.frame(Variable = character(), Value = character())
            #         output$oneSampHT <- renderTable(values$dfKnownHyp)
            #         
            #         row1 <- data.frame(Variable = "Sample Size", Value = paste(ZTest[1]))
            #         row2 <- data.frame(Variable = "Sample Mean", Value = paste(ZTest[2]))
            #         row3 <- data.frame(Variable = "Population Standard Deviation", Value = paste(ZTest[3]))
            #         row4 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(ZTest[4]))
            #         row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(ZTest[5]))
            #         row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(ZTest[6]))
            #         row7 <- data.frame(Variable = "P-Value", Value = paste(ZTest[7]))
            #         
            #         values$dfKnownHyp <- rbind(row1, row2, row3, row4, row5, row6, row7) 
            #       }
            #       
            #       else if(input$sigmaKnown == 'Unknown'){
            #         
            #         sSampOne <- input$sampSD
            #         
            #         source("R/OneSampTTest.R")
            #         
            #         TTest <- TTest(nSampOne, xbarSampOne, sSampOne, hypMeanSampOne, alternative, sigLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfUnKnownHyp <- data.frame(Variable = character(), Value = character())
            #         output$oneSampHTUnknown <- renderTable(values$dfUnKnownHyp)
            #         
            #         row1 <- data.frame(Variable = "Sample Size", Value = paste(TTest[1]))
            #         row2 <- data.frame(Variable = "Sample Mean", Value = paste(TTest[2]))
            #         row3 <- data.frame(Variable = "Sample Standard Deviation", Value = paste(TTest[3]))
            #         row4 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TTest[4]))
            #         row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TTest[5]))
            #         row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TTest[6]))
            #         row7 <- data.frame(Variable = "P-Value", Value = paste(TTest[7]))
            #         
            #         values$dfUnKnownHyp <- rbind(row1, row2, row3, row4, row5, row6, row7) 
            #       } # input$sigmaKnown == 'Unknown'
            #     } # input$inferenceType == 'Hypothesis Testing'
            #   } # input$dataAvailability == 'Summarized Data'
            #   
            #   else if(input$dataAvailability == 'Enter Raw Data'){
            #     
            #     datRawData <- createNumLst(input$sample1)
            #     
            #     rawSampleSize <- length(datRawData)
            #     rawSampleMean <- mean(datRawData)
            #     
            #     if(input$inferenceType == 'Confidence Interval'){
            #       
            #       if(input$sigmaKnownRaw == 'rawKnown'){
            #         
            #         rawPopuSD <- input$popuSDRaw
            #         
            #         source("R/OneSampZInt.R")
            #         
            #         ZIntervalRaw <- ZInterval(rawSampleSize, rawSampleMean, rawPopuSD, confLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfKnownRaw <- data.frame(Variable = character(), Value = character())
            #         output$oneSampCIRaw <- renderTable(values$dfKnownRaw)
            #         
            #         row1 <- data.frame(Variable = "Sample Mean", Value = paste(ZIntervalRaw[1]))
            #         row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(ZIntervalRaw[2]))
            #         row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(ZIntervalRaw[3]))
            #         row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(ZIntervalRaw[4]))
            #         row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(ZIntervalRaw[5]))
            #         
            #         values$dfKnownRaw <- rbind(row1, row2, row3, row4, row5)
            #       }
            #       
            #       else if(input$sigmaKnownRaw == 'rawUnknown'){
            #         
            #         rawSampleSD <- sd(datRawData)
            #         
            #         source("R/OneSampTInt.R")
            #         
            #         TIntervalRaw <- TInterval(rawSampleSize, rawSampleMean, rawSampleSD, confLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfUnknownRaw <- data.frame(Variable = character(), Value = character())
            #         output$oneSampCIRawUnknown <- renderTable(values$dfUnknownRaw)
            #         
            #         row1 <- data.frame(Variable = "Sample Mean", Value = paste(TIntervalRaw[1]))
            #         row2 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TIntervalRaw[2]))
            #         row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TIntervalRaw[3]))
            #         row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TIntervalRaw[4]))
            #         row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TIntervalRaw[5]))
            #         
            #         values$dfUnknownRaw <- rbind(row1, row2, row3, row4, row5) 
            #       } # input$sigmaKnownRaw == 'rawUnknown'
            #     } # input$inferenceType == 'Confidence Interval'
            #     
            #     else if(input$inferenceType == 'Hypothesis Testing'){
            #       
            #       hypMeanSampOne <- input$hypMean 
            #       
            #       if(input$sigmaKnownRaw == 'rawKnown'){
            #         
            #         rawPopuSD <- input$popuSDRaw
            #         
            #         source("R/OneSampZTest.R")
            #         
            #         ZTestRaw <- ZTest(rawSampleSize, rawSampleMean, rawPopuSD, hypMeanSampOne, alternative, sigLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfKnownHypRaw <- data.frame(Variable = character(), Value = character())
            #         output$oneSampHTRaw <- renderTable(values$dfKnownHypRaw)
            #         
            #         row1 <- data.frame(Variable = "Sample Size", Value = paste(ZTestRaw[1]))
            #         row2 <- data.frame(Variable = "Sample Mean", Value = paste(ZTestRaw[2]))
            #         row3 <- data.frame(Variable = "Population Standard Deviation", Value = paste(ZTestRaw[3]))
            #         row4 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(ZTestRaw[4]))
            #         row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(ZTestRaw[5]))
            #         row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(ZTestRaw[6]))
            #         row7 <- data.frame(Variable = "P-Value", Value = paste(ZTestRaw[7]))
            #         
            #         values$dfKnownHypRaw <- rbind(row1, row2, row3, row4, row5, row6, row7) 
            #       }
            #       
            #       else if(input$sigmaKnownRaw == 'rawUnknown'){
            #         
            #         rawSampleSD <- sd(datRawData)
            #         
            #         source("R/OneSampTTest.R")
            #         
            #         TTestRaw <- TTest(rawSampleSize, rawSampleMean, rawSampleSD, hypMeanSampOne, alternative, sigLvl)
            #         
            #         values <- reactiveValues()
            #         values$dfUnKnownHypRaw <- data.frame(Variable = character(), Value = character())
            #         output$oneSampHTRawUnknown <- renderTable(values$dfUnKnownHypRaw)
            #         
            #         row1 <- data.frame(Variable = "Sample Size", Value = paste(TTestRaw[1]))
            #         row2 <- data.frame(Variable = "Sample Mean", Value = paste(TTestRaw[2]))
            #         row3 <- data.frame(Variable = "Sample Standard Deviation", Value = paste(TTestRaw[3]))
            #         row4 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TTestRaw[4]))
            #         row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TTestRaw[5]))
            #         row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TTestRaw[6]))
            #         row7 <- data.frame(Variable = "P-Value", Value = paste(TTestRaw[7]))
            #         
            #         values$dfUnKnownHypRaw <- rbind(row1, row2, row3, row4, row5, row6, row7) 
            #       } # input$sigmaKnownRaw == 'rawUnknown'
            #     } # input$inferenceType == 'Hypothesis Testing'
            #   } # input$dataAvailability == 'Enter Raw Data'
            }
            else
            {
              hide(id = "inferenceData")
            }
          }
          else if(input$popuParameter == 'Population Proportion'){
          #   source('R/OnePropZInt.R')
          #   source('R/OnePropZTest.R')
          #   print("Inferences for One Population Proportion are under contruction")
            #output$renderInference <- renderUI({
            
            #hide('oneSampPropMP')
            
            if(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
            {
              output$oneSampProportion <- renderUI({
                
                tagList(
                  
                  fluidRow(
                    column(width = 4,
                           titlePanel("Sample Data Summary"),
                           hr(),
                           withMathJax(DTOutput('oneSampPropData', width = "95%")),
                    ),
                    column(width = 8,
                           titlePanel('Summary Details'),
                           hr(),
                           
                           conditionalPanel(
                             condition = "input.oneSampPropData_rows_selected == 0",
                             
                             p("Select one or more variables from the summary table for more information"),
                           ),
                           
                           conditionalPanel(
                             condition = "input.oneSampPropData_rows_selected != 0",
                             
                             uiOutput('oneSampPropDataNDetails'),
                             uiOutput('oneSampPropDataXDetails'),
                             uiOutput('oneSampPropDataPhatDetails'),
                             uiOutput('oneSampPropDataQhatDetails'),
                             uiOutput('oneSampPropDataConfLvlDetails'),
                             uiOutput('oneSampPropDataSigLvlDetails'),
                             uiOutput('oneSampPropDataCVDetails'),
                             uiOutput('oneSampPropDataSEDetails'),
                             uiOutput('oneSampPropDataMEDetails'),
                             uiOutput('oneSampPropDataLCLDetails'),
                             uiOutput('oneSampPropDataUCLDetails'),
                             uiOutput('oneSampPropDataHypDetails'),
                             uiOutput('oneSampPropDataTSDetails'),
                             uiOutput('oneSampPropDataPValDetails'),
                           )
                    )
                  ),

                  br(),
                  hr(),
                  br(),
                  
                  conditionalPanel(
                    condition = "input.inferenceType == 'Confidence Interval'",
                    
                    titlePanel(tags$u("Confidence Interval")),
                    br(),
                    uiOutput('oneSampPropCI'),
                    br(),
                  ),
                  
                  conditionalPanel(
                    condition = "input.inferenceType == 'Hypothesis Testing'",
                    
                    titlePanel(tags$u("Hypothesis Test")),
                    br(),
                    uiOutput('oneSampPropHT'),
                    br(),
                    plotOutput('oneSampPropHTPlot', width = "75%", height = "300px"),
                    br(),
                    uiOutput('oneSampPropHTIntrp'),
                    br(),
                  ),
                )
                
              })
              
              oneSampPropSucc <- input$numSuccesses
              oneSampPropTrials <- input$numTrials
              

              if(input$inferenceType == 'Confidence Interval') 
              {
                source('R/OnePropZInt.R')
                
                oneSampPropZInt <- OnePropZInterval(oneSampPropSucc, oneSampPropTrials, confLvl)
                oneSampPropME <- oneSampPropZInt["Z Critical"] * oneSampPropZInt["Std Error"]
                
              # (\\( n\\))  (\\( x\\))  (\\(\\hat{p}\\))  ( 1 - \\(\\hat{p}\\))
                dataRow1 <- data.frame(Variable = "Number of Trials \\( (n)\\)", Value = paste(oneSampPropTrials))
                dataRow2 <- data.frame(Variable = "Number of Successes \\( (x)\\)", Value = paste(oneSampPropSucc))
                dataRow3 <- data.frame(Variable = "Sample Proportion of Success \\( (\\hat{p})\\)", Value = paste(round(oneSampPropZInt["phat"], digits = 3)))
                dataRow4 <- data.frame(Variable = "Sample Proportion of Failure \\( (\\hat{q})\\)", Value = paste(round((1 - oneSampPropZInt["phat"]), digits = 3)))
                dataRow5 <- data.frame(Variable = "Confidence Level \\( (1 - \\alpha)\\)", Value = paste(confLvl*100, "%"))
                dataRow6 <- data.frame(Variable = "Z Critical Value \\( (CV)\\)", Value = paste(oneSampPropZInt["Z Critical"]))
                dataRow7 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = paste(round(oneSampPropZInt["Std Error"], digits = 3)))
                dataRow8 <- data.frame(Variable = "Margin of Error \\( (ME)\\)", Value = paste(round(oneSampPropME, digits = 3)))
                dataRow9 <- data.frame(Variable = "Lower Confidence Limit \\( (LCL)\\)", Value = paste(round(oneSampPropZInt["LCL"], digits = 3)))
                dataRow10 <- data.frame(Variable = "Upper Confidence Limit \\( (UCL)\\)", Value = paste(round(oneSampPropZInt["UCL"], digits = 3)))
                
                propIntData <- rbind(dataRow1, dataRow2, dataRow3, dataRow4, dataRow5, dataRow6, dataRow7, dataRow8, dataRow9, dataRow10)
                
                #row4 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TTestRaw[4]))
                #row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TTestRaw[5]))
                #row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TTestRaw[6]))
                #row7 <- data.frame(Variable = "P-Value", Value = paste(TTestRaw[7]))
                
                output$oneSampPropData <- renderDT(
                  datatable(propIntData,
                            options = list(
                              dom = 't',
                              pageLength = -1,
                              ordering = FALSE,
                              searching = FALSE,
                              paging = FALSE
                              ),
                            rownames = FALSE,
                            filter = "none"
                            )
                )
                
                output$oneSampPropDataNDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\( n\\) denotes the number of trials, or sample size."),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataXDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\( x\\) denotes the number of 'successful' trials, or the number of observations in the sample with a desired attribute."),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataPhatDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\( \\hat{p} = \\dfrac{x}{n} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                            oneSampPropSucc,
                            oneSampPropTrials,
                            oneSampPropZInt["phat"]),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataQhatDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(1 - \\hat{p} = 1 - %0.3f = %0.3f\\)",
                            oneSampPropZInt["phat"],
                            (1 - oneSampPropZInt["phat"])),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataConfLvlDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("The confidence level \\( (1 - \\alpha)\\) or \\( C\\) is the probability that the produced interval contains the unknown parameter."),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataCVDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(CV = z_{\\alpha/2} = %0.3f\\)",
                            oneSampPropZInt["Z Critical"]),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataSEDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(SE = \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} = \\sqrt{\\dfrac{%0.3f(1-%0.3f)}{%1.0f}} = %0.3f\\)",
                            oneSampPropZInt["phat"],
                            oneSampPropZInt["phat"],
                            oneSampPropTrials,
                            oneSampPropZInt["Std Error"]),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataMEDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(ME = CV * SE = %0.3f * %0.3f = %0.3f\\)",
                            oneSampPropZInt["Z Critical"],
                            oneSampPropZInt["Std Error"],
                            oneSampPropME),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataLCLDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(LCL = \\hat{p} - ME = %0.3f - %0.3f = %0.3f\\)",
                            oneSampPropZInt["phat"],
                            oneSampPropME,
                            oneSampPropZInt["LCL"]),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropDataUCLDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(UCL = \\hat{p} + ME = %0.3f + %0.3f = %0.3f\\)",
                            oneSampPropZInt["phat"],
                            oneSampPropME,
                            oneSampPropZInt["UCL"]),
                    hr()
                    )
                  )
                })
                
                output$oneSampPropCI <- renderUI({
                  p(
                    withMathJax(
                        sprintf("CI \\(= \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}}\\)"),
                        br(),
                        br(),
                        sprintf("CI \\(= %0.3f \\pm %0.3f \\sqrt{\\dfrac{%0.3f(1-%0.3f)}{%1.0f}}\\)",
                                oneSampPropZInt["phat"],
                                oneSampPropZInt["Z Critical"],
                                oneSampPropZInt["phat"],
                                oneSampPropZInt["phat"],
                                oneSampPropTrials),
                        br(),
                        br(),
                        sprintf("CI \\(= (%0.3f, %0.3f)\\)",
                                oneSampPropZInt["LCL"],
                                oneSampPropZInt["UCL"]),
                        br(),
                        br(),
                        p(tags$b("Interpretation:")),
                        sprintf("We are %1.0f%% confident that the population proportion \\( (p) \\) is between %0.3f and %0.3f.",
                                confLvl*100,
                                oneSampPropZInt["LCL"],
                                oneSampPropZInt["UCL"])
                    )
                  )
                })
                
                
                
              } # input$inferenceType == 'Confidence Interval'
              else if(input$inferenceType == 'Hypothesis Testing') 
              {
                oneSampHypProp <- input$hypProportion
                
                source('R/OnePropZTest.R')
                
                oneSampPropZTest <- OnePropZTest(oneSampPropSucc, oneSampPropTrials, oneSampHypProp, alternative, sigLvl)
                
                if(oneSampPropZTest["P-Value"] < 0.0001)
                {
                  pValue <- "\\( P \\lt\\) 0.0001"
                }
                else
                {
                  pValue <- paste("\\( P =\\)", oneSampPropZTest["P-Value"])
                }
                
                if(alternative == "two.sided")
                {
                  critZVal <- paste("\\( \\pm\\)", oneSampPropZTest["Z Critical"])
                  htPlotCritVals <- c(-oneSampPropZTest["Z Critical"], oneSampPropZTest["Z Critical"]) 
                }
                else
                {
                  critZVal <- paste(oneSampPropZTest["Z Critical"])
                  htPlotCritVals <- oneSampPropZTest["Z Critical"]
                }
                
                dataRow1 <- data.frame(Variable = "Number of Trials \\( (n)\\)", Value = paste(oneSampPropTrials))
                dataRow2 <- data.frame(Variable = "Number of Successes \\( (x)\\)", Value = paste(oneSampPropSucc))
                dataRow3 <- data.frame(Variable = "Sample Proportion of Success \\( (\\hat{p})\\)", Value = paste(round(oneSampPropZTest["Sample Proportion"], digits = 3)))
                dataRow4 <- data.frame(Variable = "Sample Proportion of Failure \\( (\\hat{q})\\)", Value = paste(round((1 - oneSampPropZTest["Sample Proportion"]), digits = 3)))
                dataRow5 <- data.frame(Variable = "Significance Level \\( (\\alpha)\\)", Value = paste(sigLvl*100, "%"))
                dataRow6 <- data.frame(Variable = "Z Critical Value \\( (CV)\\)", Value = paste(critZVal))
                dataRow7 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = paste(oneSampPropZTest["Std Error"]))
                dataRow8 <- data.frame(Variable = "Hypothesized Proportion \\( (p_{0})\\)", Value = paste(oneSampHypProp))
                dataRow9 <- data.frame(Variable = "Test Statistic \\( (Z)\\)", Value = paste(oneSampPropZTest["Test Statistic"]))
                dataRow10 <- data.frame(Variable = "P-Value \\( (P)\\)", Value = paste(pValue))
                
                propTestData <- rbind(dataRow1, dataRow2, dataRow3, dataRow4, dataRow5, dataRow6, dataRow7, dataRow8, dataRow9, dataRow10)
                
                output$oneSampPropData <- renderDT(
                  datatable(propTestData,
                            options = list(
                              dom = 't',
                              pageLength = -1,
                              ordering = FALSE,
                              searching = FALSE,
                              paging = FALSE
                            ),
                            rownames = FALSE,
                            filter = "none")
                )
                

                output$oneSampPropDataNDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\( n\\) denotes the number of trials, or sample size."),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataXDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\( x\\) denotes the number of 'successful' trials, or the number of observations in the sample with a desired attribute."),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataPhatDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\( \\hat{p} = \\dfrac{x}{n} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                            oneSampPropSucc,
                            oneSampPropTrials,
                            oneSampPropZTest["Sample Proportion"]),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataQhatDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(1 - \\hat{p} = 1 - %0.3f = %0.3f\\)",
                            oneSampPropZTest["Sample Proportion"],
                            (1 - oneSampPropZTest["Sample Proportion"])),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataSigLvlDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("The significance level \\(( \\alpha\\)) is the probability that the null hypothesis \\( H_{0}\\) is true; that is, the maximum allowable 
                            probability of \\( H_{0}\\) being true evidenced by the data in order to reject \\( H_{0}\\) in favour of the 
                            alternative hypothesis \\( H_{a}\\)."),
                      hr()
                   )
                  )
                })
                
                output$oneSampPropDataCVDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(CV = %s =\\) %s",
                            critZAlph,
                            critZVal),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataSEDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(SE = \\sqrt{\\dfrac{p_{0}(1 - p_{0})}{n}} = \\sqrt{\\dfrac{%0.3f(1 - %0.3f)}{%1.0f}} = %0.3f\\)",
                            oneSampHypProp,
                            oneSampHypProp,
                            oneSampPropTrials,
                            oneSampPropZTest["Std Error"]),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataHypDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\(p_{0}\\) is the assumed population proportion used in the null hypothesis \\((H_{0})\\)"),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataTSDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("\\( z = \\dfrac{\\hat{p} - p_{0}}{SE_{\\hat{p}}} = \\dfrac{%0.3f - %0.3f}{%0.3f} = %0.3f\\)",
                              oneSampPropZTest["Sample Proportion"],
                              oneSampHypProp,
                              oneSampPropZTest["Std Error"],
                              oneSampPropZTest["Test Statistic"]),
                      hr()
                    )
                  )
                })
                
                output$oneSampPropDataPValDetails <- renderUI({
                  p(
                    withMathJax(
                      sprintf("The smaller the P-Value, the stronger the evidence against \\( H_{0}\\). If \\( P \\leq \\alpha\\),
                              then we can say at level \\( \\alpha\\) the data are statistically significant and we reject \\( H_{0}\\)"),
                      hr()
                    )
                  )
                })
                

                if(oneSampPropZTest["P-Value"] > sigLvl)
                {
                  pvalSymbol <- "\\( \\gt\\)"
                  suffEvidence <- "do not provide"
                  reject <- "do not reject"
                  region <- "acceptance"
                }
                else
                {
                  pvalSymbol <- "\\( \\leq\\)"
                  suffEvidence <- "provide"
                  reject <- "reject"
                  region <- "rejection"
                }
                
                output$oneSampPropHT <- renderUI({
                  p(
                    withMathJax(
                      #h4(tags$u("Performing the Hypothesis Test:")),
                      #br(),
                      sprintf("\\( H_{0}: p %s %0.2f\\)",
                              nullHyp,
                              oneSampHypProp),
                      br(),
                      sprintf("\\( H_{a}: p %s %0.2f\\)",
                              altHyp,
                              oneSampHypProp),
                      br(),
                      br(),
                      sprintf("\\( \\alpha = %g \\)",
                              sigLvl),
                      br(),
                      br(),
                      sprintf("\\(z = \\dfrac{\\hat{p} - p_{0}}{\\sqrt{p_{0}(1 - p_{0})/n}}\\)"),
                      br(),
                      br(),
                      sprintf("\\(z = \\dfrac{%0.3f - %0.3f}{\\sqrt{%0.3f(1 - %0.3f)/%1.0f}}\\)",
                              oneSampPropZTest["Sample Proportion"],
                              oneSampHypProp,
                              oneSampHypProp,
                              oneSampHypProp,
                              oneSampPropTrials),
                      br(),
                      br(),
                      sprintf("\\(z = %0.3f\\)",
                              oneSampPropZTest["Test Statistic"]),
                      br(),
                      br(),
                      br(),
                      p(tags$b("Using P-Value Method:")),
                      p(pValue),
                      sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
                              pvalSymbol,
                              sigLvl,
                              reject),
                      br(),
                      br(),
                      br(),
                      p(tags$b("Using Critical Value Method:")),
                      sprintf("Critical Value(s) = %s",
                              critZVal),
                      br(),
                      br(),
                      sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                              region,
                              reject)
                      
                    )
                  )
                })
                
                output$oneSampPropHTPlot <- renderPlot({
                  
                  htPlot <- hypZTestPlot(oneSampPropZTest["Test Statistic"], htPlotCritVals, alternative)
                  htPlot
                })
                
                output$oneSampPropHTIntrp <- renderUI({
                  p(
                    p(tags$b("Conclusion:")),
                    sprintf("At the %1.0f%% level, the data %s sufficient evidence to reject the null hypothesis \\( (H_{0}) \\) that the population 
                              proportion \\( (p) \\) \\( %s\\) %0.2f.",
                            sigLvl*100,
                            suffEvidence,
                            nullHyp,
                            oneSampHypProp),
                    br(),
                  )
                })
                
  
                
              } # input$inferenceType == 'Hypothesis Test'
              #show('oneSampPropMP')
            }
            else
            {
              output$oneSampProportion <- renderUI({ 
                validate(
                  need(input$numSuccesses, "Numeric value for Number of Successes (x) required"),
                  need(input$numTrials, "Numeric value for Number of Trials (n) required"),
                  
                  errorClass = "myClass"
                )
                
                validate(
                  need(input$numSuccesses %% 1 == 0, "Number of Successes (x) must be an integer"),
                  need(input$numSuccesses >= 0, "Number of Successes (x) cannot be negative"),
                  need(input$numTrials %% 1 == 0, "Number of Trials (n) must be an integer"),
                  need(input$numTrials > 0, "Number of Trials (n) must be greater than 0") %then%
                    need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),
                  
                  errorClass = "myClass"
                )
                
                if(!onepropht_iv$is_valid())
                {
                  validate(
                    need(input$hypProportion, "Hypothesized Population Proportion must be between 0 and 1") %then%
                      need(input$hypProportion >= 0 && input$hypProportion <= 1, "Hypothesized Population Proportion must be between 0 and 1"),
                    
                    errorClass = "myClass"
                  )
                }
              })
            }
          } # input$popuParameter == 'Population Proportion'
        } # one sample
        
        else if(input$samplesSelect == '2'){
          
          if(input$inferenceType2 == 'Confidence Interval'){

            if(input$confidenceLevel2 == '90%'){
              confLvl <- 0.9
            }
            else if(input$confidenceLevel2 == '95%'){
              confLvl <- 0.95
            }
            else{
              confLvl <- 0.99
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
              nullHyp <- "\\leq"
              altHyp <- "\\gt"
              critZAlph <- "z_{\\alpha}"
            }
            else if(input$altHypothesis2 == "2"){
              alternative <- "two.sided"
              nullHyp <- "="
              altHyp <- "\\neq"
              critZAlph <- "\\pm z_{\\alpha/2}"
            }
            else{
              alternative <- "less"
              nullHyp <- "\\geq"
              altHyp <- "\\lt"
              critZAlph <- "-z_{\\alpha}"
            }
          }
          
          if(input$popuParameters == 'Independent Population Means'){
            if(si_iv$is_valid()) {

            #   output$indMeansTable <- renderUI({
            #     tagList(
            #       withMathJax(),
            #       withMathJax(DTOutput('indMeansData', width = "95%"))
            #     )
            #   })
            # 
            # 
            #     df_IndMeansData <- data.frame(Variable = character(), Value = character())
            # 
            #     if (input$dataAvailability2 == 'Summarized Data') {
            #       data <- IndMeansSummData()
            #       sigmaKnown <- input$bothsigmaKnown
            #       sigmaEqual <- input$bothsigmaEqual
            #     } else if(input$dataAvailability2 == 'Enter Raw Data'){
            #       data <- IndMeansRawData()
            #       sigmaKnown <- input$bothsigmaKnownRaw
            #       sigmaEqual <- input$bothsigmaEqualRaw
            #     }
            # 
            #     if (input$inferenceType2 == 'Confidence Interval') {
            # 
            #       if (sigmaKnown == 'bothKnown') {
            # 
            #         source('R/TwoSampZInt.R')
            # 
            #         TwoSampZInt <- TwoSampZInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, ConfLvl())
            # 
            #         row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampZInt["Difference of means"]))
            #         row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(TwoSampZInt["Z Critical"]))
            #         row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampZInt["Std Error"]))
            #         row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TwoSampZInt["LCL"]))
            #         row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TwoSampZInt["UCL"]))
            # 
            #         df_IndMeansData <- rbind(row1, row2, row3, row4, row5)
            # 
            #       } else if (sigmaKnown == 'bothUnknown') {
            # 
            #         source('R/TwoSampTInt.R')
            # 
            #         TwoSampTIntRaw <- TwoSampTInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, sigmaEqual, ConfLvl())
            # 
            #         row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampTIntRaw["Difference of means"]))
            #         row2 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TwoSampTIntRaw["T Critical"]))
            #         row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampTIntRaw["Std Error"]))
            #         row4 <- data.frame(Variable = "Lower Confidence Limit (LCL)", Value = paste(TwoSampTIntRaw["LCL"]))
            #         row5 <- data.frame(Variable = "Upper Confidence Limit (UCL)", Value = paste(TwoSampTIntRaw["UCL"]))
            # 
            #         df_IndMeansData  <- rbind(row1, row2, row3, row4, row5)
            #       }
            # 
            #     } else if (input$inferenceType2 == 'Hypothesis Testing') {
            # 
            #       if (sigmaKnown == 'bothKnown') {
            # 
            #         source('R/TwoSampZTest.R')
            # 
            #         TwoSampZTestRaw <- TwoSampZTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, IndMeansHypInfo()$alternative, SigLvl())
            #         critical <- TwoSampZTestRaw["Z Critical"]
            # 
            #         row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampZTestRaw["Difference of means"]))
            #         row2 <- data.frame(Variable = "Z Critical Value (CV)", Value = paste(TwoSampZTestRaw["Z Critical"]))
            #         row3 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampZTestRaw["Std Error"]))
            #         row4 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TwoSampZTestRaw["Test Statistic"]))
            #         row5 <- data.frame(Variable = "P-Value", Value = paste(TwoSampZTestRaw["P-Value"]))
            # 
            #         df_IndMeansData  <- rbind(row1, row2, row3, row4, row5)
            # 
            #       } else if (sigmaKnown == 'bothUnknown') {
            # 
            #         source('R/TwoSampTTest.R')
            # 
            #         TwoSampTTest <- TwoSampTTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, sigmaEqual, IndMeansHypInfo()$alternative, SigLvl())
            #         critical <- TwoSampTTest["T Critical"]
            # 
            #         row1 <- data.frame(Variable = "Difference of Sample Means", Value = paste(TwoSampTTest["Difference of means"]))
            #         row2 <- data.frame(Variable = "Degrees of freedom (df)", Value = paste(TwoSampTTest["df"]))
            #         row3 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TwoSampTTest["T Critical"]))
            #         row4 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TwoSampTTest["Std Error"]))
            #         row5 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TwoSampTTest["Test Statistic"]))
            #         row6 <- data.frame(Variable = "P-Value", Value = paste(TwoSampTTest["P-Value"]))
            # 
            #         df_IndMeansData  <- rbind(row1, row2, row3, row4, row5, row6)
            #       }
            #     }
            # 
            #     output$indMeansData <- renderDT({
            #       withMathJax()
            #       datatable(df_IndMeansData,
            #                 options = list(
            #                   dom = 't',
            #                   pageLength = -1,
            #                   ordering = FALSE,
            #                   searching = FALSE,
            #                   paging = FALSE
            #                 ),
            #                 rownames = FALSE,
            #                 filter = "none"
            #       )
            # 
            #     })
            # 
            # 
            #     output$indMeansHT <- renderUI({
            #       if(sigmaKnown == 'bothUnknown' && input$inferenceType2 == 'Hypothesis Testing') {
            #       withMathJax()
            # 
            #       if(sigmaKnown == 'bothKnown'){
            #         sdSymbol <- "\\sigma"
            #         testStat <- "z"
            #       }
            #       else if(sigmaKnown == 'bothUnknown'){
            #         sdSymbol <- "s"
            #         testStat <- "t"
            #       }
            # 
            #       intrpInfo <- IndMeansHypInfo()
            # 
            #       if(df_IndMeansData[6,2] < 0.0001)
            #       {
            #         pValue <- "\\lt 0.0001"
            #       }
            #       else
            #       {
            #         pValue <- df_IndMeansData[6,2]
            #       }
            # 
            #       if(df_IndMeansData[6,2] > SigLvl())
            #       {
            #         pvalSymbol <- "\\( \\gt\\)"
            #         suffEvidence <- "do not provide"
            #         reject <- "do not reject"
            #         region <- "acceptance"
            #       }
            #       else
            #       {
            #         pvalSymbol <- "\\( \\leq\\)"
            #         suffEvidence <- "provide"
            #         reject <- "reject"
            #         region <- "rejection"
            #       }
            # 
            #       if(intrpInfo$alternative == "two.sided")
            #       {
            #         if(testStat == 'z') {
            #           critVal <- paste("\\( \\pm\\)", critical)
            #         } else {
            #           critVal <- paste("\\( \\pm\\)", critical)
            #         }
            # 
            #       }
            #       else
            #       {
            #         if(testStat == 'z') {
            #           critVal <- critical
            #         } else {
            #           critVal <- critical
            #         }
            # 
            #       }
            # 
            #       tagList(
            # 
            #         p(
            #           withMathJax(
            #             #h4(tags$u("Performing the Hypothesis Test:")),
            #             #br(),
            #             sprintf("\\( H_{0}: \n \\mu_{1} %s \\mu_{2}\\)",
            #                     intrpInfo$nullHyp),
            #             br(),
            #             sprintf("\\( H_{a}: \\mu_{1} %s \\mu_{2}\\)",
            #                     intrpInfo$altHyp),
            #             br(),
            #             br(),
            #             sprintf("\\( \\alpha = %s \\)",
            #                     SigLvl()),
            #             br(),
            #             br(),
            #             sprintf("\\( %s = \\dfrac{ \\bar{x}_{1} - \\bar{x}_{2} }{ s_{p} \\sqrt{(1/n_{1}) + (1/n_{2})} } \\)",
            #                     testStat),
            #             br(),
            #             sprintf("where"),
            #             br(),
            #             sprintf("\\( s_{p} = \\sqrt{\\dfrac{(n_{1} - 1)s_{1}^2 + (n_{2} - 1)s_{2}^2}{n_{1} + n_{2} - 2}} \\)"),
            #             br(),
            #             br(),
            #             br(),
            #             p(tags$b("Using P-Value Method:")),
            #             sprintf("\\(P = %s\\)",
            #                     pValue),
            #             br(),
            #             sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
            #                     pvalSymbol,
            #                     SigLvl(),
            #                     reject),
            #             br(),
            #             br(),
            #             br(),
            #             p(tags$b("Using Critical Value Method:")),
            #             sprintf("Critical Value(s) = %s",
            #                     critVal),
            #             br(),
            #             sprintf("Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
            #                     testStat,
            #                     region,
            #                     reject),
            #             br(),
            #             br()
            # 
            #           )
            #         ),
            # 
            #         # plotOutput('oneMeanHTPlot', width = "75%", height = "300px"),
            #         # br(),
            # 
            #         withMathJax(
            #           p(tags$b("Conclusion:")),
            #           p(
            #             sprintf("At the %1.0f%% significance level, the data %s sufficient evidence to reject the null hypothesis
            #                     \\( (H_{0}) \\) that \\( \\mu_{1} %s \\mu_{2} \\).",
            #                     SigLvl()*100,
            #                     suffEvidence,
            #                     intrpInfo$nullHyp),
            #             br(),
            #           )
            #         )
            #       )
            #     }
            #     })
            # 
            # 
            # }

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

                  TwoSampZInt <- TwoSampZInt(xbar1, sigma1, n1, xbar2, sigma2, n2, confLvl)

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

                  TwoSampTInt <- TwoSampTInt(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, confLvl)

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

                if(input$bothsigmaKnownRaw == 'bothKnown'){

                  sigma1 <- input$popuSDRaw1
                  sigma2 <- input$popuSDRaw2

                  source('R/TwoSampZInt.R')

                  TwoSampZIntRaw <- TwoSampZInt(xbar1, sigma1, n1, xbar2, sigma2, n2, confLvl)

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

                else if(input$bothsigmaKnownRaw == 'bothUnknown'){

                  s1 <- sd(raw_sample1)
                  s2 <- sd(raw_sample2)

                  source('R/TwoSampTInt.R')

                  TwoSampTIntRaw <- TwoSampTInt(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, confLvl)

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

                if(input$bothsigmaKnownRaw == 'bothKnown'){

                  sigma1 <- input$popuSDRaw1
                  sigma2 <- input$popuSDRaw2

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

                else if(input$bothsigmaKnownRaw == 'bothUnknown'){

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
          }
          else if(input$popuParameters == 'Population Proportions'){ -
            
            if(si_iv$is_valid())
            {
              if(input$numTrials1 >= input$numSuccesses1 && input$numTrials2 >= input$numSuccesses2)
              {
                output$twoSampProportion <- renderUI({
                  
                  tagList(
                    
                    fluidRow(
                      column(width = 4,
                             titlePanel("Sample Data Summary"),
                             hr(),
                             withMathJax(DTOutput('twoSampPropData', width = "100%")),
                      ),
                      column(width = 8,
                            titlePanel('Summary Details'),
                            hr(),
                             
                            conditionalPanel(
                              condition = "input.twoSampPropData_rows_selected == 0",
                               
                              p("Select one or more variables from the summary table for more information"),
                            ),
                             
                            conditionalPanel(
                              condition = "input.twoSampPropData_rows_selected != 0",
                              
                              uiOutput('twoSampPropDetails')
                               
                               #uiOutput('twoSampPropDataNOneDetails'),
                               #uiOutput('twoSampPropDataXOneDetails'),
                               #uiOutput('twoSampPropDataNTwoDetails'),
                               #uiOutput('twoSampPropDataXTwoDetails'),
                               #uiOutput('twoSampPropDataPhatOneDetails'),
                               #uiOutput('twoSampPropDataQhatOneDetails'),
                               #uiOutput('twoSampPropDataPhatTwoDetails'),
                               #uiOutput('twoSampPropDataQhatTwoDetails'),
                               #uiOutput('twoSampPropDataPhatDiffDetails'),
                               #uiOutput('twoSampPropDataConfLvlDetails'),
                               #uiOutput('twoSampPropDataSigLvlDetails'),
                               #uiOutput('twoSampPropDataCVDetails'),
                               #uiOutput('twoSampPropDataSEDetails'),
                               #uiOutput('twoSampPropDataMEDetails'),
                               #uiOutput('twoSampPropDataLCLDetails'),
                               #uiOutput('twoSampPropDataUCLDetails'),
                               #uiOutput('twoSampPropDataHypDetails'),
                               #uiOutput('twoSampPropDataTSDetails'),
                               #uiOutput('twoSampPropDataPValDetails'),
                            )
                      )
                    ),
                    
                    br(),
                    br(),
                    hr(),
                    br(),
                    
                    conditionalPanel(
                      condition = "input.inferenceType2 == 'Confidence Interval'",
                      
                      titlePanel(tags$u("Confidence Interval")),
                      br(),
                      uiOutput('twoSampPropCI'),
                      br(),
                    ),
                    
                    conditionalPanel(
                      condition = "input.inferenceType2 == 'Hypothesis Testing'",
                      
                      titlePanel(tags$u("Hypothesis Test")),
                      br(),
                      uiOutput('twoSampPropHT'),
                      br(),
                      plotOutput('twoSampPropHTPlot', width = "75%", height = "300px"),
                      br(),
                      uiOutput('twoSampPropHTIntrp'),
                      br(),
                    ),
                  )
                  
                })
                
                twoSampPropSucc1 <- input$numSuccesses1
                twoSampPropTrial1 <- input$numTrials1
                twoSampPropSucc2 <- input$numSuccesses2
                twoSampPropTrial2 <- input$numTrials2
                
                if(input$inferenceType2 == 'Confidence Interval')
                {
                  source('R/TwoPropZInt.R')
                  
                  twoSampPropZInt <- TwoPropZInt(twoSampPropSucc1, twoSampPropTrial1, twoSampPropSucc2, twoSampPropTrial2, confLvl)
                  
                  # (\\( n\\))  (\\( x\\))  (\\(\\hat{p}\\))  ( 1 - \\(\\hat{p}\\))
                  dataRow1 <- data.frame(Variable = "Number of Trials 1 \\( (n_{1})\\)", Value = paste(twoSampPropTrial1))
                  dataRow2 <- data.frame(Variable = "Number of Successes 1 \\( (x_{1})\\)", Value = paste(twoSampPropSucc1))
                  dataRow3 <- data.frame(Variable = "Number of Trials 2 \\( (n_{2})\\)", Value = paste(twoSampPropTrial2))
                  dataRow4 <- data.frame(Variable = "Number of Successes 2 \\( (x_{2})\\)", Value = paste(twoSampPropSucc2))
                  dataRow5 <- data.frame(Variable = "Sample Proportion of Success 1 \\( (\\hat{p}_{1})\\)", Value = paste(round(twoSampPropZInt["Sample Proportion 1"], digits = 3)))
                  dataRow6 <- data.frame(Variable = "Sample Proportion of Failure 1 \\( (\\hat{q}_{1})\\)", Value = paste(round((1 - twoSampPropZInt["Sample Proportion 1"]), digits = 3)))
                  dataRow7 <- data.frame(Variable = "Sample Proportion of Success 2 \\( (\\hat{p}_{2})\\)", Value = paste(round(twoSampPropZInt["Sample Proportion 2"], digits = 3)))
                  dataRow8 <- data.frame(Variable = "Sample Proportion of Failure 2 \\( (\\hat{q}_{2})\\)", Value = paste(round((1 - twoSampPropZInt["Sample Proportion 2"]), digits = 3)))
                  dataRow9 <- data.frame(Variable = "Difference of Proportions \\( (\\hat{p}_{1} - \\hat{p}_{2})\\)", Value = paste(round(twoSampPropZInt["Difference of proportions"], digits = 3)))
                  dataRow10 <- data.frame(Variable = "Confidence Level \\( (1 - \\alpha)\\)", Value = paste(confLvl*100, "%"))
                  dataRow11 <- data.frame(Variable = "Z Critical Value \\((CV)\\)", Value = paste(twoSampPropZInt["Z Critical"]))
                  dataRow12 <- data.frame(Variable = "Standard Error \\( (SE)\\)", Value = paste(round(twoSampPropZInt["Std Error"], digits = 3)))
                  dataRow13 <- data.frame(Variable = "Margin of Error \\( (ME)\\)", Value = paste(round(twoSampPropZInt["Margin of Error"], digits = 3)))
                  dataRow14 <- data.frame(Variable = "Lower Confidence Limit \\( (LCL)\\)", Value = paste(round(twoSampPropZInt["LCL"], digits = 3)))
                  dataRow15 <- data.frame(Variable = "Upper Confidence Limit \\( (UCL)\\)", Value = paste(round(twoSampPropZInt["UCL"], digits = 3)))
                  
                  twoPropIntData <- rbind(dataRow1, dataRow2, dataRow3, dataRow4, dataRow5, dataRow6, dataRow7, dataRow8, dataRow9, dataRow10, dataRow11, dataRow12, dataRow13, dataRow14, dataRow15)
                  
                  #row4 <- data.frame(Variable = "T Critical Value (CV)", Value = paste(TTestRaw[4]))
                  #row5 <- data.frame(Variable = "Standard Error (SE)", Value = paste(TTestRaw[5]))
                  #row6 <- data.frame(Variable = "Test Statistic (TS)", Value = paste(TTestRaw[6]))
                  #row7 <- data.frame(Variable = "P-Value", Value = paste(TTestRaw[7]))
                  
                  output$twoSampPropData <- renderDT(
                    datatable(twoPropIntData,
                              options = list(
                                dom = 't',
                                pageLength = -1,
                                ordering = FALSE,
                                searching = FALSE,
                                paging = FALSE
                              ),
                              rownames = FALSE,
                              filter = "none"
                    )
                  )
                  
                  output$twoSampPropDetails <- renderUI({
                    
                    tagList(
                      hidden(
                        withMathJax(),
                        div(id = "twoSampPropDataNOneDetails",
                            sprintf("\\( n_{1}\\) denotes the number of trials, or sample size taken from population 1."),
                            hr()),
                        
                        div(id = "twoSampPropDataXOneDetails",
                            sprintf("\\( x_{1}\\) denotes the number of 'successful' trials, or the number of observations with a desired attribute in \\( n_{1}\\)."),
                            hr()),
                        
                        div(id = "twoSampPropDataNTwoDetails",
                            sprintf("\\( n_{2}\\) denotes the number of trials, or sample size taken from population 2."),
                            hr()),
                        
                        div(id = "twoSampPropDataXTwoDetails",
                            sprintf("\\( x_{2}\\) denotes the number of 'successful' trials, or the number of observations with a desired attribute in \\( n_{2}\\)."),
                            hr()),
                        
                        div(id = "twoSampPropDataPhatOneDetails",
                            sprintf("\\( \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                                    twoSampPropSucc1,
                                    twoSampPropTrial1,
                                    twoSampPropZInt["Sample Proportion 1"]),
                            hr()),
                        
                        div(id = "twoSampPropDataQhatOneDetails",
                            sprintf("\\( \\hat{q}_{1} = 1 - \\hat{p}_{1} = 1 - %0.3f = %0.3f\\)",
                                    twoSampPropZInt["Sample Proportion 1"],
                                    (1 - twoSampPropZInt["Sample Proportion 1"])),
                            hr()),
                        
                        div(id = "twoSampPropDataPhatTwoDetails",
                            sprintf("\\( \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                                    twoSampPropSucc2,
                                    twoSampPropTrial2,
                                    twoSampPropZInt["Sample Proportion 2"]),
                            hr()),
                        
                        div(id = "twoSampPropDataQhatTwoDetails",
                            sprintf("\\( \\hat{q}_{2} = 1 - \\hat{p}_{2} = 1 - %0.3f = %0.3f\\)",
                                    twoSampPropZInt["Sample Proportion 2"],
                                    (1 - twoSampPropZInt["Sample Proportion 2"])),
                            hr()),
                        
                        div(id = "twoSampPropDataPhatDiffDetails",
                            sprintf("\\( (\\hat{p}_{1} - \\hat{p}_{2}) = %0.3f - %0.3f = %0.3f\\)",
                                    twoSampPropZInt["Sample Proportion 1"],
                                    twoSampPropZInt["Sample Proportion 2"],
                                    twoSampPropZInt["Difference of proportions"]),
                            hr()),
                        
                        div(id = "twoSampPropDataConfLvlDetails",
                            sprintf("The confidence level \\(( 1 - \\alpha)\\) or \\( C\\) is the probability that the produced interval contains the unknown parameter."),
                            hr()),
                        
                        div(id = "twoSampPropDataCVDetails",
                            sprintf("\\(CV = z_{\\alpha/2} = %0.3f\\)",
                                    twoSampPropZInt["Z Critical"]),
                            hr()),
                        
                        div(id = "twoSampPropDataSEDetails",
                            sprintf("\\(SE = \\sqrt{\\dfrac{\\hat{p}_{1}(1-\\hat{p}_{1})}{n_{1}} + \\dfrac{\\hat{p}_{2}(1-\\hat{p}_{2})}{n_{2}}} = 
                                  \\sqrt{\\dfrac{%0.3f(1-%0.3f)}{%1.0f} + \\dfrac{%0.3f(1-%0.3f)}{%1.0f}} = %0.3f\\)",
                                    twoSampPropZInt["Sample Proportion 1"],
                                    twoSampPropZInt["Sample Proportion 1"],
                                    twoSampPropTrial1,
                                    twoSampPropZInt["Sample Proportion 2"],
                                    twoSampPropZInt["Sample Proportion 2"],
                                    twoSampPropTrial2,
                                    twoSampPropZInt["Std Error"]),
                            hr()),
                        
                        div(id = "twoSampPropDataMEDetails",
                            sprintf("\\(ME = CV * SE = %0.3f * %0.3f = %0.3f\\)",
                                    twoSampPropZInt["Z Critical"],
                                    twoSampPropZInt["Std Error"],
                                    twoSampPropZInt["Margin of Error"]),
                            hr()),
                        
                        div(id = "twoSampPropDataLCLDetails",
                            sprintf("\\(LCL = (\\hat{p}_{1} - \\hat{p}_{2}) - ME = %0.3f - %0.3f = %0.3f\\)",
                                    twoSampPropZInt["Difference of proportions"],
                                    twoSampPropZInt["Margin of Error"],
                                    twoSampPropZInt["LCL"]),
                            hr()),
                        
                        div(id = "twoSampPropDataUCLDetails",
                            sprintf("\\(UCL = (\\hat{p}_{1} - \\hat{p}_{2}) + ME = %0.3f + %0.3f = %0.3f\\)",
                                    twoSampPropZInt["Difference of proportions"],
                                    twoSampPropZInt["Margin of Error"],
                                    twoSampPropZInt["UCL"]),
                            hr())
                      )
                    )
                   
                  })
                  
                  #output$twoSampPropDataNOneDetails <- renderUI({
                  #  p(sprintf("\\( n_{1}\\) denotes the number of trials, or sample size taken from population 1."),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataXOneDetails <- renderUI({
                  #  p(sprintf("\\( x_{1}\\) denotes the number of 'successful' trials, or the number of observations with a desired attribute in \\( n_{1}\\)."),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataNTwoDetails <- renderUI({
                  #  p(sprintf("\\( n_{2}\\) denotes the number of trials, or sample size taken from population 2."),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataXTwoDetails <- renderUI({
                  #  p(sprintf("\\( x_{2}\\) denotes the number of 'successful' trials, or the number of observations with a desired attribute in \\( n_{2}\\)."),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataPhatOneDetails <- renderUI({
                  #  p(sprintf("\\( \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                  #            twoSampPropSucc1,
                  #            twoSampPropTrial1,
                  #            twoSampPropZInt["Sample Proportion 1"]),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataQhatOneDetails <- renderUI({
                  #  p(sprintf("\\( \\hat{q}_{1} = 1 - \\hat{p}_{1} = 1 - %0.3f = %0.3f\\)",
                  #            twoSampPropZInt["Sample Proportion 1"],
                  #            (1 - twoSampPropZInt["Sample Proportion 1"])),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataPhatTwoDetails <- renderUI({
                  #  p(sprintf("\\( \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                  #            twoSampPropSucc2,
                  #            twoSampPropTrial2,
                  #            twoSampPropZInt["Sample Proportion 2"]),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataQhatTwoDetails <- renderUI({
                  #  p(sprintf("\\( \\hat{q}_{2} = 1 - \\hat{p}_{2} = 1 - %0.3f = %0.3f\\)",
                  #            twoSampPropZInt["Sample Proportion 2"],
                  #            (1 - twoSampPropZInt["Sample Proportion 2"])),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataPhatDiffDetails <- renderUI({
                  #  p(sprintf("\\( (\\hat{p}_{1} - \\hat{p}_{2}) = %0.3f - %0.3f = %0.3f\\)",
                  #            twoSampPropZInt["Sample Proportion 1"],
                  #            twoSampPropZInt["Sample Proportion 2"],
                  #            twoSampPropZInt["Difference of proportions"]),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataConfLvlDetails <- renderUI({
                  #  p(sprintf("The confidence level \\(( 1 - \\alpha\\)) or \\( C\\) is the probability that the produced interval contains the unknown parameter."),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataCVDetails <- renderUI({
                  #  p(sprintf("\\(CV = z_{\\alpha/2} = %0.3f\\)",
                  #            twoSampPropZInt["Z Critical"]),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataSEDetails <- renderUI({
                  #  p(sprintf("\\(SE = \\sqrt{\\dfrac{\\hat{p}_{1}(1-\\hat{p}_{1})}{n_{1}} + \\dfrac{\\hat{p}_{2}(1-\\hat{p}_{2})}{n_{2}}} = 
                  #            \\sqrt{\\dfrac{%0.3f(1-%0.3f)}{%1.0f} + \\dfrac{%0.3f(1-%0.3f)}{%1.0f}} = %0.3f\\)",
                  #            twoSampPropZInt["Sample Proportion 1"],
                  #            twoSampPropZInt["Sample Proportion 1"],
                  #            twoSampPropTrial1,
                  #            twoSampPropZInt["Sample Proportion 2"],
                  #            twoSampPropZInt["Sample Proportion 2"],
                  #            twoSampPropTrial2,
                  #            twoSampPropZInt["Std Error"]),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataMEDetails <- renderUI({
                  #  p(sprintf("\\(ME = CV * SE = %0.3f * %0.3f = %0.3f\\)",
                  #            twoSampPropZInt["Z Critical"],
                  #            twoSampPropZInt["Std Error"],
                  #            twoSampPropZInt["Margin of Error"]),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataLCLDetails <- renderUI({
                  #  p(sprintf("\\(LCL = (\\hat{p}_{1} - \\hat{p}_{2}) - ME = %0.3f - %0.3f = %0.3f\\)",
                  #            twoSampPropZInt["Difference of proportions"],
                  #            twoSampPropZInt["Margin of Error"],
                  #            twoSampPropZInt["LCL"]),
                  #    hr())
                  #})
                  
                  #output$twoSampPropDataUCLDetails <- renderUI({
                  #  p(sprintf("\\(UCL = (\\hat{p}_{1} - \\hat{p}_{2}) + ME = %0.3f + %0.3f = %0.3f\\)",
                  #            twoSampPropZInt["Difference of proportions"],
                  #            twoSampPropZInt["Margin of Error"],
                  #            twoSampPropZInt["UCL"]),
                  #    hr())
                  #})
                  
                  output$twoSampPropCI <- renderUI({
                    p(
                      withMathJax(
                        sprintf("\\( CI = (\\hat{p}_{1} - \\hat{p}_{2}) \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_{1}(1-\\hat{p}_{1})}{n_{1}} + \\dfrac{\\hat{p}_{2}(1-\\hat{p}_{2})}{n_{2}}}\\)"),
                        br(),
                        br(),
                        sprintf("\\( CI = %0.3f \\pm %0.3f \\sqrt{\\dfrac{%0.3f(1-%0.3f)}{%1.0f} + \\dfrac{%0.3f(1-%0.3f)}{%1.0f}}\\)",
                                twoSampPropZInt["Difference of proportions"],
                                twoSampPropZInt["Z Critical"],
                                twoSampPropZInt["Sample Proportion 1"],
                                twoSampPropZInt["Sample Proportion 1"],
                                twoSampPropTrial1,
                                twoSampPropZInt["Sample Proportion 2"],
                                twoSampPropZInt["Sample Proportion 2"],
                                twoSampPropTrial2),
                        br(),
                        br(),
                        sprintf("\\( CI = (%0.3f, %0.3f)\\)",
                                twoSampPropZInt["LCL"],
                                twoSampPropZInt["UCL"]),
                        br(),
                        br(),
                        br(),
                        p(tags$b("Interpretation:")),
                        sprintf("We are %1.0f%% confident that the difference in population proportions \\( (p_{1} - p_{2}) \\) is between %0.3f and %0.3f.",
                                confLvl*100,
                                twoSampPropZInt["LCL"],
                                twoSampPropZInt["UCL"])
                      )
                    )
                  })
                  
                  
                  
                } # input$inferenceType2 == 'Confidence Interval'
                else if(input$inferenceType2 == 'Hypothesis Testing')
                {
                  source('R/TwoPropZTest.R')
                  
                  twoSampPropZTest <- TwoPropZTest(twoSampPropSucc1, twoSampPropTrial1, twoSampPropSucc2, twoSampPropTrial2, 0, alternative, sigLvl)
                  
                  if(twoSampPropZTest["P-Value"] < 0.0001)
                  {
                    pValue <- "\\( P \\lt\\) 0.0001"
                  }
                  else
                  {
                    pValue <- paste("\\( P =\\)", twoSampPropZTest["P-Value"])
                  }
                  
                  if(alternative == "two.sided")
                  {
                    critZVal <- paste("\\( \\pm\\)", twoSampPropZTest["Z Critical"])
                    htPlotCritVals <- c(-twoSampPropZTest["Z Critical"], twoSampPropZTest["Z Critical"])
                  }
                  else
                  {
                    critZVal <- paste(twoSampPropZTest["Z Critical"])
                    htPlotCritVals <- twoSampPropZTest["Z Critical"]
                  }
                  
                  propDiff <- twoSampPropZTest["Sample Proportion 1"] - twoSampPropZTest["Sample Proportion 2"]
                  
                  dataRow1 <- data.frame(Variable = "Number of Trials 1 \\( (n_{1})\\)", Value = paste(twoSampPropTrial1))
                  dataRow2 <- data.frame(Variable = "Number of Successes 1 \\( (x_{1})\\)", Value = paste(twoSampPropSucc1))
                  dataRow3 <- data.frame(Variable = "Number of Trials 2 \\( (n_{2})\\)", Value = paste(twoSampPropTrial2))
                  dataRow4 <- data.frame(Variable = "Number of Successes 2 \\( (x_{2})\\)", Value = paste(twoSampPropSucc2))
                  dataRow5 <- data.frame(Variable = "Sample Proportion of Success 1 \\( (\\hat{p}_{1})\\)", Value = paste(twoSampPropZTest["Sample Proportion 1"]))
                  dataRow6 <- data.frame(Variable = "Sample Proportion of Failure 1 \\( (\\hat{q}_{1})\\)", Value = paste(1 - twoSampPropZTest["Sample Proportion 1"]))
                  dataRow7 <- data.frame(Variable = "Sample Proportion of Success 2 \\( (\\hat{p}_{2})\\)", Value = paste(twoSampPropZTest["Sample Proportion 2"]))
                  dataRow8 <- data.frame(Variable = "Sample Proportion of Failure 2 \\( (\\hat{q}_{2})\\)", Value = paste(1 - twoSampPropZTest["Sample Proportion 2"]))
                  dataRow9 <- data.frame(Variable = "Difference of Proportions \\( (\\hat{p}_{1} - \\hat{p}_{2})\\)", Value = paste(propDiff))
                  dataRow10 <- data.frame(Variable = "Significance Level \\( (\\alpha)\\)", Value = paste(sigLvl*100, "%"))
                  dataRow11 <- data.frame(Variable = "Z Critical Value \\((CV)\\)", Value = paste(twoSampPropZTest["Z Critical"]))
                  dataRow12 <- data.frame(Variable = "Standard Error \\( (SE_{D_{P}})\\)", Value = paste(twoSampPropZTest["Std Error"]))
                  dataRow13 <- data.frame(Variable = "Pooled Proportion \\( (\\hat{p})\\)", Value = paste(twoSampPropZTest["Pooled Proportion"]))
                  dataRow14 <- data.frame(Variable = "Test Statistic \\( (z)\\)", Value = paste(twoSampPropZTest["Test Statistic"]))
                  dataRow15 <- data.frame(Variable = "P-Value \\( (P)\\)", Value = paste(pValue))
                  
                  twoPropTestData <- rbind(dataRow1, dataRow2, dataRow3, dataRow4, dataRow5, dataRow6, dataRow7, dataRow8, dataRow9, dataRow10, dataRow11, dataRow12, dataRow13, dataRow14, dataRow15)
                  
                  
                  output$twoSampPropData <- renderDT(
                    datatable(twoPropTestData,
                              options = list(
                                dom = 't',
                                pageLength = -1,
                                ordering = FALSE,
                                searching = FALSE,
                                paging = FALSE
                              ),
                              rownames = FALSE,
                              filter = "none")
                  )
                  

                  output$twoSampPropDetails <- renderUI({
                    
                    tagList(
                      hidden(
                        withMathJax(),
                        div(id = "twoSampPropDataNOneDetails",
                            sprintf("\\( n_{1}\\) denotes the number of trials, or sample size taken from population 1."),
                            hr()),
                        
                        div(id = "twoSampPropDataXOneDetails",
                            sprintf("\\( x_{1}\\) denotes the number of 'successful' trials, or the number of observations with a desired attribute in \\( n_{1}\\)."),
                            hr()),
                        
                        div(id = "twoSampPropDataNTwoDetails",
                            sprintf("\\( n_{2}\\) denotes the number of trials, or sample size taken from population 2."),
                            hr()),
                        
                        div(id = "twoSampPropDataXTwoDetails",
                            sprintf("\\( x_{2}\\) denotes the number of 'successful' trials, or the number of observations with a desired attribute in \\( n_{2}\\)."),
                            hr()),
                        
                        div(id = "twoSampPropDataPhatOneDetails",
                            sprintf("\\( \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                                    twoSampPropSucc1,
                                    twoSampPropTrial1,
                                    twoSampPropZTest["Sample Proportion 1"]),
                            hr()),
                        
                        div(id = "twoSampPropDataQhatOneDetails",
                            sprintf("\\( \\hat{q}_{1} = 1 - \\hat{p}_{1} = 1 - %0.3f = %0.3f\\)",
                                    twoSampPropZTest["Sample Proportion 1"],
                                    (1 - twoSampPropZTest["Sample Proportion 1"])),
                            hr()),
                        
                        div(id = "twoSampPropDataPhatTwoDetails",
                            sprintf("\\( \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%1.0f}{%1.0f} = %0.3f\\)",
                                    twoSampPropSucc2,
                                    twoSampPropTrial2,
                                    twoSampPropZTest["Sample Proportion 2"]),
                            hr()),
                        
                        div(id = "twoSampPropDataQhatTwoDetails",
                            sprintf("\\( \\hat{q}_{2} = 1 - \\hat{p}_{2} = 1 - %0.3f = %0.3f\\)",
                                    twoSampPropZTest["Sample Proportion 2"],
                                    (1 - twoSampPropZTest["Sample Proportion 2"])),
                            hr()),
                        
                        div(id = "twoSampPropDataPhatDiffDetails",
                            sprintf("\\( (\\hat{p}_{1} - \\hat{p}_{2}) = %0.3f - %0.3f = %0.3f\\)",
                                    twoSampPropZTest["Sample Proportion 1"],
                                    twoSampPropZTest["Sample Proportion 2"],
                                    propDiff),
                            hr()),
                        
                        div(id = "twoSampPropDataSigLvlDetails",
                            sprintf("The significance level \\(( 1 - \\alpha\\)) or \\( C\\) is the probability that the produced interval contains the unknown parameter."),
                            hr()),
                        
                        div(id = "twoSampPropDataCVDetails",
                            sprintf("\\(CV = %s =\\) %s",
                                    critZAlph,
                                    critZVal),
                            hr()),
                        
                        
                        div(id = "twoSampPropDataSEDetails",
                            sprintf("\\( SE_{D_{P}} = \\sqrt{\\hat{p}(1-\\hat{p}) (\\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}}) } = 
                                  \\sqrt{%0.3f(1-%0.3f) (\\dfrac{1}{%1.0f} + \\dfrac{1}{%1.0f}) } = %0.3f\\)",
                                    twoSampPropZTest["Pooled Proportion"],
                                    twoSampPropZTest["Pooled Proportion"],
                                    twoSampPropTrial1,
                                    twoSampPropTrial2,
                                    twoSampPropZTest["Std Error"]),
                            hr()),
                        
                        div(id = "twoSampPropDataPooledDetails",
                            sprintf("\\(\\hat{p} = \\dfrac{x_{1} + x_{2}}{n_{1} + n_{2}} = \\dfrac{%1.0f + %1.0f}{%1.0f + %1.0f} = %0.3f\\)",
                                    twoSampPropSucc1,
                                    twoSampPropSucc2,
                                    twoSampPropTrial1,
                                    twoSampPropTrial2,
                                    twoSampPropZTest["Pooled Proportion"]),
                            hr()),
                        
                        div(id = "twoSampPropDataTSDetails",
                            sprintf("\\( z = \\dfrac{\\hat{p}_{1} - \\hat{p}_{2}}{ SE_{D_{P}} } = \\dfrac{%0.3f - %0.3f}{%0.3f} = %0.3f\\)",
                                    twoSampPropZTest["Sample Proportion 1"],
                                    twoSampPropZTest["Sample Proportion 2"],
                                    twoSampPropZTest["Std Error"],
                                    twoSampPropZTest["Test Statistic"]),
                            hr()),
                        
                        div(id = "twoSampPropDataPValDetails",
                            sprintf("The smaller the P-Value, the stronger the evidence against \\( H_{0}\\). If \\( P \\leq \\alpha\\),
                              then we can say at level \\( \\alpha\\) the data are statistically significant and we reject \\( H_{0}\\)"),
                            hr())
                      )
                    )
                    
                  })
                  

                  if(twoSampPropZTest["P-Value"] > sigLvl)
                  {
                    pvalSymbol <- "\\( \\gt\\)"
                    suffEvidence <- "do not provide"
                    reject <- "do not reject"
                    region <- "acceptance"
                  }
                  else
                  {
                    pvalSymbol <- "\\( \\leq\\)"
                    suffEvidence <- "provide"
                    reject <- "reject"
                    region <- "rejection"
                  }
                  
                  output$twoSampPropHT <- renderUI({
                    p(
                      withMathJax(
                        #h4(tags$u("Performing the Hypothesis Test:")),
                        #br(),
                        sprintf("\\( H_{0}: p_{1} %s p_{2}\\)",
                                nullHyp),
                        br(),
                        sprintf("\\( H_{a}: p_{1} %s p_{2}\\)",
                                altHyp),
                        br(),
                        br(),
                        sprintf("\\( \\alpha = %g \\)",
                                sigLvl),
                        br(),
                        br(),
                        sprintf("\\(z = \\dfrac{\\hat{p}_{1} - \\hat{p}_{2}}{\\sqrt{\\hat{p}(1-\\hat{p})(\\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}})}}\\)"),
                        br(),
                        br(),
                        sprintf("\\(z = \\dfrac{%0.3f - %0.3f}{\\sqrt{%0.3f(1-%0.3f)(\\dfrac{1}{%1.0f} + \\dfrac{1}{%1.0f})}}\\)",
                                twoSampPropZTest["Sample Proportion 1"],
                                twoSampPropZTest["Sample Proportion 2"],
                                twoSampPropZTest["Pooled Proportion"],
                                twoSampPropZTest["Pooled Proportion"],
                                twoSampPropTrial1,
                                twoSampPropTrial2),
                        br(),
                        br(),
                        sprintf("\\(z = %0.3f\\)",
                                twoSampPropZTest["Test Statistic"]),
                        br(),
                        br(),
                        br(),
                        p(tags$b("Using P-Value Method:")),
                        p(pValue),
                        sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
                                pvalSymbol,
                                sigLvl,
                                reject),
                        br(),
                        br(),
                        br(),
                        p(tags$b("Using Critical Value Method:")),
                        sprintf("Critical Value(s) = %s",
                                critZVal),
                        
                        br(),
                        sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                                region,
                                reject)
                        
                      )
                    )
                  })
                  
                  output$twoSampPropHTPlot <- renderPlot({
                    
                    htPlot <- hypZTestPlot(twoSampPropZTest["Test Statistic"], htPlotCritVals, alternative)
                    htPlot
                  })
                  
                  output$twoSampPropHTIntrp <- renderUI({
                    p(
                      p(tags$b("Conclusion:")),
                      sprintf("At the %1.0f%% significance level, the data %s sufficient evidence to reject the null hypothesis \\( (H_{0}) \\) that the population 
                              proportion \\( p_{1} %s p_{2}\\).",
                              sigLvl*100,
                              suffEvidence,
                              nullHyp),
                      br(),
                    )
                  })
                  
                  
                  
                } # input$inferenceType2 == 'Hypothesis Test'
              }
              else
              {
                output$twoSampProportion <- renderUI({
                  validate(
                    need(input$numSuccesses1 <= input$numTrials1, "Number of Successes 1 (x1) cannot be greater than Number of Trials 1 (n1)"),
                    need(input$numSuccesses2 <= input$numTrials2, "Number of Successes 2 (x2) cannot be greater than Number of Trials 2 (n2)"),
                    
                    errorClass = "myClass"
                  )
                })
              }
            }
            else
            {
              output$twoSampProportion <- renderUI({ 
                validate(
                  need(input$numSuccesses1, "Numeric value for Number of Successes 1 (x1) required"),
                  need(input$numTrials1, "Numeric value for Number of Trials 1 (n1) required"),
                  need(input$numSuccesses2, "Numeric value for Number of Successes 2 (x2) required"),
                  need(input$numTrials2, "Numeric value for Number of Trials 2 (n2) required"),
                  
                  errorClass = "myClass"
                )
                
                validate(
                  need(input$numSuccesses1 %% 1 == 0, "Number of Successes 1 (x1) must be an integer"),
                  need(input$numSuccesses1 >= 0, "Number of Successes 1 (x1) cannot be negative"),
                  need(input$numTrials1 %% 1 == 0, "Number of Trials 1 (n1) must be an integer"),
                  need(input$numTrials1 > 0, "Number of Trials 1 (n1) must be greater than 0"),
                  need(input$numSuccesses2 %% 1 == 0, "Number of Successes 1 (x2) must be an integer"),
                  need(input$numSuccesses2 >= 0, "Number of Successes 1 (x2) cannot be negative"),
                  need(input$numTrials2 %% 1 == 0, "Number of Trials 2 (n2) must be an integer"),
                  need(input$numTrials2 > 0, "Number of Trials 2 (n2) must be greater than 0"),
                  
                  errorClass = "myClass"
                )
              })
            }
          #   # source('R/TwoPropZInt.R')
          #   # source('R/TwoPropZTest.R')
          #   print("Inference for the difference between two Population Proportions")
          }
        }
       #) # renderInference
    }) # input$goInference
    
    #### one prop details toggle ----
    observeEvent(input$oneSampPropData_rows_selected, {
      
      s = input$oneSampPropData_rows_selected
      
      if(1 %in% input$oneSampPropData_rows_selected)
      {
        show(id = "oneSampPropDataNDetails")
      }
      else
      {
        hide(id = "oneSampPropDataNDetails")
      }
      
      if(2 %in% input$oneSampPropData_rows_selected)
      {
        show(id = "oneSampPropDataXDetails")
      }
      else
      {
        hide(id = "oneSampPropDataXDetails")
      }
      
      if(3 %in% input$oneSampPropData_rows_selected)
      {
        show(id = "oneSampPropDataPhatDetails")
      }
      else
      {
        hide(id = "oneSampPropDataPhatDetails")
      }
      
      if(4 %in% input$oneSampPropData_rows_selected)
      {
        show(id = "oneSampPropDataQhatDetails")
      }
      else
      {
        hide(id = "oneSampPropDataQhatDetails")
      }
      
      if(6 %in% input$oneSampPropData_rows_selected)
      {
        show(id = "oneSampPropDataCVDetails")
      }
      else
      {
        hide(id = "oneSampPropDataCVDetails")
      }
      
      if(7 %in% input$oneSampPropData_rows_selected)
      {
        show(id = "oneSampPropDataSEDetails")
      }
      else
      {
        hide(id = "oneSampPropDataSEDetails")
      }
      
      if(input$inferenceType == 'Confidence Interval')
      {
          hide(id = "oneSampPropDataSigLvlDetails")
          hide(id = "oneSampPropDataHypDetails")
          hide(id = "oneSampPropDataTSDetails")
          hide(id = "oneSampPropDataPValDetails")
        
          if(5 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataConfLvlDetails")
          }
          else
          {
            hide(id = "oneSampPropDataConfLvlDetails")
          }
          
          if(8 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataMEDetails")
          }
          else
          {
            hide(id = "oneSampPropDataMEDetails")
          }
          
          if(9 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataLCLDetails")
          }
          else
          {
            hide(id = "oneSampPropDataLCLDetails")
          }
          
          if(10 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataUCLDetails")
          }
          else
          {
            hide(id = "oneSampPropDataUCLDetails")
          }
      }
      else if(input$inferenceType == 'Hypothesis Testing')
      {
          hide(id = "oneSampPropDataConfLvlDetails")
          hide(id = "oneSampPropDataMEDetails")
          hide(id = "oneSampPropDataLCLDetails")
          hide(id = "oneSampPropDataUCLDetails")
          
          if(5 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataSigLvlDetails")
          }
          else
          {
            hide(id = "oneSampPropDataSigLvlDetails")
          }
          
          if(8 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataHypDetails")
          }
          else
          {
            hide(id = "oneSampPropDataHypDetails")
          }
          
          if(9 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataTSDetails")
          }
          else
          {
            hide(id = "oneSampPropDataTSDetails")
          }
          
          if(10 %in% input$oneSampPropData_rows_selected)
          {
            show(id = "oneSampPropDataPValDetails")
          }
          else
          {
            hide(id = "oneSampPropDataPValDetails")
          }
      }
    })
    
    #### two prop details toggle ----
    observeEvent(input$twoSampPropData_rows_selected, { 
      
      if(1 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataNOneDetails")
      }
      else
      {
        hide(id = "twoSampPropDataNOneDetails")
      }
      
      if(2 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataXOneDetails")
      }
      else
      {
        hide(id = "twoSampPropDataXOneDetails")
      }
      
      if(3 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataNTwoDetails")
      }
      else
      {
        hide(id = "twoSampPropDataNTwoDetails")
      }
      
      if(4 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataXTwoDetails")
      }
      else
      {
        hide(id = "twoSampPropDataXTwoDetails")
      }
      
      if(5 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataPhatOneDetails")
      }
      else
      {
        hide(id = "twoSampPropDataPhatOneDetails")
      }
      
      if(6 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataQhatOneDetails")
      }
      else
      {
        hide(id = "twoSampPropDataQhatOneDetails")
      }
      
      if(7 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataPhatTwoDetails")
      }
      else
      {
        hide(id = "twoSampPropDataPhatTwoDetails")
      }
      
      if(8 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataQhatTwoDetails")
      }
      else
      {
        hide(id = "twoSampPropDataQhatTwoDetails")
      }
      
      if(9 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataPhatDiffDetails")
      }
      else
      {
        hide(id = "twoSampPropDataPhatDiffDetails")
      }
      
      if(11 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataCVDetails")
      }
      else
      {
        hide(id = "twoSampPropDataCVDetails")
      }
      
      if(12 %in% input$twoSampPropData_rows_selected)
      {
        show(id = "twoSampPropDataSEDetails")
      }
      else
      {
        hide(id = "twoSampPropDataSEDetails")
      }
      
      if(input$inferenceType2 == 'Confidence Interval')
      {
        hide(id = "twoSampPropDataSigLvlDetails")
        hide(id = "twoSampPropDataHypDetails")
        hide(id = "twoSampPropDataTSDetails")
        hide(id = "twoSampPropDataPValDetails")
        
        if(10 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataConfLvlDetails")
        }
        else
        {
          hide(id = "twoSampPropDataConfLvlDetails")
        }
        
        if(13 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataMEDetails")
        }
        else
        {
          hide(id = "twoSampPropDataMEDetails")
        }
        
        if(14 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataLCLDetails")
        }
        else
        {
          hide(id = "twoSampPropDataLCLDetails")
        }
        
        if(15 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataUCLDetails")
        }
        else
        {
          hide(id = "twoSampPropDataUCLDetails")
        }
      }
      else if(input$inferenceType2 == 'Hypothesis Testing')
      {
        hide(id = "oneSampPropDataConfLvlDetails")
        hide(id = "oneSampPropDataMEDetails")
        hide(id = "oneSampPropDataLCLDetails")
        hide(id = "oneSampPropDataUCLDetails")
        
        if(10 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataSigLvlDetails")
        }
        else
        {
          hide(id = "twoSampPropDataSigLvlDetails")
        }
        
        if(13 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataPooledDetails")
        }
        else
        {
          hide(id = "twoSampPropDataPooledDetails")
        }
        
        if(14 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataTSDetails")
        }
        else
        {
          hide(id = "twoSampPropDataTSDetails")
        }
        
        if(15 %in% input$twoSampPropData_rows_selected)
        {
          show(id = "twoSampPropDataPValDetails")
        }
        else
        {
          hide(id = "twoSampPropDataPValDetails")
        }
      }
    })
    #observeEvent(input$oneSampPropData_rows_selected, {
      
      #rows <- input$oneSampPropData_rows_selected
      
      #output$oneSampPropDataDetails <- renderUI({
        
        #tagList(
          #titlePanel('Details'),
          #br(),
          
          
        #)
      #})
    #})
    
    # --------------------------------------------------------------------- #

        
# **************************************************************************** #
    
    
    #  -------------------------------------------------------------------- #
    ## ----------- Linear Regression and Correlation functions ------------
    #  -------------------------------------------------------------------- #

    
    ### Reactives ----
    # --------------------------------------------------------------------- #
    
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

    # --------------------------------------------------------------------- #
    
    
    ### Observers ----
    # --------------------------------------------------------------------- #
    
    observeEvent(input$slrUserData, {
      hide(id = "RegCorMP")
      hide(id = "slrResponse")
      hide(id = "slrExplanatory")
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
        #output$slrUploadVars <- renderUI({
          
          #tagList(
            
            #selectizeInput(
              #inputId = "slrExplanatory",
              #label = strong("Choose the Explanatory Variable (x)"),
              #choices = c(colnames(slrUploadData())),
              #options = list(
                #placeholder = 'Select a variable',
                #onInitialize = I('function() { this.setValue(""); }')
              #)
            #),
            
            #selectizeInput(
              #inputId = "slrResponse",
              #label = strong("Choose the Response Variable (y)"),
              #choices = c(colnames(slrUploadData())),
              #options = list(
                #placeholder = 'Select a variable',
                #onInitialize = I('function() { this.setValue(""); }')
              #)
            #),
          #)
        #})
        
        #slruploadvars_iv$add_rule("slrResponse", sv_required())
        #slruploadvars_iv$add_rule("slrExplanatory", sv_required())
        #slruploadvars_iv$add_rule("slrResponse", ~ if(sampleDiffUpload() != 0) "Missing values detected, x and y must have the same number of observations")
      
        #slruploadvars_iv$condition(function() {isTRUE(input$dataRegCor == 'Upload Data' && slrupload_iv$is_valid()) })
        
        #regcor_iv$add_validator(slruploadvars_iv)
        
        #slruploadvars_iv$enable()
      #}
      #else
      #{
        #output$slrUploadVars <- renderUI({
          #""
        #})
      #}
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
              need(input$slrResponse != "", "Please select a response variable (y)") %then%
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
            output$slrTabs <- renderUI({ ####tab generation ----
              
              tagList(
                
                tabsetPanel(id = "slrTabset", selected = "Simple Linear Regression",
                            
                            tabPanel(id = "slr", title = "Simple Linear Regression",
                                     
                                     conditionalPanel(
                                       condition = "input.scatterPlot == 1",
                                       
                                       titlePanel("Scatterplot"),
                                       plotOutput("scatterplot", width = "500px"),
                                       br(),
                                     ),
                                     
                                     titlePanel("Data"),
                                     DTOutput("slrDataTable", width = "750px"),
                                     br(),
                                     
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
                                     
                                     #conditionalPanel(
                                       #condition = "input.kendall == 1",
                                       
                                     titlePanel("Kendall's Rank Correlation"),
                                     verbatimTextOutput("Kendall"),
                                     br(),
                                     #),
                                     
                                     #conditionalPanel(
                                       #condition = "input.spearman == 1",
                                       
                                     titlePanel("Spearman's Rank Correlation"),
                                     verbatimTextOutput("Spearman"),
                                     #),
                                     #br(),
                            ),
                ),
              )
            })
            
            model <- lm(daty ~ datx)
            
            main <- input$main
            xlab <- input$xlab
            ylab <- input$ylab
            
            df <- data.frame(datx, daty, datx*daty, datx^2, daty^2)
            names(df) <- c("x", "y", "xy", "x<sup>2</sup>", "y<sup>2</sup>")
            dfTotaled <- bind_rows(df, summarise(df, across(where(is.numeric), sum)))
            rownames(dfTotaled)[nrow(dfTotaled)] <- "Totals"
            
            output$slrDataTable <- renderDT(
              datatable(round(dfTotaled, digits = 3),
                        options = list(pageLength = -1, 
                                      lengthMenu = list(c(-1, 10, 25, 50, 100), c("All", "10", "25", "50", "100"))
                                      ),
                        escape = FALSE
              ) %>% formatStyle(
                                names(dfTotaled),
                                target = 'row',
                                fontWeight = styleRow(dim(dfTotaled)[1], "bold")
              )
            )
            
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
            
            #df <- data.frame(datx, daty, datx*daty, datx^2, daty^2)
            #names(df) <- c("X", "Y", "XY", "X^2", "Y^2")
            #print(df)
            
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

    # --------------------------------------------------------------------- #

        
# **************************************************************************** #
    
    
    #  -------------------------------------------------------------------- #
    #  ------------------------ Component Display -------------------------
    #  -------------------------------------------------------------------- #
    
    # observeEvent(input$dropDownMenu, {
    #   hide(id = 'descriptiveStatsMP')
    #   hide(id = "probabilityMP")
    #   hide(id = "inferenceMP")
    #   hide(id = "RegCorMP")
    # })
    

    #### Descriptive Statistics ----
    #  -------------------------------------------------------------------- #
    
    observeEvent(!ds_iv$is_valid(), {
      hide(id = "descriptiveStatsMP")
    })
    
    observeEvent({input$descriptiveStat
                  input$dsUploadVars}, {
      hide(id = 'descrStatsData')                
    })
    
    
    # observeEvent(input$dsUploadVars, {
    #   hide(id = 'descrStatsData')                
    # })
    
    observeEvent(input$dataInput, {
      hide(id = 'descrStatsData')
      hide(id = 'dsUploadVars')
    })
    
    observe({
      if(is.null(input$dsGraphOptions)) {
        hideTab(inputId = 'dsTabset', target = 'Graphs')
        updateTabsetPanel(inputId = 'dsTabset', selected = 'Table')
      } else {
        showTab(inputId = 'dsTabset', target = 'Graphs')
      }
    })
    
    observeEvent(input$goDescpStats, {
      show(id = 'descriptiveStatsMP')
    })
    
    observeEvent(input$resetAll,{
      hide(id = 'descriptiveStatsMP')
      shinyjs::reset("descriptiveStatsPanel")
      #shinyjs::reset("sideBar")
    })
    
    #  -------------------------------------------------------------------- #
    
    

    #### Probability Distributions ----
    #  -------------------------------------------------------------------- #
    
    observeEvent(!pd_iv$is_valid(), {
      hide(id = 'probabilityMP')
    })
    
    #-----------------------#
    # Binomial Distribution #
    #-----------------------#
    
    observeEvent(input$goBinom, {
      show(id = 'probabilityMP')
    })
    
    observeEvent(input$probability, {
      hide(id = 'probabilityMP')
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
    
    #  -------------------------------------------------------------------- #
    
    

    #### Statistical Inference ----
    #  -------------------------------------------------------------------- #
    
    observeEvent(!si_iv$is_valid(), {
      hide(id = "inferenceMP")
      hide(id = "inferenceData")
    })
    
    observeEvent({input$samplesSelect
      input$sampleSize
      input$sampleMean
      input$popuParameter
      input$dataAvailability
      input$dataAvailability2
      input$sigmaKnown
      input$sigmaKnownRaw
      input$popuSD
      input$popuSDRaw
      input$sampSD
      input$inferenceType
      input$inferenceType2
      input$significanceLevel2
      input$confidenceLevel2}, {
        hide(id = "inferenceData")
      })
    
    observeEvent(input$goInference, {
      show(id = "inferenceMP")
    })
    
    observeEvent(input$resetInference, {
      hide(id = "inferenceMP")
      shinyjs::reset("inferencePanel")
    })
  
    #  -------------------------------------------------------------------- #
    
    

    #### Regression and Correlation ----
    #  -------------------------------------------------------------------- #
    
    observeEvent(!regcor_iv$is_valid(), {
      hide(id = "RegCorMP")
    })
    
    observeEvent(input$dataRegCor, {
      hide(id = "RegCorMP")
      hide(id = "slrResponse")
      hide(id = "slrExplanatory")
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
    
    #  -------------------------------------------------------------------- #
}
  
shinyApp(ui = ui, server = server)