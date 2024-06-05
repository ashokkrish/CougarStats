
  
  # ----------------------- #  
  # ---- UI components ---- 
  # ----------------------- #
  ui <- fluidPage(
    theme = bs_theme(version = 4,
                     primary = "#18536F"),
    
    tags$head(
        tags$link(rel = "stylesheet", 
                  type="text/css", 
                  href="cougarstats-styles.css"),

        tags$link(rel = "icon",
                  type="image/x-icon",
                  href="favicon.ico"),
    ),
  
    navbarPage(
      title = div(img(src ="CougarStatsLogo.png", height = 100), 
                  span(" CougarStats ", class = "pageTitle")), 
      
                             
      # --------------------- #  
      # ---- Methods Tab ---- 
      # --------------------- #
      tabPanel(
        title = "Methods",
                                    
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
                          "Sample Size Estimation",
                          "Statistical Inference", 
                          "Regression and Correlation"),
              options = list(
                placeholder = 'Please select a topic below'
              ),
            ),
                                          
                                          
            #   ----------------------------------- #  
            ### ---- Descriptive Stats sidebar ---- 
            #   ----------------------------------- #
            conditionalPanel(
              id = "descriptiveStatsPanel",
              condition = "input.dropDownMenu == 'Descriptive Statistics'",
              style = "display: none;",
                                                           
              # radioButtons(
              #   inputId      = "dataInput",
              #   label        = strong("Data"),
              #   choiceValues = list("Enter Raw Data", 
              #                       "Upload Data"),
              #   choiceNames  = list("Enter Raw Data", 
              #                    "Upload Data"),
              #   selected     = "Enter Raw Data", #character(0), #
              #   inline       = TRUE
              # ), #,width = '1000px'),
              
              radioButtons(
                inputId      = "dataInput",
                label        = strong("Data"),
                choiceValues = list("Enter Raw Data", 
                                    "Upload Data"),
                choiceNames  = list("Enter Raw Data", 
                                    "Upload Data"),
                selected     = "Enter Raw Data", #character(0), #
                inline       = TRUE),
                                                           
              conditionalPanel(
                condition = "input.dataInput == 'Enter Raw Data'",
                                                             
                textAreaInput("descriptiveStat", 
                  label       = strong("Sample"), 
                  value       = "2.14,   2.09,   2.65,   3.56,   5.55,   5.00,   5.55,   8.09,   10.79", 
                  placeholder = "Enter values separated by a comma with decimals as points", 
                  rows        = 3
                ),
              ),
                                                           
              conditionalPanel(
                condition = "input.dataInput == 'Upload Data'",
                                                             
                fileInput(
                  inputId = 'dsUserData', 
                  label   = strong('Upload your data (.csv or .xls or .xlsx or .txt)'),
                  accept  = c('text/csv','text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.csv',
                              '.txt',
                              '.xls',
                              '.xlsx')
                ),
                                                             
                selectizeInput(
                  inputId  = "dsUploadVars",
                  label    = strong("Choose a Variable"),
                  choices  = c(""),
                  multiple = FALSE,
                  options  = list(placeholder = 'Select a variable',
                             onInitialize = I('function() { this.setValue(""); }')
                  )
                ),
              ),
              
              br(),
                                                           
              shinyWidgets::pickerInput(
                inputId  = "dsTableFilters",
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
                                            "Kurtosis")
                ),
                selected = c("Observations",
                             "Mean",
                             "Minimum", 
                             "First Quartile (Q1)", 
                             "Second Quartile or Median (Q2)", 
                             "Third Quartile (Q3)", 
                             "Maximum", 
                             "Sample Standard Deviation"),
                options  = pickerOptions(
                                actionsBox = TRUE,
                                selectedTextFormat = 'count',
                                style = "btn-outline-primary",
                                hideDisabled = TRUE
                           ),
                multiple = TRUE
              ),
                                                           
              br(),
                                                           
              selectizeInput(
                inputId  = "dsGraphOptions",
                label    = strong("Graphs"), 
                choices  = c("Boxplot", 
                            "Histogram", 
                            "Stem and Leaf Plot"),
                selected = c("Boxplot"),
                multiple = TRUE,
                options = list(hideSelected = FALSE,
                               placeholder = 'Select graph(s) to display')
              ),
                                                           
              br(),
                                                           
              actionButton(
                inputId = "goDescpStats", 
                label = "Calculate",
                class = "act-btn"
                # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              actionButton(
                inputId = "resetAll", 
                label = "Reset Values",
                class = "act-btn"
                # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ) #, onclick = "history.go(0)"
            ), #DescriptiveStats Panel
                                          
            #   ------------------------------------------- #  
            ### ---- Probability Distributions sidebar ---- 
            #   ------------------------------------------- #
            conditionalPanel(id = "probPanel",
              condition = "input.dropDownMenu == 'Probability Distributions'",
              style = "display: none;",
                                                           
              radioButtons(
                inputId  = "probability", 
                label    = strong("Distribution"), 
                choices  = c("Contingency Table",
                            "Binomial", 
                            "Poisson", 
                            "Normal"), 
                selected = NULL, 
                inline   = TRUE),
              
              
              #    ---------------------------- #
              #### ---- Contingency Tables ----
              #    ---------------------------- #
              conditionalPanel(id = "contingencyPanel", 
                condition = "input.probability == 'Contingency Table'",
                                                                            
                radioButtons(
                  inputId = "cTableDimension",
                  label   = strong("Dimension"),
                  choices = c("2 x 2",
                              "2 x 3",
                              "3 x 2",
                              "3 x 3"),
                  inline  = TRUE),
                                                                            
                # radioButtons(
                #   inputId = "cTableType",
                #   label   = strong("Data Format"),
                #   choices = c("Frequency Distribution",
                #               "Probability Distribution"),
                #   inline  = TRUE
                # ),
                                                                            
                conditionalPanel(
                  condition = "input.cTableDimension == '2 x 2'",
                                                                                            
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Frequency Distribution'",

                    matrixInput(
                      inputId = "cMatrix2x2",
                      inputClass = "cMatrix",
                      value = matrix(c(18,22, 21,152), 
                                     2, 2, 
                                     dimnames = list(c("R1", "R2"), 
                                                     c("C1", "C2"))),
                      rows = list(editableNames = TRUE),
                      cols = list(editableNames = TRUE)
                    ),
                  # ), # Frequency Distribution

                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Probability Distribution'",

                  #   matrixInput(
                  #     inputId = "pMatrix2x2",
                  #     inputClass = "cMatrix",
                  #     value = matrix(c(0.18,0.22, 0.41,0.19), 
                  #                    2, 2, 
                  #                    dimnames = list(c("R1", "R2"), 
                  #                                    c("C1", "C2"))),
                  #     rows = list(editableNames = TRUE),
                  #     cols = list(editableNames = TRUE)
                  #   ),
                  # ), # Probability Distribution
                ), # 2x2
                                                                            
                conditionalPanel(
                  condition = "input.cTableDimension == '2 x 3'",
                                                                            
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Frequency Distribution'",
                                                                              
                    matrixInput(
                      inputId = "cMatrix2x3",
                      inputClass = "cMatrix",
                      value = matrix(c(30,210, 26,121, 0,20), 
                                     2, 3, 
                                     dimnames = list(c("R1", "R2"), 
                                                     c("C1", "C2", "C3"))),
                      rows = list(editableNames = TRUE),
                      cols = list(editableNames = TRUE)
                    ),
                  # ), # Frequency Distribution
                                                                              
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Probability Distribution'",
                  #                                                               
                  #   matrixInput(
                  #     inputId = "pMatrix2x3",
                  #     inputClass = "cMatrix",
                  #     value = matrix(c(0.28,0.11, 0.08,0.26, 0.13,0.14), 
                  #                    2, 3, 
                  #                    dimnames = list(c("R1", "R2"), 
                  #                                    c("C1", "C2", "C3"))),
                  #     rows = list(editableNames = TRUE),
                  #     cols = list(editableNames = TRUE)
                  #   ),
                  # ), # Probability Distribution
                ), # 2x3
                                                                            
                conditionalPanel(
                  condition = "input.cTableDimension == '3 x 2'",
                                                                              
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Frequency Distribution'",
                                                                                
                    matrixInput(
                      inputId = "cMatrix3x2",
                      inputClass = "cMatrix",
                      value = matrix(c(115,75,142, 250,183,235), 
                                     3, 2, 
                                     dimnames = list(c("R1", "R2", "R3"), 
                                                     c("C1", "C2"))),
                      rows = list(editableNames = TRUE),
                      cols = list(editableNames = TRUE)
                    ),
                  # ), # Frequency Distribution
                                                                              
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Probability Distribution'",
                  #                                                               
                  #   matrixInput(
                  #     inputId = "pMatrix3x2",
                  #     inputClass = "cMatrix",
                  #     value = matrix(c(0.115,0.075,0.142, 0.250,0.183,0.235), 
                  #                    3, 2, 
                  #                    dimnames = list(c("R1", "R2", "R3"), 
                  #                                    c("C1", "C2"))),
                  #     rows = list(editableNames = TRUE),
                  #     cols = list(editableNames = TRUE)
                  #   ),
                  # ), # Probability Distribution
                ), # 3x2
                                                                            
                conditionalPanel(
                  condition = "input.cTableDimension == '3 x 3'",
                                                                                            
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Frequency Distribution'",
                                                
                    matrixInput(
                      inputId = "cMatrix3x3",
                      inputClass = "cMatrix",
                      value = matrix(c(6,14,50, 38,31,50, 31,4,5), 
                                     3, 3, 
                                     dimnames = list(c("R1", "R2", "R3"), 
                                                     c("C1", "C2", "C3"))),
                      rows = list(editableNames = TRUE),
                      cols = list(editableNames = TRUE)
                    ),
                  # ), # Frequency Distribution
                                                                                
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Probability Distribution'",
                  #                                                                 
                  #   matrixInput(
                  #     inputId    = "pMatrix3x3",
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
                  inputId = "cTableProb",
                  label   = strong("Probabilities"),
                  choices = c("Marginal",
                              "Joint",
                              "Union",
                              "Conditional"),
                  inline = TRUE),
                                                                            
                actionButton(
                  inputId = "gocTable", 
                  label   = "Calculate",
                  class = "act-btn"
                  # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ),
                actionButton(
                  inputId = "resetcTable", 
                  label   = "Reset Values",
                  class = "act-btn"
                  # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ),
              ), # Contingency Tables

              
              #    ------------------ # 
              #### ---- Binomial ----
              #    ------------------ #
              conditionalPanel(id = "binomialPanel", 
                condition = "input.probability == 'Binomial'",
                                                                            
                numericInput(
                  inputId = "numTrialsBinom",
                  label   = strong("Number of Trials (\\( n\\))"),
                  value   = 7, 
                  min     = 1, 
                  step    = 1
                ),
                                                                            
                numericInput(
                  inputId = "successProbBinom",
                  label   = strong("Probability of Success (\\( p\\))"),
                  value   = 0.15, 
                  min     = 0, 
                  max     = 1, 
                  step    = 0.00001
                ),
                
                HTML("<label class='si-label'><b>Probability</b></label>"),                                                            
                radioButtons(
                  inputId      = "calcBinom",
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
                  inline       = FALSE), #,width = '1000px'),
                                                                            
                conditionalPanel(
                  condition = "input.calcBinom != 'between'",
                                                                              
                  numericInput(
                    inputId = "numSuccessesBinom",
                    label   = strong("Number of Successes (\\( x\\))"),
                    value   = 2, 
                    min     = 0, 
                    step    = 1
                  )
                ), # !between
                                                                            
                conditionalPanel(
                  condition = "input.calcBinom == 'between'",
                                                                            
                  numericInput(
                    inputId = "numSuccessesBinomx1",
                    label   = strong("Number of Successes (\\( x_{1}\\))"),
                    value   = 2, 
                    min     = 0, 
                    step    = 1
                  ),
                                                                              
                  numericInput(
                    inputId = "numSuccessesBinomx2",
                    label   = strong("Number of Successes (\\( x_{2}\\))"),
                    value   = 4, 
                    min     = 0, 
                    step    = 1
                  )
                ), # between
                                                                            
                br(),
                p(strong("Options")),
                hr(),
                                                                            
                checkboxInput(
                  inputId = "showBinomTable", 
                  label   = "Display Probability Distribution Table", 
                  value   = TRUE
                ),
                                                                            
                actionButton(
                  inputId = "goBinom", 
                  label   = "Calculate",
                  class = "act-btn"
                  # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ),
                actionButton(
                  inputId = "resetBinomial", 
                  label   = "Reset Values",
                  class = "act-btn"
                  # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ), # , onclick = "history.go(0)"
                # br(),
                # downloadButton('downloadBinomResults', 'Download Results'),
                
              ), # Binomial
                           
              
              #    ----------------- #
              #### ---- Poisson ----
              #    ----------------- #
              conditionalPanel(id = "poissonPanel", 
                condition = "input.probability == 'Poisson'",
                                                                            
                numericInput(
                  inputId = "muPoisson", 
                  label   = strong("Average (\\( \\mu\\))"),
                  value   = 4.5
                ),
                
                HTML("<label class='si-label'><b>Probability</b></label>"),                                                            
                radioButtons(
                  inputId      = "calcPoisson",
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
                  inline       = FALSE), #,width = '1000px'),
                                                                            
                conditionalPanel(
                  condition = "input.calcPoisson != 'between'",
                                                                              
                  numericInput(
                    inputId = "xPoisson", 
                    label   = strong("Number of Successes (\\( x\\))"),
                    value   = 4, 
                    min     = 0, 
                    step    = 1
                  ),
                ), # !between
                                                                            
                conditionalPanel(
                  condition = "input.calcPoisson == 'between'",
                                                                            
                  numericInput(
                    inputId = "x1Poisson",
                    label   = strong("Number of Successes (\\( x_{1}\\))"),
                    value   = 4, 
                    min     = 0, 
                    step    = 1
                  ),
                                                                              
                  numericInput(
                    inputId = "x2Poisson",
                    label   = strong("Number of Successes (\\( x_{2}\\))"),
                    value   = 6,
                    min     = 0, 
                    step    = 1
                  )
                ), # between
                                                                            
                br(),
                p(strong("Options")),
                hr(),
                                                                            
                checkboxInput(
                  inputId = "showPoissTable", 
                  label   = "Display Probability Distribution Table", 
                  value   = TRUE
                ),
                                                                            
                actionButton(
                  inputId = "goPoisson", 
                  label   = "Calculate",
                  class = "act-btn"
                  # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ),
                actionButton(
                  inputId = "resetPoisson", 
                  label   = "Reset Values",
                  class = "act-btn"
                  # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ) #, onclick = "history.go(0)"
              ), # Poisson
                                          
              
              #    ---------------- #
              #### ---- Normal ----
              #    ---------------- #
              conditionalPanel(id = "normalPanel", 
                condition = "input.probability == 'Normal'",
                                                                            
                numericInput(
                  inputId = "popMean", 
                  label   = strong("Population Mean (\\( \\mu\\))"), 
                  value   = 0, 
                  step    = 0.00001
                ),
                                                                            
                numericInput(
                  inputId = "popSD",
                  label   = strong("Population Standard Deviation (\\( \\sigma\\))"),
                  value   = 1, 
                  min     = 0, 
                  step    = 0.00001
                ),
                                                                            
                radioButtons(
                  inputId      = "calcQuantiles",
                  label        = strong("Type of Calculation"),
                  choiceValues = list("Probability", "Quantile"),
                  choiceNames  = list("Probability", "Quantile"),
                  inline       = TRUE),
                                                                            
                conditionalPanel( ##### Probability ----
                  condition = "input.calcQuantiles == 'Probability'",
                                                                              
                  checkboxInput(
                    inputId = "sampMeanDistr",
                    label   = strong("Sampling Distribution of the Sample Mean"),
                    value   = 0
                  ),
                                                                              
                  conditionalPanel(
                    condition = "input.sampMeanDistr == 0",
                            
                    HTML("<label class='si-label'><b>Probability</b></label>"),
                    radioButtons(
                      inputId      = "calcNormal",
                      label        = NULL, 
                      choiceValues = list("cumulative", 
                                          "upperTail", 
                                          "between"),
                      choiceNames  = list("\\(P(X \\leq x)\\) or \\(P(X < x)\\)", 
                                          "\\(P(X \\ge x)\\) or \\(P(X \\gt x)\\)", 
                                          "\\(P(x_1 \\leq X \\leq x_2)\\)"),
                      inline       = FALSE),
                                                                                
                    conditionalPanel(
                      condition = "input.calcNormal != 'between'",
                                                                                  
                      numericInput(
                        inputId = "xValue",
                        label   = strong("Normally Distributed Variable (\\( x\\))"),
                        value   = 0, 
                        step    = 0.00001
                      ),
                    ), # !between
                                                                                
                    conditionalPanel(
                      condition = "input.calcNormal == 'between'",
                                                                                  
                      numericInput(
                        inputId = "x1Value",
                        label   = strong("Normally Distributed Variable (\\( x_{1}\\))"),
                        value   = -1, 
                        step    = 0.00001
                      ),
                                                                                  
                      numericInput(
                        inputId = "x2Value",
                        label   = strong("Normally Distributed Variable (\\( x_{2}\\))"),
                        value   = 1, 
                        step    = 0.00001
                      ),
                    ), # between
                  ), # !sampMeanDistr
                                                                              
                  conditionalPanel(
                    condition = "input.sampMeanDistr == 1",
                              
                    HTML("<label class='si-label'><b>Probability</b></label>"),                                                  
                    radioButtons(
                      inputId      = "calcNormSampDistr",
                      label        = NULL, 
                      choiceValues = list("cumulative", 
                                          "upperTail", 
                                          "between"),
                      choiceNames  = list("\\(P(\\bar{X} \\leq x)\\) or \\(P(\\bar{X} < x)\\)", 
                                          "\\(P(\\bar{X} \\ge x)\\) or \\(P(\\bar{X} \\gt x)\\)", 
                                          "\\(P(x_1 \\leq \\bar{X} \\leq x_2)\\)"),
                      inline       = FALSE), #,width = '1000px'),
                                                                                
                    conditionalPanel(
                      condition = "input.calcNormSampDistr != 'between'",
                                                                                
                      numericInput(
                        inputId = "sampDistrxValue",
                        label   = strong("Normally Distributed Variable (\\( \\bar{x}\\))"),
                        value   = 0, 
                        step    = 0.00001
                      ),
                    ), # !between
                                                                                
                    conditionalPanel(
                      condition = "input.calcNormSampDistr == 'between'",
                                                                                  
                      numericInput(
                        inputId = "sampDistrx1Value",
                        label   = strong("Normally Distributed Variable (\\( \\bar{x}_{1}\\))"),
                        value   = -1, 
                        step    = 0.00001
                      ),
                                                                                  
                      numericInput(
                        inputId = "sampDistrx2Value",
                        label   = strong("Normally Distributed Variable (\\( \\bar{x}_{2}\\))"),
                        value   = 1, 
                        step    = 0.00001
                      ),
                    ), # between
                                                                                
                    numericInput(
                      inputId = "sampDistrSize",
                      label   = strong("Sample Size (\\( n\\))"),
                      value   = 10, 
                      step    = 1
                    ),
                  ), # sampMeanDistr
                                                                              
                  actionButton(
                    inputId = "goNormalProb", 
                    label = "Calculate",
                    class = "act-btn"
                    # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  ),
                  actionButton(
                    inputId = "resetNormalProb", 
                    label = "Reset Values",
                    class = "act-btn"
                    # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  ) #, onclick = "history.go(0)"
                ), # Normal Probability
                                                                            
                conditionalPanel( ##### Quantile ----
                  condition = "input.calcQuantiles == 'Quantile'",
                     
                  radioButtons(
                    inputId      = "calcQuartiles",
                    label        = NULL,
                    choiceValues = list("Quartiles", 
                                        "Percentile"),
                    choiceNames  = list("Quartiles \\( (Q_{1}, Q_{2}, Q_{3}) \\)", 
                                        "Percentile"),
                    inline       = TRUE),
                                                                              
                  conditionalPanel(
                    condition = "input.calcQuartiles == 'Percentile'",
                                                                                
                    autonumericInput(
                      inputId       = "percentileValue",
                      label         = strong("Percentile Value"),
                      value         = 25,
                      minimumValue  = 0,
                      maximumValue  = 100,
                      decimalPlaces = 0,
                      align         = 'left',
                      suffixText    = '%',
                      wheelOn       = 'hover',
                      wheelStep     = 1
                    ),
                  ), # Percentile
                                                                              
                  actionButton(
                    inputId = "goNormalQuan", 
                    label   = "Calculate",
                    class = "act-btn"
                    # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  ),
                  actionButton(
                    inputId = "resetNormalQuan", 
                    label   = "Reset Values",
                    class = "act-btn"
                    # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  ) #, onclick = "history.go(0)"
                ), # Quantile
              ) # Normal
            ), #ProbPanel
                                          
                                        
            #   --------------------------------------- #  
            ### --- Sample Size Estimation sidebar ---- 
            #   --------------------------------------- #
            conditionalPanel(id = "sampSizeEstPanel",
              condition = "input.dropDownMenu == 'Sample Size Estimation'",
              style = "display: none;",
              
              # htmltools::tagAppendAttributes(
              #   prettyRadioButtons(
              #     inputId      = "sampSizeEstParameter",
              #     label        = strong("Parameter of Interest"),
              #     choiceValues = list("Population Mean",
              #                         "Population Proportion"),
              #     choiceNames  = list("Population Mean (\\( \\mu \\)) ",
              #                         "Population Proportion (\\( p\\))"),
              #     selected     = "Population Mean", #character(0), #
              #     inline       = TRUE,
              #     outline      = TRUE,
              #     status       = "primary"
              #   ), #,width = '1000px'),
              # 
              #   class = "latexRadio"
              # ),
              radioButtons(
                inputId      = "sampSizeEstParameter",
                label        = strong("Parameter of Interest"),
                choiceValues = list("Population Mean",
                                    "Population Proportion"),
                choiceNames  = list("Population Mean (\\( \\mu \\)) ",
                                    "Population Proportion (\\( p\\))"),
                selected     = "Population Mean", #character(0), #
                inline       = TRUE),

              radioButtons(
                inputId  = "confLeveln",
                label    = strong("Confidence Level (\\( 1- \\alpha\\))"),
                choices  = c("90%", 
                             "95%",
                             "99%"),
                selected = c("95%"),
                inline   = TRUE),
              
              # 4 Options: Mean & Margin of Error, Mean & Width of Interval,
              #            Prop & Margin of Error, Prop & Width of Interval
              
              
              # Population Mean + (MoE or WoI)
              radioButtons(
                inputId      = "sseEstimationType",
                label        = strong("Estimation Type"),
                choiceValues = list("Margin of Error",
                                    "Width of Interval"),
                choiceNames  = list("Margin of Error (\\( E\\)) ",
                                    "Width of Interval (\\( W\\))"),
                selected     = "Margin of Error",
                inline       = TRUE),
                                                           
              conditionalPanel(
                condition = "input.sampSizeEstParameter == 'Population Mean'",
                                                             
                numericInput(
                  inputId = "ssePopuSD",
                  label   = strong("Population Standard Deviation (\\( \\sigma\\))"),
                  value   = "12", 
                  min     = 0.00001, 
                  step    = 0.00001
                ),
                
                conditionalPanel(
                  condition = "input.sseEstimationType == 'Margin of Error'",
                  
                  numericInput(
                    inputId = "sseMeanMargErr",
                    label   = strong("Margin of Error (\\( E\\))"),
                    value   = "8", 
                    min     = 0.00001, 
                    step    = 0.01
                  )
                ),
                
                conditionalPanel(
                  condition = "input.sseEstimationType == 'Width of Interval'",
                  
                  numericInput(
                    inputId = "sseMeanWoI",
                    label   = strong("Width of Interval (\\( W\\))"),
                    value   = "16", 
                    min     = 0.00001, 
                    step    = 0.01
                  )
                )
              ),
              
              
              # Population Proportion + (MoE or WoI)                                             
              conditionalPanel(
                condition = "input.sampSizeEstParameter == 'Population Proportion'",
                                                             
                numericInput(
                  inputId = "sseTargetProp",
                  label   = strong("Target Proportion (\\( \\hat{p} \\))"),
                  value   = "0.5", 
                  min     = 0.00001, 
                  step    = 0.01
                ),
                
                conditionalPanel(
                  condition = "input.sseEstimationType == 'Margin of Error'",
                  
                  numericInput(
                    inputId = "ssePropMargErr",
                    label   = strong("Margin of Error (\\( E\\))"),
                    value   = "0.01", 
                    min     = 0.00001, 
                    step    = 0.01
                  )
                ),
                
                conditionalPanel(
                  condition = "input.sseEstimationType == 'Width of Interval'",
                  
                  numericInput(
                    inputId = "ssePropWoI",
                    label   = strong("Width of Interval (\\( W\\))"),
                    value   = "0.02", 
                    min     = 0.00001, 
                    step    = 0.01
                  )
                )
              ),
               
                                                          
              actionButton(
                inputId = "goSampSizeEst", 
                label   = "Calculate",
                class = "act-btn"
                # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              actionButton(
                inputId = "resetSampSizeEst", 
                label   = "Reset Values",
                class = "act-btn"
                # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ), # Sample Size Est
                                          
                                          
            #   --------------------------------------- #  
            ### ---- Statistical Inference sidebar ---- 
            #   --------------------------------------- #
            conditionalPanel(id = "inferencePanel",
              condition = "input.dropDownMenu == 'Statistical Inference'",
              style     = "display: none;",
                            
              HTML("<label class='si-label'><b>Methodology</b></label>"),
              
              radioButtons(
                inputId      = "siMethod",
                label        = NULL,
                choiceValues = list("1", 
                                    "2",
                                    "Multiple",
                                    "Categorical"),
                choiceNames  = list("Inference about 1 sample\\(\\)", 
                                    "Inference about 2 samples\\(\\)",
                                    "Inference about more than 2 samples (e.g. ANOVA)\\(\\)",
                                    "Inference for Categorical Data (e.g \\( \\chi^2 \\) test)"),
                selected     = "1"), #,width = '1000px'),
              
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
                condition = "input.siMethod == '1'",
                                                                           
                radioButtons(
                  inputId      = "popuParameter",
                  label        = strong("Parameter of Interest"),
                  choiceValues = list("Population Mean",
                                      "Population Standard Deviation",
                                      "Population Proportion"),
                  choiceNames  = list("Population Mean (\\( \\mu \\)) ",
                                      "Population Standard Deviation (\\( \\sigma\\)) ",
                                      "Population Proportion (\\( p\\))"),
                  selected     = "Population Mean", #character(0), #
                  inline       = FALSE), #,width = '1000px'),
                                                                             
                conditionalPanel( ##### Mean ----
                  condition = "input.popuParameter == 'Population Mean'",
                                                                                               
                  radioButtons(
                    inputId      = "dataAvailability",
                    label        = strong("Data Availability"),
                    choiceValues = list("Summarized Data", 
                                        "Enter Raw Data",
                                        "Upload Data"),
                    choiceNames  = list("Summarized Data", 
                                        "Enter Raw Data",
                                        "Upload Data"),
                    selected     = "Summarized Data", # character(0), # 
                    inline       = TRUE), #,width = '1000px'),
                                                                                               
                  conditionalPanel( ###### Summarized ----
                    condition = "input.dataAvailability == 'Summarized Data'",
                                                                                                                 
                    numericInput(
                      inputId = "sampleSize",
                      label   = strong("Sample Size (\\( n\\))"),
                      value   = 18, 
                      min     = 1, 
                      step    = 1
                    ),
                                                                                                                 
                    numericInput(
                      inputId = "sampleMean",
                      label   = strong("Sample Mean (\\( \\bar{x}\\))"),
                      value   = 103.5375, 
                      step    = 0.00001
                    ),
                                                                                                                 
                    radioButtons(
                      inputId      = "sigmaKnown",
                      label        = strong("Is Population Standard Deviation (\\( \\sigma\\)) known?"),
                      choiceValues = list("Known", 
                                          "Unknown"),
                      choiceNames  = list("Known", 
                                          "Unknown"),
                      selected     = "Known", #character(0),
                      inline       = TRUE), #,width = '1000px'),
                                                                                                                 
                    conditionalPanel( ####### " Known ----
                      condition = "input.sigmaKnown == 'Known'",
                                                                                                                                 
                      numericInput(
                        inputId = "popuSD",
                        label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                        value   = 8.25, 
                        min     = 0.00001, 
                        step    = 0.00001
                      )
                    ), #Sigma Known
                                                                                                                 
                    conditionalPanel( ####### " Unknown ----
                      condition = "input.sigmaKnown == 'Unknown'",
                                                                                                                               
                      numericInput(
                        inputId = "sampSD",
                        label   = strong("Sample Standard Deviation (\\( s\\)) Value"),
                        value   = 4.78, 
                        min     = 0.00001, 
                        step    = 0.00001
                      )
                    ), # Sigma Unknown
                  ), # One Sample Summarized Data
                                                                                               
                  conditionalPanel( ###### Raw ----
                    condition = "input.dataAvailability == 'Enter Raw Data'",
                                                                                                               
                    textAreaInput(
                      inputId     = "sample1", 
                      label       = strong("Sample"), 
                      value       = "202, 210, 215, 220, 220, 224, 225, 228, 228, 228", 
                      placeholder = "Enter values separated by a comma with decimals as points", 
                      rows        = 3
                    ),
                                                                                                                 
                    radioButtons(
                      inputId      = "sigmaKnownRaw",
                      label        = strong("Population Standard Deviation (\\( \\sigma\\))"),
                      choiceValues = list("rawKnown", 
                                          "rawUnknown"),
                      choiceNames  = list("Known", 
                                          "Unknown"),
                      selected     = "rawUnknown",
                      inline       = TRUE), #,width = '1000px'),
                                                                                                                 
                    conditionalPanel( ###### " Known ----
                      condition = "input.sigmaKnownRaw == 'rawKnown'",
                                                                                                                                   
                      numericInput(
                        inputId = "popuSDRaw",
                        label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                        value   = 8.25, 
                        min     = 0.00001, 
                        step    = 0.00001
                      )
                    ), # Sigma Known
                                                                                                                 
                    conditionalPanel( ###### " Unknown
                      condition = "input.sigmaKnownRaw == 'rawUnknown'"
                    ) # Sigma Unknown
                  ), # One Sample Raw Data
                                                                                               
                  conditionalPanel( ###### Upload ----
                    condition = "input.dataAvailability == 'Upload Data'",
                                                                                                 
                    fileInput(
                      inputId = "oneMeanUserData", 
                      label   = strong("Upload your Data (.csv or .xls or .xlsx or .txt)"), 
                      accept  = c("text/csv",
                                  "text/comma-separated-values", 
                                  "text/plain", 
                                  ".csv",
                                  ".xls",
                                  ".xlsx")
                    ),
                                                                                                 
                    selectizeInput(
                      inputId = "oneMeanVariable",
                      label   = strong("Choose a Column for Analysis"),
                      choices = c(""),
                      options = list(placeholder = 'Select a column',
                                     onInitialize = I('function() { this.setValue(""); }'))
                    ),
                                                                                                 
                    radioButtons(
                      inputId      = "sigmaKnownUpload",
                      label        = strong("Population Standard Deviation (\\( \\sigma\\))"),
                      choiceValues = list("Known", 
                                          "Unknown"),
                      choiceNames  = list("Known", 
                                          "Unknown"),
                      selected     = "Unknown",
                      inline       = TRUE),
                                                                                                 
                    conditionalPanel( ###### " Known ----
                      condition = "input.sigmaKnownUpload == 'Known'",
                                                                                                                   
                      numericInput(
                        inputId = "popuSDUpload",
                        label   = strong("Population Standard Deviation (\\( \\sigma\\)) Value"),
                        value   = 5, 
                        min     = 0.00001, 
                        step    = 0.00001
                      )
                    ), # Sigma Known
                  ), # One Sample upload data
                ), # One Population Mean
                                                                             
                conditionalPanel(##### Proportion ----
                  condition = "input.popuParameter == 'Population Proportion'",
                                                                                              
                  numericInput(
                    inputId = "numSuccesses",
                    label   = strong("Number of Successes (\\( x\\))"),
                    value   = 1087, 
                    min     = 0, 
                    step    = 1
                  ),
                                                                                              
                  numericInput(
                    inputId = "numTrials",
                    label   = strong("Number of Trials (\\( n\\))"),
                    value   = 1430, 
                    min     = 1, 
                    step    = 1
                  ),
                ), #One Population Proportion 
                
                ##### Sample Standard Deviation ----
                conditionalPanel(
                  condition = "input.popuParameter == 'Population Standard Deviation'",
                  
                  numericInput(
                    inputId = "SSDSampleSize",
                    label   = strong("Sample Size (\\( n\\))"),
                    value   = 30, 
                    step    = 0.00001
                  ),
                  
                  numericInput(
                    inputId = "SSDStdDev",
                    label   = strong("Sample Standard Deviation (\\( s\\))"),
                    value   = 12.23, 
                    step    = 0.00001
                  ),
                  
                ),
                                                                         
                #conditionalPanel(
                #   condition = "input.dataAvailability == 'Summarized Data' || input.dataAvailability == 'Enter Raw Data' || input.popuParameter == 'Population Proportion'",
                                                                           
                radioButtons(
                  inputId      = "inferenceType",
                  label        = strong("Inference Type"),
                  choiceValues = list("Confidence Interval", 
                                      "Hypothesis Testing"),
                  choiceNames  = list("Confidence Interval", 
                                      "Hypothesis Testing"),
                  selected     = "Confidence Interval", #character(0), # 
                  inline       = TRUE), #,width = '1000px'),
                                                                             
                conditionalPanel(
                  condition = "input.inferenceType == 'Confidence Interval'",
                                                                               
                  radioButtons(
                    inputId  = "confidenceLevel", 
                    label    = strong("Confidence Level (\\( 1- \\alpha\\))"), 
                    choices  = c("90%", 
                                 "95%",
                                 "99%"), 
                    selected = c("95%"),
                    inline   = TRUE)
                ), # Confidence Interval
                                                                             
                conditionalPanel(
                  condition = "input.inferenceType == 'Hypothesis Testing'",
                                                                             
                  radioButtons(
                    inputId  = "significanceLevel", 
                    label    = strong("Significance Level (\\( \\alpha\\))"), 
                    choices  = c("10%", 
                                 "5%",
                                 "1%"),
                    selected = c("5%"),
                    inline   = TRUE),
                                                                               
                  conditionalPanel(
                    condition = "input.popuParameter == 'Population Mean'",
                                                                                 
                    numericInput(
                      inputId = "hypMean",
                      label   = strong("Hypothesized Population Mean (\\( \\mu_{0}\\)) Value"),
                      value   = 99, 
                      step    = 0.00001
                    ),
                  ), # Population Mean
                                                                               
                  conditionalPanel(
                    condition = "input.popuParameter == 'Population Proportion'",
                                                                                 
                    numericInput(
                      inputId = "hypProportion",
                      label   = strong("Hypothesized Population Proportion (\\( p_{0}\\)) Value"),
                      value   = 0.73, 
                      min     = 0, 
                      max     = 1, 
                      step    = 0.00001
                    ),
                  ), # Population Proportion
                                                                               
                  selectizeInput(
                    inputId  = "altHypothesis",
                    label    = strong("Alternate Hypothesis (\\( H_{a}\\))"),
                    choices  = c("< " = 1,
                                 "&ne; " = 2,
                                 "> " = 3),
                    selected = 2,
                    options  = list(render = I(render)),
                  ),
                ), # Hypothesis Testing
                                                                           
                conditionalPanel(
                  condition = "input.popuParameter == 'Population Mean' && input.dataAvailability != 'Summarized Data'",
                                                                               
                  p(strong("Graph Options")),
                  
                  checkboxInput(
                    inputId = "oneMeanBoxplot",
                    label   = "Boxplot for Sample Data",
                    value   = TRUE
                  )
                ) # Pop Mean ! Summarized
              ), #"input.siMethod == '1'"
                                                           
              conditionalPanel( #### 2 Sample ----
                condition = "input.siMethod == '2'",
                        
                HTML("<label class='si-label'><b>Parameter of Interest</b></label>"),                                   
                
                radioButtons(
                  inputId      = "popuParameters",
                  label        = NULL,
                  choiceValues = list("Independent Population Means", 
                                      "Dependent Population Means", 
                                      "Population Proportions"),
                  choiceNames  = list("Two Independent Populations (\\( \\mu_{1} - \\mu_{2} \\))", 
                                      "Dependent (Paired) Populations (\\( \\mu_{d} \\))", 
                                      "Two Population Proportions (\\( p_{1} - p_{2}\\))"),
                  selected     = "Independent Population Means", #character(0), #
                  inline       = FALSE), #,width = '1000px'),
                
                                                             
                conditionalPanel( ##### Ind Pop Means ----
                  condition = "input.popuParameters == 'Independent Population Means'",
                                                               
                  radioButtons(
                    inputId      = "dataAvailability2",
                    label        = strong("Data Availability"),
                    choiceValues = list("Summarized Data", 
                                        "Enter Raw Data",
                                        "Upload Data"),
                    choiceNames  = list("Summarized Data", 
                                        "Enter Raw Data",
                                        "Upload Data"),
                    selected     = "Summarized Data", #character(0), # 
                    inline       = TRUE), #,width = '1000px'),
                                                               
                  conditionalPanel(
                    condition = "input.dataAvailability2 == 'Summarized Data'",
                                                                 
                    numericInput(
                      inputId = "sampleSize1",
                      label   = strong("Sample Size 1 (\\( n_{1}\\))"),
                      value   = 21, 
                      min     = 2, 
                      step    = 1
                    ),
                                                                 
                    numericInput(
                      inputId = "sampleMean1",
                      label   = strong("Sample Mean 1 (\\( \\bar{x}_{1}\\))"),
                      value   = 29.6, 
                      step    = 0.00001
                    ),
                                                                 
                    numericInput(
                      inputId = "sampleSize2",
                      label   = strong("Sample Size 2 (\\( n_{2}\\))"),
                      value   = 21, 
                      min     = 2, 
                      step    = 1
                    ),
                                                                 
                    numericInput(
                      inputId = "sampleMean2",
                      label   = strong("Sample Mean 2 (\\( \\bar{x}_{2}\\))"),
                      value   = 33.9, 
                      step    = 0.00001
                    ),
                                                                 
                    radioButtons(
                      inputId      = "bothsigmaKnown",
                      label        = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                      choiceValues = list("bothKnown", 
                                          "bothUnknown"),
                      choiceNames  = list("Both Known", 
                                          "Both Unknown"),
                      selected     = "bothKnown",
                      inline       = TRUE), #,width = '1000px'),
                                                                 
                    conditionalPanel(
                      condition = "input.bothsigmaKnown == 'bothKnown'",
                                                                 
                      numericInput(
                        inputId = "popuSD1",
                        label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                        value   = 5.36, 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                                                                   
                      numericInput(
                        inputId = "popuSD2",
                        label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                        value   = 5.97, 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                    ), # Sigma Both Known
                                                                 
                    conditionalPanel(
                      condition = "input.bothsigmaKnown == 'bothUnknown'",
                                                                   
                      radioButtons(
                        inputId      = "bothsigmaEqual",
                        label        = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                        choiceValues = list("TRUE", 
                                            "FALSE"),
                        choiceNames  = list("Yes (Pooled)", 
                                            "No (Welch-Satterthwaite df)"),
                        selected     = "TRUE",
                        inline       = TRUE), #,width = '1000px'),
                                                                   
                      numericInput(
                        inputId = "sampSD1",
                        label   = strong("Sample Standard Deviation 1 (\\( s_{1}\\)) Value"),
                        value   = 5.24, 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                                                                   
                      numericInput(
                        inputId = "sampSD2",
                        label   = strong("Sample Standard Deviation 2 (\\( s_{2}\\)) Value"),
                        value   = 5.85, 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                    ), # Sigma Both Unknown
                  ), # Summarized Data
                                                             
                  conditionalPanel(
                    condition = "input.dataAvailability2 == 'Enter Raw Data'",
                                                                 
                    textAreaInput(
                      inputId     = "raw_sample1", 
                      label       = strong("Sample 1"), 
                      value       = "101.1,  111.1,  107.6,  98.1,  99.5,  98.7,  103.3,  108.9,  109.1,  103.3", 
                      placeholder = "Enter values separated by a comma with decimals as points", 
                      rows        = 3
                    ),
                                                                 
                    textAreaInput(
                      inputId     = "raw_sample2", 
                      label       = strong("Sample 2"), 
                      value       = "107.1,  105.0,  98.0,  97.9,  103.3,  104.6,  100.1,  98.2,  97.9", 
                      placeholder = "Enter values separated by a comma with decimals as points", 
                      rows        = 3
                    ),
                                                                 
                    radioButtons(
                      inputId      = "bothsigmaKnownRaw",
                      label        = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                      choiceValues = list("bothKnown", 
                                          "bothUnknown"),
                      choiceNames  = list("Both Known", 
                                          "Both Unknown"),
                      selected     = "bothUnknown",
                      inline       = TRUE), #,width = '1000px'),
                                                                 
                    conditionalPanel(
                      condition = "input.bothsigmaKnownRaw == 'bothUnknown'",
                                                                 
                      radioButtons(
                        inputId      = "bothsigmaEqualRaw",
                        label        = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                        choiceValues = list("TRUE", 
                                            "FALSE"),
                        choiceNames  = list("Yes (Pooled)", 
                                            "No (Welch-Satterthwaite df)"),
                        selected     = "TRUE",
                        inline       = TRUE)
                    ), # Sigma Both Unknown
                                                                 
                    conditionalPanel(
                      condition = "input.bothsigmaKnownRaw == 'bothKnown'",
                                                                   
                      numericInput(
                        inputId = "popuSDRaw1",
                        label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                        value   = 4.54, 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                                                                   
                      numericInput(
                        inputId = "popuSDRaw2",
                        label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                        value   = 3.47, 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                    ), #Sigma Both Known
                  ), # Raw Data
                                                               
                  conditionalPanel(
                    condition = "input.dataAvailability2 == 'Upload Data'",
                                                                 
                    fileInput(
                      inputId = "indMeansUserData", 
                      label   = strong("Upload your Data (.csv or .xls or .xlsx or .txt)"), 
                      accept  = c("text/csv",
                                  "text/comma-separated-values", 
                                  "text/plain", 
                                  ".csv",
                                  ".xls",
                                  ".xlsx")
                    ),
                                                                 
                    selectizeInput(
                      inputId = "indMeansUplSample1",
                      label   = strong("Choose a Column for Sample 1"),
                      choices = c(""),
                      options = list(placeholder = 'Select a column',
                                     onInitialize = I('function() { this.setValue(""); }')),
                    ),
                                                                 
                    selectizeInput(
                      inputId = "indMeansUplSample2",
                      label   = strong("Choose a Column for Sample 2"),
                      choices = c(""),
                      options = list(placeholder = 'Select a column',
                                     onInitialize = I('function() { this.setValue(""); }'))
                    ),
                                                               
                    radioButtons(
                      inputId      = "bothsigmaKnownUpload",
                      label        = strong("Are Population Standard Deviations (\\( \\sigma_{1}\\) and \\( \\sigma_{2}\\)) known?"),
                      choiceValues = list("bothKnown", 
                                          "bothUnknown"),
                      choiceNames  = list("Both Known", 
                                          "Both Unknown"),
                      selected     = "bothUnknown",
                      inline       = TRUE), #,width = '1000px'),
                                                                 
                    conditionalPanel(
                      condition = "input.bothsigmaKnownUpload == 'bothUnknown'",
                                                                   
                      radioButtons(
                        inputId      = "bothsigmaEqualUpload",
                        label        = strong("Assume Population Variances are equal (\\( \\sigma_{1}^2\\) = \\( \\sigma_{2}^2\\))?"),
                        choiceValues = list("TRUE", 
                                            "FALSE"),
                        choiceNames  = list("Yes (Pooled)", 
                                            "No (Welch-Satterthwaite df)"),
                        selected     = "TRUE",
                        inline       = TRUE)
                    ), # Sigma Both Unknown
                                                                 
                    conditionalPanel(
                      condition = "input.bothsigmaKnownUpload == 'bothKnown'",
                                                                   
                      numericInput(
                        inputId = "popuSDUpload1",
                        label   = strong("Population Standard Deviation 1 (\\( \\sigma_{1}\\)) Value"),
                        value   = "", 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                                                                   
                      numericInput(
                        inputId = "popuSDUpload2",
                        label   = strong("Population Standard Deviation 2 (\\( \\sigma_{2}\\)) Value"),
                        value   = "", 
                        min     = 0.00001, 
                        step    = 0.00001
                      ),
                    ), # Sigma Both Known
                  ), # Upload Data
                ), # Two Independent Samples

                conditionalPanel( ##### Dep Pop Means ----
                  condition = "input.popuParameters == 'Dependent Population Means'",
                                                                              
                  radioButtons(
                    inputId      = "dataTypeDependent",
                    label        = strong("Data Availability"),
                    choiceValues = list("Enter Raw Data",
                                        "Upload Data"),
                    choiceNames  = list("Enter Raw Data",
                                        "Upload Data"),
                    selected     = "Enter Raw Data", #character(0), # 
                    inline       = TRUE), #,width = '1000px'),
                                                                              
                  conditionalPanel(
                    condition = "input.dataTypeDependent == 'Enter Raw Data'",
                                                                                               
                    textAreaInput(
                      inputId     = "before", 
                      label       = strong("Before"), 
                      value       = "484, 478, 492, 444, 436, 398, 464, 476", 
                      placeholder = "Enter values separated by a comma with decimals as points", 
                      rows        = 3
                    ),
                                                                                               
                    textAreaInput(
                      inputId     = "after", 
                      label       = strong("After"), 
                      value       = "488, 478, 480, 426, 440, 410, 458, 460", 
                      placeholder = "Enter values separated by a comma with decimals as points", 
                      rows        = 3
                    ),
                  ), # Raw Data
                                                                              
                  conditionalPanel(
                    condition = "input.dataTypeDependent == 'Upload Data'",
                                                                              
                    fileInput(
                      inputId = "depMeansUserData", 
                      label   = strong("Upload your Data (.csv or .xls or .xlsx or .txt)"), 
                      accept  = c("text/csv",
                                  "text/comma-separated-values", 
                                  "text/plain", 
                                  ".csv",
                                  ".xls",
                                  ".xlsx")
                    ),
                                                                                               
                    selectizeInput(
                      inputId = "depMeansUplSample1",
                      label   = strong("Choose a Column for 'Before' Sample Data"),
                      choices = c(""),
                      options = list(placeholder = 'Select a column',
                                     onInitialize = I('function() { this.setValue(""); }'))
                    ),
                                                                                             
                    selectizeInput(
                      inputId = "depMeansUplSample2",
                      label   = strong("Choose a Column for 'After' Sample Data"),
                      choices = c(""),
                      options = list(placeholder = 'Select a column',
                                     onInitialize = I('function() { this.setValue(""); }'))
                    ),
                  ) # Upload Data
                ), # Two Dependent Samples
                                                             
                conditionalPanel( ##### Pop Props ----
                  condition = "input.popuParameters == 'Population Proportions'",
                                                           
                  numericInput(
                    inputId = "numSuccesses1",
                    label   = strong("Number of Successes 1 (\\( x_{1}\\))"),
                    value   = 174, 
                    min     = 0, 
                    step    = 1
                  ),
                                                               
                  numericInput(
                    inputId = "numTrials1",
                    label   = strong("Number of Trials 1 (\\( n_{1}\\))"),
                    value   = 300, 
                    min     = 1, 
                    step    = 1
                  ),
                                                               
                  numericInput(
                    inputId = "numSuccesses2",
                    label   = strong("Number of Successes 2 (\\( x_{2}\\))"),
                    value   = 111, 
                    min     = 0, 
                    step    = 1
                  ),
                                                               
                  numericInput(
                    inputId = "numTrials2",
                    label   = strong("Number of Trials 2 (\\( n_{2}\\))"),
                    value   = 300, 
                    min     = 1, 
                    step    = 1
                  ),
                ), # Two Population Proportions

                radioButtons(
                  inputId      = "inferenceType2",
                  label        = strong("Inference Type"),
                  choiceValues = list("Confidence Interval", 
                                      "Hypothesis Testing"),
                  choiceNames  = list("Confidence Interval", 
                                      "Hypothesis Testing"),
                  selected     = "Confidence Interval", #character(0), # 
                  inline       = TRUE), #,width = '1000px'),
                                                               
                conditionalPanel(
                  condition = "input.inferenceType2 == 'Confidence Interval'",
                                                                 
                  radioButtons(
                    inputId  = "confidenceLevel2",
                    label    = strong("Confidence Level (\\( 1- \\alpha\\))"),
                    choices  = c("90%", 
                                 "95%",
                                 "99%"),
                    selected = c("95%"),
                    inline   = TRUE)
                ), # Confidence Interval
                                                               
                conditionalPanel(
                  condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                                                 
                  radioButtons(
                    inputId  = "significanceLevel2", 
                    label    = strong("Significance Level (\\( \\alpha\\))"), 
                    choices  = c("10%", 
                                 "5%",
                                 "1%"),
                    selected = c("5%"),
                    inline   = TRUE),
                                                                 
                  selectizeInput(
                    inputId  = "altHypothesis2",
                    label    = strong("Alternate Hypothesis (\\( H_{a}\\))"),
                    choices  = c("< " = 1,
                                 "&ne; " = 2,
                                 "> " = 3),
                    selected = 2,
                    options  = list(render = I(render))
                  ), 
                ), # Hypothesis Testing
                                                             
                conditionalPanel(
                  condition = "input.popuParameters == 'Independent Population Means' && input.dataAvailability2 != 'Summarized Data'",
                                                             
                  p(strong("Graph Options")),
                  
                  checkboxInput(
                    inputId = "indMeansBoxplot",
                    label   = "Side-by-side Boxplot for Sample Data",
                    value   = TRUE
                  )
                ), # Ind Means !Summarized
              ), # "input.siMethod == '2'",
              
              conditionalPanel( #### More than 2 Samples ----
                condition = 'input.siMethod == "Multiple"',
                
                fileInput(
                  inputId = "anovaUserData", 
                  label   = strong("Upload your Data (.csv or .xls or .xlsx or .txt)"), 
                  accept  = c("text/csv",
                              "text/comma-separated-values", 
                              "text/plain", 
                              ".csv",
                              ".xls",
                              ".xlsx")
                ),
                
                hidden(tagList(
                  div(id = "anovaUploadInputs",
                      
                    radioButtons(
                      inputId = "anovaFormat",
                      label   = strong("Data Format"),
                      choiceNames = c("Values in multiple columns",
                                      "Responses and factors stacked in two columns"),
                      choiceValues = c("Multiple",
                                       "Stacked")),
                      
                    conditionalPanel(
                      condition = "input.anovaFormat == 'Multiple'",
                      
                      selectizeInput(
                        inputId = "anovaMultiColumns",
                        label = strong("Choose columns to conduct analysis"),
                        choices = c(""),
                        multiple = TRUE,
                        selected = NULL,
                        options = list(hideSelected = FALSE,
                                       placeholder = 'Select two or more columns',
                                       onInitialize = I('function() { this.setValue(""); }')),
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.anovaFormat == 'Stacked'",
                      
                      selectizeInput(
                        inputId = "anovaResponse",
                        label = strong("Response Variable"),
                        choices = c(""),
                        selected = NULL,
                        options = list(placeholder = 'Select a variable',
                                       onInitialize = I('function() { this.setValue(""); }')),
                      ),
                      
                      selectizeInput(
                        inputId = "anovaFactors",
                        label = strong("Factors"),
                        choices = c(""),
                        selected = NULL,
                        options = list(placeholder = 'Select a factor',
                                       onInitialize = I('function() { this.setValue(""); }')),
                      )
                    )
                  ),  
                )),
                
                radioButtons(
                  inputId  = "anovaSigLvl", 
                  label    = strong("Significance Level (\\( \\alpha\\))"), 
                  choices  = c("10%", 
                               "5%",
                               "1%"),
                  selected = "5%",
                  inline   = TRUE),
                
                checkboxGroupInput(
                  inputId = "anovaOptions",
                  label = p(strong("Options")),
                  choiceNames = c("Include post hoc tests"),
                  choiceValues = c("posthoc"),
                  selected = NULL
                ),
                
                selectizeInput(
                  inputId = "anovaGraphs",
                  label = strong("Graphs"),
                  choices = c("Side-by-side Boxplot", 
                              "Histogram of Residuals", 
                              "QQ Plot of Residuals",
                              "Plot Group Means"),
                  multiple = TRUE,
                  selected = c("Side-by-side Boxplot",
                               "Plot Group Means"),
                  options = list(hideSelected = FALSE,
                                 placeholder = 'Select graph(s) to display'),
                )
                
              ),
              
              conditionalPanel( #### Chi-Square ----
                condition = 'input.siMethod == "Categorical"',
                                
                radioButtons(
                  inputId = "chisquareDimension",
                  label   = strong("Dimension"),
                  choices = c("2 x 2",
                              "2 x 3",
                              "3 x 2",
                              "3 x 3"),
                  inline  = TRUE),
                                
                conditionalPanel(
                  condition = "input.chisquareDimension == '2 x 2'",
                                  
                  matrixInput(
                    inputId = "chiSqInput2x2",
                    inputClass = "cMatrix",
                    value = matrix(c(173,599, 160,851),
                                   nrow = 2, 
                                   ncol = 2, 
                                   dimnames = list(c("R1", "R2"), 
                                                   c("C1", "C2"))),
                    rows = list(editableNames = TRUE),
                    cols = list(editableNames = TRUE),
                    class = "numeric"
                  ),
                ),
                                
                conditionalPanel(
                  condition = "input.chisquareDimension == '2 x 3'",
                                  
                  matrixInput(
                    inputId = "chiSqInput2x3",
                    inputClass = "cMatrix",
                    value = matrix(c(160,40, 140,60, 40,60),
                                   nrow = 2, 
                                   ncol = 3, 
                                   dimnames = list(c("R1", "R2"), 
                                                   c("C1", "C2", "C3"))),
                    rows = list(editableNames = TRUE),
                    cols = list(editableNames = TRUE),
                    class = "numeric"
                  ),
                ),
                                
                conditionalPanel(
                  condition = "input.chisquareDimension == '3 x 2'",
                                  
                  matrixInput(
                    inputId = "chiSqInput3x2",
                    inputClass = "cMatrix",
                    value = matrix(c(162,106,201, 353,259,332),
                                   nrow = 3, 
                                   ncol = 2, 
                                   dimnames = list(c("R1", "R2", "R3"), 
                                                   c("C1", "C2"))),
                    rows = list(editableNames = TRUE),
                    cols = list(editableNames = TRUE),
                    class = "numeric"
                  ),
                ),
                                
                conditionalPanel(
                  condition = "input.chisquareDimension == '3 x 3'",
                                  
                  matrixInput(
                    inputId = "chiSqInput3x3",
                    inputClass = "cMatrix",
                    value = matrix(c(6,14,50, 38,31,50, 31,4,5),
                                   nrow = 3, 
                                   ncol = 3, 
                                   dimnames = list(c("R1", "R2", "R3"), 
                                                   c("C1", "C2", "C3"))),
                    rows = list(editableNames = TRUE),
                    cols = list(editableNames = TRUE),
                    class = "numeric"
                  ),
                ),
                
                textInput(
                  inputId = "chiSquareRowHeader",
                  label = "Name for Row Variable (optional):",
                  value = ""
                ),

                textInput(
                  inputId = "chiSquareColHeader",
                  label = "Name for Column Variable (optional):",
                  value = ""
                ),
                                
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
                  inputId  = "chisquareMethod", 
                  label    = strong("Hypothesis Test"), 
                  choiceNames  = c("Chi-Square test for independence", 
                                   "Fisher's Exact test"),
                  choiceValues = c("Chi-Square",
                                   "Fisher"),
                  selected = c("Chi-Square"),
                  inline   = TRUE),
                                
                conditionalPanel(
                  condition = 'input.chisquareMethod == "Chi-Square" && input.chisquareDimension == "2 x 2"',
                                  
                  checkboxInput(
                    inputId = "chiSquareYates",
                    label   = "with Yates continuity correction",
                    value   = FALSE
                  ),
                  
                  # tooltip(
                  #   span("Card title ", bsicons::bs_icon("question-circle-fill")),
                  #   "Additional info",
                  #   placement = "right"
                  # ),
                ),
                
                radioButtons(
                  inputId  = "chisquareSigLvl", 
                  label    = strong("Significance Level (\\( \\alpha\\))"), 
                  choices  = c("10%", 
                               "5%",
                               "1%"),
                  selected = c("5%"),
                  inline   = TRUE),
              ),

              actionButton(
                inputId = "goInference", 
                label   = "Calculate",
                class = "act-btn"
                # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              actionButton(
                inputId = "resetInference", 
                label   = "Reset Values",
                class = "act-btn"
                # style   = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ) #, onclick = "history.go(0)"
            ), #inferencePanel
                                          
              
            #   -------------------------------------------- #  
            ### ---- Regression and Correlation sidebar ---- 
            #   -------------------------------------------- #
            conditionalPanel(id = "RegCorPanel",
              condition = "input.dropDownMenu == 'Regression and Correlation'",
              style     = "display: none;",
                                                           
              radioButtons(
                inputId      = "simple_vs_multiple",
                label        = strong("Regression Type"),
                choiceValues = list("SLR"), 
                                    # "MLR"),
                choiceNames  = list("Simple Linear Regression and Correlation Analysis"), 
                                    # "Multiple Linear Regression"),
                selected     = "SLR", #character(0), # 
                inline       = TRUE), #,width = '1000px'),
                                                           
              conditionalPanel(
                condition = "input.simple_vs_multiple == 'SLR'",
                                                           
                radioButtons(
                  inputId      = "dataRegCor",
                  label        = strong("Data"),
                  choiceValues = list("Enter Raw Data", 
                                      "Upload Data"),
                  choiceNames  = list("Enter Raw Data", 
                                      "Upload Data"),
                  selected     = "Enter Raw Data", #character(0), #
                  inline       = TRUE), #,width = '1000px'),
                                                           
                conditionalPanel(
                  condition = "input.dataRegCor == 'Enter Raw Data'",
                                                             
                  textAreaInput(
                    inputId     = "x", 
                    label       = strong("\\( x\\) (Independent Variable)"), 
                    value       = "10, 13, 18, 19, 22, 24, 27, 29, 35, 38", 
                    placeholder = "Enter values separated by a comma with decimals as points", 
                    rows        = 3
                  ),
                    
                  textAreaInput(
                    inputId     = "y", 
                    label       = strong("\\( y\\) (Dependent Variable)"), 
                    value       = "66, 108, 161, 177, 228, 235, 268, 259, 275, 278", 
                    placeholder = "Enter values separated by a comma with decimals as points", 
                    rows        = 3
                  ),
                                                               
                  # textAreaInput("x", label = strong("\\( x\\) (Independent Variable)"), value = "635, 644, 711, 708, 836, 820, 810, 870, 856, 923", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                  # textAreaInput("y", label = strong("\\( y\\) (Dependent Variable)"), value = "100, 93, 88, 84, 77, 75, 74, 63, 57, 55", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                                                           
                  # textAreaInput("x", label = strong("\\( x\\) (Independent Variable)"), value = "87, 92, 100, 103, 107, 110, 112, 127", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                  # textAreaInput("y", label = strong("\\( y\\) (Dependent Variable)"), value = "39, 47, 60, 50, 60, 65, 115, 118", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                                                             
                  # textAreaInput("x", label = strong("\\( x\\) (Independent Variable)"), value = "61, 111, 125, 134, 169, 173, 244", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                  # textAreaInput("y", label = strong("\\( y\\) (Dependent Variable)"), value = "4, 14, 15, 18, 21, 26, 38", placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                ), # Raw Data
                                                             
                conditionalPanel(
                  condition = "input.dataRegCor == 'Upload Data'",
                                                               
                  fileInput(
                    inputId = "slrUserData", 
                    label   = strong("Upload your data (.csv or .xls or .xlsx or .txt)"), 
                    accept  = c("text/csv",
                                "text/comma-separated-values", 
                                "text/plain", 
                                ".csv",
                                ".xls",
                                ".xlsx")
                  ),
                                                               
                  selectizeInput(
                    inputId = "slrExplanatory",
                    label   = strong("Choose the Explanatory Variable (x)"),
                    choices = c(""),
                    options = list(placeholder = 'Select a variable',
                                   onInitialize = I('function() { this.setValue(""); }'))
                  ),
                                                               
                  selectizeInput(
                    inputId = "slrResponse",
                    label   = strong("Choose the Response Variable (y)"),
                    choices = c(""),
                    options = list(placeholder = 'Select a variable',
                                   onInitialize = I('function() { this.setValue(""); }'))
                  ),
                ), # Upload Data
                                                           
                br(),
                p(strong("Graph Options")),
                hr(),
                                                             
                checkboxInput(
                  inputId = "scatterPlot", 
                  label   = "Scatterplot of \\( x\\) versus \\( y\\)", 
                  value   = TRUE
                ),
                                                             
                br(),
              ), # SLR
                                                           
              conditionalPanel(
                condition = "input.simple_vs_multiple == 'MLR'",
                                                             
                fileInput(
                  inputId = "headerfileMLR", 
                  label   = "Upload data",
                  accept  = c("text/csv",
                              "text/comma-separated-values",
                              "text/tab-separated-values",
                              "text/plain",
                              ".csv",
                              ".txt",
                              ".xls",
                              ".xlsx")
                )
              ), # MLR
                                                           
              actionButton(
                inputId = "goRegression", 
                label = "Calculate",
                class = "act-btn"
                # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ),
              actionButton(
                inputId = "resetRegCor", 
                label = "Reset Values",
                class = "act-btn"
                # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
              ) #, onclick = "history.go(0)"
            ), #RegCorPanel
            # br(),
            # downloadButton('describe_download', "Download Report", class="butt" ), br(),
            # tags$head(tags$style(".butt{background-color:#337ab7;} .butt{color:#fff;}")), br(),
            # radioButtons('format', 'Document format', c('PDF', 'Word'), inline = TRUE),
            
          ), #SidebarPanel
          
          # tags$img(src = "./www/app-store-logo.svg", width = "99px")

                                        
          #  --------------------------- #  
          ## ---- Methods mainPanel ---- 
          #  --------------------------- #
          mainPanel(
                                          
            #   -------------------------------- #  
            ### ---- Descriptive Stats main ---- 
            #   -------------------------------- #
            div(id = "descriptiveStatsMP",
                                              
              conditionalPanel(
                condition = "input.dropDownMenu == 'Descriptive Statistics'",
                style     = "display: none;",
                                                
                uiOutput('renderDescrStats'),
                                                
                div(id = "descrStatsData",
                    
                  tabsetPanel(
                    id       = "dsTabset", 
                    selected = "Descriptive Statistics",
                                                                
                    tabPanel(
                      id    = "dsTable", 
                      title = "Descriptive Statistics", 
                      value = 'Descriptive Statistics',

                      withMathJax(),
                        
                      uiOutput('dsDataTable'),
                                                                         
                      br(),
                                                                         
                      conditionalPanel(
                        condition = "input.dsTableFilters.indexOf('First Quartile (Q1)') > -1 | 
                                    input.dsTableFilters.indexOf('Third Quartile (Q3)') > -1 |
                                    input.dsTableFilters.indexOf('IQR') > -1 | 
                                    input.dsTableFilters.indexOf('Potential Outliers') > -1",
                                                                           
                        helpText("* Note: Quartiles are calculated by excluding the median on both sides."),
                      ),
                                                                         
                      br(),
                      br(),
                                                                         
                      conditionalPanel(
                        condition = "input.dsTableFilters.indexOf('Mean') > -1 | 
                                    input.dsTableFilters.indexOf('Sample Standard Deviation') > -1",
                                                                           
                        fluidRow(
                                                                             
                          column(width = 4,
                            br(),
                            DTOutput('sampleDataTable'),
                            br(),
                            br()
                          ),
                                                                             
                          column(width = 8,
                            conditionalPanel(
                              condition = "input.dsTableFilters.indexOf('Mean') > -1",
                              
                              withMathJax(),
                              titlePanel(tags$u('Sample Mean')),
                              br(),
                              uiOutput('dsMeanCalc'),
                              br()
                            ),
                                                                                    
                            conditionalPanel(
                              condition = "input.dsTableFilters.indexOf('Sample Standard Deviation') > -1",
                              
                              withMathJax(),
                              titlePanel(tags$u('Sample Standard Deviation')),
                              br(),
                              uiOutput('dsSDCalc'),
                              br(),
                              br(),
                              br()
                            )
                          ),
                        ),
                      ),
                    ),# dsTable tabPanel
                                                                
                    tabPanel(
                      id    = "dsGraphs", 
                      title = "Graphs", 
                      value = 'Graphs',

                      conditionalPanel(
                        condition = "input.dsGraphOptions.indexOf('Boxplot') > -1",
                                                  
                        h3("Boxplot"),
                        br(),
                        plotOptionsMenuUI(
                          id = "dsBoxplot", 
                          plotType = "Boxplot",
                          title = "Boxplot"),                         
                        uiOutput("renderDSBoxplot")
                      ), # Boxplot
                                                                         
                      conditionalPanel(
                        condition = "input.dsGraphOptions.indexOf('Histogram') > -1",
                                                                           
                        h3("Histogram"),
                        br(),
                                                                           
                        plotOptionsMenuUI(id = "dsHisto",
                                          title = "Histogram"),
                        uiOutput("renderDSHistogram"),
                      ), # Histogram
                                                                         
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
                      ), # Stem and Leaf
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
                style     = "display: none;",
                                                                
                conditionalPanel(
                  condition = "input.probability == 'Contingency Table'",
 
                  conditionalPanel(
                    condition = "input.cTableDimension == '2 x 2'",
                                                                   
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Frequency Distribution'",
                                                                     
                      uiOutput("render2x2cTable"),
                      br(),
                    # ), # 2x2 Frequency Distribution
                                                                   
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Probability Distribution'",
                    #                                                  
                    #   uiOutput("render2x2pTable"),
                    #   br(),
                    # ), # 2x2 Probability Distribution
                  ), # cTableDimension == 2 x 2
                                                                 
                  conditionalPanel(
                    condition = "input.cTableDimension == '2 x 3'",
                                                                   
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Frequency Distribution'",
                                                                     
                      uiOutput("render2x3cTable"),
                      br(),
                    # ), # 2x3 Frequency Distribution
                    #                                                
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Probability Distribution'",
                    #                                                  
                    #   uiOutput("render2x3pTable"),
                    #   br(),
                    # ), # 2x3 Probability Distribution
                  ), # cTableDimension == 2 x 3
                                                                 
                  conditionalPanel(
                    condition = "input.cTableDimension == '3 x 2'",
                                                                   
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Frequency Distribution'",
                                                                     
                      uiOutput("render3x2cTable"),
                      br(),
                    # ), # 3x2 Frequency Distribution
                                                                   
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Probability Distribution'",
                    #                                                  
                    #   uiOutput("render3x2pTable"),
                    #   br(),
                    # ), # 3x2 Probability Distribution
                  ), # cTableDimension == 3 x 2
                
                  conditionalPanel(
                    condition = "input.cTableDimension == '3 x 3'",
                                                                   
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Frequency Distribution'",
                                                                     
                      uiOutput("render3x3cTable"),
                      br(),
                    # ), # 3x3 Frequency Distribution
                                                                   
                    # conditionalPanel(
                    #   condition = "input.cTableType == 'Probability Distribution'",
                    #                                                  
                    #   uiOutput("render3x3pTable"),
                    #   br(),
                    # ), # 3x3 Probability Distribution
                  ), # cTableDimension == 3 x 3
                
                  # conditionalPanel(
                  #   condition = "input.cTableDimension == 'Other'",
                  # 
                  #   uiOutput("renderOthercTable"),
                  #   br(),
                  # )
                                                                 
                  # conditionalPanel(
                  #   condition = "input.cTableType == 'Frequency Distribution'",
                                                                   
                    conditionalPanel(
                      condition = "input.cTableProb == 'Marginal'",
                                                                     
                      uiOutput('renderMarginalProbs')
                    ), # Marginal
                                                                   
                    conditionalPanel(
                      condition = "input.cTableProb == 'Joint'",
                                                                     
                      uiOutput('renderJointProbs')
                    ), # Joint
                                                                   
                    conditionalPanel(
                      condition = "input.cTableProb == 'Union'",
                                                                   
                      uiOutput('renderUnionProbs', width = '960px')
                    ), # Union
                                                                   
                    conditionalPanel(
                      condition = "input.cTableProb == 'Conditional'",
                                                                     
                      uiOutput('renderConditionalProbs')
                    ) # Conditional
                  # ),
                ), # Contingency Table
                  
                                              
                conditionalPanel(
                  condition = "input.probability == 'Binomial'",
                                                                
                  br(),
                  uiOutput("renderProbabilityBinom"),
                  br(),
                ), # Binomial
                     
                                           
                conditionalPanel(
                  condition = "input.probability == 'Poisson'",
                                                                
                  br(),
                  uiOutput("renderProbabilityPoisson"),
                  br(),
                ), # Poisson
                
                                                
                conditionalPanel(
                  condition = "input.probability == 'Normal'",
                                                                
                  br(),
                                                                 
                  conditionalPanel(
                    condition = "input.calcQuantiles == 'Probability'",
                                                                   
                    conditionalPanel(
                      condition = "input.sampMeanDistr == 0",
                                                                     
                      uiOutput("renderProbabilityNorm")
                    ),
                                                                   
                    conditionalPanel(
                      condition = "input.sampMeanDistr == 1",
                                                                   
                      uiOutput("renderSampMeanDistr")
                    ),
                  ), # Probability
                                                                 
                  conditionalPanel(
                    condition = "input.calcQuantiles == 'Quantile'",
                                                                   
                    conditionalPanel(
                      condition = "input.calcQuartiles == 'Quartiles'",
                                                                     
                      uiOutput("renderNormQuartiles")
                    ), # Quartiles
                                                                   
                    conditionalPanel(
                      condition = "input.calcQuartiles == 'Percentile'",
                                                                     
                      uiOutput("renderNormPercentile")
                    ) # Percentile
                  ), # Quantile
                  br(),
                ) # Normal
              ) # Probability Distributions
            ), #Probability MainPanel 
                                          
                                          
            #   ------------------------------------ #  
            ### --- Sample Size Estimation main ---- 
            #   ------------------------------------ #
            div(id = "ssEstimationMP",
              
              conditionalPanel(
                condition = "input.dropDownMenu == 'Sample Size Estimation'",
                style     = "display: none;",
                                              
                uiOutput("ssEstimationValidation"),
                                              
                div(id = "ssEstimationData",
                                              
                  conditionalPanel( #### Samp Size Mean Est ----
                    condition = "input.sampSizeEstParameter == 'Population Mean'",
                                                                
                    titlePanel(tags$u("Sample Size Estimate (\\( n \\))")),
                    br(),
                    uiOutput('sampSizeMeanEstimate'),
                    br(),
                  ), # Population Mean
                                              
                  conditionalPanel( #### Samp Size Prop Est ----
                    condition = "input.sampSizeEstParameter == 'Population Proportion'",
                                                                
                    titlePanel(tags$u("Sample Size Estimate (\\( n \\))")),
                    br(),
                    uiOutput('sampSizePropEstimate'),
                    br(),
                  ), # Population Proportion
                ) # SSEstimationData
              ) # Sample Size Estimation
            ), # Sample Size Est Main Panel
                                          
                                          
            #   ------------------------------------ #  
            ### ---- Statistical Inference main ---- 
            #   ------------------------------------ #
            div(id = "inferenceMP",
                                              
              conditionalPanel(
                condition = "input.dropDownMenu == 'Statistical Inference'",
                style     = "display: none;",
                                              
                uiOutput("inferenceValidation"),
                                                
                div(id = "inferenceData",
                                                    
                  conditionalPanel( #### One samp ----
                    condition = "input.siMethod == '1'",
                                                                      
                    conditionalPanel( ##### Pop Mean ----
                      condition = "input.popuParameter == 'Population Mean'",
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType == 'Confidence Interval'",
                                                                                          
                        titlePanel(tags$u("Confidence Interval")),
                        br(),
                        uiOutput('oneMeanCI'),
                        br(),
                      ), # Confidence Interval
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType == 'Hypothesis Testing'",
                                                                                          
                        titlePanel(tags$u("Hypothesis Test")),
                        br(),
                        uiOutput('oneMeanHT'),
                        br(),
                      ), # Hypothesis Testing
                                                                                        
                      conditionalPanel(
                        condition = "input.dataAvailability != 'Summarized Data' && input.oneMeanBoxplot == 1",
                                                                                          
                        br(),
                        hr(),
                        br(),
                        titlePanel(tags$u("Boxplot")),
                        br(),
                        plotOptionsMenuUI(
                          id = "oneMeanBoxplot",
                          plotType = "Boxplot",
                          title = "Boxplot"
                        ),
                        uiOutput("renderOneMeanBoxplot"),
                        br(),
                        br()
                      )
                    ), # One Population Mean
                                                                      
                    conditionalPanel( ##### Pop Prop ----
                      condition = "input.popuParameter == 'Population Proportion'",
      
                      conditionalPanel(
                        condition = "input.inferenceType == 'Confidence Interval'",
                                                                                          
                        titlePanel(tags$u("Confidence Interval")),
                        br(),
                        uiOutput('onePropCI'),
                        br(),
                      ), # Confidence Interval
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType == 'Hypothesis Testing'",
                                                                                        
                        titlePanel(tags$u("Hypothesis Test")),
                        br(),
                        uiOutput('onePropHT'),
                        br(),
                      ), # Hypothesis Testing
                    ), # One Population Proportion
                  ), # "input.siMethod == '1'"
                                                    
                  conditionalPanel( #### Two Samp ----
                    condition = "input.siMethod == '2'",
                                                                      
                    conditionalPanel( ##### Ind Pop Means ----
                      condition = "input.popuParameters == 'Independent Population Means'",
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType2 == 'Confidence Interval'",
                                                                                        
                        titlePanel(tags$u("Confidence Interval")),
                        br(),
                        uiOutput('indMeansCI'),
                        br(),
                      ), # Confidence interval
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                                                                          
                        titlePanel(tags$u("Hypothesis Test")),
                        br(),
                        uiOutput('indMeansHT'),
                        br()
                      ), # Hypothesis Testing
                                                                                        
                      conditionalPanel(
                        condition = "input.dataAvailability2 != 'Summarized Data' && input.indMeansBoxplot == 1",
                                                                                          
                        br(),
                        hr(),
                        br(),
                        titlePanel(tags$u("Boxplot")),
                        br(),
                        plotOptionsMenuUI(
                          id = "indMeansBoxplot", 
                          plotType = "Boxplot",
                          title = "Boxplot"),
                        uiOutput("renderIndMeansBoxplot"),
                        br(),
                        br()
                      )
                    ), # Two Independent Samples
                                                                      
                    #-------------#
                    # PAIRED DATA #
                    #-------------#
                    conditionalPanel( ##### Dep Pop Means ----
                      condition = "input.popuParameters == 'Dependent Population Means'",
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType2 == 'Confidence Interval'",
                                                                                          
                        titlePanel(tags$u("Confidence Interval")),
                        br(),
                        uiOutput('depMeansCI'),
                        br(),
                      ), # CI
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                                                                          
                        titlePanel(tags$u("Hypothesis Test")),
                        br(),
                        uiOutput('depMeansHT'),
                        br()
                      ), # HT

                      hr(),
                      br(),
                                                                                        
                      titlePanel(tags$u("Data")),
                      br(),
                      
                      fluidRow(
                        column(width = 8, 
                          uiOutput('depMeansTable'),
                        ),
                        
                        column(width = 4, 
                          br(),
                        )
                      ),
                      br(),
                      br(),
                    ), # Two Dependent Samples
                                                                      
                    #----------------------------#
                    # TWO POPULATION PROPORTIONS #
                    #----------------------------#
                    conditionalPanel( ##### Pop Props ----
                      condition = "input.popuParameters == 'Population Proportions'",
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType2 == 'Confidence Interval'",
                                                                                          
                        titlePanel(tags$u("Confidence Interval")),
                        br(),
                        uiOutput('twoPropCI'),
                        br(),
                      ), # Confidence Interval
                                                                                        
                      conditionalPanel(
                        condition = "input.inferenceType2 == 'Hypothesis Testing'",
                                                                                          
                        titlePanel(tags$u("Hypothesis Test")),
                        br(),
                        uiOutput('twoPropHT'),
                        br(),
                      ), # Hypothesis Testing
                    ), # Two Population Proportions
                  ), # "input.siMethod == '2'"
                  
                  conditionalPanel( #### ANOVA ----
                    condition = "input.siMethod == 'Multiple'",
                    
                    tabsetPanel(
                      id       = "anovaTabset", 
                      selected = "Analysis",
                      
                      tabPanel(
                        id    = "anova", 
                        title = "Analysis",
                        
                        titlePanel("One-way Analysis of Variance (ANOVA)"),
                        hr(),
                        br(),
                        uiOutput("anovaOutput"),
                        br(),
                        br(),
                        conditionalPanel(
                          condition = "input.anovaOptions.indexOf('posthoc') > -1",
                          
                          uiOutput('anovaPosthocAnalysis')
                        )
                      ),
                      
                      tabPanel(
                        title = "Graphs",
                        
                        conditionalPanel(
                          condition = "input.anovaGraphs.indexOf('Side-by-side Boxplot') > -1",
                          
                          titlePanel("Side-by-side Boxplot"),
                          br(),
                          br(),
                          plotOptionsMenuUI(
                            id = "anovaBoxplot", 
                            plotType = "Boxplot",
                            title = "Side-by-Side Boxplot"),
                          uiOutput("renderAnovaBoxplot")
                        ),
                        
                        conditionalPanel(
                          condition = "input.anovaGraphs.indexOf('Histogram of Residuals') > -1",
                          
                          titlePanel("Histogram of Residuals"),
                          br(),
                          br(),
                          plotOptionsMenuUI(
                            id = "anovaHistogram",
                            title = "Histogram of Residuals",
                            xlab = "Residuals",
                            ylab = "Frequency"),
                          uiOutput("renderAnovaHistogram")
                        ),
                        
                        conditionalPanel(
                          condition = "input.anovaGraphs.indexOf('QQ Plot of Residuals') > -1",
                          
                          titlePanel("QQ Plot of Residuals"),
                          br(),
                          br(),
                          plotOptionsMenuUI(
                            id = "anovaQQplot",
                            title = "QQ Plot of Residuals",
                            xlab = "Normal Quantiles",
                            ylab = "Residuals",
                            colour = "#0F3345"),
                          uiOutput("renderAnovaQQplot")
                        ),
                        
                        conditionalPanel(
                          condition = "input.anovaGraphs.indexOf('Plot Group Means') > -1",
                          
                          titlePanel("Group Means"),
                          br(),
                          br(),
                          plotOptionsMenuUI(
                            id = "anovaMeanPlot",
                            title = "Group Means",
                            xlab = "Group",
                            ylab = "Mean",
                            colour = "#0F3345"),
                          uiOutput("renderAnovaMeanPlot")
                        )
                      ),
                      
                      tabPanel(
                        id    = "anovaData",
                        title = "Uploaded Data",
                        
                        uiOutput("renderAnovaDataView")
                      )
                    )
                  ),
                  
                  conditionalPanel( #### Chi-Square ----
                    condition = "input.siMethod == 'Categorical'",
                                    
                    conditionalPanel( ##### Chi-Square Test for Independence ----
                      condition = "input.chisquareMethod == 'Chi-Square'",  
                      titlePanel("Chi-Square Test for Independence"),
                      hr(),
                      br(),
                                    # accordion(
                                      #   accordion_panel(
                                      #     title = "Observed Frequencies", 
                                      #     uiOutput("renderChiSqObs"),
                                      #     br(),
                                      #     br(),
                                      #   ),
                                      #   
                                      #   accordion_panel(
                                      #     title = "Expected Frequencies",
                                      #     uiOutput("renderChiSqExp"),
                                      #     br(),
                                      #     br(),
                                      #   ),
                                      #   
                                      #   accordion_panel(
                                      #     title = "Calculation of the \\( \\chi^2 \\) statistic value",
                                      #     uiOutput("renderChiSqResults"),
                                      #     br(),
                                      #   ),
                                      #   
                                      #   accordion_panel(
                                      #     title = "Chi-Square Test",
                                      #     uiOutput("chiSqTest"),
                                      #     br(),
                                      #     br(),
                                      #   ),
                                      #   
                                      #   open = TRUE
                                      # ),
                      h4("Observed Frequencies"),
                      br(),
                      uiOutput("renderChiSqObs"),
                      br(),
                      br(),
                      h4("Expected Frequencies"),
                      br(),
                      uiOutput("renderChiSqExp"),
                      br(),
                      br(),
                      h4("Calculation of the \\( \\chi^2 \\) statistic value"),
                      br(),
                      uiOutput("renderChiSqResults"),
                      br(),
                      hr(),
                      br(),
                      uiOutput("chiSqTest"),
                      br(),
                      br(),
                    ),
                    
                    conditionalPanel( #### Fisher's Exact Test ----
                      condition = "input.chisquareMethod == 'Fisher'",
                      
                      titlePanel("Fisher's Exact Test"),
                      hr(),
                      br(),
                      h4("Observed Frequencies"),
                      uiOutput("renderFishersObs"),
                      br(),
                      br(),
                      hr(),
                      br(),
                      h4("Hypothesis Test"),
                      uiOutput("fishersTest"),
                      br(),
                      br(),
                    )
                  ) # input.siMethod == 'Categorical'
                )# Inference Data
              )
            ), # inferenceMP
                                          
            #   ----------------------------------------- #  
            ### ---- Regression and Correlation main ---- 
            #   ----------------------------------------- #
            div(id = "RegCorMP",
                                              
              conditionalPanel(
                condition = "input.dropDownMenu == 'Regression and Correlation'",
                style     = "display: none;",
                                              
                conditionalPanel(
                  condition = "input.simple_vs_multiple == 'SLR'",
                                                  
                  uiOutput("slrValidation"),
                                                  
                  div(id = "SLRData",
                    tabsetPanel(
                      id       = "slrTabset", 
                      selected = "Simple Linear Regression",
                                                                  
                      tabPanel(
                        id    = "slr", 
                        title = "Simple Linear Regression",
                                                                           
                        conditionalPanel(
                          condition = "input.scatterPlot == 1",
                                                                             
                          titlePanel("Scatterplot"),
                          br(),
                          plotOptionsMenuUI(
                            id = "slrScatter", 
                            plotType = "Scatterplot",
                            title = "Scatterplot",
                            xlab = "x",
                            ylab = "y",
                            dim = "in px",
                            includeFlip = FALSE),
                          uiOutput("renderSLRScatterplot"),
                          br(),
                          hr(),
                        ), # Scatterplot
                                                                           
                        titlePanel("Data"),
                        br(),
                        DTOutput("slrDataTable", width = "750px"),
                        br(),
                        hr(),
                                                                           
                        titlePanel("Estimated equation of the regression line"),
                        br(),
                        uiOutput('regLineEquation'),
                        # verbatimTextOutput("linearRegression"),
                        # br(),
                        # hr(),
                        #                                                    
                        # titlePanel("95% confidence interval for regression parameters"),
                        # br(),
                        # verbatimTextOutput("confintLinReg"),
                        # br(),
                        # hr(),
                        #                                                    
                        # titlePanel("ANOVA for regression"),
                        # br(),
                        # verbatimTextOutput("anovaLinReg"),
                        #br(),
                      ), # slr tabpanel
                                                                  
                      # tabPanel(id = "normality", title = "Normality of Residuals",
                      #          
                      #          #----------------------------------#
                      #          # Tests for normality of residuals #
                      #          #----------------------------------#
                      #          titlePanel("Anderson-Darling test"),
                      #          verbatimTextOutput("AndersonDarlingTest"),
                      #          br(),
                      #          
                      #          titlePanel("Kolmogorov-Smirnov test"),
                      #          verbatimTextOutput("KolmogorovSmirnovTest"),
                      #          br(),
                      #          
                      #          titlePanel("Shapiro-Wilk test"),
                      #          verbatimTextOutput("ShapiroTest"),
                      #          #br(),
                      # ),
                                                                
                      # tabPanel(id = "resid", title = "Residual Plots",
                      #          #-----------------------------#
                      #          # Plots for Residual Analysis #
                      #          #-----------------------------#
                      #          titlePanel("Q-Q plot"),
                      #          plotOutput("qqplot", width = "500px"),
                      #          #br(),
                      #          
                      #          titlePanel("Other diagnostic plots"),
                      #          plotOutput("moreplots", width = "500px"),
                      #          #br(),
                      # ),
                      
                      #----------------------------------#
                      # Correlation Coefficient Analysis #
                      #----------------------------------#                                            
                      tabPanel(
                        id    = "correlation", 
                        title = "Correlation Analysis",

                        titlePanel("Pearson's Product-Moment Correlation"),
                        br(),
                        br(),
                        uiOutput('pearsonCorFormula'),
                        br(),
                        # verbatimTextOutput("PearsonCorTest"),
                        # br(),
                        # verbatimTextOutput("PearsonConfInt"),
                        # br(),
                        hr(),
                                                                           
                        titlePanel("Kendall's Rank Correlation"),
                        br(),
                        uiOutput("kendallEstimate"),
                        br(),
                        hr(),
                                                                         
                        titlePanel("Spearman's Rank Correlation"),
                        br(),
                        uiOutput("spearmanEstimate"),
                        br(),
                        br()
                      ), # correlation tabpanel
                                                                  
                      tabPanel(
                        id    = "slrDataFile", 
                        title = "Uploaded Data", 
                        value = "Uploaded Data",
                                                                           
                        titlePanel("Data File"),
                        br(),
                        br(),
                        div(DTOutput("slrViewUpload"), style = "width: 75%"),
                        br(),
                        br(),
                      ), # slrDataFile tabpanel
                    ),
                  ),
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
        br(),
        br(),
        tags$a(
          href="https://apps.apple.com/us/app/cougarstats/id6476070179", 
          target = "_blank",
          tags$img(src="AppStoreLogo.svg", 
                   title="App Store Link", 
                   width="150px")
        )      ), # Methods Panel
                             
      # --------------------- #  
      # ---- Authors Tab ---- 
      # --------------------- #
      tabPanel("Authors",
        h3("Development Team", style= "font-weight:bold"),
                                      
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
                                      
        p(span("Michael Myer", style= "font-weight:bold")),
        p("Senior Developer"),
        #p(span("Lead Developer", style = "font-weight:bold")),
        p("Undergraduate Student, Mount Royal University,"),
        p("Calgary, AB, CANADA"),
                                      
        br(),
        
        p(span("Samantha Brian", style= "font-weight:bold")),
        p("Developer"),
        #p(span("Lead Developer", style = "font-weight:bold")),
        p("Undergraduate Student, Mount Royal University,"),
        p("Calgary, AB, CANADA"),
        
        br(),
        
        p(span("Michael Walsh", style= "font-weight:bold")),
        p("Developer"),
        #p(span("Lead Developer", style = "font-weight:bold")),
        p("Undergraduate Student, Mount Royal University,"),
        p("Calgary, AB, CANADA"),
        
        br(),
                                      
        p("Acknowledgement: In Fall 2022 an earlier version of this interactive R Shiny app was presented as Crystal Wai's 
          COMP 5690: Senior Computer Science Project. From June - August 2023 this project was funded by a student research 
          grant (awarded to Michael Myer) conferred by the Faculty of Science and Technology at Mount Royal University. 
          Starting September 2023 this project is funded by a Provost's Teaching-Learning Enhancement Grant (TLEG). 
          Michael Myer is currently hired as a Project Assistant through this grant."), 
        br(),
                                      
        p("This interactive R Shiny app is maintained by Dr. Ashok Krishnamurthy. We welcome questions, insights, and feedback."),
                                      
        hr(),
                                      
        h5("Built with",
          img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "35px"),
          "by",
          img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo.png", height = "35px"),
          "."
        )
      ) # Authors Panel
    ) #navbarPage
  )#UI 
