library(shiny)
library(shinythemes)
library(moments)
library(ggplot2)
library(shinyjs)
library(shinyvalidate)
library(dplyr)

render <- "
{
  option: function(data, escape){return '<div class=\"option\">'+data.label+'</div>';},
  item: function(data, escape){return '<div class=\"item\">'+data.label+'</div>';}
}"

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Intro to Stats"),
                
                sidebarLayout(
                  #shinyjs::useShinyjs(),
                  sidebarPanel(
                    withMathJax(),
                    shinyjs::useShinyjs(),
                    id = "sideBar", 
                    selectInput(
                      inputId= "dropDownMenu",
                      label= "Choose your test",
                      choices = c("Descriptive Statistics", "Probability", "Inference"),
                    ),
                    conditionalPanel(
                      condition = "input.dropDownMenu == 'Descriptive Statistics'",
                      textAreaInput("descriptiveStat", "Sample", value = "2.14, 2.09, 2.65, 3.56, 5.55, 5.00, 5.55, 3.09, 6.79", placeholder = "Enter values separated by a comma with decimals as points", rows = 6),
                      #checkboxGroupInput(inputId = "checkBoxDescpStats", "Select", selected = c("Mean", "Mode","Median"),choices = c("Mean","Mode","Median","Standard Deviation","Interquartile Range","Box Plot")),
                      actionButton(inputId = "goDescpStats", "Calculate",
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      actionButton("resetAll","Reset Values",
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    ),
                    conditionalPanel(
                      id = "probPanel",
                      condition = "input.dropDownMenu == 'Probability'",
                      radioButtons("probability", "Distribution", choices = c("Binomial", "Poisson", "Normal"), selected = NULL, inline = TRUE),
                      conditionalPanel(
                        condition = "input.probability == 'Binomial'",
                        
                        numericInput(inputId = "numTrailsBinom", 
                                     label = "Number of Trails (n):",
                                     value = 10, min = 1, step = 1),
                        
                        numericInput(inputId = "successProbBinom", 
                                     label = "Probability of Success (p):",
                                     value = 0.5, min = 0, max = 1, step = 0.00001),
                        
                        numericInput(inputId = "numSuccessesBinom", 
                                     label = "Number of Successes (x):",
                                     value = 0, min = 0, step = 1),
                        # checkboxGroupInput(inputId = "calcBinom", 
                        #                    label = "",
                        #                    choiceValues = list("P(X=x)","P(X \\leq x)\\)","P(X>x)"),
                        #                    choiceNames = list("P(X=x)","P(X \\leq x)\\)","P(X>x)")),
                        checkboxGroupInput(inputId = "calcBinom", 
                                           label = "",
                                           choiceValues = list("exact","cumulative","P(X>x)"),
                                           choiceNames = list("P(X=x)","\\(P(X \\leq x)\\)","P(X>x)")),
                        actionButton(inputId = "goBinom", "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetAllB","Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      ),
                      
                      conditionalPanel(
                        condition = "input.probability == 'Poisson'", 
                        numericInput("muPoisson", "Average (mu)", value = ""),  # CHANGE THIS TO A NUMERIC INPUT
                        numericInput("xPoisson", "Number of Successes (x)", value = ""), # CHANGE THIS TO A NUMERIC INPUT
                        checkboxGroupInput(inputId = "calcPoisson",
                                           label = "", 
                                           choiceValues = list("lowerTail", "upperTail","interval"),
                                           choiceNames = list("\\P(X \\leq x)\\)","\\P(X > x)\\)","\\P(a \\leq X \\leq b)\\)")
                                           ),
                        conditionalPanel(
                          condition = "input.calcPoisson = 'interval'",
                          numericInput("aPoisson", "a:",
                                       value = 6, min = 0, step = 1
                          ),
                          numericInput("bPoisson", "b: \\( (a \\leq b) \\)",
                                       value = 10, min = 0, step = 1
                          )
                        ),
                        actionButton(inputId = "goPoisson", "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetAll","Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        
                      ),
                      
                      conditionalPanel(
                        withMathJax(),
                        condition = "input.probability == 'Normal'", 
                        numericInput(inputId = "popMean", 
                                     label = "Population Mean (mu):", 
                                     value = 0, step = 0.00001),
                        numericInput(inputId = "popSD",
                                     label = "Population Standard Deviation (sigma):",
                                     value = 1, min = 0, step = 0.00001), 
                        numericInput(inputId = "xValue",
                                     label = "x:",
                                     value = 0, step = 0.00001),
                        checkboxGroupInput(inputId = "calcNormal",
                                           label = "", 
                                           choiceValues = list("P(X \\leq x)", "P(X > x)"),
                                           choiceNames = list("\\P(X \\leq x)\\)", "P(X > x)")
                        ),
                        actionButton(inputId = "goNormal", "Calculate",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        actionButton("resetAll","Reset Values",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.dropDownMenu == 'Inference'",
                      
                      radioButtons(inputId = "samplesSelect",
                                   label = strong("Number of samples"),
                                   choiceValues = list("1","2"),
                                   choiceNames = list("1","2"),
                                   selected = "1", #character(0), #
                                   inline = TRUE,
                                   width = "1000px"),
                      
                      radioButtons(inputId = "dataAvailability",
                                   label = strong("Data Availability"),
                                   choiceValues = list("Summarized Data","Enter Raw Data"),
                                   choiceNames = list("Summarized Data","Enter Raw Data"),
                                   selected = character(0), # "Summarized Data", #
                                   inline = TRUE,
                                   width = "1000px"),
                      
                      conditionalPanel(
                        condition = "input.samplesSelect == '1' && input.dataAvailability == 'Summarized Data'",
                        
                        numericInput(inputId = "sampleSize",
                                     label = "Sample Size (n):",
                                     value = 0, min = 0, step = 1),
                        
                        numericInput(inputId = "sampleMean",
                                     label = "Sample Mean:",
                                     value = 0, step = 0.00001),
                        
                        radioButtons(inputId = "sigmaKnown",
                                     label = strong("Population Standard Deviation"),
                                     choiceValues = list("Known","Unknown"),
                                     choiceNames = list("Known","Unknown"),
                                     selected = "Known", #character(0), #
                                     inline = TRUE,
                                     width = "1000px"),
                        
                        conditionalPanel(
                          condition = "input.sigmaKnown == 'Known'",
                          
                          numericInput(inputId = "popuSD",
                                       label = "Population Standard Deviation Value:",
                                       value = 0, min = 0, step = 0.00001)),
                        
                        conditionalPanel(
                          condition = "input.sigmaKnown == 'Unknown'",
                          
                          numericInput(inputId = "sampSD",
                                       label = "Sample Standard Deviation Value:",
                                       value = 0, min = 0, step = 0.00001)),
                      ),
                      
                      conditionalPanel(
                        condition = "input.samplesSelect == '2' && input.dataAvailability == 'Summarized Data'",
                        
                        numericInput(inputId = "sampleSize1",
                                     label = "Sample Size 1 (n1):",
                                     value = 0, min = 0, step = 1),
                        
                        numericInput(inputId = "sampleMean1",
                                     label = "Sample Mean 1:",
                                     value = 0, step = 0.00001),
                        
                        numericInput(inputId = "sampleSize2",
                                     label = "Sample Size 2 (n2):",
                                     value = 0, min = 0, step = 1),
                        
                        numericInput(inputId = "sampleMean2",
                                     label = "Sample Mean 2:",
                                     value = 0, step = 0.00001),
                        
                        radioButtons(inputId = "bothsigmaKnown",
                                     label = strong("Population Standard Deviations"),
                                     choiceValues = list("bothKnown","bothUnknown"),
                                     choiceNames = list("bothKnown","bothUnknown"),
                                     selected = "bothKnown",
                                     inline = TRUE,
                                     width = "1000px"),
                        
                        conditionalPanel(
                          condition = "input.bothsigmaKnown == 'bothKnown'",
                          
                          numericInput(inputId = "popuSD1",
                                       label = "Population Standard Deviation 1 Value:",
                                       value = 0, min = 0, step = 0.00001),
                          
                          numericInput(inputId = "popuSD2",
                                       label = "Population Standard Deviation 2 Value:",
                                       value = 0, min = 0, step = 0.00001),
                        ),
                        
                        conditionalPanel(
                          condition = "input.bothsigmaKnown == 'bothUnknown'",
                          
                          numericInput(inputId = "sampSD1",
                                       label = "Sample Standard Deviation 1 Value:",
                                       value = 0, min = 0, step = 0.00001),
                          
                          numericInput(inputId = "sampSD2",
                                       label = "Sample Standard Deviation 2 Value:",
                                       value = 0, min = 0, step = 0.00001),
                        ),
                        
                        conditionalPanel(
                          condition = "input.bothsigmaKnown == 'bothUnknown'",
                          
                          radioButtons(inputId = "bothsigmaKnown",
                                       label = strong("Assume Population Variances are equal"),
                                       choiceValues = list("Yes","No"),
                                       choiceNames = list("Yes","No"),
                                       selected = "Yes", # character(0), #
                                       inline = TRUE,
                                       width = "1000px"),
                        ),
                      ),
                      
                      conditionalPanel(
                        condition = "input.samplesSelect == '1' && input.dataAvailability == 'Enter Raw Data'",
                        
                        textAreaInput("sample1", "Sample 1", value = NULL, placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                      ),
                      
                      conditionalPanel(
                        condition = "input.samplesSelect == '2' && input.dataAvailability == 'Enter Raw Data'",
                        
                        textAreaInput("sample1", "Sample 1", value = NULL, placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        
                        textAreaInput("sample2", "Sample 2", value = NULL, placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        
                        radioButtons(inputId = "samplesType",
                                     label = strong("Type of Samples"),
                                     choiceValues = list("Independent Samples","Dependent Samples"),
                                     choiceNames = list("Independent Samples","Dependent Samples (Paired Data)"),
                                     selected = "Independent Samples", 
                                     inline = TRUE,
                                     width = "1000px"),
                      ),
                      
                      conditionalPanel(
                        condition = "input.dataAvailability == 'Summarized Data' || input.dataAvailability == 'Enter Raw Data'",
                        
                        radioButtons(inputId = "inferenceType",
                                     label = strong("Inference Type"),
                                     choiceValues = list("Confidence Interval","Hypothesis Testing"),
                                     choiceNames = list("Confidence Interval","Hypothesis Testing"),
                                     selected = "Confidence Interval", # character(0), #
                                     inline = TRUE,
                                     width = "1000px"),
                        
                        conditionalPanel(
                          condition = "input.inferenceType == 'Confidence Interval'",
                          
                          checkboxGroupInput(inputId = "confidenceLevel", "Confidence Level", selected = c("95%"), choices = c("90%", "95%","99%"), inline = TRUE)
                        ),
                        
                        conditionalPanel(
                          condition = "input.inferenceType == 'Hypothesis Testing'",
                          
                          checkboxGroupInput(inputId = "significanceLevel", "Significance Level", selected = c("5%"), choices = c("10%", "5%","1%"), inline = TRUE),
                        ),
                        
                        conditionalPanel(
                          condition = "input.samplesSelect == '1' && input.inferenceType == 'Hypothesis Testing'",
                          
                          numericInput(inputId = "hypMean",
                                       label = "Hypothesized Population Mean Value:",
                                       value = 0, step = 0.00001),
                          
                          selectizeInput(
                            inputId = "altHypothesis",
                            label = "Alternate Hypothesis",
                            choices = c(
                              "&le; " = 1,
                              "&ne; " = 2,
                              "&ge; " = 3
                            ),
                            selected = 2,
                            options = list(
                              render = I(render)
                            )
                          )
                        )
                      ),
                      
                      actionButton(inputId = "goInference", "Calculate",
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      actionButton("resetAll","Reset Values",
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                  ),
                  
                  mainPanel(
                    div(id="despStats",
                      conditionalPanel(
                        condition = "input.dropDownMenu == 'Descriptive Statistics'",
                        tableOutput("table"),
                        plotOutput("boxplotDespStats")
                     )
                    ),
                    div(id="probabilityMP",
                         conditionalPanel(
                           condition = "input.dropDownMenu == 'Probability'",
                           uiOutput("render_probability")
                           )

                         )
                      )
                  )
                )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Data validation
  iv <- InputValidator$new()
  
  #numTrailsBinom
  
  iv$add_rule("numTrailsBinom", sv_required())
  iv$add_rule("numTrailsBinom", sv_integer())
  iv$add_rule("numTrailsBinom", sv_gt(0))
  
  # successProbBinom
  
  iv$add_rule("successProbBinom", sv_required())
  iv$add_rule("successProbBinom", sv_gte(0))
  iv$add_rule("successProbBinom", sv_lte(1))
  
  iv$enable()
  
  #numSuccessesBinom
  
  iv$add_rule("numSuccessesBinom", sv_required())
  iv$add_rule("numSuccessesBinom", sv_integer())
  iv$add_rule("numSuccessesBinom", sv_gte(0))
  
  # String List to Numeric List
  createNumLst <- function(text) {
    text <- gsub("","", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  # 
  Modes <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
  }
  
  observeEvent(input$goDescpStats, {
    dat <- createNumLst(input$descriptiveStat)
    if(anyNA(dat) | length(dat)<2){
      "Invalid input or not enough observations"
    } else{
      dat <- createNumLst(input$descriptiveStat)
      #print(dat)
      values <- reactiveValues()
      values$df <- data.frame(Variable = character(), Value = character())
      output$table <- renderTable(values$df)
      row1 <- data.frame(Variable = "Count", Value = paste0(length(dat)))
      row2 <- data.frame(Variable = "Sum", Value = paste0(sum(dat)))
      row3 <- data.frame(Variable = "Mean", Value = paste0(round(mean(dat),4)))
      row4 <- data.frame(Variable = "Mode", Value = paste(Modes(dat)))
      row5 <- data.frame(Variable = "Q1", Value = paste0(quantile(dat, 0.25)))
      row6 <- data.frame(Variable = "Median (Q2)", Value = paste0(median(dat)))
      row7 <- data.frame(Variable = "Q3", Value = paste0(quantile(dat, 0.75)))
      row8 <- data.frame(Variable = "Minimum", Value = paste0(min(dat)))
      row9 <- data.frame(Variable = "Maximum", Value = paste0(max(dat)))
      row10 <- data.frame(Variable = "IQR", Value = paste0(IQR(dat)))
      row11 <- data.frame(Variable = "Range", Value = paste0(range(dat)[2]-range(dat)[1]))
      row12 <- data.frame(Variable = "Sample Standard Deviation", Value = paste0(round(sd(dat),4)))
      row13 <- data.frame(Variable = "Sample Variance", Value = paste0(round(var(dat),4)))
      row14 <- data.frame(Variable = "Check for Outliers Lower", Value = paste(quantile(dat, 0.25) - (1.5*IQR(dat))))
      row15 <- data.frame(Variable = "Check for Outliers Upper", Value = paste(quantile(dat, 0.75) + (1.5*IQR(dat))))
      row16 <- data.frame(Variable = "Number of Outliers", Value = paste("In progress"))
      row17 <- data.frame(Variable = "Skewness", Value = paste0(round(skewness(dat),4)))
      row18 <- data.frame(Variable = "Kurtosis", Value = paste0(round(kurtosis(dat),4)))

      values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12, row13, row14, row15, row16, row17, row18)
      
      output$boxplotDespStats <- renderPlot({
        
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
        #   geom_boxplot(show.legend = FALSE)
      })
      
    }
  })
  
  
  observeEvent(input$goBinom, {
    output$render_probability <- renderUI({
      binomNum <- input$numTrailsBinom
      binomSu <- input$successProbBinom
      binomNumSu <- input$numSuccessesBinom
      if(input$calcBinom == 'exact' & input$calcBinom == 'cumulative' & input$calcBinom == 'P(X>x)'){
        withMathJax(
          paste0("\\(P(X = \\)","",binomNumSu,"\\()\\)","\\( = \\)","",round(dbinom(binomNumSu,binomNum,binomSu),4)),
          br(),
          paste0("\\(P(X \\leq \\)","",binomNumSu,"\\()\\)","","\\( = \\)","",round(pbinom(binomNumSu,binomNum,binomSu,lower.tail = TRUE),4)),
          br(),
          paste0("\\(P(X > \\)","",binomNumSu,"\\()\\)","","\\( = \\)","",round(pbinom(binomNumSu,binomNum,binomSu,lower.tail = FALSE),4))
        )
      }
      else if(input$calcBinom == 'exact'){
        withMathJax(
          paste0("\\(P(X = \\)","",binomNumSu,"\\()\\)","\\( = \\)","",round(dbinom(binomNumSu,binomNum,binomSu),4))
        )
      }
      else if(input$calcBinom == 'cumulative'){
        withMathJax(
          paste0("\\(P(X \\leq \\)","",binomNumSu,"\\()\\)","","\\( = \\)","",round(pbinom(binomNumSu,binomNum,binomSu,lower.tail = TRUE),4)))
      }
      else if(input$calcBinom == 'P(X>x)'){
        withMathJax(
          paste0("\\(P(X > \\)","",binomNumSu,"\\()\\)","","\\( = \\)","",round(pbinom(binomNumSu,binomNum,binomSu,lower.tail = FALSE),4)))
      }
      else{
        print("Select")
      }
    })
  })
  
  observeEvent(input$goPoisson, {
    output$render_probability <- renderUI({
      poissonAvg <- input$muPoisson
      numPoisson <- input$xPoisson 
      aP <- input$aPoisson
      bP <- input$bPoisson
      if(input$calcPoisson == "lowerTail"){
        withMathJax("\\(P(X \\leq \\)"," ",numPoisson," ","\\()\\)", " ", "\\( = \\)",round(ppois(numPoisson,poissonAvg,lower.tail = TRUE),4))
      }
      else if(input$calcPoisson == "upperTail"){
        withMathJax(
          paste0("\\(P(X > \\)", numPoisson, "\\()\\)", " ", "\\( = \\)" , " ", round(ppois(numPoisson, poissonAvg, lower.tail = FALSE), 4))
        )
      }
      else if(input$calcPoisson == "interval"){
        withMathJax(
          paste0("\\(P(\\)",aP," ", "\\(\\leq X\\leq \\)", " ", bP, "\\()\\)"," ", "\\( = \\)", " ", ifelse(input$aP > input$bP, "a must be less than or equal to b", round(ppois(input$bP, poissonAvg, lower.tail = TRUE) - ppois(input$aP - 1, poissonAvg, lower.tail = TRUE), 4)))   
        )
      }
      else{
        print("Please Select")
      }
    })
  })
  
  observeEvent(input$goNormal, {
    output$render_probability <- renderUI({
      normMean <- input$popMean
      normSd <- input$popSD
      normX <- input$xValue
      if(input$calcNormal == "P(X \\leq x)" && input$calcNormal == "P(X > x)"){
        withMathJax(
          paste0("\\(P(X \\leq \\)"," ",normX, "\\()\\)"," ", "\\( = \\)"," ", round(pnorm(normX, normMean, normSd),4)),
          br(),
          paste0("\\(P(X > \\)", " ",normX,"\\()\\)", " ", "\\( = \\)", " ",round(1 - pnorm(normX,normMean,normSd),4))
        )
      }
      else if(input$calcNormal == "P(X \\leq x)"){
        withMathJax(
          paste0("\\(P(X \\leq \\)"," ",normX, "\\()\\)"," ", "\\( = \\)"," ", round(pnorm(normX, normMean, normSd),4))
        )
      }
      else if(input$calcNormal == "P(X > x)"){
        withMathJax(
          paste0("\\(P(X > \\)", " ",normX,"\\()\\)", " ", "\\( = \\)", " ",round(1 - pnorm(normX,normMean,normSd),4))
        )
      }
      else{
        print("Please Select")
      }
      
      
    })
  })
  
  observeEvent(input$resetAll, {
    shinyjs::reset("sideBar")
  })
  
  
  observeEvent(input$resetAll,{
    hide(id = 'despStats')
  })
  
  observeEvent(input$goDescpStats, {
    show(id = 'despStats')
  })
  
  observeEvent(input$resetAllB, {
    hide(id = 'probabilityMP')
  })
  
  observeEvent(input$goBinom, {
    show(id = 'probabilityMP')
  }
    
  )

}
  
# Run the application 
shinyApp(ui = ui, server = server)