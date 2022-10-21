library(shiny)
library(shinythemes)
library(moments)

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
                  sidebarPanel(
                    selectInput(
                      inputId= "dropDownMenu",
                      label= "Choose your test",
                      choices = c("Descriptive Statistics", "Probability","Inference"),
                    ),
                    conditionalPanel(
                      condition = "input.dropDownMenu == 'Descriptive Statistics'",
                      textAreaInput("descriptiveStat", "Sample", value= "2.14,2.09,2.65,3.56,5.55,5.00,5.55,3.09,6.79", placeholder = "Enter values seperated by a comma with decimals as points", rows = 6), #make wider and taller
                      #checkboxGroupInput(inputId = "checkBoxDescpStats", "Select", selected = c("Mean", "Mode","Median"),choices = c("Mean","Mode","Median","Standard Deviation","Interquartile Range","Box Plot")), #default mean, mode, median, must select at least once
                      actionButton(inputId = "goDescpStats", "Calculate") #blue
                    ),
                    conditionalPanel(
                      condition = "input.dropDownMenu == 'Probability'",
                      radioButtons("probability", "Choose", choices = c("Discrete","Continuous"), selected = NULL, inline=TRUE),
                      textAreaInput("probabilitySample", "Sample", value= NULL, placeholder = "Enter values seperated by a comma with decimals as points", rows = 6)
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
                                     choiceNames = list("Known","Unknown"),
                                     selected = character(0),
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
                        
                        textAreaInput("sample1", "Sample 1", value = NULL, placeholder = "Enter values seperated by a comma with decimals as points", rows = 3),
                      ),
                      
                      conditionalPanel(
                        condition = "input.samplesSelect == '2' && input.dataAvailability == 'Enter Raw Data'",
                        
                        textAreaInput("sample1", "Sample 1", value = NULL, placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        
                        textAreaInput("sample2", "Sample 2", value = NULL, placeholder = "Enter values separated by a comma with decimals as points", rows = 3),
                        
                        radioButtons(inputId = "samplesType",
                                     label = strong("Type of Samples"),
                                     choiceValues = list("Independent Samples","Dependent Samples"),
                                     choiceNames = list("Independent Samples","Dependent Samples (Paired Data)"),
                                     selected = character(0), # "Independent Samples", #
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
                        ),
                      ),
                      
                      actionButton(inputId = "goInference", "Calculate",
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      actionButton("resetAll","Reset Values",
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )
                  ),
                  
                  mainPanel(
                    div( id="despStats",
                      conditionalPanel(
                        condition = "input.dropDownMenu == 'Descriptive Statistics'",
                        tableOutput("table"),
                        plotOutput("boxplotDespStats")
                    )
                    
                    )
                  )
                )
)

  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # # #String List to Numeric List
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
      print(dat)
      values <- reactiveValues()
      values$df <- data.frame(Variable = character(), Value = character())
      output$table <- renderTable(values$df)
      row1 <- data.frame(Variable = "Mean", Value = paste0(mean(dat)))
      row2 <- data.frame(Variable = "Median (Q2)", Value = paste0(median(dat)))
      row3 <- data.frame(Variable = "Mode", Value = paste(Modes(dat)))
      row4 <- data.frame(Variable = "Q1", Value = paste0(quantile(dat, 0.25)))
      row5 <- data.frame(Variable = "Q3", Value = paste0(quantile(dat, 0.75)))
      row6 <- data.frame(Variable = "Minimum", Value = paste0(min(dat)))
      row7 <- data.frame(Variable = "Maximum", Value = paste0(max(dat)))
      row8 <- data.frame(Variable = "IQR", Value = paste0(IQR(dat)))
      row9 <- data.frame(Variable = "Range", Value = paste0(range(dat)[2]-range(dat)[1]))
      row10 <- data.frame(Variable = "Sample Standard Deviation", Value = paste0(sd(dat)))
      row11 <- data.frame(Variable = "Sample Variance", Value = paste0(var(dat)))
      row12 <- data.frame(Variable = "Check for Outliers Lower", Value = paste("In progress"))
      row13 <- data.frame(Variable = "Check for Outliers Upper", Value = paste("In progress"))
      row14 <- data.frame(Variable = "Number of Outliers", Value = paste("In progress"))
      row15 <- data.frame(Variable = "Skewness", Value = paste0(skewness(dat)))
      row16 <- data.frame(Variable = "Kurtosis", Value = paste0(kurtosis(dat)))
      row17 <- data.frame(Variable = "Count", Value = paste0(length(dat)))
      row18 <- data.frame(Variable = "Sum", Value = paste0(sum(dat)))
      values$df <- rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12, row13, row14, row15, row16, row17, row18)
      
      output$boxplotDespStats <- renderPlot({
        boxplot(dat)
      })
      
    }
  })

  }
  


# Run the application 
shinyApp(ui = ui, server = server)