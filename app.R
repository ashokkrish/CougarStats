library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Intro to Stats"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId= "dropDownMenu",
                      label= "Choose your test",
                      choices = c("Descriptive Statistics", "Probability","Inference","Correlation & Regression"),
                    ),
                    conditionalPanel(
                      condition = "input.dropDownMenu == 'Descriptive Statistics'",
                      textAreaInput("descriptiveStat", "Sample", value=NULL, placeholder = "Enter values seperated by a comma with decimals as points", rows = 6), #make wider and taller
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
                    conditionalPanel(
                      condition = "input.dropDownMenu == 'Descriptive Statistics'",
                      uiOutput("resultsDescpStats")
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
  # # Modes <- function(x) {
  # #   ux <- unique(x)
  # #   tab <- tabulate(match(x, ux))
  # #   ux[tab == max(tab)]
  # # }
  # 
  # # Mode <- function(x) {
  # #   a <- table(x)
  # #   as.numeric(names(a)[a == max(a)])
  # # }
  # 
  output$resultsDescpStats <- renderUI({
    dat <- createNumLst(input$descriptiveStat)
    if(anyNA(dat) | length(dat)<2){
      "Invalid input or not enough observations"
    } else{
      withMathJax(
        paste0("\\(\\bar{x}=\\)", mean(dat)),
        #paste0("Mode = ", Modes(dat)),
        br(),
        paste0("Median (Q2) =", median(dat)),
        br(),
        #paste("Five Number Summary:"),
        #br(),
        #paste0(min(dat), " ", quantile(dat,0.25)," ", median(dat), " ", quantile(dat, 0.75)," ", max(dat)),
        #br(),
        paste0("Q1: ", quantile(dat,0.25)),
        br(),
        paste0("Q3: ", quantile(dat,0.75)),
        br(),
        paste0("IQR:",IQR(dat)),
        br(),
        paste0("Standard Deviation: ",sd(dat)),
        br(),
        paste0("Variance: ",var(dat)),
        br(),
        paste0("Range",range(dat)[2]-range(dat)[1]),
        br()
        #paste0("Boxplot", boxplot(c(dat)))
      )
    }
  })

  }
  


# Run the application 
shinyApp(ui = ui, server = server)