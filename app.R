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
                      checkboxGroupInput(inputId = "checkBoxDescpStats", "Select", selected = c("Mean", "Mode","Median"),choices = c("Mean","Mode","Median","Standard Deviation","Interquartile Range","Box Plot")), #default mean, mode, median, must select at least once
                      actionButton(inputId = "goDescpStats", "Calculate") #blue
                    ),
                    conditionalPanel(
                      condition = "input.dropDownMenu == 'Probability'",
                      radioButtons("probability", "Choose", choices = c("1","2"), selected = NULL, inline=TRUE),
                      textAreaInput("probabilitySample", "Sample", value= NULL, placeholder = "Enter values seperated by a comma with decimals as points", rows = 6)
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
      source("R/DescpStats.R")
    }
  })

  }
  


# Run the application 
shinyApp(ui = ui, server = server)