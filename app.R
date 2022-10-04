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
              choices = c("Descriptive Stastics", "Probability","Inference","Correlation & Regression"),
              ),
            conditionalPanel(
              condition = "input.dropDownMenu == 'Descriptive Stastics'", 
              textInput("descriptiveStastics", "Sample", value=NULL, placeholder = "Enter values seperated by a comma with decimals as points"), #make wider and taller
              checkboxGroupInput(inputId = "checkBoxDescpStats", "Select", selected = c("Mean", "Mode","Median"),choices = c("Mean","Mode","Median","Standard Deviation","Interquartile Range","Box Plot")), #default mean, mode, median, must select at least once
              actionButton(inputId = "goDescpStats", "Calculate") #blue
            )
        ),

        mainPanel(
          conditionalPanel(
            condition = "input.dropDownMenu == 'Descriptive Stastics'",
            uiOutput("resultsDescpStats")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #String List to Numeric List
  createNumLst <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  Modes <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
  }
  
  # Mode <- function(x) {
  #   a <- table(x)
  #   as.numeric(names(a)[a == max(a)])
  # }
  
  output$resultsDescpStats <- renderUI({
    dat <- createNumLst(input$descriptiveStastics)
    if(anyNA(dat) | length(dat)<2){
          "Invalid input or not enough observations"
    } else{
      withMathJax(
        paste0("\\(\\bar{x}=\\)", mean(dat)),
        #paste0("Mode = ", Modes(dat)),
        br(),
        paste0("Median =", median(dat)),
        br(),
        paste("Five Number Summary:"),
        br(),
        paste0(min(dat), " ", quantile(dat,0.25)," ", median(dat), " ", quantile(dat, 0.75)," ", max(dat)),
        br(), 
        paste0("Q3: ", quantile(dat,0.25)), 
        br(),
        paste0("Q3: ", quantile(dat,0.75)),
        br(),
        paste0("IQR:",IQR(dat)),
        br(),
        paste0("Standard Deviation: ",sd(dat)),
        br(),
        paste0("Variance: ",var(dat)),
        br(),
        paste0(range(dat)[2]-range(dat)[1])
      )
      
    }
  })
  
  # output$resultsDescpStats <- renderUI({
  #   dat <- createNumLst(input$descriptiveStastics)
  #   if(anyNA(dat) | length(dat)<2){
  #     "Invalid input or not enough observations"
  #   } else if(input$checkBoxDescpStats == 'Mean'){
  #       withMathJax(
  #         paste0("mean = ",mean(dat))
  #     )}
  #     else if(input$checkBoxDescpStats == 'Mode'){
  #       withMathJax(
  #         paste0("mode = ", modeFunction(dat))
  #       )
  #     }
  #     else if(input$checkBoxDescpStats == 'Median'){
  #       withMathJax(
  #         paste0("median = ", median(dat))
  #       )
  #     }
  #   else{}
  #   })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
