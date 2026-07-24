sampleSizeConfidCoeEstUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(),
      
      radioButtons(
        inputId = ns("estimateParameter"),
        label = strong("Estimate"), 
        choiceValues = list("Sample Size",
                            "Confidence Coefficient"),
        choiceNames = list("Sample Size (\\(n\\))",
                           "Confidence Coefficient (\\(1- \\alpha\\))"),
        selected = "Sample Size",
        inline = TRUE
      ),
      
      uiOutput(ns("sampleSizeConfidCoeEstSidebarUI"))
    ), #sidebarPanel
    
    mainPanel(
      uiOutput(ns("sampleSizeConfidCoeEstMainPanelUI"))
    ) #mainPanel
  ) #sidebarLayout
}

sampleSizeConfidCoeEstServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    sse_instance_counter  <- reactiveVal(0)
    cce_instance_counter  <- reactiveVal(0)
    
    current_sse_module_id  <- reactive({ paste0("sse_ss",  sse_instance_counter()) })
    current_cce_module_id  <- reactive({ paste0("sse_cc",  cce_instance_counter()) })

    observeEvent(input$estimateParameter, {
      req(input$estimateParameter)
      
      if(input$estimateParameter == "Sample Size"){
        module_id <- current_sse_module_id()
        
        output$sampleSizeConfidCoeEstSidebarUI <- renderUI({
          sampSizeEstSidebarUI(
            session$ns(module_id)
          )
        })
        
        output$sampleSizeConfidCoeEstMainPanelUI <- renderUI({
          ssEstimationMP(
            session$ns(module_id)
          )
        })
        
        sampSizeEstServer(module_id)
      }
      else if(input$estimateParameter == "Confidence Coefficient"){
        module_id <- current_cce_module_id()
        
        output$sampleSizeConfidCoeEstSidebarUI <- renderUI({
          confidCoefSidebarUI(
            session$ns(module_id)
          )
        })
        
        output$sampleSizeConfidCoeEstMainPanelUI <- renderUI({
          ccEstimationMP(
            session$ns(module_id)
          )
        })
        
        confidenceCoefficientServer(module_id)
      }  
     })
  })
}