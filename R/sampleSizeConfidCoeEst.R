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

    # Each sub-module server is created exactly once, under a fixed id. Changing
    # the "Estimate" radio only swaps the UI that is shown for them; creating the
    # servers again on every change would stack up duplicate observers.
    sse_module_id <- "sse_ss0"
    cce_module_id <- "sse_cc0"

    output$sampleSizeConfidCoeEstSidebarUI <- renderUI({
      req(input$estimateParameter)

      if(input$estimateParameter == "Sample Size"){
        sampSizeEstSidebarUI(
          session$ns(sse_module_id)
        )
      }
      else if(input$estimateParameter == "Confidence Coefficient"){
        confidCoefSidebarUI(
          session$ns(cce_module_id)
        )
      }
    })

    output$sampleSizeConfidCoeEstMainPanelUI <- renderUI({
      req(input$estimateParameter)

      if(input$estimateParameter == "Sample Size"){
        ssEstimationMP(
          session$ns(sse_module_id)
        )
      }
      else if(input$estimateParameter == "Confidence Coefficient"){
        ccEstimationMP(
          session$ns(cce_module_id)
        )
      }
    })

    sampSizeEstServer(sse_module_id)
    confidenceCoefficientServer(cce_module_id)
  })
}
