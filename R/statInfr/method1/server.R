statInfrMethodOneServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    source("validation.R", TRUE)
    source("printers.R", TRUE)
    source("observers.R", TRUE)
    source("reactives.R", TRUE)

    observeEvent(iv$validate(), {
      if (iv$is_valid()) {
        shiny::showNotification("Enabling the calculate button; form is valid!", type = "message")
        shinyjs::enable("calculate")
      } else {
        shiny::showNotification("Disabling the calculate button while form is invalid!", type = "error")
        shinyjs::disable("calculate")
      }
    })

    observeEvent(input$calculate, {
      req(iv$is_valid())
      ## TODO: render the UI, as needed.
      if (input$popuParameter == "Population Mean") {
        output$oneSamplePopulationMeanAnalysis <- renderUI({
          if (input$inferenceType == "Confidence Interval") {
            printOneMeanCI()
          } else if (input$inferenceType == "Hypothesis Testing") {
            printOneMeanHT()
          }
        })
        showTabs("mainPanelNavbarPage", id_helper("Mean"), id_helper("Mean", "Analysis"))
      } else if (input$popuParameter == "Population Standard Deviation") {
        showTabs("mainPanelNavbarPage", id_helper("StandardDeviation"), id_helper("Mean", "Analysis"))
      } else if (input$PopuParameter == "Population Proportion") {
        showTabs("mainPanelNavbarPage", id_helper("Proportion"), id_helper("Mean", "Analysis"))
      } else {
        stop("input$popuParameter has an invalid value!")
      }
    })

    observeEvent(input$reset, {
      ## NOTE: see the NOTE in the observers.R file on the dataAvailability
      ## observer regarding input$upload.
      reset("sidebarPanel")
    })
  })
}
