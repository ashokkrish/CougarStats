statInfrMethodOneServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    source("validation.R", TRUE)
    source("printers.R", TRUE)
    source("observers.R", TRUE)
    source("reactives.R", TRUE)

    observeEvent(iv$validate(), {
      if (iv$is_valid()) {
        ## shiny::showNotification("Enabling the calculate button; form is valid!", type = "message")
        shinyjs::enable("calculate")
      } else {
        ## shiny::showNotification("Disabling the calculate button while form is invalid!", type = "error")
        shinyjs::disable("calculate")
      }
    })

    observeEvent(input$calculate, {
      req(iv$is_valid())
      gsub.ed <- gsub("Population| ", "", input$popuParameter)
      ## NOTE B.C.: see the observer of popuParameter, which also hides tabs and shows tabs,
      ## though at an earlier time!
      showTabs("mainPanelNavbarPage", id_helper(gsub.ed), id_helper(gsub.ed, "Analysis"))
    })

    observeEvent(input$reset, {
      ## NOTE B.C.: respecting input$upload, see the note in the observer of dataAvailability.
      reset("sidebarPanel")
    })
  })
}
