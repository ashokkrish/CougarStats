server <- function(session, input, output) {
  insertUI("#mainBanner", "beforeEnd",
           tagList(actionButton("authors_show", "Authors", class = "btn btn-info"),
                   actionButton("togglemode", "Toggle Dark Mode", class = "btn btn-warning")))

  darkmode_toggle(inputid = "togglemode")

  observeEvent(input$authors_show, {
    showModal(modalDialog(includeHTML("www/authors.html"),
                          size = "xl",
                          easyClose = TRUE,
                          fade = FALSE,
                          footer = NULL))
  })

  descStatsServer(id = "ds")
  probDistServer(id = "pd")
  sampSizeEstServer(id = "sse")
  statInfrServer(id = "si")
  regressionAndCorrelationServer(id = "rc")
  machineLearningServer(id = "ml")
}
