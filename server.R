server <- function(session, input, output) {
  # Insert the buttons wrapped in <li class="nav-item"> so they are *siblings of the other
  # navigation items (<li>s) inside the #mainBanner <ul>. This makes them flow exactly the
  # same as the tab links (inline in the navbar-nav). We use beforeEnd so they appear at
  # the end of the nav items list.
  # We also add an explicit onclick that calls Shiny.setInputValue to ensure the input
  # is sent even if event propagation in the tabset ul interferes with standard actionButton
  # binding (this was preventing the modal from appearing).
  insertUI("#mainBanner", "beforeEnd",
           tagList(
             tags$li(
               class = "nav-item",
               actionButton("authors_show", "Authors", class = "btn btn-info",
                            onclick = "event.stopImmediatePropagation(); Shiny.setInputValue('authors_show', Date.now(), {priority: 'event'});")
             ),
             tags$li(
               class = "nav-item",
               actionButton("togglemode", "Toggle Dark Mode", class = "btn btn-warning")
             )
           ),
           immediate = TRUE)

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
