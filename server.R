## contributor <- function(name, funding, url = NULL, position = "Senior Developer", date_range = NULL) {
##   contributor <- list(
##     name = name,
##     funding = funding,
##     url = url,
##     position = position
##   )
##   class(contributor) <- c("contributor", class(contributor))
##   contributor
## }
## contributor_card <- function(contributor) {
##   stopifnot(is(contributor, "contributor"))
##   card(
##     div(
##       style = "align-self: left;",
##       contributor$name,
##       contributor$funding
##     ),
##     div(
##       style = "align-self: right;",
##       a(contributor$position,
##         href = contributor$url,
##         target = "_blank"),
##       contributor$date_range
##     )
##   )
## }
## contributor_table <- function(contributors) {
##   stopifnot(all(sapply(contributors, is, class2 = "contributor")))
##   make_table_row <- function(contributor) {
##     browser()
##     n <- tags$td(contributor$name)
##     p <- tags$td(contributor$position)
##     f <- tags$td(contributor$funding)
##     u <- tags$td(contributor$url)
##     d <- tags$td(contributor$date_range)
##     tags$tr(n, p, f, u, d)
##   }
##   withTags(table(th(tr(td("Contributor"), td("Position"), td("Funding"), td("URL"), td("Dates"))),
##                  lapply(contributors, make_table_row)))
## }

## contributors <- list(
##   current = list(
##     "Ashok" = contributor(
##       h1("Ashok Krishnmamurthy"),
##       p("Associate Professor (Statistician), Mount Royal University", br(), "Calgary AB, Canada"),
##       "https://www.mtroyal.ca/ProgramsCourses/FacultiesSchoolsCentres/ScienceTechnology/Departments/MathematicsComputing/Faculty/akrishnamurthy.htm",
##       "Supervisor"
##     ),
##     "Bryce" = contributor(
##       h2("Bryce Carson"),
##       p("MRU Open Educational Resources Grant (OEG) program"),
##       "https://github.com/bryce-carson"
##     ),
##     "Murad" = contributor(
##       h2("Murad Ahmed"),
##       p("Alberta Innovates Summer Research Studentships"),
##       "https://github.com/murad256"
##     ),
##     "Kai" = contributor(
##       h2("Kai Agoto"),
##       p("Faculty of Science and Technology (FST) Student Research Award"),
##       "https://github.com/magoto12"
##     ),
##     "Jacie" = contributor(
##       h2("Jacie Bennett"),
##       p("MRU Internal Research Grant Fund"),
##       "https://github.com/jacie-b"
##     ),
##     "Darren" = contributor(
##       h2("Darren Law Yan Lun"),
##       p("MRU Internal Research Grant Fund"),
##       "https://github.com/darrenlyl"
##     )
##   ),

##   past = list(
##     "Maryam" = contributor(
##       h2("Maryam Abou El Nasr"),
##       p("MATH 4199: Directed Readings"),
##       "https://github.com/Maryaen",
##       c("January – April 2026", "May 2025 – August 2025"),
##       ),
##     "Diana" = contributor(
##       h2("Diana Vi"),
##       p("Faculty of Science and Technology (FST) Student Research Award"),
##       date_range = "May 2025 – August 2025"
##     ),
##     "Samantha" = contributor(
##       h2("Samantha Brian"),
##       p("Provost's Teaching and Learning Enhancement Grant (TLEG)"),
##       date_range = "January 2025 – May 2025"
##     ),
##     "Bryce" = contributor(
##       h2("Bryce Carson"),
##       p("Provost's Teaching and Learning Enhancement Grant (TLEG)"),
##       date_range = "November 2024 – January 2025"
##     ),
##     "Michael" = contributor(
##       h2("Michael Myer"),
##       p("Provost's Teaching and Learning Enhancement Grant (TLEG)"),
##       date_range = ("September 2023 – April 2024")
##     ),
##     "Michael" = contributor(
##       h2("Michael Myer"),
##       p("Faculty of Science and Technology (FST) Student Research Award"),
##       date_range = p("June – August 2023")
##     ),
##     "Crystal" = contributor(
##       h2("Crystal Wai"),
##       p("COMP 5690: Senior Computer Science Project"),
##       date_range = p("September – December 2022")
##     )
##   )
## )

server <- function(session, input, output) {
  darkmode_toggle(inputid = "togglemode")

  observeEvent(input$authors_show, {
    showModal(modalDialog(
      ## FIXME: writeTags is entirely unhappy for some reason.
      ## lapply(contributors$current, contributor_card),
      ## contributor_table(contributors$past),
      ## NOTE: falling back to markdown.
      includeHTML("www/authors.html"),

      title = "About CougarStats",
      size = "xl",
      easyClose = TRUE,
      fade = FALSE
    ))
  })

  descStatsServer(id = "ds")
  probDistServer(id = "pd")
  sampSizeEstServer(id = "sse")
  statInfrServer(id = "si")
  regressionAndCorrelationServer(id = "rc")
  machineLearningServer(id = "ml")
}
