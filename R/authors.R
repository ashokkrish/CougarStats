authorsUI <- function() {
  tagList(
    shiny::includeMarkdown("www/authors.md"),
    hr(),
    p("We welcome questions, insights, and feedback."),
    HTML("<h5>Created with Posit&trade; and Shiny&#174;</h5>")

  )
}
