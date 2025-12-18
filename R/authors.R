authorsUI <- function() {
  tagList(
    shiny::includeMarkdown("www/authors.md"),
    hr(),
    p("Comments, bug reports and suggestions for improvements can be sent to Ashok Krishnamurthy."),
    HTML("<h5>Created with Posit&trade; and Shiny&#174;</h5>")

  )
}
