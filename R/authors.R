authorsUI <- function() {
  tagList(
    shiny::includeMarkdown("www/authors.md"),
    
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.querySelectorAll('a').forEach(function(link) {
          link.setAttribute('target', '_blank');
          link.setAttribute('rel', 'noopener noreferrer');
        });
      });
    ")),
    
    hr(),
    p("Comments, bug reports and suggestions for improvements can be sent to Ashok Krishnamurthy."),
    HTML("<h5>Created with Posit&trade; and Shiny&#174;</h5>")
  )
}
