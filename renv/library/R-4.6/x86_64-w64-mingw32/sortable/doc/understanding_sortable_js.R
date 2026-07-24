## ----echo = FALSE-------------------------------------------------------------
## get knitr just the way we like it

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  tidy = FALSE,
  cache = FALSE
)

## -----------------------------------------------------------------------------
library(sortable)
library(htmltools)

## ----echo=FALSE---------------------------------------------------------------
sortable_js(css_id = "example01")

## -----------------------------------------------------------------------------
library(htmltools)
tagList(
  tags$ul(
    id = "example02",
    tags$li("drag me"),
    tags$li("sort me"),
    tags$li("any way you like")
  ),
  sortable_js("example02")
)

## -----------------------------------------------------------------------------
library(base64enc)
library(withr)

# create two plots for demo purposes
pngfile_1 <- tempfile(fileext = ".png")
with_png(pngfile_1, width = 300, height = 200,{
  plot(1:100, rnorm(100), pch = 21, bg = "red")
  title(main = "Moves Like Jagger")
})

pngfile_2 <- tempfile(fileext = ".png")
with_png(pngfile_2, width = 300, height = 200,{
  barplot(1:9, col = blues9)
  title(main = "I Like the Way You Move")
})

## -----------------------------------------------------------------------------
tagList(
  tags$div(
    id = "example03",
    tags$image(src = base64enc::dataURI(file = pngfile_1, mime = "image/png")),
    tags$image(src = base64enc::dataURI(file = pngfile_2, mime = "image/png"))
  ),
  sortable_js(css_id = "example03")
)

## ----shiny-drag-vars-to-plot, eval=FALSE--------------------------------------
# knitr::read_chunk(
#   system.file("shiny/drag_vars_to_plot/app.R", package = "sortable")
# )

## ----echo=FALSE, cache=FALSE--------------------------------------------------
knitr::read_chunk(
  system.file("shiny/shiny_tabset/app.R", package = "sortable")
)


## ----shiny-tabset-app, eval=FALSE---------------------------------------------
# ## Example shiny app to drag-and-drop tabsets in a shiny app
# 
# 
# # all credit for code goes to RStudio
# # https://github.com/rstudio/shiny/tree/main/006-tabsets
# library(sortable)
# library(shiny)
# 
# ui = # Define UI for random distribution application
#   shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Tabsets"),
# 
#     # Sidebar with controls to select the random distribution type
#     # and number of observations to generate. Note the use of the
#     # br() element to introduce extra vertical spacing
#     sidebarLayout(
#       sidebarPanel(
#         radioButtons(
#           "dist", "Distribution type:",
#           c(
#             "Normal" = "norm",
#             "Uniform" = "unif",
#             "Log-normal" = "lnorm",
#             "Exponential" = "exp"
#           )
#         ),
#         br(),
# 
#         sliderInput(
#           "n",
#           "Number of observations:",
#           value = 500,
#           min = 1,
#           max = 1000)
#       ),
# 
#       # Show a tabset that includes a plot, summary, and table view
#       # of the generated distribution
#       mainPanel(
#         tabsetPanel(
#           type = "tabs",
#           id = "sortTab",
#           tabPanel("Plot", plotOutput("plot")),
#           tabPanel("Summary", verbatimTextOutput("summary")),
#           tabPanel("Table", tableOutput("table"))
#         )
#       )
#     ),
#     sortable_js("sortTab")
#   ))
# 
# server = function(input, output) {
# 
#   # Reactive expression to generate the requested distribution.
#   # This is called whenever the inputs change. The output
#   # functions defined below then all use the value computed from
#   # this expression
#   data <- reactive({
#     dist <- switch(
#       input$dist,
#       norm = rnorm,
#       unif = runif,
#       lnorm = rlnorm,
#       exp = rexp,
#       rnorm
#     )
# 
#     dist(input$n)
#   })
# 
#   # Generate a plot of the data. Also uses the inputs to build
#   # the plot label. Note that the dependencies on both the inputs
#   # and the data reactive expression are both tracked, and
#   # all expressions are called in the sequence implied by the
#   # dependency graph
#   output$plot <- renderPlot({
#     dist <- input$dist
#     n <- input$n
# 
#     hist(data(),
#          main=paste('r', dist, '(', n, ')', sep=''))
#   })
# 
#   # Generate a summary of the data
#   output$summary <- renderPrint({
#     summary(data())
#   })
# 
#   # Generate an HTML table view of the data
#   output$table <- renderTable({
#     data.frame(x = data())
#   })
# 
# }
# 
# shinyApp( ui, server )

