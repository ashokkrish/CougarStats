## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sortable)

## ----echo=FALSE---------------------------------------------------------------
library(htmltools)
tags$div(
  class = "shiny-app-frame",
  tags$iframe(
    src = "https://andrie-de-vries.shinyapps.io/sortable_custom_css_app/",
    width = "100%",
    height = 450
  )
)

## ----echo=FALSE, cache=FALSE--------------------------------------------------
knitr::read_chunk(
  system.file("shiny/custom_css/app.R", package = "sortable")
)

## ----custom-css-app, eval=FALSE-----------------------------------------------
# ## Example shiny app with custom css
# 
# library(shiny)
# library(sortable)
# 
# ui <- fluidPage(
#   fluidRow(
#     column(
#       width = 12,
#       tags$b("Exercise"),
#       rank_list(
#         text = "Drag the items in any desired order",
#         labels = list(
#           "one",
#           "two",
#           "three",
#           "four",
#           "five"
#         ),
#         input_id = "rank_list_1",
#         class = c("default-sortable", "custom-sortable") # add custom style
#       ),
#       tags$style(
#         HTML("
#           .rank-list-container.custom-sortable {
#             background-color: #8A8;
#           }
#           .custom-sortable .rank-list-item {
#             background-color: #BDB;
#           }
#         ")
#       ),
#       tags$b("Result"),
#       verbatimTextOutput("results")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   output$results <- renderPrint({
#     input$rank_list_1 # This matches the input_id of the rank list
#   })
# }
# 
# shinyApp(ui, server)

