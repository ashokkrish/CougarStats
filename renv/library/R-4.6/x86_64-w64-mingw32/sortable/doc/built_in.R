## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
library(htmltools)
tags$div(
  class = "shiny-app-frame",
  tags$iframe(
    src = "https://andrie-de-vries.shinyapps.io/sortable_rank_list_app/",
    width = "100%",
    height = 550
    )
)

## ----echo=FALSE, cache=FALSE--------------------------------------------------
knitr::read_chunk(
  system.file("shiny/rank_list/app.R", package = "sortable")
)

## ----rank-list-app, eval=FALSE------------------------------------------------
# ## Example shiny app with rank list
# 
# library(shiny)
# library(sortable)
# 
# labels <- list(
#   "one",
#   "two",
#   "three",
#   htmltools::tags$div(
#     htmltools::em("Complex"), " html tag without a name"
#   ),
#   "five" = htmltools::tags$div(
#     htmltools::em("Complex"), " html tag with name: 'five'"
#   )
# )
# 
# rank_list_basic <- rank_list(
#   text = "Drag the items in any desired order",
#   labels = labels,
#   input_id = "rank_list_basic"
# )
# 
# rank_list_swap <- rank_list(
#   text = "Notice that dragging causes items to swap",
#   labels = labels,
#   input_id = "rank_list_swap",
#   options = sortable_options(swap = TRUE)
# )
# 
# rank_list_multi <- rank_list(
#   text = "You can select multiple items, then drag as a group",
#   labels = labels,
#   input_id = "rank_list_multi",
#   options = sortable_options(multiDrag = TRUE)
# )
# 
# 
# 
# ui <- fluidPage(
#   fluidRow(
#     column(
#       width = 12,
#     tags$h2("Default, multi-drag and swapping behaviour"),
#       tabsetPanel(
#         type = "tabs",
#         tabPanel(
#           "Default",
#             tags$b("Exercise"),
#             rank_list_basic,
#             tags$b("Result"),
#             verbatimTextOutput("results_basic")
#         ),
#         tabPanel(
#           "Multi-drag",
#             tags$b("Exercise"),
#             rank_list_multi,
#             tags$b("Result"),
#             verbatimTextOutput("results_multi")
#         ),
#         tabPanel(
#           "Swap",
#             tags$b("Exercise"),
#             rank_list_swap,
#             tags$b("Result"),
#             verbatimTextOutput("results_swap")
#         )
#       )
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   output$results_basic <- renderPrint({
#     input$rank_list_basic # This matches the input_id of the rank list
#   })
#   output$results_multi <- renderPrint({
#     input$rank_list_multi # This matches the input_id of the rank list
#   })
#   output$results_swap <- renderPrint({
#     input$rank_list_swap # This matches the input_id of the rank list
#   })
# }
# 
# shinyApp(ui, server)

## ----echo=FALSE---------------------------------------------------------------
library(htmltools)
tags$div(
  class = "shiny-app-frame",
  tags$iframe(
    src = "https://andrie-de-vries.shinyapps.io/sortable_bucket_list_app/",
    width = "100%",
    height = 800
    )
)

## ----echo=FALSE, cache=FALSE--------------------------------------------------
knitr::read_chunk(
  system.file("shiny/bucket_list/app.R", package = "sortable")
)

## ----bucket-list-app, eval=FALSE----------------------------------------------
# ## Example shiny app with bucket list
# 
# library(shiny)
# library(sortable)
# 
# ui <- fluidPage(
#   tags$head(
#     tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
#   ),
#   fluidRow(
#     column(
#       tags$b("Exercise"),
#       width = 12,
#       bucket_list(
#         header = "Drag the items in any desired bucket",
#         group_name = "bucket_list_group",
#         orientation = "horizontal",
#         add_rank_list(
#           text = "Drag from here",
#           labels = list(
#             "one",
#             "two",
#             "three",
#             htmltools::tags$div(
#               htmltools::em("Complex"), " html tag without a name"
#             ),
#             "five" = htmltools::tags$div(
#               htmltools::em("Complex"), " html tag with name: 'five'"
#             )
#           ),
#           input_id = "rank_list_1"
#         ),
#         add_rank_list(
#           text = "to here",
#           labels = NULL,
#           input_id = "rank_list_2"
#         )
#       )
#     )
#   ),
#   fluidRow(
#     column(
#       width = 12,
#       tags$b("Result"),
#       column(
#         width = 12,
# 
#         tags$p("input$rank_list_1"),
#         verbatimTextOutput("results_1"),
# 
#         tags$p("input$rank_list_2"),
#         verbatimTextOutput("results_2"),
# 
#         tags$p("input$bucket_list_group"),
#         verbatimTextOutput("results_3")
#       )
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   output$results_1 <-
#     renderPrint(
#       input$rank_list_1 # This matches the input_id of the first rank list
#     )
#   output$results_2 <-
#     renderPrint(
#       input$rank_list_2 # This matches the input_id of the second rank list
#     )
#   output$results_3 <-
#     renderPrint(
#       input$bucket_list_group # Matches the group_name of the bucket list
#     )
# 
# }
# 
# 
# shinyApp(ui, server)

