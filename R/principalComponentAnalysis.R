
library(shiny)
library(shinyjs)
library(datamods)
library(bslib)

PCASidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "si-label", # Using the same class as the "Methodology" label for consistent styling
      tags$b("PCA Options")
    ),
    # Radio buttons for selecting the data scaling method
    radioButtons(ns("transformation"),
                 label = NULL, # Set label to NULL to prevent extra space
                 choices = c("Original scale",
                             "Logarithmic transformation",
                             "Box-Cox transformation",
                             "Standardized Box-Cox transformation"),
                 selected = "Original scale"),
    
    actionButton(ns("calculate"), "Calculate", class = "act-btn"),
    actionButton(ns("reset"), "Reset Values", class = "act-btn")
  )
}

PCAMainPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    import_file_ui(id = ns("dataImport"), title = ""),
    
    hidden(
      div(
        id = ns("pcaResultsPanel"),
        navbarPage(title = NULL,
                   id = ns("mainPanelTabs"),
                   theme = bs_theme(version = 4),
                   tabPanel(title = "PCA Results",
                            # Placeholder for PCA results output
                            verbatimTextOutput(ns("pcaSummary"))
                   )
        )
      )
    )
  )
}

PCAServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Data import server logic
    imported_data <- import_file_server(
      id = "dataImport",
      trigger_return = "change",
      btn_show_data = TRUE,
      return_class = "tbl_df"
    )
    

    observeEvent(input$calculate, {
      req(imported_data$data())
      shinyjs::show("pcaResultsPanel")
    })
    
    observeEvent(input$reset, {
      shinyjs::hide("pcaResultsPanel")
    })
    
    observe({
      req(imported_data$data())

    })
  })
}

