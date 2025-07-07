# pca.R

# Required libraries
library(shiny)
library(shinyjs)
library(datamods)
library(bslib)

#' PCA Sidebar UI Function
#'
#' @param id A string, the namespace id for the module.
#' @return A UI definition for the sidebar panel.
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

#' PCA Main Panel UI Function
#'
#' @param id A string, the namespace id for the module.
#' @return A UI definition for the main panel, with tabs.
PCAMainPanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    # Data import UI is now at the top level, always visible
    import_file_ui(id = ns("dataImport"), title = ""),
    
    # This div contains the results panels and is hidden by default
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

#' PCA Server Function
#'
#' This function contains the server-side logic for the PCA tab.
#'
#' @param id A string, the namespace id for the module.
PCAServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Data import server logic
    imported_data <- import_file_server(
      id = "dataImport",
      trigger_return = "change",
      btn_show_data = TRUE,
      return_class = "tbl_df"
    )
    
    # Show results on "Calculate"
    observeEvent(input$calculate, {
      req(imported_data$data())
      shinyjs::show("pcaResultsPanel")
    })
    
    # Hide results on "Reset"
    observeEvent(input$reset, {
      shinyjs::hide("pcaResultsPanel")
    })
    
    # Observer to react to the imported data and transformation choice for analysis
    observe({
      req(imported_data$data())
      
      # Placeholder for where you would add the PCA logic.
      # This logic would use input$transformation and imported_data$data()
      
    })
  })
}

