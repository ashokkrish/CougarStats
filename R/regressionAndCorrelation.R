# R/regressionAndCorrelation.R

regressionAndCorrelationUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(), # Ensure useShinyjs is called
      radioButtons(ns("multiple"),
                   tags$b("Regression type"),
                   choices = list("Simple Linear Regression and Correlation Analysis" = "SLR",
                                  "Multiple Linear Regression" = "MLR"),
                   selected = "SLR" # Or your preferred default
      ),
      uiOutput(ns("simpleOrMultipleRegressionSidebarUI"))
    ),
    mainPanel(
      uiOutput(ns("simpleOrMultipleRegressionMainPanelUI"))
    )
  )
}

regressionAndCorrelationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # For SLR - can remain static if it doesn't need this aggressive reset
    SLR_MODULE_ID_STATIC <- "slr_static_instance"
    SLRServer(SLR_MODULE_ID_STATIC)
    
    # --- Dynamic ID Generation for MLR ---
    # Counter to create unique IDs
    mlr_instance_counter <- reactiveVal(0)
    
    # This reactive will hold the current unique ID for the MLR module
    current_mlr_module_id <- reactive({
      paste0("mlr_dynamic_instance_", mlr_instance_counter())
    })
    # --- End Dynamic ID Generation ---
    
    # Observer for the main radio button (input$multiple)
    observeEvent(input$multiple, {
      if (input$multiple == "MLR") {
        # Increment counter to generate a new ID for MLR, forcing re-instantiation
        mlr_instance_counter(mlr_instance_counter() + 1)
        
        # The renderUI calls will now use the new, unique ID from current_mlr_module_id()
        # This ensures that when MLR UI is rendered, it's for a "new" module instance.
        output$simpleOrMultipleRegressionSidebarUI <- renderUI({
          req(current_mlr_module_id()) # Make sure the ID is ready
          MLRSidebarUI(session$ns(current_mlr_module_id()))
        })
        output$simpleOrMultipleRegressionMainPanelUI <- renderUI({
          req(current_mlr_module_id())
          MLRMainPanelUI(session$ns(current_mlr_module_id()))
        })
        
        # The MLRServer call is now also tied to the dynamic ID changing
        # This is done in a separate observer below that reacts to current_mlr_module_id()
        
      } else if (input$multiple == "SLR") {
        # For SLR, we use the static ID (no reset-on-switch behavior needed here for SLR)
        output$simpleOrMultipleRegressionSidebarUI <- renderUI({ SLRSidebarUI(session$ns(SLR_MODULE_ID_STATIC)) })
        output$simpleOrMultipleRegressionMainPanelUI <- renderUI({ SLRMainPanelUI(session$ns(SLR_MODULE_ID_STATIC)) })
      }
    }, ignoreNULL = FALSE) # ignoreNULL = FALSE to handle initial state
    
    # This observer re-calls MLRServer whenever current_mlr_module_id() changes,
    # effectively creating a new server-side instance of the MLR module.
    observeEvent(current_mlr_module_id(), {
      req(input$multiple == "MLR") # Only proceed if MLR is the selected tab
      # print(paste("Instantiating MLRServer with ID:", current_mlr_module_id())) # For debugging
      MLRServer(current_mlr_module_id())
    }, ignoreNULL = TRUE) # ignoreNULL = TRUE initially because current_mlr_module_id has an initial value
    
  })
}