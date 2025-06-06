# R/regressionAndCorrelation.R

regressionAndCorrelationUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      shinyjs::useShinyjs(), 
      radioButtons(ns("multiple"),
                   tags$b("Regression type"),
                   choices = list("Simple Linear Regression and Correlation Analysis" = "SLR",
                                  "Multiple Linear Regression" = "MLR",
                                  "Binary Logistic Regression" = "LOGR"), # Added Logistic Regression
                   selected = "SLR" 
      ),
      uiOutput(ns("regressionSidebarUI")) # Changed to a more generic name
    ),
    mainPanel(
      uiOutput(ns("regressionMainPanelUI")) # Changed to a more generic name
    )
  )
}

regressionAndCorrelationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # For SLR - can remain static
    SLR_MODULE_ID_STATIC <- "slr_static_instance"
    SLRServer(SLR_MODULE_ID_STATIC) # Called once
    
    # --- Dynamic ID Generation for MLR ---
    mlr_instance_counter <- reactiveVal(0)
    current_mlr_module_id <- reactive({
      paste0("mlr_dynamic_instance_", mlr_instance_counter())
    })
    
    # --- Dynamic ID Generation for Logistic Regression (LOGR) ---
    logr_instance_counter <- reactiveVal(0)
    current_logr_module_id <- reactive({
      paste0("logr_dynamic_instance_", logr_instance_counter())
    })
    
    # Observer for the main radio button (input$multiple)
    observeEvent(input$multiple, {
      if (input$multiple == "MLR") {
         # Increment counter to generate a new ID for MLR, forcing re-instantiation
        mlr_instance_counter(mlr_instance_counter() + 1) # Increment for new instance
        # The renderUI calls will now use the new, unique ID from current_mlr_module_id()
        # This ensures that when MLR UI is rendered, it's for a "new" module instance.
        output$regressionSidebarUI <- renderUI({
          req(current_mlr_module_id()) # Make sure the ID is ready
          MLRSidebarUI(session$ns(current_mlr_module_id()))
        })
        output$regressionMainPanelUI <- renderUI({
          req(current_mlr_module_id())
          MLRMainPanelUI(session$ns(current_mlr_module_id()))
        })
        # The MLRServer call is now also tied to the dynamic ID changing
        # This is done in a separate observer below that reacts to current_mlr_module_id()
      } else if (input$multiple == "SLR") {
        output$regressionSidebarUI <- renderUI({ SLRSidebarUI(session$ns(SLR_MODULE_ID_STATIC)) })
        # For SLR, we use the static ID (no reset-on-switch behavior needed here for SLR)
        output$regressionMainPanelUI <- renderUI({ SLRMainPanelUI(session$ns(SLR_MODULE_ID_STATIC)) })
      } else if (input$multiple == "LOGR") {
        logr_instance_counter(logr_instance_counter() + 1) # Increment for new instance
        output$regressionSidebarUI <- renderUI({
          req(current_logr_module_id())
          LogisticRegressionSidebarUI(session$ns(current_logr_module_id()))
        })
        output$regressionMainPanelUI <- renderUI({
          req(current_logr_module_id())
          LogisticRegressionMainPanelUI(session$ns(current_logr_module_id()))
        })
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE) # ignoreInit=FALSE to render the default on load
    
    # This observer re-calls MLRServer whenever current_mlr_module_id() changes,
    # effectively creating a new server-side instance of the MLR module.
    observeEvent(current_mlr_module_id(), {
      req(input$multiple == "MLR") 
      MLRServer(current_mlr_module_id())
    }, ignoreNULL = TRUE) 
    
    # Observer for LOGR Server instantiation
    observeEvent(current_logr_module_id(), {
      req(input$multiple == "LOGR")
      LogisticRegressionServer(current_logr_module_id())
    }, ignoreNULL = TRUE)

  })
}