# =========================================================================== #  
# ---- UI Components -------------------------------------------------------- 
# =========================================================================== #

sampSizeEstUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      #  ========================================================================= #  
      ## -------- Sidebar Panel -------------------------------------------------- 
      #  ========================================================================= #
      sidebarPanel(
        withMathJax(),
        shinyjs::useShinyjs(),
        div(
          id = ns("inputPanel"),
          
          radioButtons(
            inputId = ns("estimateParameter"),
            label = strong("Estimate"), 
            choiceValues = list("Sample Size",
                                "Confidence Coefficient"),
            choiceNames = list("Sample Size (\\(n\\))",
                               "Confidence Coefficient (\\(1- \\alpha\\))"),
            selected = "Sample Size",
            inline = TRUE
          ),
          
          radioButtons(
            inputId      = ns("sampSizeEstParameter"),
            label        = strong("Parameter of Interest"),
            choiceValues = list("Population Mean",
                                "Population Proportion"),
            choiceNames  = list("Population Mean (\\( \\mu \\)) ",
                                "Population Proportion (\\( p\\))"),
            selected     = "Population Mean", #character(0), #
            inline       = TRUE),
          
          
          
          # 4 Options: Mean & Margin of Error, Mean & Width of Interval,
          #            Prop & Margin of Error, Prop & Width of Interval
          
          # Population Mean + (MoE or WoI)
          
          conditionalPanel(
            ns = ns,
            condition = "input.estimateParameter == 'Confidence Coefficient' && input.sampSizeEstParameter == 'Population Mean' ",
            
            numericInput(
              inputId = ns("confSampSize"),
              label = strong("Sample Size (\\(n\\))"),
              value = "18",
              min = 1, 
              step = 1
            ),
            
            numericInput(
              inputId = ns("confPopSD"),
              label = strong("Population Standard Deviation (\\( \\sigma\\))"),
              value = "12",
              min     = 0.00001, 
              step    = 0.00001
            ),
            
            radioButtons(
              inputId = ns("ccEstimationType"),
              label = strong("Estimation Type"),
              choiceValues = list("Margin of Error",
                                  "Width of Interval"),
              choiceNames  = list("Margin of Error (\\( E\\)) ",
                                  "Width of Interval (\\( W\\))"),
              selected = "Margin of Error",
              inline = TRUE
              
            ),
            
            
            
            conditionalPanel(
              ns = ns,
              condition = "input.ccEstimationType == 'Margin of Error'",
              
              numericInput(
                inputId = ns("ccMargErr"),
                label   = strong("Margin of Error (\\( E\\))"),
                value   = "0.01", 
                min     = 0.00001, 
                step    = 0.01)
            ),
            
            conditionalPanel(
              ns = ns,
              condition = "input.ccEstimationType == 'Width of Interval'",
              
              numericInput(
                inputId = ns("ccMeanWoI"),
                label   = strong("Width of Interval (\\( W\\))"),
                value   = "16", 
                min     = 0.00001, 
                step    = 0.01)
            ),
            actionButton(
              inputId = ns("goConfidCoeEst"),
              label = "Calculate",
              class = "act-btn"
            ), 
            
            actionButton(
              inputId = ns("resetConfidCoeEst"),
              label = "Reset Values",
              class = "act-btn"
            )
          ),
          
          conditionalPanel(
            ns = ns,
            condition = "input.estimateParameter == 'Confidence Coefficient' && input.sampSizeEstParameter == 'Population Proportion' ",
            
            numericInput(
              inputId = ns("ccPropSampSize"),
              label = strong("Sample Size \\(n\\)"),
              value = "18",
              min = 1, 
              step = 1
            ),
            
            numericInput(
              inputId = ns("ccTargetProp"),
              label   = strong("Planning value for the Population Proportion (p)"),
              value   = "0.5", 
              min     = 0.00001, 
              step    = 0.01),
            
            checkboxInput(
              inputId = ns("propNormalDistribution"),
              label   = "Assume data follows a normal distribution",
              value   = TRUE
            ),
            
            radioButtons(
              inputId = ns("ccPropEstimationType"),
              label = strong("Estimation Type"),
              choiceValues = list("Margin of Error",
                                  "Width of Interval"),
              choiceNames  = list("Margin of Error (\\( E\\)) ",
                                  "Width of Interval (\\( W\\))"),
              selected = "Margin of Error",
              inline = TRUE
              
            ),
            
            
            
            conditionalPanel(
              ns = ns,
              condition = "input.ccPropEstimationType == 'Margin of Error'",
              
              numericInput(
                inputId = ns("ccPropMargErr"),
                label   = strong("Margin of Error (\\( E\\))"),
                value   = "0.01", 
                min     = 0.00001, 
                step    = 0.01)
            ),
            
            conditionalPanel(
              ns = ns,
              condition = "input.ccPropEstimationType == 'Width of Interval'",
              
              numericInput(
                inputId = ns("ccPropWoI"),
                label   = strong("Width of Interval (\\( W\\))"),
                value   = "16", 
                min     = 0.00001, 
                step    = 0.01)
              
            ),
            actionButton(
              inputId = ns("goConfidCoeEst"),
              label = "Calculate",
              class = "act-btn"
            ), 
            
            actionButton(
              inputId = ns("resetConfidCoeEst"),
              label = "Reset Values",
              class = "act-btn"
            )
          ), 
          
          conditionalPanel(
            ns = ns,
            condition = "input.estimateParameter == 'Sample Size' && input.sampSizeEstParameter == 'Population Mean'",
            
            radioButtons(
              inputId  = ns("confLeveln"),
              label    = strong("Confidence Level (\\( 1- \\alpha\\))"),
              choices = c("90%", 
                           "95%",
                           "99%"),
              selected = "95%",
              inline   = TRUE),
            
            numericInput(
              inputId = ns("ssePopuSD"),
              label   = strong("Population Standard Deviation (\\( \\sigma\\))"),
              value   = "12", 
              min     = 0.00001, 
              step    = 0.00001),
            
            radioButtons(
              inputId      = ns("sseEstimationType"),
              label        = strong("Estimation Type"),
              choiceValues = list("Margin of Error",
                                  "Width of Interval"),
              choiceNames  = list("Margin of Error (\\( E\\)) ",
                                  "Width of Interval (\\( W\\))"),
              selected     = "Margin of Error",
              inline       = TRUE),
            
            
            
            conditionalPanel(
              ns = ns,
              condition = "input.sseEstimationType == 'Margin of Error'",
              
              numericInput(
                inputId = ns("sseMeanMargErr"),
                label   = strong("Margin of Error (\\( E\\))"),
                value   = "8", 
                min     = 0.00001, 
                step    = 0.01)
            ), #sseEstimationType == 'Margin of Error'
            
            conditionalPanel(
              ns = ns,
              condition = "input.sseEstimationType == 'Width of Interval'",
              
              numericInput(
                inputId = ns("sseMeanWoI"),
                label   = strong("Width of Interval (\\( W\\))"),
                value   = "16", 
                min     = 0.00001, 
                step    = 0.01)
            ), #sseEstimationType == 'Width of Interval'
            actionButton(
              inputId = ns("goSampSizeEst"), 
              label   = "Calculate",
              class = "act-btn"),
            
            actionButton(
              inputId = ns("resetSampSizeEst"), 
              label   = "Reset Values",
              class = "act-btn"),
          ), #sampSizeEstParameter == 'Population Mean'
          
          # Population Proportion + (MoE or WoI)                                             
          conditionalPanel(
            ns = ns,
            condition = "input.estimateParameter == 'Sample Size' && input.sampSizeEstParameter == 'Population Proportion'",
            
            radioButtons(
              inputId  = ns("confLeveln"),
              label    = strong("Confidence Level (\\( 1- \\alpha\\))"),
              choices  = c("90%", 
                           "95%",
                           "99%"),
              selected = "95%",
              inline   = TRUE),
            
            numericInput(
              inputId = ns("sseTargetProp"),
              label   = strong("Target Proportion (\\( \\hat{p} \\))"),
              value   = "0.5", 
              min     = 0.00001, 
              step    = 0.01),
            
            checkboxInput(
              inputId = ns("normalDistribution"),
              label   = "Assume data follows a normal distribution",
              value   = TRUE
            ),
            
            
            radioButtons(
              inputId      = ns("sseEstimationTypeProp"),
              label        = strong("Estimation Type"),
              choiceValues = list("Margin of Error",
                                  "Width of Interval"),
              choiceNames  = list("Margin of Error (\\( E\\)) ",
                                  "Width of Interval (\\( W\\))"),
              selected     = "Margin of Error",
              inline       = TRUE),
            
            conditionalPanel(
              ns = ns,
              condition = "input.sseEstimationTypeProp == 'Margin of Error'",
              
              numericInput(
                inputId = ns("ssePropMargErr"),
                label   = strong("Margin of Error (\\( E\\))"),
                value   = "0.01", 
                min     = 0.00001, 
                step    = 0.01)
            ), #sseEstimationType == 'Margin of Error'
            
            conditionalPanel(
              ns = ns,
              condition = "input.sseEstimationTypeProp == 'Width of Interval'",
              
              numericInput(
                inputId = ns("ssePropWoI"),
                label   = strong("Width of Interval (\\( W\\))"),
                value   = "0.02", 
                min     = 0.00001, 
                step    = 0.01)
            ), #sseEstimationType == 'Width of Interval'
            
            actionButton(
              inputId = ns("goSampSizeEst"), 
              label   = "Calculate",
              class = "act-btn"),
            
            actionButton(
              inputId = ns("resetSampSizeEst"), 
              label   = "Reset Values",
              class = "act-btn"),
          ), #sampSizeEstParameter == 'Population Proportion'
          
          
          
          
          
        ) #inputPanel
      ), #sidebarPanel
      
      #  ========================================================================= #  
      ## -------- Main Panel ----------------------------------------------------- 
      #  ========================================================================= #
      mainPanel(
        div(
          id = ns("ssEstimationMP"),
          
          uiOutput(ns("ssEstimationValidation")),
          
          div(
            id = ns("ssEstimationData"),
            
            #### ------------ Samp Size Mean Est -----------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.estimateParameter == 'Sample Size' && input.sampSizeEstParameter == 'Population Mean'",
              
              #titlePanel(tags$u("Sample Size Estimate (\\( n \\))")),
              #br(),
              uiOutput(ns('sampSizeMeanEstimate')),
              br(),
            ), #sampSizeEstParameter == Population Mean
            
            #### ------------ Samp Size Prop Est -----------------------------------------
            conditionalPanel( 
              ns = ns,
              condition = "input.estimateParameter == 'Sample Size' && input.sampSizeEstParameter == 'Population Proportion'",
              
              #titlePanel(tags$u("Sample Size Estimate (\\( n \\))")),
              #br(),
              uiOutput(ns('sampSizePropEstimate')),
              br(),
            ) #sampSizeEstParameter == Population Proportion
          ) #SSEstimationData
        ), #ssEstimationMP
        
        div(
          id = ns("ccEstimationMP"),
          
          uiOutput(ns("ccEstimationValidation")),
          
          div(
            id = ns("ccEstimationData"), 
            
            uiOutput(ns("ccEstimationValidation")),
            
            conditionalPanel(
              ns = ns,
              #condition = "input.estimateParameter == 'Confidence Coefficient' && input.sampSizeEstParameter == 'Population Mean'",
              condition = "input.sampSizeEstParameter == 'Population Mean'",
              uiOutput(ns('ccMeanEstimate')),
              br(),
            ), #confidence coefficient est population mean

            conditionalPanel(
              ns = ns,
              #condition = "input.estimateParameter == 'Confidence Coefficient' && input.sampSizeEstParameter == 'Population Proportion'",
              
              condition = "input.sampSizeEstParameter == 'Population Proportion'",
              uiOutput(ns('ccPropEstimate')),
              br(),
            ) #confidence coefficient est population prop
          ), #ccEstimationData 
          
          
          
        ) #ccEstimationMP
      ) #mainPanel
    ) #sidebarLayout
  ) # UI tagList
}

# =========================================================================== #  
# ---- Server Components ---------------------------------------------------- 
# =========================================================================== #

sampSizeEstServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #  ========================================================================= #
    ## -------- Data Validation ------------------------------------------------
    #  ========================================================================= #
    sse_iv <- InputValidator$new()
    sseMean_iv <- InputValidator$new()
    sseMeanMargin_iv <- InputValidator$new()
    sseMeanWidth_iv <- InputValidator$new()
    sseProp_iv <- InputValidator$new()
    ssePropMargin_iv <- InputValidator$new()
    ssePropWidth_iv <- InputValidator$new()
    
    #### -------- confidence coefficients
    
    cce_iv <- InputValidator$new()
    cceSampSize_iv <- InputValidator$new() 
    cceMeanMargin_iv <- InputValidator$new() 
    ccePopSD_iv <- InputValidator$new() 
    cceProp_iv <- InputValidator$new()
    cceMeanMargin_iv <- InputValidator$new() 
    cceMeanWidth_iv <- InputValidator$new()
    ccePropSampSize_iv <- InputValidator$new() 
    ccePropMargin_iv <- InputValidator$new() 
    ccePropWidth_iv <- InputValidator$new() 
    
    ### ------------ Rules -------------------------------------------------------
    
    #### ---------------- popuSD 
    sseMean_iv$add_rule("ssePopuSD", sv_required())
    sseMean_iv$add_rule("ssePopuSD", sv_gt(0))
    sseMeanMargin_iv$add_rule("sseMeanMargErr", sv_required())
    sseMeanMargin_iv$add_rule("sseMeanMargErr", sv_gt(0))
    sseMeanWidth_iv$add_rule("sseMeanWoI", sv_required())
    sseMeanWidth_iv$add_rule("sseMeanWoI", sv_gt(0))
    
    #### ---------------- targetProp 
    sseProp_iv$add_rule("sseTargetProp", sv_required())
    sseProp_iv$add_rule("sseTargetProp", sv_gt(0))
    sseProp_iv$add_rule("sseTargetProp", sv_lt(1))
    ssePropMargin_iv$add_rule("ssePropMargErr", sv_required())
    ssePropMargin_iv$add_rule("ssePropMargErr", sv_gt(0))
    ssePropMargin_iv$add_rule("ssePropMargErr", sv_lte(1))
    ssePropWidth_iv$add_rule("ssePropWoI", sv_required())
    ssePropWidth_iv$add_rule("ssePropWoI", sv_gt(0))
    ssePropWidth_iv$add_rule("ssePropWoI", sv_lte(1))
    
    #### ------- confidence coefficient popMean 
    cceSampSize_iv$add_rule("confSampSize", sv_required())
    cceSampSize_iv$add_rule("confSampSize", sv_gt(1))
    ccePopSD_iv$add_rule("confPopSD", sv_required())
    ccePopSD_iv$add_rule("confPopSD",sv_gt(0))
    cceMeanMargin_iv$add_rule("ccMargErr", sv_required())
    cceMeanMargin_iv$add_rule("ccMargErr",sv_gt(0))
    cceMeanWidth_iv$add_rule("ccMeanWoI", sv_required())
    cceMeanWidth_iv$add_rule("ccMeanWoI",sv_gt(0))
    
    #### ------ confidence coefficient popProp 
    
    ccePropSampSize_iv$add_rule("ccPropSampSize", sv_required())
    ccePropSampSize_iv$add_rule("ccPropSampSize", sv_gt(1))
    cceProp_iv$add_rule("ccTargetProp", sv_required())
    cceProp_iv$add_rule("ccTargetProp",sv_gt(0))
    ccePropMargin_iv$add_rule("ccPropMargErr", sv_required())
    ccePropMargin_iv$add_rule("ccPropMargErr",sv_gt(0))
    ccePropMargin_iv$add_rule("ccPropMargErr",sv_lte(1))
    ccePropWidth_iv$add_rule("ccPropWoI", sv_required())
    ccePropWidth_iv$add_rule("ccPropWoI",sv_gt(0))
    ccePropWidth_iv$add_rule("ccPropWoI",sv_lte(1))
    
    ### ------------ Conditions --------------------------------------------------
    
    sseMean_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Mean'))
    sseMeanMargin_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Mean' &&
                                           input$sseEstimationType == 'Margin of Error'))
    sseMeanWidth_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Mean' &&
                                          input$sseEstimationType == 'Width of Interval'))
    
    sseProp_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Proportion'))
    ssePropMargin_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Proportion' &&
                                           input$sseEstimationTypeProp == 'Margin of Error'))
    ssePropWidth_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Proportion' &&
                                          input$sseEstimationTypeProp == 'Width of Interval'))
    
    
    
    ### ------------ Dependencies ------------------------------------------------
    
    sse_iv$add_validator(sseMean_iv)
    sse_iv$add_validator(sseMeanMargin_iv)
    sse_iv$add_validator(sseMeanWidth_iv)
    
    sse_iv$add_validator(sseProp_iv)
    sse_iv$add_validator(ssePropMargin_iv)
    sse_iv$add_validator(ssePropWidth_iv)
    
    cce_iv$add_validator(cceSampSize_iv)
    cce_iv$add_validator(ccePopSD_iv)
    cce_iv$add_validator(cceMeanMargin_iv)
    cce_iv$add_validator(cceMeanWidth_iv)
    
    cce_iv$add_validator(ccePropSampSize_iv)
    cce_iv$add_validator(cceProp_iv)
    cce_iv$add_validator(ccePropMargin_iv)
    cce_iv$add_validator(ccePropWidth_iv)
    
    ### ------------ Activation --------------------------------------------------
    
    sse_iv$enable()
    
    sseMean_iv$enable()
    sseMeanMargin_iv$enable()
    sseMeanWidth_iv$enable()
    
    sseProp_iv$enable()
    ssePropMargin_iv$enable()
    ssePropWidth_iv$enable()
    
    cce_iv$enable() 
    
    cceSampSize_iv$enable() 
    ccePopSD_iv$enable() 
    cceMeanMargin_iv$enable() 
    cceMeanWidth_iv$enable() 
    
    ccePropSampSize_iv$enable() 
    cceProp_iv$enable() 
    ccePropMargin_iv$enable() 
    ccePropWidth_iv$enable() 
    
    #  ========================================================================= #
    ## -------- Functions ------------------------------------------------------
    #  ========================================================================= #
    getSampSizeEstMean <- function(critVal, popuSD, margErr, widthInt) {
      if(input$sseEstimationType == "Width of Interval"){
        n <- ((2 * critVal * popuSD) / widthInt) ^ 2
      }
      else {
        n <- ((critVal * popuSD) / margErr) ^ 2
      }
      return(n)
    }
    
    
    getSampSizeEstProp <- function(critVal, phat, margErr, widthInt) {
      if(input$sseEstimationTypeProp == "Width of Interval"){
        n <- phat * (1 - phat) * (critVal / (widthInt/2))^2
      }
      else{
        n <- phat * (1 - phat) * (critVal / margErr)^2
      }
      return(n)
    }
    
    
    #  ========================================================================= #
    ## -------- Reactives ------------------------------------------------------
    #  ========================================================================= #
    criticalValue <- reactive({
      
      if(input$confLeveln == "90%") {
        critVal <- 1.645
      } else if(input$confLeveln == "95%") {
        critVal <- 1.96
      } else if(input$confLeveln == "99%") {
        critVal <- 2.576
      }
      return(critVal)
    })
    
    
    #  ========================================================================= #
    ## -------- Observers ------------------------------------------------------
    #  ========================================================================= #
    
    ### ------------ Outputs -----------------------------------------------------
    
    #### ---------------- Validation ---------------------------------------------
    output$ssEstimationValidation <- renderUI({
      if (!sse_iv$is_valid()) {
        
        # Population Mean
        if (input$sampSizeEstParameter == 'Population Mean') {
          validate(
            need(input$ssePopuSD, "Population Standard Deviation is required.") %then%
              need(input$ssePopuSD > 0, "Population Standard Deviation must be positive."),
            
            if (input$sseEstimationType == 'Margin of Error') {
              need(input$sseMeanMargErr, "Margin of Error is required.") %then%
                need(input$sseMeanMargErr > 0, "Margin of Error must be positive.")
            },
            if (input$sseEstimationType == 'Width of Interval') {
              need(input$sseMeanWoI, "Width of Interval is required.") %then%
                need(input$sseMeanWoI > 0, "Width of Interval must be positive.")
            },
            errorClass = "myClass"
          )
        }
        
        # Population Proportion
        else if (input$sampSizeEstParameter == 'Population Proportion') {
          validate(
            need(input$sseTargetProp, "Target Proportion is required.") %then%
              need(input$sseTargetProp > 0 && input$sseTargetProp < 1,
                   "Target Proportion must be greater than 0 and less than 1."),
            
            if (input$sseEstimationTypeProp == 'Margin of Error') { 
              need(input$ssePropMargErr, "Margin of Error is required.") %then%
                need(input$ssePropMargErr > 0 && input$ssePropMargErr <= 1,
                     "Margin of Error must be greater than 0 and less than or equal to 1.")
            },
            if (input$sseEstimationTypeProp == 'Width of Interval') {
              need(input$ssePropWoI, "Width of Interval is required.") %then%
                need(input$ssePropWoI > 0 && input$ssePropWoI <= 1,
                     "Width of Interval must be greater than 0 and less than or equal to 1.")
            },
            errorClass = "myClass"
          )
        }
      }
    })
    
    output$ccEstimationValidation <- renderUI({
      if(!cce_iv$is_valid()){
        
        #Population Mean
        if(input$estimateParameter == 'Confidence Coefficient' && input$sampSizeEstParameter == 'Population Mean'){
          validate(
            need(input$confSampSize, "Sample Size is required") %then% 
              need(input$confSampSize, "Sample Size must be positive"), 
            
            need(input$confPopSD, "Population Standard Deviation is required" ) %then%
              need(input$confPopSD, "Population Standard Deviation must be positive"),
            
            
            if(input$ccEstimationType == "Margin of Error"){
              need(input$ccMargErr, "Margin of Error is required.") %then%
                need(input$ccMargErr, "Margin of Error must be positive.")
            },
            if(input$ccEstimationType == "Width of Interval"){
              need(input$ccMeanWoI, "Width of Interval is required") %then% 
                need(input$ccMeanWoI, "Width of Interval must be positive.")
            },
            errorClass = "myClass"
          )
        }
        
        #Population Proportion
        else if(input$estimateParameter == 'Confidence Coefficient' && input$sampSizeEstParameter == 'Population Proportion'){
          validate(
            need(input$ccPropSampSize ,"Sample Size is required") %then% 
              need(input$ccPropSampSize, "Sample Size must be positive"),
            
            need(input$ccTargetProp, "Target Proportion is required.") %then% 
              need(input$ccTargetProp, "Target Proportion must be greater than 0 and less than 1."), 
            
            if(input$ccPropEstimationType == "Margin of Error"){
              need(input$ccPropMargErr, "Margin of Error is required.") %then% 
                need(input$ccPropMargErr, "Margin of Error must be greater than 0 and less than or equal to 1.") 
              
            },
            if(input$ccPropEstimationType == "Width of Interval"){
              need(input$ccPropMeanWoI,"Width of Interval is required.") %then%
                need(input$ccPropMeanWoI, "Width of Interval must be greater than 0 and less than or equal to 1.")
            }
            
          )
        }
        
      }
    })
    
    #### ---------------- Mean Estimate output -----------------------------------
    output$sampSizeMeanEstimate <- renderUI({
      
      n <- getSampSizeEstMean(criticalValue(), input$ssePopuSD, input$sseMeanMargErr, input$sseMeanWoI)
      nEstimate <- ceiling(n)
      
      tagList(
        withMathJax(),
        br(),
        
        # Print Population Mean formula for SSE using Margin of Error
        if(input$sseEstimationType == "Margin of Error"){
          list(
            sprintf("\\( n = \\left( \\dfrac{Z_{\\alpha / 2} \\: \\sigma}{E} \\right)^{2} \\)"),
            sprintf("\\( = \\left( \\dfrac{ (%s)(%s) }{%s} \\right)^{2} \\)",
                    criticalValue(),
                    input$ssePopuSD,
                    input$sseMeanMargErr),
            sprintf("\\( = %0.4f \\)",
                    n)
          )
        }
        # Print Population Mean formula for SSE using Width of Interval
        else {
          list(
            sprintf("\\( n = \\left( \\dfrac{(2)Z_{\\alpha / 2} \\: \\sigma}{W} \\right)^{2} \\)"),
            sprintf("\\( = \\left( \\dfrac{ (2)(%s)(%s) }{%s} \\right)^{2} \\)",
                    criticalValue(),
                    input$ssePopuSD,
                    input$sseMeanWoI),
            sprintf("\\( = %0.4f \\)",
                    n)
          )
        },
        
        br(),
        br(),
        sprintf("\\( n \\approx %1.0f \\)",
                nEstimate),
        br(),
        br(),
        sprintf("The recommended sample size (\\( n \\)) is \\(%1.0f\\) for a \\( %s \\)%% confidence 
                  level with a population standard deviation \\( (\\sigma) = %s\\) and
                  a ",
                nEstimate,
                input$confLeveln,
                input$ssePopuSD),
        
        # Print blurb with Margin of Error
        if(input$sseEstimationType == "Margin of Error"){
          list(
            sprintf("margin of error \\( (E) = %s \\).", input$sseMeanMargErr),
            br()
          )
        }
        # Print blurb with Width of Interval
        else{
          list(
            sprintf("width of interval \\( (W) = %s \\).", input$sseMeanWoI),
            br()
          )
        }
      ) #tagList
    })
    
    #### ---------------- Proportion Estimate output -----------------------------
    output$sampSizePropEstimate <- renderUI({
      req(sse_iv$is_valid())
      
      if(isTRUE(input$normalDistribution)) {
        n <- getSampSizeEstProp(criticalValue(), input$sseTargetProp, input$ssePropMargErr, input$ssePropWoI)
        nEstimate <- ceiling(n)
        
        tagList(
          withMathJax(),
          br(),
          
          # Print Population Proportion formula for SSE using Margin of Error
          if(input$sseEstimationTypeProp == "Margin of Error"){
            list(
              sprintf("\\( n = \\hat{p} (1 - \\hat{p}) \\left( \\dfrac{Z_{\\alpha / 2}}{E} \\right)^{2} \\)"),
              sprintf("\\( = \\; %s \\; (%s) \\left( \\dfrac{%s}{%s} \\right)^{2} \\)",
                      input$sseTargetProp,
                      1 - input$sseTargetProp,
                      criticalValue(),
                      input$ssePropMargErr),
              sprintf("\\( = \\; %0.4f \\)",
                      n)
            )
          }
          # Print Population Proportion formula for SSE using Width of Interval
          else {
            list(
              sprintf("\\( n = \\hat{p} (1 - \\hat{p}) \\left( \\dfrac{(2)Z_{\\alpha / 2}}{W} \\right)^{2} \\)"),
              sprintf("\\( = \\; %s \\; (%s) \\left( \\dfrac{(2)%s}{(%s)} \\right)^{2} \\)",
                      input$sseTargetProp,
                      1 - input$sseTargetProp,
                      criticalValue(),
                      input$ssePropWoI),
              sprintf("\\( = \\; %0.4f \\)",
                      n)
            )
          },
          
          br(),
          br(),
          sprintf("\\( n \\approx %1.0f \\)",
                  nEstimate),
          br(),
          br(),
          sprintf("The recommended sample size (\\( n \\)) is \\(%1.0f\\) for a \\( %s \\)%% confidence 
                level with a target proportion \\( (\\hat{p}) = %s\\) and a ",
                  nEstimate,
                  input$confLeveln,
                  input$sseTargetProp),
          if(input$sseEstimationTypeProp == "Margin of Error"){
            list(
              sprintf("margin of error \\( (E) = %s \\).", input$ssePropMargErr),
              br()
            )
          }
          else{
            list(
              sprintf("width of interval \\( (W) = %s \\).", input$ssePropWoI),
              br()
            )
          }
        ) #tagList
      } else {
        
        conf.level = switch(input$confLeveln,
                            "90%" = 0.90,
                            "95%" = 0.95,
                            "99%" = 0.99)
        
        if (input$sseEstimationTypeProp == "Margin of Error") {
          E <- input$ssePropMargErr
        } else {
          E <- input$ssePropWoI / 2
        }
        
        n <- sample_size_clopper_pearson(
          p0 = input$sseTargetProp,
          conf.level = conf.level,
          margin.error = E
        )
        
        nEstimate <- ceiling(n)
        
        tagList(
          withMathJax(),
          br(),
          
          sprintf("\\( n = %d \\)", nEstimate),
          br(),
          br(),
          tags$em("Note: When the data cannot be assumed to follow a normal distribution, there isn’t a simple formula to calculate the required sample size. Instead, the sample size is found by testing different values until the exact confidence interval is narrow enough and meets the chosen confidence level. This approach is based on the Clopper–Pearson exact method for binomial proportions.")
        )
      }
    })
    
    #### ----- Confidence Coefficient Mean Estimate output
    
    output$ccMeanEstimate <- renderUI({
      
      tagList(
        withMathJax(),
        br(),
        
        if(input$ccEstimationType == "Margin of Error"){
          
          confCoe <- confidence_coefficient_mean(
            n = input$confSampSize,
            sigma = input$confPopSD,
            margin.error = input$ccMargErr
          )
          
          list(
            sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha) = 2 \\cdot \\Phi\\left( \\frac{E \\cdot \\sqrt{n}}{\\sigma}\\right) - 1\\)"),
            br(),
            sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha) = 2 \\cdot \\Phi\\left( \\frac{\\left(\\frac{%s}{2}\\right) \\cdot \\sqrt{%s}}{%s} \\right) - 1\\)",
                    input$ccMargErr,
                    input$confSampSize,
                    input$confPopSD),
            br(),
            sprintf( "\\(\\text{Confidence Coefficient} = %.0f\\%%\\)",
                     confCoe * 100),
            
            
            br(),
            br(),
            
            
            sprintf("Given the sample size of \\(n\\) is %s, an anticipated standard deviation of %s, and the desired margin of error of
                  %s, the resulting confidence coefficient is %.0f%%. In other words, a %.0f%% confidence interval constructed using the \\(\\sigma\\) known
                  would achieve the specified precision.",
                    input$confSampSize,
                    input$confPopSD,
                    input$ccMargErr,
                    confCoe* 100,
                    confCoe* 100)
          )
          
    
          
        }
        
        else{
          
          confCoe <- confidence_coefficient_mean(
            n = input$confSampSize,
            sigma = input$confPopSD,
            width = input$ccMeanWoI
          )
          
          list(
            sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha) = 2 \\cdot \\Phi\\left( \\frac{W \\cdot \\sqrt{n}}{\\sigma}\\right) - 1\\)"),
            br(),
            sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha) = 2 \\cdot \\Phi\\left(\\frac{\\left(\\frac{%s}{2}\\right) \\cdot \\sqrt{%s}}{%s}\\right) - 1\\)",
                    input$ccMeanWoI,
                    input$confSampSize,
                    input$confPopSD),
            br(),
            sprintf("\\(\\text{Confidence Coefficient} = %.0f\\%%\\)",
                    confCoe * 100),
            
            br(),
            br(),
            
            
            sprintf("Given the sample size of \\(n\\) is %s, an anticipated standard deviation of %s, and the desired width of interval of
                  %s, the resulting confidence coefficient is %.0f%%. In other words, a %.0f%% confidence interval constructed using the \\(\\sigma\\)) known
                  would achieve the specified precision.",
                    input$confSampSize,
                    input$confPopSD,
                    input$ccMeanWoI,
                    confCoe* 100,
                    confCoe* 100)
          )
          
          
        }
      )
      
    })
    
    #### ------ Confidence Coefficient Proportion Estimate output 
    
    output$ccPropEstimate <- renderUI({
      
      if(isTRUE(input$propNormalDistribution)){
        
        tagList(
          withMathJax(),
          br(),
          
          if(input$ccPropEstimationType == "Margin of Error"){
            
            confCoe <- confidence_coefficient_proportion(
              n = input$ccPropSampSize,
              p0 = input$ccTargetProp,
              margin.error = input$ccPropMargErr
            )
            
            list(
              sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha) = 2 \\cdot \\Phi\\left( \\frac{E \\cdot \\sqrt{n}} {\\sqrt{p(1 - p)}} \\right) - 1 \\)"),
              br(),
              sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha) = 2 \\cdot \\Phi\\left(\\frac{%s \\cdot \\sqrt{%s}}{\\sqrt{%s(1 - %s)}}\\right) - 1\\)",
                      input$ccPropMargErr,
                      input$ccPropSampSize,
                      input$ccTargetProp,
                      input$ccTargetProp),
              br(),
              sprintf("\\(\\text{Confidence Coefficient} = %.0f\\%%\\)",
                      confCoe * 100),
              
              br(),
              br(),
              
              
              sprintf("Given a sample size of \\(n\\) is %s, an anticipated proportion of %s, and a desired margin of error of %s, the resulting confidence coefficient is
                    %.0f%%. In other words, a %.0f%% confidence interval constructed using the normal approximation would achieve the specified precision",
                      input$ccPropSampSize,
                      input$ccTargetProp,
                      input$ccPropMargErr,
                      confCoe* 100,
                      confCoe* 100)
            )
            
            
            
          }
          else{
            
            confCoe <- confidence_coefficient_proportion(
              n = input$ccPropSampSize,
              p0 = input$ccTargetProp,
              width = input$ccPropWoI
            )
            
            list(
              sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha) = 2 \\cdot \\Phi\\left( \\frac{W \\cdot \\sqrt{n}} {2 \\cdot \\sqrt{p(1 - p)}} \\right) - 1\\)"),
              br(),
              sprintf("\\(\\text{Confidence Coefficient } (1 - \\alpha)=2 \\cdot \\Phi\\left(\\frac{%s \\cdot \\sqrt{%s}}{2 \\cdot \\sqrt{%s(1 - %s)}} \\right) - 1\\)",
                      input$ccPropWoI,
                      input$ccPropSampSize,
                      input$ccTargetProp,
                      input$ccTargetProp),
              br(),
              sprintf("\\(\\text{Confidence Coefficient} = %.0f\\%%\\)",
                      confCoe * 100),
              
              br(),
              br(),
              
              
              sprintf("Given a sample size of \\(n\\) is %s, an anticipated proportion of %s, and a desired width of interval of %s, the resulting confidence coefficient is %.0f%%.
                    In other words, a %.0f%% confidence interval constructed using the normal approximation would achieve the specified precision.",
                      input$ccPropSampSize,
                      input$ccTargetProp,
                      input$ccPropWoI,
                      confCoe* 100,
                      confCoe* 100
              )
            )
            
            
            
          }
        )
        
      }
      else {
        if(input$ccPropEstimationType == "Margin of Error"){
          E <- input$ccPropMargErr
        }
        else{
          E <- input$ccPropWoI / 2
        }
        confCoe <- confidence_coefficient_cp(input$ccPropSampSize, input$ccTargetProp,margin.error = E)
        
        tagList(
          withMathJax(),
          br(),
          sprintf("\\(\\text{Confidence Coefficient} = %.0f\\%%\\)", confCoe*100),
          br(),
          br(),
          tags$em("Note: When the data cannot be assumed to follow a normal distribution, there isn’t a simple formula to calculate the required sample size. Instead, the confidence interval is found by testing different values until the exact interval width is narrow enough and meets the target width. This approach is based on the Clopper–Pearson exact method for binomial proportions.")
        )
      }
      
    })
    
    ### ------------ Component Display -------------------------------------------
    
    observeEvent(input$goSampSizeEst, {
      if(sse_iv$is_valid()) {
        show(id = "ssEstimationData")
      } else {
        hide(id = "ssEstimationData")
      }
    })
    
    observeEvent(!sse_iv$is_valid(), {
      hide(id = "ssEstimationMP")
      hide(id = "ssEstimationData")
    })
    
    observeEvent({input$sampSizeEstParameter
      input$popuSDSampSizeEst
      input$targetPropSampSizeEst
      input$margErrSampSizeEst}, {
        hide(id = "ssEstimationData")
      })
    
    observeEvent(input$goSampSizeEst, {
      show(id = "ssEstimationMP")
    })
    
    observeEvent(input$resetSampSizeEst, {
      hide(id = "ssEstimationMP")
      shinyjs::reset("sampSizeEstPanel")
    })
    
    observeEvent(input$goConfidCoeEst, {

      if (cce_iv$is_valid()) {

        shinyjs::hide(
          id = session$ns("ccEstimationData"),
          asis = TRUE
        )

      } else {

        shinyjs::show(
          id = session$ns("ccEstimationData"),
          asis = TRUE
        )

      }
    })
    
    observeEvent(input$resetConfidCoeEst, {

      shinyjs::hide(
        id = session$ns("ccEstimationData"),
        asis = TRUE
        )
        
      
    })
    
    
    
  })
}
