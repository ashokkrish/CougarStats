# =========================================================================== #  
# ---- UI Components -------------------------------------------------------- 
# =========================================================================== #

confidCoefSidebarUI <- function(id){
  ns <- NS(id)
  
  tagList(
    
    withMathJax(),
    shinyjs::useShinyjs(), 
    
    radioButtons(
      inputId      = ns("confCoeEstParameter"),
      label        = strong("Parameter of Interest"),
      choiceValues = list("Population Mean",
                          "Population Proportion"),
      choiceNames  = list("Population Mean (\\( \\mu \\)) ",
                          "Population Proportion (\\( p\\))"),
      selected     = "Population Mean",
      inline       = TRUE),
    
    conditionalPanel(
      ns = ns,
      condition = "input.confCoeEstParameter == 'Population Mean'",
      
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
        inputId      = ns("ccEstimationType"),
        label        = strong("Estimation Type"),
        choiceValues = list("Margin of Error",
                            "Width of Interval"),
        choiceNames  = list("Margin of Error (\\( E\\)) ",
                            "Width of Interval (\\( W\\))"),
        selected     = "Margin of Error",
        inline       = TRUE),
      
      conditionalPanel(
        ns = ns,
        condition = "input.ccEstimationType == 'Margin of Error'",
        
        numericInput(
          inputId = ns("ccMargErr"),
          label   = strong("Margin of Error (\\( E\\))"),
          value   = "8", 
          min     = 0.00001, 
          step    = 0.01)
      ), #ccEstimationType == 'Margin of Error'
      
      conditionalPanel(
        ns = ns,
        condition = "input.ccEstimationType == 'Width of Interval'",
        
        numericInput(
          inputId = ns("ccMeanWoI"),
          label   = strong("Width of Interval (\\( W\\))"),
          value   = "16", 
          min     = 0.00001, 
          step    = 0.01)
      ), #ccEstimationType == 'Width of Interval'
      
      actionButton(
        inputId = ns("goConfidCoeEst"),
        label   = "Calculate",
        class   = "act-btn"), 
      
      actionButton(
        inputId = ns("resetConfidCoeEst"),
        label   = "Reset Values",
        class   = "act-btn")
    ), #confCoeEstParameter == 'Population Mean'
    
    # Population Proportion 
    conditionalPanel(
      ns = ns,
      condition = "input.confCoeEstParameter == 'Population Proportion'",
      
      numericInput(
        inputId = ns("ccPropSampSize"),
        label = strong("Sample Size \\(n\\)"),
        value = "18",
        min = 1, 
        step = 1),
      
      numericInput(
        inputId = ns("ccTargetProp"),
        label   = strong("Planning value for the Population Proportion (\\( p\\))"),
        value   = "0.5", 
        min     = 0.00001, 
        step    = 0.01),
      
      checkboxInput(
        inputId = ns("propNormalDistribution"),
        label   = "Assume data follows a normal distribution",
        value   = TRUE),
      
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
      ), #ccPropEstimationType == 'Margin of Error'
      
      conditionalPanel(
        ns = ns,
        condition = "input.ccPropEstimationType == 'Width of Interval'",
        
        numericInput(
          inputId = ns("ccPropWoI"),
          label   = strong("Width of Interval (\\( W\\))"),
          value   = "0.02", 
          min     = 0.00001, 
          step    = 0.01)
      ), #ccPropEstimationType == 'Width of Interval'
      
      actionButton(
        inputId = ns("goConfidCoeEst"),
        label = "Calculate",
        class = "act-btn"), 
      
      actionButton(
        inputId = ns("resetConfidCoeEst"),
        label = "Reset Values",
        class = "act-btn")
    )
  ) #tagList
}

ccEstimationMP <- function(id) {
  ns <- NS(id)
  
  shinyjs::hidden(
    div(
      id = ns("ccEstMP"),
      
      uiOutput(ns("ccEstimationValidation")),
      
      div(
        id = ns("ccEstData"), 
        
        #### ------------ Population Mean -----------------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.confCoeEstParameter == 'Population Mean'",
          
          uiOutput(ns('ccMeanEstimate')),
          br()
        ), #confCoeEstParameter == Population Mean
        
        #### ------------ Population Proportion -----------------------------------------
        conditionalPanel(
          ns = ns,
          condition = "input.confCoeEstParameter == 'Population Proportion'",
          
          uiOutput(ns('ccPropEstimate')),
          br()
        ) #confCoeEstParameter == Population Proportion
      )
    )
  )
}

# =========================================================================== #  
# ---- Server Components ---------------------------------------------------- 
# =========================================================================== #

confidenceCoefficientServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns 
    
    #  ========================================================================= #
    ## -------- Data Validation ------------------------------------------------
    #  ========================================================================= #
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
    
    cceSampSize_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Mean'))
    ccePopSD_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Mean'))
    cceMeanMargin_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Mean' &&
                                           input$ccEstimationType == 'Margin of Error'))
    cceMeanWidth_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Mean' &&
                                          input$ccEstimationType == 'Width of Interval'))
    
    cceProp_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Proportion'))
    ccePropSampSize_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Proportion'))
    ccePropMargin_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Proportion' &&
                                           input$ccPropEstimationType == 'Margin of Error'))
    ccePropWidth_iv$condition( ~ isTRUE(input$confCoeEstParameter == 'Population Proportion' &&
                                          input$ccPropEstimationType == 'Width of Interval'))
    
    ### ------------ Dependencies ------------------------------------------------
    
    cce_iv$add_validator(cceSampSize_iv)
    cce_iv$add_validator(ccePopSD_iv)
    cce_iv$add_validator(cceMeanMargin_iv)
    cce_iv$add_validator(cceMeanWidth_iv)
    
    cce_iv$add_validator(ccePropSampSize_iv)
    cce_iv$add_validator(cceProp_iv)
    cce_iv$add_validator(ccePropMargin_iv)
    cce_iv$add_validator(ccePropWidth_iv)
    
    ### ------------ Activation --------------------------------------------------
    
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
    ## -------- Observers ------------------------------------------------------
    #  ========================================================================= #
    
    ### ------------ Outputs -----------------------------------------------------
    
    #### ---------------- Validation ---------------------------------------------
    
    output$ccEstimationValidation <- renderUI({
      if(!cce_iv$is_valid()){
        
        # Population Mean
        if(input$confCoeEstParameter == 'Population Mean'){
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
        
        # Population Proportion
        else if(input$confCoeEstParameter == 'Population Proportion'){
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
    
    observeEvent(input$goConfidCoeEst, {
      if (cce_iv$is_valid()) {
        shinyjs::show("ccEstMP")
      } else {
        shinyjs::hide("ccEstMP")
      }
    })
    
    observeEvent(!cce_iv$is_valid(), {
        shinyjs::hide("ccEstMP")
    })
    
    observeEvent(input$resetConfidCoeEst, {
      shinyjs::hide("ccEstMP")
      shinyjs::reset("confidCoefSidebarUI")
    })
  })
}