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
            inputId      = ns("sampSizeEstParameter"),
            label        = strong("Parameter of Interest"),
            choiceValues = list("Population Mean",
                                "Population Proportion"),
            choiceNames  = list("Population Mean (\\( \\mu \\)) ",
                                "Population Proportion (\\( p\\))"),
            selected     = "Population Mean", #character(0), #
            inline       = TRUE),
          
          radioButtons(
            inputId  = ns("confLeveln"),
            label    = strong("Confidence Level (\\( 1- \\alpha\\))"),
            choices  = c("90%", 
                         "95%",
                         "99%"),
            selected = c("95%"),
            inline   = TRUE),
          
          # 4 Options: Mean & Margin of Error, Mean & Width of Interval,
          #            Prop & Margin of Error, Prop & Width of Interval
          
          # Population Mean + (MoE or WoI)
          
          conditionalPanel(
            ns = ns,
            condition = "input.sampSizeEstParameter == 'Population Mean'",
            
            numericInput(
              inputId = ns("ssePopuSD"),
              label   = strong("Population Standard Deviation (\\( \\sigma\\))"),
              value   = "12", 
              min     = 0.00001, 
              step    = 0.00001)
          ), #sampSizeEstParameter == 'Population Mean'
          
          # Population Proportion + (MoE or WoI)                                             
          conditionalPanel(
            ns = ns,
            condition = "input.sampSizeEstParameter == 'Population Proportion'",
            
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
            )
          ), #sampSizeEstParameter == 'Population Proportion'
          
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
              inputId = ns("sseMargErr"),
              label   = strong("Margin of Error (\\( E\\))"),
              value   = "8", 
              min     = 0.00001, 
              step    = 0.01)
          ), #sseEstimationType == 'Margin of Error'
          
          conditionalPanel(
            ns = ns,
            condition = "input.sseEstimationType == 'Width of Interval'",
            
            numericInput(
              inputId = ns("sseWoI"),
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
            class = "act-btn")
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
              condition = "input.sampSizeEstParameter == 'Population Mean'",
                                    
              titlePanel(tags$u("Sample Size Estimate (\\( n \\))")),
              br(),
              uiOutput(ns('sampSizeMeanEstimate')),
              br(),
            ), #sampSizeEstParameter == Population Mean
                  
 #### ------------ Samp Size Prop Est -----------------------------------------
            conditionalPanel( 
              ns = ns,
              condition = "input.sampSizeEstParameter == 'Population Proportion'",
                                  
              titlePanel(tags$u("Sample Size Estimate (\\( n \\))")),
              br(),
              uiOutput(ns('sampSizePropEstimate')),
              br(),
            ) #sampSizeEstParameter == Population Proportion
          ) #SSEstimationData
        ) #ssEstimationMP
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
    sseMargin_iv <- InputValidator$new()
    sseWidth_iv <- InputValidator$new()
    sseProp_iv <- InputValidator$new()

 ### ------------ Rules -------------------------------------------------------
    
 #### ---------------- popuSD 
    sseMean_iv$add_rule("ssePopuSD", sv_required())
    sseMean_iv$add_rule("ssePopuSD", sv_gt(0))
    sseMargin_iv$add_rule("sseMargErr", sv_required())
    sseMargin_iv$add_rule("sseMargErr", sv_gt(0))
    sseWidth_iv$add_rule("sseWoI", sv_required())
    sseWidth_iv$add_rule("sseWoI", sv_gt(0))
    
 #### ---------------- targetProp 
    sseProp_iv$add_rule("sseTargetProp", sv_required())
    sseProp_iv$add_rule("sseTargetProp", sv_gt(0))
    sseProp_iv$add_rule("sseTargetProp", sv_lt(1))

 ### ------------ Conditions --------------------------------------------------
    
    sseMean_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Mean'))
    
    sseProp_iv$condition( ~ isTRUE(input$sampSizeEstParameter == 'Population Proportion'))

    sseMargin_iv$condition( ~ isTRUE(input$sseEstimationType == 'Margin of Error'))
    sseWidth_iv$condition( ~ isTRUE(input$sseEstimationType == 'Width of Interval'))
 ### ------------ Dependencies ------------------------------------------------
    
    sse_iv$add_validator(sseMean_iv)
    sse_iv$add_validator(sseMargin_iv)
    sse_iv$add_validator(sseWidth_iv)
    
    sse_iv$add_validator(sseProp_iv)
    
 ### ------------ Activation --------------------------------------------------
    
    sse_iv$enable()
    
    sseMean_iv$enable()
    sseMargin_iv$enable()
    sseWidth_iv$enable()
    
    sseProp_iv$enable()
    
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
      if(input$sseEstimationType == "Width of Interval"){
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
        
        # Common validation for Margin of Error or Width of Interval depending on selected estimation type
        if (input$sseEstimationType == "Margin of Error") {
          validate(
            need(input$sseMargErr, "Margin of Error is required.") %then%
              need(input$sseMargErr > 0, "Margin of Error must be positive."),
            errorClass = "myClass"
          )
        } else if (input$sseEstimationType == "Width of Interval") {
          validate(
            need(input$sseWoI, "Width of Interval is required.") %then%
              need(input$sseWoI > 0, "Width of Interval must be positive."),
            errorClass = "myClass"
          )
        }
        
        # Population Mean specific validation
        if (input$sampSizeEstParameter == 'Population Mean') {
          validate(
            need(input$ssePopuSD, "Population Standard Deviation is required.") %then%
              need(input$ssePopuSD > 0, "Population Standard Deviation must be positive."),
            errorClass = "myClass"
          )
        }
        
        # Population Proportion specific validation
        if (input$sampSizeEstParameter == 'Population Proportion') {
          validate(
            need(input$sseTargetProp, "Target Proportion is required.") %then%
              need(input$sseTargetProp > 0 && input$sseTargetProp < 1, "Target Proportion must be between 0 and 1."),
            errorClass = "myClass"
          )
        }
      }
    })
    
 #### ---------------- Mean Estimate output -----------------------------------
    output$sampSizeMeanEstimate <- renderUI({
      req(sseMean_iv, sseMargin_iv, sseWidth_iv)
      n <- getSampSizeEstMean(criticalValue(), input$ssePopuSD, input$sseMargErr, input$sseWoI)
      nEstimate <- ceiling(n)
      
      tagList(
        withMathJax(),
        br(),
        br(),
        
        # Print Population Mean formula for SSE using Margin of Error
        if(input$sseEstimationType == "Margin of Error"){
          list(
            sprintf("\\( n = \\left( \\dfrac{Z_{\\alpha / 2} \\: \\sigma}{E} \\right)^{2} \\)"),
            sprintf("\\( = \\left( \\dfrac{ (%s)(%s) }{%s} \\right)^{2} \\)",
                    criticalValue(),
                    input$ssePopuSD,
                    input$sseMargErr),
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
                    input$sseWoI),
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
            sprintf("margin of error \\( (E) = %s \\)", input$sseMargErr),
            br()
          )
        }
        # Print blurb with Width of Interval
        else{
          list(
            sprintf("width of interval \\( (W) = %s \\)", input$sseWoI),
            br()
          )
        }
      ) #tagList
    })
    
 #### ---------------- Proportion Estimate output -----------------------------
    output$sampSizePropEstimate <- renderUI({
      req(sseProp_iv, sseMargin_iv, sseWidth_iv)
      
      if(isTRUE(input$normalDistribution)) {
        n <- getSampSizeEstProp(criticalValue(), input$sseTargetProp, input$sseMargErr, input$sseWoI)
        nEstimate <- ceiling(n)
        
        tagList(
          withMathJax(),
          br(),
          br(),
          
          # Print Population Proportion formula for SSE using Margin of Error
          if(input$sseEstimationType == "Margin of Error"){
            list(
              sprintf("\\( n = \\hat{p} (1 - \\hat{p}) \\left( \\dfrac{Z_{\\alpha / 2}}{E} \\right)^{2} \\)"),
              sprintf("\\( = \\; %s \\; (%s) \\left( \\dfrac{%s}{%s} \\right)^{2} \\)",
                      input$sseTargetProp,
                      1 - input$sseTargetProp,
                      criticalValue(),
                      input$sseMargErr),
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
                      input$sseWoI),
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
          br(),
          sprintf("The recommended sample size (\\( n \\)) is \\(%1.0f\\) for a \\( %s \\)%% confidence 
                level with a target proportion \\( (\\hat{p}) = %s\\) and a ",
                  nEstimate,
                  input$confLeveln,
                  input$sseTargetProp),
          if(input$sseEstimationType == "Margin of Error"){
            list(
              sprintf("margin of error \\( (E) = %s \\).", input$sseMargErr),
              br()
            )
          }
          else{
            list(
              sprintf("width of interval \\( (W) = %s \\).", input$sseWoI),
              br()
            )
          }
        ) #tagList
        
      } else { # Clopper-Pearson method (normalDistribution unchecked)
        
        conf.level = switch(input$confLeveln,
                            "90%" = 0.90,
                            "95%" = 0.95,
                            "99%" = 0.99)
       
        if (input$sseEstimationType == "Margin of Error") {
          n <- sample_size_clopper_pearson(
            p0 = input$sseTargetProp,
            conf.level,
            margin.error = input$sseMargErr
          )
        } else {
          n <- sample_size_clopper_pearson(
            p0 = input$sseTargetProp,
            conf.level, 
            width = input$sseWoI
          )
        }
        
        nEstimate <- ceiling(n)
        
        tagList(
          withMathJax(),
          br(),
          
          sprintf("\\( n = %d \\)", nEstimate),
          br(),
          br(),
          tags$em("* Note: There is no closed-form formula as there is in the case of the normal approximation. Instead, sample size was determined numerically by checking whether the resulting exact confidence interval satisfies the desired width and confidence level. This is called the Clopper-Pearson exact binomial method (also known as the exact confidence interval for a binomial proportion).")
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
  })
}
