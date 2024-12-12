singleRegressionCorrelationUI <- function(id) {
  ns <- NS(id)

  tagList(div(
    div(
      id = ns("SLRData"),
      tabsetPanel(
        id       = ns("slrTabset"),
        selected = "Simple Linear Regression",

        tabPanel(
          id    = ns("slr"),
          title = "Simple Linear Regression",

          conditionalPanel(
            ns = ns,
            condition = "input.scatterPlot == 1",

            titlePanel("Scatterplot"),
            br(),
            plotOptionsMenuUI(
              id          = ns("slrScatter"),
              plotType    = "Scatterplot",
              title       = "Scatterplot",
              xlab        = "x",
              ylab        = "y",
              dim         = "in px",
              includeFlip = FALSE),
            uiOutput(ns("renderSLRScatterplot")),
            br(),
            hr(),
            ), #scatterPlot == 1

          titlePanel("Data"),
          br(),
          DTOutput(ns("slrDataTable"), width = "750px"),
          br(),
          hr(),

          titlePanel("Estimated equation of the regression line"),
          br(),
          uiOutput(ns('regLineEquation')),

          ## verbatimTextOutput(ns("linearRegression")),
          ## br(),
          ## hr(),
          ##
          ## titlePanel("95% confidence interval for regression parameters"),
          ## br(),
          ## verbatimTextOutput(ns("confintLinReg")),
          ## br(),
          ## hr(),
          ##
          ## titlePanel("ANOVA for regression"),
          ## br(),
          ## verbatimTextOutput(ns("anovaLinReg")),
          ##br(),
          ), # slr tabpanel

#### ---------------- Normality of Residuals Tab -----------------------------
        ## tabPanel(
        ##   id    = ns("normality"),
        ##   title = "Normality of Residuals",
        ##
        ##   #----------------------------------#
        ##   # Tests for normality of residuals #
        ##   #----------------------------------#
        ##   titlePanel("Anderson-Darling test"),
        ##   verbatimTextOutput(ns("AndersonDarlingTest")),
        ##   br(),
        ##
        ##   titlePanel("Kolmogorov-Smirnov test"),
        ##   verbatimTextOutput(ns("KolmogorovSmirnovTest")),
        ##   br(),
        ##
        ##   titlePanel("Shapiro-Wilk test"),
        ##   verbatimTextOutput(ns("ShapiroTest")),
        ##   br(),
        ## ),

#### ---------------- Residual Plots Tab -------------------------------------
        ## tabPanel(
        ##   id    = ns("resid"),
        ##   title = "Residual Plots",
        ##   #-----------------------------#
        ##   # Plots for Residual Analysis #
        ##   #-----------------------------#
        ##   titlePanel("Q-Q plot"),
        ##   plotOutput(ns("qqplot"), width = "500px"),
        ##   br(),
        ##
        ##   titlePanel("Other diagnostic plots"),
        ##   plotOutput(ns("moreplots"), width = "500px"),
        ##   br(),
        ## ),


#### ---------------- Correlation Coefficient Analysis Tab -------------------
        tabPanel(
          id    = ns("correlation"),
          title = "Correlation Analysis",

          titlePanel("Pearson's Product-Moment Correlation"),
          br(),
          br(),
          uiOutput(ns('pearsonCorFormula')),
          br(),
          ## verbatimTextOutput(ns("PearsonCorTest")),
          ## br(),
          ## verbatimTextOutput(ns("PearsonConfInt")),
          ## br(),
          hr(),

          titlePanel("Kendall's Rank Correlation"),
          br(),
          uiOutput(ns("kendallEstimate")),
          br(),
          hr(),

          titlePanel("Spearman's Rank Correlation"),
          br(),
          uiOutput(ns("spearmanEstimate")),
          br(),
          br()
        ), #correlation tabPanel

        ), #slrTabset tabsetPanel
      ), #SLRData div
    ))
}

singleRegressionCorrelationServer <- function(id, uploadedTibble, selectedVariables) {
  moduleServer(id, function(input, output, session) {
    plotOptionsMenuServer("slrScatter")

    GetPlotHeight  <- function(plotToggle, pxValue, ui) {
      ifelse(plotToggle == 'in px' && !is.na(pxValue),
             height <- pxValue,
             height <- 400)

      ifelse(ui,
             return(paste0(height, "px")),
             return(height))
    }

    GetPlotWidth  <- function(plotToggle, pxValue, ui) {
      if(plotToggle == 'in px' && !is.na(pxValue)) {
        width <- pxValue

        if(ui) {
          width <- paste0(width, "px")
        }
      } else {
        width <- "auto"
      }
      return(width)
    }

### SLR Validation messages ----
    datx <- reactiveVal()
    daty <- reactiveVal()
    observe({
      req(isTruthy(uploadedTibble$data()))
      validate(need(length(selectedVariables$explanatoryVariables()) == 1,
                    "At least one explanatory variable must be selected to run a linear regression."))

      datx(uploadedTibble$data() %>% dplyr::select(-all_of(selectedVariables$explanatoryVariables())))
      daty(uploadedTibble$data() %>% dplyr::select(all_of(selectedVariables$responseVariable())))

      with(uploadedTibble$data(), {
        model <- lm(reformulate(selectedVariables$explanatoryVariables(), selectedVariables$responseVariable()))
      })

      message(dim(datx()))
      message(dim(daty()))

      
      df <- data.frame(datx(), daty(), datx()*daty(), datx()^2, daty()^2)
      names(df) <- c("x", "y", "xy", "x<sup>2</sup>", "y<sup>2</sup>")
      dfTotaled <- bind_rows(df, summarise(df, across(where(is.numeric), sum)))
      rownames(dfTotaled)[nrow(dfTotaled)] <- "Totals"
      sumXSumY <- dfTotaled["Totals", "x"] * dfTotaled["Totals", "y"]
      sumXSqrd <- dfTotaled["Totals", "x"] ^ 2
      sumYSqrd <- dfTotaled["Totals", "y"] ^ 2

      output$slrDataTable <- renderDT(
        datatable(round(dfTotaled, digits = 3),
                  options = list(pageLength = -1,
                                 lengthMenu = list(c(-1, 10, 25, 50, 100), c("All", "10", "25", "50", "100"))
                                 ),
                  escape = FALSE
                  ) %>% formatStyle(names(dfTotaled),
                                    target = 'row',
                                    fontWeight = styleRow(dim(dfTotaled)[1], "bold"))
      )

      output$renderSLRScatterplot <- renderUI({
        tagList(
          plotOutput(session$ns("slrScatterplot"),
                     height = GetPlotHeight(input[["slrScatter-Height"]], input[["slrScatter-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["slrScatter-Width"]], input[["slrScatter-WidthPx"]], ui = TRUE)),
          )
      })

      output$slrScatterplot <- renderPlot({ # scatterplot ----
        RenderScatterplot(df,
                          input[["slrScatter-Title"]],
                          input[["slrScatter-Xlab"]],
                          input[["slrScatter-Ylab"]],
                          input[["slrScatter-Colour"]],
                          input[["slrScatter-PointsColour"]],
                          input[["slrScatter-LineWidth"]],
                          input[["slrScatter-PointSize"]],
                          input[["slrScatter-Gridlines"]])

      }, height = function() {
        GetPlotHeight(input[["slrScatter-Height"]],
                      input[["slrScatter-HeightPx"]],
                      ui = FALSE)
      },
      width = function() {
        GetPlotWidth(input[["slrScatter-Width"]],
                     input[["slrScatter-WidthPx"]],
                     ui = FALSE)
      })

      if (summary(model)$coefficients["datx", "Estimate"] > 0) {
        slopeDirection <- "increase"
        yHatOp <- "+"
        b0HatOp <- "-"
      } else {
        slopeDirection <- "decrease"
        yHatOp <- "-"
        b0HatOp <- "+"
      }


      output$regLineEquation <- renderUI({
        interceptEstimate <- round(summary(model)$coefficients["(Intercept)", "Estimate"], 4)
        slopeEstimate <- round(summary(model)$coefficients["datx", "Estimate"], 4)

        withMathJax(
          p("The estimated equation of the regression line is given by "),
          sprintf("\\( \\qquad \\hat{y} = \\hat{\\beta}_{0} + \\hat{\\beta}_{1} x \\)"),
          br(),
          br(),
          p("where"),
          sprintf("\\( \\qquad \\hat{\\beta}_{1} = \\dfrac{ \\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } \\)"),
          sprintf("\\( \\, = \\, \\dfrac{ %g - \\dfrac{ (%g)(%g) }{ %g } }{ %g - \\dfrac{ (%g)^2 }{ %g } } \\)",
                  dfTotaled["Totals", "xy"],
                  dfTotaled["Totals", "x"],
                  dfTotaled["Totals", "y"],
                  length(datx()),
                  dfTotaled["Totals", "x<sup>2</sup>"],
                  dfTotaled["Totals", "x"],
                  length(datx())),
          sprintf("\\( \\, = \\, \\dfrac{ %g - \\dfrac{ %g }{ %g } }{ %g - \\dfrac{ %g }{ %g } } \\)",
                  dfTotaled["Totals", "xy"],
                  sumXSumY,
                  length(datx()),
                  dfTotaled["Totals", "x<sup>2</sup>"],
                  sumXSqrd,
                  length(datx())),
          sprintf("\\( \\, = \\, \\dfrac{ %g - %g }{ %g - %g } \\)",
                  dfTotaled["Totals", "xy"],
                  sumXSumY / length(datx()),
                  dfTotaled["Totals", "x<sup>2</sup>"],
                  sumXSqrd / length(datx())),
          sprintf("\\( \\, = \\, \\dfrac{ %g }{ %g } \\)",
                  dfTotaled["Totals", "xy"] - (sumXSumY) / length(datx()),
                  dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx())),
          sprintf("\\( \\, = \\, %0.4f \\)",
                  slopeEstimate),
          br(),
          br(),
          p("and"),
          sprintf("\\( \\qquad \\hat{\\beta}_{0} = \\bar{y} - \\hat{\\beta}_{1} \\bar{x}\\)"),
          sprintf("\\( \\, = \\, %g - (%0.4f) (%g) \\)",
                  mean(daty()),
                  summary(model)$coefficients["datx", "Estimate"],
                  mean(datx())),
          sprintf("\\( \\, = \\, %g %s %0.4f\\)",
                  mean(daty()),
                  b0HatOp,
                  abs(slopeEstimate) * mean(datx())),
          sprintf("\\( \\, = \\, %0.4f \\)",
                  interceptEstimate),
          br(),
          br(),
          br(),
          sprintf("\\( \\hat{y} = %0.4f %s %0.4f x \\)",
                  interceptEstimate,
                  yHatOp,
                  abs(slopeEstimate)),
          br(),
          br(),
          br(),
          p(tags$b("Interpretation of regression coefficients:"),
            br(),
            br(),
            "Within the scope of observation, ", interceptEstimate, " is the estimated value of ",
            em("y"), " when ", em("x"), "= 0. A slope of ", slopeEstimate,
            " represents the estimated ", slopeDirection, " in ", em("y"),
            " for a unit increase of ", em("x.")),
                                        # sprintf("Within the scope of observation, %s is the estimated value of ",
                                        #         interceptEstimate),

                                        # sprintf("when x = 0. A slope of %s represents the estimated %s in y for a
                                        #         unit increase of x.",
                                        #
                                        #         slopeEstimate,
                                        #         slopeDirection),
          br(),
          br()
        )
      })

      output$linearRegression <- renderPrint({
        summary(model)
      })

      output$confintLinReg <- renderPrint({
        confint(model) # Prints the 95% CI for the regression parameters
      })

      output$anovaLinReg <- renderPrint({
        anova(model) # Prints the ANOVA table
      })

      ##----------------------------------#
      ## Tests for normality of residuals #
      ##----------------------------------#
                                        #
      ## Anderson-Darling Normality Test
      ## output$AndersonDarlingTest <- renderPrint({
      ##   ad.test(model$residuals)
      ## })
      ##
      ## # Kolmogorov-Smirnov Normality Test
      ## output$KolmogorovSmirnovTest <- renderPrint({
      ##   ks.test(model$residuals, "pnorm")
      ## })
      ##
      ## # Shapiro-Wilk Normality Test
      ## output$ShapiroTest <- renderPrint({
      ##   shapiro.test(model$residuals)
      ## })
      ##
      ## # Q-Q plot for residuals
      ## output$qqplot <- renderPlot({
      ##   #qqnorm(model$residuals, ylab = "Residuals", xlab = "Z Scores", main = "Q-Q plot of Standardized Residuals", pch = 19) #+
      ##   #qqline(model$residuals)
      ##   qqPlot(model$residuals, main = "Q-Q Plot", xlab = "Z Scores",  ylab = "Residuals", pch = 19)
      ## })
      ##
      ## output$moreplots <- renderPlot({
      ##   par(mfrow = c(2, 2))
      ##   plot(model, which = 1:4, pch = 19)
      ## })

      if(length(datx()) > 2) {

        pearson <- cor.test(datx(), daty(), method = "pearson")

        if(!is.na(pearson$estimate)) {
          if(pearson$estimate < 0) {
            pearsonSign <- "negative"
          } else {
            pearsonSign <- "positive"
          }

          if(abs(pearson$estimate) > 0.6) {
            pearsonStrength <- "strong"
          } else if (abs(pearson$estimate) > 0.3) {
            pearsonStrength <- "moderate"
          } else {
            pearsonStrength <- "weak"
          }

          output$pearsonCorFormula <- renderUI({
            withMathJax(
              sprintf("\\( r \\; = \\; \\dfrac
                                      {\\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }
                                      {\\sqrt{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } \\sqrt{ \\sum y^2 - \\dfrac{ (\\sum y) ^2 }{ n } } } \\)"),
              br(),
              br(),
              br(),
              sprintf("\\( \\quad = \\; \\dfrac
                                      {%g - \\dfrac{ (%g)(%g) }{ %g } }
                                      {\\sqrt{ %g - \\dfrac{ (%g)^2 }{ %g } } \\sqrt{ %g - \\dfrac{ (%g) ^2 }{ %g } } } \\)",
                      dfTotaled["Totals", "xy"],
                      dfTotaled["Totals", "x"],
                      dfTotaled["Totals", "y"],
                      length(datx()),
                      dfTotaled["Totals", "x<sup>2</sup>"],
                      dfTotaled["Totals", "x"],
                      length(datx()),
                      dfTotaled["Totals", "y<sup>2</sup>"],
                      dfTotaled["Totals", "y"],
                      length(datx())),
              br(),
              br(),
              br(),

              sprintf("\\( \\quad = \\; \\dfrac
                                      { %g }
                                      {\\sqrt{ %g } \\sqrt{ %g } } \\)",
                      dfTotaled["Totals", "xy"] - sumXSumY / length(datx()),
                      dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx()),
                      dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx())),

              sprintf("\\( = \\; \\dfrac
                                      { %g }
                                      { %g } \\)",
                      dfTotaled["Totals", "xy"] - sumXSumY / length(datx()),
                      sqrt(dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx())) * sqrt(dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx()))),

              sprintf("\\( = \\; %0.4f \\)",
                      pearson$estimate),
              br(),
              br(),
              br(),
              p(tags$b("Interpretation:")),
              sprintf("There is a %s %s linear relationship between \\(x\\) and \\(y\\).",
                      pearsonStrength,
                      pearsonSign),
              br()
            )
          })

          output$PearsonCorTest <- renderPrint({
            pearson
          })

          if(length(datx()) > 3) {
            output$PearsonConfInt <- renderPrint({
              pearson$conf.int
            })
          } else {
            output$PearsonConfInt <- renderPrint ({
              noquote("Computation of the Confidence Interval requires a minimum sample size of 4.")
            })
          }
        }
      } else {
        output$PearsonCorTest <- renderPrint ({
          noquote("Pearson's Product-Moment Correlation requires a minimum sample size of 3 for computation.")
        })
      }

      kendall <- cor.test(datx(), daty(), method = "kendall")
      spearman <- cor.test(datx(), daty(), method = "spearman")

      output$kendallEstimate <- renderUI({
        sprintf("\\( \\tau \\; = \\; %0.4f \\)",
                kendall$estimate)
      })

      output$spearmanEstimate <- renderUI({
        sprintf("\\( \\displaystyle r_{s} \\; = \\; 1 - \\dfrac{ 6 \\, \\sum\\limits_{i=1}^n d^2_{i}}{ n(n^2 - 1)} \\; = \\; %0.4f \\)",
                spearman$estimate)
      })
    }) |> bindEvent(selectedVariables$calculate())
  })
}
