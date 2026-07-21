statInfrMethodCategoricalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    chiSqActiveData <- reactive({
      if (input$chisquareDimension == "2 x 2") {
        active <- input$chiSqInput2x2
      } else if (input$chisquareDimension == "2 x 3") {
        active <- input$chiSqInput2x3
      } else if (input$chisquareDimension == "3 x 2") {
        active <- input$chiSqInput3x2
      } else if (input$chisquareDimension == "3 x 3") {
        active <- input$chiSqInput3x3
      }

      activeData <- list(active, suppressWarnings(as.numeric(active)))
      names(activeData) <- c("data", "numeric")

      return(activeData)
    })

    chiSqActiveMatrix <- reactive({
      active <- matrix(chiSqActiveData()$numeric, ncol = ncol(chiSqActiveData()$data))
      colnames(active) <- colnames(chiSqActiveData()$data)
      rownames(active) <- rownames(chiSqActiveData()$data)

      return(active)
    })

    chiSqResults <- reactive({
      req(si_iv$is_valid())
      return(suppressWarnings(ChiSquareTest(chiSqActiveMatrix(), input$chiSquareYates)))
    })

    chiSqTotaled <- reactive({
      if (!any(is.na(chiSqActiveData()$numeric))) {
        chiSqTotaledMatrix <- chiSqActiveMatrix()
        chiSqTotaledMatrix <- cbind(chiSqTotaledMatrix, Total = round(rowSums(chiSqTotaledMatrix), 4))
        chiSqTotaledMatrix <- rbind(chiSqTotaledMatrix, Total = round(colSums(chiSqTotaledMatrix), 4))

        return(chiSqTotaledMatrix)
      }
    })


    fishersResults <- reactive({
      req(si_iv$is_valid())
      return(fisher.test(chiSqActiveMatrix()))
    })

    ## ---------------- Chi-Square Validation
    if (!chiSq2x2_iv$is_valid()) {
      validate(
        need(input$chiSqInput2x2, "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %||%
        need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(any(chiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqTotaled()[, "Total"] > 0) && all(chiSqTotaled()["Total", ] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        errorClass = "myClass"
      )
    }

    if (!chiSq2x3_iv$is_valid()) {
      validate(
        need(input$chiSqInput2x3, "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %||%
        need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(any(chiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqTotaled()[, "Total"] > 0) && all(chiSqTotaled()["Total", ] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        errorClass = "myClass"
      )
    }

    if (!chiSq3x2_iv$is_valid()) {
      validate(
        need(input$chiSqInput3x2, "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %||%
        need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(any(chiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqTotaled()[, "Total"] > 0) && all(chiSqTotaled()["Total", ] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        errorClass = "myClass"
      )
    }

    if (!chiSq3x3_iv$is_valid()) {
      validate(
        need(input$chiSqInput3x3, "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %||%
        need(all(chiSqActiveData()$numeric %% 1 == 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqActiveData()$numeric >= 0), "Fields must be positive integers."),
        errorClass = "myClass"
      )

      validate(
        need(any(cchiSqActiveData()$numeric != 0), "All cell values cannot be equal to zero."),
        errorClass = "myClass"
      )

      validate(
        need(all(chiSqTotaled()[, "Total"] > 0) && all(chiSqTotaled()["Total", ] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        errorClass = "myClass"
      )
    }


    observe({
      if (input$chisquareMethod == "Fisher") {
        updateRadioButtons(session, "chisquareDimension", selected = "2 x 2")
        ## hide radio buttons
        runjs(r"--[
          $('input[value="2 x 3"]').parent().hide();
          $('input[value="3 x 2"]').parent().hide();
          $('input[value="3 x 3"]').parent().hide();
        ]--")
        ## hide matrices
        hide(id = "chiSqInput2x3")
        hide(id = "chiSqInput3x2")
        hide(id = "chiSqInput3x3")
      } else {
        ## show radio buttons
        runjs(r"--[
          $('input[value="2 x 3"]').parent().show();
          $('input[value="3 x 2"]').parent().show();
          $('input[value="3 x 3"]').parent().show();
        ]--")
        ## show matrices
        show(id = "chiSqInput2x3")
        show(id = "chiSqInput3x2")
        show(id = "chiSqInput3x3")
      }
    })

    observeEvent(input$chisquareDimension, {
      if (input$chisquareDimension != "2 x 2") {
        shinyjs::disable(selector = '#chisquareMethod input[value="Fisher"]')

        updatePrettyRadioButtons(
          inputId = "chisquareMethod",
          selected = "Chi-Square"
        )
      } else {
        shinyjs::enable(selector = '#chisquareMethod input[value="Fisher"]')
      }

      shinyjs::reset(id = "chiSquareRowHeader")
      shinyjs::reset(id = "chiSquareColHeader")
    })

      PrintFishersTest <- function() {
        results <- fishersResults()

        if (input$chisquareSigLvl == "10%") {
          sigLvl <- 0.1
        } else if (input$chisquareSigLvl == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }

        if (results$p.value > sigLvl) {
          pValSymbol <- "\\gt"
          suffEvidence <- "isn't"
          reject <- "do not reject"
          pValue <- paste(round(results$p.value, 4))
        } else {
          pValSymbol <- "\\leq"
          suffEvidence <- "is"
          reject <- "reject"

          if (results$p.value < 0.0001 && results$p.value > 0) {
            pValue <- "p < 0.0001"
          } else {
            pValue <- paste(round(results$p.value, 4))
          }
        }

        fishersOutput <- tagList(
          withMathJax(),
          br(),
          br(),
          sprintf("\\( H_{0} \\): The Row variable and Column variable are not associated (independent)"),
          br(),
          sprintf("\\( H_{a} \\): The Row variable and Column variable are associated (dependent)"),
          br(),
          br(),
          sprintf(
            "\\( \\alpha = %s \\)",
            sigLvl
          ),
          br(),
          br()
        )

        fishersPVal <- PrintFishersPVal(pValue, pValSymbol, sigLvl, reject)
        fishersConclusion <- PrintChiSqConclusion(sigLvl, suffEvidence)

        tagAppendChildren(fishersOutput, fishersPVal, fishersConclusion)
      }

      PrintFishersPVal <- function(pValue, pValSymbol, sigLvl, reject) {
        fishersData <- chiSqTotaled()

        if (input$chisquareDimension == "2 x 2") {
          tagList(
            p(tags$b("P-Value:")),
            sprintf("\\( p = \\dfrac{(a + b)! \\; (c + d)! \\; (a + c)! \\; (b + d)!}{a! \\; b! \\; c! \\; d! \\; n!} \\)"),
            br(),
            br(),
            sprintf(
              "\\( \\phantom{p} = \\dfrac{(%s + %s)! \\; (%s + %s)! \\; (%s + %s)! \\; (%s + %s)!}{%s! \\; %s! \\; %s! \\; %s! \\; %s!} \\)",
              fishersData[1, 1],
              fishersData[1, 2],
              fishersData[2, 1],
              fishersData[2, 2],
              fishersData[1, 1],
              fishersData[2, 1],
              fishersData[1, 2],
              fishersData[2, 2],
              fishersData[1, 1],
              fishersData[1, 2],
              fishersData[2, 1],
              fishersData[2, 2],
              fishersData[3, 3]
            ),
            br(),
            br(),
            sprintf(
              "\\( \\phantom{p} = %s \\)",
              pValue
            ),
            br(),
            br(),
            sprintf(
              "Since \\( p %s %0.2f \\), %s \\( H_{0}\\).",
              pValSymbol,
              sigLvl,
              reject
            ),
            br(),
            br()
          )
        } else {
          tagList(
            p(tags$b("P-Value:")),
            sprintf(
              "\\( p = %s \\)",
              pValue
            ),
            br(),
            br(),
            sprintf(
              "Since \\( p %s %0.2f \\), %s \\( H_{0}\\).",
              pValSymbol,
              sigLvl,
              reject
            ),
            br(),
            br()
          )
        }
      }
    

      output$fishersTest <- renderUI({
        PrintFishersTest()
      })

    
      output$chiSqTest <- renderUI({
        PrintChiSqTest()
      })

    
    PrintChiSqTest <- function() {
        data <- chiSqResults()
        if (input$chiSquareYates && input$chisquareDimension == "2 x 2") {
          chiSqStat <- data$Matrix[nrow(data$Matrix), "(|O - E| - 0.5)<sup>2</sup> / E"]
        } else {
          chiSqStat <- data$Matrix[nrow(data$Matrix), "(O - E)<sup>2</sup> / E"]
        }

        if (input$chisquareSigLvl == "10%") {
          sigLvl <- 0.1
        } else if (input$chisquareSigLvl == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }

        critVal <- round(qchisq(1 - sigLvl, df = data$Results$parameter), cvDigits)

        if (data$Results$p.value < sigLvl) {
          pValSymbol <- "\\leq"
          reject <- "reject"
          region <- "rejection"
          suffEvidence <- "is"
        } else {
          pValSymbol <- "\\gt"
          reject <- "do not reject"
          region <- "acceptance"
          suffEvidence <- "isn't"
        }

        chiSqOutput <- tagList(
          withMathJax(),
          titlePanel("5-Step Process"),
          br(),
          br(),
          sprintf("\\( H_{0} \\): The Row variable and Column variable are not associated (independent)"),
          br(),
          sprintf("\\( H_{a} \\): The Row variable and Column variable are associated (dependent)"),
          br(),
          br(),
          sprintf(
            "\\( \\alpha = %s \\)",
            sigLvl
          ),
          br(),
          br()
        )

        if (input$chisquareDimension == "2 x 2" && input$chiSquareYates) {
                                        # Yates correction is only applied when O - E is > 0.5
          chiSqFormula <- PrintChiSqYatesFormula(chiSqStat)
        } else {
          chiSqFormula <- PrintChiSqFormula(chiSqStat)
        }

        chiSqPVal <- PrintChiSqPVal(data$Results$p.value, chiSqStat, pValSymbol, sigLvl, reject)
        chiSqCV <- PrintChiSqCV(critVal, reject, region, alpha = sigLvl, df = data$Results$parameter)
        chiSqConclusion <- PrintChiSqConclusion(sigLvl, suffEvidence)

        tagAppendChildren(chiSqOutput, chiSqFormula, chiSqPVal, chiSqCV, chiSqConclusion)
      }

    output$renderChiSqExp <- renderUI({
      DTOutput(session$ns("chiSqExp"), width = "500px")
    })

    PrintChiSqYatesFormula <- function(chiSqStat) {
        data <- chiSqResults()$Matrix
        yates <- data[, "(O - E)"]
        yates <- round((abs(yates) - 0.5)^2 / data[, "E"], 4)
        yates <- head(yates, -1)

        if (all(abs(data[nrow(data) - 1, "(O - E)"]) > 0.5)) {
          chiSqStat <- sum(yates)
          chiSqSum <- ""
          chiSqSmplf <- ""

          for (row in 1:(nrow(data) - 2)) {
            chiSqSum <- paste0(chiSqSum, "\\dfrac{(|", data[row, "O"], " - ", data[row, "E"], "| - 0.5)^2}{", data[row, "E"], "} + ")
            chiSqSmplf <- paste0(chiSqSmplf, yates[row], " + ")
          }

          chiSqSum <- paste0(chiSqSum, "\\dfrac{(|", data[nrow(data) - 1, "O"], " - ", data[nrow(data) - 1, "E"], "| - 0.5)^2}{", data[nrow(data) - 1, "E"], "}")
          chiSqSmplf <- paste0(chiSqSmplf, yates[nrow(data) - 1])

          formula <- tagList(
            p(tags$b("Test Statistic:")),
            sprintf("\\( \\chi^2_{Yates} = \\large{ \\sum{ \\dfrac{(|O - E| - 0.5)^2}{E} } } \\)"),
            br(),
            br(),
            sprintf(
              "\\( \\phantom{\\chi^2} =  %s \\)",
              chiSqSum
            ),
            br(),
            br(),
            br(),
            sprintf(
              "\\( \\phantom{\\chi^2} =  %s \\)",
              chiSqSmplf
            ),
            br(),
            br(),
            br(),
            sprintf(
              "\\( \\phantom{\\chi^2} = %0.4f \\)",
              chiSqStat
            ),
            br(),
            br(),
            br()
          )
        } else {
          disclaimer <- tagList(
            p(tags$i("*Note: Yates’ continuity correction is not applied in this
                 case because the correction factor is greater than |O - E| for
                 one or more of the differences.*")),
            br(),
            br()
          )
          formula <- tagAppendChildren(PrintChiSqFormula(chiSqStat), disclaimer)
        }

        return(formula)
    }

    PrintChiSqConclusion <- function(sigLvl, suffEvidence) {
        conclusion <- tagList(
          p(tags$b("Conclusion:")),
          p(
            sprintf(
              "At the %1.0f%% significance level, there %s sufficient
                evidence to reject the null hypothesis \\( (H_{0}) \\) that the
                Row variable and Column variable are not associated.",
              sigLvl * 100,
              suffEvidence
            ),
            br()
          )
        )

        return(conclusion)
      }
    
    PrintChiSqCV <- function(critVal, reject, region, alpha, df) {
        cvOutput <- tagList(
          p(tags$b("Using Critical Value Method:")),
          sprintf(
            "Critical Value \\( = \\chi^2_{\\alpha, \\, df} = \\chi^2_{\\alpha, \\, (r - 1) \\times (c - 1)}
              = \\chi^2_{%s, \\, (%s - 1) \\times (%s - 1)} =  \\chi^2_{%s, \\, %s} = %s \\)",
            alpha,
            nrow(chiSqActiveData()$data),
            ncol(chiSqActiveData()$data),
            alpha,
            df,
            critVal
          ),
          br(),
          br(),
          sprintf(
            "Since the test statistic \\( (\\chi^2)\\) falls within the %s region, %s \\( H_{0}\\).",
            region,
            reject
          ),
          br(),
          br(),
          br(),
          plotOutput(session$ns("chiSqPlot"), width = "50%", height = "400px"),
          br(),
          br()
        )
      }

    PrintChiSqPVal <- function(pValue, tsValue, pValSymbol, sigLvl, reject) {
        pvalCalc <- paste("P(\\, \\chi^2 \\, \\ge \\,", round(tsValue, 4), ")")

        if (pValue < 0.0001 && pValue > 0) {
          pValue <- "P < 0.0001"
        } else {
          pValue <- paste(round(pValue, 4))
        }

        pValOutput <- tagList(
          p(tags$b("Using P-Value Method:")),
          sprintf(
            "\\( P = %s = %s\\)",
            pvalCalc,
            pValue
          ),
          br(),
          br(),
          sprintf(
            "Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
            pValSymbol,
            sigLvl,
            reject
          ),
          br(),
          br(),
          br()
        )

        return(pValOutput)
      }
    
    PrintChiSqFormula <- function(chiSqStat) {
        data <- chiSqResults()$Matrix

        chiSqSum <- ""
        chiSqSmplf <- ""

        for (row in 1:(nrow(data) - 2)) {
          chiSqSum <- paste0(chiSqSum, "\\dfrac{(", data[row, "O"], " - ", data[row, "E"], ")^2}{", data[row, "E"], "} + ")
          chiSqSmplf <- paste0(chiSqSmplf, data[row, "(O - E)<sup>2</sup> / E"], " + ")
        }

        chiSqSum <- paste0(chiSqSum, "\\dfrac{(", data[nrow(data) - 1, "O"], " - ", data[nrow(data) - 1, "E"], ")^2}{", data[nrow(data) - 1, "E"], "}")
        chiSqSmplf <- paste0(chiSqSmplf, data[nrow(data) - 1, "(O - E)<sup>2</sup> / E"])

        formula <- tagList(
          p(tags$b("Test Statistic:")),
          sprintf("\\( \\chi^2 = \\large{\\displaystyle \\sum{ \\dfrac{(O - E)^2}{E} } } \\)"),
          br(),
          br(),
          sprintf(
            "\\( \\phantom{\\chi^2} =  %s \\)",
            chiSqSum
          ),
          br(),
          br(),
          br(),
          sprintf(
            "\\( \\phantom{\\chi^2} =  %s \\)",
            chiSqSmplf
          ),
          br(),
          br(),
          br(),
          sprintf(
            "\\( \\phantom{\\chi^2} = %s \\)",
            chiSqStat
          ),
          br(),
          br(),
          br()
        )

        return(formula)
      }

      output$chiSqPlot <- renderPlot({ ## chisq plot ----
        data <- chiSqResults()
        chisq_df <- data$Results$parameter
        chisq_ts <- round(data$Results$statistic, 4)
        ## chisq_ts <- data$Matrix[nrow(data$Matrix), "(O - E)<sup>2</sup> / E"]

        if (input$chisquareSigLvl == "10%") {
          sigLvl <- 0.1
        } else if (input$chisquareSigLvl == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }

        cv <- round(qchisq(1 - sigLvl, df = chisq_df), 4)
        ## lower95 <- qchisq(.025, chisq_df)
        ## upper95 <- qchisq(.975, chisq_df)

        xSeq <- c(seq(0, 15, length.out = 75), cv, chisq_ts)
        rrLabel <- c((cv + max(xSeq)) / 2)
        x_vector <- sort(c(xSeq, rrLabel))
        p_vector <- dchisq(x_vector, df = chisq_df)

        df <- distinct(data.frame(x = x_vector, y = p_vector))
        cvDF <- filter(df, x %in% cv)
        tsDF <- filter(df, x %in% chisq_ts)
        rrLabelDF <- filter(df, x %in% rrLabel)
        arLabelDF <- filter(df, y %in% max(p_vector))

        ggplot(
          df,
          aes(x = x, y = y)
        ) +
          stat_function(
            fun = dchisq,
            args = list(df = chisq_df),
            geom = "Density",
            fill = NA
          ) +
          shadeHtArea(df, cv, "greater") +
          geom_segment(
            data = filter(df, y %in% max(p_vector)),
            aes(x = 0, xend = 0, y = 0, yend = y, alpha = 0.5),
            linetype = "solid",
            linewidth = 0.75,
            color = "black",
            show.legend = FALSE
          ) +
          geom_text(
            data = filter(df, x %in% c(0)),
            aes(x = x, y = 0, label = "0"),
            size = 14 / .pt,
            fontface = "bold",
            nudge_y = -.03,
            check_overlap = TRUE
          ) +
          geom_segment(
            data = cvDF,
            aes(x = x, xend = x, y = 0, yend = y),
            linetype = "solid",
            lineend = "butt",
            linewidth = 1.5,
            color = "#023B70"
          ) +
          geom_text(
            data = cvDF,
            aes(x = x, y = 0, label = x),
            size = 14 / .pt,
            fontface = "bold",
            nudge_y = -.03,
            check_overlap = TRUE
          ) +
          geom_segment(
            data = tsDF,
            aes(x = x, xend = x, y = 0, yend = y + .055),
            linetype = "solid",
            linewidth = 1.25,
            color = "#BD130B"
          ) +
          geom_text(
            data = tsDF,
            aes(x = x, y = y, label = x),
            size = 14 / .pt,
            fontface = "bold",
            nudge_y = .075,
            check_overlap = TRUE
          ) +
          geom_text(
            data = arLabelDF,
            aes(x = x, y = 0, label = "A R"),
            size = 16 / .pt,
            fontface = "bold",
            vjust = -4,
            check_overlap = TRUE
          ) +
          geom_text(
            data = rrLabelDF,
            aes(x = x, y = y, label = "RR"),
            size = 16 / .pt,
            fontface = "bold",
            vjust = -4,
            check_overlap = TRUE
          ) +
          theme_void() +
          ylab("") +
          xlab(expression(bold(chi^2))) +
          scale_y_continuous(breaks = NULL) +
          theme(axis.title.x = element_text(size = 20))
        ## coord_cartesian(clip="off")
      })
  })
}
