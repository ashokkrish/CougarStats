## FIXME: no plotOutput with this id!
output$onePopulationSDHTChiSqPlot <- renderPlot({
  chiSqTestData(envir = environment())

  ## Clamp the minimum to zero.
  minimumChiSqValue <- min(chiSqTestStatistic, chiSqCValue) - 1
  if (minimumChiSqValue < 0) {
    minimumChiSqValue <- 0
  }

  maximumChiSqValue <- max(chiSqTestStatistic, chiSqCValue) + 1
  if (maximumChiSqValue < 20) {
    maximumChiSqValue <- 20
  }

  par(font.lab = 2, font.axis = 2)

  ## Plot the main curve.
  curve(
    dchisq(x, df = degreesOfFreedom),
    ## from = minimumChiSqValue,
    from = 0,
    to = maximumChiSqValue,
    ## main = sprintf("Chi-Square Distribution (df = %d)", degreesOfFreedom),
    main = NULL,
    lwd = 2, ## line width
    font.lab = 2,
    xlab = expression(x = chi^2),
    ylab = expression(y = f(chi^2))
  )

  ## Account for two-tailed hypothesis tests.
  if (length(chiSqCValue) == 1) {
    ## applies to lower and upper tailed tests
    lowerRejectionRegion <- sort(seq(0, chiSqCValue, by = 0.00001))
    upperRejectionRegion <- sort(seq(
      chiSqCValue,
      maximumChiSqValue,
      by = 0.00001
    ))
    lowerPVector <- dchisq(lowerRejectionRegion, df = degreesOfFreedom)
    upperPVector <- dchisq(upperRejectionRegion, df = degreesOfFreedom)
    if (input$altHypothesis == 1) {
      polygon(
        c(lowerRejectionRegion, rev(lowerRejectionRegion)),
        c(lowerPVector, rep(0, length(lowerPVector))),
        col = adjustcolor("red", alpha = 0.3),
        border = NA
      )
    } else {
      polygon(
        c(upperRejectionRegion, rev(upperRejectionRegion)),
        c(upperPVector, rep(0, length(upperPVector))),
        col = adjustcolor("red", alpha = 0.3),
        border = NA
      )
    }
    segments(
      x0 = chiSqTestStatistic,
      y0 = 0,
      y1 = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
      col = adjustcolor("blue", alpha = 0.8),
      lwd = 5
    )
    segments(
      x0 = chiSqCValue,
      y0 = 0,
      y1 = dchisq(chiSqCValue, df = degreesOfFreedom),
      col = adjustcolor("red", alpha = 0.8),
      lwd = 5
    )
    text(
      x = chiSqCValue,
      y = dchisq(chiSqCValue, df = degreesOfFreedom),
      labels = as.character(round(chiSqCValue, 4)),
      pos = 3, ## to the left of the specified (x, y) coordinate.
      offset = 1
    )
    text(
      x = chiSqTestStatistic,
      y = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
      labels = as.character(round(chiSqTestStatistic, 4)),
      pos = 2, ## to the left of the specified (x, y) coordinate.
      offset = 1
    )
  } else {
    ## two-tailed hypothesis tests
    lowerRejectionRegion <- seq(0, chiSqCValue[[1]], by = 0.00001)
    lowerPVector <- dchisq(lowerRejectionRegion, df = degreesOfFreedom)
    polygon(
      c(lowerRejectionRegion, rev(lowerRejectionRegion)),
      c(lowerPVector, rep(0, length(lowerPVector))),
      col = adjustcolor("red", alpha = 0.3),
      border = NA
    )

    upperRejectionRegion <- seq(
      chiSqCValue[[2]],
      max(chiSqTestStatistic, chiSqCValue) + 1
    )
    upperPVector <- dchisq(upperRejectionRegion, df = degreesOfFreedom)
    polygon(
      c(upperRejectionRegion, rev(upperRejectionRegion)),
      c(upperPVector, rep(0, length(upperPVector))),
      col = adjustcolor("red", alpha = 0.3),
      border = NA
    )

    segments(
      x0 = chiSqTestStatistic,
      y0 = 0,
      y1 = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
      col = adjustcolor("blue", alpha = 0.8),
      lwd = 5
    )
    segments(
      x0 = chiSqCValue[[1]],
      y0 = 0,
      y1 = dchisq(chiSqCValue[[1]], df = degreesOfFreedom),
      col = adjustcolor("red", alpha = 0.8),
      lwd = 5
    )
    segments(
      x0 = chiSqCValue[[2]],
      y0 = 0,
      y1 = dchisq(chiSqCValue[[2]], df = degreesOfFreedom),
      col = adjustcolor("red", alpha = 0.8),
      lwd = 5
    )

    text(
      x = chiSqCValue[[1]],
      y = dchisq(chiSqCValue[[1]], df = degreesOfFreedom),
      labels = as.character(round(chiSqCValue[[1]], 4)),
      pos = 3, ## to the left of the specified (x, y) coordinate.
      offset = 1
    )
    text(
      x = chiSqCValue[[2]],
      y = dchisq(chiSqCValue[[2]], df = degreesOfFreedom),
      labels = as.character(round(chiSqCValue[[2]], 4)),
      pos = 3, ## to the left of the specified (x, y) coordinate.
      offset = 1
    )
    text(
      x = chiSqTestStatistic,
      y = dchisq(chiSqTestStatistic, df = degreesOfFreedom),
      labels = as.character(round(chiSqTestStatistic, 4)),
      pos = 2, ## to the left of the specified (x, y) coordinate.
      offset = 1
    )
  }

  segments(
    x0 = 0,
    y0 = 0,
    x1 = maximumChiSqValue,
    y1 = 0,
    col = adjustcolor("black", alpha = 1.0)
  )
})

output$onePopulationSDHT <- renderUI({
  ## Required data: n, s, alpha, sigma_naught, hypothesis_alternative; ns(x)
  ## doesn't seem to be required here. Review why that might be.
  ##
  ## Inputs: SSDSampleSize (n), SSDStdDev (s), significanceLevel, hypStdDeviation (sigma_naught),
  ## altHypothesis (e.g. "<").
  ##
  ## Useful data: SigLvl() [numeric];

  chiSqTestData(envir = environment())

  ## "if P <= alpha, reject H0"
  if (chiSqPValue <= SigLvl()) {
    rejectionOrAcceptanceStatement <-
      sprintf("Since \\( P \\leq %0.2f \\), reject \\( H_{0}\\).", SigLvl())
  } else {
    rejectionOrAcceptanceStatement <-
      sprintf(
        "Since \\( P \\gt %0.2f \\), do not reject \\( H_{0}\\).",
        SigLvl()
      )
  }

  hypothesisFormattedString <- function(hypothesis, nullOrAltHypothesisString) {
    sprintf(
      r"--[\( H_%s: \sigma %s %0.3f \)]--", ## σ
      hypothesis,
      nullOrAltHypothesisString,
      input$hypStdDeviation
    )
  }

  ## UI
  withMathJax(
    hypothesisFormattedString("0", nullHypString),
    br(),
    hypothesisFormattedString("a", altHypString),
    br(),
    br(),
    sprintf("\\( \\alpha = %0.2f \\)", SigLvl()),
    br(),
    br(),
    p(tags$b("Test Statistic:")),
    sprintf("Given:"),
    br(),
    sprintf(r"--[\( n = %d \)]--", input$SSDSampleSize),
    br(),
    sprintf(r"--[\( s = %0.4f \)]--", input$SSDStdDev),
    br(),
    sprintf(r"--[\( \sigma_0 = %.4f \)]--", input$hypStdDeviation),
    br(),
    br(),
    br(),
    p(
      r"--[
            \(
            \displaystyle \chi^2 = \frac{(n-1)s^2}{\sigma^2_0}
            \)
           ]--"
    ),
    br(),
    sprintf(
      r"--(
           \(
           \displaystyle
           \chi^2 = \frac{(%d - 1)  %0.4f ^2}{%0.4f^2} = %0.4f\\
           \)
           )--",
      input$SSDSampleSize,
      input$SSDStdDev,
      input$hypStdDeviation,
      chiSqTestStatistic
    ),
    br(),
    br(),
    p(tags$b("Using P-Value Method:")),
    if (input$altHypothesis == 2) {
      sprintf(
        "\\( P = 2 \\times min(P\\left( \\chi^2 \\le %0.4f \\right), P\\left( \\chi^2 \\ge %0.4f \\right)) = %0.4f \\)",
        chiSqTestStatistic,
        chiSqTestStatistic,
        chiSqPValue
      )
    } else {
      sprintf(
        "\\( P = P\\left( \\chi^2 %s %s \\right) = %0.4f \\)",
        pValueMethodRelationalOperatorString,
        sprintf("%0.4f", chiSqTestStatistic),
        chiSqPValue
      )
    },
    br(),
    br(),
    rejectionOrAcceptanceStatement,
    br(),
    br(),
    br(),
    p(tags$b("Using Critical Value Method:")),
    sprintf(
      "\\(df = n - 1 = %d - 1 = %d\\)",
      input$SSDSampleSize,
      degreesOfFreedom
    ),
    br(),
    br(),
    if (input$altHypothesis != 2) {
      HTML(sprintf(
        "Critical value(s): \\( \\chi^2_{%0.2f,%d} = %0.4f \\) <br/>",
        SigLvl(),
        degreesOfFreedom,
        chiSqCValue
      ))
    } else {
      HTML(sprintf(
        "Critical value(s): <br/>
                  \\( \\chi^2_{\\alpha/2,df} = \\chi^2_{%0.4f,%d} = %0.4f \\) <br/>
                  \\( \\chi^2_{1 - \\alpha/2,df} = \\chi^2_{%0.4f,%d} = %0.4f \\) <br/>",
        SigLvl() / 2,
        degreesOfFreedom,
        chiSqCValue[[1]],
        1 - SigLvl() / 2,
        degreesOfFreedom,
        chiSqCValue[[2]]
      ))
    },
    br(),
    ## Chi square critical value conclusion.
    ## Example from mu: "Since the test statistic (z) falls within the rejection region, reject H0."
    if (input$altHypothesis != 2) {
      HTML(sprintf(
        r"--(\(\begin{align} \displaystyle \chi^2 &%s \chi^2_{%0.2f,%d} \\ %0.4f &%s %0.4f  \\ \end{align} \)<br/>)--",
        ## Both of these are alternative hypothesis-dependent
        {
          if (chiSqTestStatistic < chiSqCValue) {
            relation("\\leq")
            "\\leq"
          } else if (chiSqTestStatistic >= chiSqCValue) {
            relation("\\geq")
            "\\geq"
          }
        },
        SigLvl(),
        degreesOfFreedom,
        chiSqTestStatistic,
        {
          if (chiSqTestStatistic < chiSqCValue) {
            relation("\\leq")
            "\\leq"
          } else if (chiSqTestStatistic >= chiSqCValue) {
            relation("\\geq")
            "\\geq"
          }
        },
        chiSqCValue
      ))
    } else {
      lessThan <- chiSqTestStatistic <= chiSqCValue[[1]]
      greaterThan <- chiSqTestStatistic >= chiSqCValue[[2]]
      between <- !lessThan && !greaterThan
      if (lessThan) {
        relation("\\leq")
        "\\leq"
      } else if (greaterThan) {
        relation("\\geq")
        "\\geq"
      }

      if (!between) {
        HTML(sprintf(
          r"--(\(\begin{align} \displaystyle \chi^2 &%s \chi^2_{%0.4f,%d} \\ %0.4f &%s %0.4f \\ \end{align} \)<br/>)--",
          relation(),
          {
            if (lessThan) SigLvl() / 2 else 1 - SigLvl() / 2
          },
          degreesOfFreedom,
          chiSqTestStatistic,
          relation(),
          if (lessThan) chiSqCValue[[1]]
        ))
      } else {
        HTML(sprintf(
          r"--(\(\begin{align} \displaystyle \chi^2_{%0.4f,%d} &< \chi^2 &< \chi^2_{%0.4f,%d} \\ %0.4f &< %0.4f &< %0.4f \\ \end{align} \)<br/>)--",
          SigLvl() / 2,
          degreesOfFreedom,
          1 - SigLvl() / 2,
          degreesOfFreedom,
          chiSqCValue[[1]],
          chiSqTestStatistic,
          chiSqCValue[[2]]
        ))
      }
    },
    br(),
    {
      conclusionString <-
        function(
          significanceLevel = SigLvl(),
          testStatisticValue = chiSqTestStatistic,
          criticalValue = chiSqCValue,
          accept = TRUE,
          lessThan = TRUE
        ) {
          sprintf(
            paste0(
              "Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
              " falls in the %s region,",
              " \\(\\chi^2 = %0.4f\\) which is %s than \\(%0.4f\\), we %sreject \\(H_0\\)",
              " as there is %ssufficient evidence to accept the",
              " alternative hypothesis."
            ),
            if (accept) "acceptance" else "rejection",
            testStatisticValue,
            if (lessThan) "less" else "greater",
            criticalValue,
            if (accept) "do not " else "",
            if (accept) "in" else ""
          )
        }

      if (input$altHypothesis != 2) {
        ## One-tailed test
        if (input$altHypothesis == 1) {
          if (chiSqTestStatistic < chiSqCValue) {
            conclusionString(accept = (accept <- FALSE), lessThan = TRUE)
          } else {
            conclusionString(accept = (accept <- TRUE), lessThan = FALSE)
          }
        } else {
          if (chiSqTestStatistic < chiSqCValue) {
            conclusionString(accept = (accept <- TRUE), lessThan = TRUE)
          } else {
            conclusionString(accept = (accept <- FALSE), lessThan = FALSE)
          }
        }
      } else {
        ## Two-tailed test
        if (chiSqTestStatistic <= chiSqCValue[[1]]) {
          accept <- FALSE
          sprintf(
            paste0(
              "Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
              " falls in the rejection region,",
              " \\(\\chi^2 = %0.4f\\) which is less than (or equal to) \\(%0.3f\\), we reject \\(H_0\\)",
              " as there is sufficient evidence to accept the",
              " alternative hypothesis."
            ),
            chiSqTestStatistic,
            chiSqCValue[[1]]
          )
        } else if ((chiSqTestStatistic >= chiSqCValue[[2]])) {
          accept <- FALSE
          sprintf(
            paste0(
              "Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
              " falls in the rejection region,",
              " \\(\\chi^2 = %0.34\\) which is greater than (or equal to) \\(%0.4f\\), we reject \\(H_0\\)",
              " as there is sufficient evidence to accept the",
              " alternative hypothesis."
            ),
            chiSqTestStatistic,
            chiSqCValue[[2]]
          )
        } else {
          accept <- TRUE
          sprintf(
            paste0(
              "Since the test statistic \\( \\left( \\chi^2 \\right) \\)",
              " falls in the acceptance region,",
              " \\(\\chi^2 = %0.3f\\) which is between \\(%0.4f\\) and \\(%0.4f\\), we do not reject \\(H_0\\)",
              " as there is insufficient evidence to accept the",
              " alternative hypothesis."
            ),
            chiSqTestStatistic,
            chiSqCValue[[1]],
            chiSqCValue[[2]]
          )
        }
      }
    },
    br(),

    ## FUTURE WORK: Revisit chi-square plots as they are currently bugged
    # plotOutput(session$ns("onePopulationSDHTChiSqPlot"), width = "50%", height = "400px"),

    ## Overall conclusion
    br(),
    p(tags$b("Conclusion:")),
    {
      if (accept) {
        conclusion <- sprintf(
          "At \\(\\alpha = %0.2f\\), since the test statistic falls in the acceptance region we fail to reject \\(H_0\\) and conclude that there is not enough statistical evidence to support that \\(\\sigma %s %s\\).",
          SigLvl(),
          altHypString,
          input$hypStdDeviation
        )
      } else {
        conclusion <- sprintf(
          "At \\(\\alpha = %0.2f\\), since the test statistic falls in the rejection region we reject \\(H_0\\) and conclude that there is enough statistical evidence to support that \\(\\sigma %s %s\\).",
          SigLvl(),
          altHypString,
          input$hypStdDeviation
        )
      }

      conclusion
    },
    br()
  ) ## withMathJax
}) ## renderUI


## FIXME: no plotOutput with this id!
output$onePropBarGraph <- renderPlot(
  {
    req(iv$is_valid() && input$numTrials >= input$numSuccesses)

    df <- tibble(
      Outcome = c("Successes", "Failures"),
      Count = c(input$numSuccesses, input$numTrials - input$numSuccesses)
    )

    ggplot(df, aes(x = Outcome, y = Count, fill = Outcome)) +
      geom_col(width = 0.5) +
      labs(
        title = "Bar Chart: Count of Successes vs Failures",
        y = "Count",
        x = ""
      ) +
      scale_fill_manual(
        values = c("Successes" = "#4CAF50", "Failures" = "#F44336")
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(size = 14, face = "bold", color = "black"),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.position = "none"
      )
  },
  width = 500,
  height = 400
)

output$onePropPieChart <- renderPlot({
  req(iv$is_valid() && input$numTrials >= input$numSuccesses)

  x <- tibble(
    Outcome = c("Successes", "Failures"),
    Count = c(input$numSuccesses, input$numTrials - input$numSuccesses)
  )

  ggplot(x, aes(x = "", y = Count, fill = Outcome)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(
      values = c("Successes" = "#4CAF50", "Failures" = "#F44336")
    ) +
    labs(title = "Success vs Failure Distribution") +
    theme_void() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16), ## center title
      legend.text = element_text(size = 12),
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", color = NA)
    )
})

output$onePropCI <- renderUI({
  req(iv$is_valid() && input$numTrials >= input$numSuccesses)

  onePropData <- OnePropZInterval(
    input$numSuccesses,
    input$numTrials,
    ConfLvl()
  )
  critVal <- round(onePropData["Z Critical"], cvDigits)

  p(
    withMathJax(
      sprintf("Given:"),
      br(),
      sprintf(
        "\\( n = %s \\)",
        onePropData["n"]
      ),
      br(),
      sprintf(
        "\\( x = %s \\)",
        onePropData["x"]
      ),
      br(),
      br(),
      br(),
      sprintf(
        "For a \\( %s \\)%% Confidence Interval: ",
        ConfLvl() * 100
      ),
      br(),
      sprintf(
        "\\( \\alpha = 1 - %s = %s \\)",
        ConfLvl(),
        1 - ConfLvl()
      ),
      br(),
      sprintf(
        "\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
        1 - ConfLvl(),
        (1 - ConfLvl()) / 2,
        critVal
      ),
      br(),
      br(),
      br(),
      sprintf(
        "\\( \\displaystyle CI = \\hat{p} \\pm \\left( z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} \\right) \\)"
      ),
      br(),
      p("where"),
      sprintf(
        "\\( \\qquad \\hat{p} = \\dfrac{x}{n} = \\dfrac{%s}{%s} = %0.4f \\)",
        onePropData["x"],
        onePropData["n"],
        onePropData["phat"]
      ),
      br(),
      br(),
      br(),
      sprintf(
        "\\( \\displaystyle CI = %0.4f \\pm \\left( %s \\sqrt{\\dfrac{%0.4f(1 - %0.4f)}{%s}} \\right) \\)",
        onePropData["phat"],
        critVal,
        onePropData["phat"],
        onePropData["phat"],
        onePropData["n"]
      ),
      br(),
      br(),
      sprintf(
        "\\( \\displaystyle \\phantom{CI} = %0.4f \\pm \\left( %g \\cdot %0.4f \\right) \\)",
        onePropData["phat"],
        critVal,
        onePropData["Std Error"]
      ),
      br(),
      br(),
      sprintf(
        "\\( \\displaystyle \\phantom{CI} = %0.4f \\pm %0.4f \\)",
        onePropData["phat"],
        onePropData["ME"]
      ),
      br(),
      br(),
      sprintf(
        "\\( \\displaystyle \\phantom{CI} = (%0.4f, %0.4f)\\)",
        onePropData["LCL"],
        onePropData["UCL"]
      ),
      br(),
      br(),
      br(),
      p(tags$b("Interpretation:")),
      sprintf(
        "We are %1.0f%% confident that the population proportion \\( (p) \\) is between \\( %0.4f \\) and \\( %0.4f \\).",
        ConfLvl() * 100,
        onePropData["LCL"],
        onePropData["UCL"]
      )
    )
  )
})

output$onePropHT <- renderUI({
  req(iv$is_valid() && input$numTrials >= input$numSuccesses)

  onePropData <- OnePropZTest(
    input$numSuccesses,
    input$numTrials,
    input$hypProportion,
    OneMeanHypInfo()$alternative,
    SigLvl()
  )

  if (input$altHypothesis == "2") {
    # two sided test
    critZVal <- paste("\\pm", round(onePropData["Z Critical"], cvDigits))
    nullHyp <- "p ="
    altHyp <- "p \\neq"
  } else {
    critZVal <- paste(round(onePropData["Z Critical"], cvDigits))

    if (input$altHypothesis == "1") {
      nullHyp <- "p \\geq"
      altHyp <- "p \\lt"
    } else {
      nullHyp <- "p \\leq"
      altHyp <- "p \\gt"
    }
  }

  if (onePropData["P-Value"] > SigLvl()) {
    pvalSymbol <- "\\gt"
    suffEvidence <- "isn't"
    reject <- "do not reject"
    region <- "acceptance"
  } else {
    pvalSymbol <- "\\leq"
    suffEvidence <- "is"
    reject <- "reject"
    region <- "rejection"
  }

  onePropHTHead <- tagList(
    withMathJax(
      sprintf(
        "\\( H_{0}: %s %g\\)",
        nullHyp,
        input$hypProportion
      ),
      br(),
      sprintf(
        "\\( H_{a}: %s %g\\)",
        altHyp,
        input$hypProportion
      ),
      br(),
      br(),
      sprintf(
        "\\( \\alpha = %g \\)",
        SigLvl()
      ),
      br(),
      # br(),
      br(),
      p(tags$b("Test Statistic:")),
      sprintf("Given:"),
      br(),
      sprintf(
        "\\( n = %s \\)",
        onePropData["n"]
      ),
      br(),
      sprintf(
        "\\( x = %s \\)",
        onePropData["x"]
      ),
      br(),
      br(),
      br(),
      sprintf(
        "\\(z = \\dfrac{\\hat{p} - p_{0}}{ \\sqrt{ \\dfrac{p_{0}(1 - p_{0})}{n} } }\\)"
      ),
      br(),
      p("where"),
      sprintf(
        "\\( \\qquad \\hat{p} = \\dfrac{x}{n} = \\dfrac{%s}{%s} = %0.4f \\)",
        onePropData["x"],
        onePropData["n"],
        onePropData["phat"]
      ),
      br(),
      br(),
      br(),
      sprintf(
        "\\(z = \\dfrac{%0.4f - %0.4f}{ \\sqrt{ \\dfrac{%0.4f(1 - %0.4f)}{%1.0f} } }\\)",
        onePropData["phat"],
        input$hypProportion,
        input$hypProportion,
        input$hypProportion,
        input$numTrials
      ),
      sprintf(
        "\\( = \\dfrac{%0.4f}{%0.4f} \\)",
        onePropData["phat"] - input$hypProportion,
        onePropData["Std Error"]
      ),
      br(),
      br(),
      sprintf(
        "\\(\\phantom{z} = %0.4f\\)",
        onePropData["Test Statistic"]
      ),
      br(),
      br(),
      br()
    )
  )

  onePropPVal <- printHTPVal(
    onePropData["P-Value"],
    "z",
    OneMeanHypInfo()$alternative,
    onePropData["Test Statistic"],
    pvalSymbol,
    reject
  )
  ## p(tags$b("Using P-Value Method:")),
  ## sprintf("\\( %s \\)",
  ## pValue),
  ## br(),
  ## sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
  ## pvalSymbol,
  ## SigLvl(),
  ## reject),
  ## br(),
  ## br(),
  ## br(),

  onePropHTTail <- tagList(
    withMathJax(
      p(tags$b("Using Critical Value Method:")),
      sprintf(
        "Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
        OneMeanHypInfo()$critSign,
        OneMeanHypInfo()$critAlph,
        OneMeanHypInfo()$critSign,
        OneMeanHypInfo()$alphaVal,
        critZVal
      ),
      br(),
      br(),
      sprintf(
        "Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
        region,
        reject
      ),
      br(),
      br(),
      plotOutput(session$ns("onePropHTPlot"), width = "75%", height = "300px"),
      br()
    )
  )

  onePropHTConclusion <- printHTConclusion(
    region,
    reject,
    suffEvidence,
    altHyp,
    input$hypProportion
  )

  tagAppendChildren(
    onePropHTHead,
    onePropPVal,
    onePropHTTail,
    onePropHTConclusion
  )
})

output$onePropHTPlot <- renderPlot({
  oneSampPropZTest <- OnePropZTest(
    input$numSuccesses,
    input$numTrials,
    input$hypProportion,
    OneMeanHypInfo()$alternative,
    SigLvl()
  )
  htPlotCritVal <- oneSampPropZTest["Z Critical"]

  htPlot <- hypZTestPlot(
    oneSampPropZTest["Test Statistic"],
    htPlotCritVal,
    OneMeanHypInfo()$alternative
  )
  htPlot
})

## ---- One population standard deviation hypothesis testing HT ----
## See #33.
chiSqTestData <- function(envir) {
  evalq(
    expr = {
      ## This paragraph is related to the final interpretation, as used in the
      ## P-value method.
      degreesOfFreedom <- input$SSDSampleSize - 1
      chiSqTestStatistic <- (degreesOfFreedom * input$SSDStdDev^2) /
        input$hypStdDeviation^2
      ## lower.tail will be false when the alternative hypothesis is >.
      isLeftTailed <- input$altHypothesis %in% c(1, 2)

      ## Establish the strings to use in MathJax-supported LaTeX for the hypotheses and relations.
      if (input$altHypothesis == 1) {
        nullHypString <- "\\geq"
        altHypString <- "\\lt"
        pValueMethodRelationalOperatorString <- "\\lt"
        chiSqCValue <- qchisq(SigLvl(), degreesOfFreedom)
        chiSqPValue <- pchisq(
          chiSqTestStatistic,
          degreesOfFreedom,
          lower.tail = isLeftTailed
        )
      } else if (input$altHypothesis == 2) {
        nullHypString <- "="
        altHypString <- "\\ne"
        pValueMethodRelationalOperatorString <- "\\lt"
        chiSqCValueLower <- qchisq(SigLvl() / 2, degreesOfFreedom)
        chiSqCValueUpper <- qchisq(1 - SigLvl() / 2, degreesOfFreedom)
        chiSqCValue <- c(chiSqCValueLower, chiSqCValueUpper)
        ## NOTE: The correct p-value is P = 2 × min(P(chisquare ≤ TS), P(chisquare ≥ TS))
        ## lower.tail: logical; if TRUE (default), probabilities are P[X <= x],
        ## otherwise, P[X > x].
        chiSqPValue <- 2 *
          min(
            pchisq(chiSqTestStatistic, degreesOfFreedom, lower.tail = TRUE),
            pchisq(chiSqTestStatistic, degreesOfFreedom, lower.tail = FALSE)
          )
      } else {
        nullHypString <- "\\leq"
        altHypString <- "\\gt"
        pValueMethodRelationalOperatorString <- "\\gt"
        chiSqCValue <- qchisq(1 - SigLvl(), degreesOfFreedom)
        chiSqPValue <- pchisq(
          chiSqTestStatistic,
          degreesOfFreedom,
          lower.tail = isLeftTailed
        )
      }
    },
    envir = envir
  )
}

## ---- One population standard deviation confidence interval CI ----
## FIXME: no uiOutput with this id!
output$oneSDCI <- renderUI({
  ## Input validation
  req(oneSD_iv$is_valid())

  ## Required data
  ## n (sample size), s (sample standard deviation), Confidence Level (1 - α)
  ## ns("SSDSampleSize")
  ## ns("SSDStdDev")
  ## ns("confidenceLevel")
  ## df = n - 1
  oneSDCIalpha <- 1 - ConfLvl() ## e.g.: 0.05
  oneSDCIdf <- input[["SSDSampleSize"]] - 1

  ## UI
  withMathJax(
    ## Preface
    sprintf("Given:"),
    br(),
    sprintf(
      "\\( n = %d \\)",
      input$SSDSampleSize
    ),
    br(),
    sprintf(
      "\\( s = %0.2f \\)",
      input$SSDStdDev
    ),
    br(),
    br(),
    br(),
    sprintf("For a %s Confidence Interval:", input$confidenceLevel),
    br(),
    sprintf(
      "\\( \\alpha = 1 - %0.2f = %0.2f \\)",
      1 - oneSDCIalpha,
      oneSDCIalpha
    ),
    br(),
    sprintf(
      "\\(  df = n - 1 = %d - 1 = %d \\)",
      input$SSDSampleSize,
      input$SSDSampleSize - 1
    ),
    br(),
    sprintf(
      "\\( \\chi^2_{  \\alpha/2, df} = \\chi^2_{    %0.2f / 2 , %d} = \\chi^2_{ %0.3f, %d } = %0.3f \\).",
      oneSDCIalpha,
      oneSDCIdf,
      ## See https://www.easysevens.com/understanding-chi-square-critical-value-a-beginners-tutorial/.
      (critOneSSDLeft <- oneSDCIalpha / 2),
      oneSDCIdf,
      (oneSSDLeft <- qchisq(p = 1 - critOneSSDLeft, df = oneSDCIdf))
    ),
    br(),
    sprintf(
      "\\( \\chi^2_{1-\\alpha/2, df} = \\chi^2_{ 1 - %0.2f / 2, %d} = \\chi^2_{ %0.3f, %d} = %0.3f \\)",
      oneSDCIalpha,
      oneSDCIdf,
      (critOneSSDRight <- 1 - oneSDCIalpha / 2),
      oneSDCIdf,
      (oneSSDRight <- qchisq(p = 1 - critOneSSDRight, df = oneSDCIdf))
    ),
    br(),
    br(),
    br(),
    sprintf(
      r"---{\(
          CI = \displaystyle
          \left(
          \sqrt{\frac{df}{\chi^2_{\alpha/2, df}}} \cdot s, \;\:
          \sqrt{\frac{df}{\chi^2_{1 - \alpha/2, df}}} \cdot s
          \right) \)}---"
    ),
    br(),
    br(),
    br(),
    sprintf(
      r"---(
          \(
          \begin{align}
          CI &= \left( \sqrt{\frac{%d}{%0.3f}} \cdot %0.3f, \;\: \sqrt{\frac{%d}{%0.3f}} \cdot %0.3f \right) \\ \\
             &= \left(%0.2f, %0.2f\right)
          \end{align}
          \)
          )---",
      ## Left/lower
      oneSDCIdf, ## df
      oneSSDLeft,
      input$SSDStdDev, ## s

      ## Right/upper
      oneSDCIdf, ## df
      oneSSDRight,
      input$SSDStdDev, # s
      (oneSSDLowerPopStdDev <- sqrt(oneSDCIdf / oneSSDLeft) * input$SSDStdDev),
      (oneSSDUpperPopStdDev <- sqrt(oneSDCIdf / oneSSDRight) * input$SSDStdDev)
    ),
    br(),
    br(),
    br(),

    ## Step three
    tags$b("Interpretation:"),
    br(),
    sprintf(
      "We are %s confident that the population standard deviation (\\( \\sigma \\)) is between \\( %0.2f \\) and \\( %0.2f \\).",
      input$confidenceLevel,
      oneSSDLowerPopStdDev,
      oneSSDUpperPopStdDev
    )
  )
})


## FIXME: no DTOutput found!
## output$onePopMeanUploadTable <- renderDT({
##   validate(
##     need(!is.null(input$upload), "Please upload a file."),
##     need(!is.null(fileInputs$oneMeanStatus) && fileInputs$oneMeanStatus == "uploaded", "Please upload a file."),
##     need(nrow(Upload()) != 0, "File is empty."),
##     need(nrow(Upload()) > 2, "Samples must include at least 2 observations."),
##     errorClass = "myClass"
##   )
##   if (onemeanupload_iv$is_valid()) {
##     datatable(
##       Upload(),
##       options = list(
##         pageLength = -1,
##         lengthMenu = list(
##           c(25, 50, 100, -1),
##           c("25", "50", "100", "all")
##         ),
##         columnDefs = list(list(
##           className = "dt-center",
##           targets = 0:ncol(Upload())
##         ))
##       )
##     )
##   }
## })

## ---------------- HT Plot ----
output$oneMeanHTPlot <- renderPlot({
  if (input$dataAvailability == "Summarized Data") {
    if (input$sigmaKnown) {
      oneMeanData <- OneMeanZTestSumm()
      sigmaKnown <- "Known"
    } else if (!input$sigmaKnown) {
      oneMeanData <- OneMeanTTestSumm()
      sigmaKnown <- "Unknown"
    }
  } else if (input$dataAvailability == "Enter Raw Data") {
    if (input$sigmaKnown) {
      oneMeanData <- OneMeanZTestRaw()
      sigmaKnown <- "Known"
    } else if (!input$sigmaKnown) {
      oneMeanData <- OneMeanTTestRaw()
      sigmaKnown <- "Unknown"
    }
  } else if (input$dataAvailability == "Upload Data") {
    if (input$sigmaKnown == "Known") {
      oneMeanData <- OneMeanZTestRaw()
      sigmaKnown <- "Known"
    } else if (!input$sigmaKnown) {
      oneMeanData <- OneMeanTTestRaw()
      sigmaKnown <- "Unknown"
    }
  }

  intrpInfo <- OneMeanHypInfo()
  htPlotCritVal <- oneMeanData[4]

  if (input$sigmaKnown) {
    oneMeanPlot <- hypZTestPlot(
      oneMeanData[6],
      htPlotCritVal,
      intrpInfo$alternative
    )
  } else {
    oneMeanPlot <- hypTTestPlot(
      oneMeanData[6],
      oneMeanData[8],
      htPlotCritVal,
      intrpInfo$alternative
    )
  }

  oneMeanPlot
})

## ---------------- Boxplot ----
output$oneMeanBoxplotOutput <- renderPlot(
  {
    req(input$calculate)
    #req(iv$is_valid())# Calculate already requires iv validity.

    if (input$dataAvailability == "Enter Raw Data") {
      dat <- createNumLst(input$sample1)
    } else if (input$dataAvailability == "Upload Data") {
      dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
    } else {
      return(NA)
    }

    df_outliers <- getOutliers(dat, "Sample")
    outlier_vals <- df_outliers$data

    df_boxplot <- data.frame(x = dat)

    RenderBoxplot(
      dat,
      df_boxplot,
      outlier_vals,
      input[["oneMeanBoxplot-Colour"]],
      input[["oneMeanBoxplot-Title"]],
      input[["oneMeanBoxplot-Xlab"]],
      input[["oneMeanBoxplot-Ylab"]],
      input[["oneMeanBoxplot-BoxWidth"]] / 10,
      input[["oneMeanBoxplot-Gridlines"]],
      input[["oneMeanBoxplot-Flip"]],
      input[["oneMeanBoxplot-OutlierLabels"]]
    )
  },
  height = function() {
    GetPlotHeight(
      input[["oneMeanBoxplot-Height"]],
      input[["oneMeanBoxplot-HeightPx"]],
      ui = FALSE
    )
  },
  width = function() {
    GetPlotWidth(
      input[["oneMeanBoxplot-Width"]],
      input[["oneMeanBoxplot-WidthPx"]],
      ui = FALSE
    )
  }
)

output$oneSamplePopulationMeanAnalysis <- renderUI({
  req(input$calculate)
  switch(
    input$inferenceType,
    "Confidence Interval" = printOneMeanCI(),
    "Hypothesis Testing" = printOneMeanHT()
  )
})
