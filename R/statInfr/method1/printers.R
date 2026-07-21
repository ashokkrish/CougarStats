printOneMeanHT <- function() {
  oneMeanData <- GetOneMeanHT()
  intrpInfo <- OneMeanHypInfo()

  if (OneMeanSigma() == "Known") {
    sdSymbol <- "\\sigma"
    testStat <- "z"
  } else {
    sdSymbol <- "s"
    testStat <- "t"
  }

  if (oneMeanData[7] > SigLvl()) {
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

  oneMeanHTOutput <- withMathJax(tagList(
    tags$h2(tags$u("Hypothesis Testing")),
    sprintf(
      "\\( H_{0}: %s %s\\)",
      intrpInfo$nullHyp,
      input$hypMean
    ),
    br(),
    sprintf(
      "\\( H_{a}: %s %s\\)",
      intrpInfo$altHyp,
      input$hypMean
    ),
    br(),
    br(),
    sprintf(
      "\\( \\alpha = %s \\)",
      SigLvl()
    ),
    br(),
    br(),
    p(tags$b("Test Statistic:"))
  ))

  formulaOutput <- printOneMeanHTFormula(sdSymbol, testStat)
  pvalOutput <- printHTPVal(oneMeanData["P-Value"], testStat, intrpInfo$alternative, oneMeanData["Test Statistic"], pvalSymbol, reject)
  cvOutput <- printOneMeanHTCV(testStat, reject, region)
  conclusionOutput <- printHTConclusion(region, reject, suffEvidence, OneMeanHypInfo()$altHyp, input$hypMean)

  tagAppendChildren(oneMeanHTOutput, printOneMeanGiven(), formulaOutput, pvalOutput, cvOutput, conclusionOutput)
}


printOneMeanHTFormula <- function(sdSymbol, testStat) {
  oneMeanData <- GetOneMeanHT()

  formulaOutput <- tagList(
    sprintf(
      "\\(%s = \\dfrac{\\bar{x} - \\mu_{0}}{ \\dfrac{%s}{\\sqrt{n}} } \\)",
      testStat,
      sdSymbol
    ),
    br()
  )

  if (input$dataAvailability != "Summarized Data") {
    formulaOutput <- tagAppendChild(formulaOutput, printOneMeanWhere(oneMeanData))
  } else {
    formulaOutput <- tagAppendChildren(formulaOutput, br(), br())
  }

  hypPopMean <- input$hypMean
  if (hypPopMean < 0) {
    hypPopMean <- paste("(", hypPopMean, ")", sep = "")
  }

  calcOutput <- tagList(
    sprintf(
      "\\(%s =  \\dfrac{%g - %g}{ \\dfrac{%g}{\\sqrt{%g}} }\\)",
      testStat,
      oneMeanData[2],
      hypPopMean,
      oneMeanData[3],
      oneMeanData[1]
    ),
    sprintf(
      "\\( = \\dfrac{%g}{%g} \\)",
      oneMeanData[2] - input$hypMean,
      oneMeanData["Std Error"]
    ),
    br(),
    br(),
    sprintf(
      "\\(\\phantom{%s} = %g\\)",
      testStat,
      oneMeanData[6]
    ),
    br(),
    br(),
    br()
  )

  formulaOutput <- tagAppendChild(formulaOutput, calcOutput)
  return(formulaOutput)
}

printOneMeanHTCV <- function(testStat, reject, region) {
  oneMeanData <- GetOneMeanHT()

  if (testStat == "z") {
    critVal <- paste(oneMeanData["Z Critical"])
  } else {
    critVal <- paste(oneMeanData["T Critical"])
  }

  if (OneMeanHypInfo()$alternative == "two.sided") {
    critVal <- paste("\\pm", critVal)
  }

  if (testStat == "z") {
    cvOutput <- tagList(
      p(tags$b("Using Critical Value Method:")),
      sprintf(
        "Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
        OneMeanHypInfo()$critSign,
        OneMeanHypInfo()$critAlph,
        OneMeanHypInfo()$critSign,
        OneMeanHypInfo()$alphaVal,
        critVal
      ),
      br(),
      br()
    )
  } else {
    cvOutput <- tagList(
      p(tags$b("Using Critical Value Method:")),
      sprintf(
        "\\( df = n - 1 = %s \\)",
        oneMeanData["Sample Size"] - 1
      ),
      br(),
      br(),
      sprintf(
        "Critical Value(s) \\( = %s t_{%s, \\, df} = %s t_{%s, \\, %s} = %s \\)",
        OneMeanHypInfo()$critSign,
        OneMeanHypInfo()$critAlph,
        OneMeanHypInfo()$critSign,
        OneMeanHypInfo()$alphaVal,
        oneMeanData["Sample Size"] - 1,
        critVal
      ),
      br(),
      br()
    )
  }

  cvEnd <- tagList(
    sprintf(
      "Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
      testStat,
      region,
      reject
    ),
    br(),
    br(),
    plotOutput(session$ns("oneMeanHTPlot"), width = "75%", height = "300px"),
    br()
  )

  cvOutput <- tagAppendChild(cvOutput, cvEnd)
  return(cvOutput)
}



printOneMeanCI <- function() {
  if (OneMeanSigma() == "Known") {
    sdSymbol <- "\\sigma"
    testStat <- "z"
    critVal <- GetOneMeanCI()["Z Critical"]
  } else {
    sdSymbol <- "s"
    testStat <- "t"
    critVal <- GetOneMeanCI()["T Critical"]
  }

  alphaOutput <- tagList(
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
    br()
  )

  withMathJax(tagList(
    tags$h2(tags$u("Confidence Interval")),
    printOneMeanGiven(),
    alphaOutput,
    printOneMeanCV(),
    printOneMeanCIFormula(),
    printOneMeanCICalc(),
    printOneMeanCIIntrp()
  ))
}

printOneMeanCV <- function() {
  oneMeanData <- GetOneMeanCI()

  if (OneMeanSigma() == "Known") {
    cvOutput <- tagList(
      sprintf(
        "\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
        1 - ConfLvl(),
        (1 - ConfLvl()) / 2,
        oneMeanData["Z Critical"]
      ),
      br(),
      br(),
      br()
    )
  } else {
    df <- oneMeanData["Sample Size"] - 1

    cvOutput <- tagList(
      sprintf(
        "\\( df = n - 1 = %s - 1 = %s\\)",
        oneMeanData["Sample Size"],
        oneMeanData["Sample Size"] - 1
      ),
      br(),
      sprintf(
        "\\( t_{\\alpha/2, \\, df} = t_{%s/2, \\, %s} = t_{%s, \\, %s} = %s \\)",
        1 - ConfLvl(),
        df,
        (1 - ConfLvl()) / 2,
        df,
        oneMeanData["T Critical"]
      ),
      br(),
      br(),
      br()
    )
  }

  return(cvOutput)
}

printOneMeanGiven <- function() {
  oneMeanData <- GetOneMeanCI()

  if (input$dataAvailability == "Summarized Data") {
    if (OneMeanSigma() == "Known") {
      sd <- "\\sigma"
    } else {
      sd <- "s"
    }

    given <- tagList(
      sprintf("Given:"),
      br(),
      sprintf(
        "\\( n = %s \\)",
        oneMeanData["Sample Size"]
      ),
      br(),
      sprintf(
        "\\( \\bar{x} = %s \\)",
        oneMeanData["Sample Mean"]
      ),
      br(),
      sprintf(
        "\\( %s = %s \\)",
        sd,
        oneMeanData[3]
      ),
      br(),
      br(),
      br()
    )
  } else {
    if (OneMeanSigma() == "Known") {
      given <- tagList(
        sprintf("Given:"),
        br(),
        sprintf(
          "\\( \\sigma = %s \\)",
          oneMeanData[3]
        ),
        br(),
        br(),
        br()
      )
    } else {
      given <- br()
    }
  }
}

printOneMeanCIFormula <- function() {
  oneMeanData <- GetOneMeanCI()

  if (OneMeanSigma() == "Known") {
    sd <- "\\sigma"
    testStat <- "z_{\\alpha/2}"
  } else {
    sd <- "s"
    testStat <- "t_{\\alpha/2, \\, df}"
  }

  formulaOutput <- tagList(
    sprintf(
      "\\( \\displaystyle CI = \\bar{x} \\pm \\left( %s \\dfrac{%s}{\\sqrt{n}} \\right) \\)",
      testStat,
      sd
    ),
    br()
  )

  if (input$dataAvailability != "Summarized Data") {
    formulaOutput <- tagAppendChild(formulaOutput, printOneMeanWhere(oneMeanData))
  } else {
    formulaOutput <- tagAppendChildren(formulaOutput, br(), br())
  }

  return(formulaOutput)
}

printOneMeanWhere <- function(oneMeanData) {
  if (OneMeanSigma() == "Known") {
    formulaOutput <- tagList(
      sprintf("where"),
      br(),
      sprintf(
        "\\( \\phantom{CII} n = %s \\; , \\)",
        oneMeanData["Sample Size"]
      ),
      sprintf(
        "\\( \\phantom{CII} \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%g} = %g \\)",
        OneMeanTotaledData()[1],
        oneMeanData["Sample Size"],
        oneMeanData["Sample Mean"]
      ),
      br(),
      br(),
      br()
    )
  } else {
    formulaOutput <- tagList(
      sprintf("where"),
      br(),
      sprintf(
        "\\( \\phantom{CII} n = %s \\; , \\)",
        oneMeanData["Sample Size"]
      ),
      sprintf(
        "\\( \\phantom{CII} \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%g} = %g \\; , \\)",
        OneMeanTotaledData()[1],
        oneMeanData["Sample Size"],
        oneMeanData["Sample Mean"]
      ),
      br(),
      sprintf("and"),
      br(),
      sprintf("\\( \\phantom{CII} s  = \\sqrt{ \\dfrac{\\sum x^{2} - \\dfrac{(\\sum x)^{2}}{n} }{n - 1} } \\)"),
      sprintf(
        "\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%g} }{%g - 1} } = %g \\)",
        OneMeanTotaledData()[2],
        OneMeanTotaledData()[1],
        oneMeanData["Sample Size"],
        oneMeanData["Sample Size"],
        oneMeanData[3]
      ),
      br(),
      br(),
      br()
    )
  }
}

printOneMeanCICalc <- function() {
  oneMeanData <- GetOneMeanCI()

  if (OneMeanSigma() == "Known") {
    critVal <- oneMeanData["Z Critical"]
  } else {
    critVal <- oneMeanData["T Critical"]
  }

  calcOutput <- tagList(
    sprintf(
      "\\( \\displaystyle CI = %s \\pm \\left( %g \\dfrac{%g}{\\sqrt{%g}} \\right) \\)",
      oneMeanData["Sample Mean"],
      critVal,
      oneMeanData[3],
      oneMeanData["Sample Size"]
    ),
    br(),
    br(),
    sprintf(
      "\\( \\displaystyle \\phantom{CI} = %s \\pm \\left( %g \\cdot %g \\right) \\)",
      oneMeanData["Sample Mean"],
      critVal,
      oneMeanData["Std Error"]
    ),
    br(),
    br(),
    sprintf(
      "\\( \\displaystyle \\phantom{CI} = %s \\pm %g \\)",
      oneMeanData["Sample Mean"],
      oneMeanData["ME"]
    ),
    br(),
    br(),
    sprintf(
      "\\( \\phantom{CI} = (%g, %g)\\)",
      oneMeanData["LCL"],
      oneMeanData["UCL"]
    ),
    br(),
    br(),
    br()
  )

  return(calcOutput)
}

printOneMeanCIIntrp <- function() {
  oneMeanData <- GetOneMeanCI()

  intrpOutput <- tagList(
    p(tags$b("Interpretation:")),
    sprintf(
      "We are %1.0f%% confident that the population mean \\( (\\mu)\\) is between \\( %g \\) and \\( %g \\).",
      ConfLvl() * 100,
      oneMeanData["LCL"],
      oneMeanData["UCL"]
    ),
    br()
  )

  return(intrpOutput)
}
