Upload <- createFileInputEventReactive(input, "upload")
## NOTE: this reactive *output* should be ignored except if you want a
## conditionalPanel to depend upon the state of Upload.
## BEGIN Copyright Attribution
## Source - https://stackoverflow.com/a/21535587
## Posted by Stéphane Laurent
## Retrieved 2026-07-20, License - CC BY-SA 3.0
output$Uploaded <- reactive({
  return(!is.null(Upload()))
})
outputOptions(output, "Uploaded", suspendWhenHidden = FALSE)
## END

OneMeanTotaledData <- reactive({
  req(iv$is_valid())

  if (input$dataAvailability == "Enter Raw Data") {
    dat <- createNumLst(input$sample1)
  } else if (input$dataAvailability == "Upload Data") {
    dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
  } else {
    dat <- 0
  }

  totaled <- list(sum(dat), sum(dat^2))
  return(totaled)
})

OneMeanHypInfo <- reactive({
  hypTestSymbols <- list()

  if (input$altHypothesis == "3") {
    hypTestSymbols$alternative <- "greater"
    hypTestSymbols$nullHyp <- "\\mu \\leq"
    hypTestSymbols$altHyp <- "\\mu \\gt"
    hypTestSymbols$critAlph <- "\\alpha"
    hypTestSymbols$critSign <- ""
    hypTestSymbols$alphaVal <- SigLvl()
  } else if (input$altHypothesis == "2") {
    hypTestSymbols$alternative <- "two.sided"
    hypTestSymbols$nullHyp <- "\\mu ="
    hypTestSymbols$altHyp <- "\\mu \\neq"
    hypTestSymbols$critAlph <- "\\alpha/2"
    hypTestSymbols$critSign <- "\\pm"
    hypTestSymbols$alphaVal <- SigLvl() / 2
  } else {
    hypTestSymbols$alternative <- "less"
    hypTestSymbols$nullHyp <- "\\mu \\geq"
    hypTestSymbols$altHyp <- "\\mu \\lt"
    hypTestSymbols$critAlph <- "\\alpha"
    hypTestSymbols$critSign <- "-"
    hypTestSymbols$alphaVal <- SigLvl()
  }

  return(hypTestSymbols)
})

OneMeanSigma <- reactive({
  req(iv$is_valid())

  if (input$dataAvailability == "Summarized Data") {
    sigmaKnown <- input$sigmaKnown
  } else if (input$dataAvailability == "Enter Raw Data") {
    if (input$sigmaKnown) {
      sigmaKnown <- "Known"
    } else {
      sigmaKnown <- "Unknown"
    }
  } else if (input$dataAvailability == "Upload Data") {
    sigmaKnown <- input$sigmaKnown
  }

  return(sigmaKnown)
})

OneMeanZIntSumm <- reactive({
  req(iv$is_valid())

  nSampOne <- input$sampleSize
  xbarSampOne <- input$sampleMean
  sigmaSampOne <- input$popuSD

  oneMeanZInt <- ZInterval(nSampOne, xbarSampOne, sigmaSampOne, ConfLvl())
  oneMeanZInt["Z Critical"] <- round(oneMeanZInt["Z Critical"], cvDigits)

  return(oneMeanZInt)
})

OneMeanZIntRaw <- reactive({
  req(iv$is_valid())

  if (input$dataAvailability == "Enter Raw Data") {
    dat <- createNumLst(input$sample1)
    popuSD <- input$popuSDRaw
  } else if (input$dataAvailability == "Upload Data") {
    dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
    popuSD <- input$popuSDUpload
  }

  sampleSize <- length(dat)
  sampleMean <- mean(dat)

  oneMeanZInt <- ZInterval(sampleSize, sampleMean, popuSD, ConfLvl())
  oneMeanZInt["Z Critical"] <- round(oneMeanZInt["Z Critical"], cvDigits)

  return(oneMeanZInt)
})

OneMeanTIntSumm <- reactive({
  req(iv$is_valid())

  nSampOne <- input$sampleSize
  xbarSampOne <- input$sampleMean
  sSampOne <- input$sampSD

  oneMeanTInt <- TInterval(nSampOne, xbarSampOne, sSampOne, ConfLvl())
  oneMeanTInt["T Critical"] <- round(oneMeanTInt["T Critical"], cvDigits)

  return(oneMeanTInt)
})

OneMeanTIntRaw <- reactive({
  req(iv$is_valid())

  if (input$dataAvailability == "Enter Raw Data") {
    dat <- createNumLst(input$sample1)
  } else if (input$dataAvailability == "Upload Data") {
    dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
  }

  sampleSize <- length(dat)
  sampleMean <- mean(dat)
  sampleSD <- sd(dat)

  oneMeanTInt <- TInterval(sampleSize, sampleMean, sampleSD, ConfLvl())
  oneMeanTInt["T Critical"] <- round(oneMeanTInt["T Critical"], cvDigits)

  return(oneMeanTInt)
})

OneMeanZTestSumm <- reactive({
  req(iv$is_valid())

  nSampOne <- input$sampleSize
  xbarSampOne <- input$sampleMean
  hypMeanSampOne <- input$hypMean
  sigmaSampOne <- input$popuSD

  oneMeanZTest <- ZTest(
    nSampOne, xbarSampOne, sigmaSampOne, hypMeanSampOne,
    OneMeanHypInfo()$alternative, SigLvl()
  )
  oneMeanZTest["Z Critical"] <- round(oneMeanZTest["Z Critical"], cvDigits)

  return(oneMeanZTest)
})

OneMeanZTestRaw <- reactive({
  req(iv$is_valid())

  if (input$dataAvailability == "Enter Raw Data") {
    dat <- createNumLst(input$sample1)
    popuSD <- input$popuSDRaw
  } else if (input$dataAvailability == "Upload Data") {
    dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
    popuSD <- input$popuSDUpload
  }

  sampleSize <- length(dat)
  sampleMean <- mean(dat)
  hypMeanVal <- input$hypMean

  oneMeanZTest <- ZTest(
    sampleSize, sampleMean, popuSD, hypMeanVal,
    OneMeanHypInfo()$alternative, SigLvl()
  )
  oneMeanZTest["Z Critical"] <- round(oneMeanZTest["Z Critical"], cvDigits)

  return(oneMeanZTest)
})

OneMeanTTestSumm <- reactive({
  req(iv$is_valid())

  nSampOne <- input$sampleSize
  xbarSampOne <- input$sampleMean
  hypMeanSampOne <- input$hypMean
  sSampOne <- input$sampSD

  oneMeanTTest <- TTest(
    nSampOne, xbarSampOne, sSampOne, hypMeanSampOne,
    OneMeanHypInfo()$alternative, SigLvl()
  )
  oneMeanTTest["T Critical"] <- round(oneMeanTTest["T Critical"], cvDigits)

  return(oneMeanTTest)
})

OneMeanTTestRaw <- reactive({
  req(iv$is_valid())

  if (input$dataAvailability == "Enter Raw Data") {
    dat <- createNumLst(input$sample1)
  } else if (input$dataAvailability == "Upload Data") {
    dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
  }

  sampleSize <- length(dat)
  sampleMean <- mean(dat)
  sampleSD <- sd(dat)
  hypMeanVal <- input$hypMean

  oneMeanTTest <- TTest(
    sampleSize, sampleMean, sampleSD, hypMeanVal,
    OneMeanHypInfo()$alternative, SigLvl()
  )
  oneMeanTTest["T Critical"] <- round(oneMeanTTest["T Critical"], cvDigits)

  return(oneMeanTTest)
})

GetOneMeanCI <- reactive({
  if (OneMeanSigma() == "Known") {
    if (input$dataAvailability == "Summarized Data") {
      oneMeanCI <- OneMeanZIntSumm()
    } else {
      oneMeanCI <- OneMeanZIntRaw()
    }
  } else {
    if (input$dataAvailability == "Summarized Data") {
      oneMeanCI <- OneMeanTIntSumm()
    } else {
      oneMeanCI <- OneMeanTIntRaw()
    }
  }

  return(oneMeanCI)
})

GetOneMeanHT <- reactive({
  if (OneMeanSigma() == "Known") {
    if (input$dataAvailability == "Summarized Data") {
      oneMeanHT <- OneMeanZTestSumm()
    } else {
      oneMeanHT <- OneMeanZTestRaw()
    }
  } else {
    if (input$dataAvailability == "Summarized Data") {
      oneMeanHT <- OneMeanTTestSumm()
    } else {
      oneMeanHT <- OneMeanTTestRaw()
    }
  }

  return(oneMeanHT)
})

relation <- reactiveVal()

criticalValue <- reactive({
  if (input$confLeveln == "90%") {
    critVal <- 1.645
  } else if (input$confLeveln == "95%") {
    critVal <- 1.96
  } else if (input$confLeveln == "99%") {
    critVal <- 2.576
  }

  return(critVal)
})