
## ------------ Independent Sample Means reactives --------------------------
IndMeansSummData <- reactive({
  req(si_iv$is_valid())

  summData <- list()

  summData$n1 <- input$sampleSize1
  summData$xbar1 <- input$sampleMean1
  summData$n2 <- input$sampleSize2
  summData$xbar2 <- input$sampleMean2
  summData$sigmaEqual <- input$bothsigmaEqual

  if (input$bothsigmaKnown == "bothKnown") {
    summData$sd1 <- input$popuSD1
    summData$sd2 <- input$popuSD2
  } else {
    summData$sd1 <- input$sampSD1
    summData$sd2 <- input$sampSD2
  }

  return(summData)
})

IndMeansRawData <- reactive({
  req(si_iv$is_valid())

  rawData <- list()

  raw_sample1 <- createNumLst(input$raw_sample1)
  raw_sample2 <- createNumLst(input$raw_sample2)

  rawData$n1 <- length(raw_sample1)
  rawData$xbar1 <- mean(raw_sample1)
  rawData$n2 <- length(raw_sample2)
  rawData$xbar2 <- mean(raw_sample2)
  rawData$sigmaEqual <- input$bothsigmaEqualRaw

  if (input$bothsigmaKnownRaw == "bothKnown") {
    rawData$sd1 <- input$popuSDRaw1
    rawData$sd2 <- input$popuSDRaw2
  } else {
    rawData$sd1 <- sd(raw_sample1)
    rawData$sd2 <- sd(raw_sample2)
  }

  return(rawData)
})

IndMeansUploadData <- createFileInputEventReactive(input, "indMeansUserData")

GetMeansUploadData <- reactive({
  req(input$indMeansUplSample1, input$indMeansUplSample2)

  dat <- list()

  sample1 <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample1]))
  sample2 <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample2]))

  dat$n1 <- length(sample1)
  dat$xbar1 <- mean(sample1)
  dat$n2 <- length(sample2)
  dat$xbar2 <- mean(sample2)
  dat$sigmaEqual <- input$bothsigmaEqualUpload

  if (input$bothsigmaKnownUpload == "bothKnown") {
    dat$sd1 <- input$popuSDUpload1
    dat$sd2 <- input$popuSDUpload2
  } else {
    dat$sd1 <- sd(sample1)
    dat$sd2 <- sd(sample2)
  }

  return(dat)
})

IndMeansSigmaKnown <- reactive({
  if (input$dataAvailability2 == "Summarized Data") {
    sigmaKnown <- input$bothsigmaKnown
  } else if (input$dataAvailability2 == "Enter Raw Data") {
    sigmaKnown <- input$bothsigmaKnownRaw
  } else if (input$dataAvailability2 == "Upload Data") {
    sigmaKnown <- input$bothsigmaKnownUpload
  }

  return(sigmaKnown)
})

IndMeansHypInfo <- reactive({
  hypTestSymbols <- list()

  if (input$altHypothesis2 == "3") {
    hypTestSymbols$alternative <- "greater"
    hypTestSymbols$nullHyp <- "\\leq"
    hypTestSymbols$altHyp <- "\\gt"
    hypTestSymbols$critAlph <- "\\alpha"
    hypTestSymbols$critSign <- ""
    hypTestSymbols$alphaVal <- SigLvl()
  } else if (input$altHypothesis2 == "2") {
    hypTestSymbols$alternative <- "two.sided"
    hypTestSymbols$nullHyp <- "="
    hypTestSymbols$altHyp <- "\\neq"
    hypTestSymbols$critAlph <- "\\alpha/2"
    hypTestSymbols$critSign <- "\\pm"
    hypTestSymbols$alphaVal <- SigLvl() / 2
  } else {
    hypTestSymbols$alternative <- "less"
    hypTestSymbols$nullHyp <- "\\geq"
    hypTestSymbols$altHyp <- "\\lt"
    hypTestSymbols$critAlph <- "\\alpha"
    hypTestSymbols$critSign <- "-"
    hypTestSymbols$alphaVal <- SigLvl()
  }

  return(hypTestSymbols)
})

IndMeansZInt <- reactive({
  req(si_iv$is_valid())

  if (input$dataAvailability2 == "Summarized Data") {
    data <- IndMeansSummData()
  } else if (input$dataAvailability2 == "Enter Raw Data") {
    data <- IndMeansRawData()
  } else if (input$dataAvailability2 == "Upload Data") {
    data <- GetMeansUploadData()
  }

  twoSampZInt <- TwoSampZInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, ConfLvl())
  twoSampZInt["Z Critical"] <- round(twoSampZInt["Z Critical"], cvDigits)

  return(twoSampZInt)
})

IndMeansTInt <- reactive({
  req(si_iv$is_valid())

  if (input$dataAvailability2 == "Summarized Data") {
    data <- IndMeansSummData()
  } else if (input$dataAvailability2 == "Enter Raw Data") {
    data <- IndMeansRawData()
  } else if (input$dataAvailability2 == "Upload Data") {
    data <- GetMeansUploadData()
  }

  twoSampTInt <- TwoSampTInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, data$sigmaEqual, ConfLvl())
  twoSampTInt["T Critical"] <- round(twoSampTInt["T Critical"], cvDigits)

  return(twoSampTInt)
})

IndMeansZTest <- reactive({
  req(si_iv$is_valid())

  if (input$dataAvailability2 == "Summarized Data") {
    data <- IndMeansSummData()
  } else if (input$dataAvailability2 == "Enter Raw Data") {
    data <- IndMeansRawData()
  } else if (input$dataAvailability2 == "Upload Data") {
    data <- GetMeansUploadData()
  }

  muNaught <- input$indMeansMuNaught

  twoSampZTest <- TwoSampZTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, IndMeansHypInfo()$alternative, SigLvl(), muNaught)
  twoSampZTest["Z Critical"] <- round(twoSampZTest["Z Critical"], cvDigits)

  return(twoSampZTest)
})

IndMeansTTest <- reactive({
  req(si_iv$is_valid())

  if (input$dataAvailability2 == "Summarized Data") {
    data <- IndMeansSummData()
  } else if (input$dataAvailability2 == "Enter Raw Data") {
    data <- IndMeansRawData()
  } else if (input$dataAvailability2 == "Upload Data") {
    data <- GetMeansUploadData()
  }

  muNaught <- input$indMeansMuNaught

  twoSampTTest <- TwoSampTTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, data$sigmaEqual, IndMeansHypInfo()$alternative, SigLvl(), muNaught)
  twoSampTTest["T Critical"] <- round(twoSampTTest["T Critical"], cvDigits)

  return(twoSampTTest)
})


## ------------ Wilcoxon Rank Sum Reactives -----------------------------------
WilcoxonUploadData <- createFileInputEventReactive(input, "wilcoxonUpl")

CheckRankSumUploadSamples <- eventReactive(c(input$wilcoxonUpl1, input$wilcoxonUpl2), {
  if (input$wilcoxonUpl1 == "" | input$wilcoxonUpl2 == "") {
    return(0)
  } else {
    before <- unlist(WilcoxonUploadData()[, input$wilcoxonUpl1])
    after <- unlist(WilcoxonUploadData()[, input$wilcoxonUpl2])
    difference <- length(na.omit(before)) - length(na.omit(after))
    return(difference)
  }
})
wRankSumTInt <- reactive({
  req(si_iv$is_valid())

  data <- GetwRankSumMeansData()

  wRankSumTInt <- TInterval(data$n, data$dbar, data$sd, ConfLvl())
  wRankSumTInt["T Critical"] <- round(wRankSumTInt["T Critical"], cvDigits)

  return(wRankSumTInt)
})

wRankSumTTest <- reactive({
  req(si_iv$is_valid())

  data <- GetwRankSumMeansData()

  n_total <- data$n1 + data$n2
  pooled_mean <- (data$mean1 * data$n1 + data$mean2 * data$n2) / n_total
  combined_data <- c(data$samp1, data$samp2)
  pooled_sd <- sd(combined_data)

  wRankSumTTest <- TTest(n_total, pooled_mean, pooled_sd, 0, IndMeansHypInfo()$alternative, SigLvl())
  wRankSumTTest["T Critical"] <- round(wRankSumTTest["T Critical"], cvDigits)

  return(wRankSumTTest)
})

wilcoxonRankedData <- reactive({
  req(input$wilcoxonRankSumTestData)

  if (input$wilcoxonRankSumTestData == "Enter Raw Data") {
    sample1_vals <- as.numeric(unlist(strsplit(input$rankSumRaw1, ",")))
    sample2_vals <- as.numeric(unlist(strsplit(input$rankSumRaw2, ",")))

    group1_name <- "Sample 1"
    group2_name <- "Sample 2"
  } else if (input$wilcoxonRankSumTestData == "Upload Data") {
    req(wilcoxonUpload_iv$is_valid())
    uploaded_data <- WilcoxonUploadData()
    req(input$wilcoxonUpl1, input$wilcoxonUpl2)

    sample1_vals <- na.omit(uploaded_data[[input$wilcoxonUpl1]])
    sample2_vals <- na.omit(uploaded_data[[input$wilcoxonUpl2]])

    group1_name <- input$wilcoxonUpl1
    group2_name <- input$wilcoxonUpl2
  } else {
    return(NULL)
  }

  combined_data <- data.frame(
    values = c(sample1_vals, sample2_vals),
    ind = c(
      rep(group1_name, length(sample1_vals)),
      rep(group2_name, length(sample2_vals))
    )
  )

  ranked_data <- combined_data %>%
    dplyr::mutate(Rank = rank(values, ties.method = "average")) %>%
    dplyr::select(Group = ind, Value = values, Rank) %>%
    dplyr::arrange(Group, Rank)

  return(ranked_data)
})

output$wilcoxonRankSumDataRanks <- renderUI({
  req(wilcoxonRankedData())
  RankedTableOutput(wilcoxonRankedData())
})


## ------------ Dependent Means Reactives -----------------------------------
DepMeansUploadData <- createFileInputEventReactive(input, "depMeansUserData")

CheckDepUploadSamples <- eventReactive(c(
  input$depMeansUplSample1,
  input$depMeansUplSample2
), {
  if (input$depMeansUplSample1 == "" | input$depMeansUplSample2 == "") {
    return(0)
  } else {
    before <- unlist(DepMeansUploadData()[, input$depMeansUplSample1])
    after <- unlist(DepMeansUploadData()[, input$depMeansUplSample2])
    difference <- length(na.omit(before)) - length(na.omit(after))
    return(difference)
  }
})

DepMeansTInt <- reactive({
  req(si_iv$is_valid())

  data <- GetDepMeansData()

  depMeansTInt <- TInterval(data$n, data$dbar, data$sd, ConfLvl())
  depMeansTInt["T Critical"] <- round(depMeansTInt["T Critical"], cvDigits)

  return(depMeansTInt)
})


DepMeansTTest <- reactive({
  req(si_iv$is_valid() && depmeansrawsd_iv$is_valid())

  data <- GetDepMeansData()

  depMeansTTest <- TTest(data$n, data$dbar, data$sd, data$muNaught, IndMeansHypInfo()$alternative, SigLvl())
  depMeansTTest["T Critical"] <- round(depMeansTTest["T Critical"], cvDigits)

  return(depMeansTTest)
})

## ------------ Signed Rank Test Reactives ------------------------------------------
rv <- reactiveValues(
  calculatePressed = FALSE,
  allowColumnValidation = TRUE
)

signedRankUploadData <- createFileInputEventReactive(input, "signedRankUpl")

CheckSignedRankUploadSamples <- eventReactive(c(input$signedRankUpl1, input$signedRankUpl2), {
  if (input$signedRankUpl1 == "" | input$signedRankUpl2 == "") {
    return(0)
  } else {
    before <- unlist(signedRankUploadData()[, input$signedRankUpl1])
    after <- unlist(signedRankUploadData()[, input$signedRankUpl2])
    difference <- length(na.omit(before)) - length(na.omit(after))
    return(difference)
  }
})
signedRankTInt <- reactive({
  req(si_iv$is_valid())

  data <- GetSignedRankMeansData()

  signedRankTInt <- TInterval(data$n, data$dbar, data$sd, ConfLvl())
  signedRankTInt["T Critical"] <- round(signedRankTInt["T Critical"], cvDigits)

  return(signedRankTInt)
})

signedRankTTest <- reactive({
  req(si_iv$is_valid())

  data <- GetSignedRankMeansData()

  n_total <- data$n1 + data$n2
  pooled_mean <- (data$mean1 * data$n1 + data$mean2 * data$n2) / n_total
  combined_data <- c(data$samp1, data$samp2)
  pooled_sd <- sd(combined_data)

  signedRankTTest <- TTest(n_total, pooled_mean, pooled_sd, 0, IndMeansHypInfo()$alternative, SigLvl())
  signedRankTTest["T Critical"] <- round(signedRankTTest["T Critical"], cvDigits)

  return(signedRankTTest)
})

signedRankedData <- reactive({
  req(input$signedRankTest)
  req(rv$calculatePressed)

  if (input$signedRankTest == "Enter Raw Data") {
    sample1_vals <- as.numeric(unlist(strsplit(input$signedRankRaw1, ",")))
    sample2_vals <- as.numeric(unlist(strsplit(input$signedRankRaw2, ",")))

    group1_name <- "Sample 1"
    group2_name <- "Sample 2"
  } else if (input$signedRankTest == "Upload Data") {
    req(signedRankUpload_iv$is_valid())
    uploaded_data <- signedRankUploadData()
    req(input$signedRankUpl1, input$signedRankUpl2)

    sample1_vals <- na.omit(uploaded_data[[input$signedRankUpl1]])
    sample2_vals <- na.omit(uploaded_data[[input$signedRankUpl2]])

    group1_name <- input$signedRankUpl1
    group2_name <- input$signedRankUpl2
  } else {
    return(NULL)
  }

  min_length <- min(length(sample1_vals), length(sample2_vals))
  sample1_vals <- sample1_vals[1:min_length]
  sample2_vals <- sample2_vals[1:min_length]

  paired_data <- data.frame(
    PairID = 1:min_length,
    Sample1 = sample1_vals,
    Sample2 = sample2_vals,
    stringsAsFactors = FALSE
  )

  signed_rank_data <- paired_data %>%
    dplyr::mutate(
             Difference = Sample1 - Sample2,
             AbsDifference = abs(Difference)
           ) %>%
    dplyr::filter(Difference != 0) %>%
    dplyr::mutate(
             Rank = rank(AbsDifference, ties.method = "average"),
             SignedRank = ifelse(Difference > 0, Rank, -Rank)
           ) %>%
    dplyr::select(
             Group = PairID, ## Using PairID as a placeholder for Group
             Sample1,
             Sample2,
             Value = Difference, ## Using Difference as Value for compatibility
             Rank,
             SignedRank
           ) %>%
    dplyr::arrange(Rank)

  return(signed_rank_data)
})

output$signedRankDataRanks <- renderUI({
  req(signedRankedData())
  SignedRankTableOutput(signedRankedData())
})

## ------------ Two Prop Reactives ------------------------------------------
checkTwoProp <- reactive({
  if (is.na(input$numSuccesses1) || is.na(input$numSuccesses2)) {
    return(-1)
  } else {
    return(input$numSuccesses1 + input$numSuccesses2)
  }
})

## ------------ Two Pop Var Reactives --------------------------------------
GetTwoPopVarData <- reactive({
  req(si_iv$is_valid())

  dat <- list()

  if (input$dataAvailability3 == "Summary") {
    dat$n1 <- input$SDSampleSize1
    dat$n2 <- input$SDSampleSize2
    dat$sd1 <- input$stdDev1
    dat$sd2 <- input$stdDev2
  } else if (input$dataAvailability3 == "Variance") {
    dat$n1 <- input$n1
    dat$n2 <- input$n2
    dat$sd1 <- input$s1sq
    dat$sd2 <- input$s2sq
  }

  return(dat)
})

GetTwoPopVarRawData <- reactive({
  req(si_iv$is_valid())

  dat <- list()

  if (input$dataAvailability3 == "Enter Raw Data") {
    samp1 <- createNumLst(input$rawSamp1SD)
    samp2 <- createNumLst(input$rawSamp2SD)
  } else if (input$dataAvailability3 == "Upload") {
    ## future work, uploading files not implemented currently
  }

  dat$sample1 <- samp1
  dat$sample2 <- samp2
  dat$n1 <- length(samp1)
  dat$n2 <- length(samp2)
  dat$sd1 <- sd(dat$sample1)
  dat$sd2 <- sd(dat$sample2)

  return(dat)
})

TwoPopVarHypInfo <- reactive({
  hypTestSymbols <- list()

  if (input$altHypothesis2 == "3") {
    hypTestSymbols$alternative <- "greater"
    hypTestSymbols$nullHyp <- "\\sigma^2_1 \\leq \\sigma^2_2"
    hypTestSymbols$altHyp <- "\\sigma^2_1 \\gt \\sigma^2_2"
    hypTestSymbols$critAlph <- "\\alpha"
    hypTestSymbols$critSign <- ""
    hypTestSymbols$alphaVal <- SigLvl()
  } else if (input$altHypothesis2 == "2") {
    hypTestSymbols$alternative <- "two.sided"
    hypTestSymbols$nullHyp <- "\\sigma^2_1 = \\sigma^2_2"
    hypTestSymbols$altHyp <- "\\sigma^2_1 \\neq \\sigma^2_2"
    hypTestSymbols$critAlph <- "\\alpha/2"
    hypTestSymbols$critSign <- "\\pm"
    hypTestSymbols$alphaVal <- SigLvl() / 2
  } else { ## less
    hypTestSymbols$alternative <- "less"
    hypTestSymbols$nullHyp <- "\\sigma^2_1 \\geq \\sigma^2_2"
    hypTestSymbols$altHyp <- "\\sigma^2_1 \\lt \\sigma^2_2"
    hypTestSymbols$critAlph <- "\\alpha"
    hypTestSymbols$critSign <- "-"
    hypTestSymbols$alphaVal <- SigLvl()
  }

  return(hypTestSymbols)
})

GetAllTwoPopVarData <- reactive({
  if (input$dataAvailability3 == "Enter Raw Data") {
    data <- GetTwoPopVarRawData()
  } else if (input$dataAvailability3 == "Upload Data") {
    ## future work, upload not currently implemented
  } else { ## Summary or Variance
    data <- GetTwoPopVarData()
  }
  return(data)
})


## ---------------- Independent Population Means Validation
if (!indmeanssumm_iv$is_valid()) {
  validate(
    need(input$sampleSize1, "Sample Size 1 (n1) must be an integer greater than 1.") %||%
    need(input$sampleSize1 > 1 & input$sampleSize1 %% 1 == 0, "Sample Size 1 (n1) must be an integer greater than 1."),
    need(input$sampleMean1, "Sample Mean 1 required."),
    need(input$sampleSize2, "Sample Size 2 (n2) must be an integer greater than 1.") %||%
    need(input$sampleSize2 > 1 & input$sampleSize2 %% 1 == 0, "Sample Size 2 (n2) must be an integer greater than 1."),
    need(input$sampleMean2, "Sample Mean 2 required."),
    errorClass = "myClass"
  )
}

if (!indmeanssdknown_iv$is_valid()) {
  validate(
    need(input$popuSD1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
    need(input$popuSD2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
    errorClass = "myClass"
  )
}

if (!indmeanssdunk_iv$is_valid()) {
  validate(
    need(input$sampSD1 && input$sampSD1 > 0, "Sample Standard Deviation (s1) must be positive."),
    need(input$sampSD2 && input$sampSD2 > 0, "Sample Standard Deviation (s2) must be positive."),
    errorClass = "myClass"
  )
}

if (!indmeansraw_iv$is_valid()) {
  validate(
    need(input$raw_sample1, "Sample 1 requires a minimum of 3 data points.") %||%
    need(length(createNumLst(input$raw_sample1)) > 2, "Sample Data requires a minimum of 3 data points."),
    need(input$raw_sample2, "Sample 2 requires a minimum of 3 data points.") %||%
    need(length(createNumLst(input$raw_sample2)) > 2, "Sample Data requires a minimum of 3 data points."),
    errorClass = "myClass"
  )

  validate("Samples require a minimum of 3 data points.")
}

if (!indmeansrawsd_iv$is_valid()) {
  validate(
    need(input$popuSDRaw1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
    need(input$popuSDRaw2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
    errorClass = "myClass"
  )
}

if (!indmeansrawsdunk_iv$is_valid()) {
  validate(
    need(sd(createNumLst(input$raw_sample1)) != 0 && sd(createNumLst(input$raw_sample2)) != 0, "The test statistic (t) will be undefined when the sample standard deviation of Sample 1 and Sample 2 are both 0."),
    errorClass = "myClass"
  )
}

if (!indmeansupload_iv$is_valid()) {
  if (is.null(input$indMeansUserData)) {
    validate("Please upload a file.")
  }

  validate(
    need(!is.null(fileInputs$indMeansStatus) && fileInputs$indMeansStatus == "uploaded", "Please upload a file."),
    errorClass = "myClass"
  )

  validate(
    need(nrow(IndMeansUploadData()) != 0, "File is empty."),
    need(ncol(IndMeansUploadData()) > 1, "File must contain at least 2 distinct samples to choose from for analysis."),
    need(nrow(IndMeansUploadData()) > 2, "Samples must include at least 2 observations."),
    errorClass = "myClass"
  )
}

if (!indmeansuploadvar_iv$is_valid()) {
  validate(
    need(input$indMeansUplSample1, "Please select a column for Sample 1."),
    need(input$indMeansUplSample2, "Please select a column for Sample 2."),
    errorClass = "myClass"
  )

  validate(
    need(
      !checkNumeric(IndMeansUploadData(), input$indMeansUplSample1),
      "Sample 1 must be numeric."
    ),
    errorClass = "myClass"
  )

  validate(
    need(
      !checkNumeric(IndMeansUploadData(), input$indMeansUplSample2),
      "Sample 2 must be numeric."
    ),
    errorClass = "myClass"
  )

  sample1Data <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample1]))
  validate(
    need(length(sample1Data) > 1, "Sample 1 must have at least 2 observations."),
    errorClass = "myClass"
  )

  sample2Data <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample2]))
  validate(
    need(length(sample2Data) > 1, "Sample 2 must have at least 2 observations."),
    errorClass = "myClass"
  )

  if (input$bothsigmaKnownUpload == "bothUnknown") {
    data <- GetMeansUploadData()
    sd1 <- data$sd1
    sd2 <- data$sd2

    validate(
      need(
        !(sd1 == 0 && sd2 == 0),
        "Both selected columns have a sample standard deviation of 0, so the test statistic (t) is undefined."
      ),
      errorClass = "myClass"
    )
  }
}

if (!indmeansuploadsd_iv$is_valid()) {
  validate(
    need(input$popuSDUpload1 && input$popuSDUpload1 > 0, "Population Standard Deviation 1 must be positive."),
    need(input$popuSDUpload2 && input$popuSDUpload2 > 0, "Population Standard Deviation 2 must be positive."),
    errorClass = "myClass"
  )
}

if (!indmeansmunaught_iv$is_valid()) {
  validate(
    need(input$indMeansMuNaught, "Hypothesized value of the Population Mean Difference is required."),
    errorClass = "myClass"
  )
}


## ---------------- Wilcoxon Rank Sum Validation
if (!wilcoxonraw_iv$is_valid()) {
  validate(
    need(input$rankSumRaw1, "Sample 1 data requires a minimum of three data points.") %||%
    need(length(createNumLst(input$rankSumRaw1)) > 2, "Sample 1 data requires a minimum of three data points."),
    need(input$rankSumRaw2, "Sample 2 data requires a minimum of three data points.") %||%
    need(length(createNumLst(input$rankSumRaw2)) > 2, "Sample 2 data requires a minimum of three data points."),
    errorClass = "myClass"
  )

  validate(
    need(length(createNumLst(input$rankSumRaw1)) == length(createNumLst(input$rankSumRaw2)), "Same number of data points required for Sample 1 and Sample 2."),
    errorClass = "myClass"
  )
}

if (!wilcoxonUpload_iv$is_valid()) {
  if (is.null(input$wilcoxonUpl)) {
    validate("Please upload a file.")
  }

  validate(
    need(!is.null(fileInputs$rankSumStatus) && fileInputs$rankSumStatus == "uploaded", "Please upload a file."),
    errorClass = "myClass"
  )

  validate(
    need(nrow(WilcoxonUploadData()) > 0, "File is empty."),
    need(ncol(WilcoxonUploadData()) >= 2, "File must contain at least 2 distinct sample 1 and sample 2 sets of data to choose from for analysis."),
    need(nrow(WilcoxonUploadData()) >= 3, "Samples must include at least 3 observations."),
    errorClass = "myClass"
  )
}

if (!wilcoxonRanksuploadvars_iv$is_valid()) {
  validate(
    need(input$wilcoxonUpl1, "Please select a column for sample 1."),
    need(input$wilcoxonUpl2, "Please select a column for sample 2."),
    need(CheckRankSumUploadSamples() == 0, "Same number of data points required for Sample 1 and Sample 2."),
    errorClass = "myClass"
  )
}


## ---------------- Wilcoxon Signed Rank Test Validation

if (!signedRankRaw_iv$is_valid()) {
  validate(
    need(input$signedRankRaw1, "Sample 1 data requires a minimum of three data points.") %||%
    need(length(createNumLst(input$signedRankRaw1)) > 2, "Sample 1 data requires a minimum of three data points."),
    need(input$signedRankRaw2, "Sample 2 data requires a minimum of three data points.") %||%
    need(length(createNumLst(input$signedRankRaw2)) > 2, "Sample 2 data requires a minimum of three data points."),
    errorClass = "myClass"
  )

  validate(
    need(length(createNumLst(input$signedRankRaw1)) == length(createNumLst(input$signedRankRaw2)), "Same number of data points required for Sample 1 and Sample 2."),
    errorClass = "myClass"
  )

  if (length(createNumLst(input$signedRankRaw1)) == length(createNumLst(input$signedRankRaw2))) {
    differences <- createNumLst(input$signedRankRaw1) - createNumLst(input$signedRankRaw2)
    validate(
      need(!all(differences == 0) && var(differences) != 0, "'Sample 1' and 'Sample 2' data are the same. In the Wilcoxon Signed Rank Test the pairs with a difference of zero are dropped.  The effective sample size is now zero. Please check your data."),
      errorClass = "myClass"
    )
  }
}

if (!signedRankUpload_iv$is_valid()) {
  if (is.null(input$signedRankUpl)) {
    validate("Please upload a file.")
  }

  validate(
    need(!is.null(fileInputs$signedRankStatus) && fileInputs$signedRankStatus == "uploaded", "Please upload a file."),
    errorClass = "myClass"
  )

  validate(
    need(nrow(signedRankUploadData()) > 0, "File is empty."),
    need(ncol(signedRankUploadData()) >= 2, "File must contain at least 2 distinct sample 1 and sample 2 sets of data to choose from for analysis."),
    need(nrow(signedRankUploadData()) >= 3, "Samples must include at least 3 observations."),
    errorClass = "myClass"
  )
}

if (!signedRankUploadvars_iv$is_valid()) {
  validate(
    need(input$signedRankUpl1, "Please select a column for sample 1."),
    need(input$signedRankUpl2, "Please select a column for sample 2."),
    need(CheckSignedRankUploadSamples() == 0, "Same number of data points required for Sample 1 and Sample 2."),
    errorClass = "myClass"
  )

  if (input$signedRankUpl1 != "" && input$signedRankUpl2 != "") {
    data <- signedRankUploadData()
    sample1 <- na.omit(unlist(data[, input$signedRankUpl1]))
    sample2 <- na.omit(unlist(data[, input$signedRankUpl2]))
    min_length <- min(length(sample1), length(sample2))
    if (min_length > 0) {
      differences <- sample1[1:min_length] - sample2[1:min_length]
      validate(
        need(!all(differences == 0) && var(differences) != 0, "'Sample 1' and 'Sample 2' data are the same. In the Wilcoxon Signed Rank Test the pairs with a difference of zero are dropped.  The effective sample size is now zero. Please check your data."),
        errorClass = "myClass"
      )
    }
  }
}

## ---------------- Dependent Population Means Validation
if (!depmeansrawsd_iv$is_valid()) {
  if (input$inferenceType2 == "Hypothesis Testing") {
    sdValidation <- "The test statistic (t) will be undefined for sample data with a sample standard deviation of difference (sd) = 0."
  } else {
    sdValidation <- paste0(
      "The confidence interval results in (",
      GetDepMeansData()$dbar,
      ",", GetDepMeansData()$dbar,
      ") when the sample standard deviation of difference (sd) = 0."
    )
  }
  validate(
    need(GetDepMeansData()$sd != 0, sdValidation),
    errorClass = "myClass"
  )
}
if (!depmeansraw_iv$is_valid()) {
  validate(
    need(input$before, "Sample 1 data requires a minimum of three data points.") %||%
    need(length(createNumLst(input$before)) > 2, "Sample 1 data requires a minimum of three data points."),
    need(input$after, "Sample 2 data requires a minimum of three data points.") %||%
    need(length(createNumLst(input$after)) > 2, "Sample 2 data requires a minimum of three data points."),
    errorClass = "myClass"
  )

  validate(
    need(length(createNumLst(input$before)) == length(createNumLst(input$after)), "Same number of data points required for Sample 1 and Sample 2."),
    errorClass = "myClass"
  )
}

if (!depmeansupload_iv$is_valid()) {
  if (is.null(input$depMeansUserData)) {
    validate("Please upload a file.")
  }

  validate(
    need(!is.null(fileInputs$depMeansStatus) && fileInputs$depMeansStatus == "uploaded", "Please upload a file."),
    errorClass = "myClass"
  )

  validate(
    need(nrow(DepMeansUploadData()) > 0, "File is empty."),
    need(ncol(DepMeansUploadData()) >= 2, "File must contain at least 2 distinct 'Before' and 'After' sets of data to choose from for analysis."),
    need(nrow(DepMeansUploadData()) >= 3, "Samples must include at least 3 observations."),
    errorClass = "myClass"
  )
}

if (!depmeansuploadvars_iv$is_valid()) {
  validate(
    need(input$depMeansUplSample1, "Please select a column for Sample 1 (e.g. Before, Pre-Treatment, Baseline)."),
    need(input$depMeansUplSample2, "Please select a column for Sample 2 (e.g. After, Post-Treatment, Follow-Up)."),
    need(CheckDepUploadSamples() == 0, "Same number of data points required for Sample 1 and Sample 2."),
    errorClass = "myClass"
  )

  validate(
    need(
      !checkNumeric(DepMeansUploadData(), input$depMeansUplSample1),
      "Sample 1 must be numeric."
    ),
    errorClass = "myClass"
  )

  validate(
    need(
      !checkNumeric(DepMeansUploadData(), input$depMeansUplSample2),
      "Sample 2 must be numeric."
    ),
    errorClass = "myClass"
  )

  sample1 <- na.omit(unlist(DepMeansUploadData()[, input$depMeansUplSample1]))
  sample2 <- na.omit(unlist(DepMeansUploadData()[, input$depMeansUplSample2]))
  validate(
    need(length(sample1) > 2, "Sample 1 must have at least 3 observations."),
    need(length(sample2) > 2, "Sample 2 must have at least 3 observations."),
    errorClass = "myClass"
  )

  validate(
    need(
      !(input$depMeansUplSample1 != "" &&
        input$depMeansUplSample2 != "" &&
        (input$depMeansUplSample1 == input$depMeansUplSample2 ||
         GetDepMeansData()$sd == 0)),
      if (input$inferenceType2 == "Hypothesis Testing") {
        "The test statistic (t) will be undefined for sample data with a sample standard deviation of difference (sd) = 0."
      } else {
        sprintf(
          "The confidence interval results in (%s,%s) when the sample standard deviation of difference (sd) = 0.", GetDepMeansData()$dbar,
          GetDepMeansData()$dbar
        )
      }
    ),
    errorClass = "myClass"
  )
}

if (!depmeansraw_iv$is_valid()) {
  if (input$inferenceType2 == "Hypothesis Testing") {
    sdValidation <- "The test statistic (t) will be undefined for sample data with a sample standard deviation of difference (sd) = 0."
  } else {
    sdValidation <- paste0(
      "The confidence interval results in (",
      GetDepMeansData()$dbar,
      ",", GetDepMeansData()$dbar,
      ") when the sample standard deviation of difference (sd) = 0."
    )
  }
  validate(
    need(GetDepMeansData()$sd != 0, sdValidation),
    errorClass = "myClass"
  )
}

if (!depmeansmunaught_iv$is_valid()) {
  validate(
    need(input$depMeansMuNaught, "Hypothesized value of the Population Mean Difference is required."),
    errorClass = "myClass"
  )
}

## ---------------- Two Population Proportion Validation
if (!twopropht_iv$is_valid()) {
  validate(
    need(checkTwoProp() > 0, "The Z test statistic is undefined when the Number of Successes 1 (x1) and Number of Successes 2 (x2) are both 0."),
    need(
      !(input$numSuccesses1 == input$numTrials1 && input$numSuccesses2 == input$numTrials2),
      "The pooled proportion equals 1, which results in an undefined test statistic (z). This happens when the number of successes equals the number of trials for both samples."
    ),
    errorClass = "myClass"
  )
}

if (!twoprop_iv$is_valid()) {
  validate(
    need(input$numSuccesses1, "Numeric value for Number of Successes 1 (x1) required"),
    need(input$numTrials1, "Numeric value for Number of Trials 1 (n1) required"),
    need(input$numSuccesses2, "Numeric value for Number of Successes 2 (x2) required"),
    need(input$numTrials2, "Numeric value for Number of Trials 2 (n2) required"),
    errorClass = "myClass"
  )

  validate(
    need(input$numSuccesses1 %% 1 == 0, "Number of Successes 1 (x1) must be an integer"),
    need(input$numSuccesses1 >= 0, "Number of Successes 1 (x1) cannot be negative"),
    need(input$numTrials1 %% 1 == 0, "Number of Trials 1 (n1) must be an integer"),
    need(input$numTrials1 > 0, "Number of Trials 1 (n1) must be greater than 0"),
    need(input$numSuccesses2 %% 1 == 0, "Number of Successes 2 (x2) must be an integer"),
    need(input$numSuccesses2 >= 0, "Number of Successes 2 (x2) cannot be negative"),
    need(input$numTrials2 %% 1 == 0, "Number of Trials 2 (n2) must be an integer"),
    need(input$numTrials2 > 0, "Number of Trials 2 (n2) must be greater than 0"),
    errorClass = "myClass"
  )
} else if (input$siMethod == "2" && input$popuParameters == "Population Proportions") {
  validate(
    need(input$numSuccesses1 <= input$numTrials1, "Number of Successes 1 (x1) cannot be greater than Number of Trials 1 (n1)"),
    need(input$numSuccesses2 <= input$numTrials2, "Number of Successes 2 (x2) cannot be greater than Number of Trials 2 (n2)"),
    errorClass = "myClass"
  )
}

if (!twopropdiffnaught_iv$is_valid()) {
  validate(
    need(input$propDiffNaught != "", "Hypothesized value of the Population Proportion Difference is required.") %||%
    need(input$propDiffNaught >= -1, "Hypothesized value of the Population Proportion Difference must be between -1 and +1, inclusive.") %||%
    need(input$propDiffNaught <= 1, "Hypothesized value of the Population Proportion Difference must be between -1 and +1, inclusive."),
    errorClass = "myClass"
  )
}

## ---------------- Two Pop Variance Validation

if (!twopopvarsum_iv$is_valid()) {
  validate(
    need(input$SDSampleSize1, "Sample size 1 is required.") %||%
    need(input$SDSampleSize1 %% 1 == 0 && input$SDSampleSize1 > 1, "Sample size 1 must be an integer greater than 1."),
    need(input$SDSampleSize2, "Sample size 2 is required.") %||%
    need(input$SDSampleSize2 %% 1 == 0 && input$SDSampleSize2 > 1, "Sample size 2 must be an integer greater than 1."),
    need(input$stdDev1, "Sample standard deviation 1 is required.") %||%
    need(input$stdDev1 > 0, "Sample standard deviation 1 must be greater than 0."),
    need(input$stdDev2, "Sample standard deviation 2 is required.") %||%
    need(input$stdDev2 > 0, "Sample standard deviation 2 must be greater than 0."),
    errorClass = "myClass"
  )
}

if (!twopopvar_iv$is_valid()) {
  validate(
    need(input$n1, "n1 is required.") %||%
    need(
      input$n1 %% 1 == 0 && input$n1 > 1,
      "n1 must be an integer greater than 1."
    ),
    need(input$s1sq, "s1^2 is required.") %||%
    need(
      input$s1sq > 0,
      "s1^2 must be greater than 0."
    ),
    need(input$n2, "n2 is required.") %||%
    need(
      input$n2 %% 1 == 0 && input$n2 > 1,
      "n2 must be an integer greater than 1."
    ),
    need(input$s2sq, "s2^2 is required.") %||%
    need(
      input$s2sq > 0,
      "s2^2 must be greater than 0."
    ),
    errorClass = "myClass"
  )
}

if (!twopopvarraw_iv$is_valid()) {
  validate(
    need(input$rawSamp1SD, "Group 1 data requires a minimum of 3 numeric values.") %||%
    need(length(createNumLst(input$rawSamp1SD)) >= 3, "Group 1 data requires a minimum of 3 numeric values.") %||%
    need(sd(createNumLst(input$rawSamp1SD)) > 0, "Group 1 must have variance."),
    need(input$rawSamp2SD, "Group 2 data requires a minimum of 3 numeric values.") %||%
    need(length(createNumLst(input$rawSamp2SD)) >= 3, "Group 2 data requires a minimum of 3 numeric values.") %||%
    need(sd(createNumLst(input$rawSamp2SD)) > 0, "Group 2 must have variance."),
    errorClass = "myClass"
  )
}


## ------------- Ind Means Outputs ------------------------------------------
    ## ------------- Uploaded Data Table --------------------------
    output$indPopMeansUploadTable <- renderDT({
      req(indmeansupload_iv$is_valid())
      datatable(IndMeansUploadData(),
                options = list(
                  pageLength = -1,
                  lengthMenu = list(
                    c(25, 50, 100, -1),
                    c("25", "50", "100", "all")
                  ),
                  columnDefs = list(list(
                    className = "dt-center",
                    targets = 0:ncol(IndMeansUploadData())
                  ))
                )
                )
    })

    ## ------------- Q-Q Plots --------------------------

    output$indMeansQQPlot <- renderPlot(
    {
      ## ind means qq plot
      req(input$indMeansQQPlot)

      if (input$dataAvailability2 == "Enter Raw Data") {
        dat1 <- createNumLst(input$raw_sample1)
        dat2 <- createNumLst(input$raw_sample2)
      } else if (input$dataAvailability2 == "Upload Data") {
        req(input$indMeansUplSample1, input$indMeansUplSample2)
        dat1 <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample1]))
        dat2 <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample2]))
      }

      df1 <- tibble(values = dat1)
      df2 <- tibble(values = dat2)

      ## QQ plot for sample 1
      qq1 <- RenderQQPlot(
        dat = df1,
        plotColour = input[["indMeansQQPlot-Colour"]],
        plotTitle = "Sample 1 Q-Q Plot",
        plotXlab = input[["indMeansQQPlot-Xlab"]],
        plotYlab = input[["indMeansQQPlot-Ylab"]],
        gridlines = input[["indMeansQQPlot-Gridlines"]],
        flip = input[["indMeansQQPlot-Flip"]]
      )

      ## QQ plot for sample 2
      qq2 <- RenderQQPlot(
        dat = df2,
        plotColour = input[["indMeansQQPlot-Colour"]],
        plotTitle = "Sample 2 Q-Q Plot",
        plotXlab = input[["indMeansQQPlot-Xlab"]],
        plotYlab = input[["indMeansQQPlot-Ylab"]],
        gridlines = input[["indMeansQQPlot-Gridlines"]],
        flip = input[["indMeansQQPlot-Flip"]]
      )

      ## pairs the graphs side by side
      plot_pair <- ggpubr::ggarrange(qq1, qq2, ncol = 2)

      ## title above the 2 graphs
      ggpubr::annotate_figure(
                plot_pair,
                top = ggpubr::text_grob(
                                input[["indMeansQQPlot-Title"]],
                                face = "bold",
                                size = 24
                              )
              )
    },
    height = function() {
      GetPlotHeight(input[["indMeansQQPlot-Height"]], input[["indMeansQQPlot-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["indMeansQQPlot-Width"]], input[["indMeansQQPlot-WidthPx"]], ui = FALSE)
    }
    )

    ## ---------------- CI ----
    output$indMeansCI <- renderUI({
      if (IndMeansSigmaKnown() == "bothKnown") {
        cInt <- IndMeansZInt()
        sdSymbol <- "\\sigma"
        testStat <- "z"
      } else if (IndMeansSigmaKnown() == "bothUnknown") {
        cInt <- IndMeansTInt()
        sdSymbol <- "s"
        testStat <- "t"
      }

      tagList(
        p(
          withMathJax(
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothKnown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothKnown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothKnown')",
              uiOutput(session$ns("sigmaKnownCIFormula"))
            ),
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown')",
              uiOutput(session$ns("sigmaUnknownCIFormula"))
            ),
            br(),
            sprintf(
              "\\( \\quad = (%g, %g)\\)",
              cInt["LCL"],
              cInt["UCL"]
            ),
            br(),
            br(),
            br(),
            p(tags$b("Interpretation:")),
            sprintf(
              "We are %1.0f%% confident that the difference in population means \\( (\\mu_{1} - \\mu_{2}) \\) is between \\( %g \\) and \\( %g \\).",
              ConfLvl() * 100,
              cInt["LCL"],
              cInt["UCL"]
            ),
            br()
          )
        )
      )
    })

    ## ---------------- Boxplot ----
    output$indMeansBoxplot <- renderPlot(
    {
      if (input$dataAvailability2 == "Enter Raw Data") {
        sample1 <- createNumLst(input$raw_sample1)
        sample2 <- createNumLst(input$raw_sample2)
      } else if (input$dataAvailability2 == "Upload Data") {
        req(input$indMeansUplSample1, input$indMeansUplSample2)
        sample1 <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample1]))
        sample2 <- na.omit(unlist(IndMeansUploadData()[, input$indMeansUplSample2]))
      }

      dat <- c(sample1, sample2)
      df_boxplot <- data.frame(
        sample = c(rep("Sample 1", length(sample1)), rep("Sample 2", length(sample2))),
        data = c(dat)
      )

      RenderSideBySideBoxplot(
        dat,
        df_boxplot,
        input[["indMeansBoxplot-Colour"]],
        input[["indMeansBoxplot-Title"]],
        input[["indMeansBoxplot-Xlab"]],
        input[["indMeansBoxplot-Ylab"]],
        input[["indMeansBoxplot-BoxWidth"]] / 10,
        input[["indMeansBoxplot-Gridlines"]],
        input[["indMeansBoxplot-Flip"]],
        input[["indMeansBoxplot-OutlierLabels"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["indMeansBoxplot-Height"]], input[["indMeansBoxplot-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["indMeansBoxplot-Width"]], input[["indMeansBoxplot-WidthPx"]], ui = FALSE)
    }
    )

    output$sigmaKnownCIFormula <- renderUI({
      if (input$dataAvailability2 == "Summarized Data") {
        data <- IndMeansSummData()
      } else if (input$dataAvailability2 == "Enter Raw Data") {
        data <- IndMeansRawData()
      } else if (input$dataAvailability2 == "Upload Data") {
        data <- GetMeansUploadData()
      }

      zInt <- IndMeansZInt()

      tagList(
        p(
          withMathJax(
            sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( z_{\\alpha/2} \\sqrt{ \\dfrac{\\sigma_{1}^2}{n_{1}} + \\dfrac{\\sigma_{2}^2}{n_{2}} } \\right) \\)"),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle \\quad = (%.4f - %s) \\pm \\left( %.4f \\sqrt{ \\dfrac{%.4f^2}{%.0f} + \\dfrac{%.4f^2}{%.0f} } \\right) \\)",
              data$xbar1,
              if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
              zInt["Z Critical"],
              data$sd1,
              data$n1,
              data$sd2,
              data$n2
            ),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle \\quad = %s \\pm \\left( %g \\cdot %g \\right) \\)",
              zInt["Difference of means"],
              zInt["Z Critical"],
              zInt["Std Error"]
            ),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle \\quad = %s \\pm %g \\)",
              zInt["Difference of means"],
              zInt["ME"]
            )
          )
        )
      )
    })

    output$sigmaUnknownCIFormula <- renderUI({
      if (input$dataAvailability2 == "Summarized Data") {
        data <- IndMeansSummData()
      } else if (input$dataAvailability2 == "Enter Raw Data") {
        data <- IndMeansRawData()
      } else if (input$dataAvailability2 == "Upload Data") {
        data <- GetMeansUploadData()
      }

      tInt <- IndMeansTInt()

      showTable <- showSummaryTable()

      if (data$sigmaEqual) {
        sp <- round(sqrt(((data$n1 - 1) * data$sd1^2 + (data$n2 - 1) * data$sd2^2) / (data$n1 + data$n2 - 2)), 4)

        tagList(
          withMathJax(
            br(),
            if (showTable) {
              list(
                PrintIndMeansSummaryTable(data),
                br(),
                br()
              )
            },
            sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( t_{\\alpha/2, \\, df} \\cdot s_{p} \\sqrt{ \\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}} } \\right) \\)"),
            br(),
            br(),
            p("where"),
            sprintf(
              "\\( \\qquad df = n_{1} + n_{2} - 2 = %g, \\)",
              tInt["df"]
            ),
            sprintf(
              "\\( \\qquad t_{\\alpha/2, \\, df} = t_{%g, \\, %g} = %g \\)",
              (1 - ConfLvl()) / 2,
              tInt["df"],
              tInt["T Critical"]
            ),
            br(),
            p("and"),
            sprintf("\\( \\displaystyle \\qquad s_{p} = \\sqrt{\\dfrac{(n_{1} - 1)s_{1}^2 + (n_{2} - 1)s_{2}^2}{n_{1} + n_{2} - 2}} \\)"),
            sprintf(
              "\\( = \\sqrt{\\dfrac{(%g - 1)%g + (%g - 1)%g}{%g + %g - 2}} = %g \\)",
              data$n1,
              data$sd1^2,
              data$n2,
              data$sd2^2,
              data$n1,
              data$n2,
              sp
            ),
            br(),
            br(),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle CI = (%.4f - %s) \\pm \\left( %.4f \\cdot %.4f \\sqrt{ \\dfrac{1}{%.0f} + \\dfrac{1}{%.0f} } \\right) \\)",
              data$xbar1,
              if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
              tInt["T Critical"],
              sp,
              data$n1,
              data$n2
            ),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle \\quad = %g \\pm \\left( %g \\cdot %g \\right) \\)",
              tInt["Difference of means"],
              tInt["T Critical"],
              tInt["Std Error"]
            ),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle \\quad = %g \\pm %g \\)",
              tInt["Difference of means"],
              tInt["ME"]
            )
          )
        )
      } else {
        tagList(
          withMathJax(
            br(),
            if (showTable) {
              list(
                PrintIndMeansSummaryTable(data),
                br(),
                br()
              )
            },
            sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( t_{\\alpha/2, \\, \\nu} \\cdot \\sqrt{ \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} } \\right) \\)"),
            br(),
            br(),
            p("where"),
            sprintf("\\( \\displaystyle \\qquad \\nu = \\: \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }
                    { \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} \\right)^2 }{n_{1} - 1} + \\dfrac{ \\left( \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }{n_{2} - 1} } \\)"),
            sprintf(
              "\\( \\displaystyle \\: = \\: \\dfrac{ \\left( \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} \\right)^2 }
                    { \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} + \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} } \\)",
              data$sd1,
              data$n1,
              data$sd2,
              data$n2,
              data$sd1,
              data$n1,
              data$n1,
              data$sd2,
              data$n2,
              data$n2
            ),
            sprintf(
              "\\( \\displaystyle \\: = \\: \\dfrac{ \\left( %g + %g \\right)^2 }
                    { \\dfrac{ %g^2 }{%g} + \\dfrac{ %g^2 }{%g} } \\)",
              (data$sd1^2) / data$n1,
              (data$sd2^2) / data$n2,
              (data$sd1^2) / data$n1,
              data$n1 - 1,
              (data$sd2^2) / data$n2,
              data$n2 - 1
            ),
            sprintf(
              "\\( \\: = \\: %g \\)",
              tInt["df"]
            ),
            br(),
            p("and"),
            sprintf(
              "\\( \\qquad t_{\\alpha/2, \\, \\nu} = t_{%g, \\, %g} = %g \\)",
              (1 - ConfLvl()) / 2,
              tInt["df"],
              tInt["T Critical"]
            ),
            br(),
            br(),
            br(),
            br(),
            sprintf(
              "\\( CI = (%.4f - %s) \\pm \\left( %.4f \\cdot \\sqrt{ \\dfrac{%.4f^2}{%.0f} + \\dfrac{%.4f^2}{%.0f} } \\right) \\)",
              data$xbar1,
              if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
              tInt["T Critical"],
              data$sd1,
              data$n1,
              data$sd2,
              data$n2
            ),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle \\quad = %g \\pm \\left( %g \\cdot %g \\right) \\)",
              tInt["Difference of means"],
              tInt["T Critical"],
              tInt["Std Error"]
            ),
            br(),
            br(),
            sprintf(
              "\\( \\displaystyle \\quad = %g \\pm %g \\)",
              tInt["Difference of means"],
              tInt["ME"]
            )
          )
        )
      }
    })

    ## ----------------- HT ----
    output$indMeansHT <- renderUI({
      withMathJax()

      intrpInfo <- IndMeansHypInfo()
      muNaught <- input$indMeansMuNaught

      if (input$dataAvailability2 == "Summarized Data") {
        data <- IndMeansSummData()
      } else if (input$dataAvailability2 == "Enter Raw Data") {
        data <- IndMeansRawData()
      } else if (input$dataAvailability2 == "Upload Data") {
        data <- GetMeansUploadData()
      }

      ## get test type and results based on sigma known/unknown
      if (IndMeansSigmaKnown() == "bothKnown") {
        hTest <- IndMeansZTest()
        testStat <- "z"
        critValDF <- paste(intrpInfo$critSign, "z_{", intrpInfo$critAlph, "} = ", intrpInfo$critSign, "z_{", intrpInfo$alphaVal, "}")
      } else if (IndMeansSigmaKnown() == "bothUnknown") {
        hTest <- IndMeansTTest()
        testStat <- "t"

        if (data$sigmaEqual) {
          critValDF <- paste(intrpInfo$critSign, "t_{", intrpInfo$critAlph, ", \\, n_{1} + n_{2} - 2} = ", intrpInfo$critSign, "t_{", intrpInfo$alphaVal, ", \\, ", hTest["df"], "}")
        } else {
          critValDF <- paste(intrpInfo$critSign, "t_{", intrpInfo$critAlph, ", \\, \\nu} = ", "\n", intrpInfo$critSign, "t_{", intrpInfo$alphaVal, ", \\, ", hTest["df"], "}")
        }
      }

      if (hTest["P-Value"] > SigLvl()) {
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

      if (intrpInfo$alternative == "two.sided") {
        critVal <- paste("\\pm", hTest[2])
      } else {
        critVal <- hTest[2]
      }
      showTable <- showSummaryTable()
      indHTHead <- tagList(
        p(
          withMathJax(
            if (!muNaught) {
              list(
                sprintf(
                  "\\( H_{0}: \\mu_{1} %s \\mu_{2}\\)",
                  intrpInfo$nullHyp
                ),
                br(),
                sprintf(
                  "\\( H_{a}: \\mu_{1} %s \\mu_{2}\\)",
                  intrpInfo$altHyp
                )
              )
            } else {
              list(
                sprintf(
                  "\\( H_{0}: (\\mu_{1} - \\mu_{2}) %s %s\\)",
                  intrpInfo$nullHyp, muNaught
                ),
                br(),
                sprintf(
                  "\\( H_{a}: (\\mu_{1} - \\mu_{2}) %s %s\\)",
                  intrpInfo$altHyp, muNaught
                )
              )
            },
            br(),
            br(),
            sprintf(
              "\\( \\alpha = %s \\)",
              SigLvl()
            ),
            br(),
            br(),
            if (showTable) {
              list(
                PrintIndMeansSummaryTable(data),
                br(),
                br()
              )
            },
            p(tags$b("Test Statistic:")),
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothKnown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothKnown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothKnown')",
              uiOutput(session$ns("sigmaKnownHTFormula"))
            ),
            conditionalPanel(
              ns = session$ns,
              condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown')
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown')",
              uiOutput(session$ns("sigmaUnknownHTFormula"))
            ),
            br(),
            br(),
            br()
          )
        )
      )

      indHTPVal <- printHTPVal(
        hTest["P-Value"],
        testStat,
        intrpInfo$alternative,
        hTest["Test Statistic"],
        pvalSymbol,
        reject
      )


      indHTTail <- tagList(
        withMathJax(
          p(tags$b("Using Critical Value Method:")),
          sprintf(
            "Critical Value(s) \\( = %s = %s\\)",
            critValDF,
            critVal
          ),
          br(),
          conditionalPanel(
            ns = session$ns,
            condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown' && input.bothsigmaEqual == 'FALSE') ||
                       (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown' && input.bothsigmaEqualRaw == 'FALSE') ||
                       (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown' && input.bothsigmaEqualUpload == 'FALSE')
                       ",
            br(),
            p("where"),
            sprintf("\\( \\displaystyle \\qquad \\nu = \\: \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }
                  { \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} \\right)^2 }{n_{1} - 1} + \\dfrac{ \\left( \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }{n_{2} - 1} } \\)"),
            sprintf(
              "\\( \\displaystyle \\: = \\: \\dfrac{ \\left( \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} \\right)^2 }
                  { \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} + \\dfrac{ \\left( \\dfrac{%g^2}{%g} \\right)^2 }{%g - 1} }\\)",
              data$sd1,
              data$n1,
              data$sd2,
              data$n2,
              data$sd1,
              data$n1,
              data$n1,
              data$sd2,
              data$n2,
              data$n2
            ),
            sprintf(
              "\\( \\displaystyle \\: = \\: \\dfrac{ \\left( %0.4f + %0.4f \\right)^2 }
                  { \\dfrac{ %0.4f^2 }{%g} + \\dfrac{ %0.4f^2 }{%g} } = %s\\)",
              (data$sd1^2) / data$n1,
              (data$sd2^2) / data$n2,
              (data$sd1^2) / data$n1,
              data$n1 - 1,
              (data$sd2^2) / data$n2,
              data$n2 - 1,
              hTest["df"]
            ),
            br(),
            br()
          ),
          br(),
          sprintf(
            "Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
            testStat,
            region,
            reject
          ),
          br(),
          br()
        ),
        plotOutput(session$ns("indMeansHTPlot"), width = "75%", height = "300px"),
        br()
      )

      if (!muNaught) {
        altHypExpr <- paste0("\\mu_{1} ", intrpInfo$altHyp)
        altHypValue <- "\\mu_{2}"
      } else {
        altHypExpr <- paste0("(\\mu_{1} - \\mu_{2}) ", intrpInfo$altHyp)
        altHypValue <- muNaught
      }

      indHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHypExpr, altHypValue)

      tagAppendChildren(indHTHead, indHTPVal, indHTTail, indHTConclusion)
    })

    output$sigmaKnownHTFormula <- renderUI({
      if (input$dataAvailability2 == "Summarized Data") {
        data <- IndMeansSummData()
      } else if (input$dataAvailability2 == "Enter Raw Data") {
        data <- IndMeansRawData()
      } else if (input$dataAvailability2 == "Upload Data") {
        data <- GetMeansUploadData()
      }

      zTest <- IndMeansZTest()
      muNaught <- input$indMeansMuNaught

      tagList(
        withMathJax(
          sprintf("\\( z = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ \\sqrt{ \\dfrac{\\sigma_{1}^2}{n_{1}} + \\dfrac{\\sigma_{2}^2}{n_{2}} } } \\)"),
          br(),
          br(),
          sprintf(
            "\\( \\phantom{z} = \\dfrac{ (%g - %s) -%s}{ \\sqrt{ \\dfrac{%g^2}{%.0f} + \\dfrac{%g^2}{%.0f} } } = \\dfrac{%g}{%g} = %g \\)",
            data$xbar1,
            if (data$xbar2 < 0) sprintf("(%g)", data$xbar2) else sprintf("%g", data$xbar2),
            if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
            data$sd1,
            data$n1,
            data$sd2,
            data$n2,
            zTest["Difference of means"],
            zTest["Std Error"],
            zTest["Test Statistic"]
          ),
          br()
        )
      )
    })

    output$sigmaUnknownHTFormula <- renderUI({
      if (input$dataAvailability2 == "Summarized Data") {
        data <- IndMeansSummData()
      } else if (input$dataAvailability2 == "Enter Raw Data") {
        data <- IndMeansRawData()
      } else if (input$dataAvailability2 == "Upload Data") {
        data <- GetMeansUploadData()
      }

      sd1Sqrd <- data$sd1^2
      if (sd1Sqrd >= 0.0001) {
        sd1Sqrd <- round(sd1Sqrd, 4)
      } else {
        sd1Sqrd <- signif(sd1Sqrd, 1)
      }

      sd2Sqrd <- data$sd2^2
      if (sd2Sqrd >= 0.0001) {
        sd2Sqrd <- round(sd2Sqrd, 4)
      } else {
        sd2Sqrd <- signif(sd2Sqrd, 1)
      }

      muNaught <- input$indMeansMuNaught
      tTest <- IndMeansTTest()

      if (data$sigmaEqual == TRUE) {
        sp <- sqrt(((data$n1 - 1) * data$sd1^2 + (data$n2 - 1) * data$sd2^2) / (data$n1 + data$n2 - 2))
        sp <- if (sp < 0.0001 && sp > -1e-2) {
                signif(sp, 1)
              } else {
                sp
              }

        tagList(
          withMathJax(
            sprintf("\\( t = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ s_{p} \\sqrt{ \\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}} } } \\)"),
            br(),
            br(),
            p("where"),
            sprintf("\\( \\displaystyle \\qquad s_{p} = \\sqrt{\\dfrac{(n_{1} - 1)s_{1}^2 + (n_{2} - 1)s_{2}^2}{n_{1} + n_{2} - 2}} \\)"),
            sprintf(
              "\\( = \\sqrt{\\dfrac{(%g - 1)%s + (%g - 1)%s}{%g + %g - 2}} = %g \\)",
              data$n1,
              round(sd1Sqrd, 4),
              data$n2,
              round(sd2Sqrd, 4),
              data$n1,
              data$n2,
              round(sp, 4)
            ),
            br(),
            br(),
            br(),
            br(),
            sprintf(
              "\\( \\phantom{t} = \\dfrac{ (%s - %s) - %s }{ %g \\sqrt{ \\dfrac{1}{%.0f} + \\dfrac{1}{%.0f} } } \\)",
              round(data$xbar1, 4),
              if (data$xbar2 < 0) sprintf("(%s)", round(data$xbar2, 4)) else sprintf("%s", round(data$xbar2, 4)),
              if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
              round(sp, 4),
              data$n1,
              data$n2
            ),
            sprintf(
              "\\( = \\dfrac{%g}{%g} = %g \\)",
              tTest["Difference of means"],
              tTest["Std Error"],
              tTest["Test Statistic"]
            ),
            br()
          )
        )
      } else {
        tagList(
          withMathJax(
            sprintf("\\( t = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ \\sqrt{ \\dfrac{s_{1}^2}{n_{1}} + \\dfrac{s_{2}^2}{n_{2}} } } \\)"),
            br(),
            br(),
            sprintf(
              "\\( \\phantom{t} = \\dfrac{ (%g - %s) - %s }{ \\sqrt{ \\dfrac{%.4f^2}{%.0f} + \\dfrac{%.4f^2}{%.0f} } } = \\dfrac{%s}{%s} = %.4f \\)",
              data$xbar1,
              if (data$xbar2 < 0) sprintf("(%.4f)", data$xbar2) else sprintf("%.4f", data$xbar2),
              if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
              data$sd1,
              data$n1,
              data$sd2,
              data$n2,
              tTest["Difference of means"],
              tTest["Std Error"],
              tTest["Test Statistic"]
            ),
            br()
          )
        )
      }
    })

    ## ---------------- HT Plot ----
    output$indMeansHTPlot <- renderPlot({
      if (IndMeansSigmaKnown() == "bothKnown") {
        data <- IndMeansZTest()
      } else if (IndMeansSigmaKnown() == "bothUnknown") {
        data <- IndMeansTTest()
      }

      intrpInfo <- IndMeansHypInfo()
      htPlotCritVal <- data[2]

      if (IndMeansSigmaKnown() == "bothKnown") {
        indMeansPlot <- hypZTestPlot(data["Test Statistic"], htPlotCritVal, intrpInfo$alternative)
      } else if (IndMeansSigmaKnown() == "bothUnknown") {
        indMeansPlot <- hypTTestPlot(data["Test Statistic"], data["df"], htPlotCritVal, intrpInfo$alternative)
      }

      indMeansPlot
    })


observeEvent(input$goInference, {
  output$renderWRankSumMeansData <- renderUI({
    tagList(
      div(DTOutput(session$ns("wRankSumUploadTable")), style = "width: 75%")
    )
  })
})


## ------------ Wilcoxon Rank Sum Outputs -------------------------------------------
output$wRankSumUploadTable <- renderDT({
  req(wilcoxonUpload_iv$is_valid())
  datatable(WilcoxonUploadData(),
            options = list(
              pageLength = -1,
              lengthMenu = list(
                c(25, 50, 100, -1),
                c("25", "50", "100", "all")
              ),
              columnDefs = list(list(
                className = "dt-center",
                targets = 0:ncol(WilcoxonUploadData())
              ))
            )
            )
})


## ---------------- Data Table ----
    output$wRankSumMeansData <- renderDT({
      rankSumData <- GetwRankSumMeansData()

      df_rankSumData <- data.frame(rankSumData$before, rankSumData$after, rankSumData$d, rankSumData$d^2)
      names(df_rankSumData) <- c("Sample 1", "Sample 2", "<em>d</em> = (Sample 1 - Sample 2)", "<em>d</em><sup>2</sup>")
      df_rankSumData <- bind_rows(df_rankSumData, summarise(df_rankSumData, across(where(is.numeric), sum)))
      rownames(df_rankSumData)[nrow(df_rankSumData)] <- "Totals"

      datatable(round(df_rankSumData, digits = 4),
                options = list(
                  dom = "lftp",
                  pageLength = -1,
                  lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")),
                  ordering = FALSE
                ),
                escape = FALSE
                ) %>% formatStyle(
                        names(df_rankSumData),
                        target = "row",
                        fontWeight = styleRow(dim(df_rankSumData)[1], "bold")
                      )
    })

    ## ---------------- HT ----
    calculate_tie_correction <- function(combined_values) {
      tie_counts <- table(combined_values)

      tie_correction <- 0
      for (t_j in tie_counts) {
        if (t_j > 1) {
          tie_correction <- tie_correction + (t_j^3 - t_j)
        }
      }
      return(tie_correction)
    }

    safe_wilcox_test <- function(...) {
      tryCatch(
        wilcox.test(...),
        error = function(e) NULL
      )
    }

    output$wilcoxonRankSum <- renderUI({
      req(!is.null(wilcoxonRankedData()))
      req(nrow(wilcoxonRankedData()) > 0)

      if (input$wilcoxonRankSumTestData == "Upload Data") {
        req(input$wilcoxonUpl1, input$wilcoxonUpl2)
        name1 <- input$wilcoxonUpl1
        name2 <- input$wilcoxonUpl2
      } else {
        name1 <- "Sample 1"
        name2 <- "Sample 2"
      }
      data_ranked <- wilcoxonRankedData()
      n1 <- nrow(wilcoxonRankedData() %>% dplyr::filter(Group == name1))
      n2 <- nrow(wilcoxonRankedData() %>% dplyr::filter(Group == name2))
      nAll <- nrow(wilcoxonRankedData())
      mu_w <- (sum(wilcoxonRankedData()$Group == name1) * (nrow(wilcoxonRankedData()) + 1)) / 2

      sigma_w <- sqrt((sum(wilcoxonRankedData()$Group == name1) * sum(wilcoxonRankedData()$Group == name2) * (nrow(wilcoxonRankedData()) + 1)) / 12)
      observed_W <- sum(wilcoxonRankedData() %>% dplyr::filter(Group == name1) %>% dplyr::pull(Rank))
      observed_W2 <- sum(wilcoxonRankedData() %>% dplyr::filter(Group == name2) %>% dplyr::pull(Rank))
      significance <- 1 - SigLvl()

      group1_data <- data_ranked %>%
        dplyr::filter(Group == name1) %>%
        dplyr::pull(Value)

      group2_data <- data_ranked %>%
        dplyr::filter(Group == name2) %>%
        dplyr::pull(Value)
      combined_values <- c(group1_data, group2_data)
      has_ties <- length(unique(combined_values)) < length(combined_values)

      ## ---------------- Guards 1 & 2: no rank variation ----------------
      validate(
        need(
          length(unique(combined_values)) > 1,
          "'Sample 1' and 'Sample 2' contain only one repeated value and are identical. The Wilcoxon rank sum test cannot be computed because there is no rank variation. Please check your data."
        ),
        errorClass = "myClass"
      )

      ## ---------------- Guard 3: tie-corrected SE is zero under Normal Approximation ----------------
      if (!is.null(input$normaprowrsRankSum) && input$normaprowrsRankSum == "Normal approximation (for large samples)") {
        tie_correction_check <- calculate_tie_correction(combined_values)
        u_std_dev_check <- sqrt((n1 * n2 / 12) * ((nAll + 1) - (tie_correction_check / (nAll * (nAll - 1)))))

        validate(
          need(
            !is.na(u_std_dev_check) && u_std_dev_check > 0,
            "Under the Normal Approximation method, the tie-corrected standard error of the rank sum is zero. The Z test statistic is undefined. This typically occurs when the combined sample has insufficient rank variation due to heavy ties. Consider using the Exact method instead, or check your data."
          ),
          errorClass = "myClass"
        )
      }

      u1_statistic <- observed_W - (n1 * (n1 + 1) / 2)
      u2_statistic <- observed_W2 - (n2 * (n2 + 1) / 2)
      u_mean <- (n1 * n2) / 2

      if (input$altHypothesis2 == "2") {
        z_critical <- qnorm(1 - SigLvl() / 2)
        critVal <- paste("\\pm", round(qnorm(1 - SigLvl() / 2), 3))
        nullHyp <- paste0("\\text{Median}_{\\text{", name1, "}} = \\text{Median}_{\\text{", name2, "}}")
        altHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\neq \\text{Median}_{\\text{", name2, "}}")
        altern <- "two.sided"
        u_test <- u1_statistic
        correction_factor <- 0
        if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrsRankSum) &&
            input$continuityCorrectionOption == "True" && input$normaprowrsRankSum == "Normal approximation (for large samples)") {
          if (observed_W > mu_w) {
            correction_factor <- -0.5 ## Subtract 0.5 if observed_W is in the upper tail
          } else if (observed_W < mu_w) {
            correction_factor <- 0.5 ## Add 0.5 if observed_W is in the lower tail
          }
        }
        z_stat <- ((observed_W - mu_w + correction_factor) / sigma_w)
        in_rejection_region <- abs(z_stat) > z_critical
      } else if (input$altHypothesis2 == "1") {
        z_critical <- qnorm(SigLvl())
        critVal <- round(qnorm(SigLvl()), 3)
        nullHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\geq \\text{Median}_{\\text{", name2, "}}")
        altHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\lt \\text{Median}_{\\text{", name2, "}}")
        altern <- "less"
        u_test <- u1_statistic
        correction_factor <- 0
        if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrsRankSum) &&
            input$continuityCorrectionOption == "True" && input$normaprowrsRankSum == "Normal approximation (for large samples)") {
          correction_factor <- 0.5
        }
        z_stat <- ((observed_W - mu_w + correction_factor) / sigma_w)
        in_rejection_region <- z_stat < z_critical
      } else {
        z_critical <- qnorm(1 - SigLvl())
        critVal <- round(qnorm(1 - SigLvl()), 3)
        nullHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\leq \\text{Median}_{\\text{", name2, "}}")
        altHyp <- paste0("\\text{Median}_{\\text{", name1, "}} \\gt \\text{Median}_{\\text{", name2, "}}")
        altern <- "greater"
        u_test <- u1_statistic
        correction_factor <- 0

        if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrsRankSum) &&
            input$continuityCorrectionOption == "True" && input$normaprowrsRankSum == "Normal approximation (for large samples)") {
          correction_factor <- -0.5
        }
        z_stat <- ((observed_W - mu_w + correction_factor) / sigma_w)
        in_rejection_region <- z_stat > z_critical
      }

      if (isTRUE(in_rejection_region)) {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      } else {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      }

                                        # no ties in data for p value
      if (isTRUE(has_ties)) {
        if (input$altHypothesis2 == "2") {
          p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
        } else if (input$altHypothesis2 == "1") {
          p_value <- pnorm(z_stat, lower.tail = TRUE)
        } else {
          p_value <- pnorm(z_stat, lower.tail = FALSE)
        }
      } else if (!has_ties) {
        test_result <- suppressWarnings(
          safe_wilcox_test(group1_data, group2_data,
                           paired = FALSE, alternative = altern,
                           conf.level = significance, exact = TRUE, conf.int = TRUE
                           )
        )
        validate(
          need(!is.null(test_result), "The Wilcoxon rank sum test could not be computed for this data. Please check your data for sufficient variation in both samples."),
          errorClass = "myClass"
        )
        p_value <- test_result$p.value
      }

      ## Confidence Interval for Two sided, Left and Right sided
      if (isTRUE(input$normaprowrsRankSum == "Exact")) {
        if (input$altHypothesis2 == "2") {
          lower <- qwilcox(SigLvl() / 2, m = n1, n = n2, lower.tail = TRUE)
          upper <- qwilcox(SigLvl() / 2, m = n1, n = n2, lower.tail = FALSE)
        } else if (input$altHypothesis2 == "1") { ## alternative = Less than
          lower <- qwilcox(SigLvl(), m = n1, n = n2, lower.tail = TRUE)
          upper <- Inf
        } else { ## alternative = greater than
          lower <- -Inf
          upper <- qwilcox(SigLvl(), m = n1, n = n2, lower.tail = FALSE)
        }
      }

      if (isTRUE(!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrsRankSum) &&
                 input$continuityCorrectionOption == "True" && input$normaprowrsRankSum == "Normal approximation (for large samples)")) {
        if (isTRUE(has_ties)) {
          test_result <- suppressWarnings(
            safe_wilcox_test(group1_data, group2_data,
                             paired = FALSE, alternative = altern,
                             conf.level = significance, exact = TRUE, correct = TRUE
                             )
          )
          validate(
            need(!is.null(test_result), "The Wilcoxon rank sum test could not be computed for this data. Please check your data for sufficient variation in both samples."),
            errorClass = "myClass"
          )
          p_value <- test_result$p.value
        } else {
          test_result <- suppressWarnings(
            safe_wilcox_test(group1_data, group2_data,
                             paired = FALSE, alternative = altern,
                             conf.level = significance, exact = FALSE, correct = TRUE
                             )
          )
          validate(
            need(!is.null(test_result), "The Wilcoxon rank sum test could not be computed for this data. Please check your data for sufficient variation in both samples."),
            errorClass = "myClass"
          )
          p_value <- test_result$p.value
        }
      } else if (isTRUE(!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrsRankSum) &&
                        input$continuityCorrectionOption == "False" && input$normaprowrsRankSum == "Normal approximation (for large samples)")) {
        if (isTRUE(has_ties)) {
          test_result <- suppressWarnings(
            safe_wilcox_test(group1_data, group2_data,
                             paired = FALSE, alternative = altern,
                             conf.level = significance, exact = TRUE, correct = FALSE
                             )
          )
          validate(
            need(!is.null(test_result), "The Wilcoxon rank sum test could not be computed for this data. Please check your data for sufficient variation in both samples."),
            errorClass = "myClass"
          )
          p_value <- test_result$p.value
        } else {
          test_result <- suppressWarnings(
            safe_wilcox_test(group1_data, group2_data,
                             paired = FALSE, alternative = altern,
                             conf.level = significance, exact = FALSE, correct = FALSE
                             )
          )
          validate(
            need(!is.null(test_result), "The Wilcoxon rank sum test could not be computed for this data. Please check your data for sufficient variation in both samples."),
            errorClass = "myClass"
          )
          p_value <- test_result$p.value
        }
      }

      tie_correction <- calculate_tie_correction(combined_values)
      u_std_dev <- sqrt((n1 * n2 / 12) * ((nAll + 1) - (tie_correction / (nAll * (nAll - 1)))))
      mw_z_stat <- ((u_test - u_mean) / u_std_dev)

      if (isTRUE(input$normaprowrsRankSum == "Exact")) {
        z_stat <- mw_z_stat
      }
      rankSumHTHead <- tagList(
        p(
          withMathJax(),
          sprintf("\\( H_{0}:\\ %s\\)", nullHyp),
          br(),
          sprintf("\\( H_{a}:\\ %s\\)", altHyp),
          br(), br(),
          sprintf("\\( \\alpha = %s \\)", SigLvl()),
          br(), br(),
          sprintf("\\(n_{1} = %s\\)", n1),
          br(),
          sprintf("\\(n_{2} = %s\\)", n2),
          br(),
          sprintf("\\( n = n_{1} + n_{2} = %s \\)", nrow(wilcoxonRankedData())),
          br(),
          p(tags$b("Sum of Ranks:")),
          sprintf("\\(  W_{1} = %s \\)", observed_W),
          br(),
          sprintf("\\(  W_{2} = %s \\)", observed_W2),
          br(), br(),
          if (input$normaprowrsRankSum == "Exact") {
            tagList(
              p(tags$b("Mann-Whitney ", tags$i("U"), " Statistic:")),
              sprintf("\\(  U_{1} = W_{1} - \\frac{n_{1}(n_{1} + 1)}{2} = %s - \\frac{%s (%s + 1)}{2} = %s \\)", observed_W, n1, n1, u1_statistic),
              br(), br(),
              sprintf("\\( U_{2} = W_{2} - \\frac{n_{2}(n_{2} + 1)}{2} = %s - \\frac{%s (%s + 1)}{2} = %s \\)", observed_W2, n2, n2, u2_statistic),
              helpText("*Note: By default, U1 is always chosen as a test statistic."),
              br(),

              ## p(tags$b("Mann-Whitney U Expected Mean:")),
              ## sprintf("\\( \\qquad \\mu_{U} = \\frac{n_{1}n_{2}}{2} = \\frac{%s(%s)}{2} = %s \\)", n1, n2, u_mean),
              ## br(),br(),
                                        #
              ## p(tags$b("Mann-Whitney U Standard Deviation:")),
              ## sprintf("\\( \\qquad \\sigma_U = \\sqrt{\\frac{n_1 n_2}{12}\\left( (n+1) - \\frac{\\sum_{j=1}^{g} (t_j^3 - t_j)}{n(n-1)}\\right)} =
              ## \\sqrt{\\frac{%s \\times %s}{12}\\left( (%s+1) - \\frac{%s}{%s \\times (%s-1)}\\right)} = %s \\)",
              ## n1, n2, nAll, ifelse(has_ties, tie_correction, 0), nAll, nAll, round(u_std_dev, 4)),
              ## br(), br(),
                                        #
              ## p(tags$b("Mann-Whitney U Test Statistic:")),
              ## sprintf("\\( \\qquad z = \\frac{U - \\mu_{U}}{\\sigma_{U}} = \\frac{%s - %s}{%s} = %s \\)",
              ## round(u_test, 4), round(u_mean, 4), round(u_std_dev, 4), round(mw_z_stat, 3)),
              )
          } else {
            if (input$normaprowrsRankSum == "Normal approximation (for large samples)") {
              tagList(
                p(tags$b("Mean:")),
                sprintf(
                  "\\(  \\mu_{W} = \\frac{n_{1}(n + 1)}{2} = \\frac{%s(%s + 1)}{2} = %s \\)",
                  n1, nAll, (sum(wilcoxonRankedData()$Group == name1) * (nrow(wilcoxonRankedData()) + 1)) / 2
                ),
                br(), br(),
                p(tags$b("Standard Deviation:")),
                sprintf(
                  "\\( \\sigma_W = \\sqrt{\\frac{n_{1}n_{2}(n + 1)}{12}} = \\sqrt{\\frac{%s \\times %s (%s + 1)}{12}} = %s \\)",
                  n1, n2, nAll, round(sigma_w, 4)
                ),
                br(), br(),
                p(tags$b("Test Statistic:")),
                if (!is.null(input$continuityCorrectionOption) && !is.null(input$normaprowrsRankSum) &&
                    input$continuityCorrectionOption == "True" && input$normaprowrsRankSum == "Normal approximation (for large samples)") {
                  if (input$altHypothesis2 == "1") { ## Less than alternative
                    sprintf(
                      "\\( z = \\frac{W - \\mu_W + 0.5}{\\sigma_W} = \\frac{%s - %s + %s}{%s} = %s \\)",
                      round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3)
                    )
                  } else if (input$altHypothesis2 == "2") { ## Two-sided alternative
                    if (observed_W > mu_w) {
                      sprintf(
                        "\\( z = \\frac{W - \\mu_W - 0.5}{\\sigma_W} = \\frac{%s - %s - %s}{%s} = %s \\)",
                        round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3)
                      )
                    } else if (observed_W < mu_w) {
                      sprintf(
                        "\\( z = \\frac{W - \\mu_W + 0.5}{\\sigma_W} = \\frac{%s - %s + %s}{%s} = %s \\)",
                        round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3)
                      )
                    } else { ## observed_W == mu_w, no continuity correction applied in formula
                      sprintf(
                        "\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                        round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3)
                      )
                    }
                  } else { ## Greater than alternative (assuming input$altHypothesis2 corresponds to "greater")
                    sprintf(
                      "\\( z = \\frac{W - \\mu_W - 0.5}{\\sigma_W} = \\frac{%s - %s - %s}{%s} = %s \\)",
                      round(observed_W, 4), round(mu_w, 4), abs(correction_factor), round(sigma_w, 4), round(z_stat, 3)
                    )
                  }
                } else { ## No continuity correction
                  if (input$altHypothesis2 == "1") { ## Less than alternative
                    sprintf(
                      "\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                      round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3)
                    )
                  } else if (input$altHypothesis2 == "2") { ## Two-sided alternative
                    sprintf(
                      "\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                      round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3)
                    )
                  } else { ## Greater than alternative
                    sprintf(
                      "\\( z = \\frac{W - \\mu_W}{\\sigma_W} = \\frac{%s - %s}{%s} = %s \\)",
                      round(observed_W, 4), round(mu_w, 4), round(sigma_w, 4), round(z_stat, 3)
                    )
                  }
                },
                br(), br()
              )
            } # ,
          },
          p(tags$b("Using P-value Method:")),
          sprintf("\\( P = %s \\)", ifelse(is.na(p_value), "NA", round(p_value, 4))),
          if (has_ties && input$normaprowrsRankSum == "Exact") {
            helpText("*Note: Exact p-values cannot be computed in the presence of ties. Normal approximation was used.")
          },
          br(),
          if (p_value <= SigLvl()) {
            tagList(
              sprintf("\\( \\text{Since } P \\leq %s, \\text{reject } H_0. \\)", SigLvl()),
              br(), br()
            )
          } else {
            tagList(
              sprintf("\\( \\text{Since } P > %s, \\text{do not reject } H_0. \\)", SigLvl()),
              br(), br()
            )
          },
          if (input$normaprowrsRankSum == "Exact") {
            tagList(
              p(tags$b("Using Critical Value Method:")),
              if (input$altHypothesis2 == "2") { ## Two-sided
                sprintf(
                  "\\( \\text{Rejection Region: reject }H_0 \\text{ whenever } U \\leq %s \\text{ or } U \\geq %s \\)",
                  round(lower, 3), round(upper, 3)
                )
              } else if (input$altHypothesis2 == "1") { ## Left-sided
                sprintf("\\(  \\text{Rejection Region: reject }H_0 \\text{ whenever } U \\leq %s \\)", round(lower, 3))
              } else { ## Right-sided
                sprintf("\\(  \\text{Rejection Region: reject }H_0 \\text{ whenever } U \\geq %s \\)", round(upper, 3))
              },
              br(),
              sprintf("\\( \\text{Observed Test Statistic: } U = %s \\)", round(u1_statistic, 3)),
              br(),
              br(),
              if (input$altHypothesis2 == "2") { ## Two-sided decision
                if (u1_statistic <= lower || u1_statistic >= upper) {
                  sprintf(
                    "\\( \\text{Since the test statistic } U_{1} \\text{ falls in the rejection region } (U \\leq %s \\text{ or } U \\geq %s), \\text{reject } H_0. \\)",
                    round(lower, 3), round(upper, 3)
                  )
                } else {
                  sprintf(
                    "\\( \\text{Since the test statistic } U_{1} \\text{ does not fall in the rejection region } (%s < U < %s), \\text{do not reject } H_0. \\)",
                    round(lower, 3), round(upper, 3)
                  )
                }
              } else if (input$altHypothesis2 == "1") { ## Left-sided decision
                if (u1_statistic <= lower) {
                  sprintf(
                    "\\( \\text{Since the test statistic } U_{1} \\text{ falls in the rejection region } (U \\leq %s), \\text{reject } H_0. \\)",
                    round(lower, 3)
                  )
                } else {
                  sprintf(
                    "\\( \\text{Since the test statistic } U_{1} \\text{ does not fall in the rejection region } (U > %s), \\text{do not reject } H_0. \\)",
                    round(lower, 3)
                  )
                }
              } else { ## Right-sided decision
                if (u1_statistic >= upper) {
                  sprintf(
                    "\\( \\text{Since the test statistic } U_{1} \\text{ falls in the rejection region } (U \\geq %s), \\text{reject } H_0. \\)",
                    round(upper, 3)
                  )
                } else {
                  sprintf(
                    "\\( \\text{Since the test statistic } U_{1} \\text{ does not fall in the rejection region } (U < %s), \\text{do not reject } H_0. \\)",
                    round(upper, 3)
                  )
                }
              }
            )
          }
        )
      )

      rankSumHTTail <-
        if (input$normaprowrsRankSum == "Normal approximation (for large samples)") {
          tagList(
            p(
              withMathJax(),
              p(tags$b("Using Critical Value Method:")),
              sprintf(
                "Critical Value(s) \\( = %s z_{%s} = %s \\)",
                if (input$altHypothesis2 == "2") "\\pm" else if (input$altHypothesis2 == "1") "-" else "",
                if (input$altHypothesis2 == "2") SigLvl() / 2 else SigLvl(),
                critVal
              ),
              br(),
              br(),
              sprintf(
                "Since the test statistic \\( (z = %s)\\) falls within the %s region, %s \\( H_{0}\\).",
                round(z_stat, 3),
                region,
                reject
              ),
              br()
            ),
            plotOutput(session$ns("wilcoxonRankSumPlot"), width = "75%", height = "300px"),
            br()
          )
        }

      depHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, "")

      tagAppendChildren(rankSumHTHead, rankSumHTTail, depHTConclusion)
    })

    ## ---------------- HT Plot ----
    calculate_z_stat <- function(input, observed_W, mu_w, sigma_w, observed_W2, n1, n2, N, u1_statistic, u2_statistic, u_mean, u_std_dev,
                                 has_ties, tie_correction) {
      z_stat_val <- NA
      correction_factor <- 0

      u_test_val <- u1_statistic

      if (isTRUE(input$normaprowrsRankSum == "Exact")) {
        z_stat_val <- ((u_test_val - u_mean) / u_std_dev)
      } else {
        if (!is.null(input$continuityCorrectionOption) && input$continuityCorrectionOption == "True") {
          if (input$altHypothesis2 == "2") {
            if (observed_W > mu_w) {
              correction_factor <- -0.5
            } else if (observed_W < mu_w) {
              correction_factor <- 0.5
            }
          } else if (input$altHypothesis2 == "1") { ## Less than test
            correction_factor <- 0.5
          } else {
            correction_factor <- -0.5
          }
        }
        z_stat_val <- ((observed_W - mu_w + correction_factor) / sigma_w)
      }
      return(z_stat_val)
    }

    output$wilcoxonRankSumPlot <- renderPlot({
      req(wilcoxonRankedData())

      wilcoxonData <- wilcoxonRankedData()

      if (input$wilcoxonRankSumTestData == "Upload Data") {
        name1 <- input$wilcoxonUpl1
        name2 <- input$wilcoxonUpl2
      } else {
        name1 <- "Sample 1"
        name2 <- "Sample 2"
      }

      n1 <- sum(wilcoxonData$Group == name1)
      n2 <- sum(wilcoxonData$Group == name2)
      N <- nrow(wilcoxonData)

      mu_w <- (n1 * (N + 1)) / 2
      sigma_w <- sqrt((n1 * n2 * (N + 1)) / 12)
      observed_W <- sum(wilcoxonData %>% dplyr::filter(Group == name1) %>% dplyr::pull(Rank))
      observed_W2 <- sum(wilcoxonData %>% dplyr::filter(Group == name2) %>% dplyr::pull(Rank))

      u1_statistic <- observed_W - (n1 * (n1 + 1) / 2)
      u2_statistic <- observed_W2 - (n2 * (n2 + 1) / 2)
      u_mean <- (n1 * n2) / 2

      group1_data_values <- wilcoxonData %>%
        dplyr::filter(Group == name1) %>%
        dplyr::pull(Value)
      group2_data_values <- wilcoxonData %>%
        dplyr::filter(Group == name2) %>%
        dplyr::pull(Value)
      combined_values <- c(group1_data_values, group2_data_values)
      has_ties <- length(unique(combined_values)) < length(combined_values)

      calculate_tie_correction <- function(x) {
        if (!is.numeric(x) || length(x) == 0) {
          return(0)
        }
        tie_counts <- table(x[duplicated(x) | duplicated(x, fromLast = TRUE)])
        sum(tie_counts^3 - tie_counts)
      }

      tie_correction <- calculate_tie_correction(combined_values)
      u_std_dev <- sqrt((n1 * n2 / 12) * ((N + 1) - (tie_correction / (N * (N - 1)))))

      validate(
        need(
          !is.na(u_std_dev) && u_std_dev > 0,
          "The Z test statistic is undefined or infinite for this data, so the Z distribution plot cannot be displayed. This typically occurs when both samples have zero within-sample variance and are perfectly separated. Consider using the Exact method instead."
        ),
        errorClass = "myClass"
      )

      z_stat <- calculate_z_stat(
        input, observed_W, mu_w, sigma_w, observed_W2, n1, n2, N,
        u1_statistic, u2_statistic, u_mean, u_std_dev,
        has_ties, tie_correction
      )

      validate(
        need(
          is.finite(z_stat),
          "The Z test statistic is undefined or infinite for this data, so the Z distribution plot cannot be displayed. This typically occurs when both samples have zero within-sample variance and are perfectly separated. Consider using the Exact method instead."
        ),
        errorClass = "myClass"
      )

      alternative <- ""
      z_critical <- NA
      if (input$altHypothesis2 == "2") {
        z_critical <- qnorm(1 - SigLvl() / 2)
        alternative <- "two.sided"
      } else if (input$altHypothesis2 == "1") {
        z_critical <- qnorm(SigLvl())
        alternative <- "less"
      } else {
        z_critical <- qnorm(1 - SigLvl())
        alternative <- "greater"
      }
      wilcoxonZTestPlot(z_stat, z_critical, alternative)
    })

    ## ---- Boxplot Side by Side
    output$sidebysidewRankSum <- renderPlot(
    {
      req(input$sidebysidewRankSum)

      if (input$wilcoxonRankSumTestData == "Enter Raw Data") {
        rankSumRaw1 <- createNumLst(input$rankSumRaw1)
        rankSumRaw2 <- createNumLst(input$rankSumRaw2)
      } else if (input$wilcoxonRankSumTestData == "Upload Data") {
        rankSumRaw1 <- na.omit(unlist(WilcoxonUploadData()[, input$wilcoxonUpl1]))
        rankSumRaw2 <- na.omit(unlist(WilcoxonUploadData()[, input$wilcoxonUpl2]))
      }

      validate(
        need(
          sd(rankSumRaw1) > 0 || sd(rankSumRaw2) > 0,
          "The side-by-side boxplot cannot be displayed when both samples contain only one repeated value."
        ),
        errorClass = "myClass"
      )

      dat <- c(rankSumRaw1, rankSumRaw2)
      df_boxplot <- data.frame(
        sample = c(rep("Sample 1", length(rankSumRaw1)), rep("Sample 2", length(rankSumRaw2))),
        data = c(dat)
      )

      RenderSideBySideBoxplot(
        dat,
        df_boxplot,
        input[["sidebysidewRankSum-Colour"]],
        input[["sidebysidewRankSum-Title"]],
        input[["sidebysidewRankSum-Xlab"]],
        input[["sidebysidewRankSum-Ylab"]],
        input[["sidebysidewRankSum-BoxWidth"]] / 10,
        input[["sidebysidewRankSum-Gridlines"]],
        input[["sidebysidewRankSum-Flip"]],
        input[["sidebysidewRankSum-OutlierLabels"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["sidebysidewRankSum-Height"]], input[["sidebysidewRankSum-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["sidebysidewRankSum-Width"]], input[["sidebysidewRankSum-WidthPx"]], ui = FALSE)
    }
    )

    ## ----- QQ Plot
    output$sidebysidewRankQQ <- renderPlot(
    {
      req(input$sidebysidewRankQQ)

      if (input$wilcoxonRankSumTestData == "Enter Raw Data") {
        rankSumRaw1 <- createNumLst(input$rankSumRaw1)
        rankSumRaw2 <- createNumLst(input$rankSumRaw2)
      } else if (input$wilcoxonRankSumTestData == "Upload Data") {
        rankSumRaw1 <- na.omit(unlist(WilcoxonUploadData()[, input$wilcoxonUpl1]))
        rankSumRaw2 <- na.omit(unlist(WilcoxonUploadData()[, input$wilcoxonUpl2]))
      }

      validate(
        need(
          sd(rankSumRaw1) > 0 || sd(rankSumRaw2) > 0,
          "The Q-Q plots cannot be displayed when both samples contain only one repeated value."
        ),
        errorClass = "myClass"
      )

      RenderWilcoxQQPlots(
        rankSumRaw1,
        rankSumRaw2,
        input[["sidebysidewRankQQ-Colour"]],
        input[["sidebysidewRankQQ-Title"]],
        input[["sidebysidewRankQQ-Xlab"]],
        input[["sidebysidewRankQQ-Ylab"]],
        input[["sidebysidewRankQQ-Gridlines"]],
        input[["sidebysidewRankQQ-Flip"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["sidebysidewRankQQ-Height"]], input[["sidebysidewRankQQ-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["sidebysidewRankQQ-Width"]], input[["sidebysidewRankQQ-WidthPx"]], ui = FALSE)
    }
    )

    ## ------------ Dep Means Outputs -------------------------------------------

    ## ------------ Uploaded Data Table (no totals) ----------------------------------
    output$depPopMeansUploadTable <- renderDT({
      req(depmeansupload_iv$is_valid())
      datatable(DepMeansUploadData(),
                options = list(
                  pageLength = -1,
                  lengthMenu = list(
                    c(25, 50, 100, -1),
                    c("25", "50", "100", "all")
                  ),
                  columnDefs = list(list(
                    className = "dt-center",
                    targets = 0:ncol(DepMeansUploadData())
                  ))
                )
                )
    })

    ## ----------- Q-Q Plots ----------------------------------------------------------
    output$depMeansQQPlot <- renderPlot(
    {
      ## dep means qq plot
      req(input$depMeansQQPlot)

      dat <- GetDepMeansData()

      ## dat$d is the difference between the samples (i.e before - after)
      df <- tibble(values = dat$d)

      RenderQQPlot(
        dat = df,
        plotColour = input[["depMeansQQPlot-Colour"]],
        plotTitle = input[["depMeansQQPlot-Title"]],
        plotXlab = input[["depMeansQQPlot-Xlab"]],
        plotYlab = input[["depMeansQQPlot-Ylab"]],
        gridlines = input[["depMeansQQPlot-Gridlines"]],
        flip = input[["depMeansQQPlot-Flip"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["depMeansQQPlot-Height"]], input[["depMeansQQPlot-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["depMeansQQPlot-Width"]], input[["depMeansQQPlot-WidthPx"]], ui = FALSE)
    }
    )

    ## ---------------- Data Table ----
    output$depMeansData <- renderDT({
      depData <- GetDepMeansData()

      df_depData <- data.frame(depData$before, depData$after, depData$d, depData$d^2)
      names(df_depData) <- c("Sample 1", "Sample 2", "<em>d</em> = (Sample 1 - Sample 2)", "<em>d</em><sup>2</sup>")
      df_depData <- bind_rows(df_depData, summarise(df_depData, across(where(is.numeric), sum)))
      rownames(df_depData)[nrow(df_depData)] <- "Totals"

      datatable(round(df_depData, digits = 4),
                options = list(
                  dom = "lftp",
                  pageLength = -1,
                  lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")),
                  ordering = FALSE
                ),
                escape = FALSE
                ) %>% formatStyle(
                        names(df_depData),
                        target = "row",
                        fontWeight = styleRow(dim(df_depData)[1], "bold")
                      )
    })

    ## ---------------- CI ----
    output$depMeansCI <- renderUI({
      tInt <- DepMeansTInt()
      dSum <- round(sum(GetDepMeansData()$d), 4)
      dSqrdSum <- round(sum(GetDepMeansData()$d^2), 4)

      p(
        withMathJax(),
        br(),
        sprintf("\\( \\displaystyle CI = \\bar{d} \\pm \\left( t_{\\alpha/2, \\, df} \\cdot \\dfrac{ s_{d} }{ \\sqrt{n} } \\right) \\)"),
        br(),
        br(),
        p("where"),
        sprintf(
          "\\( \\qquad \\bar{d} = \\dfrac{ \\sum d }{ n } = \\dfrac{%s}{%s} = %s \\; , \\)",
          dSum,
          tInt["Sample Size"],
          tInt["Sample Mean"]
        ),
        sprintf("\\( \\qquad s_{d} = \\sqrt{ \\dfrac{\\sum d^{2} - \\dfrac{(\\sum d)^{2}}{n} }{n - 1} } \\)"),
        sprintf(
          "\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\; , \\)",
          dSqrdSum,
          dSum,
          tInt["Sample Size"],
          tInt["Sample Size"],
          tInt["Sample SD"]
        ),
        sprintf(
          "\\( \\qquad df = n - 1 = %s \\)",
          tInt["Sample Size"] - 1
        ),
        br(),
        br(),
        br(),
        sprintf(
          "\\( \\displaystyle CI = %g \\pm \\left( t_{%g/2, \\, %g} \\cdot \\dfrac{ %g }{ \\sqrt{ %g } } \\right) \\)",
          tInt["Sample Mean"],
          1 - ConfLvl(),
          tInt["Sample Size"] - 1,
          tInt["Sample SD"],
          tInt["Sample Size"]
        ),
        br(),
        br(),
        sprintf(
          "\\( \\displaystyle \\phantom{CI} = %g \\pm \\left( t_{%g, \\, %g} \\cdot \\dfrac{ %g }{ %g } \\right) \\)",
          tInt["Sample Mean"],
          (1 - ConfLvl()) / 2,
          tInt["Sample Size"] - 1,
          tInt["Sample SD"],
          sqrt(tInt["Sample Size"])
        ),
        br(),
        br(),
        sprintf(
          "\\( \\displaystyle \\phantom{CI} = %g \\pm ( %g \\cdot %g ) \\)",
          tInt["Sample Mean"],
          tInt["T Critical"],
          tInt["Std Error"]
        ),
        br(),
        br(),
        sprintf(
          "\\( \\displaystyle \\phantom{CI} = %g \\pm  %g  \\)",
          tInt["Sample Mean"],
          tInt["ME"]
        ),
        br(),
        br(),
        sprintf(
          "\\( \\displaystyle \\phantom{CI} = (%g, \\, %g)  \\)",
          tInt["LCL"],
          tInt["UCL"]
        ),
        br(),
        br(),
        br(),
        p(tags$b("Interpretation:")),
        sprintf(
          "We are \\( %1.0f \\)%% confident that the population mean difference \\( (\\mu_{d})\\) is between \\( %g \\) and \\( %g \\).",
          ConfLvl() * 100,
          tInt["LCL"],
          tInt["UCL"]
        ),
        br(),
        br(),
        br()
      )
    })

    ## ---------------- HT ----
    output$depMeansHT <- renderUI({
      req(GetDepMeansData()$sd != 0)

      tTest <- DepMeansTTest()
      dSum <- round(sum(GetDepMeansData()$d), 4)
      dSqrdSum <- round(sum(GetDepMeansData()$d^2), 4)
      muNaught <- round(input$depMeansMuNaught, 4)

      intrpInfo <- IndMeansHypInfo()

      if (tTest["P-Value"] > SigLvl()) {
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

      if (input$altHypothesis2 == "2") {
        critVal <- paste("\\pm", tTest["T Critical"])
        nullHyp <- "\\mu_{d} ="
        altHyp <- "\\mu_{d} \\neq"
      } else {
        critVal <- tTest["T Critical"]

        if (input$altHypothesis2 == "1") {
          nullHyp <- "\\mu_{d} \\geq"
          altHyp <- "\\mu_{d} \\lt"
        } else {
          nullHyp <- "\\mu_{d} \\leq"
          altHyp <- "\\mu_{d} \\gt"
        }
      }

      depHTHead <- tagList(
        p(
          withMathJax(),
          sprintf(
            "\\( H_{0}: %s %s\\)",
            nullHyp, muNaught
          ),
          br(),
          sprintf(
            "\\( H_{a}: %s %s\\)",
            altHyp, muNaught
          ),
          br(),
          br(),
          sprintf(
            "\\( \\alpha = %s \\)",
            SigLvl()
          ),
          br(),
          br(),
          p(tags$b("Test Statistic:")),
          sprintf("\\( \\displaystyle t = \\dfrac{\\bar{d} - (\\mu_{d})_{0}}{ \\left( \\dfrac{ s_{d} }{ \\sqrt{n} } \\right) } \\qquad \\)"),
          br(),
          br(),
          p("where"),
                                        # HERE
          sprintf(
            "\\( \\qquad \\bar{d} = \\dfrac{ \\sum d }{ n } = \\dfrac{%s}{%s} = %s \\; , \\)",
            dSum,
            tTest["Sample Size"],
            tTest["Sample Mean"]
          ),
          sprintf("\\( \\qquad s_{d} = \\sqrt{ \\dfrac{\\sum d^{2} - \\dfrac{(\\sum d)^{2}}{n} }{n - 1} } \\)"),
          sprintf(
            "\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\; , \\)",
            dSqrdSum,
            dSum,
            tTest["Sample Size"],
            tTest["Sample Size"],
            tTest["Sample SD"]
          ),
          br(),
          br(),
          br(),
          sprintf(
            "\\( t = \\dfrac{%g - %s}{ \\left( \\dfrac{ %g }{ \\sqrt{ %g } } \\right) } \\)",
            tTest["Sample Mean"],
            if (muNaught < 0) sprintf("(%g)", muNaught) else sprintf("%g", muNaught),
            tTest["Sample SD"],
            tTest["Sample Size"]
          ),
          ## sprintf("\\( \\displaystyle \\; = \\; \\dfrac{%g}{ \\left( \\dfrac{ %g }{ %g } \\right) } \\)",
          ## tTest["Sample Mean"] - muNaught,
          ## tTest["Sample SD"],
          ## sqrt(tTest["Sample Size"])),
          ## br(),
          ## br(),
          sprintf(
            "\\( \\displaystyle \\phantom{t} = \\; \\dfrac{ %g }{ %g } \\)",
            tTest["Sample Mean"] - muNaught,
            tTest["Std Error"]
          ),
          sprintf(
            "\\( \\displaystyle \\; = \\; %g \\)",
            tTest["Test Statistic"]
          ),
          br(),
          br(),
          br()
        )
      )

      depHTPVal <- printHTPVal(
        tTest["P-Value"],
        "t",
        intrpInfo$alternative,
        tTest["Test Statistic"],
        pvalSymbol,
        reject
      )

      depHTTail <- tagList(
        p(
          withMathJax(),
          p(tags$b("Using Critical Value Method:")),
          sprintf(
            "Critical Value(s) \\( = %s t_{%s, \\, df} = %s t_{%s, \\, %s} = %s \\)",
            IndMeansHypInfo()$critSign,
            IndMeansHypInfo()$critAlph,
            IndMeansHypInfo()$critSign,
            IndMeansHypInfo()$alphaVal,
            tTest["Sample Size"] - 1,
            critVal
          ),
          br(),
          br(),
          p("where"),
          sprintf(
            "\\( \\qquad df = n - 1 = %s \\)",
            tTest["Sample Size"] - 1
          ),
          br(),
          br(),
          sprintf(
            "Since the test statistic \\( (t)\\) falls within the %s region, %s \\( H_{0}\\).",
            region,
            reject
          ),
          br()
        ),
        plotOutput(session$ns("depMeansHTPlot"), width = "75%", height = "300px"),
        br()
      )

      depHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, muNaught)

      tagAppendChildren(depHTHead, depHTPVal, depHTTail, depHTConclusion)
    })

    ## ---------------- HT Plot ----
    output$depMeansHTPlot <- renderPlot({
      if (GetDepMeansData()$sd != 0) {
        tTest <- DepMeansTTest()
        intrpInfo <- IndMeansHypInfo()

        htPlotCritVal <- tTest["T Critical"]

        depMeansPlot <- hypTTestPlot(tTest["Test Statistic"], tTest["df"], htPlotCritVal, intrpInfo$alternative)
        depMeansPlot
      }
    })

    ## ------------ Signed Rank Test Outputs --------------------------------------------
    output$signedRankHypothesisTest <- renderUI({
      req(!is.null(signedRankedData()))
      req(nrow(signedRankedData()) > 0)

      if (input$signedRankTest == "Upload Data") {
        req(input$signedRankUpl1, input$signedRankUpl2)
        name1 <- input$signedRankUpl1
        name2 <- input$signedRankUpl2
      } else {
        name1 <- "Sample 1"
        name2 <- "Sample 2"
      }

      data_ranked <- signedRankedData()

      positive_ranks <- data_ranked$SignedRank[data_ranked$SignedRank > 0]
      negative_ranks <- data_ranked$SignedRank[data_ranked$SignedRank < 0]

      W_plus <- sum(positive_ranks)
      W_minus <- sum(abs(negative_ranks))

      n <- nrow(data_ranked)

      W_stat <- W_plus

      mu_w <- n * (n + 1) / 4

      ranks <- abs(data_ranked$Rank)
      tie_groups <- table(ranks)
      tie_adjustment <- sum((tie_groups^3 - tie_groups) / 48)
      sigma_w <- sqrt(n * (n + 1) * ((2 * n) + 1) / 24)

      significance <- 1 - SigLvl()

      z_stat <- (W_stat - mu_w) / sigma_w

      if (W_stat > mu_w) {
        z_stat_corrected <- (W_stat - 0.5 - mu_w) / sigma_w
      } else if (W_stat < mu_w) {
        z_stat_corrected <- (W_stat + 0.5 - mu_w) / sigma_w
      } else {
        z_stat_corrected <- 0
      }

      if (input$altHypothesis2 == "2") {
        z_critical <- qnorm(1 - SigLvl() / 2)
        critVal <- paste("\\pm", round(qnorm(1 - SigLvl() / 2), 3))
        nullHyp <- paste0("\\text{Median difference} = 0")
        altHyp <- paste0("\\text{Median difference} \\neq 0")
        altern <- "two.sided"
        in_rejection_region <- abs(z_stat) > z_critical
      } else if (input$altHypothesis2 == "1") {
        z_critical <- qnorm(SigLvl())
        critVal <- round(qnorm(SigLvl()), 3)
        nullHyp <- paste0("\\text{Median difference} \\geq 0")
        altHyp <- paste0("\\text{Median difference} \\lt 0")
        altern <- "less"
        in_rejection_region <- z_stat < z_critical
      } else {
        z_critical <- qnorm(1 - SigLvl())
        critVal <- round(qnorm(1 - SigLvl()), 3)
        nullHyp <- paste0("\\text{Median difference} \\leq 0")
        altHyp <- paste0("\\text{Median difference} \\gt 0")
        altern <- "greater"
        in_rejection_region <- z_stat > z_critical
      }

      if (in_rejection_region) {
        pvalSymbol <- "\\leq"
        suffEvidence <- "is"
        reject <- "reject"
        region <- "rejection"
      } else {
        pvalSymbol <- "\\gt"
        suffEvidence <- "isn't"
        reject <- "do not reject"
        region <- "acceptance"
      }

      sample1_data <- data_ranked$Sample1
      sample2_data <- data_ranked$Sample2

      if (input$altHypothesis2 == "2") {
        p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
      } else if (input$altHypothesis2 == "1") {
        p_value <- pnorm(z_stat, lower.tail = TRUE)
      } else {
        p_value <- pnorm(z_stat, lower.tail = FALSE)
      }
      ## Jacie: show for exact approximation
      if (input$normaprowrs == "Exact") {
        signedRankHTHead <- tagList(
          p(
            withMathJax(),
            sprintf("\\( H_{0}:\\ %s\\)", nullHyp),
            br(),
            sprintf("\\( H_{a}:\\ %s\\)", altHyp),
            br(), br(),
            sprintf("\\( \\alpha = %s \\)", SigLvl()),
            br(), br(),
            sprintf("\\(n = %s\\) (number of non-zero differences)", n),
            br(),
            p(tags$b("Sum of Signed Ranks:")),
            sprintf("\\(  W^{+} = %s \\) (sum of positive ranks)", W_plus),
            br(),
            sprintf("\\(  W^{-} = %s \\) (sum of negative ranks)", W_minus),
            br(), br(),
            )
        )
      }
      ## Jacie: show for normal approximation for large samples
      else {
        signedRankHTHead <- tagList(
          p(
            withMathJax(),
            sprintf("\\( H_{0}:\\ %s\\)", nullHyp),
            br(),
            sprintf("\\( H_{a}:\\ %s\\)", altHyp),
            br(), br(),
            sprintf("\\( \\alpha = %s \\)", SigLvl()),
            br(), br(),
            sprintf("\\(n = %s\\) (number of non-zero differences)", n),
            br(),
            p(tags$b("Sum of Signed Ranks:")),
            sprintf("\\(  W^{+} = %s \\) (sum of positive ranks)", W_plus),
            br(),
            sprintf("\\(  W^{-} = %s \\) (sum of negative ranks)", W_minus),
            br(), br(),
            p(tags$b("Mean:")),
            sprintf(
              "\\(  \\mu_{W^+} = \\frac{n(n + 1)}{4} = \\frac{%s(%s + 1)}{4} = %s \\)",
              n, n, mu_w
            ),
            br(), br(),
            p(tags$b("Standard Deviation:")),
            sprintf(
              "\\( \\sigma_{W^+} = \\sqrt{\\frac{n(n+1)(2n+1)}{24}} = \\sqrt{\\frac{%s (%s+1) (2 \\times %s +1)}{24}} = %s \\)",
              n, n, n, round(sigma_w, 4)
            ),
            br(), br(),
            p(tags$b("Test Statistic:")),
            sprintf(
              "\\(  z = \\frac{W^{+} - \\mu_{W^+}}{\\sigma_{W^+}} = \\frac{%s - %s}{%s} = %s \\)",
              W_stat,
              mu_w,
              round(sigma_w, 4),
              round(z_stat, 3)
            ),
            br(), br(),
            p(tags$b("Using P-value Method:")),
            sprintf("\\( P = %s \\)", round(p_value, 4)),
            br(),
            if (p_value <= SigLvl()) {
              tagList(
                sprintf("\\( \\text{Since } P \\leq %s, \\text{reject } H_0. \\)", SigLvl()),
                br(), br()
              )
            } else {
              tagList(
                sprintf("\\( \\text{Since } P > %s, \\text{do not reject } H_0. \\)", SigLvl()),
                br(), br()
              )
            }
          )
        )


        signedRankHTTail <- tagList(
          p(
            withMathJax(),
            p(tags$b("Using Critical Value Method:")),
            sprintf(
              "Critical Value(s) \\( = %s z_{%s} = %s \\)",
              if (input$altHypothesis2 == "2") "\\pm" else if (input$altHypothesis2 == "1") "-" else "",
              if (input$altHypothesis2 == "2") SigLvl() / 2 else SigLvl(),
              critVal
            ),
            br(),
            br(),
            sprintf(
              "Since the test statistic \\( (z = %s)\\) falls within the %s region, %s \\( H_{0}\\).",
              round(z_stat, 3),
              region,
              reject
            ),
            br()
          ),
          plotOutput(session$ns("signedRankPlot"), width = "75%", height = "300px"),
          br()
        )

        depHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, "")

        tagAppendChildren(signedRankHTHead, signedRankHTTail, depHTConclusion)
      }
    })

    output$signedRankPlot <- renderPlot({
      signedRankData <- signedRankedData()

      if (is.null(signedRankData) || nrow(signedRankData) == 0) {
        return(NULL)
      }

      if (input$signedRankTest == "Upload Data") {
        name1 <- input$signedRankUpl1
        name2 <- input$signedRankUpl2
      } else {
        name1 <- "Sample 1"
        name2 <- "Sample 2"
      }

      positive_ranks <- signedRankData$SignedRank[signedRankData$SignedRank > 0]
      negative_ranks <- signedRankData$SignedRank[signedRankData$SignedRank < 0]

      W_plus <- sum(positive_ranks)
      W_minus <- sum(abs(negative_ranks))

      n <- nrow(signedRankData)

      W_stat <- W_plus

      mu_w <- n * (n + 1) / 4

      ranks <- abs(signedRankData$Rank)
      tie_groups <- table(ranks)
      tie_adjustment <- sum((tie_groups^3 - tie_groups) / 48)
      sigma_w <- sqrt(n * (n + 1) * (2 * n + 1) / 24)

      z_stat <- (W_stat - mu_w) / sigma_w

      alternative <- ""
      z_critical <- NA

      if (input$altHypothesis2 == "2") {
        z_critical <- qnorm(1 - SigLvl() / 2)
        alternative <- "two.sided"
      } else if (input$altHypothesis2 == "1") {
        z_critical <- qnorm(SigLvl())
        alternative <- "less"
      } else {
        z_critical <- qnorm(1 - SigLvl())
        alternative <- "greater"
      }

      wilcoxonPlot <- wilcoxonZTestPlot(z_stat, z_critical, alternative)
      wilcoxonPlot
    })

    output$signedRankUploadTable <- renderDT({
      req(signedRankUpload_iv$is_valid())
      datatable(signedRankUploadData(),
                options = list(
                  pageLength = -1,
                  lengthMenu = list(
                    c(25, 50, 100, -1),
                    c("25", "50", "100", "all")
                  ),
                  columnDefs = list(list(
                    className = "dt-center",
                    targets = 0:ncol(signedRankUploadData())
                  ))
                )
                )
    })

    output$signedRankQQ <- renderPlot(
    {
      req(input$signedRankQQPlot)
      req(!is.null(signedRankedData()))
      req(nrow(signedRankedData()) > 0)

      data_ranked <- signedRankedData()
      differences <- data_ranked$Sample1 - data_ranked$Sample2
      differences <- differences[differences != 0]

      ## Coalesce NULL to the default value; collapse character vectors of
      ## empty strings to a single empty string; substitute default value for
      ## zero-length vectors (as appropriate, a singular empty string,
      ## zero-length character vector, or the number zero).
      safe_input <- function(input_name = c("Title", "Xlab", "Ylab", "Colour", "Gridlines", "Flip")) {
        input_name <- match.arg(input_name)
        if (input_name %in% c("Title", "Xlab", "Ylab")) {
          default_value <- ""
        } else if (input_name == "Colour") {
          default_value <- "blue"
        } else if (input_name == "Gridlines") {
          default_value <- character()
        } else if (input_name == "Flip") {
          default_value <- 0
        }

        value <- input[[paste0("signedRankQQ-", input_name)]] %||% default_value
        if (is.character(value) && all(value == "") || length(value) == 0) {
          value <- default_value
        }

        value
      }

      RenderSignedRankQQPlot(
        data.frame(values = differences),
        safe_input("Colour"),
        safe_input("Title"),
        safe_input("Xlab"),
        safe_input("Ylab"),
        safe_input("Gridlines"),
        safe_input("Flip")
      )
    },
    height = function() {
      height_val <- input[["signedRankQQ-Height"]]
      height_px_val <- input[["signedRankQQ-HeightPx"]]
      if (is.null(height_val) || is.null(height_px_val)) {
        return(400)
      }
      GetPlotHeight(height_val, height_px_val, ui = FALSE)
    },
    width = function() {
      width_val <- input[["signedRankQQ-Width"]]
      width_px_val <- input[["signedRankQQ-WidthPx"]]
      if (is.null(width_val) || is.null(width_px_val)) {
        return(600)
      }
      GetPlotWidth(width_val, width_px_val, ui = FALSE)
    }
    )

    ## ------------ Two Prop Outputs --------------------------------------------

    ## ---------------- CI ----
    output$twoPropCI <- renderUI({
      req(si_iv$is_valid())

      twoPropZInt <- TwoPropZInt(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, ConfLvl())
      twoPropZInt["Z Critical"] <- round(twoPropZInt["Z Critical"], cvDigits)

      p(
        withMathJax(
          sprintf("Given:"),
          br(),
          sprintf(
            "\\( x_{1} = %s \\)",
            input$numSuccesses1
          ),
          br(),
          sprintf(
            "\\( n_{1} = %s \\)",
            input$numTrials1
          ),
          br(),
          sprintf(
            "\\( x_{2} = %s \\)",
            input$numSuccesses2
          ),
          br(),
          sprintf(
            "\\( n_{2} = %s \\)",
            input$numTrials2
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
            twoPropZInt["Z Critical"]
          ),
          br(),
          br(),
          br(),
          sprintf("\\( \\displaystyle CI = (\\hat{p}_{1} - \\hat{p}_{2}) \\pm \\left( z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_{1}(1-\\hat{p}_{1})}{n_{1}} + \\dfrac{\\hat{p}_{2}(1-\\hat{p}_{2})}{n_{2}}} \\right) \\)"),
          br(),
          p("where"),
          sprintf(
            "\\( \\displaystyle \\qquad \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%s}{%s} = %0.4f,\\)",
            input$numSuccesses1,
            input$numTrials1,
            twoPropZInt["Sample Proportion 1"]
          ),
          br(),
          p("and"),
          sprintf(
            "\\( \\displaystyle \\qquad \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%s}{%s} = %0.4f,\\)",
            input$numSuccesses2,
            input$numTrials2,
            twoPropZInt["Sample Proportion 2"]
          ),
          br(),
          br(),
          br(),
          sprintf(
            "\\( \\displaystyle CI = (%0.4f - %0.4f) \\pm \\left( %s \\sqrt{\\dfrac{%0.4f(1-%0.4f)}{%1.0f} + \\dfrac{%0.4f(1-%0.4f)}{%1.0f}} \\right) \\)",
            twoPropZInt["Sample Proportion 1"],
            twoPropZInt["Sample Proportion 2"],
            twoPropZInt["Z Critical"],
            twoPropZInt["Sample Proportion 1"],
            twoPropZInt["Sample Proportion 1"],
            input$numTrials1,
            twoPropZInt["Sample Proportion 2"],
            twoPropZInt["Sample Proportion 2"],
            input$numTrials2
          ),
          br(),
          br(),
          sprintf(
            "\\( \\phantom{CI} = %0.4f \\pm ( %s \\cdot %0.4f ) \\)",
            twoPropZInt["Difference of proportions"],
            twoPropZInt["Z Critical"],
            twoPropZInt["Std Error"]
          ),
          br(),
          br(),
          sprintf(
            "\\( \\phantom{CI} = %0.4f \\pm %0.4f \\)",
            twoPropZInt["Difference of proportions"],
            twoPropZInt["Margin of Error"]
          ),
          br(),
          br(),
          sprintf(
            "\\( \\phantom{CI} = (%0.4f, %0.4f)\\)",
            twoPropZInt["LCL"],
            twoPropZInt["UCL"]
          ),
          br(),
          br(),
          br(),
          p(tags$b("Interpretation:")),
          sprintf(
            "We are %1.0f%% confident that the difference in population proportions \\( (p_{1} - p_{2}) \\) is between \\( %0.4f \\) and \\( %0.4f \\).",
            ConfLvl() * 100,
            twoPropZInt["LCL"],
            twoPropZInt["UCL"]
          )
        )
      )
    })

    ## ---------------- HT ----
    output$twoPropHT <- renderUI({
      req(si_iv$is_valid())

      diffNaught <- input$propDiffNaught
      twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, diffNaught, IndMeansHypInfo()$alternative, SigLvl())
      twoPropZTest["Z Critical"] <- round(twoPropZTest["Z Critical"], cvDigits)

      if (input$altHypothesis2 == "2") {
        critZVal <- paste("\\pm", twoPropZTest["Z Critical"])
        htPlotCritVals <- c(-twoPropZTest["Z Critical"], twoPropZTest["Z Critical"])
        nullHyp <- "p_{1} ="
        altHyp <- "p_{1} \\neq"
      } else {
        critZVal <- paste(twoPropZTest["Z Critical"])
        htPlotCritVals <- twoPropZTest["Z Critical"]

        if (input$altHypothesis2 == "1") {
          nullHyp <- "p_{1} \\geq"
          altHyp <- "p_{1} \\lt"
        } else {
          nullHyp <- "p_{1} \\leq"
          altHyp <- "p_{1} \\gt"
        }
      }

      propDiff <- twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"]

      if (twoPropZTest["P-Value"] > SigLvl()) {
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

      twoPropHTHead <- tagList(
        withMathJax(
          sprintf(
            "\\( H_{0}: %s p_{2}\\)",
            nullHyp
          ),
          br(),
          sprintf(
            "\\( H_{a}: %s p_{2}\\)",
            altHyp
          ),
          br(),
          br(),
          sprintf(
            "\\( \\alpha = %g \\)",
            SigLvl()
          ),
                                        # br(),
          br(),
          p(tags$b("Test Statistic:")),
          sprintf("Given:"),
          br(),
          sprintf(
            "\\( x_{1} = %s \\)",
            input$numSuccesses1
          ),
          br(),
          sprintf(
            "\\( n_{1} = %s \\)",
            input$numTrials1
          ),
          br(),
          sprintf(
            "\\( x_{2} = %s \\)",
            input$numSuccesses2
          ),
          br(),
          sprintf(
            "\\( n_{2} = %s \\)",
            input$numTrials2
          ),
          br(),
          br(),
          br(),
          sprintf("\\(z = \\dfrac{ (\\hat{p}_{1} - \\hat{p}_{2}) - (p_{1} - p_{2})_{0} }{\\sqrt{\\hat{p}(1-\\hat{p})\\left(\\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}}\\right)}}\\)"),
          br(),
          br(),
          p("where"),
          sprintf("\\( \\displaystyle \\qquad \\hat{p} = \\dfrac{x_{1} + x_{2}}{n_{1} + n_{2}} \\)"),
          sprintf(
            "\\( = \\dfrac{%g + %g}{%g + %g} = %0.4f, \\)",
            input$numSuccesses1,
            input$numSuccesses2,
            input$numTrials1,
            input$numTrials2,
            twoPropZTest["Pooled Proportion"]
          ),
          br(),
          p("and"),
          sprintf(
            "\\( \\displaystyle \\qquad \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%s}{%s} = %0.4f,\\)",
            input$numSuccesses1,
            input$numTrials1,
            twoPropZTest["Sample Proportion 1"]
          ),
          br(),
          p("and"),
          sprintf(
            "\\( \\displaystyle \\qquad \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%s}{%s} = %0.4f,\\)",
            input$numSuccesses2,
            input$numTrials2,
            twoPropZTest["Sample Proportion 2"]
          ),
          br(),
          br(),
          br(),
          sprintf(
            "\\( z = \\dfrac{ (%0.4f - %0.4f) - %s}{\\sqrt{%0.4f(1-%0.4f)\\left(\\dfrac{1}{%g} + \\dfrac{1}{%g}\\right)}}\\)",
            twoPropZTest["Sample Proportion 1"],
            twoPropZTest["Sample Proportion 2"],
            if (diffNaught < 0) sprintf("(%.4f)", diffNaught) else sprintf("%.4f", diffNaught),
            twoPropZTest["Pooled Proportion"],
            twoPropZTest["Pooled Proportion"],
            input$numTrials1,
            input$numTrials2
          ),
          sprintf(
            "\\( = \\dfrac{%0.4f}{%0.4f} \\)",
            twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"] - diffNaught,
            twoPropZTest["Std Error"]
          ),
          br(),
          br(),
          sprintf(
            "\\(\\phantom{z} = %0.4f\\)",
            twoPropZTest["Test Statistic"]
          ),
          br(),
          br(),
          br()
        )
      )

      twoPropHTPVal <- printHTPVal(
        twoPropZTest["P-Value"],
        "z",
        IndMeansHypInfo()$alternative,
        twoPropZTest["Test Statistic"],
        pvalSymbol,
        reject
      )

      twoPropHTTail <- tagList(
        withMathJax(
          p(tags$b("Using Critical Value Method:")),
          sprintf(
            "Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
            IndMeansHypInfo()$critSign,
            IndMeansHypInfo()$critAlph,
            IndMeansHypInfo()$critSign,
            IndMeansHypInfo()$alphaVal,
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
          plotOutput(session$ns("twoPropHTPlot")),
          br()
        )
      )

      twoPropHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, "p_{2}")

      tagAppendChildren(twoPropHTHead, twoPropHTPVal, twoPropHTTail, twoPropHTConclusion)
    })

    ## ---------------- HT Plot ----
    output$twoPropHTPlot <- renderPlot({
      req(si_iv$is_valid())

      twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, input$propDiffNaught, IndMeansHypInfo()$alternative, SigLvl())
      htPlotCritVal <- round(twoPropZTest["Z Critical"], cvDigits)

      htPlot <- hypZTestPlot(twoPropZTest["Test Statistic"], htPlotCritVal, IndMeansHypInfo()$alternative)
      htPlot
    })

    ## --------------- Stacked Bar Plot and Pie Chart ----
    output$twoPropBarPlot <- renderPlot({
      req(
        input$numTrials1 >= input$numSuccesses1,
        input$numTrials2 >= input$numSuccesses2
      )

      df <- tibble(
        Group = c("Group 1", "Group 1", "Group 2", "Group 2"),
        Outcome = c("Successes", "Failures", "Successes", "Failures"),
        Count = c(
          input$numSuccesses1, input$numTrials1 - input$numSuccesses1,
          input$numSuccesses2, input$numTrials2 - input$numSuccesses2
        )
      )

      ggplot(df, aes(x = Group, y = Count, fill = Outcome)) +
        geom_col(position = "fill", width = 0.5) +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(
          title = "Stacked Bar Chart: Proportion of Successes vs Failures",
          y = "Proportion", x = ""
        ) +
        scale_fill_manual(values = c("Successes" = "#4CAF50", "Failures" = "#F44336")) +
        theme(
          axis.text.x = element_text(size = 14, face = "bold", color = "black"),
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 18, face = "bold"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    })

    output$twoPropPieChart <- renderPlot({
      req(
        input$numTrials1 >= input$numSuccesses1,
        input$numTrials2 >= input$numSuccesses2
      )

      df <- tibble(
        Group = rep(c("Group 1", "Group 2"), each = 2),
        Outcome = c("Successes", "Failures", "Successes", "Failures"),
        Count = c(
          input$numSuccesses1, input$numTrials1 - input$numSuccesses1,
          input$numSuccesses2, input$numTrials2 - input$numSuccesses2
        )
      )

      ## Calculate percentage for labels
      df <- df %>%
        group_by(Group) %>%
        mutate(
          Percent = Count / sum(Count),
          Label = paste0(Outcome, " (", scales::percent(Percent), ")")
        )

      ggplot(df, aes(x = "", y = Count, fill = Outcome)) +
        geom_col(color = "white") +
        coord_polar(theta = "y") +
        facet_wrap(~Group) + ## Separate pie charts per group
        scale_fill_manual(values = c("Successes" = "#4CAF50", "Failures" = "#F44336")) +
        labs(title = "Success vs Failure Distribution by Group") +
        theme_void() +
        theme(
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          strip.text = element_text(size = 14, face = "bold")
        )
    })

    ## ------------ Two Pop Var Outputs ----------------------------------------------
    ## ----------- CI
    output$twoPopVarCI <- renderUI({
      req(si_iv$is_valid())

      data <- GetAllTwoPopVarData()
      is_variance <- (input$dataAvailability3 == "Variance")

      CI <- TwoPopVarCI(data$n1, data$sd1, data$n2, data$sd2, ConfLvl(), is_variance)
      df1 <- data$n1 - 1
      df2 <- data$n2 - 1
      conf_percent <- ConfLvl() * 100
      alpha <- 1 - ConfLvl()

      tagList(
        withMathJax(
          if (input$dataAvailability3 != "Enter Raw Data") {
            p("Given:")
          } else {
            p("From the Data:")
          },
          printTwoPopVarGivens(data, is_variance),
          p(sprintf("For a \\(%.0f\\%%\\) confidence interval:", conf_percent)),
          p(sprintf("\\(\\alpha = 1 - %.2f = %.2f\\)", ConfLvl(), alpha)),
          br(),

          ## df
          printDegreesFreedom(df1, df2),

          ## critical values
          p(sprintf(
            "\\(F_{\\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)",
            alpha / 2, df2, df1, CI$F_lower
          )),
          p(sprintf(
            "\\(F_{1 - \\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)",
            1 - (alpha / 2), df2, df1, CI$F_upper
          )),
          br(),

          ## F stat calculation
          printFStat(data$sd1, data$sd2, CI$F_statistic, is_variance),

          ## formula for CI
          p("\\( \\displaystyle CI = \\left( F_{\\alpha/2,\\ df_2\\,,\\ df_1} \\cdot \\dfrac{s_1^2}{s_2^2},\\ F_{1 - \\alpha/2,\\ df_2\\,,\\ df_1} \\cdot \\dfrac{s_1^2}{s_2^2} \\right) \\)"),
          br(),

          ## formula with subbed in values
          p(sprintf(
            "\\( \\displaystyle CI = \\left( %.4f \\cdot %.4f,\\ %.4f \\cdot %.4f \\right) \\)",
            CI$F_lower, CI$F_statistic, CI$F_upper, CI$F_statistic
          )),
          br(),

          ## CI result
          p(sprintf("\\( \\displaystyle CI = (%.4f, %.4f) \\)", CI$CI_lower, CI$CI_upper)),
          br(),

          ## interpretation
          HTML(sprintf("<strong>Interpretation:</strong>")),
          br(),
          br(),
          p(sprintf(
            "We are \\(%.0f\\%%\\) confident that the ratio of the population variances is between \\(%.4f\\) and \\(%.4f\\).",
            conf_percent, CI$CI_lower, CI$CI_upper
          ))
        )
      )
    })

    ## ------------ HT
    output$twoPopVarHT <- renderUI({
      req(si_iv$is_valid())

      data <- GetAllTwoPopVarData()
      hyp_labels <- TwoPopVarHypInfo()

      is_variance <- (input$dataAvailability3 == "Variance")
      sig_lvl <- SigLvl()
      alt_hyp <- hyp_labels$alternative

      HT <- TwoPopVarHT(data$n1, data$sd1, data$n2, data$sd2, sig_lvl, alt_hyp, is_variance)
      df1 <- data$n1 - 1
      df2 <- data$n2 - 1

      text <- twoPopVarOutputText(HT, sig_lvl)

      tagList(
        withMathJax(
          ## hypotheses
          p(sprintf("\\(H_0: %s\\)", hyp_labels$nullHyp)),
          p(sprintf("\\(H_a: %s\\)", hyp_labels$altHyp)),
          p(sprintf("\\( \\alpha = %.2f \\)", sig_lvl)),
          p(strong("Test Statistic:")),
          if (input$dataAvailability3 != "Enter Raw Data") {
            p("Given:")
          } else {
            p("From the Data")
          },

          ## print givens
          printTwoPopVarGivens(data, is_variance),

          ## print F stat calc
          printFStat(data$sd1, data$sd2, HT$F_statistic, is_variance, is_HT = TRUE),
          br(),

          ## print P value method
          printFTestPVal(
            pValue = HT$p_value,
            testStatVal = HT$F_statistic,
            alternative = alt_hyp,
            pValSign = text$pValSign,
            rejectWord = text$rejectWord
          ),

          ## print crit value method
          p(tags$b("Using Critical Value Method:")),
          ## two sided
          if (alt_hyp == "two.sided") {
            list(
              p(sprintf("Critical Values:")),
              sprintf(
                "\\(F_{\\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)",
                sig_lvl / 2, df2, df1, HT$crit_lower
              ), br(),
              sprintf(
                "\\(F_{1-\\alpha/2,\\ df_2,\\ df_1} = F_{%.3f,\\ %d,\\ %d} = %.4f\\)",
                1 - sig_lvl / 2, df1, df2, HT$crit_upper
              )
            )

            ## greater than
          } else if (alt_hyp == "greater") {
            sprintf(
              "Critical Value = \\(F_{1-\\alpha,\\ df_1,\\ df_2} = F_{%.2f,\\ %d,\\ %d} = %.4f\\)",
              1 - sig_lvl, df1, df2, HT$crit_val
            )

            ## less than
          } else {
            sprintf(
              "Critical Value = \\(F_{\\alpha,\\ df_2,\\ df_1} = F_{%.2f,\\ %d,\\ %d} = %.4f\\)",
              sig_lvl, df2, df1, HT$crit_val
            )
          },
          br(), br(),

          ## print degrees freedom for crit val method
          p(sprintf("where")),
          div(
            style = "margin-left: 20px;",
            printDegreesFreedom(df1, df2)
          ),
          sprintf(
            "Since the test statistic \\((F)\\) falls within the %s region, %s \\( H_0 \\).",
            text$region, text$rejectWord
          ),
          br(), br(), br(),

          ## conclusion
          p(strong("Conclusion:")),
          sprintf(
            "At \\(\\alpha = %.2f\\), since the test statistic falls within the %s region, we %s \\(H_0\\)
                  and conclude that there %s enough statistical evidence to support that \\(%s\\).",
            sig_lvl, text$region, text$rejectWord, text$isWord, hyp_labels$altHyp
          )
        )
      )
    })

observeEvent(input$goInference, {
  req(input$popuParameters == "Wilcoxon Signed Rank Test")

  rv$calculatePressed <- TRUE

  output$renderSignedRankUploadData <- renderUI({
    tagList(
      titlePanel("Data File"),
      br(),
      br(),
      div(DTOutput(session$ns("signedRankUploadTable")), style = "width: 75%"),
      br(),
      br()
    )
  })
})


observeEvent(input$signedRankQQPlot, {
  if (input$signedRankQQPlot) {
    showTab(inputId = "signedRankTabset", target = "Graphs")
  } else {
    if (input$signedRankTabset == "Graphs") {
      updateTabsetPanel(inputId = "signedRankTabset", selected = "Analysis")
    }
    hideTab(inputId = "signedRankTabset", target = "Graphs")
  }
})

## ---------- Wilcoxon Signed Rank Test Observers --------------------

    observeEvent(input$signedRankUpl, priority = 5, {
      rv$calculatePressed <- FALSE

      rv$allowColumnValidation <- FALSE

      hide(id = "signedRankUpl1")
      hide(id = "signedRankUpl2")
      fileInputs$signedRankStatus <- "uploaded"

      freezeReactiveValue(input, "signedRankUpl1")
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "signedRankUpl1",
        choices = c(""),
        selected = ""
      )

      freezeReactiveValue(input, "signedRankUpl2")
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "signedRankUpl2",
        choices = c(""),
        selected = ""
      )

      if (signedRankUpload_iv$is_valid()) {
        freezeReactiveValue(input, "signedRankUpl1")
        updateSelectInput(
          session = getDefaultReactiveDomain(),
          "signedRankUpl1",
          choices = c("", colnames(signedRankUploadData())),
          selected = ""
        )

        freezeReactiveValue(input, "signedRankUpl2")
        updateSelectInput(
          session = getDefaultReactiveDomain(),
          "signedRankUpl2",
          choices = c("", colnames(signedRankUploadData())),
          selected = ""
        )

        shinyjs::show(id = "signedRankUpl1")
        shinyjs::show(id = "signedRankUpl2")
      }

      Sys.sleep(0.1)
      rv$allowColumnValidation <- TRUE
    })


observeEvent(c(input$sidebysidewRankSum, input$sidebysidewRankQQ), {
  if (input$sidebysidewRankSum || input$sidebysidewRankQQ) {
    showTab(inputId = "wilcoxonRankSumTabset", target = "Graphs")
  } else {
    if (input$wilcoxonRankSumTabset == "Graphs") {
      updateTabsetPanel(inputId = "wilcoxonRankSumTabset", selected = "Analysis")
    }
    hideTab(inputId = "wilcoxonRankSumTabset", target = "Graphs")
  }
})

## ---------- Wilcoxon Rank Sum Observers --------------------
observeEvent(input$wilcoxonUpl, priority = 5, {
  hide(id = "wilcoxonUpl1")
  hide(id = "wilcoxonUpl2")
  fileInputs$rankSumStatus <- "uploaded"

  if (wilcoxonUpload_iv$is_valid()) {
    freezeReactiveValue(input, "wilcoxonUpl1")
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "wilcoxonUpl1",
      choices = c(colnames(WilcoxonUploadData()))
    )

    freezeReactiveValue(input, "wilcoxonUpl2")
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "wilcoxonUpl2",
      choices = c(colnames(WilcoxonUploadData()))
    )
    shinyjs::show(id = "wilcoxonUpl1")
    shinyjs::show(id = "wilcoxonUpl2")
  }
})


observeEvent(input$goInference, {
  output$renderDepPopMeansData <- renderUI({
    tagList(
      div(DTOutput(session$ns("depPopMeansUploadTable")), style = "width: 75%")
    )
  })
})


observeEvent(input$depMeansUserData, priority = 5, {
  hide(id = "depMeansUplSample1")
  hide(id = "depMeansUplSample2")
  fileInputs$depMeansStatus <- "uploaded"

  if (depmeansupload_iv$is_valid()) {
    freezeReactiveValue(input, "depMeansUplSample1")
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "depMeansUplSample1",
      choices = c(colnames(DepMeansUploadData()))
    )

    freezeReactiveValue(input, "depMeansUplSample2")
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "depMeansUplSample2",
      choices = c(colnames(DepMeansUploadData()))
    )
    shinyjs::show(id = "depMeansUplSample1")
    shinyjs::show(id = "depMeansUplSample2")
  }
})


observeEvent(input$depMeansQQPlot, {
  if (input$depMeansQQPlot) {
    showTab(inputId = "depPopMeansTabset", target = "Graphs")
  } else {
    if (input$depPopMeansTabset == "Graphs") {
      updateTabsetPanel(inputId = "depPopMeansTabset", selected = "Analysis")
    }
    hideTab(inputId = "depPopMeansTabset", target = "Graphs")
  }
})

observeEvent(input$indMeansUserData, priority = 5, {
      hide(id = "indMeansUplSample1")
      hide(id = "indMeansUplSample2")
      fileInputs$indMeansStatus <- "uploaded"
      ## if(onemeanupload_iv$is_valid())
      ## {
      freezeReactiveValue(input, "indMeansUplSample1")
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "indMeansUplSample1",
        choices = c(colnames(IndMeansUploadData()))
      )

      freezeReactiveValue(input, "indMeansUplSample2")
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "indMeansUplSample2",
        choices = c(colnames(IndMeansUploadData()))
      )
      shinyjs::show(id = "indMeansUplSample1")
      shinyjs::show(id = "indMeansUplSample2")
      ## }
    })

    observeEvent(input$goInference, {
      output$renderIndPopMeansData <- renderUI({
        tagList(
          div(DTOutput(session$ns("indPopMeansUploadTable")), style = "width: 75%")
        )
      })
    })

    observeEvent(c(input$indMeansBoxplot, input$indMeansQQPlot), {
      if (input$indMeansBoxplot || input$indMeansQQPlot) {
        showTab(inputId = "indPopMeansTabset", target = "Graphs")
      } else {
        if (input$indPopMeansTabset == "Graphs") {
          updateTabsetPanel(inputId = "indPopMeansTabset", selected = "Analysis")
        }
        hideTab(inputId = "indPopMeansTabset", target = "Graphs")
      }
    })

observeEvent(input$calculate, {
  if (input$popuParameters == "Population Proportions") {
    req(!is.na(input$numSuccesses1) && !is.na(input$numTrials1))
    req(!is.na(input$numSuccesses2) && !is.na(input$numTrials2))

    if (input$numSuccesses1 > input$numTrials1 || input$numSuccesses2 > input$numTrials2) {
      print("amde it")
    }
  } else if (input$popuParameters == "Independent Population Means") {
    output$renderIndMeansBoxplot <- renderUI({
      plotOutput(
        session$ns("indMeansBoxplot"),
        height = GetPlotHeight(
          input[["indMeansBoxplot-Height"]],
          input[["indMeansBoxplot-HeightPx"]],
          ui = TRUE
        ),
        width = GetPlotWidth(
          input[["indMeansBoxplot-Width"]],
          input[["indMeansBoxplot-WidthPx"]],
          ui = TRUE
        )
      )
    })
    output$renderIndMeansQQPlot <- renderUI({
      plotOutput(
        session$ns("indMeansQQPlot"),
        height = GetPlotHeight(
          input[["indMeansQQPlot-Height"]],
          input[["indMeansQQPlot-HeightPx"]],
          ui = TRUE
        ),
        width = GetPlotWidth(
          input[["indMeansQQPlot-Width"]],
          input[["indMeansQQPlot-WidthPx"]],
          ui = TRUE
        )
      )
    })
  } else if (input$popuParameters == "Wilcoxon rank sum test") {
    ## Hide tabs if deeper validation fails (not caught by si_iv)
    rank_data <- tryCatch(wilcoxonRankedData(), error = function(e) NULL)
    hide_due_to_invalid <- FALSE

    if (!is.null(rank_data)) {
      combined_vals <- rank_data$Value
      if (length(unique(combined_vals)) <= 1) {
        hide_due_to_invalid <- TRUE
      } else if (!is.null(input$normaprowrsRankSum) &&
                 input$normaprowrsRankSum == "Normal approximation (for large samples)") {
        n1_chk <- sum(rank_data$Group == "Sample 1")
        n2_chk <- sum(rank_data$Group == "Sample 2")
        nAll_chk <- nrow(rank_data)
        tie_counts_chk <- table(combined_vals)
        tie_corr_chk <- sum(sapply(tie_counts_chk, function(t) if (t > 1) t^3 - t else 0))
        u_std_dev_chk <- sqrt((n1_chk * n2_chk / 12) *
                              ((nAll_chk + 1) - (tie_corr_chk / (nAll_chk * (nAll_chk - 1)))))
        if (is.na(u_std_dev_chk) || u_std_dev_chk <= 0) {
          hide_due_to_invalid <- TRUE
        }
      }
    }

    ## if (hide_due_to_invalid) {
    ## }

    output$renderSidebysidewRankSum <- renderUI({
      plotOutput(
        session$ns("sidebysidewRankSum"),
        height = GetPlotHeight(
          input[["sidebysidewRankSum-Height"]],
          input[["sidebysidewRankSum-HeightPx"]],
          ui = TRUE
        ),
        width = GetPlotWidth(
          input[["sidebysidewRankSum-Width"]],
          input[["sidebysidewRankSum-WidthPx"]],
          ui = TRUE
        )
      )
    })
  } else if (input$popuParameters == "Dependent Population Means") {
    output$depMeansTable <- renderUI({
      DTOutput(session$ns("depMeansData"))
    })
    output$renderDepMeansQQPlot <- renderUI({
      plotOutput(
        session$ns("depMeansQQPlot"),
        height = GetPlotHeight(
          input[["depMeansQQPlot-Height"]],
          input[["depMeansQQPlot-HeightPx"]],
          ui = TRUE
        ),
        width = GetPlotWidth(
          input[["depMeansQQPlot-Width"]],
          input[["depMeansQQPlot-WidthPx"]],
          ui = TRUE
        )
      )
    })
  }


  do.call(showResultTabs,
          switch(
            input$popuParameters,
            "Independent Population Means" = list(
              tabset = "indPopMeansTabset",
              tabs = c("Analysis", "Graphs")
            ),
            "Wilcoxon Signed Rank Test" = list(
              tabset = "signedRankTabset",
              tabs = c("Analysis", "Data with Ranks", "Graphs")
            ),
            "Dependent Population Means" = list(
              tabset = "depPopMeansTabset",
              tabs = c("Analysis", "Data with Calculations", "Graphs")
            ),
            "Wilcoxon rank sum test" = list(
              tabset = "wilcoxonRankSumTabset",
              tabs = c("Analysis", "Data with Ranks", "Graphs")
            )
          ))

})


      wilcoxonZTestPlot <- function(testStatistic, critValue, altHypothesis) {
        x_bound <- max(4, abs(testStatistic) * 1.15)
        x <- round(seq(from = -x_bound, to = x_bound, by = 0.1), 2)

        if (altHypothesis == "two.sided") {
          CVs <- c(-critValue, critValue)
          RRLabels <- c(-2.5, 2.5)
        } else {
          CVs <- c(critValue)
          if (altHypothesis == "less") {
            RRLabels <- c(-2.5)
          } else {
            RRLabels <- c(2.5)
          }
        }

        xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))

        df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
        cvDF <- filter(df, x %in% CVs)
        RRLabelsDF <- filter(df, x %in% RRLabels)
        tsDF <- filter(df, x %in% testStatistic)
        centerDF <- filter(df, x %in% c(0))

        htPlot <- ggplot(df, aes(x = x, y = y)) +
          geom_hline(yintercept = 0, color = "black", linewidth = 0.25)

        if (altHypothesis == "two.sided") {
          htPlot <- htPlot +
            geom_area(
              data = subset(df, x <= -critValue),
              aes(y = y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4
            ) +
            geom_area(
              data = subset(df, x >= critValue),
              aes(y = y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4
            )
        } else if (altHypothesis == "less") {
          htPlot <- htPlot +
            geom_area(
              data = subset(df, x <= critValue),
              aes(y = y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4
            )
        } else if (altHypothesis == "greater") {
          htPlot <- htPlot +
            geom_area(
              data = subset(df, x >= critValue),
              aes(y = y),
              fill = "#023B70",
              color = NA,
              alpha = 0.4
            )
        }

        htPlot <- htPlot +
          stat_function(
            fun = dnorm,
            geom = "line",
            linewidth = 0.75
          ) +
          theme_void() +
          scale_y_continuous(breaks = NULL) +
          ylab("") +
          xlab("Z") +
          geom_segment(
            data = centerDF,
            aes(x = x, xend = x, y = 0, yend = y),
            linetype = "dotted",
            linewidth = 0.75,
            color = "black"
          ) +
          geom_text(
            data = centerDF,
            aes(x = x, y = y / 2, label = "AR"),
            size = 16 / .pt,
            fontface = "bold"
          ) +
          geom_text(
            data = centerDF,
            aes(x = x, y = 0, label = "0"),
            size = 14 / .pt,
            fontface = "bold",
            nudge_y = -.03
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
            aes(x = x, y = y, label = round(x, 3)),
            size = 14 / .pt,
            fontface = "bold",
            nudge_y = .075
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
            aes(x = x, y = 0, label = round(x, 3)),
            size = 14 / .pt,
            fontface = "bold",
            nudge_y = -.03
          ) +
          geom_text(
            data = RRLabelsDF,
            aes(x = x, y = y, label = "RR"),
            size = 16 / .pt,
            fontface = "bold",
            nudge_y = .025
          ) +
          theme(axis.title.x = element_text(size = 16, face = "bold.italic")) +
          coord_cartesian(clip = "off")

        return(htPlot)
      }

printFStat <- function(sd1, sd2, F_statistic, is_variance, is_HT = FALSE) {
  if (!is_variance) {
    p(sprintf("\\(%s\\dfrac{s_1^2}{s_2^2} = \\dfrac{%.4f^2}{%.4f^2} = %.4f \\)", if (is_HT) "F = " else "", sd1, sd2, F_statistic))
  } else {
    p(sprintf("\\(%s\\dfrac{s_1^2}{s_2^2} = \\dfrac{%.4f}{%.4f} = %.4f \\)", if (is_HT) "F = " else "", sd1, sd2, F_statistic))
  }
}


twoPopVarOutputText <- function(HT, sig_lvl) {
  if (!HT$reject_null) {
    region <- "acceptance"
    isWord <- "isn't"
  } else {
    region <- "rejection"
    isWord <- "is"
  }
  rejectWord <- if (HT$p_value <= sig_lvl) "reject" else "do not reject"
  pValSign <- if (HT$p_value <= sig_lvl) "\\leq" else ">"

  return(list(
    region = region,
    isWord = isWord,
    rejectWord = rejectWord,
    pValSign = pValSign
  ))
}


printFTestPVal <- function(pValue, testStatVal, alternative, pValSign, rejectWord) {
  if (pValue < 0.0001) {
    pValueFormatted <- "P \\lt 0.0001"
  } else {
    pValueFormatted <- sprintf("%.4f", pValue)
  }

  if (alternative == "two.sided") {
    pvalCalc <- sprintf("2 \\times P(F \\lt %.4f)", testStatVal)
  } else if (alternative == "greater") {
    pvalCalc <- sprintf("P(F \\gt %.4f)", testStatVal)
  } else {
    pvalCalc <- sprintf("P(F \\lt %.4f)", testStatVal)
  }

  tagList(
    p(tags$b("Using P-Value Method:")),
    sprintf("\\(P = %s = %s\\)", pvalCalc, pValueFormatted),
    br(), br(),
    sprintf(
      "Since \\( P %s %.2f \\), %s \\(H_0\\).",
      pValSign,
      SigLvl(),
      rejectWord
    ),
    br(), br(), br()
  )
}


printDegreesFreedom <- function(df1, df2) {
  n1 <- df1 + 1
  n2 <- df2 + 1

  list(
    p(sprintf("\\(df_1 = n_1 - 1 = %d - 1 = %d\\)", n1, df1)),
    p(sprintf("\\(df_2 = n_2 - 1 = %d - 1 = %d\\)", n2, df2)),
    br()
  )
}


printTwoPopVarGivens <- function(data, is_variance) {
  if (is_variance) {
    tagList(
      sprintf("\\(n_1 = %d\\)", data$n1),
      br(),
      sprintf("\\(s_1^2 = %.4f\\)", data$sd1),
      br(),
      sprintf("\\(n_2 = %d\\)", data$n2),
      br(),
      sprintf("\\(s_2^2 = %.4f\\)", data$sd2),
      br(),
      br()
    )
  } else {
    tagList(
      sprintf("\\(n_1 = %d\\)", data$n1),
      br(),
      sprintf("\\(s_1 = %.4f\\)", data$sd1),
      br(),
      sprintf("\\(n_2 = %d\\)", data$n2),
      br(),
      sprintf("\\(s_2 = %.4f\\)", data$sd2),
      br(),
      br()
    )
  }
}


TwoPopVarHT <- function(n1, sd1, n2, sd2, sig_lvl, alt_hyp = "two.sided", is_variance) {
        df1 <- n1 - 1
        df2 <- n2 - 1
        crit_lower <- 0
        crit_upper <- 0
        crit_val <- 0

        if (is_variance) {
          var1 <- sd1
          var2 <- sd2
        } else { ## else summary, so sd's need to be ^2
          var1 <- sd1^2
          var2 <- sd2^2
        }

        F_stat <- var1 / var2

        if (alt_hyp == "greater") {
          p_value <- pf(F_stat, df1, df2, lower.tail = FALSE)
          crit_val <- qf(1 - sig_lvl, df1, df2)
          reject <- F_stat > crit_val
        } else if (alt_hyp == "less") {
          p_value <- pf(F_stat, df1, df2, lower.tail = TRUE)
          crit_val <- qf(sig_lvl, df1, df2)
          reject <- F_stat < crit_val
        } else { ## two sided
          if (F_stat > 1) {
            p_value <- 2 * pf(F_stat, df1, df2, lower.tail = FALSE)
          } else {
            p_value <- 2 * pf(F_stat, df1, df2, lower.tail = TRUE)
          }
          crit_lower <- qf(sig_lvl / 2, df1, df2)
          crit_upper <- qf(1 - sig_lvl / 2, df1, df2)
          reject <- F_stat < crit_lower || F_stat > crit_upper
        }

        return(list(
          F_statistic = F_stat,
          p_value = p_value,
          reject_null = reject,
          crit_upper = crit_upper,
          crit_lower = crit_lower,
          crit_val = crit_val
        ))
      }


TwoPopVarCI <- function(n1, sd1, n2, sd2, conf_level = 0.95, is_variance) {
  df1 <- n1 - 1
  df2 <- n2 - 1

  alpha <- 1 - conf_level

  if (is_variance) {
    var1 <- sd1
    var2 <- sd2
  } else { ## else summary, so sd's need to be ^2
    var1 <- sd1^2
    var2 <- sd2^2
  }
  F_stat <- var1 / var2

  F_critical_lower <- qf(alpha / 2, df2, df1)
  F_critical_upper <- qf(1 - alpha / 2, df2, df1)

  CI_lower <- F_stat * F_critical_lower
  CI_upper <- F_stat * F_critical_upper

  return(list(
    CI_lower = CI_lower,
    CI_upper = CI_upper,
    F_lower = F_critical_lower,
    F_upper = F_critical_upper,
    F_statistic = F_stat
  ))
}


GetSignedRankMeansData <- function() {
  req(si_iv$is_valid())

  dat <- list()

  if (input$signedRankTest == "Upload Data") {
    samp1 <- na.omit(as.numeric(unlist(signedRankUploadData()[, input$signedRankUpl1])))
    samp2 <- na.omit(as.numeric(unlist(signedRankUploadData()[, input$signedRankUpl2])))
  } else if (input$signedRankTest == "Enter Raw Data") {
    samp1 <- createNumLst(input$signedRankRaw1)
    samp2 <- createNumLst(input$signedRankRaw2)
  }

  dat$samp1 <- samp1
  dat$samp2 <- samp2
  dat$n1 <- length(samp1)
  dat$n2 <- length(samp2)
  dat$mean1 <- mean(samp1)
  dat$mean2 <- mean(samp2)

  return(dat)
}


GetwRankSumMeansData <- function() {
  req(si_iv$is_valid())

  dat <- list()

  if (input$wilcoxonRankSumTestData == "Upload Data") {
    samp1 <- na.omit(as.numeric(unlist(WilcoxonUploadData()[, input$wilcoxonUpl1])))
    samp2 <- na.omit(as.numeric(unlist(WilcoxonUploadData()[, input$wilcoxonUpl2])))
  } else if (input$wilcoxonRankSumTestData == "Enter Raw Data") {
    samp1 <- createNumLst(input$rankSumRaw1)
    samp2 <- createNumLst(input$rankSumRaw2)
  }

  dat$samp1 <- samp1
  dat$samp2 <- samp2
  dat$n1 <- length(samp1)
  dat$n2 <- length(samp2)
  dat$mean1 <- mean(samp1)
  dat$mean2 <- mean(samp2)

  return(dat)
}


GetDepMeansData <- function() {
### JB note: this req caused a bunch of errors for input validation on Mu Naught in Dep Means hypothesis testing
### leaving it commented out for now
        ## req(si_iv$is_valid())

        dat <- list()

        if (input$dataTypeDependent == "Upload Data") {
          req(input$depMeansUplSample1, input$depMeansUplSample2)
          sampBefore <- na.omit(unlist(DepMeansUploadData()[, input$depMeansUplSample1]))
          sampAfter <- na.omit(unlist(DepMeansUploadData()[, input$depMeansUplSample2]))
        } else if (input$dataTypeDependent == "Enter Raw Data") {
          sampBefore <- createNumLst(input$before)
          sampAfter <- createNumLst(input$after)
        }

        dat$before <- sampBefore
        dat$after <- sampAfter
        dat$d <- (sampBefore - sampAfter)
        dat$n <- length(sampBefore)
        dat$dbar <- sum(dat$d) / dat$n
        dat$sd <- sqrt(sum((dat$d - dat$dbar)^2) / (dat$n - 1))
        dat$muNaught <- input$depMeansMuNaught

        return(dat)
      }


showSummaryTable <- function() {
  showTable <-
    (input$dataAvailability2 == "Enter Raw Data" && input$bothsigmaKnownRaw == "bothUnknown") ||
    (input$dataAvailability2 == "Upload Data" && input$bothsigmaKnownUpload == "bothUnknown")
  return(showTable)
}


PrintIndMeansSummaryTable <- function(data) {
        df <- data.frame(
          "Sample Size" = c(data$n1, data$n2),
          "Sample Mean" = c(data$xbar1, data$xbar2),
          "Sample Standard Deviation" = c(data$sd1, data$sd2),
          "Sample Variance" = c(data$sd1^2, data$sd2^2),
          row.names = c("Sample 1", "Sample 2"),
          check.names = FALSE
        )

        colNames <- c("Sample Size", "Sample Mean", "Sample Standard Deviation", "Sample Variance")

        headers <- htmltools::withTags(table(
                                class = "display",
                                style = "max-width: 600px; table-layout: fixed; width: 100%;",
                                thead(
                                  tr(
                                    th("",
                                       style = "border: 1px solid rgba(0, 0, 0, 0.15);
                    border-bottom: 1px solid rgba(0, 0, 0, 0.3);"
                    ),
                    lapply(colNames, th,
                           style = "border-right: 1px solid rgba(0, 0, 0, 0.15);
                        border-top: 1px solid rgba(0, 0, 0, 0.15);"
                        )
                    )
                    )
                    ))

        datatable(df,
                  class = "cell-border stripe",
                  container = headers,
                  options = list(
                    dom = "t",
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE,
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    columnDefs = list(
                      list(className = "dt-center", targets = 0:4),
                      list(width = "150px", targets = 0:4)
                    )
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none"
                  ) %>%
          formatRound(columns = 1, digits = 0) %>%
          formatRound(columns = 2:4, digits = 4) %>%
          formatStyle(columns = 0, fontWeight = "bold")
      }
