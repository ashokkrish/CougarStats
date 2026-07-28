iv <- InputValidator$new()
## NOTE: each input validator in the tree of validators is prefixed with its
## parents name (excepting a prefix of iv, as this is the conventional
## suffix).
##
## iv
## ├── hypothesisTesting
## ├── populationMean
## │   ├── EnterRawData
## │   ├── SummarizedData
## │   └── UploadData
## ├── populationProportion
## ├── populationStandardDeviation
## └── sampleStandardDeviation
##
## NOTE: leaf nodes of the tree are created, conditioned, and populated with
## rules before their parent node is created, conditioned, given children,
## and populated with rules. The root iv is created first, however, to
## signal the beginning of the input validation seciton of code. Conversely,
## we end with enabling iv which also enables all its children and
## grandchildren and so on.

populationMeanSummarizedData_iv <- InputValidator$new()
populationMeanSummarizedData_iv$condition(function() {
  input$dataAvailability == "Summarized Data"
})
add_rules(populationMeanSummarizedData_iv, "sampleSize", sv_required(), sv_integer(), sv_gt(1))
add_rules(populationMeanSummarizedData_iv, "sampleMean", sv_required(), sv_numeric())


populationMeanEnterRawData_iv <- InputValidator$new()
populationMeanEnterRawData_iv$condition(function() {
  input$dataAvailability == "Enter Raw Data"
})
add_rules(populationMeanEnterRawData_iv,
          "sample1",
          sv_required(),
          sv_regex(
            "^( )*(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
            "Data must be numeric values separated by a comma (ie: 2,3,4)"
          ))
populationMeanEnterRawData_iv$add_rule("sample1", ~ {
  if (input$sigma == "rawkUknown" && input$inferenceType == "Hypothesis Testing" && (0 == sd(createNumLst(.)))) {
    "No variance in sample data"
  } else {
    NULL
  }
})
populationMeanEnterRawData_iv$add_rule("populationStandardDeviation", ~ {
  if (input$sigmaKnown) {
    sv_required()(.) %||% sv_gt(0)(.)
  } else {
    NULL
  }
})


populationMeanUploadData_iv <- InputValidator$new()
populationMeanUploadData_iv$condition(function() {
  input$dataAvailability == "Upload Data"
})
populationMeanUploadData_iv$add_rule("upload", sv_required())
populationMeanUploadData_iv |> add_rule_accepted_file_formats("upload")
populationMeanUploadData_iv$add_rule("upload", function(value) if (file.size(value$datapath) == 0) "File is empty")
populationMeanUploadData_iv$add_rule("upload", function(value) if (nrow(Upload()) == 0) "Table has no rows")
populationMeanUploadData_iv$add_rule("upload", function(value) if (nrow(Upload()) < 2) "Table has less than two rows")
populationMeanUploadData_iv$add_rule("uploadVariable", sv_required())
populationMeanUploadData_iv$add_rule("uploadVariable", ~ {
  if (checkNumeric(Upload(), .)) {
    "Selected column contains non-numeric data."
  }
})
populationMeanUploadData_iv$add_rule("uploadVariable", ~ {
  if (!(. %in% names(Upload()))) {
    return(NULL)
  }
  dat <- na.omit(unlist(Upload()[, .]))
  if (length(dat) < 2) "Samples must include at least 2 observations"
})
populationMeanUploadData_iv$add_rule("uploadVariable", ~ {
  if (!(. %in% names(Upload()))) {
    return(NULL)
  }
  dat <- na.omit(unlist(Upload()[, .]))
  if (input$sigmaKnown && input$inferenceType == "Hypothesis Testing" && length(dat) > 1 && sd(dat) == 0) {
    "No variance in selected column"
  }
})


populationMean_iv <- InputValidator$new()
populationMean_iv$condition(function() {
  input$popuParameter == "Population Mean"
})
populationMean_iv$add_validator(populationMeanSummarizedData_iv)
populationMean_iv$add_validator(populationMeanEnterRawData_iv)
populationMean_iv$add_validator(populationMeanUploadData_iv)
iv$add_validator(populationMean_iv, "Population Mean IV")


populationProportion_iv <- InputValidator$new()
populationProportion_iv$condition(function() {
  input$popuParameter == "Population Proportion"
})
populationProportion_iv$add_rule("numSuccesses", sv_required())
populationProportion_iv$add_rule("numSuccesses", sv_integer())
populationProportion_iv$add_rule("numSuccesses", sv_gt(-1))
populationProportion_iv$add_rule("numTrials", sv_required())
populationProportion_iv$add_rule("numTrials", sv_integer())
populationProportion_iv$add_rule("numTrials", sv_gt(0))
iv$add_validator(populationProportion_iv, "Population Proportion IV")


populationStandardDeviation_iv <- InputValidator$new()
populationStandardDeviation_iv$condition(function() input$popuParameter == "Population Standard Deviation")
populationStandardDeviation_iv$add_rule("SSDSampleSize", sv_required())
populationStandardDeviation_iv$add_rule("SSDSampleSize", sv_integer())
populationStandardDeviation_iv$add_rule("SSDSampleSize", sv_gt(1))
populationStandardDeviation_iv$add_rule("SSDStdDev", sv_required())
populationStandardDeviation_iv$add_rule("SSDStdDev", sv_numeric())
populationStandardDeviation_iv$add_rule("SSDStdDev", sv_gt(0))
iv$add_validator(populationStandardDeviation_iv, "Population Standard Deviation IV")


populationStandardDeviationCheckbox_iv <- InputValidator$new()
populationStandardDeviationCheckbox_iv$condition(function() input$sigmaKnown)
populationStandardDeviationCheckbox_iv$add_rule("populationStandardDeviation", sv_required())
populationStandardDeviationCheckbox_iv$add_rule("populationStandardDeviation", sv_numeric())
populationStandardDeviationCheckbox_iv$add_rule("populationStandardDeviation", sv_gt(0))
iv$add_validator(populationStandardDeviationCheckbox_iv, "Population Standard Deviation Checkbox Input IV")


hypothesisTesting_iv <- InputValidator$new()
hypothesisTesting_iv$condition(function() {
  input$inferenceType == "Hypothesis Testing"
})
hypothesisTestingPopulationMean_iv <- InputValidator$new()
hypothesisTestingPopulationMean_iv$condition(function() input$popuParameter == "Population Mean")
hypothesisTestingPopulationMean_iv$add_rule("hypMean", sv_required())
hypothesisTestingPopulationMean_iv$add_rule("hypMean", sv_numeric())
hypothesisTestingPopulationProportion_iv <- InputValidator$new()
hypothesisTestingPopulationProportion_iv$condition(function() input$popuParameter == "Population Proportion")
hypothesisTestingPopulationProportion_iv$add_rule("hypProportion", sv_required())
hypothesisTestingPopulationProportion_iv$add_rule("hypProportion", sv_numeric())
hypothesisTestingPopulationProportion_iv$add_rule("hypProportion", sv_gte(0))
hypothesisTestingPopulationProportion_iv$add_rule("hypProportion", sv_lte(1))
hypothesisTestingPopulationStandardDeviation_iv <- InputValidator$new()
hypothesisTestingPopulationStandardDeviation_iv$condition(function() input$popuParameter == "Population Standard Deviation")
hypothesisTestingPopulationStandardDeviation_iv$add_rule("hypStdDeviation", sv_required())
hypothesisTestingPopulationStandardDeviation_iv$add_rule("hypStdDeviation", sv_numeric())
hypothesisTestingPopulationStandardDeviation_iv$add_rule("hypStdDeviation", sv_gte(0.001))
hypothesisTesting_iv$add_validator(hypothesisTestingPopulationMean_iv)
hypothesisTesting_iv$add_validator(hypothesisTestingPopulationStandardDeviation_iv)
hypothesisTesting_iv$add_validator(hypothesisTestingPopulationProportion_iv)
iv$add_validator(hypothesisTesting_iv, "Population Hypothesis Testing IV")


sampleStandardDeviation_iv <- InputValidator$new()
sampleStandardDeviation_iv$condition(function() {
  is.null(input$sigmaKnown) || (!(input$sigmaKnown) && input$popuParameter != "Population Proportion")
})
sampleStandardDeviation_iv$add_rule("sampleStandardDeviation", sv_required())
sampleStandardDeviation_iv$add_rule("sampleStandardDeviation", sv_numeric())
sampleStandardDeviation_iv$add_rule("sampleStandardDeviation", sv_gt(0))
iv$add_validator(sampleStandardDeviation_iv, "Sample Standard Deviation IV")


iv$enable()
