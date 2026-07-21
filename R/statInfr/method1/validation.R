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
## └── populationStandardDeviation
##
## NOTE: leaf nodes of the tree are created, conditioned, and populated with
## rules before their parent node is created, conditioned, given children,
## and populated with rules. The root iv is created first, however, to
## signal the beginning of the input validation seciton of code. Conversely,
## we end with enabling iv which also enables all its children and
## grandchildren and so on.
##
## onemean_iv <- InputValidator$new()
## onemeansdknown_iv <- InputValidator$new()
## onemeansdunk_iv <- InputValidator$new()
## onemeanraw_iv <- InputValidator$new()
## onemeanht_iv <- InputValidator$new()
## onemeanupload_iv <- InputValidator$new()
## onemeanuploadvar_iv <- InputValidator$new()
## onemeanuploadsd_iv <- InputValidator$new()

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
populationMeanEnterRawData_iv$add_rule("popuSDRaw", ~ {
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
iv$add_validator(populationProportion_iv, "Population Proportion IV")



populationStandardDeviation_iv <- InputValidator$new()
populationStandardDeviation_iv$condition(function() {
  input$popuParameter == "Population Standard Deviation"
})
iv$add_validator(populationStandardDeviation_iv, "Population Standard Deviation IV")



hypothesisTesting_iv <- InputValidator$new()
hypothesisTesting_iv$condition(function() {
  input$inferenceType == "Hypothesis Testing"
})
iv$add_validator(hypothesisTesting_iv, "Population Hypothesis Testing IV")



## TODO: input validation for the file upload must be reworked.
## onemeanupload_iv$add_rule("upload", sv_required())
## onemeanupload_iv |> add_rule_accepted_file_formats("upload")
## onemeanupload_iv$add_rule("upload", ~ if (nrow(Upload()) == 0) "File is empty")
## onemeanupload_iv$add_rule("upload", ~ if (nrow(Upload()) < 3) "Samples must include at least 2 observations")


## popuSD
## onemeansdknown_iv |>
##   add_rules("popuSD", sv_required(), sv_gt(0))
## onemeansdkunk_iv |>
##   add_rules("popuSD", sv_required(), sv_gt(0))

## popuSDUpload
## onemeanuploadsd_iv$add_rule("popuSDUpload", sv_required())
## onemeanuploadsd_iv$add_rule("popuSDUpload", sv_gt(0))

## selectUploadVariable
## onemeanuploadvar_iv$add_rule("selectUploadVariable", sv_required())
## onemeanuploadvar_iv$add_rule("selectUploadVariable", ~ {
##   if (checkNumeric(Upload(), input$selectUploadVariable)) {
##     "Selected column contains non-numeric data."
##   }
## })
## onemeanuploadvar_iv$add_rule("selectUploadVariable", ~ {
##   if (!(input$selectUploadVariable %in% names(Upload()))) {
##     return(NULL)
##   }
##   dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
##   if (length(dat) < 2) "Samples must include at least 2 observations"
## })
## onemeanuploadvar_iv$add_rule("selectUploadVariable", ~ {
##   if (!(input$selectUploadVariable %in% names(Upload()))) {
##     return(NULL)
##   }
##   dat <- na.omit(unlist(Upload()[, input$selectUploadVariable]))
##   if (input$sigmaKnown && input$inferenceType == "Hypothesis Testing" && length(dat) > 1 && sd(dat) == 0) {
##     "No variance in selected column"
##   }
## })


## sampSD
## onemeansdunk_iv$add_rule("sampSD", sv_required())
## onemeansdunk_iv$add_rule("sampSD", sv_gt(0))

iv$enable()
