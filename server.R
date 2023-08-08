server <- function(input, output) {
  
  # ------------------------- #
  # ---- Data Validation ----
  # ------------------------- #
  
  iv <- InputValidator$new()
  ds_iv <- InputValidator$new()
  dsraw_iv <- InputValidator$new()
  dsupload_iv <- InputValidator$new()
  dsuploadvars_iv <- InputValidator$new()
  
  pd_iv <- InputValidator$new()
  binom_iv <- InputValidator$new()
  binomprob_iv <- InputValidator$new()
  binombetween_iv <- InputValidator$new()
  poiss_iv <- InputValidator$new()
  poissprob_iv <- InputValidator$new()
  poissbetween_iv <- InputValidator$new()
  norm_iv <- InputValidator$new()
  normprob_iv <- InputValidator$new()
  normbetween_iv <- InputValidator$new()
  
  si_iv <- InputValidator$new()
  onemean_iv <- InputValidator$new()
  onemeansdknown_iv <- InputValidator$new()
  onemeansdunk_iv <- InputValidator$new()
  onemeanraw_iv <- InputValidator$new()
  onemeanht_iv <- InputValidator$new()
  onemeanupload_iv <- InputValidator$new()
  onemeanuploadvar_iv <- InputValidator$new()
  onemeanuploadsd_iv <- InputValidator$new()
  indmeanssumm_iv <- InputValidator$new()
  indmeansraw_iv <- InputValidator$new()
  indmeansupload_iv <- InputValidator$new()
  indmeansuploadvar_iv <- InputValidator$new()
  indmeanssdknown_iv <- InputValidator$new()
  indmeanssdunk_iv <- InputValidator$new()
  indmeansrawsd_iv <- InputValidator$new()
  indmeansuploadsd_iv <- InputValidator$new()
  depmeansraw_iv <- InputValidator$new()
  depmeansupload_iv <- InputValidator$new()
  depmeansuploadvars_iv <- InputValidator$new()
  oneprop_iv <- InputValidator$new()
  onepropht_iv <- InputValidator$new()
  twoprop_iv <- InputValidator$new()
  
  regcor_iv <- InputValidator$new()
  slrraw_iv <- InputValidator$new()
  slrupload_iv <- InputValidator$new()
  slruploadvars_iv <- InputValidator$new()
  
  ## DS rules ----
  
  # descriptiveStat
  
  dsraw_iv$add_rule("descriptiveStat", sv_required())
  dsraw_iv$add_rule("descriptiveStat", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                                "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
  
  dsupload_iv$add_rule("dsUserData", sv_required())
  dsupload_iv$add_rule("dsUserData", ~ if(!(tools::file_ext(input$dsUserData$name) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  dsupload_iv$add_rule("dsUserData", ~ if(ncol(dsUploadData()) < 1) "Data must include one variable")
  dsupload_iv$add_rule("dsUserData", ~ if(nrow(dsUploadData()) < 2) "Samples must include at least 2 observations")
  
  dsuploadvars_iv$add_rule("dsUploadVars", sv_required())
  
  #ds_iv$add_rule("dsTableFilters", sv_required())
  
  
  # ------------------ #
  #     Conditions     #
  # ------------------ #
  ds_iv$condition(~ isTRUE(input$dropDownMenu == 'Descriptive Statistics'))
  dsraw_iv$condition(~ isTRUE(input$dataInput == 'Enter Raw Data'))
  dsupload_iv$condition(~ isTRUE(input$dataInput == 'Upload Data'))
  dsuploadvars_iv$condition(function() {isTRUE(input$dataInput == 'Upload Data' && 
                                               dsupload_iv$is_valid()) })
  
  
  # ------------------ #
  #     Dependency     #
  # ------------------ #
  ds_iv$add_validator(dsraw_iv)
  ds_iv$add_validator(dsupload_iv)
  ds_iv$add_validator(dsuploadvars_iv)
  
  
  # ------------------ #
  #     Activation     #
  # ------------------ #
  ds_iv$enable()
  dsraw_iv$enable()
  dsupload_iv$enable()
  dsuploadvars_iv$enable()
  
  ## PD rules ----
  
  # numTrialsBinom 
  
  binom_iv$add_rule("numTrialsBinom", sv_required())
  binom_iv$add_rule("numTrialsBinom", sv_integer())
  binom_iv$add_rule("numTrialsBinom", sv_gt(0))
  
  # successProbBinom (PD)
  
  binom_iv$add_rule("successProbBinom", sv_required())
  binom_iv$add_rule("successProbBinom", sv_gte(0))
  binom_iv$add_rule("successProbBinom", sv_lte(1))
  
  # numSuccessesBinom (PD)
  
  binomprob_iv$add_rule("numSuccessesBinom", sv_required())
  binomprob_iv$add_rule("numSuccessesBinom", sv_integer())
  binomprob_iv$add_rule("numSuccessesBinom", sv_gte(0))
  
  # numSuccessesBinomx1 (PD)
  binombetween_iv$add_rule("numSuccessesBinomx1", sv_required())
  binombetween_iv$add_rule("numSuccessesBinomx1", sv_integer())
  binombetween_iv$add_rule("numSuccessesBinomx1", sv_gte(0))
  
  # numSuccessesBinomx2 (PD)
  binombetween_iv$add_rule("numSuccessesBinomx2", sv_required())
  binombetween_iv$add_rule("numSuccessesBinomx2", sv_integer())
  binombetween_iv$add_rule("numSuccessesBinomx2", sv_gte(0))
  
  # muPoisson (PD)
  
  poiss_iv$add_rule("muPoisson", sv_required())
  poiss_iv$add_rule("muPoisson", sv_gt(0))
  
  # xPoisson (PD)
  
  poissprob_iv$add_rule("xPoisson", sv_required())
  poissprob_iv$add_rule("xPoisson", sv_integer())
  poissprob_iv$add_rule("xPoisson", sv_gte(0))
  
  # x1Poisson (PD)
  poissbetween_iv$add_rule("x1Poisson", sv_required())
  poissbetween_iv$add_rule("x1Poisson", sv_integer())
  poissbetween_iv$add_rule("x1Poisson", sv_gte(0))
  
  # x2Poisson (PD)
  poissbetween_iv$add_rule("x2Poisson", sv_required())
  poissbetween_iv$add_rule("x2Poisson", sv_integer())
  poissbetween_iv$add_rule("x2Poisson", sv_gte(0))
  
  # popMean (PD)
  
  norm_iv$add_rule("popMean", sv_required())
  
  # popuSD (PD)
  
  norm_iv$add_rule("popSD", sv_required())
  norm_iv$add_rule("popSD", sv_gt(0))
  
  # xValue (PD)
  
  normprob_iv$add_rule("xValue", sv_required())
  
  normbetween_iv$add_rule("x1Value", sv_required())
  normbetween_iv$add_rule("x2Value", sv_required())
  
  # ------------------ #
  #     Conditions     #
  # ------------------ #
  binom_iv$condition(~ isTRUE(input$probability == 'Binomial'))
  
  binomprob_iv$condition(~ isTRUE(input$probability == 'Binomial' && 
                                  input$calcBinom != 'between'))
  
  binombetween_iv$condition(~ isTRUE(input$probability == 'Binomial' && 
                                     input$calcBinom == 'between'))
  
  poiss_iv$condition(~ isTRUE(input$probability == 'Poisson'))
  
  poissprob_iv$condition(~ isTRUE(input$probability == 'Poisson' && 
                                  input$calcPoisson != 'between'))
  
  poissbetween_iv$condition(~ isTRUE(input$probability == 'Poisson' && 
                                     input$calcPoisson == 'between'))
  
  norm_iv$condition(~ isTRUE(input$probability == 'Normal'))
  
  normprob_iv$condition(~ isTRUE(input$probability == 'Normal' && 
                                 input$calcNormal != 'between'))
  
  normbetween_iv$condition(~ isTRUE(input$probability == 'Normal' && 
                                    input$calcNormal == 'between'))
  # ------------------ #
  #     Dependency     #
  # ------------------ #
  binom_iv$add_validator(binomprob_iv)
  binom_iv$add_validator(binombetween_iv)
  
  poiss_iv$add_validator(poissprob_iv)
  poiss_iv$add_validator(poissbetween_iv)
  
  norm_iv$add_validator(normprob_iv)
  norm_iv$add_validator(normbetween_iv)
  
  pd_iv$add_validator(binom_iv)
  pd_iv$add_validator(poiss_iv)
  pd_iv$add_validator(norm_iv)
  
  # ------------------ #
  #     Activation     #
  # ------------------ #
  pd_iv$enable()
  binom_iv$enable()
  binomprob_iv$enable()
  binombetween_iv$enable()
  poiss_iv$enable()
  poissprob_iv$enable()
  poissbetween_iv$enable()
  norm_iv$enable()
  normprob_iv$enable()
  normbetween_iv$enable()
  
  #--------------- #
  ## SI rules ----
  #--------------- #
  
  # sampleSize 
  
  onemean_iv$add_rule("sampleSize", sv_required())
  onemean_iv$add_rule("sampleSize", sv_integer())
  onemean_iv$add_rule("sampleSize", sv_gt(1))
  
  # sampleMean 
  
  onemean_iv$add_rule("sampleMean", sv_required())
  
  # sample1 
  
  onemeanraw_iv$add_rule("sample1", sv_required())
  onemeanraw_iv$add_rule("sample1", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                             "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
  
  # One Mean Upload Data
  onemeanupload_iv$add_rule("oneMeanUserData", sv_required())
  onemeanupload_iv$add_rule("oneMeanUserData", ~ if(!(tools::file_ext(input$oneMeanUserData$name) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  onemeanupload_iv$add_rule("oneMeanUserData", ~ if(nrow(OneMeanUploadData()) == 0) "File is empty")
  onemeanupload_iv$add_rule("oneMeanUserData", ~ if(nrow(OneMeanUploadData()) < 3) "Samples must include at least 2 observations")
  
  # popuSD 
  
  onemeansdknown_iv$add_rule("popuSD", sv_required()) 
  onemeansdknown_iv$add_rule("popuSD", sv_gt(0))
  
  # popuSDRaw 
  
  onemeanraw_iv$add_rule("popuSDRaw", sv_required()) 
  onemeanraw_iv$add_rule("popuSDRaw", sv_gt(0))
  
  # popuSDUpload
  
  onemeanuploadsd_iv$add_rule("popuSDUpload", sv_required()) 
  onemeanuploadsd_iv$add_rule("popuSDUpload", sv_gt(0))
  
  # oneMeanVariable
  
  onemeanuploadvar_iv$add_rule("oneMeanVariable", sv_required())
  
  # sampSD 
  
  onemeansdunk_iv$add_rule("sampSD", sv_required())
  onemeansdunk_iv$add_rule("sampSD", sv_gt(0))
  
  # sampleSize1 
  
  indmeanssumm_iv$add_rule("sampleSize1", sv_required())
  indmeanssumm_iv$add_rule("sampleSize1", sv_integer())
  indmeanssumm_iv$add_rule("sampleSize1", sv_gt(1))
  
  # sampleMean1 
  
  indmeanssumm_iv$add_rule("sampleMean1", sv_required())
  
  # sampleSize2 
  
  indmeanssumm_iv$add_rule("sampleSize2", sv_required())
  indmeanssumm_iv$add_rule("sampleSize2", sv_integer())
  indmeanssumm_iv$add_rule("sampleSize2", sv_gt(1))
  
  # sampleMean2 
  
  indmeanssumm_iv$add_rule("sampleMean2", sv_required()) 
  
  # popuSD1 
  
  indmeanssdknown_iv$add_rule("popuSD1", sv_required()) 
  indmeanssdknown_iv$add_rule("popuSD1", sv_gt(0))
  
  # popuSD2 
  
  indmeanssdknown_iv$add_rule("popuSD2", sv_required()) 
  indmeanssdknown_iv$add_rule("popuSD2", sv_gt(0))
  
  # sampSD1 
  
  indmeanssdunk_iv$add_rule("sampSD1", sv_required())
  indmeanssdunk_iv$add_rule("sampSD1", sv_gt(0))
  
  # sampSD2 
  
  indmeanssdunk_iv$add_rule("sampSD2", sv_required()) 
  indmeanssdunk_iv$add_rule("sampSD2", sv_gt(0))
  
  # raw_sample1
  
  indmeansraw_iv$add_rule("raw_sample1", sv_required())
  indmeansraw_iv$add_rule("raw_sample1", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)"))
  
  # raw_sample2 
  
  indmeansraw_iv$add_rule("raw_sample2", sv_required())
  indmeansraw_iv$add_rule("raw_sample2", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)."))
  
  indmeansrawsd_iv$add_rule("popuSDRaw1", sv_required()) 
  indmeansrawsd_iv$add_rule("popuSDRaw1", sv_gt(0))
  
  
  indmeansrawsd_iv$add_rule("popuSDRaw2", sv_required()) 
  indmeansrawsd_iv$add_rule("popuSDRaw2", sv_gt(0))
  
  #indMeansUserData
  
  indmeansupload_iv$add_rule("indMeansUserData", sv_required())
  indmeansupload_iv$add_rule("indMeansUserData", ~ if(!(tools::file_ext(input$indMeansUserData$name) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  indmeansupload_iv$add_rule("indMeansUserData", ~ if(nrow(IndMeansUploadData()) == 0) "File is empty.")
  indmeansupload_iv$add_rule("indMeansUserData", ~ if(ncol(IndMeansUploadData()) < 2) "File must contain at least 2 distinct samples to choose from for analysis.")
  indmeansupload_iv$add_rule("indMeansUserData", ~ if(nrow(IndMeansUploadData()) < 3) "Samples must include at least 2 observations.")
  
  indmeansuploadsd_iv$add_rule("popuSDUpload1", sv_required()) 
  indmeansuploadsd_iv$add_rule("popuSDUpload1", sv_gt(0))
  
  indmeansuploadsd_iv$add_rule("popuSDUpload2", sv_required()) 
  indmeansuploadsd_iv$add_rule("popuSDUpload2", sv_gt(0))
  
  indmeansuploadvar_iv$add_rule("indMeansUplSample1", sv_required())
  indmeansuploadvar_iv$add_rule("indMeansUplSample2", sv_required())
  
  
  # before
  
  depmeansraw_iv$add_rule("before", sv_required())
  depmeansraw_iv$add_rule("before", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)"))
  
  # after
  
  depmeansraw_iv$add_rule("after", sv_required())
  depmeansraw_iv$add_rule("after", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)."))
  
  
  depmeansraw_iv$add_rule("before", ~ if(length(createNumLst(input$before)) != length(createNumLst(input$after))) "Before and After must have the same number of observations.")
  depmeansraw_iv$add_rule("after", ~ if(length(createNumLst(input$before)) != length(createNumLst(input$after))) "Before and After must have the same number of observations.")
  
  
  depmeansupload_iv$add_rule("depMeansUserData", sv_required())
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(!(tools::file_ext(input$depMeansUserData$name) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(nrow(DepMeansUploadData()) == 0) "File is empty.")
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(ncol(DepMeansUploadData()) < 2) "File must contain at least 2 distinct 'Before' and 'After' sets of data to choose from for analysis.")
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(nrow(DepMeansUploadData()) < 4) "Samples must include at least 3 observations.")

  
  depmeansuploadvars_iv$add_rule("depMeansUplSample1", sv_required())
  depmeansuploadvars_iv$add_rule("depMeansUplSample2", sv_required())
  depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ if(DepSamplesDiff() != 0) "Before and After must have the same number of observations.")
  depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ if(DepSamplesDiff() != 0) "Before and After must have the same number of observations.")

  # numSuccessesProportion
  
  oneprop_iv$add_rule("numSuccesses", sv_required(message = "Numeric value required."))
  oneprop_iv$add_rule("numSuccesses", sv_integer())
  oneprop_iv$add_rule("numSuccesses", sv_gte(0))
  
  # x1
  twoprop_iv$add_rule("numSuccesses1", sv_required())
  twoprop_iv$add_rule("numSuccesses1", sv_integer())
  twoprop_iv$add_rule("numSuccesses1", sv_gte(0))
  
  # x2
  twoprop_iv$add_rule("numSuccesses2", sv_required())
  twoprop_iv$add_rule("numSuccesses2", sv_integer())
  twoprop_iv$add_rule("numSuccesses2", sv_gte(0))
  
  # numTrialsProportion
  
  oneprop_iv$add_rule("numTrials", sv_required(message = "Numeric value required."))
  oneprop_iv$add_rule("numTrials", sv_integer())
  oneprop_iv$add_rule("numTrials", sv_gt(0))
  
  # n1
  twoprop_iv$add_rule("numTrials1", sv_required())
  twoprop_iv$add_rule("numTrials1", sv_integer())
  twoprop_iv$add_rule("numTrials1", sv_gt(0))
  
  # n2
  twoprop_iv$add_rule("numTrials2", sv_required())
  twoprop_iv$add_rule("numTrials2", sv_integer())
  twoprop_iv$add_rule("numTrials2", sv_gt(0))
  
  # hypMean 
  
  onemeanht_iv$add_rule("hypMean", sv_required())
  
  # hypProportion 
  
  onepropht_iv$add_rule("hypProportion", sv_required())
  onepropht_iv$add_rule("hypProportion", sv_gt(0))
  onepropht_iv$add_rule("hypProportion", sv_lt(1))
  
  
  # ------------------ #
  #     Conditions     #
  # ------------------ #
  onemean_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                input$popuParameter == 'Population Mean' && 
                                input$dataAvailability == 'Summarized Data'))
  
  onemeansdknown_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                       input$popuParameter == 'Population Mean' && 
                                       input$dataAvailability == 'Summarized Data' && 
                                       input$sigmaKnown == 'Known'))
  
  onemeansdunk_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                     input$popuParameter == 'Population Mean' && 
                                     input$dataAvailability == 'Summarized Data' && 
                                     input$sigmaKnown == 'Unknown'))
  
  onemeanraw_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                   input$popuParameter == 'Population Mean' && 
                                   input$dataAvailability == 'Enter Raw Data'))
  
  onemeanupload_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                      input$popuParameter == 'Population Mean' && 
                                      input$dataAvailability == 'Upload Data'))
  
  onemeanuploadvar_iv$condition(function() {isTRUE(input$samplesSelect == '1' && 
                                                   input$dataAvailability == 'Upload Data' && 
                                                   onemeanupload_iv$is_valid()) })
  
  onemeanuploadsd_iv$condition(function() {isTRUE(input$samplesSelect == '1' &&
                                                  input$dataAvailability == 'Upload Data' && 
                                                  input$sigmaKnownUpload == 'Known' && 
                                                  onemeanupload_iv$is_valid()) })
  
  onemeanht_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                  input$popuParameter == 'Population Mean' && 
                                  input$inferenceType == 'Hypothesis Testing'))
  
  indmeanssumm_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                     input$popuParameters == 'Independent Population Means' && 
                                     input$dataAvailability2 == 'Summarized Data'))
  
  indmeansraw_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                    input$popuParameters == 'Independent Population Means' && 
                                    input$dataAvailability2 == 'Enter Raw Data'))
  
  indmeanssdknown_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                        input$popuParameters == 'Independent Population Means' && 
                                        input$dataAvailability2 == 'Summarized Data' && 
                                        input$bothsigmaKnown == 'bothKnown'))
  
  indmeanssdunk_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                      input$popuParameters == 'Independent Population Means' && 
                                      input$dataAvailability2 == 'Summarized Data' && 
                                        input$bothsigmaKnown == 'bothUnknown'))
  
  indmeansrawsd_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                      input$popuParameters == 'Independent Population Means' && 
                                      input$dataAvailability2 == 'Enter Raw Data' && 
                                      input$bothsigmaKnownRaw == 'bothKnown'))
  
  indmeansupload_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                       input$popuParameters == 'Independent Population Means' && 
                                       input$dataAvailability2 == 'Upload Data'))
  
  indmeansuploadvar_iv$condition(function() {isTRUE(input$samplesSelect == '2' && 
                                                    input$popuParameters == 'Independent Population Means' && 
                                                    input$dataAvailability2 == 'Upload Data' && 
                                                    indmeansupload_iv$is_valid()) })
  
  indmeansuploadsd_iv$condition(function() {isTRUE(input$samplesSelect == '2' && 
                                                   input$popuParameters == 'Independent Population Means' && 
                                                   input$dataAvailability2 == 'Upload Data' && 
                                                   input$bothsigmaKnownUpload == 'bothKnown' && 
                                                   indmeansupload_iv$is_valid()) })
  
  depmeansraw_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                    input$popuParameters == 'Dependent Population Means' && 
                                    input$dataTypeDependent == 'Enter Raw Data'))
  
  depmeansupload_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                       input$popuParameters == 'Dependent Population Means' && 
                                       input$dataTypeDependent == 'Upload Data'))
  
  depmeansuploadvars_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                           input$popuParameters == 'Dependent Population Means' && 
                                           input$dataTypeDependent == 'Upload Data') &&
                                           depmeansupload_iv$is_valid())
  
  oneprop_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                input$popuParameter == 'Population Proportion'))
  
  onepropht_iv$condition(~ isTRUE(input$samplesSelect == '1' && 
                                  input$popuParameter == 'Population Proportion' && 
                                  input$inferenceType == 'Hypothesis Testing'))
  
  twoprop_iv$condition(~ isTRUE(input$samplesSelect == '2' && 
                                input$popuParameters == 'Population Proportions'))
  
  
  # ------------------ #
  #     Dependency     #
  # ------------------ #
  si_iv$add_validator(onemean_iv)
  si_iv$add_validator(onemeansdknown_iv)
  si_iv$add_validator(onemeansdunk_iv)
  si_iv$add_validator(onemeanraw_iv)
  si_iv$add_validator(onemeanht_iv)
  si_iv$add_validator(onemeanupload_iv)
  si_iv$add_validator(onemeanuploadvar_iv)
  si_iv$add_validator(onemeanuploadsd_iv)
  si_iv$add_validator(indmeanssumm_iv)
  si_iv$add_validator(indmeansraw_iv)
  si_iv$add_validator(indmeanssdknown_iv)
  si_iv$add_validator(indmeanssdunk_iv)
  si_iv$add_validator(indmeansrawsd_iv)
  si_iv$add_validator(indmeansupload_iv)
  si_iv$add_validator(indmeansuploadvar_iv)
  si_iv$add_validator(indmeansuploadsd_iv)
  si_iv$add_validator(depmeansraw_iv)
  si_iv$add_validator(depmeansupload_iv)
  si_iv$add_validator(depmeansuploadvars_iv)
  si_iv$add_validator(oneprop_iv)
  si_iv$add_validator(onepropht_iv)
  si_iv$add_validator(twoprop_iv)
  
  
  # ------------------ #
  #     activation     #
  # ------------------ #
  si_iv$enable()
  onemean_iv$enable()
  onemeansdknown_iv$enable()
  onemeansdunk_iv$enable()
  onemeanraw_iv$enable()
  onemeanht_iv$enable()
  onemeanupload_iv$enable()
  onemeanuploadvar_iv$enable()
  onemeanuploadsd_iv$enable()
  indmeanssumm_iv$enable()
  indmeansraw_iv$enable()
  indmeanssdknown_iv$enable()
  indmeanssdunk_iv$enable()
  indmeansrawsd_iv$enable()
  indmeansupload_iv$enable()
  indmeansuploadvar_iv$enable()
  indmeansuploadsd_iv$enable()
  depmeansraw_iv$enable
  depmeansupload_iv$enable()
  depmeansuploadvars_iv$enable()
  oneprop_iv$enable()
  onepropht_iv$enable()
  twoprop_iv$enable()
  
  
  ## RC rules ---- 
  
  slrraw_iv$add_rule("x", sv_required())
  #iv$add_rule("x", sv_regex("^[0-9]+(.[0-9]+)?(, [0-9](.[0-9]+)?)+$", "Data can only be numeric values separated by commas"))
  slrraw_iv$add_rule("x", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                   "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
  slrraw_iv$add_rule("x", ~ if(sampleDiffRaw() != 0) "x and y must have the same number of observations")
  
  slrraw_iv$add_rule("y", sv_required())
  slrraw_iv$add_rule("y", sv_regex("^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+$", 
                                   "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
  slrraw_iv$add_rule("y", ~ if(sampleDiffRaw() != 0) "x and y must have the same number of observations")
  
  slrupload_iv$add_rule("slrUserData", sv_required())
  slrupload_iv$add_rule("slrUserData", ~ if(!(tools::file_ext(input$slrUserData$name) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) == 0) "File is empty")
  slrupload_iv$add_rule("slrUserData", ~ if(ncol(slrUploadData()) < 2) "Data must include one response and (at least) one explanatory variable")
  slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) < 3) "Samples must include at least 2 observations")
  
  slruploadvars_iv$add_rule("slrResponse", sv_required())
  slruploadvars_iv$add_rule("slrExplanatory", sv_required())
  slruploadvars_iv$add_rule("slrResponse", ~ if(sampleDiffUpload() != 0) "Missing values detected, x and y must have the same number of observations")
  
  slrraw_iv$condition(~ isTRUE(input$dataRegCor == 'Enter Raw Data'))
  slrupload_iv$condition(~ isTRUE(input$dataRegCor == 'Upload Data'))
  slruploadvars_iv$condition(function() {isTRUE(input$dataRegCor == 'Upload Data' && slrupload_iv$is_valid()) })
  
  
  regcor_iv$add_validator(slrraw_iv)
  regcor_iv$add_validator(slrupload_iv)
  regcor_iv$add_validator(slruploadvars_iv)
  
  iv$enable()
  regcor_iv$enable()
  slrraw_iv$enable()
  slrupload_iv$enable()
  slruploadvars_iv$enable()
  #slruploadvars_iv$disable()
  
  
  # -------------------------- #
  # ---- Functions/Output ----
  # -------------------------- #
  
  # String List to Numeric List
  createNumLst <- function(text) {
    text <- gsub("[^0-9.,-]","", text) #purge non-numeric characters 
    text <- gsub("^,", "", text)      #purge any leading commas
    text <- gsub(",(,)+", ",", text)  #transform multiple consecutive commas into a single comma
    text <- gsub(",$", "", text)      #purge any trailing commas
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    suppressWarnings(na.omit(as.numeric(split)))
  }
  
  # **************************************************************************** #
  
  #  -------------------------------------------------------------------- #
  ## ------------------- Descriptive Stats functions --------------------
  #  -------------------------------------------------------------------- #
  
  
  ### Non-Reactive Functions ----
  # --------------------------------------------------------------------- #
  
  # Function to find the mode(s)
  Modes <- function(x) {
    modes <- Mode(x)
    if (anyNA(modes)) {return("No mode exists")}
    else if (length(modes) == 1) {return(paste(modes))}
    else if (length(modes) > 1) {
      modesList <- paste(modes[1])
      
      for(index in 2:length(modes)) {
        modesList <- paste0(modesList, ", ", modes[index])
      }
      return(modesList)
      }
  }
  
  GetOutliers <- function(dat, lower, upper) {
    outliers <- c()
    
    for(x in dat) {
      if(x < lower | x > upper) {
        outliers <-c(outliers, x)
      }
    }

    return(sort(outliers))
  }
  
  
  # Function to find the population standard deviation
  pop.sd <- function(x) {
    sqrt(sum((x-mean(x))^2)/length(x))
  }
  
  
  # Function for populating the value column of the datatable
  createDSColumn <- function(dat) ({
    
    sampSize <- length(dat)
    sampSum <- sum(dat)
    sumSquares <- sum(dat^2)
    xbar <- round(mean(dat),4)
    sampMode <- Modes(dat)
    sampMin <- min(dat)
    #popuStdDev <- round(pop.sd(dat),4) # round(sqrt((n-1)/n) * sampStdDev(dat), 4)
    quartile1 <-  fivenum(dat)[2]
    sampMedian <- median(dat)
    quartile3 <-  fivenum(dat)[4]
    sampMax <- max(dat)
    sampIQR <- round(quartile3 - quartile1, 4)
    lowerFence <- round(quartile1 - (1.5*sampIQR), 4)
    upperFence <- round(quartile3 + (1.5*sampIQR), 4)
    numOutliers <- sum(dat < lowerFence) + sum(dat > upperFence)
    
    if(numOutliers == 0) {
      outliers <- "There are no outliers."
    } else {
      outliers <- paste(as.character(GetOutliers(dat, lowerFence, upperFence)), collapse=", ")
    }
    
    sampRange <- range(dat)[2]-range(dat)[1]
    sampStdDev <- round(sd(dat),4)
    sampVar <- round(var(dat),4)
    sampMeanSE <- round(sd(dat)/sqrt(length(dat)), 4)
    
    coeffVar <- round(sampStdDev/xbar, 4)
    if(is.infinite(coeffVar)) {
      coeffVar <- "Infinity"
    }
    
    if(sampSize < 3){
      sampSkewness <- round(skewness(dat, type = 1), 4)
    } else {
      sampSkewness <- round(skewness(dat, type = 2), 4)
    }
    if(sampSize < 4){
      sampKurtosis <- round(kurtosis(dat, type = 1), 4)
    } else {
      sampKurtosis <- round(kurtosis(dat, type = 2), 4)
    }
    
    if(is.nan(sampSkewness)) {
      sampSkewness <- "Not enough variability or data points in the dataset."
    }
    
    if(is.nan(sampKurtosis)) {
      sampKurtosis <- "Not enough variability or data points in the dataset."
    }
    
    
    dfCol <- data.frame(Value = c(sampSize, 
                                  sampSum, 
                                  sumSquares, 
                                  xbar, 
                                  sampMode, 
                                  sampMin, 
                                  quartile1, 
                                  sampMedian, 
                                  quartile3, 
                                  sampMax, 
                                  sampIQR, 
                                  lowerFence, 
                                  upperFence, 
                                  numOutliers,
                                  outliers,
                                  sampRange, 
                                  sampStdDev, 
                                  sampVar, 
                                  sampMeanSE, 
                                  coeffVar, 
                                  sampSkewness, 
                                  sampKurtosis)
    )
  })
  
  
  # --------------------------------------------------------------------- #
  
  
  ### Reactives ----
  # --------------------------------------------------------------------- #
  
  # Function to convert the raw data input into a numeric list
  dsRawData <- reactive ({
    dat <- createNumLst(input$descriptiveStat)
  })
  
  
  # Function to read the uploaded data file
  dsUploadData <- eventReactive(input$dsUserData, {
    ext <- tools::file_ext(input$dsUserData$name)
    
    switch(ext, 
           csv = read_csv(input$dsUserData$datapath, show_col_types = FALSE),
           xls = read_excel(input$dsUserData$datapath),
           xlsx = read_excel(input$dsUserData$datapath),
           txt = read_tsv(input$dsUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format.")
    )
  })
  
  
  getDsDataframe <- reactive({
    
    req(ds_iv$is_valid())
    
    df <- data.frame(Category = c("Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", 
                                  "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", 
                                  "Outliers", "Outliers", "Outliers", "Outliers", "Outliers", 
                                  "Dispersion", "Dispersion", "Dispersion", "Dispersion", "Dispersion", 
                                  "Distribution", "Distribution"),
                     Variable = c("Number of Observations", 
                                  "Sum", 
                                  "Sum of Squares", 
                                  "Mean", 
                                  "Mode", 
                                  "Minimum", 
                                  "First Quartile \\( (Q_{1}) \\)*", 
                                  "Second Quartile or Median \\( (Q_{2}) \\)*", 
                                  "Third Quartile \\( (Q_{3}) \\)*", 
                                  "Maximum", 
                                  "Interquartile Range (IQR)", 
                                  "Check for Outliers: Lower Fence", 
                                  "Check for Outliers: Upper Fence", 
                                  "Number of Potential Outliers",
                                  "Outlier Values",
                                  "Range", 
                                  "Sample Standard Deviation", 
                                  "Sample Variance", 
                                  "Standard Error of the Mean", 
                                  "Coefficient of Variation",
                                  "Skewness", 
                                  "Kurtosis"))
    
    
    if(input$dataInput == 'Upload Data')
    {
      for( x in input$dsUploadVars)
      {
        dat <- as.data.frame(dsUploadData())[, x]
        newCol <- createDSColumn(dat)
        df[x] <- newCol
      }
      colnames(df) <- c("Category", "Variable", input$dsUploadVars)
    }
    else
    {
      dat <- dsRawData()
      newCol <- createDSColumn(dat)
      df$Value <-newCol
    }
    
    rownames(df) <- c("Observations", 
                      "Sum", 
                      "Sum of Squares", 
                      "Mean", 
                      "Mode", 
                      "Min", 
                      "First Quartile (Q1)", 
                      "Median", 
                      "Third Quartile (Q3)", 
                      "Max", 
                      "IQR", 
                      "Lower Fence", 
                      "Upper Fence", 
                      "Potential Outliers",
                      "Outlier Values",
                      "Range", 
                      "Sample Standard Deviation", 
                      "Sample Variance", 
                      "Standard Error of the Mean", 
                      "Coefficient of Variation", 
                      "Skewness", 
                      "Kurtosis")
    
    return(df)
  })
  
  # --------------------------------------------------------------------- #
  
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  # Fills the variable selection options based on data file columns
  observeEvent(input$dsUserData, {
    hide(id = "descriptiveStatsMP")
    hide(id = "dsUploadVars")
    
    if(dsupload_iv$is_valid())
    {
      freezeReactiveValue(input, "dsUploadVars")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "dsUploadVars",
                        choices = c(colnames(dsUploadData()))
      )
      
      show(id = "dsUploadVars")
    }
  })
  
  
  
  observeEvent(input$goDescpStats, {
    
    output$renderDescrStats <- renderUI({
      if(!dsupload_iv$is_valid())
      {
        validate(
          need(input$dsUserData, "Please upload your data to continue"),
          need(nrow(dsUploadData()) != 0 && ncol(dsUploadData()) > 0, "File is empty"),
          need(nrow(dsUploadData()) > 1, "Sample Data must include at least 2 observations"),
          
          errorClass = "myClass"
        )
      }
      else if(!dsuploadvars_iv$is_valid())
      {
        validate(
          need(input$dsUploadVars != "", "Please select a variable"),
          
          errorClass = "myClass"
        )
        
      }
      else if(!dsraw_iv$is_valid())
      {
        validate(
          need(!anyNA(dsRawData()), "Sample Data must be numeric"),
          need(length(dsRawData()) >= 2, "Sample Data must include at least 2 observations"),
          
          errorClass = "myClass"
        )

      }
    })
    
    if(ds_iv$is_valid())
    {
      output$dsDataTable <- renderUI({

        tagList(
          withMathJax(),
          conditionalPanel(
            condition = "input.dsTableFilters == ''",
            
            br(),
            p("Select one or more options from the Options menu to see more information.")
          ),
          
          conditionalPanel(
            condition = "input.dsTableFilters != ''",
            
            withMathJax(DTOutput("dsTableData"))
          ),
          
        )
      })
      
      df <- getDsDataframe()
      filteredDf <- filter(df, rownames(df) %in% input$dsTableFilters)
      
      output$dsTableData <- renderDT(datatable(filteredDf,
                                               extensions = 'RowGroup',
                                               options = list(
                                                 rowGroup = list(dataSrc = 0),
                                                 columnDefs = list(list(visible=FALSE, targets=c(0))),
                                                 dom = 't',
                                                 pageLength = -1,
                                                 ordering = FALSE,
                                                 searching = FALSE,
                                                 paging = FALSE,
                                                 autoWidth = TRUE,
                                                 scrollX = TRUE
                                               ),
                                               rownames = FALSE,
                                               filter = "none",
                                               
      ))
      
      if(input$dataInput == 'Upload Data')
      {
        for( x in input$dsUploadVars)
        {
          dat <- as.data.frame(dsUploadData())[, x]
        }
        colnames(df) <- c("Category", "Variable", input$dsUploadVars)
      }
      else
      {
        dat <- dsRawData()
      }
      
      df_boxplot <- data.frame(x = dat)
      
      if(df[15,3] != "There are no outliers.") {
        df_outliers <- createNumLst(df[15,3])
      } else {
        df_outliers <- data.frame()
      }
      
      output$dsBoxplot <- renderPlot({
        
        #--------------------#
        # Horizontal boxplot #
        #--------------------#
        
        bp <- ggplot(df_boxplot, aes(x = x, y = 0)) +
          geom_boxplot(fill = "#03376d",
                       alpha = .5,
                       outlier.shape = NA) +
          geom_point(data = filter(df_boxplot, x %in% df_outliers),
                     size = 5) +
          geom_text(data = filter(df_boxplot, x %in% df_outliers),
                    aes(x = x, y = 0, label = x),
                    size = 15 / .pt,
                    vjust = -1.25) +
          labs(x = "Values",
               y = "") +
          theme_minimal() +
          theme(axis.title.x = element_text(size = 18, face = "bold", vjust = -1.5),
                axis.text.x.bottom = element_text(size = 16),
                axis.text.y.left = element_blank()) +
          ylim(-1, 1)
        
        if(length(unique(dat)) == 1) {
          bp + scale_x_continuous(breaks = dat, limits = c(dat[1] - 1, dat[1] + 1))
        } else {
          bp + scale_x_continuous(n.breaks = 8) 
        }
        
      })
      
      
      output$dsHistogram <- renderPlot({
        hist <- ggplot(data.frame(x = dat)) +
          geom_histogram(aes(x = x),
                         bins = 15,
                         fill = "#03376d",
                         color = "black",
                         alpha = .5) +
          labs(x = "Values",
               y = "Frequency") +
          theme_minimal() +
          theme(axis.title.x = element_text(size = 16, 
                                            face = "bold", 
                                            vjust = -1.5),
                axis.title.y = element_text(size = 16, 
                                            face = "bold", 
                                            vjust = 1.5),
                axis.text.x.bottom = element_text(size = 14),
                axis.text.y.left = element_text(size = 14))
        
        # if(length(unique(dat)) == 1) {
        #   hist + scale_x_continuous(breaks = dat, limits = c(dat[1] - 1, dat[1] + 1))
        # } else {
          hist + scale_x_continuous(n.breaks = 10) 
        # }

      })

      
      
      
      output$dsStemLeaf <- renderPrint({
        
        if(length(unique(dat)) > 1) {
          stem.leaf(dat, m = 1, depths = FALSE)
        } else {
          stem(dat, scale = 1)
        }
        
      })
      
      show(id = 'descrStatsData')
    } else {
      hide(id = 'descrStatsData')
    }
    # show(id = 'descriptiveStatsMP') 
  })
  
  dsTableProxy <- dataTableProxy('dsTableData')
  
  observeEvent(input$dsTableFilters, {
    
    df <- getDsDataframe()
    newFilter <- filter(df, rownames(df) %in% input$dsTableFilters)
    
    replaceData(dsTableProxy, newFilter, resetPaging = FALSE, rownames = FALSE)
  })
  
  # --------------------------------------------------------------------- #
  
  
  # **************************************************************************** #
  
  
  #  -------------------------------------------------------------------- #
  ## --------------- Probability Distribution functions -----------------
  #  -------------------------------------------------------------------- #
  
  
  ### Non-Reactive Functions ----
  # --------------------------------------------------------------------- #
  
  shadeNormArea <- function(x){
    area <- dnorm(x, input$popMean, input$popSD)
    
    if(input$calcNormal == "cumulative") #less
    {
      area[x > input$xValue] <- NA
    }
    else if(input$calcNormal == "between") #twosided
    {
      area[x <= input$x1Value | x >= input$x2Value] <- NA
    }
    else if(input$calcNormal == "upperTail") #greater
    {
      area[x < input$xValue] <- NA
    }
    return(area)
  }
  
  normPlot <- function(normValue){
    normTail = qnorm(0.999, mean = input$popMean, sd = input$popSD, lower.tail = FALSE)
    normHead = qnorm(0.999, mean = input$popMean, sd = input$popSD, lower.tail = TRUE)
    #xSeq = seq(normTail, normHead, by = 0.005)
    
    if(input$calcNormal == "between")
    {
      normLines <- c(input$x1Value, input$x2Value)
      probXLab <- (input$x2Value + input$x1Value)/2 
    }
    else
    {
      normLines <- input$xValue
      
      if(input$calcNormal == "cumulative")
      {
        probXLab <- (input$xValue - normHead/4)
      }
      else if(input$calcNormal == "upperTail")
      {
        probXLab <- (input$xValue + normHead/4)
      }
    }
    xSeq = sort(c(normTail, normHead, normLines, probXLab))
    
    df <- data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD))
    lineDF <- filter(df, x %in% normLines)
    probDF <- filter(df, x %in% probXLab)
    
    nPlot <- ggplot(df, aes(x = x, y = y)) +
      stat_function(fun = dnorm, 
                    args = list(mean = input$popMean, sd = input$popSD), 
                    geom = "density",
                    #xlim = c(normTail, normHead),
                    fill = "#03376d",
                    alpha = 0.3) + 
      stat_function(fun = shadeNormArea, 
                    geom = "area",
                    #xlim = c(normTail, normHead),
                    fill = "#03376d",
                    alpha = 0.7) +
      theme_minimal()  +
      theme(axis.title.x = element_text(size = 16, 
                                        face = "bold"),
            axis.text.x.bottom = element_text(size = 14)) +
      scale_x_continuous(breaks = waiver()) +
      scale_y_continuous(breaks = NULL) +
      ylab("") + xlab(paste("X")) +
      geom_text(data = lineDF, 
                aes(x = x, y = 0, label = x), 
                size = 14 / .pt, 
                fontface = "bold", 
                vjust = "outward") 
    #geom_text(data = probDF, aes(x = x, y = y/2, label = normValue), size = 24 / .pt, fontface = "bold")
    
    
    return(nPlot)
  }
  
  # --------------------------------------------------------------------- #
  
  
  ### Reactives ----
  # --------------------------------------------------------------------- #
  
  getNormValue <- reactive({
    req(pd_iv$is_valid())
    
    if(input$calcNormal == "cumulative")
    {
      normValue <- round(pnorm(input$xValue, input$popMean, input$popSD, lower.tail = TRUE),4)
      #paste("\\(P(X \\leq \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = TRUE),4))
    }
    else if(input$calcNormal == "upperTail")
    {
      normValue <- round(pnorm(input$xValue, input$popMean, input$popSD, lower.tail = FALSE),4)
      #paste("\\(P(X > \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = FALSE),4))
    }
    else if(input$calcNormal == 'between')
    {
      req(input$x1Value <= input$x2Value)
      normValue <- round(pnorm(input$x2Value, input$popMean, input$popSD, lower.tail = TRUE) - pnorm(input$x1Value, input$popMean, input$popSD, lower.tail = TRUE), 4)
    }
  })
  
  # --------------------------------------------------------------------- #
  
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  
  #### Binomial ----
  observeEvent(input$goBinom, {
    
    output$renderProbabilityBinom <- renderUI({
      withMathJax(
        if(!pd_iv$is_valid())
        {
          if(!binomprob_iv$is_valid())
          {
            validate(
              need(input$numTrialsBinom, "Number of Trials (n) must be a positive integer") %then%
                need(input$numTrialsBinom > 0 && input$numTrialsBinom %% 1 == 0, "Number of Trials (n) must be a positive integer"),
              need(input$successProbBinom, "Probability of Success (p) must be between 0 and 1") %then%
                need(input$successProbBinom >= 0 && input$successProbBinom <= 1, "Probability of Success (p) must be between 0 and 1"),
              need(input$numSuccessesBinom != "", "Enter a value for the Number of Successes (x)") %then%
                need(input$numSuccessesBinom >= 0 && input$numSuccessesBinom %% 1 == 0, "Number of Successes (x) must be a positive integer"),
              
              errorClass = "myClass"
            )
          }
          
          if(!binombetween_iv$is_valid())
          {
            validate(
              need(input$numTrialsBinom, "Number of Trials (n) must be a positive integer") %then%
                need(input$numTrialsBinom > 0 && input$numTrialsBinom %% 1 == 0, "Number of Trials (n) must be a positive integer"),
              need(input$successProbBinom, "Probability of Success (p) must be between 0 and 1") %then%
                need(input$successProbBinom >= 0 && input$successProbBinom <= 1, "Probability of Success (p) must be between 0 and 1"),
              need(input$numSuccessesBinomx1, "Number of Successes (x1) must be a positive integer") %then%
                need(input$numSuccessesBinomx1 >= 0 && input$numSuccessesBinomx1 %% 1 == 0, "Number of Successes (x1) must be a positive integer"),
              need(input$numSuccessesBinomx2, "Enter a value for the Number of Successes (x2)") %then%
                need(input$numSuccessesBinomx2 >= 0 && input$numSuccessesBinomx2 %% 1 == 0, "Number of Successes (x2) must be a positive integer"),
              
              errorClass = "myClass"
            )
          }
          
          validate(
            need(input$numTrialsBinom, "Number of Trials (n) must be a positive integer") %then%
              need(input$numTrialsBinom > 0 && input$numTrialsBinom %% 1 == 0, "Number of Trials (n) must be a positive integer"),
            need(input$successProbBinom, "Probability of Success (p) must be between 0 and 1") %then%
              need(input$successProbBinom >= 0 && input$successProbBinom <= 1, "Probability of Success (p) must be between 0 and 1"),
            
            errorClass = "myClass"
          )
        }
        else
        {
          binom_n <- input$numTrialsBinom
          binom_p <- input$successProbBinom
          binom_mu <- round(binom_n * binom_p, 4)
          binom_var <- round(binom_mu * (1 - binom_p), 4)
          binom_sd <- round(sqrt(binom_var), 4)
          
          if(input$calcBinom != 'between')
          {
            binom_x <- input$numSuccessesBinom
            
            validate(
              need(binom_x <= binom_n, "Number of Successes (x) must be less than or equal to the Number of Trials (n)"),
              
              errorClass = "myClass"
            )
            
            if(input$calcBinom == 'exact'){
              binomProb <- paste("P(X = ", binom_x, ")") #= ", dbinom(binom_x,binom_n,binom_p))
              binomForm <- paste("\\binom{", binom_n, "}{", binom_x, "}", binom_p, "^", binom_x, "(1-", binom_p, ")^{", binom_n, "-", binom_x, "}")
              binomVal <- round(dbinom(binom_x,binom_n,binom_p), 4)
            }
            else if(input$calcBinom == 'cumulative'){
              binomProb <- paste("P(X \\leq ", binom_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              binomForm <- paste("\\sum_{x = 0}^", binom_x, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE), 4)
            }
            else if(input$calcBinom == 'upperTail'){
              binomProb <- paste("P(X \\geq ", binom_x, ")") # = ", pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE))
              binomForm <- paste("\\sum_{x = ", binom_x, "}^", binom_n, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE), 4)
              
            }
            else if(input$calcBinom == 'greaterThan'){
              binomProb <- paste("P(X \\gt ", binom_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE))
              binomForm <- paste("\\sum_{x = ", binom_x + 1, "}^", binom_n, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE), 4)
            }
            else if(input$calcBinom == 'lessThan'){
              binomProb <- paste("P(X \\lt ", binom_x, ")") # = ", pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE))
              binomForm <- paste("\\sum_{x = 0}^", binom_x - 1, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE), 4)
            }
            
          }
          else if(input$calcBinom == 'between')
          {
            binom_x1 <- input$numSuccessesBinomx1
            
            binom_x2 <- input$numSuccessesBinomx2
            
            validate(
              need(binom_x1 <= binom_n, "Number of Successes (x1) must be less than or equal to the Number of Trials (n)"),
              need(binom_x2 <= binom_n, "Number of Successes (x2) must be less than or equal to the Number of Trials (n)"),
              need(binom_x1 <= binom_x2, "Number of Successes (x1) must be less than or equal to Number of Successes (x2)"),
              
              errorClass = "myClass"
            )
            
            binomProb <- paste("P(", binom_x1, " \\leq X \\leq ", binom_x2, ")")
            binomForm <- paste("\\sum_{x = ", binom_x1, "}^", binom_x2, " \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
            binomVal <- round(pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE) - pbinom(binom_x1-1,binom_n,binom_p,lower.tail = TRUE), 4)
          }
          
          tagList(
            withMathJax(
              div(
                h3(
                  sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Bin(%1.0f,%g): \\)",
                          binomProb,
                          binom_n,
                          binom_p)
                ),
                hr(),
                br(),
                p(tags$b("Using the Probability Mass Function: ")),
                sprintf("\\( \\qquad \\qquad P(X = x) = \\binom{n}{x} p^x (1-p)^{n-x} \\)"),
                sprintf("\\( \\qquad \\) for \\( x = 0, 1, 2, ... n\\)"),
                br(),
                br(),
                br(),
                sprintf("\\( \\displaystyle %s = %s\\)",
                        binomProb,
                        binomForm),
                br(),
                br(),
                sprintf("\\( %s = %0.4f\\)",
                        binomProb,
                        binomVal),
                br(),
                br(),
                br(),
                sprintf("Mean \\( (\\mu) = np = %g\\)",
                        binom_mu),
                br(),
                br(),
                sprintf("Standard Deviation \\( (\\sigma) = \\sqrt{np(1 - p)} = %g\\)",
                        binom_sd),
                br(),
                br(),
                sprintf("Variance \\( (\\sigma^{2}) = np(1 - p) = %g\\)",
                        binom_var)
              ),
              br(),
              conditionalPanel(
                condition = "input.showBinomTable == 1",
                
                br(),
                titlePanel("Probability Distribution Table"),
                hr(),
                DTOutput("binomDistrTable", width = "25%"),
                br()
              )
            )
          )
        }
      )
    })
    
    output$binomDistrTable <- DT::renderDT({
      req(pd_iv$is_valid())
      
      if(input$numTrialsBinom < 50)
      {
        dfBinom <- data.frame(value = seq(0, input$numTrialsBinom), value = round(dbinom(x = 0:input$numTrialsBinom, size = input$numTrialsBinom, prob = input$successProbBinom), 4))
        colnames(dfBinom) <- c("X", "P(X = x)")
        
        datatable(dfBinom,
                  options = list(
                    dom = 't',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE
                  ),
                  rownames = FALSE,
                  filter = "none"
        ) %>% formatRound(2, digits = 4)
      }
      else
      {
        dfBinom <- data.frame(value = "Probability distribution table limited to sample sizes less than 50")
        colnames(dfBinom) <- c("Sample Size Too Large")
        datatable(dfBinom,
                  options = list(
                    dom = '',
                    pageLength = -1,
                    ordering = FALSE,
                    searching = FALSE,
                    paging = FALSE
                  ),
                  rownames = FALSE,
                  filter = "none"
        )
      }
      
      
    })
  })
  
  #### Poisson ----
  observeEvent(input$goPoisson, {
    
    output$renderProbabilityPoisson <- renderUI({
      
      withMathJax(
        
        if(!pd_iv$is_valid())
        {
          if(!poissprob_iv$is_valid())
          {
            validate(
              need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
              need(input$xPoisson , "Number of Successes (x) must be a positive integer") %then%
                need(input$xPoisson >= 0 && input$xPoisson %% 1 == 0, "Number of Successes (x) must be a positive integer"),
              
              errorClass = "myClass"
            )
          }
          
          if(!poissbetween_iv$is_valid())
          {
            validate(
              need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
              need(input$x2Poisson, "Enter a value for the Number of Successes (x1)") %then%
                need(input$x1Poisson >= 0 && input$x1Poisson %% 1 == 0, "Number of Successes (x1) must be a positive integer"),
              need(input$x2Poisson, "Enter a value for the Number of Successes (x2)") %then%
                need(input$x2Poisson >= 0 && input$x2Poisson %% 1 == 0, "Number of Successes (x2) must be a positive integer"),
              
              errorClass = "myClass"
            )
          }
          
          validate(
            need(input$muPoisson && input$muPoisson > 0, "Average Number of Successes (mu) must be greater than zero"),
            
            errorClass = "myClass"
          )
        }
        else
        {
          poisson_mu <- input$muPoisson
          poisson_sd <- round(sqrt(input$muPoisson), 4)
          
          if(input$calcPoisson != 'between')
          {
            poisson_x <- input$xPoisson
            
            if(input$calcPoisson == 'exact'){
              poissProb <- paste("P(X = ", poisson_x, ")") #= ", dbinom(binom_x,binom_n,binom_p))
              poissForm <- paste("\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^", poisson_x, "}{", poisson_x, "!}")
              poissVal <- round(dpois(poisson_x,poisson_mu), 4)
            }
            else if(input$calcPoisson == 'cumulative'){
              poissProb <- paste("P(X \\leq ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("\\sum_{x = 0}^", poisson_x, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = TRUE), 4)
            }
            else if(input$calcPoisson == 'upperTail'){
              poissProb <- paste("P(X \\geq ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("1 - \\sum_{x = 0}^", poisson_x - 1, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x - 1,poisson_mu,lower.tail = FALSE), 4)
            }
            else if(input$calcPoisson == 'greaterThan'){
              poissProb <- paste("P(X \\gt ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("1 - \\sum_{x = 0}^", poisson_x, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = FALSE), 4)
            }
            else if(input$calcPoisson == 'lessThan'){
              poissProb <- paste("P(X \\lt ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("\\sum_{x = 0}^", poisson_x - 1, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x - 1,poisson_mu,lower.tail = TRUE), 4)
            }
          }
          else if(input$calcPoisson == 'between')
          {
            validate(
              need(input$x1Poisson <= input$x2Poisson, "Number of Successes (x1) must be less than or equal to Number of Successes (x2)"),
              
              errorClass = "myClass"
            )
            
            poisson_x1 <- input$x1Poisson
            poisson_x2 <- input$x2Poisson
            
            poissProb <- paste("P(", poisson_x1, " \\leq X \\leq ", poisson_x2, ")")
            poissForm <- paste("\\sum_{x = ", poisson_x1, "}^", poisson_x2, "\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
            poissVal <- round(ppois(poisson_x2, poisson_mu, lower.tail = TRUE) - ppois(poisson_x1 - 1, poisson_mu, lower.tail = TRUE), 4)
          }
          
          tagList(
            withMathJax(
              div(
                h4(
                  sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Pois(%g): \\)",
                          poissProb,
                          poisson_mu)
                ),
                hr(),
                br(),
                p(tags$b("Using the Probability Mass Function: ")),
                sprintf("\\( \\qquad \\qquad P(X = x) = \\dfrac{e^{-\\mu} \\mu^x}{x!} \\)"),
                sprintf("\\( \\qquad \\) for \\( x = 0, 1, 2, ... \\)"),
                br(),
                br(),
                br(),
                sprintf("\\( \\displaystyle %s = %s\\)",
                        poissProb,
                        poissForm),
                br(),
                br(),
                sprintf("\\( %s = %0.4f\\)",
                        poissProb,
                        poissVal),
                br(),
                br(),
                br(),
                sprintf("Mean \\( (\\mu) = \\mu = %g\\)",
                        poisson_mu),
                br(),
                br(),
                sprintf("Standard Deviation \\( (\\sigma) = \\sqrt{\\mu} = %g\\)",
                        poisson_sd),
                br(),
                br(),
                sprintf("Variance \\( (\\sigma^{2}) = \\mu = %g\\)",
                        poisson_mu)
              ),
              br(),
              conditionalPanel(
                condition = "input.showPoissTable == 1",
                
                br(),
                titlePanel("Probability Distribution Table"),
                hr(),
                DTOutput("poissDistrTable", width = "25%"),
                br()
              )
            )
          )
        }
      )
    })
    
    output$poissDistrTable <- DT::renderDT({
      req(pd_iv$is_valid())
      
      dfPoiss <- data.frame(value = seq(qpois(0.0001, input$muPoisson), qpois(0.9999, input$muPoisson)), value = round(dpois(x = qpois(0.0001, input$muPoisson):qpois(0.9999, input$muPoisson), lambda = input$muPoisson), 4))
      colnames(dfPoiss) <- c("X", "P(X = x)")
      
      datatable(dfPoiss,
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE
                ),
                rownames = FALSE,
                filter = "none"
      ) %>% formatRound(2, digits = 4)
      
    })
  })
  
  ### Normal ----
  observeEvent(input$goNormal, {
    
    output$renderProbabilityNorm <- renderUI({
      
      if(!pd_iv$is_valid())
      {
        if(!normprob_iv$is_valid())
        {
          validate(
            need(input$popMean, "Enter a value for Population Mean (mu)"),
            need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
            need(input$xValue, "Enter a value for Normally Distributed Variable (x)"),
            
            errorClass = "myClass"
          )
        }
        
        if(!normbetween_iv$is_valid())
        {
          validate(
            need(input$popMean, "Enter a value for Population Mean (mu)"),
            need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
            need(input$x1Value, "Enter a value for Normally Distributed Variable (x)"),
            need(input$x2Value, "Enter a value for Normally Distributed Variable (x)"),
            
            errorClass = "myClass"
          )
        }
        
        validate(
          need(input$popMean, "Enter a value for Population Mean (mu)"),
          need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
          
          errorClass = "myClass"
        )
      }
      
      norm_mu <- input$popMean
      norm_sigma <- input$popSD
      
      if(input$calcNormal != 'between')
      {
        norm_x <- input$xValue
        
        if(input$calcNormal == "cumulative"){
          normProb <- paste("P(X \\leq ", norm_x,")")
          normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\leq \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
          normForm <- paste("= P(Z \\leq", round((norm_x - norm_mu)/norm_sigma, 4), ")")
        }
        else if(input$calcNormal == "upperTail"){
          normProb <- paste("P(X \\gt ", norm_x,")")
          normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\gt \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
          normForm <- paste("= P(Z \\gt", round((norm_x - norm_mu)/norm_sigma, 4), ")")
        }
      }
      else if(input$calcNormal == 'between')
      {
        norm_x1 <- input$x1Value
        norm_x2 <- input$x2Value
        
        validate(
          need(norm_x1 <= norm_x2, "Normally Distributed Variable (x1) must be less than or equal to Normally Distributed Variable (x2)"),
          
          errorClass = "myClass"
        )
        
        normProb <- paste("P(", norm_x1, " ",  " \\leq X \\leq"," ", norm_x2,")") 
        normProbTransform <- paste("P \\left( \\dfrac{", norm_x1, " - ", norm_mu, "}{", norm_sigma, "} \\leq \\dfrac{X - \\mu}{\\sigma} \\leq",
                                   "\\dfrac{", norm_x2, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
        normForm <- paste("= P(", round((norm_x1 - norm_mu)/norm_sigma, 4), "\\leq Z \\leq", round((norm_x2 - norm_mu)/norm_sigma, 4), ") = ", 
                          round(pnorm(norm_x2,norm_mu, norm_sigma,lower.tail = TRUE), 4), " - ", round(pnorm(norm_x1,norm_mu, norm_sigma,lower.tail = TRUE), 4))
      }
      
      tagList(
        withMathJax(
          div(
            h4(
              sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim N(\\mu = %g, \\sigma^2 = %g): \\)",
                      normProb,
                      norm_mu,
                      norm_sigma^2)
            ),
            hr(),
            br(),
            sprintf("\\( \\displaystyle %s = %s\\)",
                    normProb,
                    normProbTransform),
            br(),
            br(),
            sprintf("\\( \\displaystyle %s = %g\\)",
                    normForm,
                    getNormValue()),
            br(),
            br(),
            br(),
            sprintf("Mean \\( (\\mu) = %g\\)",
                    norm_mu),
            br(),
            br(),
            sprintf("Standard Deviation \\( (\\sigma) = %g\\)",
                    norm_sigma),
            br(),
            br(),
            sprintf("Variance \\( (\\sigma^{2}) = %g\\)",
                    norm_sigma^2)
          ),
          br(),
          hr(),
          br(),
          plotOutput('normDistrPlot', width = "75%", height = "300px"),
          br()
        )
      )
    })
  })
  
  output$normDistrPlot <- renderPlot({
    normPlot(getNormValue())
  })
  
  # --------------------------------------------------------------------- #
  
  
  # **************************************************************************** #
  
  
  #  -------------------------------------------------------------------- #
  ## ----------------- Statistical Inference functions ------------------
  #  -------------------------------------------------------------------- #
  
  
  ### Non-Reactive Functions ----
  # --------------------------------------------------------------------- #
  
  shadeHtZArea <- function(x, critValues, altHypothesis){
    area <- dnorm(x, 0, 1)
    
    if(altHypothesis == "less") #less
    {
      area[x > critValues] <- NA
    }
    else if(altHypothesis == "two.sided") #twosided
    {
      area[x > critValues[1] & x < critValues[2]] <- NA
    }
    else if(altHypothesis == "greater") #greater
    {
      area[x < critValues] <- NA
    }
    return(area)
  }
  
  hypZTestPlot <- function(testStatistic, critValues, altHypothesis){
    normTail = qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE)
    normHead = qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE)
    xSeq = sort(c(normTail, normHead, testStatistic, critValues, 0))
    
    if(testStatistic < normTail)
    {
      normTail = testStatistic
      
    } else if(testStatistic > normHead)
    {
      normHead = testStatistic
    } 
    
    df <- data.frame(x = xSeq, y = dnorm(xSeq, 0, 1))
    cvDF <- filter(df, x %in% critValues)
    tsDF <- filter(df, x %in% testStatistic)
    centerDF <- filter(df, x %in% c(0))
    
    htPlot <- ggplot(df, aes(x = x, y = y)) +
      stat_function(fun = dnorm, 
                    geom = "density",
                    xlim = c(normTail, normHead),
                    fill = "#03376d",
                    alpha = 0.3) + 
      stat_function(fun = shadeHtZArea, 
                    args = list(critValues, altHypothesis), 
                    geom = "area",
                    xlim = c(normTail, normHead),
                    fill = "#03376d",
                    alpha = 0.7) +
      theme_void() +  
      scale_y_continuous(breaks = NULL) +
      ylab("") + xlab("Z") +
      geom_segment(data = filter(df, x %in% c(0)),
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "dotted",
                   linewidth = 0.75,
                   color='#03376d') +
      geom_text(data = filter(df, x %in% c(0)),
                aes(x = x, y = y/2, label = "A R"),
                size = 16 / .pt,
                fontface = "bold") +
      geom_text(data = filter(df, x %in% c(0)),
                aes(x = x, y = 0, label = "0"),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = -.01) +
      geom_segment(data = tsDF,
                   aes(x = x, xend = x, y = -0.01, yend = y + .03),
                   linetype = "solid",
                   linewidth = 1.25,
                   color='#03376d') +
      geom_text(data = tsDF,
                aes(x = x, y = y, label = "TS"),
                size = 16 / .pt,
                fontface = "bold",
                nudge_y = .045) +
      geom_text(data = tsDF,
                aes(x = x, y = 0, label = x),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = -.02) +
      geom_segment(data = cvDF,
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "blank",
                   lineend = 'round',
                   linewidth = 1.5,
                   color='#03376d') +
      geom_text(data = cvDF,
                aes(x = x, y = 0, label = x),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = -.01) +
      geom_text(data = cvDF,
                aes(x = x + x/4, y = y, label = "RR"),
                size = 16 / .pt,
                fontface = "bold",
                nudge_y = .03) +
      theme(axis.title.x = element_text(size = 16, face = "bold"))
    
    return(htPlot)
  }
  
  
  shadeHtTArea <- function(x, testStatistic, df, critValues, altHypothesis){
    area <- dt(x, df)
    
    if(altHypothesis == "less") #less
    {
      area[x > critValues] <- NA
    }
    else if(altHypothesis == "two.sided") #twosided
    {
      area[x > critValues[1] & x < critValues[2]] <- NA
    }
    else if(altHypothesis == "greater") #greater
    {
      area[x < critValues] <- NA
    }
    return(area)
  }
  
  
  hypTTestPlot <- function(testStatistic, degfree, critValues, altHypothesis){
    tTail = qt(0.999, df = degfree, lower.tail = FALSE)
    tHead = qt(0.999, df = degfree, lower.tail = TRUE)
    #xSeq = seq(normTail, normHead, by = 0.005)
    xSeq = sort(c(tTail, tHead, testStatistic, critValues, 0))
    
    if(testStatistic < tTail)
    {
      tTail = testStatistic
      
    } else if(testStatistic > tHead)
    {
      tHead = testStatistic
    } 
    
    df <- data.frame(x = xSeq, y = dt(xSeq, degfree))
    cvDF <- filter(df, x %in% critValues)
    tsDF <- filter(df, x %in% testStatistic)
    centerDF <- filter(df, x %in% c(0))
    
    htPlot <- ggplot(df, aes(x = x, y = y)) +
      stat_function(fun = dt, 
                    args = list(df = degfree), 
                    geom = "density",
                    xlim = c(tTail, tHead),
                    fill = "#03376d",
                    alpha = 0.3) + 
      stat_function(fun = shadeHtTArea, 
                    args = list(testStatistic, degfree, critValues, altHypothesis), 
                    geom = "area",
                    xlim = c(tTail, tHead),
                    fill = "#03376d",
                    alpha = 0.7) +
      theme_void()  +
      scale_y_continuous(breaks = NULL) +
      ylab("") + 
      xlab("t") +
      geom_segment(data = filter(df, x %in% c(0)), 
                   aes(x = x, xend = x, y = 0, yend = y), 
                   linetype = "dotted", 
                   linewidth = 0.75, color='#03376d') +
      geom_text(data = filter(df, x %in% c(0)), 
                aes(x = x, y = y/2, label = "A R"), 
                size = 16 / .pt, 
                fontface = "bold") +
      geom_text(data = filter(df, x %in% c(0)), 
                aes(x = x, y = 0, label = "0"), 
                size = 14 / .pt, 
                fontface = "bold", 
                nudge_y = -.01) +
      geom_segment(data = tsDF, 
                   aes(x = x, xend = x, y = -0.01, yend = y + .03), 
                   linetype = "solid", 
                   linewidth = 1.25, 
                   color='#03376d') +
      geom_text(data = tsDF, 
                aes(x = x, y = y, label = "TS"), 
                size = 16 / .pt, 
                fontface = "bold", 
                nudge_y = .045) +
      geom_text(data = tsDF, 
                aes(x = x, y = 0, label = x), 
                size = 14 / .pt, 
                fontface = "bold", 
                nudge_y = -.02) +
      geom_segment(data = cvDF, 
                   aes(x = x, xend = x, y = 0, yend = y), 
                   linetype = "blank", 
                   lineend = 'round', 
                   linewidth = 1.5, 
                   color='#03376d') +
      geom_text(data = cvDF, 
                aes(x = x, y = 0, label = x), 
                size = 14 / .pt, 
                fontface = "bold", 
                nudge_y = -.01) +
      geom_text(data = cvDF, 
                aes(x = x + x/4, y = y, label = "RR"), 
                size = 16 / .pt, 
                fontface = "bold", 
                nudge_y = .03) +
      theme(axis.title.x = element_text(size = 16, face = "bold"))
    
    return(htPlot)
  }
  
  # --------------------------------------------------------------------- #
  
  
  ### Reactives ----
  # --------------------------------------------------------------------- #
  
  ConfLvl <- reactive({
    
    if(input$samplesSelect == '1') {
      
      if(input$confidenceLevel == '90%') {
        confLvl <- 0.9
      } else if(input$confidenceLevel == '95%') {
        confLvl <- 0.95
      } else {
        confLvl <- 0.99
      }
      
    } else if(input$samplesSelect == '2') {
      
      if(input$confidenceLevel2 == '90%') {
        confLvl <- 0.9
      } else if(input$confidenceLevel2 == '95%') {
        confLvl <- 0.95
      } else {
        confLvl <- 0.99
      }
    } 
    
    return(confLvl)
  })
  
  
  SigLvl <- reactive({
    
    if(input$samplesSelect == '1') {
      
      if(input$significanceLevel == "10%") {
        sigLvl <- 0.1 
      } else if(input$significanceLevel == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
    } else if (input$samplesSelect == '2') {
      
      if(input$significanceLevel2 == "10%") {
        sigLvl <- 0.1 
      } else if(input$significanceLevel2 == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
    }
    
    return(sigLvl)
  })
  
  #### One Mean reactives ----
  
  OneMeanUploadData <- eventReactive(input$oneMeanUserData, {
    
    ext <- tools::file_ext(input$oneMeanUserData$name)
    
    switch(ext, 
           csv = read_csv(input$oneMeanUserData$datapath, show_col_types = FALSE),
           xls = read_excel(input$oneMeanUserData$datapath),
           xlsx = read_excel(input$oneMeanUserData$datapath),
           txt = read_tsv(input$oneMeanUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format.")
    )
  })
  
  
  OneMeanHypInfo <- reactive({
    hypTestSymbols <- list()
    
    if(input$altHypothesis == "3"){
      hypTestSymbols$alternative <- "greater"
      hypTestSymbols$nullHyp <- "\\leq"
      hypTestSymbols$altHyp <- "\\gt"
      hypTestSymbols$critAlph <- "\\alpha"
      hypTestSymbols$critSign <- ""
      hypTestSymbols$alphaVal <- SigLvl()
    }
    else if(input$altHypothesis == "2"){
      hypTestSymbols$alternative <- "two.sided"
      hypTestSymbols$nullHyp <- "="
      hypTestSymbols$altHyp <- "\\neq"
      hypTestSymbols$critAlph <- "\\alpha/2"
      hypTestSymbols$critSign <- "\\pm"
      hypTestSymbols$alphaVal <- SigLvl()/2
    }
    else{
      hypTestSymbols$alternative <- "less"
      hypTestSymbols$nullHyp <- "\\geq"
      hypTestSymbols$altHyp <- "\\lt"
      hypTestSymbols$critAlph <- "\\alpha"
      hypTestSymbols$critSign <- "-"
      hypTestSymbols$alphaVal <- SigLvl()
    }
    
    return(hypTestSymbols)
  })
  
  
  OneMeanZIntSumm <- reactive({
    req(si_iv$is_valid())
    
    nSampOne <- input$sampleSize
    xbarSampOne <- input$sampleMean
    sigmaSampOne <- input$popuSD
    
    oneMeanZInt <- ZInterval(nSampOne, xbarSampOne, sigmaSampOne, ConfLvl())
    
    return(oneMeanZInt)
  })
  
  
  OneMeanZIntRaw <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability == 'Enter Raw Data') {
      dat <- createNumLst(input$sample1)
      popuSD <- input$popuSDRaw
      
    } else if(input$dataAvailability == 'Upload Data') {
      dat <- unlist(OneMeanUploadData()[,input$oneMeanVariable])
      popuSD <- input$popuSDUpload
    }
    
    sampleSize <- length(dat)
    sampleMean <- mean(dat)
    
    oneMeanZInt <- ZInterval(sampleSize, sampleMean, popuSD, ConfLvl())
    
    return(oneMeanZInt)
  })
  
  
  OneMeanTIntSumm <- reactive({
    req(si_iv$is_valid())
    
    nSampOne <- input$sampleSize
    xbarSampOne <- input$sampleMean  
    sSampOne <- input$sampSD
    
    oneMeanTInt <- TInterval(nSampOne, xbarSampOne, sSampOne, ConfLvl())
    
    return(oneMeanTInt)
  })
  
  
  OneMeanTIntRaw <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability == 'Enter Raw Data') {
      dat <- createNumLst(input$sample1)

    } else if(input$dataAvailability == 'Upload Data') {
      dat <- unlist(OneMeanUploadData()[,input$oneMeanVariable])
    }
    
    sampleSize <- length(dat)
    sampleMean <- mean(dat)
    sampleSD <- sd(dat)
    
    oneMeanTInt <- TInterval(sampleSize, sampleMean, sampleSD, 
                             ConfLvl())
    
    return(oneMeanTInt) 
  })
  
  
  OneMeanZTestSumm <- reactive({
    req(si_iv$is_valid())
    
    nSampOne <- input$sampleSize
    xbarSampOne <- input$sampleMean 
    hypMeanSampOne <- input$hypMean 
    sigmaSampOne <- input$popuSD
    
    oneMeanZTest <- ZTest(nSampOne, xbarSampOne, sigmaSampOne, hypMeanSampOne,
                          OneMeanHypInfo()$alternative, SigLvl())
    
    return (oneMeanZTest)
  }) 
  
  OneMeanZTestRaw <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability == 'Enter Raw Data') {
      dat <- createNumLst(input$sample1)
      popuSD <- input$popuSDRaw
    } else if (input$dataAvailability == 'Upload Data') {
      dat <- unlist(OneMeanUploadData()[,input$oneMeanVariable])
      popuSD <- input$popuSDUpload
    }
    
    
    sampleSize <- length(dat)
    sampleMean <- mean(dat)
    hypMeanVal <- input$hypMean 
    
    oneMeanZTest <- ZTest(sampleSize, sampleMean, popuSD, 
                          hypMeanVal, OneMeanHypInfo()$alternative, 
                          SigLvl())
    
    return (oneMeanZTest) 
  })
  
  
  OneMeanTTestSumm <- reactive({
    req(si_iv$is_valid())
    
    nSampOne <- input$sampleSize
    xbarSampOne <- input$sampleMean
    hypMeanSampOne <- input$hypMean 
    sSampOne <- input$sampSD
    
    oneMeanTTest <- TTest(nSampOne, xbarSampOne, sSampOne, hypMeanSampOne, 
                          OneMeanHypInfo()$alternative, SigLvl())
    
    return(oneMeanTTest)
  })
  
  
  
  
  OneMeanTTestRaw <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability == 'Enter Raw Data') {
      dat <- createNumLst(input$sample1)

    } else if (input$dataAvailability == 'Upload Data') {
      dat <- unlist(OneMeanUploadData()[,input$oneMeanVariable])
    }
    
    sampleSize <- length(dat)
    sampleMean <- mean(dat)
    sampleSD <- sd(dat)
    hypMeanVal <- input$hypMean 
    
    oneMeanTTest <- TTest(sampleSize, sampleMean, sampleSD, 
                          hypMeanVal, OneMeanHypInfo()$alternative, 
                          SigLvl())
    
    return(oneMeanTTest)
  })
  
  
  #### Independent Sample Means reactives ----
  
  IndMeansSummData <- reactive({
    req(si_iv$is_valid())
    
    summData <- list()
    
    summData$n1 <- input$sampleSize1
    summData$xbar1 <- input$sampleMean1
    summData$n2 <- input$sampleSize2
    summData$xbar2 <- input$sampleMean2
    summData$sigmaEqual <- input$bothsigmaEqual
    
    if(input$bothsigmaKnown == 'bothKnown'){
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
    
    rawData$n1  <- length(raw_sample1)
    rawData$xbar1 <- mean(raw_sample1)
    rawData$n2  <- length(raw_sample2)
    rawData$xbar2 <- mean(raw_sample2)
    rawData$sigmaEqual <- input$bothsigmaEqualRaw
    
    if(input$bothsigmaKnownRaw == 'bothKnown'){
      rawData$sd1 <- input$popuSDRaw1
      rawData$sd2 <- input$popuSDRaw2
    } else {
      rawData$sd1 <- sd(raw_sample1)
      rawData$sd2 <- sd(raw_sample2)
    }
    
    return(rawData)
  })
  
  IndMeansUploadData <- eventReactive(input$indMeansUserData, {
    
    ext <- tools::file_ext(input$indMeansUserData$name)
    
    switch(ext, 
           csv = read_csv(input$indMeansUserData$datapath, show_col_types = FALSE),
           xls = read_excel(input$indMeansUserData$datapath),
           xlsx = read_excel(input$indMeansUserData$datapath),
           txt = read_tsv(input$indMeansUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format")
    )
  })
  
  GetMeansUploadData <- reactive({
    req(si_iv$is_valid())
    
    dat <- list()
    
    sample1 <- unlist(IndMeansUploadData()[,input$indMeansUplSample1])
    sample2 <- unlist(IndMeansUploadData()[,input$indMeansUplSample2])
    
    dat$n1  <- length(sample1)
    dat$xbar1 <- mean(sample1)
    dat$n2  <- length(sample2)
    dat$xbar2 <- mean(sample2)
    dat$sigmaEqual <- input$bothsigmaEqualUpload
    
    if(input$bothsigmaKnownUpload == 'bothKnown'){
      dat$sd1 <- input$popuSDUpload1
      dat$sd2 <- input$popuSDUpload2
    } else {
      dat$sd1 <- sd(sample1)
      dat$sd2 <- sd(sample2)
    }

    return(dat)
  })
  
  
  
  IndMeansSigmaKnown <- reactive({
    
    if (input$dataAvailability2 == 'Summarized Data') {
      sigmaKnown <- input$bothsigmaKnown
    } else if(input$dataAvailability2 == 'Enter Raw Data'){
      sigmaKnown <- input$bothsigmaKnownRaw
    } else if(input$dataAvailability2 == 'Upload Data'){
      sigmaKnown <- input$bothsigmaKnownUpload
    }
    
    return(sigmaKnown)
  })
  
  
  IndMeansHypInfo <- reactive({
    hypTestSymbols <- list()
    
    if(input$altHypothesis2 == "3"){
      hypTestSymbols$alternative <- "greater"
      hypTestSymbols$nullHyp <- "\\leq"
      hypTestSymbols$altHyp <- "\\gt"
      hypTestSymbols$critAlph <- "\\alpha"
      hypTestSymbols$critSign <- ""
      hypTestSymbols$alphaVal <- SigLvl()
    }
    else if(input$altHypothesis2 == "2"){
      hypTestSymbols$alternative <- "two.sided"
      hypTestSymbols$nullHyp <- "="
      hypTestSymbols$altHyp <- "\\neq"
      hypTestSymbols$critAlph <- "\\alpha/2"
      hypTestSymbols$critSign <- "\\pm"
      hypTestSymbols$alphaVal <- SigLvl()/2
    }
    else{
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
    
    if (input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data') {
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    twoSampZInt <- TwoSampZInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, ConfLvl())
    
    return(twoSampZInt)
  })
  
  
  IndMeansTInt <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data'){
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    twoSampTInt <- TwoSampTInt(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, data$sigmaEqual, ConfLvl())
    
    return(twoSampTInt)
  })
  
  
  IndMeansZTest <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data'){
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    twoSampZTest <- TwoSampZTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, IndMeansHypInfo()$alternative, SigLvl())
    
    return(twoSampZTest)
  })
  
  
  IndMeansTTest <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data'){
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    twoSampTTest <- TwoSampTTest(data$xbar1, data$sd1, data$n1, data$xbar2, data$sd2, data$n2, data$sigmaEqual, IndMeansHypInfo()$alternative, SigLvl())
    
    return(twoSampTTest)
  })
  
  
  #### Dependent Means Reactives ----
  
  DepMeansRawData <- eventReactive({input$before
                                    input$after},{
    req(si_iv$is_valid())
    
    rawData <- list()
    
    rawBefore <- createNumLst(input$before)
    rawAfter <- createNumLst(input$after)
    
    rawData$n1  <- length(rawBefore)
    rawData$n2  <- length(rawAfter)
    rawData$diff <- rawData$n1 - rawData$n2
    print(rawData)
    return(rawData)
  })
  
  
  DepMeansUploadData <- eventReactive(input$depMeansUserData, {
    
    ext <- tools::file_ext(input$depMeansUserData$name)
    
    switch(ext, 
           csv = read_csv(input$depMeansUserData$datapath, show_col_types = FALSE),
           xls = read_excel(input$depMeansUserData$datapath),
           xlsx = read_excel(input$depMeansUserData$datapath),
           txt = read_tsv(input$depMeansUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format")
    )
  })
  
  GetDepMeansUplData <- reactive({
    req(si_iv$is_valid())
    
    dat <- list()
    
    sampBefore <- unlist(DepMeansUploadData()[,input$depMeansUplSample1])
    sampAfter <- unlist(DepMeansUploadData()[,input$depMeansUplSample2])
    
    dat$n1  <- length(sampBefore)
    dat$xbar1 <- mean(sampBefore)
    dat$n2  <- length(sampAfter)
    dat$xbar2 <- mean(sampAfter)
  
    return(dat)
  })
  
  DepSamplesDiff <- eventReactive (c(input$depMeansUplSample1, 
                                       input$depMeansUplSample2), {
                                         if(input$depMeansUplSample1 == "" | input$depMeansUplSample2 == "")
                                         {
                                           return(0)
                                         }
                                         else
                                         {
                                           before <- unlist(DepMeansUploadData()[, input$depMeansUplSample1])
                                           after <- unlist(DepMeansUploadData()[, input$depMeansUplSample2])
                                           difference <- length(na.omit(before)) - length(na.omit(after))
                                           return(difference)
                                         }
                                       })
  
  # --------------------------------------------------------------------- #
  
  
  
  ### Outputs ----
  # --------------------------------------------------------------------- #
  
  
  #### Validation ----
  
  # One Mean Validation 
  # ------------------------------------------------------------------------ #
  output$inferenceValidation <- renderUI({
    
    if(!onemean_iv$is_valid()) {
      validate(
        need(input$sampleSize, "Sample size (n) must be an integer greater than 1.") %then%
          need(input$sampleSize > 1 & input$sampleSize %% 1 == 0, "Sample size (n) must be an integer greater than 1."),
        need(input$sampleMean, "Sample mean required."),
        
        errorClass = "myClass"
      )
    }
    
    if(!onemeanraw_iv$is_valid()) {
      validate(
        need(input$sample1, "Sample Data required.") %then%
          need(length(createNumLst(input$sample1)) > 1, "Sample Data requires a minimum of 2 data points."),
        need(input$popuSDRaw & input$popuSDRaw > 0, "Population Standard Deviation must be positive."),
        #need(input$popuSDRaw > 0, "Population Standard Deviation must be greater than 0"),
        
        errorClass = "myClass"
      )
    }
    
    if(!onemeansdknown_iv$is_valid()) {
      validate(
        need(input$popuSD & input$popuSD > 0, "Population Standard Deviation must be positive."),
        #need(input$popuSD > 0, "Population Standard Deviation must be greater than 0"),
        
        errorClass = "myClass"
      )
    }
    
    if(!onemeansdunk_iv$is_valid()) {
      validate(
        need(input$sampSD && input$sampSD > 0, "Sample Standard Deviation (s) must be positive."),
        
        errorClass = "myClass"
      )
    }
    
    if(!onemeanupload_iv$is_valid()) {
      validate(
        need(input$oneMeanUserData, "Please upload your data to continue."),
        need(nrow(OneMeanUploadData()) != 0, "File is empty."),
        need(nrow(OneMeanUploadData()) > 2, "Samples must include at least 2 observations."),
        
        errorClass = "myClass"
      )
    }
    
    if(!onemeanuploadvar_iv$is_valid()) {
      validate(
        need(input$oneMeanVariable != "", "Please select a column for analysis."),
          
        errorClass = "myClass"
      )
    }
    
    if(!onemeanuploadsd_iv$is_valid()) {
      validate(
        need(input$popuSDUpload && input$popuSDUpload > 0, "Population Standard Deviation must be positive."),
      
        errorClass = "myClass"
      )
    }
    
    if(!onemeanht_iv$is_valid()) {
      validate(
        need(input$hypMean, "Hypothesized Population Mean value required."),
        
        errorClass = "myClass"
      )
    }
    
  # One Prop Validation 
  # ------------------------------------------------------------------------ #
    
    if(!oneprop_iv$is_valid()) {
      validate(
        need(input$numSuccesses, "Numeric value for Number of Successes (x) required"),
        need(input$numTrials, "Numeric value for Number of Trials (n) required"),

        errorClass = "myClass"
      )

      validate(
        need(input$numSuccesses %% 1 == 0, "Number of Successes (x) must be an integer"),
        need(input$numSuccesses >= 0, "Number of Successes (x) cannot be negative"),
        need(input$numTrials %% 1 == 0, "Number of Trials (n) must be an integer"),
        need(input$numTrials > 0, "Number of Trials (n) must be greater than 0") %then%
          need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),

        errorClass = "myClass"
      )
    } else if(input$samplesSelect == '1' && input$popuParameter == 'Population Proportion') {
      req(input$numSuccesses && input$numTrials)
      validate(
        need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),

        errorClass = "myClass"
      )

    }
     
    if(!onepropht_iv$is_valid()) {
      validate(
        need(input$hypProportion, "Hypothesized Population Proportion must be between 0 and 1") %then%
          need(input$hypProportion > 0 && input$hypProportion < 1, "Hypothesized Population Proportion must be between 0 and 1"),
          
        errorClass = "myClass"
      )
    }
    
    
  # Independent Population Means Validation 
  # ------------------------------------------------------------------------ #
    
    if(!indmeanssumm_iv$is_valid()) {
      
      validate(
        need(input$sampleSize1, "Sample Size 1 (n1) must be an integer greater than 1.") %then%
          need(input$sampleSize1 > 1 & input$sampleSize1 %% 1 == 0, "Sample Size 1 (n1) must be an integer greater than 1."),
        need(input$sampleMean1, "Sample Mean 1 required."),
        
        need(input$sampleSize2, "Sample Size 2 (n2) must be an integer greater than 1.") %then%
          need(input$sampleSize2 > 1 & input$sampleSize2 %% 1 == 0, "Sample Size 2 (n2) must be an integer greater than 1."),
        need(input$sampleMean2, "Sample Mean 2 required."),
        
        errorClass = "myClass"
      )
    }
    
    if(!indmeanssdknown_iv$is_valid())
    {
      validate(
        need(input$popuSD1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
        need(input$popuSD2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
        
        errorClass = "myClass"
      )
    }
    
    if(!indmeanssdunk_iv$is_valid())
    {
      validate(
        need(input$sampSD1 && input$sampSD1 > 0, "Sample Standard Deviation (s1) must be positive."),
        need(input$sampSD2 && input$sampSD2 > 0, "Sample Standard Deviation (s2) must be positive."),
        
        errorClass = "myClass"
      )
    }
    
    if(!indmeansraw_iv$is_valid()) {
      
      validate(
        need(input$raw_sample1, "Sample 1 requires a minimum of 3 data points.") %then%
          need(length(createNumLst(input$raw_sample1)) > 2, "Sample Data requires a minimum of 3 data points."),
        need(input$raw_sample2, "Sample 2 requires a minimum of 3 data points.") %then%
          need(length(createNumLst(input$raw_sample2)) > 2, "Sample Data requires a minimum of 3 data points."),
        
        errorClass = "myClass"
      )
      
      validate("Samples require a minimum of 3 data points.")
    }
    
    if(!indmeansrawsd_iv$is_valid()) {
      
      validate(
        need(input$popuSDRaw1 & input$popuSD1 > 0, "Population Standard Deviation 1 must be positive."),
        need(input$popuSDRaw2 & input$popuSD2 > 0, "Population Standard Deviation 2 must be positive."),
        
        errorClass = "myClass"
      )
    }
    
    if(!indmeansupload_iv$is_valid()) {

      validate(
        need(input$indMeansUserData, "Please upload your data to continue."),
        need(nrow(IndMeansUploadData()) != 0, "File is empty."),
        need(ncol(IndMeansUploadData()) > 1, "File must contain at least 2 distinct samples to choose from for analysis."),
        need(nrow(IndMeansUploadData()) > 2, "Samples must include at least 2 observations."),
        
        errorClass = "myClass"
      )
    }
    
    if(!indmeansuploadvar_iv$is_valid()) {
      
      validate(
        need(input$indMeansUplSample1, "Please select a column for Sample 1."),
        need(input$indMeansUplSample2, "Please select a column for Sample 2."),
        
        errorClass = "myClass"
      )
    }
    
    if(!indmeansuploadsd_iv$is_valid()) {
      
      validate(
        need(input$popuSDUpload1 && input$popuSDUpload1 > 0, "Population Standard Deviation 1 must be positive."),
        need(input$popuSDUpload2 && input$popuSDUpload2 > 0, "Population Standard Deviation 2 must be positive."),
        
        errorClass = "myClass"
      )
    }
    
  # Dependent Population Means Validation 
  # ------------------------------------------------------------------------ #
    
    if(!depmeansraw_iv$is_valid()) {
      
      validate(
        need(input$before, "'Before' sample data requires a minimum of 3 data points.") %then%
          need(length(createNumLst(input$before)) > 2, "'Before' sample Data requires a minimum of 3 data points."),
        need(input$after, "'After' sample data requires a minimum of 3 data points.") %then%
          need(length(createNumLst(input$after)) > 2, "'After' sample Data requires a minimum of 3 data points."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(length(createNumLst(input$before)) == length(createNumLst(input$after)), "Same number of data points required for 'Before' and 'After' sample data."),

        errorClass = "myClass"
      )
    } 
    
    if(!depmeansupload_iv$is_valid()) {
      
      validate(
        need(input$depMeansUserData, "Please upload your data to continue"),
        need(nrow(DepMeansUploadData()) > 0, "File is empty."),
        need(ncol(DepMeansUploadData()) >= 2, "File must contain at least 2 distinct 'Before' and 'After' sets of data to choose from for analysis."),
        need(nrow(DepMeansUploadData()) >= 3, "Samples must include at least 3 observations."),
        
        errorClass = "myClass"
      )
    }
    
    if(!depmeansuploadvars_iv$is_valid()) {
      
      validate(
        need(input$depMeansUplSample1, "Please select a column for the 'Before' sample data."),
        need(input$depMeansUplSample2, "Please select a column for the 'After' sample data."),
        need(DepSamplesDiff() == 0, "Same number of data points required for 'Before' and 'After' sample data."),
        
        errorClass = "myClass"
      )
    }
    
  # Two Population Proportion Validation 
  # ------------------------------------------------------------------------ #
    
    if(!twoprop_iv$is_valid()) {
      
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
        need(input$numSuccesses2 %% 1 == 0, "Number of Successes 1 (x2) must be an integer"),
        need(input$numSuccesses2 >= 0, "Number of Successes 1 (x2) cannot be negative"),
        need(input$numTrials2 %% 1 == 0, "Number of Trials 2 (n2) must be an integer"),
        need(input$numTrials2 > 0, "Number of Trials 2 (n2) must be greater than 0"),
        
        errorClass = "myClass"
      )
    
    } else if (input$samplesSelect == '2' && input$popuParameters == 'Population Proportions') {
      req(input$numSuccesses1 && input$numTrials1)
      req(input$numSuccesses2 && input$numTrials2)
      
      validate(
        need(input$numSuccesses1 <= input$numTrials1, "Number of Successes 1 (x1) cannot be greater than Number of Trials 1 (n1)"),
        need(input$numSuccesses2 <= input$numTrials2, "Number of Successes 2 (x2) cannot be greater than Number of Trials 2 (n2)"),
        
        errorClass = "myClass"
      )
    }
  })
 
  
  #### One Mean outputs ----
  
  
  ##### CI ----
  output$oneMeanCI <- renderUI({
    withMathJax()
    if(input$dataAvailability == 'Summarized Data'){
      
      if(input$sigmaKnown == 'Known'){
        
        oneMeanData <- OneMeanZIntSumm()
        sdSymbol <- "\\sigma"
        testStat <- "z"
        critVal <- oneMeanData["Z Critical"]
        
      } else if(input$sigmaKnown == 'Unknown'){
        
        oneMeanData <- OneMeanTIntSumm()
        sdSymbol <- "s"
        testStat <- "t"
        critVal <- oneMeanData["T Critical"]
        
      } 
    } else if(input$dataAvailability == 'Enter Raw Data'){
      
      if(input$sigmaKnownRaw == 'rawKnown'){
        
        oneMeanData <- OneMeanZIntRaw()
        sdSymbol <- "\\sigma"
        testStat <- "z"
        critVal <- oneMeanData["Z Critical"]
        
      } else if(input$sigmaKnownRaw == 'rawUnknown'){
        
        oneMeanData <- OneMeanTIntRaw()
        sdSymbol <- "s"
        testStat <- "t"
        critVal <- oneMeanData["T Critical"]
      } 
    } else if(input$dataAvailability == 'Upload Data'){
      
      if(input$sigmaKnownUpload == 'Known'){
        
        oneMeanData <- OneMeanZIntRaw()
        sdSymbol <- "\\sigma"
        testStat <- "z"
        critVal <- oneMeanData["Z Critical"]
        
      } else if(input$sigmaKnownUpload == 'Unknown'){
        
        oneMeanData <- OneMeanTIntRaw()
        sdSymbol <- "s"
        testStat <- "t"
        critVal <- oneMeanData["T Critical"]
      } 
    }
    
    p(
      withMathJax(
        sprintf("\\( CI = \\bar{x} \\pm %s_{\\alpha/2} \\cdot \\dfrac{%s}{\\sqrt{n}}\\)",
                testStat,
                sdSymbol),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\quad = %s \\pm \\left( %g \\cdot \\dfrac{%g}{\\sqrt{%g}} \\right) \\)",
                oneMeanData["Sample Mean"],
                critVal,
                oneMeanData[3],
                oneMeanData['Sample Size']),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\quad = %s \\pm \\left( %g \\cdot %g \\right) \\)",
                oneMeanData["Sample Mean"],
                critVal,
                oneMeanData['Std Error']),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\quad = %s \\pm %g \\)",
                oneMeanData["Sample Mean"],
                oneMeanData['ME']),
        br(),
        br(),
        sprintf("\\( \\quad = (%g, %g)\\)",
                oneMeanData["LCL"],
                oneMeanData["UCL"]),
        br(),
        br(),
        br(),
        p(tags$b("Interpretation:")),
        sprintf("We are %1.0f%% confident that the population mean \\( (\\mu)\\) is between \\( %g \\) and \\( %g \\).",
                ConfLvl()*100,
                oneMeanData["LCL"],
                oneMeanData["UCL"]),
        br()
      )
    )
  })
  
  
  ##### HT ----
  output$oneMeanHT <- renderUI({
    withMathJax()
    
    if(input$dataAvailability == 'Summarized Data'){
      
      if(input$sigmaKnown == 'Known'){
        oneMeanData <- OneMeanZTestSumm()
        sdSymbol <- "\\sigma"
        testStat <- "z"
      }
      else if(input$sigmaKnown == 'Unknown'){
        oneMeanData <- OneMeanTTestSumm()
        sdSymbol <- "s"
        testStat <- "t"
      } 
    } else if(input$dataAvailability == 'Enter Raw Data'){
      
      if(input$sigmaKnownRaw == 'rawKnown'){
        oneMeanData <- OneMeanZTestRaw()
        sdSymbol <- "\\sigma"
        testStat <- "z"
      }
      else if(input$sigmaKnownRaw == 'rawUnknown'){
        oneMeanData <- OneMeanTTestRaw()
        sdSymbol <- "s"
        testStat <- "t"
      } 
    } else if(input$dataAvailability == 'Upload Data'){
      
      if(input$sigmaKnownUpload == 'Known'){
        
        oneMeanData <- OneMeanZTestRaw()
        sdSymbol <- "\\sigma"
        testStat <- "z"
        critVal <- oneMeanData["Z Critical"]
        
      } else if(input$sigmaKnownUpload == 'Unknown'){
        
        oneMeanData <- OneMeanTTestRaw()
        sdSymbol <- "s"
        testStat <- "t"
        critVal <- oneMeanData["T Critical"]
      } 
    }
    
    intrpInfo <- OneMeanHypInfo()
    
    if(oneMeanData[7] < 0.0001)
    {
      pValue <- "\\lt 0.0001"
    }
    else
    {
      pValue <- paste(oneMeanData[7])
    }
    
    if(oneMeanData[7] > SigLvl())
    {
      pvalSymbol <- "\\gt"
      suffEvidence <- "do not provide"
      reject <- "do not reject"
      region <- "acceptance"
    }
    else
    {
      pvalSymbol <- "\\leq"
      suffEvidence <- "provide"
      reject <- "reject"
      region <- "rejection"
    }
    
    if(intrpInfo$alternative == "two.sided")
    {
      if(testStat == 'z') {
        critVal <- paste("\\pm", oneMeanData["Z Critical"])
      } else {
        critVal <- paste("\\pm", oneMeanData["T Critical"])
      }
      
    }
    else
    {
      if(testStat == 'z') {
        critVal <- paste(oneMeanData["Z Critical"])
      } else {
        critVal <- paste(oneMeanData["T Critical"])
      }
      
    }
    
    tagList(
      
      p(
        withMathJax(
          #h4(tags$u("Performing the Hypothesis Test:")),
          #br(),
          sprintf("\\( H_{0}: \n \\mu %s %s\\)",
                  intrpInfo$nullHyp,
                  input$hypMean),
          br(),
          sprintf("\\( H_{a}: \\mu %s %s\\)",
                  intrpInfo$altHyp,
                  input$hypMean),
          br(),
          br(),
          sprintf("\\( \\alpha = %s \\)",
                  SigLvl()),
          br(),
          br(),
          br(),
          sprintf("\\(%s = \\dfrac{\\bar{x} - \\mu_{0}}{ \\dfrac{%s}{\\sqrt{n}} } = \\dfrac{%s - %s}{ \\dfrac{%s}{\\sqrt{%s}} }\\)",
                  testStat,
                  sdSymbol,
                  oneMeanData[2],
                  input$hypMean,
                  oneMeanData[3],
                  oneMeanData[1]),
          br(),
          br(),
          sprintf("\\(%s = %0.4f\\)",
                  testStat,
                  oneMeanData[6]),
          br(),
          br(),
          br(),
          p(tags$b("Using P-Value Method:")),
          sprintf("\\(P = %s\\)",
                  pValue),
          br(),
          sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
                  pvalSymbol,
                  SigLvl(),
                  reject),
          br(),
          br(),
          br(),
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s %s_{%s} = %s %s_{%s} = %s \\)",
                  OneMeanHypInfo()$critSign,
                  testStat,
                  OneMeanHypInfo()$critAlph,
                  OneMeanHypInfo()$critSign,
                  testStat,
                  OneMeanHypInfo()$alphaVal,
                  critVal),
          br(),
          sprintf("Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
                  testStat,
                  region,
                  reject)
          
        )
      ),
      
      plotOutput('oneMeanHTPlot', width = "75%", height = "300px"),
      br(),
      
      withMathJax(
        p(tags$b("Conclusion:")),
        p(
          sprintf("At the %1.0f%% significance level, the data %s sufficient evidence to reject the null hypothesis \\( (H_{0}) \\) that the population 
                              mean \\( (\\mu) \\) \\( %s %g\\).",
                  SigLvl()*100,
                  suffEvidence,
                  intrpInfo$nullHyp,
                  input$hypMean),
          br(),
        )
      )
    )
  })
  
  
  ##### HT Plot ----
  output$oneMeanHTPlot <- renderPlot({
    
    if(input$dataAvailability == 'Summarized Data') {
      
      if(input$sigmaKnown == 'Known') {
        oneMeanData <- OneMeanZTestSumm()
        sigmaKnown <- 'Known'
        
      } else if(input$sigmaKnown == 'Unknown') {
        oneMeanData <- OneMeanTTestSumm()
        sigmaKnown <- 'Unknown'
      }
    } else if(input$dataAvailability == 'Enter Raw Data') {
      
      if(input$sigmaKnownRaw == 'rawKnown'){
        oneMeanData <- OneMeanZTestRaw()
        sigmaKnown <- 'Known'
        
      } else if(input$sigmaKnownRaw == 'rawUnknown') {
        oneMeanData <- OneMeanTTestRaw()
        sigmaKnown <- 'Unknown'
      }
    } else if(input$dataAvailability == 'Upload Data'){
      
      if(input$sigmaKnownUpload == 'Known'){
        oneMeanData <- OneMeanZTestRaw()
        sigmaKnown <- 'Known'
        
      } else if(input$sigmaKnownUpload == 'Unknown'){
        oneMeanData <- OneMeanTTestRaw()
        sigmaKnown <- 'Unknown'
      } 
    }
    
    intrpInfo <- OneMeanHypInfo()
    
    if(intrpInfo$alternative == "two.sided") {
      critZVal <- paste("\\( \\pm\\)", oneMeanData[4])
      htPlotCritVals <- c(-oneMeanData[4], oneMeanData[4])
      
    } else {
      critZVal <- paste(oneMeanData[4])
      htPlotCritVals <- oneMeanData[4]
    }
    
    if(sigmaKnown== 'Known') {
      oneMeanPlot <- hypZTestPlot(oneMeanData[6], htPlotCritVals, intrpInfo$alternative)
      
    } else {
      oneMeanPlot <- hypTTestPlot(oneMeanData[6], oneMeanData[8], htPlotCritVals, intrpInfo$alternative)
    }
    
    oneMeanPlot
  })
  
  
  
  #### One Prop outputs ----
  
  
  ##### CI ----
  output$onePropCI <- renderUI({
    req(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
    
    oneSampPropZInt <- OnePropZInterval(input$numSuccesses, input$numTrials, ConfLvl())

    p(
      withMathJax(
        sprintf("CI \\(= \\hat{p} \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}}\\)"),
        br(),
        br(),
        sprintf("CI \\(= %s \\pm %0.3f \\sqrt{\\dfrac{%0.3f(1-%0.3f)}{%1.0f}}\\)",
                oneSampPropZInt["phat"],
                oneSampPropZInt["Z Critical"],
                oneSampPropZInt["phat"],
                oneSampPropZInt["phat"],
                input$numTrials),
        br(),
        br(),
        sprintf("CI \\(= (%0.3f, %0.3f)\\)",
                oneSampPropZInt["LCL"],
                oneSampPropZInt["UCL"]),
        br(),
        br(),
        p(tags$b("Interpretation:")),
        sprintf("We are %1.0f%% confident that the population proportion \\( (p) \\) is between \\( %g \\) and \\( %g \\).",
                ConfLvl()*100,
                oneSampPropZInt["LCL"],
                oneSampPropZInt["UCL"])
      )
    )
  })
  
  
  ##### HT ----
  output$onePropHT <- renderUI({
    req(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
    
    oneSampPropZTest <- OnePropZTest(input$numSuccesses, input$numTrials, input$hypProportion, OneMeanHypInfo()$alternative, SigLvl())
    
    if(OneMeanHypInfo()$alternative == "two.sided") {
      critZVal <- paste("\\pm", oneSampPropZTest["Z Critical"])
    } else {
      critZVal <- paste(oneSampPropZTest["Z Critical"])
    }
    
    if(oneSampPropZTest["P-Value"] < 0.0001) {
      pValue <- "P \\lt 0.0001"
    } else {
      pValue <- paste("P = ", oneSampPropZTest["P-Value"])
    }
    
    if(oneSampPropZTest["P-Value"] > SigLvl()) {
      pvalSymbol <- "\\( \\gt\\)"
      suffEvidence <- "do not provide"
      reject <- "do not reject"
      region <- "acceptance"
    } else {
      pvalSymbol <- "\\( \\leq\\)"
      suffEvidence <- "provide"
      reject <- "reject"
      region <- "rejection"
    }
    
    p(
      withMathJax(
        sprintf("\\( H_{0}: p %s %g\\)",
                OneMeanHypInfo()$nullHyp,
                input$hypProportion),
        br(),
        sprintf("\\( H_{a}: p %s %g\\)",
                OneMeanHypInfo()$altHyp,
                input$hypProportion),
        br(),
        br(),
        sprintf("\\( \\alpha = %g \\)",
                SigLvl()),
        br(),
        br(),
        br(),
        sprintf("\\(z = \\dfrac{\\hat{p} - p_{0}}{ \\sqrt{ \\dfrac{p_{0}(1 - p_{0})}{n} } }\\)"),
        br(),
        br(),
        sprintf("\\(z = \\dfrac{%0.3f - %0.3f}{ \\sqrt{ \\dfrac{%0.3f(1 - %0.3f)}{%1.0f} } }\\)",
                oneSampPropZTest["Sample Proportion"],
                input$hypProportion,
                input$hypProportion,
                input$hypProportion,
                input$numTrials),
        br(),
        br(),
        sprintf("\\(z = %0.4f\\)",
                oneSampPropZTest["Test Statistic"]),
        br(),
        br(),
        br(),
        p(tags$b("Using P-Value Method:")),
        sprintf("\\( %s \\)",
                pValue),
        br(),
        sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
                pvalSymbol,
                SigLvl(),
                reject),
        br(),
        br(),
        br(),
        p(tags$b("Using Critical Value Method:")),
        sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$critAlph,
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$alphaVal,
                critZVal),
        br(),
        sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                region,
                reject),
        br(),
        br(),
        plotOutput('onePropHTPlot', width = "75%", height = "300px"),
        br(),
        p(tags$b("Conclusion:")),
        sprintf("At the %1.0f%% level, the data %s sufficient evidence to reject the null hypothesis \\( (H_{0}) \\) that the population 
                              proportion \\( (p) \\) \\( %s %g \\).",
                SigLvl()*100,
                suffEvidence,
                OneMeanHypInfo()$nullHyp,
                input$hypProportion),
        br()
      )
    )
  })
  
  ##### HT Plot ----
  output$onePropHTPlot <- renderPlot({
    
    oneSampPropZTest <- OnePropZTest(input$numSuccesses, input$numTrials, input$hypProportion, OneMeanHypInfo()$alternative, SigLvl())
    
    if(OneMeanHypInfo()$alternative == "two.sided") {
      htPlotCritVals <- c(-oneSampPropZTest["Z Critical"], oneSampPropZTest["Z Critical"]) 
    } else {
      htPlotCritVals <- oneSampPropZTest["Z Critical"]
    }
    
    htPlot <- hypZTestPlot(oneSampPropZTest["Test Statistic"], htPlotCritVals, OneMeanHypInfo()$alternative)
    htPlot
  })
  
  
  
  #### Ind Means outputs ----
  
  
  ##### CI ----
  output$indMeansCI <- renderUI({
    
    if(IndMeansSigmaKnown() == 'bothKnown'){
      cInt <- IndMeansZInt() 
      sdSymbol <- "\\sigma"
      testStat <- "z"
    }
    else if(IndMeansSigmaKnown() == 'bothUnknown'){
      cInt <- IndMeansTInt()
      sdSymbol <- "s"
      testStat <- "t"
    }
    
    tagList(
      
      p(
        withMathJax(
          conditionalPanel(
            condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothKnown') 
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothKnown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothKnown')",
            
            uiOutput('sigmaKnownCIFormula')
          ),
          conditionalPanel(
            condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown') 
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown')",
            
            uiOutput('sigmaUnknownCIFormula')
          ),
          br(),
          sprintf("\\( \\quad = (%g, %g)\\)",
                  cInt["LCL"],
                  cInt["UCL"]),
          br(),
          br(),
          br(),
          p(tags$b("Interpretation:")),
          sprintf("We are %1.0f%% confident that the difference in population means \\( (\\mu_{1} - \\mu_{2}) \\) is between \\( %g \\) and \\( %g \\).",
                  ConfLvl()*100,
                  cInt["LCL"],
                  cInt["UCL"]),
          br()
        )
      )
      
    )
    
    
  })
  
  
  output$sigmaKnownCIFormula <- renderUI({
    
    if (input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data') {
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    zInt <- IndMeansZInt()
    
    tagList(
      
      p(
        withMathJax(
          sprintf("\\( CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm z_{\\alpha/2} \\cdot \\sqrt{ \\dfrac{\\sigma_{1}^2}{n_{1}} + \\dfrac{\\sigma_{2}^2}{n_{2}} } \\)"),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = (%g - %g) \\pm \\left( %g \\cdot \\sqrt{ \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} } \\right) \\)",
                  data$xbar1,
                  data$xbar2,
                  zInt['Z Critical'],
                  data$sd1,
                  data$n1,
                  data$sd2,
                  data$n2),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = %s \\pm \\left( %g \\cdot %g \\right) \\)",
                  zInt['Difference of means'],
                  zInt['Z Critical'],
                  zInt['Std Error']),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = %s \\pm %g \\)",
                  zInt['Difference of means'],
                  zInt['ME'])
        )
      )
      
    )
  })
  
  
  output$sigmaUnknownCIFormula <- renderUI({
    
    if (input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data') {
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    tInt <- IndMeansTInt()
    
    if(data$sigmaEqual) {
      sp <- round(sqrt(((data$n1-1) * data$sd1^2 + (data$n2-1) * data$sd2^2) / (data$n1 + data$n2 - 2)), 4)
      
      tagList(
        withMathJax(
          sprintf("\\( CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm t_{\\alpha/2, \\, n_{1} + n_{2} - 2} \\cdot s_{p} \\sqrt{ \\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}} } \\)"),
          br(),
          br(),
          p(tags$b("where")),
          sprintf("\\( \\displaystyle \\qquad s_{p} = \\sqrt{\\dfrac{(n_{1} - 1)s_{1}^2 + (n_{2} - 1)s_{2}^2}{n_{1} + n_{2} - 2}} \\)"),
          sprintf("\\( = \\sqrt{\\dfrac{(%g - 1)%g + (%g - 1)%g}{%g + %g - 2}} = %g \\)",
                  data$n1,
                  data$sd1^2,
                  data$n2,
                  data$sd2^2,
                  data$n1,
                  data$n2,
                  sp),
          br(),
          p(tags$b("and")),
          sprintf("\\( \\qquad t_{\\alpha/2, \\, n_{1} + n_{2} - 2} = t_{%g, \\, %g} = %g \\)",
                  (1 - ConfLvl()) / 2,
                  tInt['df'],
                  tInt['T Critical']),
          br(),
          br(),
          br(),
          br(),
          sprintf("\\( \\displaystyle CI = (%g - %g) \\pm \\left( %g \\cdot %g \\sqrt{ \\dfrac{1}{%g} + \\dfrac{1}{%g} } \\right) \\)",
                  data$xbar1,
                  data$xbar2,
                  tInt['T Critical'],
                  sp,
                  data$n1,
                  data$n2),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = %g \\pm \\left( %g \\cdot %g \\right) \\)",
                  tInt['Difference of means'],
                  tInt['T Critical'],
                  tInt['Std Error']),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = %g \\pm %g \\)",
                  tInt['Difference of means'],
                  tInt['ME'])
          
        )
      )
    } else {
      
      tagList(
        withMathJax(
          sprintf("\\( CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm t_{\\alpha/2, \\, \\nu} \\cdot \\sqrt{ \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} } \\)"),
          br(),
          br(),
          p(tags$b("where")),
          sprintf("\\( \\displaystyle \\qquad \\nu = \\: \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }
                    { \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} \\right)^2 }{n_{1} - 1} + \\dfrac{ \\left( \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }{n_{2} - 1} } \\)"),
          sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} \\right)^2 }
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
                  data$n2),
          sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( %g + %g \\right)^2 }
                    { \\dfrac{ %g^2 }{%g} + \\dfrac{ %g^2 }{%g} } \\)",
                  (data$sd1^2) / data$n1,
                  (data$sd2^2) / data$n2,
                  (data$sd1^2) / data$n1,
                  data$n1 - 1,
                  (data$sd2^2) / data$n2,
                  data$n2 - 1),
          sprintf("\\( \\: = \\: %g \\)",
                  tInt['df']),
          br(),
          p(tags$b("and")),
          sprintf("\\( \\qquad t_{\\alpha/2, \\, \\nu} = t_{%g, \\, %g} = %g \\)",
                  (1- ConfLvl()) / 2,
                  tInt['df'],
                  tInt['T Critical']),
          br(),
          br(),
          br(),
          br(),
          sprintf("\\( CI = (%g - %g) \\pm \\left( %g \\cdot \\sqrt{ \\dfrac{%g}{%g} + \\dfrac{%g}{%g} } \\right) \\)",
                  data$xbar1,
                  data$xbar2,
                  tInt['T Critical'],
                  data$sd1,
                  data$n1,
                  data$sd2,
                  data$n2),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = %g \\pm \\left( %g \\cdot %g \\right) \\)",
                  tInt['Difference of means'],
                  tInt['T Critical'],
                  tInt['Std Error']),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = %g \\pm %g \\)",
                  tInt['Difference of means'],
                  tInt['ME'])
        )
      )
    }
    
    
  })
  
  
  ##### HT ----
  output$indMeansHT <- renderUI({
    
    withMathJax()
    
    intrpInfo <- IndMeansHypInfo()
    
    if (input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data') {
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    #get test type and results based on sigma known/unknown
    if(IndMeansSigmaKnown() == 'bothKnown'){
      hTest <- IndMeansZTest() 
      testStat <- "z"
      critValDF <- paste(intrpInfo$critSign, "z_{", intrpInfo$critAlph, "} = ", intrpInfo$critSign, "z_{", intrpInfo$alphaVal, "}")
    }
    else if(IndMeansSigmaKnown() == 'bothUnknown'){
      hTest <- IndMeansTTest()
      testStat <- "t"
      
      if(data$sigmaEqual) {
        critValDF <- paste(intrpInfo$critSign, "t_{", intrpInfo$critAlph, ", \\, n_{1} + n_{2} - 2} = ", intrpInfo$critSign, "t_{", intrpInfo$alphaVal, ", \\, ", hTest['df'], "}")
      } else {
        critValDF <- paste(intrpInfo$critSign, "t_{", intrpInfo$critAlph, ", \\, \\nu} = ", "\n", intrpInfo$critSign, "t_{", intrpInfo$alphaVal, ", \\, ", hTest['df'], "}")
      }
    }
    
    if(hTest["P-Value"] < 0.0001)
    {
      pValue <- "\\lt 0.0001"
    }
    else
    {
      pValue <- hTest["P-Value"]
    }
    
    if(hTest["P-Value"] > SigLvl())
    {
      pvalSymbol <- "\\gt"
      suffEvidence <- "do not provide"
      reject <- "do not reject"
      region <- "acceptance"
    }
    else
    {
      pvalSymbol <- "\\leq"
      suffEvidence <- "provide"
      reject <- "reject"
      region <- "rejection"
    }
    
    if(intrpInfo$alternative == "two.sided") {
      critVal <- paste("\\pm", hTest[2])
      
    } else {
      critVal <- hTest[2]
    }
    
    tagList(
      
      p(
        withMathJax(
          #h4(tags$u("Performing the Hypothesis Test:")),
          #br(),
          sprintf("\\( H_{0}: \n \\mu_{1} = \\mu_{2}\\)"),
          br(),
          sprintf("\\( H_{a}: \\mu_{1} %s \\mu_{2}\\)",
                  intrpInfo$altHyp),
          br(),
          br(),
          sprintf("\\( \\alpha = %s \\)",
                  SigLvl()),
          br(),
          br(),
          conditionalPanel(
            condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothKnown') 
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothKnown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothKnown')",
            
            uiOutput('sigmaKnownHTFormula')
          ),
          conditionalPanel(
            condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown') 
                         || (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown')
                         || (input.dataAvailability2 == 'Upload Data' && input.bothsigmaKnownUpload == 'bothUnknown')",
            
            uiOutput('sigmaUnknownHTFormula')
          ),
          br(),
          br(),
          br(),
          p(tags$b("Using P-Value Method:")),
          sprintf("\\( P = %s \\)",
                  pValue),
          br(),
          sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
                  pvalSymbol,
                  SigLvl(),
                  reject),
          br(),
          br(),
          br(),
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s = %s\\)",
                  critValDF,
                  critVal),
          br(),
          
          conditionalPanel(
            condition = "(input.dataAvailability2 == 'Summarized Data' && input.bothsigmaKnown == 'bothUnknown' && input.bothsigmaEqual == 'FALSE') || 
                         (input.dataAvailability2 == 'Enter Raw Data' && input.bothsigmaKnownRaw == 'bothUnknown' && input.bothsigmaEqualRaw == 'FALSE')
                         ",

            br(),
            p(tags$b("where")),
            sprintf("\\( \\displaystyle \\qquad \\nu = \\: \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }
                    { \\dfrac{ \\left( \\dfrac{s^2_{1}}{n_{1}} \\right)^2 }{n_{1} - 1} + \\dfrac{ \\left( \\dfrac{s^2_{2}}{n_{2}} \\right)^2 }{n_{2} - 1} } \\)"),
            sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} \\right)^2 }
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
                    data$n2),
            sprintf("\\( \\displaystyle \\: = \\: \\dfrac{ \\left( %0.4f + %0.4f \\right)^2 }
                    { \\dfrac{ %0.4f^2 }{%g} + \\dfrac{ %0.4f^2 }{%g} } = %s\\)",
                    (data$sd1^2) / data$n1,
                    (data$sd2^2) / data$n2,
                    (data$sd1^2) / data$n1,
                    data$n1 - 1,
                    (data$sd2^2) / data$n2,
                    data$n2 - 1,
                    hTest['df']),
            br(),
            br()
          ),
          
          br(),
          sprintf("Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
                  testStat,
                  region,
                  reject),
          br(),
          br()
          
        )
      ),
      
      plotOutput('indMeansHTPlot', width = "75%", height = "300px"),
      br(),
      
      withMathJax(
        p(tags$b("Conclusion:")),
        p(
          sprintf("At the %1.0f%% significance level, the data %s sufficient evidence to reject the null hypothesis
                                \\( (H_{0}) \\) that \\( \\mu_{1} = \\mu_{2} \\).",
                  SigLvl()*100,
                  suffEvidence),
          br(),
        )
      )
    )
  })
  
  
  output$sigmaKnownHTFormula <- renderUI({
    
    if (input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data') {
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    zTest <- IndMeansZTest()
    
    tagList(
      withMathJax(
        sprintf("\\( z = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ \\sqrt{ \\dfrac{\\sigma_{1}^2}{n_{1}} + \\dfrac{\\sigma_{2}^2}{n_{2}} } } \\)"),
        br(),
        br(),
        sprintf("\\( z = \\dfrac{ (%g - %g) - 0}{ \\sqrt{ \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} } } = \\dfrac{%g}{%s} = %0.4f \\)",
                data$xbar1,
                data$xbar2,
                data$sd1,
                data$n1,
                data$sd2,
                data$n2,
                zTest['Difference of means'],
                zTest['Std Error'],
                zTest['Test Statistic']),
        br()
      )
    )
  })
  
  
  output$sigmaUnknownHTFormula <- renderUI({
    
    if (input$dataAvailability2 == 'Summarized Data') {
      data <- IndMeansSummData()
    } else if(input$dataAvailability2 == 'Enter Raw Data') {
      data <- IndMeansRawData()
    } else if(input$dataAvailability2 == 'Upload Data') {
      data <- GetMeansUploadData()
    }
    
    sd1Sqrd <- data$sd1^2
    if( sd1Sqrd >= 0.0001) {
      sd1Sqrd <- round(sd1Sqrd, 4)
    } else {
      sd1Sqrd <- signif(sd1Sqrd, 1)
    }
    
    sd2Sqrd <- data$sd2^2
    if( sd1Sqrd >= 0.0001) {
      sd2Sqrd <- round(sd2Sqrd, 4)
    } else {
      sd2Sqrd <- signif(sd2Sqrd, 1)
    }
    
    tTest <- IndMeansTTest()
    
    if(data$sigmaEqual == TRUE) {
      sp <- round(sqrt(((data$n1-1) * data$sd1^2 + (data$n2-1) * data$sd2^2) / (data$n1 + data$n2 - 2)), 4)
      
      tagList(
        withMathJax(
          sprintf("\\( t = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ s_{p} \\sqrt{ \\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}} } } \\)"),
          br(),
          br(),
          p(tags$b("where")),
          sprintf("\\( \\displaystyle \\qquad s_{p} = \\sqrt{\\dfrac{(n_{1} - 1)s_{1}^2 + (n_{2} - 1)s_{2}^2}{n_{1} + n_{2} - 2}} \\)"),
          sprintf("\\( = \\sqrt{\\dfrac{(%g - 1)%s + (%g - 1)%s}{%g + %g - 2}} = %g \\)",
                  data$n1,
                  sd1Sqrd,
                  data$n2,
                  sd2Sqrd,
                  data$n1,
                  data$n2,
                  sp),
          br(),
          br(),
          br(),
          br(),
          sprintf("\\( t = \\dfrac{ (%g - %g) - 0 }{ %g \\sqrt{ \\dfrac{1}{%g} + \\dfrac{1}{%g} } } \\)",
                  data$xbar1,
                  data$xbar2,
                  sp,
                  data$n1,
                  data$n2),
          sprintf("\\( = \\dfrac{%g}{%s} = %0.4f \\)",
                  tTest['Difference of means'],
                  tTest['Std Error'],
                  tTest['Test Statistic']),
          br()
        )
      )
      
    } else {
      tagList(
        withMathJax(
          sprintf("\\( t = \\dfrac{ (\\bar{x}_{1} - \\bar{x}_{2}) - (\\mu_{1} - \\mu_{2})_{0} }{ \\sqrt{ \\dfrac{s_{1}^2}{n_{1}} + \\dfrac{s_{2}^2}{n_{2}} } } \\)"),
          br(),
          br(),
          sprintf("\\( t = \\dfrac{ (%g - %g) - 0 }{ \\sqrt{ \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} } } = \\dfrac{%g}{%g} = %0.4f \\)",
                  data$xbar1,
                  data$xbar2,
                  data$sd1,
                  data$n1,
                  data$sd2,
                  data$n2,
                  tTest['Difference of means'],
                  tTest['Std Error'],
                  tTest['Test Statistic']),
          br()
        )
      )
    }
    
    
  })
  
  
  ##### HT Plot ----
  output$indMeansHTPlot <- renderPlot({
    
    if(IndMeansSigmaKnown() == 'bothKnown'){
      data <- IndMeansZTest()
    }
    else if(IndMeansSigmaKnown() == 'bothUnknown'){
      data <- IndMeansTTest()
    }

    intrpInfo <- IndMeansHypInfo()

    if(intrpInfo$alternative == "two.sided") {
      htPlotCritVals <- c(-data[2], data[2])

    } else {
      htPlotCritVals <- data[2]
    }

    if(IndMeansSigmaKnown() == 'bothKnown') {
      indMeansPlot <- hypZTestPlot(data['Test Statistic'], htPlotCritVals, intrpInfo$alternative)

    } else if(IndMeansSigmaKnown() == 'bothUnknown'){
      indMeansPlot <- hypTTestPlot(data['Test Statistic'], data['df'], htPlotCritVals, intrpInfo$alternative)
    }

    indMeansPlot
  })
  
  
  
  #### Two Prop outputs ----
  
  
  ##### CI ----
  output$twoPropCI <- renderUI({
    
    
    twoSampPropZInt <- TwoPropZInt(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, ConfLvl())
    
    p(
      withMathJax(
        sprintf("\\( CI = (\\hat{p}_{1} - \\hat{p}_{2}) \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_{1}(1-\\hat{p}_{1})}{n_{1}} + \\dfrac{\\hat{p}_{2}(1-\\hat{p}_{2})}{n_{2}}}\\)"),
        br(),
        br(),
        br(),
        sprintf("\\( CI = %s \\pm %0.3f \\sqrt{\\dfrac{%0.3f(1-%0.3f)}{%1.0f} + \\dfrac{%0.3f(1-%0.3f)}{%1.0f}}\\)",
                twoSampPropZInt["Difference of proportions"],
                twoSampPropZInt["Z Critical"],
                twoSampPropZInt["Sample Proportion 1"],
                twoSampPropZInt["Sample Proportion 1"],
                input$numTrials1,
                twoSampPropZInt["Sample Proportion 2"],
                twoSampPropZInt["Sample Proportion 2"],
                input$numTrials2),
        br(),
        br(),
        sprintf("\\( \\quad = %s \\pm %g \\cdot %g \\)",
                twoSampPropZInt["Difference of proportions"],
                twoSampPropZInt["Z Critical"],
                twoSampPropZInt["Std Error"]),
        br(),
        br(),
        sprintf("\\( \\quad = %s \\pm %g \\)",
                twoSampPropZInt["Difference of proportions"],
                twoSampPropZInt["Margin of Error"]),
        br(),
        br(),
        sprintf("\\( \\quad = (%0.3f, %0.3f)\\)",
                twoSampPropZInt["LCL"],
                twoSampPropZInt["UCL"]),
        br(),
        br(),
        br(),
        p(tags$b("Interpretation:")),
        sprintf("We are %1.0f%% confident that the difference in population proportions \\( (p_{1} - p_{2}) \\) is between \\( %0.3f \\) and \\( %0.3f \\).",
                ConfLvl()*100,
                twoSampPropZInt["LCL"],
                twoSampPropZInt["UCL"])
      )
    )
  })
  
  
  
  ##### HT ----
  output$twoPropHT <- renderUI({
    
    
    
    twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, 0, IndMeansHypInfo()$alternative, SigLvl())
    
    if(twoPropZTest["P-Value"] < 0.0001)
    {
      pValue <- "P \\lt 0.0001"
    }
    else
    {
      pValue <- paste("P = ", twoPropZTest["P-Value"])
    }
    
    if(IndMeansHypInfo()$alternative == "two.sided")
    {
      critZVal <- paste("\\pm", twoPropZTest["Z Critical"])
      htPlotCritVals <- c(-twoPropZTest["Z Critical"], twoPropZTest["Z Critical"])
    }
    else
    {
      critZVal <- paste(twoPropZTest["Z Critical"])
      htPlotCritVals <- twoPropZTest["Z Critical"]
    }
    
    propDiff <- twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"]
    
    if(twoPropZTest["P-Value"] > SigLvl())
    {
      pvalSymbol <- "\\gt"
      suffEvidence <- "do not provide"
      reject <- "do not reject"
      region <- "acceptance"
    }
    else
    {
      pvalSymbol <- "\\leq"
      suffEvidence <- "provide"
      reject <- "reject"
      region <- "rejection"
    }
    
    p(
      withMathJax(
        sprintf("\\( H_{0}: p_{1} %s p_{2}\\)",
                IndMeansHypInfo()$nullHyp),
        br(),
        sprintf("\\( H_{a}: p_{1} %s p_{2}\\)",
                IndMeansHypInfo()$altHyp),
        br(),
        br(),
        sprintf("\\( \\alpha = %g \\)",
                SigLvl()),
        br(),
        br(),
        br(),
        sprintf("\\(z = \\dfrac{ (\\hat{p}_{1} - \\hat{p}_{2}) - (p_{1} - p_{2})_{0} }{\\sqrt{\\hat{p}(1-\\hat{p})(\\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}})}}\\)"),
        br(),
        br(),
        p(tags$b("where")),
        sprintf("\\( \\displaystyle \\qquad \\hat{p} = \\dfrac{x_{1} + x_{2}}{n_{1} + n_{2}} \\)"),
        sprintf("\\( = \\dfrac{%g + %g}{%g + %g} = %g \\)",
                input$numSuccesses1,
                input$numSuccesses2,
                input$numTrials1,
                input$numTrials2,
                twoPropZTest["Pooled Proportion"]),
        br(),
        br(),
        br(),
        sprintf("\\(z = \\dfrac{ (%g - %g) - 0}{\\sqrt{%g(1-%g)(\\dfrac{1}{%g} + \\dfrac{1}{%g})}}\\)",
                twoPropZTest["Sample Proportion 1"],
                twoPropZTest["Sample Proportion 2"],
                twoPropZTest["Pooled Proportion"],
                twoPropZTest["Pooled Proportion"],
                input$numTrials1,
                input$numTrials2),
        sprintf("\\( = \\dfrac{%g}{%g} = %0.4f\\)",
                twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"],
                twoPropZTest["Std Error"],
                twoPropZTest["Test Statistic"]),
        br(),
        br(),
        sprintf("\\(z = %0.4f\\)",
                twoPropZTest["Test Statistic"]),
        br(),
        br(),
        br(),
        br(),
        p(tags$b("Using P-Value Method:")),
        sprintf("\\( %s \\)",
                pValue),
        br(),
        sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
                pvalSymbol,
                SigLvl(),
                reject),
        br(),
        br(),
        br(),
        p(tags$b("Using Critical Value Method:")),
        sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                IndMeansHypInfo()$critSign,
                IndMeansHypInfo()$critAlph,
                IndMeansHypInfo()$critSign,
                IndMeansHypInfo()$alphaVal,
                critZVal),
        
        br(),
        sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                region,
                reject),
        br(),
        br(),
        plotOutput('twoPropHTPlot'),
        br(),
        p(tags$b("Conclusion:")),
        sprintf("At the %1.0f%% significance level, the data %s sufficient evidence to reject the null hypothesis \\( (H_{0}) \\) that the population 
                              proportion \\( p_{1} %s p_{2}\\).",
                SigLvl()*100,
                suffEvidence,
                IndMeansHypInfo()$nullHyp),
        br()
      )
    )
  })
  
  
  ##### HT Plot ----
  output$twoPropHTPlot <- renderPlot({
    
    twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, 0, IndMeansHypInfo()$alternative, SigLvl())
    
    if(IndMeansHypInfo()$alternative == "two.sided")
    {
      htPlotCritVals <- c(-twoPropZTest["Z Critical"], twoPropZTest["Z Critical"])
    }
    else
    {
      htPlotCritVals <- twoPropZTest["Z Critical"]
    }
    
    htPlot <- hypZTestPlot(twoPropZTest["Test Statistic"], htPlotCritVals, IndMeansHypInfo()$alternative)
    htPlot
  })
  
  # --------------------------------------------------------------------- #
  
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  observeEvent(input$oneMeanUserData, priority = 5, {
    hide(id = "inferenceData")
    hide(id = "oneMeanVariable")
    # if(onemeanupload_iv$is_valid())
    # {
      freezeReactiveValue(input, "oneMeanVariable")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "oneMeanVariable",
                        choices = c(colnames(OneMeanUploadData()))
      )

      show(id = "oneMeanVariable")
    # }
  })
  
  observeEvent(input$indMeansUserData, priority = 5, {
    hide(id = "inferenceData")
    hide(id = "indMeansUplSample1")
    hide(id = "indMeansUplSample2")
    # if(onemeanupload_iv$is_valid())
    # {
    freezeReactiveValue(input, "indMeansUplSample1")
    updateSelectInput(session = getDefaultReactiveDomain(),
                      "indMeansUplSample1",
                      choices = c(colnames(IndMeansUploadData()))
    )
    
    freezeReactiveValue(input, "indMeansUplSample2")
    updateSelectInput(session = getDefaultReactiveDomain(),
                      "indMeansUplSample2",
                      choices = c(colnames(IndMeansUploadData()))
    )
    show(id = "indMeansUplSample1")
    show(id = "indMeansUplSample2")
    # }
  })
  
  observeEvent(input$depMeansUserData, priority = 5, {
    hide(id = "inferenceData")
    hide(id = "depMeansUplSample1")
    hide(id = "depMeansUplSample2")
    
    if(depmeansupload_iv$is_valid()) {
      freezeReactiveValue(input, "depMeansUplSample1")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "depMeansUplSample1",
                        choices = c(colnames(DepMeansUploadData()))
      )
  
      freezeReactiveValue(input, "depMeansUplSample2")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "depMeansUplSample2",
                        choices = c(colnames(DepMeansUploadData()))
      )
      show(id = "depMeansUplSample1")
      show(id = "depMeansUplSample2")
    }
  })
  
  
  observeEvent(input$goInference, {
    #output$renderInference <- renderDataTable(

    if(si_iv$is_valid()) {
      show(id = "inferenceData")
      
    } else {
      hide(id = "inferenceData")
    }
    
    if(input$samplesSelect == '1'){

      if(input$popuParameter == 'Population Proportion') {
        req(input$numTrials && input$numSuccesses)
        if(input$numTrials < input$numSuccesses) {
          hide(id = "inferenceData")
        }
      } # input$popuParameter == 'Population Proportion'
    } # one sample
    
    else if(input$samplesSelect == '2') {
 
      if(input$popuParameters == 'Population Proportions') {
        req(input$numSuccesses1 && input$numTrials1)
        req(input$numSuccesses2 && input$numTrials2)
        
        if(input$numSuccesses1 > input$numTrials1 | input$numSuccesses2 > input$numTrials2) {
         hide(id = 'inferenceData') 
        }
        
      }
    }
    #) # renderInference
  }) # input$goInference
  
  
  
  # --------------------------------------------------------------------- #
  
  
  # **************************************************************************** #
  
  
  #  -------------------------------------------------------------------- #
  ## ----------- Linear Regression and Correlation functions ------------
  #  -------------------------------------------------------------------- #
  
  
  ### Reactives ----
  # --------------------------------------------------------------------- #
  
  slrUploadData <- eventReactive(input$slrUserData, {
    ext <- tools::file_ext(input$slrUserData$name)
    
    switch(ext, 
           csv = read_csv(input$slrUserData$datapath, show_col_types = FALSE),
           xls = read_excel(input$slrUserData$datapath),
           xlsx = read_excel(input$slrUserData$datapath),
           txt = read_tsv(input$slrUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format")
    )
  })
  
  sampleDiffRaw <- eventReactive({input$x
    input$y}, {
      datx <- createNumLst(input$x)
      daty <- createNumLst(input$y)
      return(length(datx) - length(daty))
    })
  
  sampleDiffUpload <- eventReactive (c(input$slrExplanatory, 
                                       input$slrResponse), {
                                         if(input$slrResponse == "" | input$slrExplanatory == "")
                                         {
                                           return()
                                         }
                                         else
                                         {
                                           datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
                                           daty <- as.data.frame(slrUploadData())[, input$slrResponse]
                                           difference <- length(na.omit(datx)) - length(na.omit(daty))
                                           return(difference)
                                         }
                                       })
  
  # --------------------------------------------------------------------- #
  
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  observeEvent(input$slrUserData, {
    hide(id = "RegCorMP")
    hide(id = "slrResponse")
    hide(id = "slrExplanatory")
    if(slrupload_iv$is_valid())
    {
      freezeReactiveValue(input, "slrExplanatory")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "slrExplanatory",
                        choices = c(colnames(slrUploadData()))
      )
      freezeReactiveValue(input, "slrResponse")
      updateSelectInput(session = getDefaultReactiveDomain(),
                        "slrResponse",
                        choices = c(colnames(slrUploadData()))
      )
      show(id = "slrResponse")
      show(id = "slrExplanatory")
    }
  })
  
  observeEvent(input$slrExplanatory, {
    updateTextInput(inputId = "xlab", value = input$slrExplanatory)
  })
  
  observeEvent(input$slrResponse, {
    updateTextInput(inputId = "ylab", value = input$slrResponse)
  })
  
  
  observeEvent(input$goRegression, {
    
    if(input$simple_vs_multiple == 'SLR')
    {
      if(!slrupload_iv$is_valid())
      {
        output$slrTabs <- renderUI({
          validate(
            need(input$slrUserData, "Please upload your data to continue"),
            need(nrow(slrUploadData()) != 0, "File is empty"),
            need(ncol(slrUploadData()) > 1, "Data must include one response and (at least) one explanatory variable"),
            need(nrow(slrUploadData()) > 2, "Samples must include at least 2 observations"),
            errorClass = "myClass"
          )
        })
      }
      else if(!slruploadvars_iv$is_valid())
      {
        output$slrTabs <- renderUI({
          validate(
            need(input$slrExplanatory != "", "Please select an explanatory variable (x)"),
            need(input$slrResponse != "", "Please select a response variable (y)") %then%
              need(sampleDiffUpload() == 0, "x and y must have the same number of observations"),
            
            errorClass = "myClass"
          )
        })
      }
      else
      {
        if(input$dataRegCor == 'Upload Data')
        {
          datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
          daty <- as.data.frame(slrUploadData())[, input$slrResponse]
        }
        else
        {
          datx <- createNumLst(input$x)
          daty <- createNumLst(input$y)
        }
        
        if(regcor_iv$is_valid())
        {
          output$slrTabs <- renderUI({ ####tab generation ----
            
            tagList(
              
              tabsetPanel(id = "slrTabset", selected = "Simple Linear Regression",
                          
                          tabPanel(id = "slr", title = "Simple Linear Regression",
                                   
                                   conditionalPanel(
                                     condition = "input.scatterPlot == 1",
                                     
                                     titlePanel("Scatterplot"),
                                     plotOutput("scatterplot", width = "500px"),
                                     br(),
                                     hr(),
                                   ),
                                   
                                   titlePanel("Data"),
                                   br(),
                                   DTOutput("slrDataTable", width = "750px"),
                                   br(),
                                   hr(),
                                   
                                   titlePanel("Estimated equation of the regression line"),
                                   br(),
                                   uiOutput('regLineEquation'),
                                   verbatimTextOutput("linearRegression"),
                                   br(),
                                   hr(),
                                   
                                   titlePanel("95% confidence interval for regression parameters"),
                                   br(),
                                   verbatimTextOutput("confintLinReg"),
                                   br(),
                                   hr(),
                                   
                                   titlePanel("ANOVA for regression"),
                                   br(),
                                   verbatimTextOutput("anovaLinReg"),
                                   #br(),
                          ), 
                          
                          tabPanel(id = "normality", title = "Normality of Residuals",
                                   
                                   #----------------------------------#
                                   # Tests for normality of residuals #
                                   #----------------------------------#
                                   titlePanel("Anderson-Darling test"),
                                   verbatimTextOutput("AndersonDarlingTest"),
                                   br(),
                                   
                                   titlePanel("Kolmogorov-Smirnov test"),
                                   verbatimTextOutput("KolmogorovSmirnovTest"),
                                   br(),
                                   
                                   titlePanel("Shapiro-Wilk test"),
                                   verbatimTextOutput("ShapiroTest"),
                                   #br(),
                          ),
                          
                          tabPanel(id = "resid", title = "Residual Plots",
                                   #-----------------------------#
                                   # Plots for Residual Analysis #
                                   #-----------------------------#
                                   titlePanel("Q-Q plot"),
                                   plotOutput("qqplot", width = "500px"),
                                   #br(),
                                   
                                   titlePanel("Other diagnostic plots"),
                                   plotOutput("moreplots", width = "500px"),
                                   #br(),
                          ),
                          
                          tabPanel(id = "correlation", title = "Correlation Analysis",
                                   
                                   #----------------------------------#
                                   # Correlation Coefficient Analysis #
                                   #----------------------------------#
                                   titlePanel("Pearson's Product-Moment Correlation"),
                                   br(),
                                   br(),
                                   uiOutput('pearsonCorFormula'),
                                   br(),
                                   verbatimTextOutput("PearsonCorTest"),
                                   br(),
                                   verbatimTextOutput("PearsonConfInt"),
                                   br(),
                                   hr(),

                                   titlePanel("Kendall's Rank Correlation"),
                                   br(),
                                   uiOutput("kendallEstimate"),
                                   br(),
                                   hr(),
                                   
                                   titlePanel("Spearman's Rank Correlation"),
                                   br(),
                                   uiOutput("spearmanEstimate"),
                                   br(),
                                   br()

                          ),
              ),
            )
          })
          
          model <- lm(daty ~ datx)
          
          main <- input$main
          xlab <- input$xlab
          ylab <- input$ylab
          
          df <- data.frame(datx, daty, datx*daty, datx^2, daty^2)
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
            ) %>% formatStyle(
              names(dfTotaled),
              target = 'row',
              fontWeight = styleRow(dim(dfTotaled)[1], "bold")
            )
          )
          
          output$scatterplot <- renderPlot({
            plot(datx, daty, main = main, xlab = xlab, ylab = ylab, pch = 19) +
              abline(lm(daty ~ datx), col = "blue")
          })
          
          output$regLineEquation <- renderUI({
            withMathJax()
            p(
              withMathJax(),
              p(tags$b("The equation of the regression line is given by ")),
              sprintf("\\( \\qquad \\hat{y} = \\hat{\\beta}_{0} + \\hat{\\beta}_{1} x \\)"),
              br(),
              br(),
              p(tags$b("where")),
              sprintf("\\( \\qquad \\hat{\\beta}_{1} = \\dfrac{ \\sum xy - \\dfrac{ (\\sum x)(\\sum y) }{ n } }{ \\sum x^2 - \\dfrac{ (\\sum x)^2 }{ n } } \\)"),
              sprintf("\\( \\, = \\, \\dfrac{ %g - \\dfrac{ (%g)(%g) }{ %g } }{ %g - \\dfrac{ (%g)^2 }{ %g } } \\)",
                      dfTotaled["Totals", "xy"],
                      dfTotaled["Totals", "x"],
                      dfTotaled["Totals", "y"],
                      length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"],
                      dfTotaled["Totals", "x"],
                      length(datx)),
              sprintf("\\( \\, = \\, \\dfrac{ %g - \\dfrac{ %g }{ %g } }{ %g - \\dfrac{ %g }{ %g } } \\)",
                      dfTotaled["Totals", "xy"],
                      sumXSumY,
                      length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"],
                      sumXSqrd,
                      length(datx)),
              sprintf("\\( \\, = \\, \\dfrac{ %g - %g }{ %g - %g } \\)",
                      dfTotaled["Totals", "xy"],
                      sumXSumY / length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"],
                      sumXSqrd / length(datx)),
              sprintf("\\( \\, = \\, \\dfrac{ %g }{ %g } \\)",
                      dfTotaled["Totals", "xy"] - (sumXSumY) / length(datx),
                      dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx)),
              sprintf("\\( \\, = \\, %0.4f \\)",
                      summary(model)$coefficients["datx", "Estimate"] ),
              br(),
              br(),
              p(tags$b("and")),
              sprintf("\\( \\qquad \\hat{\\beta}_{0} = \\bar{y} - \\hat{\\beta}_{1} \\bar{x}\\)"),
              sprintf("\\( \\, = \\, %g - %0.4f (%g) \\)",
                      mean(daty),
                      summary(model)$coefficients["datx", "Estimate"],
                      mean(datx)),
              sprintf("\\( \\, = \\, %g - %0.4f\\)",
                      mean(daty),
                      summary(model)$coefficients["datx", "Estimate"] * mean(datx)),
              sprintf("\\( \\, = \\, %0.4f \\)",
                      summary(model)$coefficients["(Intercept)", "Estimate"]),
              br(),
              br(),
              br(),
              sprintf("\\( \\hat{y} = %0.4f + %0.4f x \\)",
                      summary(model)$coefficients["(Intercept)", "Estimate"],
                      summary(model)$coefficients["datx", "Estimate"]),
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
          
          #----------------------------------#
          # Tests for normality of residuals #
          #----------------------------------#
          
          # Anderson-Darling Normality Test 
          output$AndersonDarlingTest <- renderPrint({ 
            ad.test(model$residuals)
          })
          
          # Kolmogorov-Smirnov Normality Test 
          output$KolmogorovSmirnovTest <- renderPrint({ 
            ks.test(model$residuals, "pnorm")
          })
          
          # Shapiro-Wilk Normality Test 
          output$ShapiroTest <- renderPrint({ 
            shapiro.test(model$residuals) 
          })
          
          # Q-Q plot for residuals
          output$qqplot <- renderPlot({
            #qqnorm(model$residuals, ylab = "Residuals", xlab = "Z Scores", main = "Q-Q plot of Standardized Residuals", pch = 19) #+
            #qqline(model$residuals)
            qqPlot(model$residuals, main = "Q-Q Plot", xlab = "Z Scores",  ylab = "Residuals", pch = 19) 
          })
          
          output$moreplots <- renderPlot({
            par(mfrow = c(2, 2))
            plot(model, which = 1:4, pch = 19)
          })
          
          
          req(length(datx) > 1) ## correlation coefficient ----
          if(length(datx) > 2)
          {
            pearson <- cor.test(datx, daty, method = "pearson")
            
            output$pearsonCorFormula <- renderUI({
              p(
                withMathJax(),
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
                       length(datx),
                       dfTotaled["Totals", "x<sup>2</sup>"],
                       dfTotaled["Totals", "x"],
                       length(datx),
                       dfTotaled["Totals", "y<sup>2</sup>"],
                       dfTotaled["Totals", "y"],
                       length(datx)),
                br(),
                br(),
                br(),
                sprintf("\\( \\quad = \\; \\dfrac
                                        {%g - \\dfrac{ %g }{ %g } }
                                        {\\sqrt{ %g - \\dfrac{ %g }{ %g } } \\sqrt{ %g - \\dfrac{ %s }{ %g } } } \\)",
                        dfTotaled["Totals", "xy"],
                        sumXSumY,
                        length(datx),
                        dfTotaled["Totals", "x<sup>2</sup>"],
                        sumXSqrd,
                        length(datx),
                        dfTotaled["Totals", "y<sup>2</sup>"],
                        sumYSqrd,
                        length(datx)),
                br(),
                br(),
                br(),
                sprintf("\\( \\quad = \\; \\dfrac
                                        {%g - %g }
                                        {\\sqrt{ %g - %g } \\sqrt{ %g - %g } } \\)",
                        dfTotaled["Totals", "xy"],
                        sumXSumY / length(datx),
                        dfTotaled["Totals", "x<sup>2</sup>"],
                        sumXSqrd / length(datx),
                        dfTotaled["Totals", "y<sup>2</sup>"],
                        sumYSqrd / length(datx)),
                br(),
                br(),
                br(),
                sprintf("\\( \\quad = \\; \\dfrac
                                        { %g }
                                        {\\sqrt{ %g } \\sqrt{ %g } } \\)",
                        dfTotaled["Totals", "xy"] - sumXSumY / length(datx),
                        dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx),
                        dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx)),
                br(),
                br(),
                br(),
                sprintf("\\( \\quad = \\; \\dfrac
                                        { %g }
                                        { (%g) (%g) } \\)",
                        dfTotaled["Totals", "xy"] - sumXSumY / length(datx),
                        sqrt(dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx)),
                        sqrt(dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx))),
                br(),
                br(),
                br(),
                sprintf("\\( \\quad = \\; \\dfrac
                                        { %g }
                                        { %g } \\)",
                        dfTotaled["Totals", "xy"] - sumXSumY / length(datx),
                        sqrt(dfTotaled["Totals", "x<sup>2</sup>"] - sumXSqrd / length(datx)) * sqrt(dfTotaled["Totals", "y<sup>2</sup>"] - sumYSqrd / length(datx))),
                br(),
                br(),
                br(),
                sprintf("\\( \\quad = \\; %0.4f \\)",
                        pearson$estimate),
                br(),
              )
              
            })
            
            output$PearsonCorTest <- renderPrint({ 
              pearson
            })
            
            if(length(datx) > 3)
            {
              output$PearsonConfInt <- renderPrint({ 
                pearson$conf.int
              })
            }
            else
            {
              output$PearsonConfInt <- renderPrint ({
                noquote("Computation of the Confidence Interval requires a minimum sample size of 4")
              })
            }
            
            # output$PearsonEstimate <- renderPrint({
            #   cat(noquote(paste(c("Pearson's r:", round(pearson$estimate[[1]], 4)))))
            # })
          }
          else
          {
            output$PearsonCorTest <- renderPrint ({
              noquote("Pearson's Product-Moment Correlation requires a minimum sample size of 3 for computation")
            })
          }
          
          kendall <- cor.test(datx, daty, method = "kendall")
          spearman <- cor.test(datx, daty, method = "spearman")
          
          output$kendallEstimate <- renderUI({
            sprintf("\\( \\tau = %0.4f \\)", 
                    kendall$estimate)
          })
          
          output$spearmanEstimate <- renderUI({
            sprintf("\\( r_{s} = %0.4f \\)", 
                    spearman$estimate)
          })
          
        } #if regcor_iv is valid
        else
        {
          output$slrTabs <- renderUI({
            
            validate(
              need(length(datx) >= 2, "Must have at least 2 observations for x"),
              need(length(daty) >= 2, "Must have at least 2 observations for y"),
              need(!anyNA(datx), "Data must be numeric"),
              need(!anyNA(daty), "Data must be numeric"),
              need(length(datx) == length(daty), "x and y must have the same number of observations"),
              
              errorClass = "myclass"
            )
          })
        }
      }
    }
    
    #cut here
    show(id = "RegCorMP")
    
  }) # input$goRegression
  
  # --------------------------------------------------------------------- #
  
  
  # **************************************************************************** #
  
  
  #  -------------------------------------------------------------------- #
  #  ------------------------ Component Display -------------------------
  #  -------------------------------------------------------------------- #
  
  
  #### Descriptive Statistics ----
  #  -------------------------------------------------------------------- #
  
  observeEvent(!ds_iv$is_valid(), {
    hide(id = "descriptiveStatsMP")
  })
  
  observeEvent({input$descriptiveStat
    input$dsUploadVars}, {
      hide(id = 'descrStatsData')                
    })
  
  observeEvent(input$dataInput, {
    hide(id = 'descrStatsData')
    hide(id = 'dsUploadVars')
  })
  
  observe({
    if(is.null(input$dsGraphOptions)) {
      hideTab(inputId = 'dsTabset', target = 'Graphs')
      updateTabsetPanel(inputId = 'dsTabset', selected = 'Descriptive Statistics')
    } else {
      showTab(inputId = 'dsTabset', target = 'Graphs')
    }
  })
  
  observeEvent(input$goDescpStats, {
    show(id = 'descriptiveStatsMP')
  })
  
  observeEvent(input$resetAll,{
    hide(id = 'descriptiveStatsMP')
    shinyjs::reset("descriptiveStatsPanel")
  })
  
  #  -------------------------------------------------------------------- #
  
  
  
  #### Probability Distributions ----
  #  -------------------------------------------------------------------- #
  
  observeEvent(!pd_iv$is_valid(), {
    hide(id = 'probabilityMP')
  })
  
  #-----------------------#
  # Binomial Distribution #
  #-----------------------#
  
  observeEvent(input$goBinom, {
    show(id = 'probabilityMP')
  })
  
  observeEvent(input$probability, {
    hide(id = 'probabilityMP')
  })
  
  observeEvent(input$resetBinomial, {
    hide(id = 'probabilityMP')
    shinyjs::reset("binomialPanel")
  })
  
  #----------------------#
  # Poisson Distribution #
  #----------------------#
  
  observeEvent(input$goPoisson, {
    show(id = "probabilityMP")
  })
  
  observeEvent(input$resetPoisson, {
    hide(id = "probabilityMP")
    shinyjs::reset("poissonPanel")
  })
  
  #---------------------#
  # Normal Distribution #
  #---------------------#
  
  observeEvent(input$goNormal, {
    show(id = "probabilityMP")
  })
  
  observeEvent(input$resetNormal, {
    hide(id = "probabilityMP")
    shinyjs::reset("normalPanel")
  })
  
  #  -------------------------------------------------------------------- #
  
  
  
  #### Statistical Inference ----
  #  -------------------------------------------------------------------- #
  
  observeEvent(!si_iv$is_valid(), {
    hide(id = "inferenceMP")
    hide(id = "inferenceData")
  })
  
  observeEvent({input$samplesSelect
                input$sampleSize
                input$sampleMean
                input$popuParameter
                input$popuParameters
                input$dataAvailability
                input$dataAvailability2
                input$sigmaKnown
                input$sigmaKnownRaw
                input$popuSD
                input$popuSDRaw
                input$sampSD
                input$inferenceType
                input$inferenceType2}, {
    hide(id = "inferenceData")
  })
  
  observeEvent(input$dataAvailability, {
    hide(id = "oneMeanVariable")
  })
  
  observeEvent(input$dataAvailability2, {
    hide(id = "indMeansUplSample1")
    hide(id = "indMeansUplSample2")
    hide(id = "depMeansUplSample1")
    hide(id = "depMeansUplSample2")
  })
  
  observeEvent(input$goInference, {
    show(id = "inferenceMP")
  })
  
  observeEvent(input$resetInference, {
    hide(id = "inferenceMP")
    shinyjs::reset("inferencePanel")
  })
  
  #  -------------------------------------------------------------------- #
  
  
  
  #### Regression and Correlation ----
  #  -------------------------------------------------------------------- #
  
  observeEvent(!regcor_iv$is_valid(), {
    hide(id = "RegCorMP")
  })
  
  observeEvent(input$dataRegCor, {
    hide(id = "RegCorMP")
    hide(id = "slrResponse")
    hide(id = "slrExplanatory")
  })
  
  
  observeEvent(input$resetRegCor, {
    # hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
    # hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
    # hideTab(inputId = 'tabSet', target = 'Residual Plots')
    hide(id = "RegCorMP")
    shinyjs::reset("RegCorPanel")
  })
  
  #  -------------------------------------------------------------------- #
}