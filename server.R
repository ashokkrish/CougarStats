server <- function(session, input, output) {
  
  # ------------------------- #
  # ---- Data Validation ----
  # ------------------------- #
  
  iv <- InputValidator$new()
  ds_iv <- InputValidator$new()
  dsraw_iv <- InputValidator$new()
  dsupload_iv <- InputValidator$new()
  dsuploadvars_iv <- InputValidator$new()
  
  pd_iv <- InputValidator$new()
  ctable_iv <- InputValidator$new()
  ctableconditional_iv <- InputValidator$new()
  ctable2x2_iv <- InputValidator$new()
  ctable2x2conditional_iv <- InputValidator$new()
  ctable2x3_iv <- InputValidator$new()
  ctable2x3conditional_iv <- InputValidator$new()
  ctable3x2_iv <- InputValidator$new()
  ctable3x2conditional_iv <- InputValidator$new()
  ctable3x3_iv <- InputValidator$new()
  ctable3x3conditional_iv <- InputValidator$new()
  ptable_iv <- InputValidator$new()
  ptableconditional_iv <- InputValidator$new()
  ptable2x2_iv <- InputValidator$new()
  ptable2x2conditional_iv <- InputValidator$new()
  ptable2x3_iv <- InputValidator$new()
  ptable2x3conditional_iv <- InputValidator$new()
  ptable3x2_iv <- InputValidator$new()
  ptable3x2conditional_iv <- InputValidator$new()
  ptable3x3_iv <- InputValidator$new()
  ptable3x3conditional_iv <- InputValidator$new()
  binom_iv <- InputValidator$new()
  binomprob_iv <- InputValidator$new()
  binombetween_iv <- InputValidator$new()
  poiss_iv <- InputValidator$new()
  poissprob_iv <- InputValidator$new()
  poissbetween_iv <- InputValidator$new()
  norm_iv <- InputValidator$new()
  normprob_iv <- InputValidator$new()
  normbetween_iv <- InputValidator$new()
  sampdistrprob_iv <- InputValidator$new()
  sampdistrbetween_iv <- InputValidator$new()
  sampdistrsize_iv <- InputValidator$new()
  percentile_iv <- InputValidator$new()
  
  sse_iv <- InputValidator$new()
  ssemean_iv <- InputValidator$new()
  sseprop_iv <- InputValidator$new()
  
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
  indmeansrawsdunk_iv <- InputValidator$new()
  indmeansuploadsd_iv <- InputValidator$new()
  depmeansraw_iv <- InputValidator$new()
  depmeansupload_iv <- InputValidator$new()
  depmeansuploadvars_iv <- InputValidator$new()
  depmeansrawsd_iv <- InputValidator$new()
  oneprop_iv <- InputValidator$new()
  onepropht_iv <- InputValidator$new()
  twoprop_iv <- InputValidator$new()
  twopropht_iv <- InputValidator$new()
  anovaupload_iv <- InputValidator$new()
  anovamulti_iv <- InputValidator$new()
  anovastacked_iv <- InputValidator$new()
  chiSq2x2_iv <- InputValidator$new()
  chiSq2x3_iv <- InputValidator$new()
  chiSq3x2_iv <- InputValidator$new()
  chiSq3x3_iv <- InputValidator$new()
  
  regcor_iv <- InputValidator$new()
  slrraw_iv <- InputValidator$new()
  slrupload_iv <- InputValidator$new()
  slruploadvars_iv <- InputValidator$new()
  
  ## DS rules ----
  
  # descriptiveStat
  
  dsraw_iv$add_rule("descriptiveStat", sv_required())
  dsraw_iv$add_rule("descriptiveStat", sv_regex("^( )*(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                                "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
  
  dsupload_iv$add_rule("dsUserData", sv_required())
  dsupload_iv$add_rule("dsUserData", ~ if(is.null(fileInputs$dsStatus) || fileInputs$dsStatus == 'reset') "Required")
  dsupload_iv$add_rule("dsUserData", ~ if(!(tolower(tools::file_ext(input$dsUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  dsupload_iv$add_rule("dsUserData", ~ if(ncol(dsUploadData()) < 1) "Data must include one variable")
  dsupload_iv$add_rule("dsUserData", ~ if(nrow(dsUploadData()) < 2) "Samples must include at least 2 observations")
  
  dsuploadvars_iv$add_rule("dsUploadVars", sv_required())
  
  
  # ------------------ #
  #     Conditions     #
  # ------------------ #
  ds_iv$condition(~ isTRUE(input$dropDownMenu == 'Descriptive Statistics'))
  dsraw_iv$condition(~ isTRUE(input$dataInput == 'Enter Raw Data'))
  dsupload_iv$condition(~ isTRUE(input$dataInput == 'Upload Data'))
  dsuploadvars_iv$condition(function() {isTRUE(input$dropDownMenu == 'Descriptive Statistics' &&
                                               input$dataInput == 'Upload Data' && 
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
  
  ctable2x2_iv$add_rule("cMatrix2x2", sv_required())
  ctable2x2_iv$add_rule("cMatrix2x2", ~ if(any(is.na(cMatrixData2x2()))) "Fields must be positive integers.")
  ctable2x2_iv$add_rule("cMatrix2x2", ~ if(any(cMatrixData2x2() < 0)) "Fields must be positive integers.")
  ctable2x2_iv$add_rule("cMatrix2x2", ~ if(any(cMatrixData2x2() %% 1 != 0)) "Fields must be positive integers.")
  ctable2x2_iv$add_rule("cMatrix2x2", ~ if(all(cMatrixData2x2() == 0)) "All cell values cannot be equal to zero.")
  
  ctable2x2conditional_iv$add_rule("cMatrix2x2", ~ if(any(cMatrix2x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ctable2x2conditional_iv$add_rule("cMatrix2x2", ~ if(any(cMatrix2x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
  
  ctable2x3_iv$add_rule("cMatrix2x3", sv_required())
  ctable2x3_iv$add_rule("cMatrix2x3", ~ if(any(is.na(cMatrixData2x3()))) "Fields must be positive integers.")
  ctable2x3_iv$add_rule("cMatrix2x3", ~ if(any(cMatrixData2x3() < 0)) "Fields must be positive integers.")
  ctable2x3_iv$add_rule("cMatrix2x3", ~ if(any(cMatrixData2x3() %% 1 != 0)) "Fields must be positive integers.")
  ctable2x3_iv$add_rule("cMatrix2x3", ~ if(all(cMatrixData2x3() == 0)) "All cell values cannot be equal to zero.")
  
  ctable2x3conditional_iv$add_rule("cMatrix2x3", ~ if(any(cMatrix2x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ctable2x3conditional_iv$add_rule("cMatrix2x3", ~ if(any(cMatrix2x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
  
  ctable3x2_iv$add_rule("cMatrix3x2", sv_required())
  ctable3x2_iv$add_rule("cMatrix3x2", ~ if(any(is.na(cMatrixData3x2()))) "Fields must be positive integers.")
  ctable3x2_iv$add_rule("cMatrix3x2", ~ if(any(cMatrixData3x2() < 0)) "Fields must be positive integers.")
  ctable3x2_iv$add_rule("cMatrix3x2", ~ if(any(cMatrixData3x2() %% 1 != 0)) "Fields must be positive integers.")
  ctable3x2_iv$add_rule("cMatrix3x2", ~ if(all(cMatrixData3x2() == 0)) "All cell values cannot be equal to zero.")
  
  ctable3x2conditional_iv$add_rule("cMatrix3x2", ~ if(any(cMatrix3x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ctable3x2conditional_iv$add_rule("cMatrix3x2", ~ if(any(cMatrix3x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
  
  ctable3x3_iv$add_rule("cMatrix3x3", sv_required())
  ctable3x3_iv$add_rule("cMatrix3x3", ~ if(any(is.na(cMatrixData3x3()))) "Fields must be positive integers.")
  ctable3x3_iv$add_rule("cMatrix3x3", ~ if(any(cMatrixData3x3() < 0)) "Fields must be positive integers.")
  ctable3x3_iv$add_rule("cMatrix3x3", ~ if(any(cMatrixData3x3() %% 1 != 0)) "Fields must be positive integers.")
  ctable3x3_iv$add_rule("cMatrix3x3", ~ if(all(cMatrixData3x3() == 0)) "All cell values cannot be equal to zero.")
  
  ctable3x3conditional_iv$add_rule("cMatrix3x3", ~ if(any(cMatrix3x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ctable3x3conditional_iv$add_rule("cMatrix3x3", ~ if(any(cMatrix3x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
  
  ptable2x2_iv$add_rule("pMatrix2x2", sv_required())
  ptable2x2_iv$add_rule("pMatrix2x2", ~ if(any(is.na(pMatrixData2x2()))) "Probabilities must be between 0 and 1.")
  ptable2x2_iv$add_rule("pMatrix2x2", ~ if(any(pMatrixData2x2() < 0)) "Probabilities must be between 0 and 1.")
  ptable2x2_iv$add_rule("pMatrix2x2", ~ if(any(pMatrixData2x2() >= 1)) "Probabilities must be between 0 and 1.")

  ptable2x2conditional_iv$add_rule("pMatrix2x2", ~ if(any(pMatrix2x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ptable2x2conditional_iv$add_rule("pMatrix2x2", ~ if(any(pMatrix2x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
  
  ptable2x3_iv$add_rule("pMatrix2x3", sv_required())
  ptable2x3_iv$add_rule("pMatrix2x3", ~ if(any(is.na(pMatrixData2x3()))) "Probabilities must be between 0 and 1.")
  ptable2x3_iv$add_rule("pMatrix2x3", ~ if(any(pMatrixData2x3() < 0)) "Probabilities must be between 0 and 1.")
  ptable2x3_iv$add_rule("pMatrix2x3", ~ if(any(pMatrixData2x3() >= 1)) "Probabilities must be between 0 and 1.")

  ptable2x3conditional_iv$add_rule("pMatrix2x3", ~ if(any(pMatrix2x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ptable2x3conditional_iv$add_rule("pMatrix2x3", ~ if(any(pMatrix2x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
  
  ptable3x2_iv$add_rule("pMatrix3x2", sv_required())
  ptable3x2_iv$add_rule("pMatrix3x2", ~ if(any(is.na(pMatrixData3x2()))) "Probabilities must be between 0 and 1.")
  ptable3x2_iv$add_rule("pMatrix3x2", ~ if(any(pMatrixData3x2() < 0)) "Probabilities must be between 0 and 1.")
  ptable3x2_iv$add_rule("pMatrix3x2", ~ if(any(pMatrixData3x2() >= 1)) "Probabilities must be between 0 and 1.")

  ptable3x2conditional_iv$add_rule("pMatrix3x2", ~ if(any(pMatrix3x2Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ptable3x2conditional_iv$add_rule("pMatrix3x2", ~ if(any(pMatrix3x2Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")
  
  ptable3x3_iv$add_rule("pMatrix3x3", sv_required())
  ptable3x3_iv$add_rule("pMatrix3x3", ~ if(any(is.na(pMatrixData3x3()))) "Probabilities must be between 0 and 1.")
  ptable3x3_iv$add_rule("pMatrix3x3", ~ if(any(pMatrixData3x3() < 0)) "Probabilities must be between 0 and 1.")
  ptable3x3_iv$add_rule("pMatrix3x3", ~ if(any(pMatrixData3x3() >= 1)) "Probabilities must be between 0 and 1.")

  ptable3x3conditional_iv$add_rule("pMatrix3x3", ~ if(any(pMatrix3x3Totaled()['Total',] == 0)) "Row and Column totals must be greater than 0.")
  ptable3x3conditional_iv$add_rule("pMatrix3x3", ~ if(any(pMatrix3x3Totaled()[,'Total'] == 0)) "Row and Column totals must be greater than 0.")

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
  
  sampdistrprob_iv$add_rule("sampDistrxValue", sv_required())
  
  sampdistrbetween_iv$add_rule("sampDistrx1Value", sv_required())
  sampdistrbetween_iv$add_rule("sampDistrx2Value", sv_required())
  
  sampdistrsize_iv$add_rule("sampDistrSize", sv_required())
  sampdistrsize_iv$add_rule("sampDistrSize", sv_integer())
  sampdistrsize_iv$add_rule("sampDistrSize", sv_gt(0))
  
  percentile_iv$add_rule("percentileValue", sv_required())
  percentile_iv$add_rule("percentileValue", sv_gt(0))
  percentile_iv$add_rule("percentileValue", sv_lt(100))
  
  # ------------------ #
  #     Conditions     #
  # ------------------ #
  ctable2x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                  input$cTableDimension == '2 x 2'))
  
  ctable2x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                             input$cTableDimension == '2 x 2' &&
                                             # input$cTableType == 'Frequency Distribution' &&
                                             input$cTableProb == 'Conditional'))
  
  ctable2x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                  input$cTableDimension == '2 x 3'))
  
  ctable2x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                             input$cTableDimension == '2 x 3' &&
                                             # input$cTableType == 'Frequency Distribution'&&
                                             input$cTableProb == 'Conditional'))
  
  ctable3x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                  input$cTableDimension == '3 x 2'))
  
  ctable3x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                             input$cTableDimension == '3 x 2' &&
                                             # input$cTableType == 'Frequency Distribution'&&
                                             input$cTableProb == 'Conditional'))
  
  ctable3x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                  input$cTableDimension == '3 x 3'))
  
  ctable3x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                             input$cTableDimension == '3 x 3' &&
                                             # input$cTableType == 'Frequency Distribution'&&
                                             input$cTableProb == 'Conditional'))
  
  ptable2x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                  input$cTableDimension == '2 x 2' &&
                                  input$cTableType == 'Probability Distribution'))

  ptable2x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                             input$cTableDimension == '2 x 2' &&
                                             input$cTableType == 'Probability Distribution'&&
                                             input$cTableProb == 'Conditional'))
  
  ptable2x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                    input$cTableDimension == '2 x 3' &&
                                    input$cTableType == 'Probability Distribution'))
  
  ptable2x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                               input$cTableDimension == '2 x 3' &&
                                               input$cTableType == 'Probability Distribution'&&
                                               input$cTableProb == 'Conditional'))
  
  ptable3x2_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                    input$cTableDimension == '3 x 2' &&
                                    input$cTableType == 'Probability Distribution'))
  
  ptable3x2conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                               input$cTableDimension == '3 x 2' &&
                                               input$cTableType == 'Probability Distribution'&&
                                               input$cTableProb == 'Conditional'))
  
  ptable3x3_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                    input$cTableDimension == '3 x 3' &&
                                    input$cTableType == 'Probability Distribution'))
  
  ptable3x3conditional_iv$condition(~ isTRUE(input$probability == 'Contingency Table' &&
                                               input$cTableDimension == '3 x 3' &&
                                               input$cTableType == 'Probability Distribution'&&
                                               input$cTableProb == 'Conditional'))

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
                                 input$calcQuantiles == 'Probability' &&
                                 input$sampMeanDistr == 0 && 
                                 input$calcNormal != 'between'))
  
  normbetween_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                    input$calcQuantiles == 'Probability' &&
                                    input$sampMeanDistr == 0 &&
                                    input$calcNormal == 'between'))
  
  sampdistrprob_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                      input$calcQuantiles == 'Probability' && 
                                      input$sampMeanDistr == 1 && 
                                      input$calcNormSampDistr != 'between'))
  
  sampdistrbetween_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                         input$calcQuantiles == 'Probability' &&
                                         input$sampMeanDistr == 1 &&
                                         input$calcNormSampDistr == 'between'))
  
  sampdistrsize_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                      input$calcQuantiles == 'Probability' && 
                                      input$sampMeanDistr == 1))
  
  percentile_iv$condition(~ isTRUE(input$probability == 'Normal' &&
                                   input$calcQuantiles == 'Quantile' &&
                                   input$calcQuartiles == 'Percentile'))
  # ------------------ #
  #     Dependency     #
  # ------------------ #
  ctable_iv$add_validator(ctable2x2_iv)
  ctable_iv$add_validator(ctable2x3_iv)
  ctable_iv$add_validator(ctable3x2_iv)
  ctable_iv$add_validator(ctable3x3_iv)
  
  ctableconditional_iv$add_validator(ctable2x2conditional_iv)
  ctableconditional_iv$add_validator(ctable2x3conditional_iv)
  ctableconditional_iv$add_validator(ctable3x2conditional_iv)
  ctableconditional_iv$add_validator(ctable3x3conditional_iv)
  
  ptable_iv$add_validator(ptable2x2_iv)
  ptable_iv$add_validator(ptable2x3_iv)
  ptable_iv$add_validator(ptable3x2_iv)
  ptable_iv$add_validator(ptable3x3_iv)

  ptableconditional_iv$add_validator(ptable2x2conditional_iv)
  ptableconditional_iv$add_validator(ptable2x3conditional_iv)
  ptableconditional_iv$add_validator(ptable3x2conditional_iv)
  ptableconditional_iv$add_validator(ptable3x3conditional_iv)
  
  binom_iv$add_validator(binomprob_iv)
  binom_iv$add_validator(binombetween_iv)
  
  poiss_iv$add_validator(poissprob_iv)
  poiss_iv$add_validator(poissbetween_iv)
  
  norm_iv$add_validator(normprob_iv)
  norm_iv$add_validator(normbetween_iv)
  norm_iv$add_validator(sampdistrprob_iv)
  norm_iv$add_validator(sampdistrbetween_iv)
  norm_iv$add_validator(sampdistrsize_iv)
  norm_iv$add_validator(percentile_iv)
  
  pd_iv$add_validator(ctable_iv)
  pd_iv$add_validator(ptable_iv)
  pd_iv$add_validator(binom_iv)
  pd_iv$add_validator(poiss_iv)
  pd_iv$add_validator(norm_iv)
  
  # ------------------ #
  #     Activation     #
  # ------------------ #
  pd_iv$enable()
  ctable_iv$enable()
  ctableconditional_iv$enable()
  ctable2x2_iv$enable()
  ctable2x2conditional_iv$enable()
  ctable2x3_iv$enable()
  ctable2x3conditional_iv$enable()
  ctable3x2_iv$enable()
  ctable3x2conditional_iv$enable()
  ctable3x3_iv$enable()
  ctable3x3conditional_iv$enable()
  ptable_iv$enable()
  ptableconditional_iv$enable()
  ptable2x2_iv$enable()
  ptable2x2conditional_iv$enable()
  ptable2x3_iv$enable()
  ptable2x3conditional_iv$enable()
  ptable3x2_iv$enable()
  ptable3x2conditional_iv$enable()
  ptable3x3_iv$enable()
  ptable3x3conditional_iv$enable()
  binom_iv$enable()
  binomprob_iv$enable()
  binombetween_iv$enable()
  poiss_iv$enable()
  poissprob_iv$enable()
  poissbetween_iv$enable()
  norm_iv$enable()
  normprob_iv$enable()
  normbetween_iv$enable()
  sampdistrprob_iv$enable()
  sampdistrbetween_iv$enable()
  sampdistrsize_iv$enable()
  percentile_iv$enable()
  
  
  #---------------- #
  ## SSE rules ----
  #---------------- #
  
  # popuSD
  
  ssemean_iv$add_rule("ssePopuSD", sv_required())
  ssemean_iv$add_rule("ssePopuSD", sv_gt(0))
  ssemean_iv$add_rule("sseMeanMargErr", sv_required())
  ssemean_iv$add_rule("sseMeanMargErr", sv_gt(0))
  
  # targetProp
  
  sseprop_iv$add_rule("sseTargetProp", sv_required())
  sseprop_iv$add_rule("sseTargetProp", sv_gt(0))
  sseprop_iv$add_rule("sseTargetProp", sv_lt(1))
  sseprop_iv$add_rule("ssePropMargErr", sv_required())
  sseprop_iv$add_rule("ssePropMargErr", sv_gt(0))
  sseprop_iv$add_rule("ssePropMargErr", sv_lte(1))
  
  # margErr
  
  
  
  
  # ------------------ #
  #     Conditions     #
  # ------------------ #
  
  ssemean_iv$condition( ~ isTRUE(input$dropDownMenu == 'Sample Size Estimation' &&
                                 input$sampSizeEstParameter == 'Population Mean'))
  sseprop_iv$condition( ~ isTRUE(input$dropDownMenu == 'Sample Size Estimation' && 
                                 input$sampSizeEstParameter == 'Population Proportion'))
  
  
  # ------------------ #
  #     Dependency     #
  # ------------------ #
  
  sse_iv$add_validator(ssemean_iv)
  sse_iv$add_validator(sseprop_iv)
  
  
  # ------------------ #
  #     Activation     #
  # ------------------ #
  
  sse_iv$enable()
  ssemean_iv$enable()
  sseprop_iv$enable()
  
  
  
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
  onemeanraw_iv$add_rule("sample1", sv_regex("^( )*(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                             "Data must be numeric values seperated by a comma (ie: 2,3,4)"))
  
  # One Mean Upload Data
  onemeanupload_iv$add_rule("oneMeanUserData", sv_required())
  onemeanupload_iv$add_rule("oneMeanUserData", ~ if(is.null(fileInputs$oneMeanStatus) || fileInputs$oneMeanStatus == 'reset') "Required")
  onemeanupload_iv$add_rule("oneMeanUserData", ~ if(!(tolower(tools::file_ext(input$oneMeanUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
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
  indmeansraw_iv$add_rule("raw_sample1", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)"))
  
  # raw_sample2 
  
  indmeansraw_iv$add_rule("raw_sample2", sv_required())
  indmeansraw_iv$add_rule("raw_sample2", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)."))
  
  indmeansrawsd_iv$add_rule("popuSDRaw1", sv_required()) 
  indmeansrawsd_iv$add_rule("popuSDRaw1", sv_gt(0))
  
  
  indmeansrawsd_iv$add_rule("popuSDRaw2", sv_required()) 
  indmeansrawsd_iv$add_rule("popuSDRaw2", sv_gt(0))
  
  
  indmeansrawsdunk_iv$add_rule("raw_sample1", ~ if(sd(createNumLst(input$raw_sample1)) == 0 && sd(createNumLst(input$raw_sample2)) == 0) "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2.")
  indmeansrawsdunk_iv$add_rule("raw_sample2", ~ if(sd(createNumLst(input$raw_sample1)) == 0 && sd(createNumLst(input$raw_sample2)) == 0) "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2.")
  
  #indMeansUserData
  
  indmeansupload_iv$add_rule("indMeansUserData", sv_required())
  indmeansupload_iv$add_rule("indMeansUserData", ~ if(is.null(fileInputs$indMeansStatus) || fileInputs$indMeansStatus == 'reset') "Required")
  indmeansupload_iv$add_rule("indMeansUserData", ~ if(!(tolower(tools::file_ext(input$indMeansUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
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
  depmeansraw_iv$add_rule("before", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)"))
  
  # after
  
  depmeansraw_iv$add_rule("after", sv_required())
  depmeansraw_iv$add_rule("after", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                                  "Data must be at least 3 numeric values seperated by a comma (ie: 2,3,4)."))
  
  
  depmeansraw_iv$add_rule("before", ~ if(length(createNumLst(input$before)) != length(createNumLst(input$after))) "Before and After must have the same number of observations.")
  depmeansraw_iv$add_rule("after", ~ if(length(createNumLst(input$before)) != length(createNumLst(input$after))) "Before and After must have the same number of observations.")
  
  
  depmeansupload_iv$add_rule("depMeansUserData", sv_required())
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(is.null(fileInputs$depMeansStatus) || fileInputs$depMeansStatus == 'reset') "Required")
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(!(tolower(tools::file_ext(input$depMeansUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(nrow(DepMeansUploadData()) == 0) "File is empty.")
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(ncol(DepMeansUploadData()) < 2) "File must contain at least 2 distinct 'Before' and 'After' sets of data to choose from for analysis.")
  depmeansupload_iv$add_rule("depMeansUserData", ~ if(nrow(DepMeansUploadData()) < 4) "Samples must include at least 3 observations.")

  
  depmeansuploadvars_iv$add_rule("depMeansUplSample1", sv_required())
  depmeansuploadvars_iv$add_rule("depMeansUplSample2", sv_required())
  depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ if(CheckDepUploadSamples() != 0) "Before and After must have the same number of observations.")
  depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ if(CheckDepUploadSamples() != 0) "Before and After must have the same number of observations.")
  
  
  depmeansrawsd_iv$add_rule("after", ~ if(GetDepMeansData()$sd == 0) "Variance required in 'Before' and 'After' sample data for hypothesis testing.")

  # numSuccessesProportion
  
  oneprop_iv$add_rule("numSuccesses", sv_required(message = "Numeric value required."))
  oneprop_iv$add_rule("numSuccesses", sv_integer())
  oneprop_iv$add_rule("numSuccesses", sv_gte(0))
  
  # x1
  twoprop_iv$add_rule("numSuccesses1", sv_required())
  twoprop_iv$add_rule("numSuccesses1", sv_integer())
  twoprop_iv$add_rule("numSuccesses1", sv_gte(0))
  twopropht_iv$add_rule("numSuccesses1", ~ if(checkTwoProp() == 0) "At least one of (x1) and (x2) must be greater than 0.")
  
  # x2
  twoprop_iv$add_rule("numSuccesses2", sv_required())
  twoprop_iv$add_rule("numSuccesses2", sv_integer())
  twoprop_iv$add_rule("numSuccesses2", sv_gte(0))
  twopropht_iv$add_rule("numSuccesses2", ~ if(checkTwoProp() == 0) "At least one of (x1) and (x2) must be greater than 0.")
  
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
  
  # Anova
  
  anovaupload_iv$add_rule("anovaUserData", sv_required())
  anovaupload_iv$add_rule("anovaUserData", ~ if(is.null(fileInputs$anovaStatus) || fileInputs$anovaStatus == 'reset') "Required")
  anovaupload_iv$add_rule("anovaUserData", ~ if(!(tolower(tools::file_ext(input$anovaUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  anovaupload_iv$add_rule("anovaUserData", ~ if(ncol(anovaUploadData()) < 2) "Data must include at least two columns")
  # anovaupload_iv$add_rule("anovaUserData", ~ if(nrow(anovaUploadData()) < 2) "")
  
  # anovamulti_iv$add_rule("anovaMultiColumns", sv_required())
  anovamulti_iv$add_rule("anovaMultiColumns", ~ if(length(input$anovaMultiColumns) < 2) "Select at least two columns")
  
  anovastacked_iv$add_rule("anovaResponse", sv_required())
  anovastacked_iv$add_rule("anovaFactors", sv_required())
  anovastacked_iv$add_rule("anovaResponse", ~ if(anovaStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
  anovastacked_iv$add_rule("anovaFactors", ~ if(anovaStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
  
  # Chi-Square
  
  ChiSqInputRules <- function(iv, inputID) {
    iv$add_rule(inputID, sv_required())
    iv$add_rule(inputID, ~ if(any(is.na(chiSqActiveData()$numeric))) "Fields must be positive integers.")
    iv$add_rule(inputID, ~ if(any(chiSqActiveData()$numeric < 0)) "Fields must be positive integers.")
    iv$add_rule(inputID, ~ if(any(chiSqActiveData()$numeric %% 1 != 0)) "Fields must be positive integers.")
    iv$add_rule(inputID, ~ if(all(chiSqActiveData()$numeric == 0)) "All cell values cannot be equal to zero.")
    iv$add_rule(inputID, ~ if(any(chiSqTotaled()[,"Total"] == 0)) "Row Totals must be greater than zero.")
    iv$add_rule(inputID, ~ if(any(chiSqTotaled()["Total",] == 0)) "Column Totals must be greater than zero.")
  }
  
  # 2 x 2
  ChiSqInputRules(chiSq2x2_iv, "chiSqInput2x2")
  
  # 2 x 3
  ChiSqInputRules(chiSq2x3_iv, "chiSqInput2x3")

  # 3 x 2
  ChiSqInputRules(chiSq3x2_iv, "chiSqInput3x2")

  # 3 x 3
  ChiSqInputRules(chiSq3x3_iv, "chiSqInput3x3")

  
  
  # ------------------ #
  #     Conditions     #
  # ------------------ #
  onemean_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                input$popuParameter == 'Population Mean' && 
                                input$dataAvailability == 'Summarized Data'))
  
  onemeansdknown_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                       input$popuParameter == 'Population Mean' && 
                                       input$dataAvailability == 'Summarized Data' && 
                                       input$sigmaKnown == 'Known'))
  
  onemeansdunk_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                     input$popuParameter == 'Population Mean' && 
                                     input$dataAvailability == 'Summarized Data' && 
                                     input$sigmaKnown == 'Unknown'))
  
  onemeanraw_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                   input$popuParameter == 'Population Mean' && 
                                   input$dataAvailability == 'Enter Raw Data'))
  
  onemeanupload_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                      input$popuParameter == 'Population Mean' && 
                                      input$dataAvailability == 'Upload Data'))
  
  onemeanuploadvar_iv$condition(function() {isTRUE(input$siMethod == '1' && 
                                                   input$popuParameter == 'Population Mean' &&
                                                   input$dataAvailability == 'Upload Data' && 
                                                   onemeanupload_iv$is_valid()) })
  
  onemeanuploadsd_iv$condition(function() {isTRUE(input$siMethod == '1' &&
                                                  input$popuParameter == 'Population Mean' &&
                                                  input$dataAvailability == 'Upload Data' && 
                                                  input$sigmaKnownUpload == 'Known') })
  
  onemeanht_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                  input$popuParameter == 'Population Mean' && 
                                  input$inferenceType == 'Hypothesis Testing'))
  
  indmeanssumm_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                     input$popuParameters == 'Independent Population Means' && 
                                     input$dataAvailability2 == 'Summarized Data'))
  
  indmeansraw_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                    input$popuParameters == 'Independent Population Means' && 
                                    input$dataAvailability2 == 'Enter Raw Data'))
  
  indmeanssdknown_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                        input$popuParameters == 'Independent Population Means' && 
                                        input$dataAvailability2 == 'Summarized Data' && 
                                        input$bothsigmaKnown == 'bothKnown'))
  
  indmeanssdunk_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                      input$popuParameters == 'Independent Population Means' && 
                                      input$dataAvailability2 == 'Summarized Data' && 
                                        input$bothsigmaKnown == 'bothUnknown'))
  
  indmeansrawsd_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                      input$popuParameters == 'Independent Population Means' && 
                                      input$dataAvailability2 == 'Enter Raw Data' && 
                                      input$bothsigmaKnownRaw == 'bothKnown'))
  
  indmeansrawsdunk_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                        input$popuParameters == 'Independent Population Means' && 
                                        input$dataAvailability2 == 'Enter Raw Data' && 
                                        input$bothsigmaKnownRaw == 'bothUnknown' &&
                                        input$inferenceType2 == 'Hypothesis Testing' &&
                                        indmeansraw_iv$is_valid()))
  
  indmeansupload_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                       input$popuParameters == 'Independent Population Means' && 
                                       input$dataAvailability2 == 'Upload Data'))
  
  indmeansuploadvar_iv$condition(function() {isTRUE(input$siMethod == '2' && 
                                                    input$popuParameters == 'Independent Population Means' && 
                                                    input$dataAvailability2 == 'Upload Data' && 
                                                    indmeansupload_iv$is_valid()) })
  
  indmeansuploadsd_iv$condition(function() {isTRUE(input$siMethod == '2' && 
                                                   input$popuParameters == 'Independent Population Means' && 
                                                   input$dataAvailability2 == 'Upload Data' && 
                                                   input$bothsigmaKnownUpload == 'bothKnown') })
  
  depmeansraw_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                    input$popuParameters == 'Dependent Population Means' && 
                                    input$dataTypeDependent == 'Enter Raw Data'))
  
  depmeansupload_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                       input$popuParameters == 'Dependent Population Means' && 
                                       input$dataTypeDependent == 'Upload Data'))
  
  depmeansuploadvars_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                           input$popuParameters == 'Dependent Population Means' && 
                                           input$dataTypeDependent == 'Upload Data' &&
                                           depmeansupload_iv$is_valid()))
  
  depmeansrawsd_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                      input$popuParameters == 'Dependent Population Means' &&
                                      input$dataTypeDependent == 'Enter Raw Data' &&
                                      depmeansraw_iv$is_valid()))
  
  oneprop_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                input$popuParameter == 'Population Proportion'))
  
  onepropht_iv$condition(~ isTRUE(input$siMethod == '1' && 
                                  input$popuParameter == 'Population Proportion' && 
                                  input$inferenceType == 'Hypothesis Testing'))
  
  twoprop_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                input$popuParameters == 'Population Proportions'))
  
  twopropht_iv$condition(~ isTRUE(input$siMethod == '2' && 
                                  input$popuParameters == 'Population Proportions' &&
                                  input$inferenceType2 == 'Hypothesis Testing'))
  
  anovaupload_iv$condition(~ isTRUE(input$siMethod == 'Multiple'))
  
  anovamulti_iv$condition(~ isTRUE(input$siMethod == 'Multiple' && 
                                   input$anovaFormat == 'Multiple' &&
                                   anovaupload_iv$is_valid()))
  
  anovastacked_iv$condition(~ isTRUE(input$siMethod == 'Multiple' && 
                                     input$anovaFormat == 'Stacked' &&
                                     anovaupload_iv$is_valid()))
  
  chiSq2x2_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                 input$chisquareDimension == '2 x 2'))
  
  chiSq2x3_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                 input$chisquareDimension == '2 x 3'))
  
  chiSq3x2_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                 input$chisquareDimension == '3 x 2'))
  
  chiSq3x3_iv$condition(~ isTRUE(input$siMethod == 'Categorical' &&
                                 input$chisquareDimension == '3 x 3'))
  

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
  si_iv$add_validator(indmeansrawsdunk_iv)
  si_iv$add_validator(indmeansupload_iv)
  si_iv$add_validator(indmeansuploadvar_iv)
  si_iv$add_validator(indmeansuploadsd_iv)
  si_iv$add_validator(depmeansraw_iv)
  si_iv$add_validator(depmeansupload_iv)
  si_iv$add_validator(depmeansuploadvars_iv)
  si_iv$add_validator(oneprop_iv)
  si_iv$add_validator(onepropht_iv)
  si_iv$add_validator(twoprop_iv)
  si_iv$add_validator(twopropht_iv)
  twoprop_iv$add_validator(twopropht_iv)
  si_iv$add_validator(anovaupload_iv)
  si_iv$add_validator(anovamulti_iv)
  si_iv$add_validator(anovastacked_iv)
  si_iv$add_validator(chiSq2x2_iv)
  si_iv$add_validator(chiSq2x3_iv)
  si_iv$add_validator(chiSq3x2_iv) 
  si_iv$add_validator(chiSq3x3_iv)
  
  
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
  indmeansrawsdunk_iv$enable()
  indmeansupload_iv$enable()
  indmeansuploadvar_iv$enable()
  indmeansuploadsd_iv$enable()
  depmeansraw_iv$enable
  depmeansupload_iv$enable()
  depmeansuploadvars_iv$enable()
  depmeansrawsd_iv$enable()
  oneprop_iv$enable()
  onepropht_iv$enable()
  twoprop_iv$enable()
  twopropht_iv$enable()
  anovaupload_iv$enable()
  anovamulti_iv$enable()
  anovastacked_iv$enable()
  chiSq2x2_iv$enable()
  chiSq2x3_iv$enable()
  chiSq3x2_iv$enable()
  chiSq3x3_iv$enable()
  
  
  ## RC rules ---- 
  
  slrraw_iv$add_rule("x", sv_required())
  #iv$add_rule("x", sv_regex("^[0-9]+(.[0-9]+)?(, [0-9](.[0-9]+)?)+$", "Data can only be numeric values separated by commas"))
  slrraw_iv$add_rule("x", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                   "Data must be numeric values seperated by a comma (ie: 2,3,4)."))
  slrraw_iv$add_rule("x", ~ if(sampleInfoRaw()$diff != 0) "x and y must have the same number of observations.")
  slrraw_iv$add_rule("x", ~ if(sampleInfoRaw()$xSD == 0) "Not enough variance in Independent Variable.")
  
  slrraw_iv$add_rule("y", sv_required())
  slrraw_iv$add_rule("y", sv_regex("( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$", 
                                   "Data must be numeric values seperated by a comma (ie: 2,3,4)."))
  slrraw_iv$add_rule("y", ~ if(sampleInfoRaw()$diff != 0) "x and y must have the same number of observations.")
  slrraw_iv$add_rule("y", ~ if(sampleInfoRaw()$ySD == 0) "Not enough variance in Dependent Variable.")
  
  slrupload_iv$add_rule("slrUserData", sv_required())
  slrupload_iv$add_rule("slrUserData", ~ if(is.null(fileInputs$slrStatus) || fileInputs$slrStatus == 'reset') "Required")
  slrupload_iv$add_rule("slrUserData", ~ if(!(tolower(tools::file_ext(input$slrUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
  slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) == 0) "File is empty.")
  slrupload_iv$add_rule("slrUserData", ~ if(ncol(slrUploadData()) < 2) "Data must include one response and (at least) one explanatory variable.")
  slrupload_iv$add_rule("slrUserData", ~ if(nrow(slrUploadData()) < 3) "Samples must include at least 2 observations.")
  # slrupload_iv$add_rule("slrUserData", ~ if(any(!is.numeric(slrUploadData()))) "File contains non-numeric data.")
  
  slruploadvars_iv$add_rule("slrExplanatory", sv_required())
  slruploadvars_iv$add_rule("slrExplanatory", ~ if(explanatoryInfoUpload()$invalid) "Explanatory variable contains non-numeric data.")
  slruploadvars_iv$add_rule("slrExplanatory", ~ if(explanatoryInfoUpload()$sd == 0) "Not enough variance in Explanatory Variable.")
  
  slruploadvars_iv$add_rule("slrResponse", sv_required())
  slruploadvars_iv$add_rule("slrResponse", ~ if(sampleDiffUpload() != 0) "Missing values detected, x and y must have the same number of observations.")
  slruploadvars_iv$add_rule("slrResponse", ~ if(responseInfoUpload()$invalid) "Response variable contains non-numeric data.")
  slruploadvars_iv$add_rule("slrResponse", ~ if(responseInfoUpload()$sd == 0) "Not enough variance in Response Variable.")
  
  
  slrraw_iv$condition(~ isTRUE(input$dataRegCor == 'Enter Raw Data'))
  slrupload_iv$condition(~ isTRUE(input$dataRegCor == 'Upload Data'))
  slruploadvars_iv$condition(function() {isTRUE(input$dataRegCor == 'Upload Data' && 
                                                slrupload_iv$is_valid()) })
  
  
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
  fileInputs <- reactiveValues(
    dsStatus = NULL,
    oneMeanStatus = NULL,
    indMeansStatus = NULL,
    depMeansStatus = NULL,
    anovaStatus = NULL,
    slrStatus = NULL
  )
  
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
  
  #  --------------------------------------------------------------------- #
  ## -------------------- Input Validator Functions ----------------------
  #  --------------------------------------------------------------------- #
  
  
  
  
  #  -------------------------------------------------------------------- #
  ## ------------------- Descriptive Stats functions --------------------
  #  -------------------------------------------------------------------- #
  
  
  ### Non-Reactive Functions ----
  # --------------------------------------------------------------------- #
  
  # https://rdrr.io/github/skgrange/threadr/src/R/decimal_count.R
  DecimalCount <- function(x) {

    req(class(x) == "numeric")
 
    # If contains a period
    if (grepl("\\.", x)) {
      x <- stringr::str_replace(x, "0+$", "")
      x <- stringr::str_replace(x, "^.+[.]", "")
      x <- stringr::str_length(x)
        
    } else {
      # Otherwise return zero
      x <- 0
    }

    return(x)
  }
  
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
  
  Range <- function(min, max) {
    if(DecimalCount(min) < DecimalCount(max)) {
      numDigits <- DecimalCount(max)
    } else {
      numDigits <- DecimalCount(min)
    }
    
    range <- round((max - min), digits = numDigits)
    
    return(range)
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
    
    if(sampMode == "No mode exists"){
      modeFreq <- paste("")
    } else{
      modeFreq <- paste("Each appears", attr(Mode(dat), "freq"), "times")
    }
    
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
    
    sampRange <- Range(min(dat), max(dat)) 
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
                                  modeFreq,
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
  
  dsReset <- reactiveVal(FALSE)
  
  # Function to convert the raw data input into a numeric list
  dsRawData <- reactive ({
    dat <- createNumLst(input$descriptiveStat)
    return(dat)
  })
  
  
  # Function to read the uploaded data file
  dsUploadData <- eventReactive(input$dsUserData, {
    ext <- tools::file_ext(input$dsUserData$name)
    ext <- tolower(ext)
    
    switch(ext, 
           csv = read_csv(input$dsUserData$datapath, show_col_types = FALSE),
           xls = read_xls(input$dsUserData$datapath),
           xlsx = read_xlsx(input$dsUserData$datapath),
           txt = read_tsv(input$dsUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format.")
    )
  })
  
  
  getDsDataframe <- reactive({
    
    req(ds_iv$is_valid())
    
    df <- data.frame(Category = c("Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", "Descriptives", 
                                  "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", "Five Number Summary", 
                                  "Outliers", "Outliers", "Outliers", "Outliers", "Outliers", 
                                  "Dispersion", "Dispersion", "Dispersion", "Dispersion", "Dispersion", 
                                  "Distribution", "Distribution"),
                     Variable = c("Number of Observations", 
                                  "Sum", 
                                  "Sum of Squares", 
                                  "Mean", 
                                  "Mode",
                                  "Mode Frequency",
                                  "Minimum", 
                                  "First Quartile (Q<sub>1</sub>)*", 
                                  "Second Quartile or Median (Q<sub>2</sub>)", 
                                  "Third Quartile (Q<sub>3</sub>)*", 
                                  "Maximum", 
                                  "Interquartile Range (IQR)*", 
                                  "Check for Outliers: Lower Fence*", 
                                  "Check for Outliers: Upper Fence*", 
                                  "Number of Potential Outliers*",
                                  "Outlier Value(s)*",
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
        dat <- na.omit(as.data.frame(dsUploadData())[, x])
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
                      "Mode Frequency",
                      "Minimum", 
                      "First Quartile (Q1)", 
                      "Second Quartile or Median (Q2)", 
                      "Third Quartile (Q3)", 
                      "Maximum", 
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
    fileInputs$dsStatus <- 'uploaded'
    
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
        if(is.null(input$dsUserData)) {
          
          validate("Please upload a file.")
        }
        
        validate(
          need(!is.null(fileInputs$dsStatus) && fileInputs$dsStatus == 'uploaded', "Please upload a file."),
          
          errorClass = "myClass"
        )
        
        validate(
          need(nrow(dsUploadData()) != 0 && ncol(dsUploadData()) > 0, "File is empty."),
          need(nrow(dsUploadData()) > 1, "Sample Data must include at least 2 observations."),
          
          errorClass = "myClass"
        )
      } else if(!dsuploadvars_iv$is_valid()) {
        validate(
          need(input$dsUploadVars != "", "Please select a variable."),
          
          errorClass = "myClass"
        )
        
      } else if(!dsraw_iv$is_valid()) {
        validate(
          need(length(dsRawData()) >= 2, "Sample Data must include at least 2 numeric observations."),
          
          errorClass = "myClass"
        )
        
        validate("Sample Data must be numeric.")

      } 
    })
    
    if(ds_iv$is_valid())
    {
      dsReset(FALSE)
      
      output$dsDataTable <- renderUI({

        tagList(
          withMathJax(),
          conditionalPanel(
            condition = "input.dsTableFilters == ''",
            
            br(),
            p("Select one or more items from the Statistics menu to see more information.")
          ),
          
          conditionalPanel(
            condition = "input.dsTableFilters != ''",
            
            withMathJax(DTOutput("dsTableData"))
          ),

          
        )
      })
      
      df <- getDsDataframe()
      
      rowFilter <- input$dsTableFilters
      
      if("Mode" %in% input$dsTableFilters && df['Mode',3] != "No mode exists."){
        rowFilter <- c(rowFilter, "Mode Frequency")
      } 
      
      if("Potential Outliers" %in% input$dsTableFilters){
        rowFilter <- c(rowFilter, "Lower Fence", "Upper Fence", "Outlier Values")
      }
      
      filteredDf <- filter(df, rownames(df) %in% rowFilter)
      
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
                                               escape = FALSE,
                                               rownames = FALSE,
                                               filter = "none",
                                               
      ))
      
      outputOptions(output, "dsTableData", suspendWhenHidden = FALSE)
      
      
      if(input$dataInput == 'Upload Data')
      {
        for( x in input$dsUploadVars)
        {
          sampleData <- na.omit(as.data.frame(dsUploadData())[, x])
        }
      }
      else
      {
        sampleData <- dsRawData()
      }

      
      sample_df <- data.frame(sampleData, sampleData^2)
      names(sample_df) <- c("x", "x<sup>2</sup>")
      dfTotaled <- bind_rows(sample_df, summarise(sample_df, across(where(is.numeric), sum)))
      rownames(dfTotaled)[nrow(dfTotaled)] <- "Totals"
      
      output$sampleDataTable <- renderDT({

        datatable(round(dfTotaled, digits = 3),
                  options = list(dom = 't',
                                 pageLength = -1, 
                                 lengthMenu = list(c(-1, 10, 25, 50, 100), c("All", "10", "25", "50", "100")),
                                 autoWidth = TRUE,
                                 scrollX = TRUE
                  ),
                  rownames = FALSE,
                  escape = FALSE
        ) %>% formatStyle(
          names(dfTotaled),
          target = 'row',
          fontWeight = styleRow(dim(dfTotaled)[1], "bold")
        )

      })
      
      
      output$dsMeanCalc <- renderUI({
        
        tagList(
          withMathJax(),
          sprintf("\\( \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%s} = %s \\)",
                  dfTotaled['Totals', 1],
                  df['Observations', 3],
                  df['Mean', 3]),
          br(),
          br(),
          br()
        )
      })
      
      output$dsSDCalc <- renderUI({
        
        tagList(
          withMathJax(),
          sprintf("\\( s = \\sqrt{ \\dfrac{\\sum x^{2} - \\dfrac{(\\sum x)^{2}}{n} }{n - 1} } \\)"),
          sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\)",
                  dfTotaled['Totals', 2],
                  dfTotaled['Totals', 1],
                  df['Observations', 3],
                  df['Observations', 3],
                  df['Sample Standard Deviation', 3])
        )
      })
      
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
      
      if(df['Outlier Values',3] != "There are no outliers.") {
        df_outliers <- createNumLst(df['Outlier Values',3])
      } else {
        df_outliers <- data.frame()
      }
      
      output$dsBoxplot <- renderPlot({
        
        #---------------- #
        #### Boxplot ---- 
        #---------------- #
        
        bp <- RenderBoxplot(dat,
                            df_boxplot,
                            df_outliers,
                            input$dsBoxplotColour,
                            input$dsBoxplotTitle,
                            input$dsBoxplotXlab,
                            input$dsBoxplotYlab)

        if(input$dsBoxplotFlip == 1){
          bp + coord_flip() +
            theme(axis.text.x.bottom = element_blank(),
                  axis.text.y.left = element_text(size = 16))
        } else {
          bp
        }

      })
      
      #------------------ #
      #### Histogram ----
      #------------------ #
      
      output$dsHistogram <- renderPlot({
        hist <- ggplot(data.frame(x = dat)) +
          geom_histogram(aes(x = x),
                         bins = 15,
                         fill = input$histogramColour,
                         color = "black") +
          labs(title = input$histogramTitle,
               x = input$histogramXlab,
               y = input$histogramYlab) +
          theme_minimal() +
          theme(plot.title = element_text(size = 24,
                                          face = "bold",
                                          hjust = 0.5),
                axis.title.x = element_text(size = 16, 
                                            face = "bold", 
                                            vjust = -1.5),
                axis.title.y = element_text(size = 16, 
                                            face = "bold", 
                                            vjust = 1.5),
                axis.text.x.bottom = element_text(size = 14),
                axis.text.y.left = element_text(size = 14),
                plot.margin = unit(c(1, 1, 1, 1),"cm"))
        
          hist + scale_x_continuous(n.breaks = 10) 
      })

      #---------------------- #
      #### Stem and Leaf ----
      #---------------------- #
      
      output$dsStemLeaf <- renderPrint({
        
        stem.leaf(dat, unit = 1, m = 1, depths = FALSE)
        
      })
      
      show(id = 'descrStatsData')
    } else {
      hide(id = 'descrStatsData')
    }
    # show(id = 'descriptiveStatsMP') 
  })
  
  dsTableProxy <- dataTableProxy('dsTableData')
  
  observeEvent(input$dsTableFilters, {
    req(dsReset() == FALSE)
    
    df <- getDsDataframe()
    
    rowFilter <- input$dsTableFilters
    
    if("Mode" %in% input$dsTableFilters && df['Mode',3] != "No mode exists."){
      rowFilter <- c(rowFilter, "Mode Frequency")
    } 
    
    if("Potential Outliers" %in% input$dsTableFilters){
      rowFilter <- c(rowFilter, "Lower Fence", "Upper Fence", "Outlier Values")
    }
    
    newFilter <- filter(df, rownames(df) %in% rowFilter)
    
    replaceData(dsTableProxy, newFilter, resetPaging = FALSE, rownames = FALSE)
  })
  
  # observeEvent(input$descriptiveStat, {
  #   req(ds_iv$is_valid())
  #   dat <- createNumLst(input$descriptiveStat)
  # 
  #   if(attr(Mode(dat), "freq") == 1){
  #     updatePickerInput(
  #       session = session,
  #       inputId = "dsTableFilters",
  #       choices = list(
  #         Descriptives = c("Observations",
  #                          "Sum",
  #                          "Sum of Squares",
  #                          "Mean",
  #                          "Mode"),
  #         'Five Number Summary' = c("Min",
  #                                   "First Quartile (Q[1])",
  #                                   "Median",
  #                                   "Third Quartile (Q3)",
  #                                   "Max"),
  #         Outliers = c("IQR",
  #                      "Lower Fence",
  #                      "Upper Fence",
  #                      "Potential Outliers",
  #                      "Outlier Values"),
  #         Dispersion = c("Range",
  #                        "Sample Standard Deviation",
  #                        "Sample Variance",
  #                        "Standard Error of the Mean",
  #                        "Coefficient of Variation"),
  #         Distribution = c("Skewness",
  #                          "Kurtosis")
  #       ),
  #       selected = c("Observations",
  #                    "Mean",
  #                    "Min",
  #                    "First Quartile (Q1)",
  #                    "Median",
  #                    "Third Quartile (Q3)",
  #                    "Max",
  #                    "Sample Standard Deviation")
  #     )
  # 
  #   } else{
  #     updatePickerInput(
  #       session = session,
  #       inputId = "dsTableFilters",
  #       choices = list(
  #         Descriptives = c("Observations",
  #                          "Sum",
  #                          "Sum of Squares",
  #                          "Mean",
  #                          "Mode",
  #                          "Mode Frequency"),
  #         'Five Number Summary' = c("Min",
  #                                   "First Quartile (Q[1])",
  #                                   "Median",
  #                                   "Third Quartile (Q3)",
  #                                   "Max"),
  #         Outliers = c("IQR",
  #                      "Lower Fence",
  #                      "Upper Fence",
  #                      "Potential Outliers",
  #                      "Outlier Values"),
  #         Dispersion = c("Range",
  #                        "Sample Standard Deviation",
  #                        "Sample Variance",
  #                        "Standard Error of the Mean",
  #                        "Coefficient of Variation"),
  #         Distribution = c("Skewness",
  #                          "Kurtosis")
  #       ),
  #       selected = c("Observations",
  #                    "Mean",
  #                    "Min",
  #                    "First Quartile (Q1)",
  #                    "Median",
  #                    "Third Quartile (Q3)",
  #                    "Max",
  #                    "Sample Standard Deviation")
  #     )
  #   }
  # })
  
  # --------------------------------------------------------------------- #
  
  
  # **************************************************************************** #
  
  
  #  -------------------------------------------------------------------- #
  ## --------------- Probability Distribution functions -----------------
  #  -------------------------------------------------------------------- #
  
  
  ### Non-Reactive Functions ----
  # --------------------------------------------------------------------- #
  
  # shadeNormArea <- function(df, normValue, normLines, probType){
  #   if(normValue > 0){
  #     if(probType == 'cumulative') {
  #       print(filter(df, x <= normLines))
  #       geom_area(data = filter(df, x <= normLines),
  #                 aes(x = x, y=y), 
  #                 fill = "#03376d", 
  #                 alpha = 0.8)
  #       
  #     } else if (probType == 'upperTail') {
  #       geom_area(data = filter(df, x >= normLines),
  #                 aes(x = x, y=y), 
  #                 fill = "#03376d", 
  #                 alpha = 0.8)
  #       
  #     } else {
  #       geom_area(data = subset(df, x >= normLines[1] & x <= normLines[2]),
  #                 aes(x = x, y=y),
  #                 adjust = 3,
  #                 fill = "#03376d",
  #                 alpha = 0.8)
  #     }
  #   }
  # }
  ResetCTable <- function(tableID, numRows, numCols, rowNames, colNames){
    newMatrix <- matrix("", numRows, numCols)
    colnames(newMatrix) <- colNames
    rownames(newMatrix) <- rowNames
    updateMatrixInput(session, tableID, newMatrix)
  }
  
  
  getProbabilities <- function(x, t){
    return(round((x/t), 4))
  }
  
  
  getTotaledMatrix <- function(cMatrix, matrixData){
    colnames(cMatrix) <- colnames(matrixData)
    rownames(cMatrix) <- rownames(matrixData)
    cMatrix <- cbind(cMatrix, Total = round(rowSums(cMatrix), 4))
    cMatrix <- rbind(cMatrix, Total = round(colSums(cMatrix), 4))
    
    return(cMatrix)
  }
  
  
  printMarginalProbs <- function(probMatrix, cMatrix){
    outputTagList <- tagList(
                             withMathJax(),
                             titlePanel('Marginal Probabilities'),
                             hr(),
                             br()
                             )
    
    for(row in 1:(nrow(probMatrix)-1)){
      newLine <- sprintf("\\( P( \\text{%s} ) = \\dfrac{%s}{%s} = %s \\)",
                         rownames(probMatrix)[row],
                         cMatrix[row,'Total'],
                         cMatrix['Total','Total'],
                         probMatrix[row,'Total'])
      outputTagList <- tagAppendChild(outputTagList, newLine)
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      
    }
    
    for(col in 1:(ncol(probMatrix)-1)){
      newLine <- sprintf("\\( P( \\text{%s} ) = \\dfrac{%s}{%s} = %s \\)",
                         colnames(probMatrix)[col],
                         cMatrix['Total', col],
                         cMatrix['Total','Total'],
                         probMatrix['Total',col])
      outputTagList <- tagAppendChild(outputTagList, newLine)
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      
    }
    
    return(outputTagList)
  }
  
  
  printJointProbs <- function(probMatrix, cMatrix){
    outputTagList <- tagList(
                             withMathJax(),
                             titlePanel('Joint Probabilities'),
                             hr(),
                             br()
                             )
    
    for(row in 1:(nrow(probMatrix) - 1)){
      for(col in 1:(ncol(probMatrix) - 1)){
        newLine <- sprintf("\\( P( \\text{%s} \\cap \\text{%s} ) \\; = 
                           \\dfrac{%s}{%s} = %s \\)",
                           rownames(probMatrix)[row],
                           colnames(probMatrix)[col],
                           cMatrix[row,col],
                           cMatrix['Total','Total'],
                           probMatrix[row,col])
        outputTagList <- tagAppendChild(outputTagList, newLine)
        outputTagList <- tagAppendChild(outputTagList, br())
        outputTagList <- tagAppendChild(outputTagList, br())
      }
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      
    }
    
    return(outputTagList)
  }
  
  
  printUnionProbs <- function(probMatrix, cMatrix){
    outputTagList <- tagList(
      withMathJax(),
      titlePanel('Union Probabilities'),
      hr(),
      br()
    )
    
    for(row in 1:(nrow(probMatrix) - 1)){
      for(col in 1:(ncol(probMatrix) - 1)){
        newLine <- sprintf("\\( P( \\text{%s} \\cup \\text{%s} ) \\; =
                           \\; P( \\text{%s} ) + P( \\text{%s} ) - P(\\text{%s} \\cap \\text{%s}) \\; =
                           \\; \\dfrac{%s}{%s} + \\dfrac{%s}{%s} - \\dfrac{%s}{%s} \\; =
                           \\; \\dfrac{%s}{%s} = %s \\quad \\)",
                           rownames(probMatrix)[row],
                           colnames(probMatrix)[col],
                           rownames(probMatrix)[row],
                           colnames(probMatrix)[col],
                           rownames(probMatrix)[row],
                           colnames(probMatrix)[col],
                           cMatrix[row,'Total'],
                           cMatrix['Total','Total'],
                           cMatrix['Total',col],
                           cMatrix['Total','Total'],
                           cMatrix[row,col],
                           cMatrix['Total','Total'],
                           cMatrix[row,'Total'] + cMatrix['Total',col] - cMatrix[row,col],
                           cMatrix['Total','Total'],
                           probMatrix[row,'Total'] + probMatrix['Total',col] - probMatrix[row,col])
        outputTagList <- tagAppendChild(outputTagList, newLine)
        outputTagList <- tagAppendChild(outputTagList, br())
        outputTagList <- tagAppendChild(outputTagList, br())
      }
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      
    }
    
    return(outputTagList)
  }
  
  printConditionalProbs <- function(cMatrix){
    outputTagList <- tagList(
                             withMathJax(),
                             titlePanel('Conditional Probabilities'),
                             hr(),
                             br(),
                             sprintf("\\( P(\\text{A} \\, | \\, \\text{B}) \\; = 
                                     \\; \\dfrac{P(\\text{A} \\cap \\text{B})}{P(\\text{B})}, 
                                     \\quad P(\\text{B}) \\gt 0 \\)"),
                             br(),
                             br(),
                             br(),
                             br()
                             )
    
    for(row in 1:(nrow(cMatrix) - 1)){
      for(col in 1:(ncol(cMatrix) - 1)){
        newLine <- sprintf("\\( P( \\text{%s} \\, | \\, \\text{%s} ) \\; = 
                           \\; \\dfrac{P( \\text{%s} \\cap \\text{%s} )}{P( \\text{%s} )} \\; = 
                           \\; \\dfrac{%s}{%s} \\bigg/ \\dfrac{%s}{%s} \\; =
                           \\; \\dfrac{%s}{%s} = %s \\)",
                           rownames(cMatrix)[row],
                           colnames(cMatrix)[col],
                           rownames(cMatrix)[row],
                           colnames(cMatrix)[col],
                           colnames(cMatrix)[col],
                           cMatrix[row,col],
                           cMatrix['Total','Total'],
                           cMatrix['Total',col],
                           cMatrix['Total','Total'],
                           cMatrix[row,col],
                           cMatrix['Total',col],
                           round( (cMatrix[row,col] / cMatrix['Total',col]), 4))
        outputTagList <- tagAppendChild(outputTagList, newLine)
        outputTagList <- tagAppendChild(outputTagList, br())
        outputTagList <- tagAppendChild(outputTagList, br())
      }
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      
    }
    
    for(col in 1:(ncol(cMatrix) - 1)){
      for(row in 1:(nrow(cMatrix) - 1)){
        newLine <- sprintf("\\( P( \\text{%s} \\, | \\, \\text{%s} ) \\; = 
                           \\; \\dfrac{P( \\text{%s} \\cap \\text{%s} )}{P( \\text{%s} )} \\; = 
                           \\; \\dfrac{%s}{%s} \\bigg/ \\dfrac{%s}{%s} \\; =
                           \\; \\dfrac{%s}{%s} = %s \\)",
                           colnames(cMatrix)[col],
                           rownames(cMatrix)[row],
                           colnames(cMatrix)[col],
                           rownames(cMatrix)[row],
                           rownames(cMatrix)[row],
                           cMatrix[row,col],
                           cMatrix['Total','Total'],
                           cMatrix[row,'Total'],
                           cMatrix['Total','Total'],
                           cMatrix[row,col],
                           cMatrix[row,'Total'],
                           round( (cMatrix[row,col] / cMatrix[row,'Total']), 4))
        outputTagList <- tagAppendChild(outputTagList, newLine)
        outputTagList <- tagAppendChild(outputTagList, br())
        outputTagList <- tagAppendChild(outputTagList, br())
      }
      outputTagList <- tagAppendChild(outputTagList, br())
      outputTagList <- tagAppendChild(outputTagList, br())
      
    }
    
    return(outputTagList)
  }
  
  
  
  shadeNormArea <- function(df, normValue, normLines, probType){
    if(normValue > 0){
      if(probType == 'cumulative') {
        geom_area(data = subset(df, x <= normLines),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4)
        
      } else if (probType == 'upperTail') {
        geom_area(data = subset(df, x >= normLines),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4)
        
      } else {
        geom_area(data = subset(df, x >= normLines[1] & x <= normLines[2]),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4)
      }
    }
  }
  
  labelNormZArea <- function(probVal, probType, normLines){
    req(pd_iv$is_valid())
    if(probType == 'cumulative') {
      centerPoint <- normLines - 0.5
      
    } else if (probType == 'upperTail') {
      centerPoint <- normLines + 0.5
      
    } else {
      centerPoint <- normLines[1] + (normLines[2] - normLines[1])/2
    }
    
    geom_label(data = NULL,
               aes(x = centerPoint, y = 0.15, label = probVal),
               fill = "#265381",
               color = "white",
               size = 24 / .pt,
               fontface = "bold",
               label.size = NA)
  }
  
  PlotTitle <- function(variance){
    if(input$sampMeanDistr == 0){
      ggtitle(bquote(bolditalic(X %~% N(.(input$popMean),.(variance)))))
    } else {
      ggtitle(bquote(bolditalic(bar(X) %~% N(.(input$popMean),.(variance)))))
    }
  }
  
  PlotXLab <- function(){
    if(input$sampMeanDistr == 0){
      xlab(expression(bolditalic( X )))
    } else {
      xlab(expression(bolditalic( bar(X) )))
    }
  }
  
  
  # normPlot <- function(normValue, normLines, standDev, probType){
  #   req(pd_iv$is_valid())
  #   
  #   x <- seq(from = input$popMean - 3*standDev, to = input$popMean + 3*standDev, by = standDev/10)
  #   xSeq <- c(x, normLines, input$popMean)
  #   
  #   df <- data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = standDev))
  #   lineDF <- filter(df, x %in% normLines)
  #   meanDF <- filter(df, x %in% input$popMean)
  #   sdLinesDF <- filter(df, x %in% c(input$popMean - standDev, input$popMean + standDev))
  #   
  #   nPlot <- ggplot(df, aes(x = x, y = y)) +
  #     geom_line() +
  #     shadeNormArea(df, normValue, normLines, probType) +
  #     geom_segment(data = lineDF,
  #                  aes(x = x, xend = x, y = 0, yend = y),
  #                  linetype = "solid",
  #                  lineend = 'round',
  #                  linewidth = 1.25,
  #                  color='#021C38') +
  #     geom_text(data = lineDF, 
  #               aes(x = x, y = 0, label = x), 
  #               size = 16 / .pt, 
  #               fontface = "bold",
  #               check_overlap = TRUE,
  #               vjust = 1.5) +
  #     geom_segment(data = meanDF,
  #                  aes(x = x, xend = x, y = 0, yend = y),
  #                  linetype = "dotted",
  #                  lineend = 'round',
  #                  linewidth = 1,
  #                  color='#021C38',
  #                  alpha = 0.5) +
  #     geom_text(data = meanDF, 
  #               aes(x = x, y = 0, label = x), 
  #               size = 16 / .pt,
  #               check_overlap = TRUE,
  #               vjust = 1.5) +
  #     geom_segment(data = sdLinesDF,
  #                  aes(x = x, xend = x, y = 0, yend = y),
  #                  linetype = "dotted",
  #                  lineend = 'round',
  #                  linewidth = 1,
  #                  color='#021C38',
  #                  alpha = 0.5) +
  #     geom_text(data = sdLinesDF, 
  #               aes(x = x, y = 0, label = x), 
  #               size = 16 / .pt,
  #               check_overlap = TRUE,
  #               vjust = 1.5) +
  #     coord_cartesian(clip="off") +
  #     ggtitle(paste0("X ~ N(", input$popMean, ", ", (standDev^2), ")")) +
  #     theme_minimal()  +
  #     theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
  #           axis.title.x = element_text(size = 18, 
  #                                       face = "bold",
  #                                       vjust = -1),
  #           axis.text.x.bottom = element_text(size = 14)) +
  #     scale_x_continuous(breaks = NULL) +
  #     ylab("Density") +
  #     xlab("X") 
  #   
  #   
  #   return(nPlot)
  # }
  
  
  normPlot <- function(normValue, normLines, popmean, variance, standDev, lineLabels, probType, plotLab){
    req(pd_iv$is_valid())
    withMathJax()
    
    x <- round(seq(from = -3, to = 3, by = 0.1), 2)
    xSeq <- unique(sort(c(x, normLines)))
    
    df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
    lineDF <- filter(df, x %in% normLines)
    lineDF['z'] <- lineLabels
    meanDF <- filter(df, x %in% c(0))
    
    nPlot <- ggplot(df, aes(x = x, y = y)) +
      geom_line(linetype = "solid",
                linewidth = 0.75,
                color='#021C38') +
      geom_area(data = df,
                aes(y=y), 
                fill = NA, 
                color = NA) +
      shadeNormArea(df, normValue, normLines, probType) +
      geom_segment(data = lineDF,
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "solid",
                   lineend = 'round',
                   linewidth = 1.25,
                   color='#021C38') +
      geom_text(data = lineDF, 
                aes(x = x, y = 0, label = z), 
                size = 16 / .pt,
                fontface = "bold",
                check_overlap = TRUE,
                vjust = 1.5) +
      geom_segment(data = meanDF,
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "dotted",
                   lineend = 'round',
                   linewidth = 1,
                   color='#021C38',
                   alpha = 0.5) +
      geom_text(data = meanDF, 
                aes(x = x, y = 0, label = popmean), 
                size = 16 / .pt,
                fontface = "bold",
                check_overlap = TRUE,
                vjust = 1.5) +
      geom_segment(data = df,
                   aes(x = -3, xend = 3, y = 0, yend = 0),
                   linetype = "solid",
                   linewidth = 0.5,
                   color='#021C38') +
      coord_cartesian(clip="off") +
      PlotTitle(standDev) +
      theme_minimal()  +
      theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
            axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
            axis.text.x.bottom = element_text(size = 14)) +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      ylab("") + 
      PlotXLab() 
    
    
    return(nPlot)
  }

  normZPlot <- function(normValue, normLines, probType){
    req(pd_iv$is_valid())
    
    x <- round(seq(from = -3, to = 3, by = 0.1), 2)
    xSeq <- unique(sort(c(x, normLines)))
    
    df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
    
    
    lineDF <- filter(df, x %in% normLines)
    meanDF <- filter(df, x %in% c(0))
    
    nPlot <- ggplot(df, aes(x = x, y = y)) +
      geom_line(linetype = "solid",
                linewidth = 0.75,
                color='#021C38') +
      geom_area(data = df,
                aes(y=y), 
                fill = NA, 
                color = NA) +
      shadeNormArea(df, normValue, normLines, probType) +
      geom_segment(data = lineDF,
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "solid",
                   lineend = 'round',
                   linewidth = 1.25,
                   color='#021C38') +
      geom_text(data = lineDF, 
                aes(x = x, y = 0, label = x), 
                size = 16 / .pt,
                fontface = "bold",
                check_overlap = TRUE,
                vjust = 1.5) +
      geom_segment(data = meanDF,
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "dotted",
                   lineend = 'round',
                   linewidth = 1,
                   color='#021C38',
                   alpha = 0.5) +
      geom_text(data = meanDF, 
                aes(x = x, y = 0, label = x), 
                size = 16 / .pt,
                fontface = "bold",
                check_overlap = TRUE,
                vjust = 1.5) +
      geom_segment(data = df,
                   aes(x = -3, xend = 3, y = 0, yend = 0),
                   linetype = "solid",
                   linewidth = 0.5,
                   color='#021C38') +
      # labelNormZArea(normValue, probType, normLines) +
      coord_cartesian(clip="off") +
      ggtitle(bquote(bolditalic( Z %~% N(0,1) ))) +
      theme_minimal()  +
      theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
            axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
            axis.text.x.bottom = element_text(size = 14)) +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      ylab("") +
      xlab("Z") 
    
    
    return(nPlot)
  }
  
  # --------------------------------------------------------------------- #
  
  
  ### Reactives ----
  # --------------------------------------------------------------------- #

  
  # --------------------- #
  # cMatrixData Reactives #
  # --------------------- #
  # Purpose:
  #   converts matrix data user input into numeric values
  
  cMatrixData2x2 <- reactive({
    suppressWarnings(as.numeric(input$cMatrix2x2))
  })
  
  cMatrixData2x3 <- reactive({
    suppressWarnings(as.numeric(input$cMatrix2x3))
  })
  
  cMatrixData3x2 <- reactive({
    suppressWarnings(as.numeric(input$cMatrix3x2))
  })
  
  cMatrixData3x3 <- reactive({
    suppressWarnings(as.numeric(input$cMatrix3x3))
  })
  
  
  # --------------------- #
  # pMatrixData Reactives #
  # --------------------- #
  # Purpose:
  #   converts matrix data input by the user into numeric values
  
  # pMatrixData2x2 <- reactive({
  #   suppressWarnings(as.numeric(input$pMatrix2x2))
  # })
  # 
  # pMatrixData2x3 <- reactive({
  #   suppressWarnings(as.numeric(input$pMatrix2x3))
  # })
  # 
  # pMatrixData3x2 <- reactive({
  #   suppressWarnings(as.numeric(input$pMatrix3x2))
  # })
  # 
  # pMatrixData3x3 <- reactive({
  #   suppressWarnings(as.numeric(input$pMatrix3x3))
  # })
  

  # ------------------------ #
  # cMatrixTotaled Reactives #
  # ------------------------ #
  # Purpose:
  #   uses the numeric user data to create a matrix with a 'Total' row and
  #   column using the GetCMatrix function.
  
  cMatrix2x2Totaled <- reactive({
    if(!any(is.na(cMatrixData2x2()))){
      cData2x2 <- matrix(cMatrixData2x2(), ncol = ncol(input$cMatrix2x2))
      cData2x2 <- getTotaledMatrix(cData2x2, input$cMatrix2x2)
      
      return(cData2x2)
    }
  })
  
  cMatrix2x3Totaled <- reactive({
    if(!any(is.na(cMatrixData2x3()))){
      cData2x3 <- matrix(cMatrixData2x3(), ncol = ncol(input$cMatrix2x3))
      cData2x3 <- getTotaledMatrix(cData2x3, input$cMatrix2x3)
      
      return(cData2x3)
    }
  })
  
  cMatrix3x2Totaled <- reactive({
    if(!any(is.na(cMatrixData3x2()))){
      cData3x2 <- matrix(cMatrixData3x2(), ncol = ncol(input$cMatrix3x2))
      cData3x2 <- getTotaledMatrix(cData3x2, input$cMatrix3x2)
      
      return(cData3x2)
    }
  })
  
  cMatrix3x3Totaled <- reactive({
    if(!any(is.na(cMatrixData3x3()))){
      cData3x3 <- matrix(cMatrixData3x3(), ncol = ncol(input$cMatrix3x3))
      cData3x3 <- getTotaledMatrix(cData3x3, input$cMatrix3x3)
      
      return(cData3x3)
    }
  })

  
# ------------------------ #
  # pMatrixTotaled Reactives #
  # ------------------------ #
  # Purpose:
  #   uses the numeric user data to create a matrix with a 'Total' row and
  #   column using the GetCMatrix function.
  
  # pMatrix2x2Totaled <- reactive({
  #   if(!any(is.na(pMatrixData2x2()))){
  #     pData2x2 <- matrix(pMatrixData2x2(), ncol = ncol(input$pMatrix2x2))
  #     pData2x2 <- getTotaledMatrix(pData2x2, input$pMatrix2x2)
  # 
  #     return(pData2x2)
  #   }
  # })
  # 
  # pMatrix2x3Totaled <- reactive({
  #   if(!any(is.na(cMatrixData2x3()))){
  #     pData2x3 <- matrix(pMatrixData2x3(), ncol = ncol(input$pMatrix2x3))
  #     pData2x3 <- getTotaledMatrix(pData2x3, input$pMatrix2x3)
  # 
  #     return(pData2x3)
  #   }
  # })
  # 
  # pMatrix3x2Totaled <- reactive({
  #   if(!any(is.na(pMatrixData3x2()))){
  #     pData3x2 <- matrix(pMatrixData3x2(), ncol = ncol(input$pMatrix3x2))
  #     pData3x2 <- getTotaledMatrix(pData3x2, input$pMatrix3x2)
  # 
  #     return(pData3x2)
  #   }
  # })
  # 
  # pMatrix3x3Totaled <- reactive({
  #   if(!any(is.na(pMatrixData3x3()))){
  #     pData3x3 <- matrix(pMatrixData3x3(), ncol = ncol(input$pMatrix3x3))
  #     pData3x3 <- getTotaledMatrix(pData3x3, input$pMatrix3x3)
  # 
  #     return(pData3x3)
  #   }
  # })


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
  
  getMeanNormValue <- reactive({
    req(pd_iv$is_valid())
    
    sampSE <- input$popSD / sqrt(input$sampDistrSize)
    
    if(input$calcNormSampDistr == "cumulative")
    {
      normValue <- round(pnorm(input$sampDistrxValue, input$popMean, sampSE, lower.tail = TRUE),4)
      #paste("\\(P(X \\leq \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = TRUE),4))
    }
    else if(input$calcNormSampDistr == "upperTail")
    {
      normValue <- round(pnorm(input$sampDistrxValue, input$popMean, sampSE, lower.tail = FALSE),4)
      #paste("\\(P(X > \\)", " ", norm_x, "\\()\\)", " ", "\\( = \\)", " ", round(pnorm(norm_x, norm_mu, norm_sigma, lower.tail = FALSE),4))
    }
    else if(input$calcNormSampDistr == 'between')
    {
      req(input$sampDistrx1Value <= input$sampDistrx2Value)
      normValue <- round(pnorm(input$sampDistrx2Value, input$popMean, sampSE, lower.tail = TRUE) - pnorm(input$sampDistrx1Value, input$popMean, sampSE, lower.tail = TRUE), 4)
    }
  })
  
  # --------------------------------------------------------------------- #
  
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  #### Contingency Table ----
  
  observeEvent(input$gocTable, {
 
    output$render2x2cTable <- renderUI({
      
      validate(
        need(input$cMatrix2x2, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(cMatrixData2x2())), "Fields must be positive integers.") %then%
          need(all(cMatrixData2x2() %% 1 == 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(cMatrixData2x2() >= 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(any(cMatrixData2x2() != 0), "All cell values cannot be equal to zero."),
        
        errorClass = "myClass"
      )
      
      tagList(

        titlePanel("Frequency Distribution Table"),
        hr(),
        br(),
        DTOutput("cTable2x2", width = '500px'),
        br(),
        br(),
        br(),
        titlePanel("Probability Distribution Table"),
        hr(),
        br(),
        DTOutput("probTable2x2", width = '500px'),
        br(),
        br(),
        br(),
      )
    })
    
    # output$render2x2pTable <- renderUI({
    #   
    #   validate(
    #     need(input$pMatrix2x2, "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(!is.na(pMatrixData2x2())), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(pMatrixData2x2() >= 0) && all(pMatrixData2x2() < 1), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(any(pMatrixData2x2() != 0), "All cell values cannot be equal to zero."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(pMatrix2x2Totaled()['Total', 'Total'] == 1, "The sum of all probabilities must equal 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   tagList(
    #     
    #     titlePanel("Probability Distribution Table"),
    #     hr(),
    #     br(),
    #     DTOutput("pTable2x2", width = '500px'),
    #     br(),
    #     br(),
    #     br(),
    #   )
    # })
    
    output$render2x3cTable <- renderUI({
      
      validate(
        need(input$cMatrix2x3, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(cMatrixData2x3())), "Fields must be positive integers.") %then%
          need(all(cMatrixData2x3() %% 1 == 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(cMatrixData2x3() >= 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(any(cMatrixData2x3() != 0), "All cell values cannot be equal to zero."),
        
        errorClass = "myClass"
      )
      
      tagList(
        
        titlePanel("Frequency Distribution Table"),
        hr(),
        br(),
        DTOutput("cTable2x3", width = '625px'),
        br(),
        br(),
        br(),
        titlePanel("Probability Distribution Table"),
        hr(),
        br(),
        DTOutput("probTable2x3", width = '625px'),
        br(),
        br(),
        br(),
      )
    })
    
    
    # output$render2x3pTable <- renderUI({
    #   
    #   validate(
    #     need(input$pMatrix2x3, "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(!is.na(pMatrixData2x3())), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(pMatrixData2x3() >= 0) && all(pMatrixData2x3() < 1), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(any(pMatrixData2x3() != 0), "All cell values cannot be equal to zero."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(pMatrix2x3Totaled()['Total', 'Total'] == 1, "The sum of all probabilities must equal 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   tagList(
    #     
    #     titlePanel("Probability Distribution Table"),
    #     hr(),
    #     br(),
    #     DTOutput("pTable2x3", width = '625px'),
    #     br(),
    #     br(),
    #     br(),
    #   )
    # })
    
    
    output$render3x2cTable <- renderUI({
      
      validate(
        need(input$cMatrix3x2, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(cMatrixData3x2())), "Fields must be positive integers.") %then%
          need(all(cMatrixData3x2() %% 1 == 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(cMatrixData3x2() >= 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(any(cMatrixData3x2() != 0), "All cell values cannot be equal to zero."),
        
        errorClass = "myClass"
      )
      
      tagList(
        
        titlePanel("Frequency Distribution Table"),
        hr(),
        br(),
        DTOutput("cTable3x2", width = '500px'),
        br(),
        br(),
        br(),
        titlePanel("Probability Distribution Table"),
        hr(),
        br(),
        DTOutput("probTable3x2", width = '500px'),
        br(),
        br(),
        br(),
      )
    })
    
    
    # output$render3x2pTable <- renderUI({
    #   
    #   validate(
    #     need(input$pMatrix3x2, "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(!is.na(pMatrixData3x2())), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(pMatrixData3x2() >= 0) && all(pMatrixData3x2() < 1), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(any(pMatrixData3x2() != 0), "All cell values cannot be equal to zero."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(pMatrix3x2Totaled()['Total', 'Total'] == 1, "The sum of all probabilities must equal 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   tagList(
    #     
    #     titlePanel("Probability Distribution Table"),
    #     hr(),
    #     br(),
    #     DTOutput("pTable3x2", width = '500px'),
    #     br(),
    #     br(),
    #     br(),
    #   )
    # })
    
    
    output$render3x3cTable <- renderUI({
      
      validate(
        need(input$cMatrix3x3, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(cMatrixData3x3())), "Fields must be positive integers.") %then%
          need(all(cMatrixData3x3() %% 1 == 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(cMatrixData3x3() >= 0), "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(any(cMatrixData3x3() != 0), "All cell values cannot be equal to zero."),
        
        errorClass = "myClass"
      )
      
      tagList(
        
        titlePanel("Frequency Distribution Table"),
        hr(),
        br(),
        DTOutput("cTable3x3", width = '625px'),
        br(),
        br(),
        br(),
        titlePanel("Probability Distribution Table"),
        hr(),
        br(),
        DTOutput("probTable3x3", width = '625px'),
        br(),
        br(),
        br(),
      )
    })
    
    
    # output$render3x3pTable <- renderUI({
    #   
    #   validate(
    #     need(input$pMatrix3x3, "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(!is.na(pMatrixData3x3())), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(all(pMatrixData3x3() >= 0) && all(pMatrixData3x3() < 1), "Probabilities must be between 0 and 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(any(pMatrixData3x3() != 0), "All cell values cannot be equal to zero."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   validate(
    #     need(pMatrix3x3Totaled()['Total', 'Total'] == 1, "The sum of all probabilities must equal 1."),
    #     
    #     errorClass = "myClass"
    #   )
    #   
    #   tagList(
    #     
    #     titlePanel("Probability Distribution Table"),
    #     hr(),
    #     br(),
    #     DTOutput("pTable3x3", width = '625px'),
    #     br(),
    #     br(),
    #     br(),
    #   )
    # })
    
    
    cData2x2 <- matrix(cMatrixData2x2(), ncol = ncol(input$cMatrix2x2))
    cData2x2 <- getTotaledMatrix(cData2x2, input$cMatrix2x2)
    
    output$cTable2x2 <- renderDT({

      # excelR::excelTable(cData2x2,
      #            editable = FALSE,
      #            autoFill = TRUE) 
      
      datatable(cData2x2,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>%
        formatStyle(columns = c(0,3),
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:3,
                    target = 'row',
                    fontWeight = styleRow(dim(cData2x2)[1], "bold"))
    })
    
    probData2x2 <- apply(cData2x2, 2, getProbabilities, t=cData2x2['Total',3])
    
    output$probTable2x2 <- renderDT({
      datatable(probData2x2,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>% 
        formatStyle(columns = c(0,3), 
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:3,
                    target = 'row',
                    fontWeight = styleRow(dim(probData2x2)[1], "bold"))
    })
    
    
    # pData2x2 <- matrix(pMatrixData2x2(), ncol = ncol(input$pMatrix2x2))
    # pData2x2 <- getTotaledMatrix(pData2x2, input$pMatrix2x2)
    # 
    # output$pTable2x2 <- renderDT({
    #   
    #   # excelR::excelTable(cData2x2,
    #   #            editable = FALSE,
    #   #            autoFill = TRUE) 
    #   
    #   datatable(pData2x2,
    #             class = 'cell-border stripe',
    #             options = list(
    #               dom = 't',
    #               pageLength = -1,
    #               ordering = FALSE,
    #               searching = FALSE,
    #               paging = FALSE,
    #               autoWidth = FALSE,
    #               scrollX = TRUE,
    #               columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3)),
    #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3)))
    #             ),
    #             selection = "none",
    #             escape = FALSE,
    #             filter = "none",) %>%
    #     formatStyle(columns = c(0,3),
    #                 fontWeight = 'bold') %>%
    #     formatStyle(columns = 1:3,
    #                 target = 'row',
    #                 fontWeight = styleRow(dim(pData2x2)[1], "bold"))
    # })
    
    
    cData2x3 <- matrix(cMatrixData2x3(), ncol = ncol(input$cMatrix2x3))
    cData2x3 <- getTotaledMatrix(cData2x3, input$cMatrix2x3)
    
    
    output$cTable2x3 <- renderDT({
      
      datatable(cData2x3,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>% 
        formatStyle(columns = c(0,4), #specify columns to format
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:4,
                    target = 'row',
                    fontWeight = styleRow(dim(cData2x3)[1], "bold"))
    })
    
    probData2x3 <- apply(cData2x3, 2, getProbabilities, t=cData2x3['Total',4])
    
    output$probTable2x3 <- renderDT({
      datatable(probData2x3,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>% 
        formatStyle(columns = c(0,4), 
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:4,
                    target = 'row',
                    fontWeight = styleRow(dim(probData2x3)[1], "bold"))
    })
    
    
    # pData2x3 <- matrix(pMatrixData2x3(), ncol = ncol(input$pMatrix2x3))
    # pData2x3 <- getTotaledMatrix(pData2x3, input$pMatrix2x3)
    # 
    # output$pTable2x3 <- renderDT({
    #   
    #   # excelR::excelTable(cData2x2,
    #   #            editable = FALSE,
    #   #            autoFill = TRUE) 
    #   
    #   datatable(pData2x3,
    #             class = 'cell-border stripe',
    #             options = list(
    #               dom = 't',
    #               pageLength = -1,
    #               ordering = FALSE,
    #               searching = FALSE,
    #               paging = FALSE,
    #               autoWidth = FALSE,
    #               scrollX = TRUE,
    #               columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3, 4)),
    #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
    #             ),
    #             selection = "none",
    #             escape = FALSE,
    #             filter = "none",) %>%
    #     formatStyle(columns = c(0,4),
    #                 fontWeight = 'bold') %>%
    #     formatStyle(columns = 1:4,
    #                 target = 'row',
    #                 fontWeight = styleRow(dim(pData2x3)[1], "bold"))
    # })
    
    
    cData3x2 <- matrix(cMatrixData3x2(), ncol = ncol(input$cMatrix3x2))
    cData3x2 <- getTotaledMatrix(cData3x2, input$cMatrix3x2)
    
    
    output$cTable3x2 <- renderDT({
      
      datatable(cData3x2,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(1, 2, 3)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>% 
        formatStyle(columns = c(0,3), #specify columns to format
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:3,
                    target = 'row',
                    fontWeight = styleRow(dim(cData3x2)[1], "bold"))
    })
    
    probData3x2 <- apply(cData3x2, 2, getProbabilities, t=cData3x2['Total',3])
    
    output$probTable3x2 <- renderDT({
      datatable(probData3x2,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(1, 2, 3)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>% 
        formatStyle(columns = c(0,3), 
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:3,
                    target = 'row',
                    fontWeight = styleRow(dim(probData3x2)[1], "bold"))
    })
    
    
    # pData3x2 <- matrix(pMatrixData3x2(), ncol = ncol(input$pMatrix3x2))
    # pData3x2 <- getTotaledMatrix(pData3x2, input$pMatrix3x2)
    # 
    # output$pTable3x2 <- renderDT({
    #   
    #   # excelR::excelTable(cData2x2,
    #   #            editable = FALSE,
    #   #            autoFill = TRUE) 
    #   
    #   datatable(pData3x2,
    #             class = 'cell-border stripe',
    #             options = list(
    #               dom = 't',
    #               pageLength = -1,
    #               ordering = FALSE,
    #               searching = FALSE,
    #               paging = FALSE,
    #               autoWidth = FALSE,
    #               scrollX = TRUE,
    #               columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3)),
    #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3)))
    #             ),
    #             selection = "none",
    #             escape = FALSE,
    #             filter = "none",) %>%
    #     formatStyle(columns = c(0,3),
    #                 fontWeight = 'bold') %>%
    #     formatStyle(columns = 1:3,
    #                 target = 'row',
    #                 fontWeight = styleRow(dim(pData3x2)[1], "bold"))
    # })
    
    
    
    cData3x3 <- matrix(cMatrixData3x3(), ncol = ncol(input$cMatrix3x3))
    cData3x3 <- getTotaledMatrix(cData3x3, input$cMatrix3x3)
    
    
    output$cTable3x3 <- renderDT({

      datatable(cData3x3,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>% 
        formatStyle(columns = c(0,4), #specify columns to format
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:4,
                    target = 'row',
                    fontWeight = styleRow(dim(cData3x3)[1], "bold"))
    })
    
    probData3x3 <- apply(cData3x3, 2, getProbabilities, t=cData3x3['Total',4])
    
    output$probTable3x3 <- renderDT({
      datatable(probData3x3,
                class = 'cell-border stripe',
                options = list(
                  dom = 't',
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = FALSE,
                  scrollX = TRUE,
                  columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
                                    list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
                ),
                selection = "none",
                escape = FALSE,
                filter = "none",) %>% 
        formatStyle(columns = c(0,4), 
                    fontWeight = 'bold') %>%
        formatStyle(columns = 1:4,
                    target = 'row',
                    fontWeight = styleRow(dim(probData3x3)[1], "bold"))
    })
    
    
    # pData3x3 <- matrix(pMatrixData3x3(), ncol = ncol(input$pMatrix3x3))
    # pData3x3 <- getTotaledMatrix(pData3x3, input$pMatrix3x3)
    # 
    # output$pTable3x3 <- renderDT({
    #   
    #   # excelR::excelTable(cData2x2,
    #   #            editable = FALSE,
    #   #            autoFill = TRUE) 
    #   
    #   datatable(pData3x3,
    #             class = 'cell-border stripe',
    #             options = list(
    #               dom = 't',
    #               pageLength = -1,
    #               ordering = FALSE,
    #               searching = FALSE,
    #               paging = FALSE,
    #               autoWidth = FALSE,
    #               scrollX = TRUE,
    #               columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3, 4)),
    #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
    #             ),
    #             selection = "none",
    #             escape = FALSE,
    #             filter = "none",) %>%
    #     formatStyle(columns = c(0,4),
    #                 fontWeight = 'bold') %>%
    #     formatStyle(columns = 1:4,
    #                 target = 'row',
    #                 fontWeight = styleRow(dim(pData3x3)[1], "bold"))
    # })
    
    
    if(input$cTableDimension == '2 x 2') {
      activeProbMatrix <- probData2x2
      activeCMatrix <- cData2x2
    } else if(input$cTableDimension == '2 x 3') {
      activeProbMatrix <- probData2x3
      activeCMatrix <- cData2x3
    } else if(input$cTableDimension == '3 x 2') {
      activeProbMatrix <- probData3x2
      activeCMatrix <- cData3x2
    } else if(input$cTableDimension == '3 x 3') {
      activeProbMatrix <- probData3x3
      activeCMatrix <- cData3x3
    }
    
    output$renderMarginalProbs <- renderUI({
      req(pd_iv$is_valid())
      printMarginalProbs(activeProbMatrix, activeCMatrix)
      
    })
    
    output$renderJointProbs <- renderUI({
      req(pd_iv$is_valid())
      printJointProbs(activeProbMatrix, activeCMatrix)
    })
    
    output$renderUnionProbs <- renderUI({
      req(pd_iv$is_valid())
      printUnionProbs(activeProbMatrix, activeCMatrix)
    })
    
    output$renderConditionalProbs <- renderUI({
      req(pd_iv$is_valid())
      if(ctableconditional_iv$is_valid()) {
        printConditionalProbs(activeCMatrix)
      } else {
        validate("Row and Column totals must be greater than 0 to calculate conditional probabilities.",
          
          errorClass = "myClass"
        )
      }
     
    })
  })
  
  
  
  observeEvent(input$resetcTable, {
    ResetCTable("cMatrix2x2", 2, 2, c("R1", "R2"), c("C1", "C2"))
    ResetCTable("cMatrix2x3", 2, 3, c("R1", "R2"), c("C1", "C2", "C3"))
    ResetCTable("cMatrix3x2", 3, 2, c("R1", "R2", "R3"), c("C1", "C2"))
    ResetCTable("cMatrix3x3", 3, 3, c("R1", "R2", "R3"), c("C1", "C2", "C3"))
  })
  
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
              binomForm <- paste("\\binom{", binom_n, "}{", binom_x, "}", binom_p, "^{", binom_x, "}(1-", binom_p, ")^{", binom_n, "-", binom_x, "}")
              binomVal <- round(dbinom(binom_x,binom_n,binom_p), 4)
            }
            else if(input$calcBinom == 'cumulative'){
              binomProb <- paste("P(X \\leq ", binom_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              binomForm <- paste("\\sum_{x = 0}^{", binom_x, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE), 4)
            }
            else if(input$calcBinom == 'upperTail'){
              binomProb <- paste("P(X \\geq ", binom_x, ")") # = ", pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE))
              binomForm <- paste("\\sum_{x = ", binom_x, "}^{", binom_n, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x - 1,binom_n,binom_p,lower.tail = FALSE), 4)
              
            }
            else if(input$calcBinom == 'greaterThan'){
              binomProb <- paste("P(X \\gt ", binom_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE))
              binomForm <- paste("\\sum_{x = ", binom_x + 1, "}^{", binom_n, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
              binomVal <- round(pbinom(binom_x,binom_n,binom_p,lower.tail = FALSE), 4)
            }
            else if(input$calcBinom == 'lessThan'){
              binomProb <- paste("P(X \\lt ", binom_x, ")") # = ", pbinom(binom_x - 1,binom_n,binom_p,lower.tail = TRUE))
              binomForm <- paste("\\sum_{x = 0}^{", binom_x - 1, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
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
            binomForm <- paste("\\sum_{x = ", binom_x1, "}^{", binom_x2, "} \\binom{", binom_n, "}{x}", binom_p, "^x (1-", binom_p, ")^{", binom_n, "- x}")
            binomVal <- round(pbinom(binom_x2,binom_n,binom_p,lower.tail = TRUE) - pbinom(binom_x1-1,binom_n,binom_p,lower.tail = TRUE), 4)
          }
          
          tagList(
            withMathJax(
              div(
                h3(
                  sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Bin(n = %1.0f, p = %g): \\)",
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
                sprintf("Population Mean \\( (\\mu) = np = %g\\)",
                        binom_mu),
                br(),
                br(),
                sprintf("Population Standard Deviation \\( (\\sigma) = \\sqrt{np(1 - p)} = %g\\)",
                        binom_sd),
                br(),
                br(),
                sprintf("Population Variance \\( (\\sigma^{2}) = np(1 - p) = %g\\)",
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
              poissForm <- paste("\\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^{", poisson_x, "}}{", poisson_x, "!}")
              poissVal <- round(dpois(poisson_x,poisson_mu), 4)
            }
            else if(input$calcPoisson == 'cumulative'){
              poissProb <- paste("P(X \\leq ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("\\sum_{x = 0}^{", poisson_x, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = TRUE), 4)
            }
            else if(input$calcPoisson == 'upperTail'){
              poissProb <- paste("P(X \\geq ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("1 - \\sum_{x = 0}^{", poisson_x - 1, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x - 1,poisson_mu,lower.tail = FALSE), 4)
            }
            else if(input$calcPoisson == 'greaterThan'){
              poissProb <- paste("P(X \\gt ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("1 - \\sum_{x = 0}^{", poisson_x, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
              poissVal <- round(ppois(poisson_x,poisson_mu,lower.tail = FALSE), 4)
            }
            else if(input$calcPoisson == 'lessThan'){
              poissProb <- paste("P(X \\lt ", poisson_x, ")") # = ", pbinom(binom_x,binom_n,binom_p,lower.tail = TRUE))
              poissForm <- paste("\\sum_{x = 0}^{", poisson_x - 1, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
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
            poissForm <- paste("\\sum_{x = ", poisson_x1, "}^{", poisson_x2, "} \\dfrac{e^{-", poisson_mu, "}", poisson_mu, "^x}{x!}")
            poissVal <- round(ppois(poisson_x2, poisson_mu, lower.tail = TRUE) - ppois(poisson_x1 - 1, poisson_mu, lower.tail = TRUE), 4)
          }
          
          tagList(
            withMathJax(
              div(
                h4(
                  sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim Pois(\\mu = %g): \\)",
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
                sprintf("Population Mean \\( (\\mu) = \\mu = %g\\)",
                        poisson_mu),
                br(),
                br(),
                sprintf("Population Standard Deviation \\( (\\sigma) = \\sqrt{\\mu} = %g\\)",
                        poisson_sd),
                br(),
                br(),
                sprintf("Population Variance \\( (\\sigma^{2}) = \\mu = %g\\)",
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
  
  #### Normal ----
  observeEvent(input$goNormalProb, {
    
    ##### Normal Probability ----
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
            need(input$x1Value, "Enter a value for Normally Distributed Variable (x1)"),
            need(input$x2Value, "Enter a value for Normally Distributed Variable (x2)"),
            
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
        probValue <- round((norm_x - norm_mu)/norm_sigma, 4)
        
        if(input$calcNormal == "cumulative"){
          normProb <- paste("P(X \\leq ", norm_x,")")
          normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\leq \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
          normForm <- paste("= P(Z \\leq", probValue, ")")
        }
        else if(input$calcNormal == "upperTail"){
          normProb <- paste("P(X \\gt ", norm_x,")")
          normProbTransform <- paste("P \\left( \\dfrac{X - \\mu}{\\sigma} \\gt \\dfrac{", norm_x, " - ", norm_mu, "}{", norm_sigma, "} \\right)")
          normForm <- paste("= P(Z \\gt", probValue, ")")
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
              sprintf("Calculating  \\( %s \\)   when  \\(  X \\sim N(\\mu = %g, \\sigma = %g): \\)",
                      normProb,
                      norm_mu,
                      norm_sigma)
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
            sprintf("Population Mean \\( (\\mu) = %g\\)",
                    norm_mu),
            br(),
            br(),
            sprintf("Population Standard Deviation \\( (\\sigma) = %g\\)",
                    norm_sigma),
            br(),
            br(),
            sprintf("Population Variance \\( (\\sigma^{2}) = %g\\)",
                    norm_sigma^2)
          ),
          br(),
          hr(),
          br(),
          fluidRow(
            column(width = 6,
                   plotOutput('normDistrPlot'),
            ),
            column(width = 6,
                   plotOutput('normZPlot'),
            )
          ),
          br(),
          br()
        )
      )
    })
    
    output$normDistrPlot <- renderPlot({
      req(pd_iv$is_valid())
      
      if(input$calcNormal == "between")
      {
        normLines <- c(round((input$x1Value - input$popMean)/input$popSD, 4), round((input$x2Value - input$popMean)/input$popSD, 4))
        lineLabels <- c(input$x1Value, input$x2Value) 
      }
      else
      {
        normLines <- round((input$xValue - input$popMean)/input$popSD, 4)
        lineLabels <- input$xValue
      }
      
      normPlot(getNormValue(), normLines, input$popMean, input$popSD^2, input$popSD, lineLabels, input$calcNormal)
    })
    
    output$normZPlot <- renderPlot({
      req(pd_iv$is_valid())
      
      if(input$calcNormal == "between")
      {
        req(input$x1Value <= input$x2Value)
        normLines <- c(round((input$x1Value - input$popMean)/input$popSD, 4), round((input$x2Value - input$popMean)/input$popSD, 4))
      }
      else
      {
        normLines <- round((input$xValue - input$popMean)/input$popSD, 4)
      }
      normZPlot(getNormValue(), normLines, input$calcNormal)
    })
    
    #### Sampling Distribution of the Sample Mean ----
    output$renderSampMeanDistr <- renderUI({
      
      if(!pd_iv$is_valid())
      {
        if(!sampdistrprob_iv$is_valid())
        {
          withMathJax()
          validate(
            need(input$popMean, "Enter a value for Population Mean (mu)."),
            need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
            need(input$sampDistrxValue, "Enter a value for Normally Distributed Variable (x)."),
            need(input$sampDistrSize > 0 && input$sampDistrSize %% 1 == 0, "Sample Size (n) must be a positive integer."),
            
            errorClass = "myClass"
          )
        }
        
        if(!sampdistrbetween_iv$is_valid())
        {
          validate(
            need(input$popMean, "Enter a value for Population Mean (mu)."),
            need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
            need(input$sampDistrx1Value, "Enter a value for Normally Distributed Variable (x1)."),
            need(input$sampDistrx2Value, "Enter a value for Normally Distributed Variable (x2)."),
            need(input$sampDistrSize > 0 && input$sampDistrSize %% 1 == 0, "Sample Size (n) must be a positive integer."),
            
            errorClass = "myClass"
          )
        }

        validate(
          need(input$popMean, "Enter a value for Population Mean (mu)."),
          need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
          need(input$sampDistrSize > 0 && input$sampDistrSize %% 1 == 0, "Sample Size (n) must be a positive integer."),
          
          errorClass = "myClass"
        )
      }
      
      if(input$calcNormSampDistr != 'between')
      {
        probValue <- round((input$sampDistrxValue - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4)

        if(input$calcNormSampDistr == "cumulative"){
          normProb <- paste("P(\\bar{X} \\leq ", input$sampDistrxValue,")")
          normProbTransform <- paste("P \\left( \\dfrac{\\bar{X} - \\mu}{ \\left( \\dfrac{\\sigma}{\\sqrt{n}} \\right) } \\leq \\dfrac{", input$sampDistrxValue, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\right)")
          normForm <- paste("= P(Z \\leq", probValue, ")")
        }
        else if(input$calcNormSampDistr == "upperTail"){
          normProb <- paste("P(\\bar{X} \\gt ", input$sampDistrxValue,")")
          normProbTransform <- paste("P \\left( \\dfrac{\\bar{X} - \\mu}{ \\left( \\dfrac{\\sigma}{\\sqrt{n}} \\right) } \\gt \\dfrac{", input$sampDistrxValue, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\right)")
          normForm <- paste("= P(Z \\gt", probValue, ")")
        }
      }
      else if(input$calcNormSampDistr == 'between')
      {
        norm_x1 <- input$sampDistrx1Value
        norm_x2 <- input$sampDistrx2Value
        sampSE <- input$popSD / sqrt(input$sampDistrSize)

        validate(
          need(norm_x1 <= norm_x2, "Normally Distributed Variable (x1) must be less than or equal to Normally Distributed Variable (x2)"),

          errorClass = "myClass"
        )

        normProb <- paste("P(", norm_x1, " ",  " \\leq \\bar{X} \\leq"," ", norm_x2,")")

        normProbTransform <- paste("P \\left( \\dfrac{", norm_x1, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\leq \\dfrac{\\bar{X} - \\mu}{ \\left( \\dfrac{\\sigma}{\\sqrt{n}} \\right) } \\leq",
                                   "\\dfrac{", norm_x2, " - ", input$popMean, "}{ \\left( \\dfrac{", input$popSD, "}{\\sqrt{", input$sampDistrSize, "}} \\right) } \\right)")
        normForm <- paste("= P(", round((norm_x1 - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), "\\leq Z \\leq", round((norm_x2 - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), ") = ", 
                          round(pnorm(norm_x2, input$popMean, sampSE, lower.tail = TRUE), 4), " - ", round(pnorm(norm_x1, input$popMean, sampSE, lower.tail = TRUE), 4))
      }
      
      sampMeanDistrSD <- input$popSD / sqrt(input$sampDistrSize)
      
      tagList(
        withMathJax(
          div(
            h4(
              sprintf("Calculating  \\( %s \\)   when  \\(  \\bar{X} \\sim N(\\mu_{\\bar{X}} = \\mu = %g, \\, \\sigma_{\\bar{X}} = \\dfrac{\\sigma}{\\sqrt{n}} = %0.4f): \\)",
                      normProb,
                      input$popMean,
                      sampMeanDistrSD)
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
                    getMeanNormValue()),
            br(),
            br(),
            br(),
            sprintf("Mean \\( (\\mu_{\\bar{X}}) = \\mu = %g\\)",
                    input$popMean),
            br(),
            br(),
            sprintf("Standard Deviation \\( (\\sigma_{\\bar{X}}) = \\dfrac{\\sigma}{\\sqrt{n}} = %0.4f\\)",
                    sampMeanDistrSD),
            br(),
            sprintf("Variance \\( (\\sigma_{\\bar{X}}^2) = \\dfrac{\\sigma^{2}}{n} = %g\\)",
                    input$popSD^2 / input$sampDistrSize)
          ),
          br(),
          hr(),
          br(),
          fluidRow(
            column(width = 6,
                   plotOutput('sampMeanDistrPlot'),
            ),
            column(width = 6,
                   plotOutput('sampMeanZPlot'),
            )
          ),
          br(),
          br()
        )
      )
    })
    

    
    output$sampMeanDistrPlot <- renderPlot({
      req(pd_iv$is_valid())
      withMathJax()
      
      sampSE <- round(input$popSD / sqrt(input$sampDistrSize), 4)
      
      if(input$calcNormSampDistr == "between")
      {
        req(input$sampDistrx1Value <= input$sampDistrx2Value)
        normLines <- c(round((input$sampDistrx1Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), 
                       round((input$sampDistrx2Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4))
        lineLabels <- c(input$sampDistrx1Value, input$sampDistrx2Value)
      }
      else
      {
        normLines <- round((input$sampDistrxValue - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4)
        lineLabels <- c(input$sampDistrxValue)
      }
      
      normPlot(getMeanNormValue(), normLines, input$popMean, round(input$popSD^2 / input$sampDistrSize, 4), sampSE, lineLabels, input$calcNormSampDistr)
    })
    
    
    
    
    
    output$sampMeanZPlot <- renderPlot({
      req(pd_iv$is_valid())
      
      if(input$calcNormSampDistr == "between")
      {
        req(input$sampDistrx1Value <= input$sampDistrx2Value)
        normLines <- c(round((input$sampDistrx1Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4), 
                       round((input$sampDistrx2Value - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4))
      }
      else
      {
        normLines <- round((input$sampDistrxValue - input$popMean)/(input$popSD/sqrt(input$sampDistrSize)), 4)
      }
      normZPlot(getMeanNormValue(), normLines, input$calcNormSampDistr)
    })

  })
  
  observeEvent(input$goNormalQuan, {
    
    ##### Quartiles ----
    output$renderNormQuartiles <- renderUI({
      
      validate(
        need(input$popMean, "Enter a value for Population Mean (mu)"),
        need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0"),
        
        errorClass = "myClass"
      )
      
      qOne <- round(qnorm(0.25, input$popMean, input$popSD, TRUE), 4)
      qTwo <- round(qnorm(0.5, input$popMean, input$popSD, TRUE), 4)
      qThree <- round(qnorm(0.75, input$popMean, input$popSD, TRUE), 4)
      
      tagList(
        withMathJax(
          div(
            h4(
              sprintf("Given \\( X \\sim N(\\mu  = %d, \\sigma = %d) \\) then",
                      input$popMean,
                      input$popSD),
            ),
            hr(),
            br(),
            br(),
            fluidRow(
              column(width = 5,
                     div(style = "padding-top: 60px;",
                         sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right) \\)",
                                 input$popMean,
                                 input$popSD),
                         br(),
                         br(),
                         sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le -0.6745) = 0.25\\)"),
                         br(),
                         br(),
                         sprintf("Quartile 1 \\( \\displaystyle (Q_{1}) \\) is obtained by solving for \\(x\\)"),
                         br(),
                         br(),
                         sprintf("\\( \\displaystyle x = %s + (-0.6745 \\times %s) = %s\\)",
                                 input$popMean,
                                 input$popSD,
                                 qOne),
                         br(),
                         br(),
                         br(),
                     ),
              ),
              column(width = 7,
                     plotOutput("quartile1Plot", height = "300px"),
                     br()    
              ),    
            ),
            hr(),
            br(),
            fluidRow(
              column(width = 5,
                     div(style = "padding-top: 60px;",
                         sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right) \\)",
                                 input$popMean,
                                 input$popSD),
                         br(),
                         br(),
                         sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le 0) = 0.50\\)"),
                         br(),
                         br(),
                         sprintf("Quartile 2 \\( \\displaystyle (Q_{2}) \\) is obtained by solving for \\(x\\)"),
                         br(),
                         br(),
                         sprintf("\\( \\displaystyle x = %s + (0 \\times %s) = %s\\)",
                                 input$popMean,
                                 input$popSD,
                                 qTwo),
                         br(),
                         br(),
                         br(),
                     ),
              ),
              column(width = 7,
                     plotOutput("quartile2Plot", height = "300px"),
                     br()    
              ),    
            ),
            hr(),
            br(),
            fluidRow(
              column(width = 5,
                     div(style = "padding-top: 60px;",
                         sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right)\\)",
                                 input$popMean,
                                 input$popSD),
                         br(),
                         br(),
                         sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le 0.6745) = 0.75\\)"),
                         br(),
                         br(),
                         sprintf("Quartile 3 \\( \\displaystyle (Q_{3}) \\) is obtained by solving for \\(x\\)"),
                         br(),
                         br(),
                         sprintf("\\( \\displaystyle x = %s + (0.6745 \\times %s) = %s\\)",
                                 input$popMean,
                                 input$popSD,
                                 qThree),
                         br(),
                         br(),
                     ),
              ),
              column(width = 7,
                     plotOutput("quartile3Plot", height = "300px"),
                     br(),
                     br()     
              ),    
            ),
          ),
          br(),
          br()
        )
      )
    })
    
    output$quartile1Plot <- renderPlot({
      
      req(pd_iv$is_valid())
      
      probability <- 0.25
      probLine <- round(qnorm(0.25, input$popMean, input$popSD, TRUE), 4)
      xStart <- input$popMean - (3 * input$popSD)
      xEnd <- input$popMean + (3 * input$popSD)
      
      x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
      xSeq <- unique(sort(c(x, input$popMean, probLine)))
      
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
      meanDF <- filter(df, x %in% c(input$popMean))
      lineDF <- filter(df, x %in% c(probLine))
      
      nPlot <- ggplot(df, aes(x = x, y = y)) +
        geom_line(linetype = "solid",
                  linewidth = 0.75,
                  color='#021C38') +
        geom_area(data = df,
                  aes(y=y), 
                  fill = NA, 
                  color = NA) +
        geom_area(data = subset(df, x <= probLine),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4) +
        geom_segment(data = lineDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'round',
                     linewidth = 1.25,
                     color='#021C38') +
        geom_text(data = lineDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = meanDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     lineend = 'round',
                     linewidth = 1,
                     color='#021C38',
                     alpha = 0.5) +
        geom_text(data = meanDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = df,
                     aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                     linetype = "solid",
                     linewidth = 0.5,
                     color='#021C38') +
        coord_cartesian(clip="off") +
        theme_minimal()  +
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
              axis.text.x.bottom = element_text(size = 14)) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        ylab("") +
        xlab("X") 
      
      nPlot
      
    })
    
    output$quartile2Plot <- renderPlot({
      
      req(pd_iv$is_valid())
      
      probability <- 0.5
      probLine <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
      xStart <- input$popMean - (3 * input$popSD)
      xEnd <- input$popMean + (3 * input$popSD)
      
      x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
      xSeq <- unique(sort(c(x, input$popMean, probLine)))
      
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
      meanDF <- filter(df, x %in% c(input$popMean))
      lineDF <- filter(df, x %in% c(probLine))
      
      nPlot <- ggplot(df, aes(x = x, y = y)) +
        geom_line(linetype = "solid",
                  linewidth = 0.75,
                  color='#021C38') +
        geom_area(data = df,
                  aes(y=y), 
                  fill = NA, 
                  color = NA) +
        geom_area(data = subset(df, x <= probLine),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4) +
        geom_segment(data = lineDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'round',
                     linewidth = 1.25,
                     color='#021C38') +
        geom_text(data = lineDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = meanDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     lineend = 'round',
                     linewidth = 1,
                     color='#021C38',
                     alpha = 0.5) +
        geom_text(data = meanDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = df,
                     aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                     linetype = "solid",
                     linewidth = 0.5,
                     color='#021C38') +
        coord_cartesian(clip="off") +
        theme_minimal()  +
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
              axis.text.x.bottom = element_text(size = 14)) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        ylab("") +
        xlab("X") 
      
      nPlot
      
    })
    
    output$quartile3Plot <- renderPlot({
      
      req(pd_iv$is_valid())
      
      probability <- 0.75
      probLine <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
      xStart <- input$popMean - (3 * input$popSD)
      xEnd <- input$popMean + (3 * input$popSD)
      
      x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
      xSeq <- unique(sort(c(x, input$popMean, probLine)))
      
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
      meanDF <- filter(df, x %in% c(input$popMean))
      lineDF <- filter(df, x %in% c(probLine))
      
      nPlot <- ggplot(df, aes(x = x, y = y)) +
        geom_line(linetype = "solid",
                  linewidth = 0.75,
                  color='#021C38') +
        geom_area(data = df,
                  aes(y=y), 
                  fill = NA, 
                  color = NA) +
        geom_area(data = subset(df, x <= probLine),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4) +
        geom_segment(data = lineDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'round',
                     linewidth = 1.25,
                     color='#021C38') +
        geom_text(data = lineDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = meanDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     lineend = 'round',
                     linewidth = 1,
                     color='#021C38',
                     alpha = 0.5) +
        geom_text(data = meanDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = df,
                     aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                     linetype = "solid",
                     linewidth = 0.5,
                     color='#021C38') +
        coord_cartesian(clip="off") +
        theme_minimal()  +
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold.italic", vjust = -1),
              axis.text.x.bottom = element_text(size = 14)) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        ylab("") +
        xlab("X") 
      
      nPlot
      
    })
    
    output$renderNormPercentile <- renderUI({
      
      validate(
        need(input$popMean, "Enter a value for Population Mean (mu)."),
        need(input$popSD && input$popSD > 0, "Population Standard Deviation (sigma) must be greater than 0."),
        need(input$percentileValue, "Enter a Percentile Value between 0 and 100."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(input$percentileValue > 0 && input$percentileValue < 100, "Percentile Value must be between 0 and 100"),
        
        errorClass = "myClass"
      )
      
      if(input$percentileValue %% 10 == 1) {
        ordinal <- 'st'
      } else if(input$percentileValue %% 10 == 2) {
        ordinal <- 'nd'
      } else if(input$percentileValue %% 10 == 3) {
        ordinal <- 'rd'
      } else {
        ordinal <- 'th'
      }
      
      probability <- (input$percentileValue / 100)
      zVal <- round(qnorm(probability, 0, 1, TRUE), 4)
      percentile <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
      
      tagList(
        withMathJax(
          div(
            h4(
              sprintf("Given \\( X \\sim N(\\mu  = %d, \\sigma = %d) \\) then",
                      input$popMean,
                      input$popSD),
            ),
            hr(),
            br(),
            br(),
            fluidRow(
              column(width = 5,
                     div(style = "padding-top: 60px;",
                         sprintf("\\( P(X \\le x) = P \\left( \\dfrac{X - \\mu}{\\sigma} \\le \\dfrac{x - %s}{%s} \\right)\\)",
                                 input$popMean,
                                 input$popSD),
                         br(),
                         br(),
                         sprintf("\\( \\phantom{ P(X\\lex) } = P(Z \\le %s) = %s\\)",
                                 zVal,
                                 probability),
                         br(),
                         br(),
                         sprintf("the \\( \\displaystyle %d^{%s} \\) percentile is obtained by solving for \\(x\\)",
                                 input$percentileValue,
                                 ordinal),
                         br(),
                         br(),
                         sprintf("\\( \\displaystyle x = %s + (%s \\times %s) = %s\\)",
                                 input$popMean,
                                 zVal,
                                 input$popSD,
                                 percentile),
                         br(),
                         br(),
                         br(),
                     ),
              ),
              column(width = 7,
                     plotOutput("percentilePlot", height = "300px"),
                     br(),
                     br()     
              ),    
            ),
          ),
          br(),
          br()
        ),
      )
    })
    
    output$percentilePlot <- renderPlot({

      req(pd_iv$is_valid())

      probability <- input$percentileValue / 100
      percentileLine <- round(qnorm(probability, input$popMean, input$popSD, TRUE), 4)
      xStart <- input$popMean - (3 * input$popSD)
      xEnd <- input$popMean + (3 * input$popSD)
      
      x <- round(seq(from = xStart, to = xEnd, length.out = 60), 2)
      xSeq <- unique(sort(c(x, input$popMean, percentileLine)))
      
      df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = input$popMean, sd = input$popSD)))
      meanDF <- filter(df, x %in% c(input$popMean))
      lineDF <- filter(df, x %in% c(percentileLine))
      
      nPlot <- ggplot(df, aes(x = x, y = y)) +
        geom_line(linetype = "solid",
                  linewidth = 0.75,
                  color='#021C38') +
        geom_area(data = df,
                  aes(y=y), 
                  fill = NA, 
                  color = NA) +
        geom_area(data = subset(df, x <= percentileLine),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4) +
        geom_segment(data = lineDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "solid",
                     lineend = 'round',
                     linewidth = 1,
                     color='#021C38') +
        geom_text(data = lineDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = meanDF,
                     aes(x = x, xend = x, y = 0, yend = y),
                     linetype = "dotted",
                     lineend = 'round',
                     linewidth = 1,
                     color='#021C38',
                     alpha = 0.5) +
        geom_text(data = meanDF, 
                  aes(x = x, y = 0, label = x), 
                  size = 16 / .pt,
                  fontface = "bold",
                  check_overlap = TRUE,
                  vjust = 1.5) +
        geom_segment(data = df,
                     aes(x = xStart, xend = xEnd, y = 0, yend = 0),
                     linetype = "solid",
                     linewidth = 0.5,
                     color='#021C38') +
        coord_cartesian(clip="off") +
        theme_minimal()  +
        theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold", vjust = -1),
              axis.text.x.bottom = element_text(size = 14)) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        ylab("") +
        xlab("X") 
      
      nPlot
      
    })
    
  })
  
  
  # --------------------------------------------------------------------- #
  
  
# **************************************************************************** #
  
  
  #  -------------------------------------------------------------------- #
  ## ---------------- Sample Size Estimation functions ------------------
  #  -------------------------------------------------------------------- #
  
  
  ### Non-Reactive Functions ----
  # --------------------------------------------------------------------- #
  
  getSampSizeEstMean <- function(critVal, popuSD, margErr) {
    
    n <- ((critVal * popuSD) / margErr) ^ 2
    
    return(n)
  }
  
  
  getSampSizeEstProp <- function(critVal, phat, margErr) {
    
    n <- phat * (1 - phat) * (critVal / margErr)^2
    
    return(n)
  }
  
  ### Outputs ----
  # --------------------------------------------------------------------- #
  
  
  #### Validation ----
  
  # Sample Size Estimation Validation 
  # ------------------------------------------------------------------------ #
  
  output$ssEstimationValidation <- renderUI({
      
    if(!sse_iv$is_valid()){
      
      if(!ssemean_iv$is_valid()){
        
        validate(
          need(input$ssePopuSD && input$ssePopuSD > 0, "Population Standard Deviation must be positive."),
          need(input$sseMeanMargErr && input$sseMeanMargErr > 0, "Margin of Error must be positive."),
          
          
          errorClass = "myClass"
        )
      }else if(!sseprop_iv$is_valid()){
        
        validate(
          need(input$sseTargetProp, "Target Proportion must be between 0 and 1.") %then%
            need(input$sseTargetProp > 0 && input$sseTargetProp < 1, "Target Proportion must be between 0 and 1."),
          need(input$ssePropMargErr, "Margin of Error must be greater than 0 and less than or equal to 1.") %then%
            need(input$ssePropMargErr > 0 && input$ssePropMargErr <= 1, "Margin of Error must be greater than 0 and less than or equal to 1."),
          
          errorClass = "myClass"
        )
      } 
    }
    
  })
  
  
  #### Sample Size Est Mean output ----
  output$sampSizeMeanEstimate <- renderUI({
    
    n <- getSampSizeEstMean(criticalValue(), input$ssePopuSD, input$sseMeanMargErr)
    nEstimate <- ceiling(n)
    
    
    tagList(
      withMathJax(),
      br(),
      br(),
      sprintf("\\( n = \\left( \\dfrac{Z_{\\alpha / 2} \\: \\sigma}{E} \\right)^{2} \\)"),
      sprintf("\\( = \\left( \\dfrac{ (%s)(%s) }{%s} \\right)^{2} \\)",
              criticalValue(),
              input$ssePopuSD,
              input$sseMeanMargErr),
      sprintf("\\( = %0.4f \\)",
              n),
      br(),
      br(),
      sprintf("\\( n \\approx %1.0f \\)",
              nEstimate),
      br(),
      br(),
      br(),
      sprintf("The recommended sample size (\\( n \\)) is \\(%1.0f\\) for a \\( %s \\)%% confidence 
              level with a population standard deviation \\( (\\sigma) = %s\\) and
              margin of error \\( (E) = %s \\).",
              nEstimate,
              input$confLeveln,
              input$ssePopuSD,
              input$sseMeanMargErr),
      br(),
    )
  })
  
  #### Sample Size Est Proportion output ----
  output$sampSizePropEstimate <- renderUI({
    
    n <- getSampSizeEstProp(criticalValue(), input$sseTargetProp, input$ssePropMargErr)
    nEstimate <- ceiling(n)
    
    
    tagList(
      withMathJax(),
      br(),
      br(),
      sprintf("\\( n = \\hat{p} (1 - \\hat{p}) \\left( \\dfrac{Z_{\\alpha / 2}}{E} \\right)^{2} \\)"),
      sprintf("\\( = \\; %s \\; (%s) \\left( \\dfrac{%s}{%s} \\right)^{2} \\)",
              input$sseTargetProp,
              1 - input$sseTargetProp,
              criticalValue(),
              input$ssePropMargErr),
      sprintf("\\( = \\; %0.4f \\)",
              n),
      br(),
      br(),
      sprintf("\\( n \\approx %1.0f \\)",
              nEstimate),
      br(),
      br(),
      br(),
      sprintf("The recommended sample size (\\( n \\)) is \\(%1.0f\\) for a \\( %s \\)%% confidence 
              level with a target proportion \\( (\\hat{p}) = %s\\) and
              margin of error \\( (E) = %s \\).",
              nEstimate,
              input$confLeveln,
              input$sseTargetProp,
              input$ssePropMargErr),
      br(),
    )
  })
  
 
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  
  observeEvent(input$goSampSizeEst, {
    
    if(sse_iv$is_valid()) {
      show(id = "ssEstimationData")
      
    } else {
      hide(id = "ssEstimationData")
    }
  })
  
  
  
  # --------------------------------------------------------------------- #
  
  
  # **************************************************************************** #
  
  
  #  -------------------------------------------------------------------- #
  ## ----------------- Statistical Inference functions ------------------
  #  -------------------------------------------------------------------- #
  
  
  ### Non-Reactive Functions ----
  # --------------------------------------------------------------------- #
  
  #### One Mean Functions ----
  
  printHTConclusion <- function(region, reject, suffEvidence, altHyp, altHypValue) {
    conclusion <- tagList(
      withMathJax(),
      p(tags$b("Conclusion:")),
      sprintf("At \\( \\alpha = %s \\), since the test statistic falls in the %s region we %s \\(
               H_{0}\\) and conclude that there %s enough statistical evidence to support that \\(%s %s\\).",
              SigLvl(),
              region,
              reject,
              suffEvidence,
              altHyp,
              altHypValue),
      br(),
      br()
    )
    
    return(conclusion)
  }
  
  
  printOneMeanCI <- function() {
    
    oneMeanData <- GetOneMeanCI()
    
    if(OneMeanSigma() == "Known"){
      sdSymbol <- "\\sigma"
      testStat <- "z"
      critVal <- oneMeanData["Z Critical"]
      
    } else {
      sdSymbol <- "s"
      testStat <- "t"
      critVal <- oneMeanData["T Critical"]
    }
    
    oneMeanCIOutput <- tagList()
    
    givenOutput <- printOneMeanGiven()
    
    alphaOutput <- tagList(
      withMathJax(),
      sprintf("For a \\( %s \\)%% Confidence Interval: ",
              ConfLvl()*100),
      br(),
      sprintf("\\( \\alpha = 1 - %s = %s \\)",
              ConfLvl(),
              1 - ConfLvl()),
      br())
    
    cvOutput <- printOneMeanCV()
    formulaOutput <- printOneMeanCIFormula()
    calcOutput <- printOneMeanCICalc()
    intrpOutput <- printOneMeanCIIntrp()
    
    oneMeanCIOutput <- tagAppendChildren(oneMeanCIOutput, givenOutput, alphaOutput, cvOutput, formulaOutput, calcOutput, intrpOutput)

    return(oneMeanCIOutput)
  }
  
  printOneMeanCV <- function() {
    
    oneMeanData <- GetOneMeanCI()
    
    if(OneMeanSigma() == "Known"){
      cvOutput <- tagList(
                          sprintf("\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
                                  1 - ConfLvl(),
                                  (1 - ConfLvl()) / 2,
                                  oneMeanData["Z Critical"]),
                          br(),
                          br(),
                          br(),
                  )
    } else {
      df <- oneMeanData["Sample Size"] - 1
      
      cvOutput <- tagList(
                          sprintf("\\( df = n - 1 \\)"),
                          br(),
                          sprintf("\\( t_{\\alpha/2, \\, df} = t_{%s/2, \\, %s} = t_{%s, \\, %s} = %s \\)",
                                  1 - ConfLvl(),
                                  df,
                                  (1 - ConfLvl()) / 2,
                                  df,
                                 oneMeanData["T Critical"]),
                          br(),
                          br(),
                          br()
                  )
    }
    
    return(cvOutput)
  }
  
  printOneMeanGiven <- function() {
    oneMeanData <- GetOneMeanCI()
    
    if(input$dataAvailability == 'Summarized Data') {
      if(OneMeanSigma() == 'Known') {
        sd <- '\\sigma'
      } else {
        sd <- 's'
      }
      
      givenOutput <- tagList(
        sprintf("Given:"),
        br(),
        sprintf("\\( n = %s \\)",
                oneMeanData['Sample Size']),
        br(),
        sprintf("\\( \\bar{x} = %s \\)",
                oneMeanData['Sample Mean']),
        br(),
        sprintf("\\( %s = %s \\)",
                sd,
                oneMeanData[3]),
        br(),
        br(),
        br()
      )
      
    } else {
      
      if(OneMeanSigma() == 'Known') {
        givenOutput <- tagList(
          sprintf("Given:"),
          br(),
          sprintf("\\( \\sigma = %s \\)",
                  oneMeanData[3]),
          br(),
          br(),
          br()
        )
      } else {
        givenOutput <- br()

      }
      
    }
  }
  
  printOneMeanCIFormula <- function() {
    oneMeanData <- GetOneMeanCI()
    
    if(OneMeanSigma() == 'Known') {
      sd <- "\\sigma"
      testStat <- "z_{\\alpha/2}"
    } else {
      sd <- "s"
      testStat <- "t_{\\alpha/2, \\, df}"
    }

    formulaOutput <- tagList(
      sprintf("\\( \\displaystyle CI = \\bar{x} \\pm \\left( %s \\dfrac{%s}{\\sqrt{n}} \\right) \\)",
              testStat,
              sd),
      br()
    )
    
    if(input$dataAvailability != "Summarized Data") {
      formulaOutput <- tagAppendChild(formulaOutput, printOneMeanWhere(oneMeanData))
    } else {
      formulaOutput <- tagAppendChildren(formulaOutput, br(), br())
    }
    
    return(formulaOutput)
  }
  
  printOneMeanWhere <- function(oneMeanData) {

    if(OneMeanSigma() == 'Known') {
      formulaOutput <- tagList(
        sprintf("where"),
        br(),
        sprintf("\\( \\phantom{CII} n = %s \\; , \\)",
                oneMeanData["Sample Size"]),
        sprintf("\\( \\phantom{CII} \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%s} = %s \\)",
                OneMeanTotaledData()[1],
                oneMeanData["Sample Size"],
                oneMeanData["Sample Mean"]),
        br(),
        br(),
        br()
      )
    } else {
      formulaOutput <- tagList(
        sprintf("where"),
        br(),
        sprintf("\\( \\phantom{CII} n = %s \\; , \\)",
                oneMeanData["Sample Size"]),
        sprintf("\\( \\phantom{CII} \\bar{x} = \\dfrac{\\sum x}{n} = \\dfrac{%s}{%s} = %s \\; , \\)",
                OneMeanTotaledData()[1],
                oneMeanData["Sample Size"],
                oneMeanData["Sample Mean"]),
        br(),
        sprintf("and"),
        br(),
        sprintf("\\( \\phantom{CII} s  = \\sqrt{ \\dfrac{\\sum x^{2} - \\dfrac{(\\sum x)^{2}}{n} }{n - 1} } \\)"),
        sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\)",
                OneMeanTotaledData()[2],
                OneMeanTotaledData()[1],
                oneMeanData["Sample Size"],
                oneMeanData["Sample Size"],
                oneMeanData[3]),
        br(),
        br(),
        br()
      )
    } 
    
  }
  
  printOneMeanCICalc <- function() {
    oneMeanData <- GetOneMeanCI()
    
    if(OneMeanSigma() == "Known"){
      critVal <- oneMeanData["Z Critical"]
    } else {
      critVal <- oneMeanData["T Critical"]
    }
    
    calcOutput <- tagList(
      sprintf("\\( \\displaystyle CI = %s \\pm \\left( %g \\dfrac{%g}{\\sqrt{%g}} \\right) \\)",
              oneMeanData["Sample Mean"],
              critVal,
              oneMeanData[3],
              oneMeanData['Sample Size']),
      br(),
      br(),
      sprintf("\\( \\displaystyle \\phantom{CI} = %s \\pm \\left( %g \\cdot %g \\right) \\)",
              oneMeanData["Sample Mean"],
              critVal,
              oneMeanData['Std Error']),
      br(),
      br(),
      sprintf("\\( \\displaystyle \\phantom{CI} = %s \\pm %g \\)",
              oneMeanData["Sample Mean"],
              oneMeanData['ME']),
      br(),
      br(),
      sprintf("\\( \\phantom{CI} = (%g, %g)\\)",
              oneMeanData["LCL"],
              oneMeanData["UCL"]),
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
      sprintf("We are %1.0f%% confident that the population mean \\( (\\mu)\\) is between \\( %g \\) and \\( %g \\).",
              ConfLvl()*100,
              oneMeanData["LCL"],
              oneMeanData["UCL"]),
      br()
    )
    
    return(intrpOutput)
  }
  
  printOneMeanHT <- function() {
    
    oneMeanData <- GetOneMeanHT()
    intrpInfo <- OneMeanHypInfo()
    
    if(OneMeanSigma() == 'Known') {
      sdSymbol <- "\\sigma"
      testStat <- "z"
    } else {
      sdSymbol <- "s"
      testStat <- "t"
    }
    
    if(oneMeanData[7] > SigLvl())
    {
      pvalSymbol <- "\\gt"
      suffEvidence <- "isn't"
      reject <- "do not reject"
      region <- "acceptance"
    }
    else
    {
      pvalSymbol <- "\\leq"
      suffEvidence <- "is"
      reject <- "reject"
      region <- "rejection"
    }
    
    oneMeanHTOutput <- tagList(
      withMathJax(),
      sprintf("\\( H_{0}: %s %s\\)",
              intrpInfo$nullHyp,
              input$hypMean),
      br(),
      sprintf("\\( H_{a}: %s %s\\)",
              intrpInfo$altHyp,
              input$hypMean),
      br(),
      br(),
      sprintf("\\( \\alpha = %s \\)",
              SigLvl()),
      br(),
      br(),
      br(),
      p(tags$b("Test Statistic:")))
    
    givenOutput <- printOneMeanGiven()
    formulaOutput <- printOneMeanHTFormula(sdSymbol, testStat)
    pvalOutput <- printHTPVal(oneMeanData["P-Value"], testStat, intrpInfo$alternative, oneMeanData["Test Statistic"], pvalSymbol, reject)
    cvOutput <- printOneMeanHTCV(testStat, reject, region)
    conclusionOutput <- printHTConclusion(region, reject, suffEvidence, OneMeanHypInfo()$altHyp, input$hypMean)
    
    tagAppendChildren(oneMeanHTOutput, givenOutput, formulaOutput, pvalOutput, cvOutput, conclusionOutput)
  }
  
  printOneMeanHTFormula <- function(sdSymbol, testStat) {
    oneMeanData <- GetOneMeanHT()
    
    formulaOutput <- tagList(
      sprintf("\\(%s = \\dfrac{\\bar{x} - \\mu_{0}}{ \\dfrac{%s}{\\sqrt{n}} } \\)",
            testStat,
            sdSymbol),
      br()
    )
    
    if(input$dataAvailability != "Summarized Data") {
      formulaOutput <- tagAppendChild(formulaOutput, printOneMeanWhere(oneMeanData)) 
    } else {
      formulaOutput <- tagAppendChildren(formulaOutput, br(), br())
    }
    
    calcOutput <- tagList(
      sprintf("\\(%s =  \\dfrac{%s - %s}{ \\dfrac{%s}{\\sqrt{%s}} }\\)",
              testStat,
              oneMeanData[2],
              input$hypMean,
              oneMeanData[3],
              oneMeanData[1]),
      sprintf("\\( = \\dfrac{%0.4f}{%s} \\)",
              oneMeanData[2] - input$hypMean,
              oneMeanData["Std Error"]),
      br(),
      br(),
      sprintf("\\(\\phantom{%s} = %0.4f\\)",
              testStat,
              oneMeanData[6]),
      br(),
      br(),
      br()
    )
    
    formulaOutput <- tagAppendChild(formulaOutput, calcOutput)
    return(formulaOutput)
  }
  
  printOneMeanHTCV <- function(testStat, reject, region) {
    oneMeanData <- GetOneMeanHT()
    
    if(OneMeanHypInfo()$alternative == "two.sided")
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
    
    if(testStat == 'z') {
      
      cvOutput <- tagList(
        p(tags$b("Using Critical Value Method:")),
        sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$critAlph,
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$alphaVal,
                critVal),
        br(),
        br(),
      )
      
    } else {
      
      cvOutput <- tagList(
        p(tags$b("Using Critical Value Method:")),
        sprintf("\\( df = n - 1 = %s \\)",
                oneMeanData["Sample Size"] - 1),
        br(),
        br(),
        sprintf("Critical Value(s) \\( = %s t_{%s, \\, df} = %s t_{%s, \\, %s} = %s \\)",
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$critAlph,
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$alphaVal,
                oneMeanData["Sample Size"] - 1,
                critVal),
        br(),
        br()
      )
    }
    
    cvEnd <- tagList(
      sprintf("Since the test statistic \\( (%s)\\) falls within the %s region, %s \\( H_{0}\\).",
              testStat,
              region,
              reject),
      br(),
      br(),
      plotOutput('oneMeanHTPlot', width = "75%", height = "300px"),
      br()
    )
    
    cvOutput <- tagAppendChild(cvOutput, cvEnd)
    return(cvOutput)
  }
  
  printHTPVal <- function(pValue, testStat, alternative, tsValue, pvalSign, reject) {
    oneMeanData <- GetOneMeanHT()
    
    if(oneMeanData[7] < 0.0001)
    {
      pValue <- "P \\lt 0.0001"
    }
    else
    {
      pValue <- paste(oneMeanData[7])
    }

    
    if(alternative == "two.sided"){
      pvalCalc <- paste("2 \\times P(", testStat, "\\, \\gt \\; \\mid", tsValue, "\\mid)")
    } else if (alternative == "greater"){
      pvalCalc <- paste("P(", testStat, "\\, > \\,", tsValue, ")")
    } else {
      pvalCalc <- paste("P(", testStat, "\\, < \\,", tsValue, ")")
    }
    
    pvalOutput <- tagList(
      p(tags$b("Using P-Value Method:")),
      sprintf("\\(P = %s = %s\\)",
              pvalCalc,
              pValue),
      br(),
      br(),
      sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
              pvalSign,
              SigLvl(),
              reject),
      br(),
      br(),
      br(),
    )
    
    return(pvalOutput)
  }
  
  
  GetDepMeansData <- function() {
    req(si_iv$is_valid())
    
    dat <- list()
    
    if(input$dataTypeDependent == 'Upload Data') {
      sampBefore <- na.omit(unlist(DepMeansUploadData()[,input$depMeansUplSample1]))
      sampAfter <- na.omit(unlist(DepMeansUploadData()[,input$depMeansUplSample2]))
    } else if(input$dataTypeDependent == 'Enter Raw Data') {
      sampBefore <- createNumLst(input$before)
      sampAfter <- createNumLst(input$after)
    }
    
    dat$before <- sampBefore
    dat$after <- sampAfter
    dat$d <- (sampBefore - sampAfter)
    dat$n  <- length(sampBefore)
    dat$dbar <- sum(dat$d) / dat$n
    dat$sd <- sqrt(sum((dat$d - dat$dbar)^2) / (dat$n - 1))
    
    return(dat)
  }
  
  shadeHtArea <- function(df, critValue, altHypothesis) {
    
      if(altHypothesis == 'less') {
        geom_area(data = subset(df, x <= critValue),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4)
          
      # } else if (altHypothesis == 'two.sided') {
      #   geom_area(data = subset(df, x <= critValueLeft),
      #             aes(y=y), 
      #             fill = "#023B70", 
      #             color = NA, 
      #             alpha = 0.4) +
      #   geom_area(data = subset(df, x >= critValue),
      #             aes(y=y), 
      #             fill = "#023B70", 
      #             color = NA, 
      #             alpha = 0.4)
          
      } else if (altHypothesis == 'greater') {
        geom_area(data = subset(df, x >= critValue),
                  aes(y=y), 
                  fill = "#023B70", 
                  color = NA, 
                  alpha = 0.4)
      }
  }
    
  #   if(altHypothesis == "less") #less
  #   {
  #     area[x > critValues] <- NA
  #   }
  #   else if(altHypothesis == "two.sided") #twosided
  #   {
  #     area[x > critValues[1] & x < critValues[2]] <- NA
  #   }
  #   else if(altHypothesis == "greater") #greater
  #   {
  #     area[x < critValues] <- NA
  #   }
  #   return(area)
  # }
  
  hypZTestPlot <- function(testStatistic, critValue, altHypothesis){
    # normTail = qnorm(0.999, mean = 0, sd = 1, lower.tail = FALSE)
    # normHead = qnorm(0.999, mean = 0, sd = 1, lower.tail = TRUE)
    # xSeq = sort(c(normTail, normHead, testStatistic, critValues, 0))
    
    x <- round(seq(from = -3, to = 3, by = 0.1), 2)
    
    if(altHypothesis == "two.sided") {
      CVs <- c(-critValue, critValue)
      RRLabels <- c((-critValue + -3)/2, (critValue + 3)/2)
    } else{
      CVs <- c(critValue)
      if(altHypothesis == 'less') {
        RRLabels <- c((critValue + -3)/2)
      } else {
        RRLabels <- c((critValue + 3)/2)
      }
    }
    
    xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
      
    # if(testStatistic < normTail)
    # {
    #   normTail = testStatistic
    #   
    # } else if(testStatistic > normHead)
    # {
    #   normHead = testStatistic
    # } 
    
    df <- distinct(data.frame(x = xSeq, y = dnorm(xSeq, mean = 0, sd = 1)))
    cvDF <- filter(df, x %in% CVs)
    RRLabelsDF <- filter(df, x %in% RRLabels)
    tsDF <- filter(df, x %in% testStatistic)
    centerDF <- filter(df, x %in% c(0))
    
    htPlot <- ggplot(df, aes(x = x, y = y)) 
    
    if(altHypothesis == 'two.sided') {
      htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
                         shadeHtArea(df, critValue, "greater")
    } else {
      htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
    }

    htPlot <- htPlot + geom_segment(data = cvDF,
                                    aes(x = x, xend = x, y = 0, yend = y),
                                    linetype = "solid",
                                    lineend = 'butt',
                                    linewidth = 1.5,
                                    color='#023B70') +
                       stat_function(fun = dnorm, 
                                     geom = "density",
                                     fill = NA) + 
                       theme_void() +  
                       scale_y_continuous(breaks = NULL) +
                       ylab("") + xlab("Z") +
                       geom_segment(data = filter(df, x %in% c(0)),
                                    aes(x = x, xend = x, y = 0, yend = y),
                                    linetype = "dotted",
                                    linewidth = 0.75,
                                    color='black') +
                       geom_text(data = filter(df, x %in% c(0)),
                                 aes(x = x, y = y/2, label = "A R"),
                                 size = 16 / .pt,
                                 check_overlap = TRUE,
                                 fontface = "bold") +
                       geom_text(data = filter(df, x %in% c(0)),
                                 aes(x = x, y = 0, label = "0"),
                                 size = 14 / .pt,
                                 fontface = "bold",
                                 nudge_y = -.03,
                                 check_overlap = TRUE) +
                       geom_segment(data = tsDF,
                                    aes(x = x, xend = x, y = 0, yend = y + .055),
                                    linetype = "solid",
                                    linewidth = 1.25,
                                    color='#BD130B') +
                       geom_text(data = tsDF,
                                 aes(x = x, y = y, label = x),
                                 size = 14 / .pt,
                                 fontface = "bold",
                                 nudge_y = .075,
                                 check_overlap = TRUE) +
                       geom_text(data = cvDF,
                                 aes(x = x, y = 0, label = x),
                                 size = 14 / .pt,
                                 fontface = "bold",
                                 nudge_y = -.03,
                                 check_overlap = TRUE) +
                       geom_text(data = RRLabelsDF,
                                 aes(x = x, y = y, label = "RR"),
                                 size = 16 / .pt,
                                 fontface = "bold",
                                 nudge_y = .025,
                                 check_overlap = TRUE) +
                       theme(axis.title.x = element_text(size = 16, face = "bold.italic")) +
                       coord_cartesian(clip="off")
    
    return(htPlot)
  }
  
  
  hypTTestPlot <- function(testStatistic, degfree, critValue, altHypothesis){
    tTail = qt(0.999, df = degfree, lower.tail = FALSE)
    tHead = qt(0.999, df = degfree, lower.tail = TRUE)
    x <- round(seq(from = tTail, to = tHead, by = 0.1), 2)
    
    if(altHypothesis == "two.sided") {
      CVs <- c(-critValue, critValue)
      RRLabels <- c((-critValue + tTail)/2, (critValue + tHead)/2)
    } else{
      CVs <- c(critValue)
      if(altHypothesis == 'less') {
        RRLabels <- c((critValue + tTail)/2)
      } else {
        RRLabels <- c((critValue + tHead)/2)
      }
    }
    
    xSeq <- unique(sort(c(x, testStatistic, CVs, RRLabels, 0)))
    
    df <- data.frame(x = xSeq, y = dt(xSeq, degfree))
    cvDF <- filter(df, x %in% CVs)
    RRLabelsDF <- filter(df, x %in% RRLabels)
    tsDF <- filter(df, x %in% testStatistic)
    centerDF <- filter(df, x %in% c(0))
    
    htPlot <- ggplot(df, aes(x = x, y = y)) 
    
    if(altHypothesis == 'two.sided') {
      htPlot <- htPlot + shadeHtArea(df, -critValue, "less") +
                         shadeHtArea(df, critValue, "greater")
    } else {
      htPlot <- htPlot + shadeHtArea(df, critValue, altHypothesis)
    }
    
    htPlot <- htPlot + stat_function(fun = dt, 
                                     args = list(df = degfree), 
                                     geom = "density",
                                     fill = NA) + 
                       theme_void()  +
                       scale_y_continuous(breaks = NULL) +
                       ylab("") + 
                       xlab("t") +
                       geom_segment(data = filter(df, x %in% c(0)), 
                                    aes(x = x, xend = x, y = 0, yend = y), 
                                    linetype = "dotted", 
                                    linewidth = 0.75, 
                                    color='black') +
                       geom_text(data = filter(df, x %in% c(0)), 
                                 aes(x = x, y = y/2, label = "A R"), 
                                 size = 16 / .pt, 
                                 fontface = "bold") +
                       geom_text(data = filter(df, x %in% c(0)), 
                                 aes(x = x, y = 0, label = "0"), 
                                 size = 14 / .pt, 
                                 fontface = "bold", 
                                 nudge_y = -.03) +
                       geom_segment(data = tsDF, 
                                    aes(x = x, xend = x, y = 0, yend = y + .03), 
                                    linetype = "solid", 
                                    linewidth = 1.25, 
                                    color='#BD130B') +
                       geom_text(data = tsDF, 
                                 aes(x = x, y = y, label = x), 
                                 size = 16 / .pt, 
                                 fontface = "bold", 
                                 nudge_y = .075) +
                       geom_segment(data = cvDF, 
                                    aes(x = x, xend = x, y = 0, yend = y), 
                                    linetype = "solid", 
                                    lineend = 'butt', 
                                    linewidth = 1.5, 
                                    color='#023B70') +
                       geom_text(data = cvDF, 
                                 aes(x = x, y = 0, label = x), 
                                 size = 14 / .pt, 
                                 fontface = "bold", 
                                 nudge_y = -.03) +
                       geom_text(data = RRLabelsDF, 
                                 aes(x = x, y = y, label = "RR"), 
                                 size = 16 / .pt, 
                                 fontface = "bold", 
                                 nudge_y = .03) +
                       theme(axis.title.x = element_text(size = 16, 
                                                         face = "bold.italic"))
    
    return(htPlot)
  }
  
  
  #### ANOVA Functions ----
  
  PrintANOVA <- function() {
    sigLvl <- 5
    
    hypothesis <- PrintANOVAHyp(sigLvl)
    testStat <- PrintANOVAFormula()
    tagAppendChildren(hypothesis, testStat)
  }
  
  PrintANOVAHyp <- function(sigLvl) {
    anovaData <- anovaOneWayResults()$data
    numFactors <- anovaOneWayResults()$numFactors
    factorCol <- anovaOneWayResults()$factorCol
    factorNames <- anovaOneWayResults()$factorNames
    
    nullHyp <- "H_{0} : "
    
    for(mu in 1:(numFactors - 1)) {
      nullHyp <- paste0(nullHyp, "\\mu_{", mu, "} = ")
    }
    nullHyp <- paste0(nullHyp, "\\mu_{", numFactors, "}")
    
    hypothesis <- tagList(
      withMathJax(),
      sprintf("\\( %s \\) ",
              nullHyp),
      br(),
      sprintf("\\( H_{a}: \\) At least two means differ"),
      br(),
      br(),
      sprintf("\\( \\alpha = %s \\)",
              sigLvl),
      br(),
      br(),
      sprintf("\\( n = %s \\)",
              anovaOneWayResults()$count),
      br(),
      sprintf("\\( k = \\) number of factors \\( = %s \\)",
              numFactors),
      br(),
      br(),
    )
    
    for(name in factorNames) {
      factorCount <- tagList(
        sprintf("\\(n_{%s} = %s\\)",
                name,
                sum(anovaData[,factorCol] == name)),
        br()
      )
      
      hypothesis <- tagAppendChildren(hypothesis, factorCount)
    }
    
    return(hypothesis)
  }
  
  PrintANOVAFormula <- function() {
    tagList(
      br(),
      p(tags$b("Anova Table:")),
      DTOutput("oneWayAnovaTable", width = '750px'),
      br(),
      br(),
      p(tags$b("Test Statistic:")),
      sprintf("\\( F = \\dfrac{MSB}{MSE} = \\dfrac{%0.4f}{%0.4f} = %0.4f \\)",
              anovaOneWayResults()$test[1,"Mean Sq"],
              anovaOneWayResults()$test[2,"Mean Sq"],
              anovaOneWayResults()$test[1,"F value"]),
      br(),
      br()
    )
  } 
  
  PrintANOVATable <- function() {
    data <- anovaOneWayResults()$test
    data <- rbind(data, c(sum(data[,"Df"]), sum(data[,"Sum Sq"]), NA, NA, NA))
    # print(data[,"Df"])
    rownames(data)[2] <- "Error"
    rownames(data)[3] <- "Total"
    colnames(data) <- c("df", "Sum of Squares (SS)", "Mean Sum of Squares (MS)", "F-ratio", "P-Value")
    
    datatable(data[,0:4],
              class = 'cell-border stripe',
              options = list(
                dom = 't',
                pageLength = -1,
                ordering = FALSE,
                searching = FALSE,
                paging = FALSE,
                autoWidth = FALSE,
                scrollX = TRUE,
                columnDefs = list(list(className = 'dt-center',
                                       targets = 0:4),
                                  list(width = '175px', 
                                       targets = 2:4))
              ),
              selection = "none",
              escape = FALSE,
              filter = "none"
    ) %>% formatRound(columns = 1,
                      digits = 0
    ) %>% formatRound(columns = 2:4,
                      digits = 4
    ) %>% formatStyle(columns = c(0),
                      fontWeight = 'bold')
  }
  
  
  #### Chi-Square Functions ----
  
  
  
  CreateChiSqObserved <- function(chiSqData) {
    
    observedTable <- datatable(chiSqData,
      class = 'cell-border stripe',
      options = list(
        dom = 't',
        pageLength = -1,
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE,
        autoWidth = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(width = '100px', 
                               targets = 0:ncol(chiSqData)),
                          list(className = 'dt-center', 
                               targets = 0:ncol(chiSqData)))
        ),
        selection = "none",
        escape = FALSE,
        filter = "none"
    )
    
    observedTable <- FormatChiSqTable(observedTable, ncol(chiSqData), nrow(chiSqData))
    
    return(observedTable)
  }
  
  CreateChiSqExpected <- function(chiSqData) {
    totaledData <- getTotaledMatrix(round(chiSqData, 4), chiSqActiveData()$data)
    
    expectedTable <- datatable(totaledData,
                               class = 'cell-border stripe',
                               options = list(
                                 dom = 't',
                                 pageLength = -1,
                                 ordering = FALSE,
                                 searching = FALSE,
                                 paging = FALSE,
                                 autoWidth = FALSE,
                                 scrollX = TRUE,
                                 columnDefs = list(list(width = '100px', 
                                                        targets = 0:ncol(totaledData)),
                                                   list(className = 'dt-center', 
                                                        targets = 0:ncol(totaledData)))
                               ),
                               selection = "none",
                               escape = FALSE,
                               filter = "none"
    )
    
    expectedTable <- FormatChiSqTable(expectedTable, ncol(totaledData), nrow(totaledData))
    
    return(expectedTable)
  }
  
  FormatChiSqTable <- function(chiSqTable, numCol, numRow) {
    
    chiSqTable %>%
      formatStyle(columns = c(0,numCol),
                  fontWeight = 'bold') %>%
      formatStyle(columns = 1:numCol,
                  target = 'row',
                  fontWeight = styleRow(dim(chiSqTotaled())[1], "bold")) %>%
      formatStyle(columns = c(0,numCol - 1),
                  borderRight = styleRow(c(1:(numRow - 1)),'2px solid #787878')) %>%
      formatStyle(columns = c(1:(numCol - 1)),
                  borderTop = styleRow(c(1),'2px solid #787878'),
                  borderBottom = styleRow(c(numRow - 1),'2px solid #787878'))
  }
  
  PrintChiSqTest <- function() {
    data <- chiSqResults()
    chiSqStat <- data$Results$statistic
    
    if(input$chisquareSigLvl == "10%") {
      sigLvl <- 0.1 
    } else if(input$chisquareSigLvl == "5%") {
      sigLvl <- 0.05
    } else {
      sigLvl <- 0.01
    }
    
    critVal <- round(qchisq(1 - sigLvl, df = data$Results$parameter), 4)
    
    if(data$Results$p.value < sigLvl) {
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
      sprintf("\\( \\alpha = %s \\)",
              sigLvl),
      br(),
      br(),
      br()
    )
    
    if(input$chisquareDimension == '2 x 2' && input$chiSquareYates) {
      #Yates correction is only applied when O - E is > 0.5
      chiSqFormula <- PrintChiSqYatesFormula(chiSqStat)
    } else {
      chiSqFormula <- PrintChiSqFormula(chiSqStat)
    }
    
    chiSqPVal <- PrintChiSqPVal(data$Results$p.value, chiSqStat, pValSymbol, sigLvl, reject)
    chiSqCV <- PrintChiSqCV(critVal, reject, region, alpha = sigLvl, df = data$Results$parameter)
    chiSqConclusion <- PrintChiSqConclusion(sigLvl, suffEvidence)
    
    tagAppendChildren(chiSqOutput, chiSqFormula, chiSqPVal, chiSqCV, chiSqConclusion)
  }
  
  
  PrintChiSqFormula <- function(chiSqStat) {
    data <- chiSqResults()$Matrix
    
    chiSqSum <- ""
    chiSqSmplf <- ""
    
    for(row in 1:(nrow(data) - 2)) {
      chiSqSum <- paste0(chiSqSum, "\\dfrac{(", data[row,"O"], " - ", data[row,"E"], ")^2}{", data[row,"E"], "} + ")
      chiSqSmplf <- paste0(chiSqSmplf, data[row,"(O - E)<sup>2</sup> / E"]," + ")
    }
    
    chiSqSum <- paste0(chiSqSum, "\\dfrac{(", data[nrow(data) - 1,"O"], " - ", data[nrow(data) - 1,"E"], ")^2}{", data[ncol(data) - 1,"E"], "}")
    chiSqSmplf <- paste0(chiSqSmplf, data[nrow(data) - 1,"(O - E)<sup>2</sup> / E"])
    
    formula <- tagList(
      p(tags$b("Test Statistic:")),
      sprintf("\\( \\chi^2 = \\large{ \\sum{ \\dfrac{(O - E)^2}{E} } } \\)"),
      br(),
      br(),
      sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
              chiSqSum),
      br(),
      br(),
      br(),
      sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
              chiSqSmplf),
      br(),
      br(),
      br(),
      sprintf("\\( \\phantom{\\chi^2} = %0.4f \\)",
              chiSqStat),
      br(),
      br(),
      br()
    )
    
    return(formula)
  }
  
  
  PrintChiSqYatesFormula <- function(chiSqStat) {
    data <- chiSqResults()$Matrix
    yates <- data[,"(O - E)"]
    yates <- round((abs(yates) - 0.5)^2 / data[,"E"], 4)

    if(all(abs(data[nrow(data) - 1,"(O - E)"]) > 0.5)) {
      
      chiSqSum <- ""
      chiSqSmplf <- ""
      
      for(row in 1:(nrow(data) - 2)) {
        chiSqSum <- paste0(chiSqSum, "\\dfrac{(|", data[row,"O"], " - ", data[row,"E"], "| - 0.5)^2}{", data[row,"E"], "} + ")
        chiSqSmplf <- paste0(chiSqSmplf, yates[row]," + ")
      }
      
      chiSqSum <- paste0(chiSqSum, "\\dfrac{(|", data[nrow(data) - 1,"O"], " - ", data[nrow(data) - 1,"E"], "| - 0.5)^2}{", data[ncol(data) - 1,"E"], "}")
      chiSqSmplf <- paste0(chiSqSmplf, yates[nrow(data) - 1])
      
      formula <- tagList(
        p(tags$b("Test Statistic:")),
        sprintf("\\( \\chi^2_{Yates} = \\large{ \\sum{ \\dfrac{(|O - E| - 0.5)^2}{E} } } \\)"),
        br(),
        br(),
        sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
                chiSqSum),
        br(),
        br(),
        br(),
        sprintf("\\( \\phantom{\\chi^2} =  %s \\)",
                chiSqSmplf),
        br(),
        br(),
        br(),
        sprintf("\\( \\phantom{\\chi^2} = %0.4f \\)",
                chiSqStat),
        br(),
        br(),
        br()
      )
    } else {
      disclaimer <- tagList(
        p(tags$i("*Note: Yates’ continuity correction is not applied in this 
                 case because the correction factor is greater than |O - E| for 
                 one or more of the differences.*")
          ),
        br(),
        br()
      )
      formula <- tagAppendChildren(PrintChiSqFormula(chiSqStat), disclaimer)
    }

    return(formula)
  }
  
  
  PrintChiSqPVal <- function(pValue, tsValue, pValSymbol, sigLvl, reject) {
    
    pvalCalc <- paste("P(\\, \\chi^2 \\, \\ge \\,", round(tsValue,4), ")")
    
    if(pValue < 0.0001 && pValue > 0) {
      pValue <- "P < 0.0001"
    } else {
      pValue <- paste(round(pValue, 4))
    }
    
    pValOutput <- tagList(
      p(tags$b("Using P-Value Method:")),
      sprintf("\\( P = %s = %s\\)",
              pvalCalc,
              pValue),
      br(),
      br(),
      sprintf("Since \\( P %s %0.2f \\), %s \\( H_{0}\\).",
              pValSymbol,
              sigLvl,
              reject),
      br(),
      br(),
      br()
    )
    
    return(pValOutput)
  }
  
  PrintChiSqCV <- function(critVal, reject, region, alpha, df) {
    
    cvOutput <- tagList(
      p(tags$b("Using Critical Value Method:")),
      sprintf("Critical Value \\( = \\chi^2_{\\alpha,df} = \\chi^2_{%s,%s} = %s \\)",
              alpha,
              df,
              critVal),
      br(),
      br(),
      sprintf("Since the test statistic \\( (\\chi^2)\\) falls within the %s region, %s \\( H_{0}\\).",
              region,
              reject),
      br(),
      br(),
      br(),
      plotOutput("chiSqPlot", width = "50%", height = "400px"),
      br(),
      br()
    )
  }
  
  PrintChiSqConclusion <- function(sigLvl, suffEvidence) {
    
    conclusion <- tagList(
      p(tags$b("Conclusion:")),
      p(
        sprintf("At the %1.0f%% significance level, there %s sufficient 
                evidence to reject the null hypothesis \\( (H_{0}) \\) that the 
                Row variable and Column variable are not associated.",
                sigLvl*100,
                suffEvidence),
        br(),
      )
    )
    
    return(conclusion)
  }
  
  PrintFishersTest <- function() {
    results <- fishersResults()
    
    if(input$chisquareSigLvl == "10%") {
      sigLvl <- 0.1 
    } else if(input$chisquareSigLvl == "5%") {
      sigLvl <- 0.05
    } else {
      sigLvl <- 0.01
    }
    
    if(results$p.value > sigLvl) {
      pValSymbol <- "\\gt"
      suffEvidence <- "isn't"
      reject <- "do not reject"
      pValue <- paste(round(results$p.value, 4))
    } else {
      pValSymbol <- "\\leq"
      suffEvidence <- "is"
      reject <- "reject"
      
      if(results$p.value < 0.0001 && results$p.value > 0) {
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
      sprintf("\\( \\alpha = %s \\)",
              sigLvl),
      br(),
      br()
    )
    
    fishersPVal <- PrintFishersPVal(pValue, pValSymbol, sigLvl, reject)
    fishersConclusion <- PrintChiSqConclusion(sigLvl, suffEvidence)
    
    tagAppendChildren(fishersOutput, fishersPVal, fishersConclusion)
  }
  
  PrintFishersPVal <- function(pValue, pValSymbol, sigLvl, reject) {
    fishersData <- chiSqTotaled()
    
    if(input$chisquareDimension == '2 x 2') {
      tagList(
        p(tags$b("P-Value:")),
        sprintf("\\( p = \\dfrac{(a + b)! \\; (c + d)! \\; (a + c)! \\; (b + d)!}{a! \\; b! \\; c! \\; d! \\; n!} \\)"),
        br(),
        br(),
        sprintf("\\( \\phantom{p} = \\dfrac{(%s + %s)! \\; (%s + %s)! \\; (%s + %s)! \\; (%s + %s)!}{%s! \\; %s! \\; %s! \\; %s! \\; %s!} \\)",
                fishersData[1,1],
                fishersData[1,2],
                fishersData[2,1],
                fishersData[2,2],
                fishersData[1,1],
                fishersData[2,1],
                fishersData[1,2],
                fishersData[2,2],
                fishersData[1,1],
                fishersData[1,2],
                fishersData[2,1],
                fishersData[2,2],
                fishersData[3,3]),
        br(),
        br(),
        sprintf("\\( \\phantom{p} = %s \\)",
                pValue),
        br(),
        br(),
        sprintf("Since \\( p %s %0.2f \\), %s \\( H_{0}\\).",
                pValSymbol,
                sigLvl,
                reject),
        br(),
        br()
      )
    } else {
      tagList(
        p(tags$b("P-Value:")),
        sprintf("\\( p = %s \\)",
                pValue),
        br(),
        br(),
        sprintf("Since \\( p %s %0.2f \\), %s \\( H_{0}\\).",
                pValSymbol,
                sigLvl,
                reject),
        br(),
        br()
      )
    }
      
  }
  
  # --------------------------------------------------------------------- #
  
  
  ### Reactives ----
  # --------------------------------------------------------------------- #
  
  ConfLvl <- reactive({
    
    req(input$siMethod != 'n')
    
    if(input$siMethod == '1') {
      
      if(input$confidenceLevel == '90%') {
        confLvl <- 0.9
      } else if(input$confidenceLevel == '95%') {
        confLvl <- 0.95
      } else {
        confLvl <- 0.99
      }
      
    } else if(input$siMethod == '2') {
      
      if(input$confidenceLevel2 == '90%') {
        confLvl <- 0.9
      } else if(input$confidenceLevel2 == '95%') {
        confLvl <- 0.95
      } else {
        confLvl <- 0.99
      }
    } else {
      confLvl <- 0
    }
    
    return(confLvl)
  })
  
  
  SigLvl <- reactive({
    
    if(input$siMethod == '1') {
      
      if(input$significanceLevel == "10%") {
        sigLvl <- 0.1 
      } else if(input$significanceLevel == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
    } else if (input$siMethod == '2') {
      
      if(input$significanceLevel2 == "10%") {
        sigLvl <- 0.1 
      } else if(input$significanceLevel2 == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }
      
    } else {
      sigLvl <- 0.05
    }
    
    return(sigLvl)
  })
  
  #### One Mean reactives ----
  
  OneMeanUploadData <- eventReactive(input$oneMeanUserData, {
    
    ext <- tools::file_ext(input$oneMeanUserData$name)
    ext <- tolower(ext)
    
    switch(ext, 
           csv = read_csv(input$oneMeanUserData$datapath, show_col_types = FALSE),
           xls = read_xls(input$oneMeanUserData$datapath),
           xlsx = read_xlsx(input$oneMeanUserData$datapath),
           txt = read_tsv(input$oneMeanUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format.")
    )
  })
  
  OneMeanUploadStatus <- reactive({
    if (is.null(fileInputs$oneMeanStatus)) {
      return(NULL)
    } else if (fileInputs$oneMeanStatus == 'uploaded') {
      return(input$file1)
    } else if (fileInputs$oneMeanStatus == 'reset') {
      return(NULL)
    }
  })
  
  OneMeanTotaledData <- reactive({
    req(si_iv$is_valid())
    
    if(input$dataAvailability == 'Enter Raw Data'){
      dat <- createNumLst(input$sample1)
      
    } else if (input$dataAvailability == 'Upload Data'){
      dat <- unlist(OneMeanUploadData()[,input$oneMeanVariable])
    } else{
      dat <- 0
    }
    
    totaled <- list(sum(dat), sum(dat^2))
    return(totaled)
  })
  
  OneMeanHypInfo <- reactive({
    hypTestSymbols <- list()
    
    if(input$altHypothesis == "3"){
      hypTestSymbols$alternative <- "greater"
      hypTestSymbols$nullHyp <- "\\mu \\leq"
      hypTestSymbols$altHyp <- "\\mu \\gt"
      hypTestSymbols$critAlph <- "\\alpha"
      hypTestSymbols$critSign <- ""
      hypTestSymbols$alphaVal <- SigLvl()
    }
    else if(input$altHypothesis == "2"){
      hypTestSymbols$alternative <- "two.sided"
      hypTestSymbols$nullHyp <- "\\mu ="
      hypTestSymbols$altHyp <- "\\mu \\neq"
      hypTestSymbols$critAlph <- "\\alpha/2"
      hypTestSymbols$critSign <- "\\pm"
      hypTestSymbols$alphaVal <- SigLvl()/2
    }
    else{
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
    req(si_iv$is_valid())
    
    if(input$dataAvailability == 'Summarized Data') {
      sigmaKnown <- input$sigmaKnown
    } else if(input$dataAvailability == 'Enter Raw Data') {
      if(input$sigmaKnownRaw == 'rawKnown') {
        sigmaKnown <- "Known"
      } else {
        sigmaKnown <- "Unknown"
      }
    } else if(input$dataAvailability == 'Upload Data') {
      sigmaKnown <- input$sigmaKnownUpload
    }
    
    return(sigmaKnown)
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
      dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
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
  
  
  GetOneMeanCI <- reactive({
    
    if(OneMeanSigma() == "Known"){
      
      if(input$dataAvailability == 'Summarized Data'){
        oneMeanCI <- OneMeanZIntSumm()
      } else {
        oneMeanCI <- OneMeanZIntRaw()
      }
      
    } else {
      
      if(input$dataAvailability == 'Summarized Data'){
        oneMeanCI <- OneMeanTIntSumm()
      } else {
        oneMeanCI <- OneMeanTIntRaw()
      }
      
    }
    
    return(oneMeanCI)
  })
  
  GetOneMeanHT <- reactive({
    
    if(OneMeanSigma() == "Known"){
      
      if(input$dataAvailability == 'Summarized Data'){
        oneMeanHT <- OneMeanZTestSumm()
      } else {
        oneMeanHT <- OneMeanZTestRaw()
      }
      
    } else {
      
      if(input$dataAvailability == 'Summarized Data'){
        oneMeanHT <- OneMeanTTestSumm()
      } else {
        oneMeanHT <- OneMeanTTestRaw()
      }
      
    }
    
    return(oneMeanHT)
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
    ext <- tolower(ext)
    
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
    
    sample1 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample1]))
    sample2 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample2]))

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
      hypTestSymbols$nullHyp <- "\\mu_{1} \\leq"
      hypTestSymbols$altHyp <- "\\mu_{1} \\gt"
      hypTestSymbols$critAlph <- "\\alpha"
      hypTestSymbols$critSign <- ""
      hypTestSymbols$alphaVal <- SigLvl()
    }
    else if(input$altHypothesis2 == "2"){
      hypTestSymbols$alternative <- "two.sided"
      hypTestSymbols$nullHyp <- "\\mu_{1} ="
      hypTestSymbols$altHyp <- "\\mu_{1} \\neq"
      hypTestSymbols$critAlph <- "\\alpha/2"
      hypTestSymbols$critSign <- "\\pm"
      hypTestSymbols$alphaVal <- SigLvl()/2
    }
    else{
      hypTestSymbols$alternative <- "less"
      hypTestSymbols$nullHyp <- "\\mu_{1} \\geq"
      hypTestSymbols$altHyp <- "\\mu_{1} \\lt"
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
  
  DepMeansUploadData <- eventReactive(input$depMeansUserData, {
    
    ext <- tools::file_ext(input$depMeansUserData$name)
    ext <- tolower(ext)
    
    switch(ext, 
           csv = read_csv(input$depMeansUserData$datapath, show_col_types = FALSE),
           xls = read_xls(input$depMeansUserData$datapath),
           xlsx = read_xlsx(input$depMeansUserData$datapath),
           txt = read_tsv(input$depMeansUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format")
    )
  })
  
  CheckDepUploadSamples <- eventReactive (c(input$depMeansUplSample1, 
                                            input$depMeansUplSample2), {
                                              
    if(input$depMeansUplSample1 == "" | input$depMeansUplSample2 == "") {
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
    
    return(depMeansTInt)
  })
  
  
  DepMeansTTest <- reactive({
    req(si_iv$is_valid() && depmeansrawsd_iv$is_valid())
    
    data <- GetDepMeansData()

    depMeansTTest <- TTest(data$n, data$dbar, data$sd, 0, IndMeansHypInfo()$alternative, SigLvl())
    
    return(depMeansTTest)

  })
  
  #### Two Prop Reactives ----
  checkTwoProp <- reactive({

    if(is.na(input$numSuccesses1) || is.na(input$numSuccesses2)) {
      return(-1)
    } else {
      return(input$numSuccesses1 + input$numSuccesses2)
    } 

  })
  
  
  #### ANOVA Reactives ----
  anovaUploadData <- eventReactive(input$anovaUserData, {
    ext <- tools::file_ext(input$anovaUserData$name)
    ext <- tolower(ext)
    
    switch(ext, 
           csv = read_csv(input$anovaUserData$datapath, show_col_types = FALSE),
           xls = read_xls(input$anovaUserData$datapath),
           xlsx = read_xlsx(input$anovaUserData$datapath),
           txt = read_tsv(input$anovaUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format.")
    )
  })
  
  anovaStackedIsValid <- eventReactive({input$anovaResponse
                                        input$anovaFactors}, {
    valid <- TRUE
    
    if(!is.null(input$anovaResponse) && !is.null(input$anovaFactors)) {
      if(input$anovaResponse == input$anovaFactors) {
        valid <- FALSE
      }
    }

    return(valid)    
  })
  
  anovaOneWayResults <- reactive({
    req(si_iv$is_valid)
    
    results <- list()
    
    if(input$anovaFormat == "Multiple") {
      anovaData <- stack(anovaUploadData()[,input$anovaMultiColumns])
      factorCol <- "ind"
      anovaFormula <- values ~ ind
      factorNames <- levels(anovaData[,factorCol])
      
    } else {
      anovaData <- anovaUploadData()
      factorCol <- input$anovaFactors
      anovaFormula <- reformulate(factorCol, input$anovaResponse)
      names <- distinct(anovaUploadData()[,factorCol])
      factorNames <- c()
      for(row in 1:nrow(names)) {
        factorNames[row] <- names[row,1]
      }
    }
    totalCount <- nrow(anovaData)
    numFactors <- length(factorNames)
    anovaTest <- aov(formula = anovaFormula, data = anovaData)
    
    results$data <- anovaData
    results$count <- totalCount
    results$factorCol <- factorCol
    results$numFactors <- numFactors
    results$factorNames <- factorNames
    results$test <- anova(anovaTest)
    
    return(results)
  })
  
  #### Chi-Square Reactives ----
  # chiSqData2x2 <- reactive({
  #   suppressWarnings(as.numeric(input$chiSqInput2x2))
  # })
  # 
  # chiSqData2x3 <- reactive({
  #   suppressWarnings(as.numeric(input$chiSqInput2x3))
  # })
  # 
  # chiSqData3x2 <- reactive({
  #   suppressWarnings(as.numeric(input$chiSqInput3x2))
  # })
  # 
  # chiSqData3x3 <- reactive({
  #   suppressWarnings(as.numeric(input$chiSqInput3x3))
  # })
  
  chiSqActiveData <- reactive({
    if(input$chisquareDimension == "2 x 2") {
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
    if(!any(is.na(chiSqActiveData()$numeric))){
      
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
  
  
  #### Samp Size Est Reactives ----
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
      
      if(is.null(input$oneMeanUserData)) {
        validate("Please upload a file.")
      }
      
      validate(
        need(!is.null(fileInputs$oneMeanStatus) && fileInputs$oneMeanStatus == 'uploaded', "Please upload a file."),
        
        errorClass = "myClass"
      )
      
      validate(
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
        need(input$hypMean, "Hypothesized value of the Population Mean is required."),
        
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
    } else if(input$siMethod == '1' && input$popuParameter == 'Population Proportion') {
      req(input$numSuccesses && input$numTrials)
      validate(
        need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),

        errorClass = "myClass"
      )

    }
     
    if(!onepropht_iv$is_valid()) {
      validate(
        need(input$hypProportion, "Hypothesized value of the Population Proportion must be between 0 and 1") %then%
          need(input$hypProportion > 0 && input$hypProportion < 1, "Hypothesized value of the Population Proportion must be between 0 and 1"),
          
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
    
    if(!indmeansrawsdunk_iv$is_valid()) {
      validate(
        need(sd(createNumLst(input$raw_sample1)) != 0 && sd(createNumLst(input$raw_sample2)) != 0, "The test statistic (t) will be undefined when the sample standard deviation of Sample 1 and Sample 2 are both 0."),
        
        errorClass = "myClass"
      )
    }
    
    if(!indmeansupload_iv$is_valid()) {
      
      if(is.null(input$indMeansUserData)) {
        validate("Please upload a file.")
      }
      
      validate(
        need(!is.null(fileInputs$indMeansStatus) && fileInputs$indMeansStatus == 'uploaded', "Please upload a file."),
        
        errorClass = "myClass"
      )

      validate(
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
      
      if(is.null(input$depMeansUserData)) {
        validate("Please upload a file.")
      }
      
      validate(
        need(!is.null(fileInputs$depMeansStatus) && fileInputs$depMeansStatus == 'uploaded', "Please upload a file."),
        
        errorClass = "myClass"
      )
      
      validate(
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
        need(CheckDepUploadSamples() == 0, "Same number of data points required for 'Before' and 'After' sample data."),
        
        errorClass = "myClass"
      )
    }
    
    if(!depmeansrawsd_iv$is_valid()) {
      
      if(input$inferenceType2 == 'Hypothesis Testing'){
        sdValidation <- "The test statistic (t) will be undefined for sample data with a sample standard deviation of difference (sd) = 0."
      } else {
        sdValidation <- paste0("The confidence interval results in (",
                               GetDepMeansData()$dbar,
                               ",", GetDepMeansData()$dbar,
                               ") when the sample standard deviation of difference (sd) = 0.")
      }
      validate(
        need(GetDepMeansData()$sd != 0, sdValidation),
        
        errorClass = "myClass"
      )
    }
    
  # Two Population Proportion Validation 
  # ------------------------------------------------------------------------ #
    
    if(!twopropht_iv$is_valid()) {
      validate(
        need(checkTwoProp() > 0, "The test statistic (t) will be undefined when the Number of Successes 1 (x1) and Number of Successes 2 (x2) are both 0."),
        
        errorClass = "myClass"
      )
    }

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
        need(input$numSuccesses2 %% 1 == 0, "Number of Successes 2 (x2) must be an integer"),
        need(input$numSuccesses2 >= 0, "Number of Successes 2 (x2) cannot be negative"),
        need(input$numTrials2 %% 1 == 0, "Number of Trials 2 (n2) must be an integer"),
        need(input$numTrials2 > 0, "Number of Trials 2 (n2) must be greater than 0"),
        
        errorClass = "myClass"
      )

    } else if (input$siMethod == '2' && input$popuParameters == 'Population Proportions') {

      validate(
        need(input$numSuccesses1 <= input$numTrials1, "Number of Successes 1 (x1) cannot be greater than Number of Trials 1 (n1)"),
        need(input$numSuccesses2 <= input$numTrials2, "Number of Successes 2 (x2) cannot be greater than Number of Trials 2 (n2)"),
        
        errorClass = "myClass"
      )

    }
    
    
    # ANOVA Validation
    # ------------------------------------------------------------------------ #
    if(!anovaupload_iv$is_valid()) {
      if(is.null(input$anovaUserData)) {
        validate("Please upload a file.")
      }
      
      validate(
        need(!is.null(fileInputs$anovaStatus) && fileInputs$anovaStatus == 'uploaded', "Please upload a file."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(nrow(anovaUploadData()) > 0, "File is empty."),
        need(ncol(anovaUploadData()) >= 2, "File must contain at least 2 distinct columns of data to choose from for analysis."),
        
        errorClass = "myClass"
      )
    }
    
    if(!anovamulti_iv$is_valid()) {
      validate(
        need(length(input$anovaMultiColumns) >= 2, "Please select two or more columns to conduct analysis."),
        
        errorClass = "myClass"
      )
    }
    
    if(!anovastacked_iv$is_valid()) {
      validate(
        need(!is.null(input$anovaResponse) && input$anovaResponse != '', "Please select a Response Variable."),
        need(!is.null(input$anovaFactors) && input$anovaFactors != '', "Please select a Factors column."),

        errorClass = "myClass"
      )
      
      validate(
        need(anovaStackedIsValid() == TRUE, "Please select distinct columns for Response Variable and Factors."),
        
        errorClass = "myClass"
      )
    }
    
    
    # Chi-Square Validation 
    # ------------------------------------------------------------------------ #
    
    if(!chiSq2x2_iv$is_valid()) {
      validate(
        need(input$chiSqInput2x2, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
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
        need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        
        errorClass = "myClass"
      )
    }
    
    if(!chiSq2x3_iv$is_valid()) {
      validate(
        need(input$chiSqInput2x3, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
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
        need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        
        errorClass = "myClass"
      )
    }
    
    if(!chiSq3x2_iv$is_valid()){
      validate(
        need(input$chiSqInput3x2, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
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
        need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        
        errorClass = "myClass"
      )
    }
    
    if(!chiSq3x3_iv$is_valid()){
      validate(
        need(input$chiSqInput3x3, "Fields must be positive integers."),
        
        errorClass = "myClass"
      )
      
      validate(
        need(all(!is.na(chiSqActiveData()$numeric)), "Fields must be positive integers.") %then%
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
        need(all(chiSqTotaled()[,"Total"] > 0) && all(chiSqTotaled()["Total",] > 0), "The test statistic will be undefined if any row or column total equals 0."),
        
        errorClass = "myClass"
      )
    }
  })
 
  
  #### One Mean outputs ----
  
  
  ##### CI ----
  output$oneMeanCI <- renderUI({

    printOneMeanCI()
    
  })
  
  
  ##### HT ----
  output$oneMeanHT <- renderUI({
    
    printOneMeanHT()
    
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
    htPlotCritVal <- oneMeanData[4]
    
    if(sigmaKnown== 'Known') {
      oneMeanPlot <- hypZTestPlot(oneMeanData[6], htPlotCritVal, intrpInfo$alternative)
      
    } else {
      oneMeanPlot <- hypTTestPlot(oneMeanData[6], oneMeanData[8], htPlotCritVal, intrpInfo$alternative)
    }

    oneMeanPlot
  })
  
  
  output$siOneMeanBoxplot <- renderPlot({
    
    if(input$dataAvailability == 'Enter Raw Data') {
      dat <- createNumLst(input$sample1)
    } else if(input$dataAvailability == 'Upload Data') {
      dat <- na.omit(unlist(OneMeanUploadData()[,input$oneMeanVariable]))
    }
    
    quartile1 <-  fivenum(dat)[2]
    quartile3 <-  fivenum(dat)[4]
    sampIQR <- round(quartile3 - quartile1, 4)
    lowerFence <- round(quartile1 - (1.5*sampIQR), 4)
    upperFence <- round(quartile3 + (1.5*sampIQR), 4)
    numOutliers <- sum(dat < lowerFence) + sum(dat > upperFence)
    
    if(numOutliers == 0) {
      outliers <- "There are no outliers."
      df_outliers <- data.frame()
    } else {
      outliers <- GetOutliers(dat, lowerFence, upperFence)
      df_outliers <- as.data.frame(outliers)
    }
    
    
    df_boxplot <- data.frame(x = dat)

    bp <- RenderBoxplot(dat,
                      df_boxplot,
                      df_outliers,
                      input$oneMeanBoxplotColour,
                      input$oneMeanBoxplotTitle,
                      input$oneMeanBoxplotXlab,
                      input$oneMeanBoxplotYLab)
    
    if(input$oneMeanBoxplotFlip == 1){
      bp + coord_flip() +
           theme(axis.text.x.bottom = element_blank(),
                 axis.text.y.left = element_text(size = 16))
    } else {
      bp
    }
  })
  
  
  #### One Prop outputs ----
  
  
  ##### CI ----
  output$onePropCI <- renderUI({
    req(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
    
    onePropData <- OnePropZInterval(input$numSuccesses, input$numTrials, ConfLvl())

    p(
      withMathJax(
        sprintf("Given:"),
        br(),
        sprintf("\\( n = %s \\)",
                onePropData["n"]),
        br(),
        sprintf("\\( x = %s \\)",
                onePropData["x"]),
        br(),
        br(),
        br(),
        sprintf("For a \\( %s \\)%% Confidence Interval: ",
                ConfLvl()*100),
        br(),
        sprintf("\\( \\alpha = 1 - %s = %s \\)",
                ConfLvl(),
                1 - ConfLvl()),
        br(),
        sprintf("\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
                1 - ConfLvl(),
                (1 - ConfLvl()) / 2,
                onePropData["Z Critical"]),
        br(),
        br(),
        br(),
        sprintf("\\( \\displaystyle CI = \\hat{p} \\pm \\left( z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} \\right) \\)"),
        br(),
        p("where"),
        sprintf("\\( \\qquad \\hat{p} = \\dfrac{x}{n} = \\dfrac{%s}{%s} = %0.4f \\)",
                onePropData["x"],
                onePropData["n"],
                onePropData["phat"]),
        br(),
        br(),
        br(),
        sprintf("\\( \\displaystyle CI = %0.4f \\pm \\left( %s \\sqrt{\\dfrac{%0.4f(1 - %0.4f)}{%s}} \\right) \\)",
                onePropData["phat"],
                onePropData["Z Critical"],
                onePropData["phat"],
                onePropData["phat"],
                onePropData["n"]),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = %0.4f \\pm \\left( %g \\cdot %0.4f \\right) \\)",
                onePropData["phat"],
                onePropData["Z Critical"],
                onePropData['Std Error']),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = %0.4f \\pm %0.4f \\)",
                onePropData["phat"],
                onePropData['ME']),
        br(),
        br(),
        sprintf("\\( \\displaystyle \\phantom{CI} = (%0.4f, %0.4f)\\)",
                onePropData["LCL"],
                onePropData["UCL"]),
        br(),
        br(),
        br(),
        p(tags$b("Interpretation:")),
        sprintf("We are %1.0f%% confident that the population proportion \\( (p) \\) is between \\( %0.4f \\) and \\( %0.4f \\).",
                ConfLvl()*100,
                onePropData["LCL"],
                onePropData["UCL"])
      )
    )
  })
  
  
  ##### HT ----
  output$onePropHT <- renderUI({
    req(si_iv$is_valid() && input$numTrials >= input$numSuccesses)
    
    onePropData <- OnePropZTest(input$numSuccesses, input$numTrials, input$hypProportion, OneMeanHypInfo()$alternative, SigLvl())
    
    if(input$altHypothesis == "2") { #two sided test
      critZVal <- paste("\\pm", onePropData["Z Critical"])
      nullHyp <- "p ="
      altHyp <- "p \\neq"
    } else {
      critZVal <- paste(onePropData["Z Critical"])
      
      if(input$altHypothesis == "1"){
        nullHyp <- "p \\geq"
        altHyp <- "p \\lt"
      } else {
        nullHyp <- "p \\leq"
        altHyp <- "p \\gt"
      }
    }
    
    if(onePropData["P-Value"] < 0.0001) {
      pValue <- "P \\lt 0.0001"
    } else {
      pValue <- paste("P = ", onePropData["P-Value"])
    }
    
    if(onePropData["P-Value"] > SigLvl()) {
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
        sprintf("\\( H_{0}: %s %g\\)",
                nullHyp,
                input$hypProportion),
        br(),
        sprintf("\\( H_{a}: %s %g\\)",
                altHyp,
                input$hypProportion),
        br(),
        br(),
        sprintf("\\( \\alpha = %g \\)",
                SigLvl()),
        br(),
        br(),
        br(),
        p(tags$b("Test Statistic:")),
        sprintf("Given:"),
        br(),
        sprintf("\\( n = %s \\)",
                onePropData["n"]),
        br(),
        sprintf("\\( x = %s \\)",
                onePropData["x"]),
        br(),
        br(),
        br(),
        sprintf("\\(z = \\dfrac{\\hat{p} - p_{0}}{ \\sqrt{ \\dfrac{p_{0}(1 - p_{0})}{n} } }\\)"),
        br(),
        p("where"),
        sprintf("\\( \\qquad \\hat{p} = \\dfrac{x}{n} = \\dfrac{%s}{%s} = %0.4f \\)",
                onePropData["x"],
                onePropData["n"],
                onePropData["phat"]),
        br(),
        br(),
        br(),
        sprintf("\\(z = \\dfrac{%0.4f - %0.4f}{ \\sqrt{ \\dfrac{%0.4f(1 - %0.4f)}{%1.0f} } }\\)",
                onePropData["phat"],
                input$hypProportion,
                input$hypProportion,
                input$hypProportion,
                input$numTrials),
        sprintf("\\( = \\dfrac{%0.4f}{%0.4f} \\)",
                onePropData["phat"] - input$hypProportion,
                onePropData["Std Error"]),
        br(),
        br(),
        sprintf("\\(\\phantom{z} = %0.4f\\)",
                onePropData["Test Statistic"]),
        br(),
        br(),
        br()
      )
    )
      
    onePropPVal <- printHTPVal(onePropData["P-Value"], "z", 
                               OneMeanHypInfo()$alternative, 
                               onePropData["Test Statistic"], 
                               pvalSymbol, 
                               reject)
        # p(tags$b("Using P-Value Method:")),
        # sprintf("\\( %s \\)",
        #         pValue),
        # br(),
        # sprintf("Since \\( P\\) %s %0.2f, %s \\( H_{0}\\).",
        #         pvalSymbol,
        #         SigLvl(),
        #         reject),
        # br(),
        # br(),
        # br(),
      
    onePropHTTail <- tagList(
      withMathJax(
        p(tags$b("Using Critical Value Method:")),
        sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$critAlph,
                OneMeanHypInfo()$critSign,
                OneMeanHypInfo()$alphaVal,
                critZVal),
        br(),
        br(),
        sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                region,
                reject),
        br(),
        br(),
        plotOutput('onePropHTPlot', width = "75%", height = "300px"),
        br()
      )
    )
    
    onePropHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, input$hypProportion)
    
    tagAppendChildren(onePropHTHead, onePropPVal, onePropHTTail, onePropHTConclusion)
  })
  
  
  ##### HT Plot ----
  output$onePropHTPlot <- renderPlot({
    
    oneSampPropZTest <- OnePropZTest(input$numSuccesses, input$numTrials, input$hypProportion, OneMeanHypInfo()$alternative, SigLvl())
    htPlotCritVal <- oneSampPropZTest["Z Critical"]
    
    htPlot <- hypZTestPlot(oneSampPropZTest["Test Statistic"], htPlotCritVal, OneMeanHypInfo()$alternative)
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
  
  
  output$siIndMeansBoxplot <- renderPlot({
    
    if(input$dataAvailability2 == 'Enter Raw Data') {
      sample1 <- createNumLst(input$raw_sample1)
      sample2 <- createNumLst(input$raw_sample2)
    } else if(input$dataAvailability2 == 'Upload Data') {
      sample1 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample1]))
      sample2 <- na.omit(unlist(IndMeansUploadData()[,input$indMeansUplSample2]))
    }
    
    dat <- c(sample1, sample2)
    df_boxplot <- data.frame(sample = c(rep("Sample 1",length(sample1)), rep("Sample 2",length(sample2))),
                             data = c(dat))
    df_outliers <- data.frame()
    
    # quartile1 <-  fivenum(dat)[2]
    # quartile3 <-  fivenum(dat)[4]
    # sampIQR <- round(quartile3 - quartile1, 4)
    # lowerFence <- round(quartile1 - (1.5*sampIQR), 4)
    # upperFence <- round(quartile3 + (1.5*sampIQR), 4)
    # numOutliers <- sum(dat < lowerFence) + sum(dat > upperFence)
    # 
    # if(numOutliers == 0) {
    #   outliers <- "There are no outliers."
    #   df_outliers <- data.frame()
    # } else {
    #   outliers <- GetOutliers(dat, lowerFence, upperFence)
    #   df_outliers <- as.data.frame(outliers)
    # }
    
    bp <- RenderSideBySideBoxplot(dat,
                                  df_boxplot,
                                  df_outliers,
                                  input$indMeansBoxplotColour,
                                  input$indMeansBoxplotTitle,
                                  input$indMeansBoxplotXlab,
                                  input$indMeansBoxplotYLab)

    if(input$indMeansBoxplotFlip == 1){
      bp + coord_flip()
    } else {
      bp
    }
    
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
          sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( z_{\\alpha/2} \\sqrt{ \\dfrac{\\sigma_{1}^2}{n_{1}} + \\dfrac{\\sigma_{2}^2}{n_{2}} } \\right) \\)"),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\quad = (%g - %g) \\pm \\left( %g \\sqrt{ \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} } \\right) \\)",
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
          sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( t_{\\alpha/2, \\, df} \\cdot s_{p} \\sqrt{ \\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}} } \\right) \\)"),
          br(),
          br(),
          p("where"),
          sprintf("\\( \\qquad df = n_{1} + n_{2} - 2 = %g, \\)",
                  tInt['df']),
          sprintf("\\( \\qquad t_{\\alpha/2, \\, df} = t_{%g, \\, %g} = %g \\)",
                  (1 - ConfLvl()) / 2,
                  tInt['df'],
                  tInt['T Critical']),
          br(),
          p("and"),
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
          sprintf("\\( \\displaystyle CI = (\\bar{x}_{1} - \\bar{x}_{2}) \\pm \\left( t_{\\alpha/2, \\, \\nu} \\cdot \\sqrt{ \\dfrac{s^2_{1}}{n_{1}} + \\dfrac{s^2_{2}}{n_{2}} } \\right) \\)"),
          br(),
          br(),
          p("where"),
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
          p("and"),
          sprintf("\\( \\qquad t_{\\alpha/2, \\, \\nu} = t_{%g, \\, %g} = %g \\)",
                  (1- ConfLvl()) / 2,
                  tInt['df'],
                  tInt['T Critical']),
          br(),
          br(),
          br(),
          br(),
          sprintf("\\( CI = (%g - %g) \\pm \\left( %g \\cdot \\sqrt{ \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} } \\right) \\)",
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
      suffEvidence <- "isn't"
      reject <- "do not reject"
      region <- "acceptance"
    }
    else
    {
      pvalSymbol <- "\\leq"
      suffEvidence <- "is"
      reject <- "reject"
      region <- "rejection"
    }
    
    if(intrpInfo$alternative == "two.sided") {
      critVal <- paste("\\pm", hTest[2])
      
    } else {
      critVal <- hTest[2]
    }
    
    indHTHead <- tagList(
      
      p(
        withMathJax(
          #h4(tags$u("Performing the Hypothesis Test:")),
          #br(),
          sprintf("\\( H_{0}: \\mu_{1} = \\mu_{2}\\)"),
          br(),
          sprintf("\\( H_{a}: %s \\mu_{2}\\)",
                  intrpInfo$altHyp),
          br(),
          br(),
          sprintf("\\( \\alpha = %s \\)",
                  SigLvl()),
          br(),
          br(),
          p(tags$b("Test Statistic:")),
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
          br()
        )
      )
    )
      
    indHTPVal <- printHTPVal(hTest["P-Value"], 
                             testStat, 
                             intrpInfo$alternative, 
                             hTest["Test Statistic"], 
                             pvalSymbol, 
                             reject)
      
      
    indHTTail <- tagList(
      withMathJax(
 
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
          p("where"),
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
          
      ),
      plotOutput('indMeansHTPlot', width = "75%", height = "300px"),
      br(),
    )

    
    indHTConclusion <- printHTConclusion(region, reject, suffEvidence, intrpInfo$altHyp, "\\mu_{2}")
    
    tagAppendChildren(indHTHead, indHTPVal, indHTTail, indHTConclusion)
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
        sprintf("\\( \\phantom{z} = \\dfrac{ (%g - %g) - 0}{ \\sqrt{ \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} } } = \\dfrac{%g}{%s} = %0.4f \\)",
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
          p("where"),
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
          sprintf("\\( \\phantom{t} = \\dfrac{ (%g - %g) - 0 }{ %g \\sqrt{ \\dfrac{1}{%g} + \\dfrac{1}{%g} } } \\)",
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
          sprintf("\\( \\phantom{t} = \\dfrac{ (%g - %g) - 0 }{ \\sqrt{ \\dfrac{%g^2}{%g} + \\dfrac{%g^2}{%g} } } = \\dfrac{%g}{%g} = %0.4f \\)",
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
    htPlotCritVal <- data[2]
    

    if(IndMeansSigmaKnown() == 'bothKnown') {
      indMeansPlot <- hypZTestPlot(data['Test Statistic'], htPlotCritVal, intrpInfo$alternative)

    } else if(IndMeansSigmaKnown() == 'bothUnknown'){
      indMeansPlot <- hypTTestPlot(data['Test Statistic'], data['df'], htPlotCritVal, intrpInfo$alternative)
    }

    indMeansPlot
  })
  
  
  #### Dep Means outputs ----
  
  ##### Data Table ----
  output$depMeansData <- renderDT({
    depData <- GetDepMeansData()

    df_depData <- data.frame(depData$before, depData$after, depData$d, depData$d^2)
    names(df_depData) <- c("Before", "After", "<em>d</em> = (Before - After)", "<em>d</em><sup>2</sup>")
    df_depData <- bind_rows(df_depData, summarise(df_depData, across(where(is.numeric), sum)))
    rownames(df_depData)[nrow(df_depData)] <- "Totals"

    datatable(round(df_depData, digits = 4),
              options = list(dom = 'lftp',
                             pageLength = -1,
                             lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")),
                             ordering = FALSE
              ),
              escape = FALSE
    ) %>% formatStyle(
      names(df_depData),
      target = 'row',
      fontWeight = styleRow(dim(df_depData)[1], "bold")
    )
  })
  
  ##### CI ----
  output$depMeansCI <- renderUI({
    tInt <- DepMeansTInt()
    dSum <- sum(GetDepMeansData()$d)
    dSqrdSum <- sum(GetDepMeansData()$d^2)
    
    p(
      withMathJax(),
      br(),
      sprintf("\\( \\displaystyle CI = \\bar{d} \\pm \\left( t_{\\alpha/2, \\, df} \\cdot \\dfrac{ s_{d} }{ \\sqrt{n} } \\right) \\)"),
      br(),
      br(),
      p("where"),
      sprintf("\\( \\qquad \\bar{d} = \\dfrac{ \\sum d }{ n } = \\dfrac{%s}{%s} = %s \\; , \\)",
              dSum,
              tInt["Sample Size"],
              tInt["Sample Mean"]),
      sprintf("\\( \\qquad s_{d} = \\sqrt{ \\dfrac{\\sum d^{2} - \\dfrac{(\\sum d)^{2}}{n} }{n - 1} } \\)"),
      sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\; , \\)",
              dSqrdSum,
              dSum,
              tInt["Sample Size"],
              tInt["Sample Size"],
              tInt['Sample SD']),
      sprintf("\\( \\qquad df = n - 1 = %s \\)",
              tInt["Sample Size"] - 1),
      br(),
      br(),
      br(),
      sprintf("\\( \\displaystyle CI = %g \\pm \\left( t_{%g/2, \\, %g} \\cdot \\dfrac{ %g }{ \\sqrt{ %g } } \\right) \\)",
              tInt["Sample Mean"],
              1 - ConfLvl(),
              tInt["Sample Size"] - 1,
              tInt["Sample SD"],
              tInt["Sample Size"]),
      br(),
      br(),
      sprintf("\\( \\displaystyle \\phantom{CI} = %g \\pm \\left( t_{%g, \\, %g} \\cdot \\dfrac{ %g }{ %g } \\right) \\)",
              tInt["Sample Mean"],
              (1 - ConfLvl()) / 2,
              tInt["Sample Size"] - 1,
              tInt["Sample SD"],
              sqrt(tInt["Sample Size"])),
      br(),
      br(),
      sprintf("\\( \\displaystyle \\phantom{CI} = %g \\pm ( %g \\cdot %g ) \\)",
              tInt["Sample Mean"],
              tInt["T Critical"],
              tInt["Std Error"]),
      br(),
      br(),
      sprintf("\\( \\displaystyle \\phantom{CI} = %g \\pm  %g  \\)",
              tInt["Sample Mean"],
              tInt["ME"]),
      br(),
      br(),
      sprintf("\\( \\displaystyle \\phantom{CI} = (%g, \\, %g)  \\)",
              tInt["LCL"],
              tInt["UCL"]),
      br(),
      br(),
      br(),
      p(tags$b("Interpretation:")),
      sprintf("We are \\( %1.0f \\)%% confident that the population mean difference \\( (\\mu_{d})\\) is between \\( %g \\) and \\( %g \\).",
              ConfLvl()*100,
              tInt["LCL"],
              tInt["UCL"]),
      br(),
      br(),
      br()
    )
    
  })
  
  ##### HT ----
  output$depMeansHT <- renderUI({

    req(GetDepMeansData()$sd != 0)
      tTest <- DepMeansTTest()
      dSum <- sum(GetDepMeansData()$d)
      dSqrdSum <- sum(GetDepMeansData()$d^2)
      
      intrpInfo <- IndMeansHypInfo()
      
      if(tTest["P-Value"] < 0.0001) {
        pValue <- "\\lt 0.0001"
      } else {
        pValue <- paste(tTest["P-Value"])
      }
      
      if(tTest["P-Value"] > SigLvl()) {
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
      
      if(input$altHypothesis2 == "2")
      {
        critVal <- paste("\\pm", tTest["T Critical"])
        nullHyp <- "\\mu_{d} ="
        altHyp <- "\\mu_{d} \\neq"
      }
      else
      {
        critVal <- tTest["T Critical"]
        
        if(input$altHypothesis2 == "1"){
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
          
          sprintf("\\( H_{0}: %s 0\\)",
                  nullHyp),
          br(),
          sprintf("\\( H_{a}: %s 0\\)",
                  altHyp),
          br(),
          br(),
          sprintf("\\( \\alpha = %s \\)",
                  SigLvl()),
          br(),
          br(),
          p(tags$b("Test Statistic:")),
          sprintf("\\( \\displaystyle t = \\dfrac{\\bar{d} - \\mu_{0}}{ \\left( \\dfrac{ s_{d} }{ \\sqrt{n} } \\right) } \\qquad \\)"),
          p("where"),
          sprintf("\\( \\qquad \\bar{d} = \\dfrac{ \\sum d }{ n } = \\dfrac{%s}{%s} = %s \\; , \\)",
                  dSum,
                  tTest["Sample Size"],
                  tTest["Sample Mean"]),
          sprintf("\\( \\qquad s_{d} = \\sqrt{ \\dfrac{\\sum d^{2} - \\dfrac{(\\sum d)^{2}}{n} }{n - 1} } \\)"),
          sprintf("\\( = \\sqrt{ \\dfrac{%s - \\dfrac{(%s)^{2}}{%s} }{%s - 1} } = %s \\; , \\)",
                  dSqrdSum,
                  dSum,
                  tTest["Sample Size"],
                  tTest["Sample Size"],
                  tTest['Sample SD']),
          br(),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\phantom{t} = \\; \\dfrac{%g - 0}{ \\left( \\dfrac{ %g }{ \\sqrt{ %g } } \\right) } \\)",
                  tTest["Sample Mean"],
                  tTest["Sample SD"],
                  tTest["Sample Size"]),
          sprintf("\\( \\displaystyle \\; = \\; \\dfrac{%g}{ \\left( \\dfrac{ %g }{ %g } \\right) } \\)",
                  tTest["Sample Mean"],
                  tTest["Sample SD"],
                  sqrt(tTest["Sample Size"])),
          br(),
          br(),
          sprintf("\\( \\displaystyle \\phantom{t} = \\; \\dfrac{ %g }{ %g } \\)",
                  tTest["Sample Mean"],
                  tTest["Std Error"]),
          sprintf("\\( \\displaystyle \\; = \\; %g \\)",
                  tTest["Test Statistic"]),
          br(),
          br(),
          br()
        )
      )
      
      depHTPVal <- printHTPVal(tTest["P-Value"], 
                               "t", 
                               intrpInfo$alternative, 
                               tTest["Test Statistic"], 
                               pvalSymbol, 
                               reject)
      
      depHTTail <- tagList(
        p(
          withMathJax(),
          p(tags$b("Using Critical Value Method:")),
          sprintf("Critical Value(s) \\( = %s t_{%s, \\, df} = %s t_{%s, \\, %s} = %s \\)",
                  IndMeansHypInfo()$critSign,
                  IndMeansHypInfo()$critAlph,
                  IndMeansHypInfo()$critSign,
                  IndMeansHypInfo()$alphaVal,
                  tTest["Sample Size"] - 1,
                  critVal),
          br(),
          br(),
          p("where"),
          sprintf("\\( \\qquad df = n - 1 = %s \\)",
                  tTest["Sample Size"] - 1),
          br(),
          br(),
          sprintf("Since the test statistic \\( (t)\\) falls within the %s region, %s \\( H_{0}\\).",
                  region,
                  reject),
          br(),
        ),
        
        plotOutput('depMeansHTPlot', width = "75%", height = "300px"),
        br()
      )
      
      depHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, 0)
      
      tagAppendChildren(depHTHead, depHTPVal, depHTTail, depHTConclusion)
  })
  
  ##### HT Plot ----
  output$depMeansHTPlot <- renderPlot({
    
    if(GetDepMeansData()$sd != 0) {
      tTest <- DepMeansTTest()
      intrpInfo <- IndMeansHypInfo()
      
      htPlotCritVal <- tTest["T Critical"]
      
      depMeansPlot <- hypTTestPlot(tTest["Test Statistic"], tTest["df"], htPlotCritVal, intrpInfo$alternative)
      depMeansPlot
    }

  })
  
  
  
  #### Two Prop outputs ----
  
  ##### CI ----
  output$twoPropCI <- renderUI({
    req(si_iv$is_valid())
    
    twoSampPropZInt <- TwoPropZInt(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, ConfLvl())
    
    p(
      withMathJax(
        sprintf("Given:"),
        br(),
        sprintf("\\( x_{1} = %s \\)",
                input$numSuccesses1),
        br(),
        sprintf("\\( n_{1} = %s \\)",
                input$numTrials1),
        br(),
        sprintf("\\( x_{2} = %s \\)",
                input$numSuccesses2),
        br(),
        sprintf("\\( n_{2} = %s \\)",
                input$numTrials2),
        br(),
        br(),
        br(),
        sprintf("For a \\( %s \\)%% Confidence Interval: ",
                ConfLvl()*100),
        br(),
        sprintf("\\( \\alpha = 1 - %s = %s \\)",
                ConfLvl(),
                1 - ConfLvl()),
        br(),
        sprintf("\\( z_{\\alpha/2} = z_{%s/2} = z_{%s} = %s \\)",
                1 - ConfLvl(),
                (1 - ConfLvl()) / 2,
                twoSampPropZInt["Z Critical"]),
        br(),
        br(),
        br(),
        sprintf("\\( \\displaystyle CI = (\\hat{p}_{1} - \\hat{p}_{2}) \\pm \\left( z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_{1}(1-\\hat{p}_{1})}{n_{1}} + \\dfrac{\\hat{p}_{2}(1-\\hat{p}_{2})}{n_{2}}} \\right) \\)"),
        br(),
        p("where"),
        sprintf("\\( \\displaystyle \\qquad \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                input$numSuccesses1,
                input$numTrials1,
                twoSampPropZInt["Sample Proportion 1"]),
        br(),
        p("and"),
        sprintf("\\( \\displaystyle \\qquad \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                input$numSuccesses2,
                input$numTrials2,
                twoSampPropZInt["Sample Proportion 2"]),
        br(),
        br(),
        br(),
        sprintf("\\( \\displaystyle CI = (%0.4f - %0.4f) \\pm \\left( %s \\sqrt{\\dfrac{%0.4f(1-%0.4f)}{%1.0f} + \\dfrac{%0.4f(1-%0.4f)}{%1.0f}} \\right) \\)",
                twoSampPropZInt["Sample Proportion 1"],
                twoSampPropZInt["Sample Proportion 2"],
                twoSampPropZInt["Z Critical"],
                twoSampPropZInt["Sample Proportion 1"],
                twoSampPropZInt["Sample Proportion 1"],
                input$numTrials1,
                twoSampPropZInt["Sample Proportion 2"],
                twoSampPropZInt["Sample Proportion 2"],
                input$numTrials2),
        br(),
        br(),
        sprintf("\\( \\phantom{CI} = %0.4f \\pm ( %s \\cdot %0.4f ) \\)",
                twoSampPropZInt["Difference of proportions"],
                twoSampPropZInt["Z Critical"],
                twoSampPropZInt["Std Error"]),
        br(),
        br(),
        sprintf("\\( \\phantom{CI} = %0.4f \\pm %0.4f \\)",
                twoSampPropZInt["Difference of proportions"],
                twoSampPropZInt["Margin of Error"]),
        br(),
        br(),
        sprintf("\\( \\phantom{CI} = (%0.4f, %0.4f)\\)",
                twoSampPropZInt["LCL"],
                twoSampPropZInt["UCL"]),
        br(),
        br(),
        br(),
        p(tags$b("Interpretation:")),
        sprintf("We are %1.0f%% confident that the difference in population proportions \\( (p_{1} - p_{2}) \\) is between \\( %0.4f \\) and \\( %0.4f \\).",
                ConfLvl()*100,
                twoSampPropZInt["LCL"],
                twoSampPropZInt["UCL"])
      )
    )
  })
  
  
  
  ##### HT ----
  output$twoPropHT <- renderUI({
    req(si_iv$is_valid())

    twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, 0, IndMeansHypInfo()$alternative, SigLvl())
    
    if(twoPropZTest["P-Value"] < 0.0001)
    {
      pValue <- "P \\lt 0.0001"
    }
    else
    {
      pValue <- paste("P = ", twoPropZTest["P-Value"])
    }
    
    
    if(input$altHypothesis2 == "2")
    {
      critZVal <- paste("\\pm", twoPropZTest["Z Critical"])
      htPlotCritVals <- c(-twoPropZTest["Z Critical"], twoPropZTest["Z Critical"])
      nullHyp <- "p_{1} ="
      altHyp <- "p_{1} \\neq"
    }
    else
    {
      critZVal <- paste(twoPropZTest["Z Critical"])
      htPlotCritVals <- twoPropZTest["Z Critical"]
      
      if(input$altHypothesis2 == "1"){
        nullHyp <- "p_{1} \\geq"
        altHyp <- "p_{1} \\lt"
      } else {
        nullHyp <- "p_{1} \\leq"
        altHyp <- "p_{1} \\gt"
      }
    }
    
    propDiff <- twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"]
    
    if(twoPropZTest["P-Value"] > SigLvl())
    {
      pvalSymbol <- "\\gt"
      suffEvidence <- "isn't"
      reject <- "do not reject"
      region <- "acceptance"
    }
    else
    {
      pvalSymbol <- "\\leq"
      suffEvidence <- "is"
      reject <- "reject"
      region <- "rejection"
    }
    
    twoPropHTHead <- tagList(
      withMathJax(
        sprintf("\\( H_{0}: %s p_{2}\\)",
                nullHyp),
        br(),
        sprintf("\\( H_{a}: %s p_{2}\\)",
                altHyp),
        br(),
        br(),
        sprintf("\\( \\alpha = %g \\)",
                SigLvl()),
        br(),
        br(),
        br(),
        p(tags$b("Test Statistic:")),
        sprintf("Given:"),
        br(),
        sprintf("\\( x_{1} = %s \\)",
                input$numSuccesses1),
        br(),
        sprintf("\\( n_{1} = %s \\)",
                input$numTrials1),
        br(),
        sprintf("\\( x_{2} = %s \\)",
                input$numSuccesses2),
        br(),
        sprintf("\\( n_{2} = %s \\)",
                input$numTrials2),
        br(),
        br(),
        br(),
        sprintf("\\(z = \\dfrac{ (\\hat{p}_{1} - \\hat{p}_{2}) - (p_{1} - p_{2})_{0} }{\\sqrt{\\hat{p}(1-\\hat{p})\\left(\\dfrac{1}{n_{1}} + \\dfrac{1}{n_{2}}\\right)}}\\)"),
        br(),
        br(),
        p("where"),
        sprintf("\\( \\displaystyle \\qquad \\hat{p} = \\dfrac{x_{1} + x_{2}}{n_{1} + n_{2}} \\)"),
        sprintf("\\( = \\dfrac{%g + %g}{%g + %g} = %0.4f, \\)",
                input$numSuccesses1,
                input$numSuccesses2,
                input$numTrials1,
                input$numTrials2,
                twoPropZTest["Pooled Proportion"]),
        br(),
        p("and"),
        sprintf("\\( \\displaystyle \\qquad \\hat{p}_{1} = \\dfrac{x_{1}}{n_{1}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                input$numSuccesses1,
                input$numTrials1,
                twoPropZTest["Sample Proportion 1"]),
        br(),
        p("and"),
        sprintf("\\( \\displaystyle \\qquad \\hat{p}_{2} = \\dfrac{x_{2}}{n_{2}} = \\dfrac{%s}{%s} = %0.4f,\\)",
                input$numSuccesses2,
                input$numTrials2,
                twoPropZTest["Sample Proportion 2"]),
        br(),
        br(),
        br(),
        sprintf("\\( z = \\dfrac{ (%0.4f - %0.4f) - 0}{\\sqrt{%0.4f(1-%0.4f)\\left(\\dfrac{1}{%g} + \\dfrac{1}{%g}\\right)}}\\)",
                twoPropZTest["Sample Proportion 1"],
                twoPropZTest["Sample Proportion 2"],
                twoPropZTest["Pooled Proportion"],
                twoPropZTest["Pooled Proportion"],
                input$numTrials1,
                input$numTrials2),
        sprintf("\\( = \\dfrac{%0.4f}{%0.4f} \\)",
                twoPropZTest["Sample Proportion 1"] - twoPropZTest["Sample Proportion 2"],
                twoPropZTest["Std Error"]),
        br(),
        br(),
        sprintf("\\(\\phantom{z} = %0.4f\\)",
                twoPropZTest["Test Statistic"]),
        br(),
        br(),
        br()
      )
    )
    
    twoPropHTPVal <- printHTPVal(twoPropZTest["P-Value"], 
                                 "z", 
                                 IndMeansHypInfo()$alternative, 
                                 twoPropZTest["Test Statistic"], 
                                 pvalSymbol, 
                                 reject)
    
    twoPropHTTail <- tagList(
      withMathJax(
        p(tags$b("Using Critical Value Method:")),
        sprintf("Critical Value(s) \\( = %s z_{%s} = %s z_{%s} = %s \\)",
                IndMeansHypInfo()$critSign,
                IndMeansHypInfo()$critAlph,
                IndMeansHypInfo()$critSign,
                IndMeansHypInfo()$alphaVal,
                critZVal),
        
        br(),
        br(),
        sprintf("Since the test statistic \\( (z)\\) falls within the %s region, %s \\( H_{0}\\).",
                region,
                reject),
        br(),
        br(),
        plotOutput('twoPropHTPlot'),
        br()
      )
    )
    
    twoPropHTConclusion <- printHTConclusion(region, reject, suffEvidence, altHyp, "p_{2}")
  
    tagAppendChildren(twoPropHTHead, twoPropHTPVal, twoPropHTTail, twoPropHTConclusion)
  })
  
  
  ##### HT Plot ----
  output$twoPropHTPlot <- renderPlot({
    req(si_iv$is_valid())
    
    twoPropZTest <- TwoPropZTest(input$numSuccesses1, input$numTrials1, input$numSuccesses2, input$numTrials2, 0, IndMeansHypInfo()$alternative, SigLvl())
    htPlotCritVal <- twoPropZTest["Z Critical"]
    
    htPlot <- hypZTestPlot(twoPropZTest["Test Statistic"], htPlotCritVal, IndMeansHypInfo()$alternative)
    htPlot
  })

  
  #### ANOVA Outputs ----
  output$anovaOutput <- renderUI({
    PrintANOVA()
  })
  
  output$oneWayAnovaTable <- renderDT({
    PrintANOVATable()
  })
  
  output$anovaUploadTable <- renderDT({
    req(anovaupload_iv$is_valid())
    datatable(anovaUploadData(),
              options = list(pageLength = -1,
                             lengthMenu = list(c(25, 50, 100, -1),
                                               c("25", "50", "100", "all"))))
  })
  
  
  #### Chi-Square Outputs ----
  # output$chiSq2x2 <- renderDT({
  #   
  #   datatable(chiSq2x2Totaled(),
  #             class = 'cell-border stripe',
  #             options = list(
  #               dom = 't',
  #               pageLength = -1,
  #               ordering = FALSE,
  #               searching = FALSE,
  #               paging = FALSE,
  #               autoWidth = FALSE,
  #               scrollX = TRUE,
  #               columnDefs = list(list(width = '100px', targets = c(0, 1, 2, 3)),
  #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3)))
  #             ),
  #             selection = "none",
  #             escape = FALSE,
  #             filter = "none",) %>%
  #     formatStyle(columns = c(0,3),
  #                 fontWeight = 'bold') %>%
  #     formatStyle(columns = 1:3,
  #                 target = 'row',
  #                 fontWeight = styleRow(dim(chiSq2x2Totaled())[1], "bold")) %>%
  #     formatStyle(columns = c(0,2),
  #                 borderRight = styleRow(c(1,2),'2px solid #787878')) %>%
  #     formatStyle(columns = c(1,2),
  #                 borderTop = styleRow(c(1),'2px solid #787878'),
  #                 borderBottom = styleRow(c(2),'2px solid #787878'))
  # })
  
  # output$chiSq2x3 <- renderDT({
  #   
  #   datatable(chiSq2x3Totaled(),
  #             class = 'cell-border stripe',
  #             options = list(
  #               dom = 't',
  #               pageLength = -1,
  #               ordering = FALSE,
  #               searching = FALSE,
  #               paging = FALSE,
  #               autoWidth = FALSE,
  #               scrollX = TRUE,
  #               columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
  #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
  #             ),
  #             selection = "none",
  #             escape = FALSE,
  #             filter = "none",) %>% 
  #     formatStyle(columns = c(0,4), #specify columns to format
  #                 fontWeight = 'bold') %>%
  #     formatStyle(columns = 1:4,
  #                 target = 'row',
  #                 fontWeight = styleRow(dim(chiSq2x3Totaled())[1], "bold")) %>%
  #     formatStyle(columns = c(0,3),
  #                 borderRight = styleRow(c(1,2),'2px solid #787878')) %>%
  #     formatStyle(columns = 1:3,
  #                 borderTop = styleRow(c(1),'2px solid #787878'),
  #                 borderBottom = styleRow(c(2),'2px solid #787878'))
  # })
  
  # output$chiSq3x2 <- renderDT({
  #   
  #   datatable(chiSq3x2Totaled(),
  #             class = 'cell-border stripe',
  #             options = list(
  #               dom = 't',
  #               pageLength = -1,
  #               ordering = FALSE,
  #               searching = FALSE,
  #               paging = FALSE,
  #               autoWidth = FALSE,
  #               scrollX = TRUE,
  #               columnDefs = list(list(width = '100px', targets = c(1, 2, 3)),
  #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3)))
  #             ),
  #             selection = "none",
  #             escape = FALSE,
  #             filter = "none",) %>% 
  #     formatStyle(columns = c(0,3), #specify columns to format
  #                 fontWeight = 'bold') %>%
  #     formatStyle(columns = 1:3,
  #                 target = 'row',
  #                 fontWeight = styleRow(dim(chiSq3x2Totaled())[1], "bold")) %>%
  #     formatStyle(columns = c(0,2),
  #                 borderRight = styleRow(1:3,'2px solid #787878')) %>%
  #     formatStyle(columns = c(1,2),
  #                 borderTop = styleRow(c(1),'2px solid #787878'),
  #                 borderBottom = styleRow(c(3),'2px solid #787878'))
  # })
  
  # output$chiSq3x3 <- renderDT({
  #   
  #   datatable(chiSq3x3Totaled(),
  #             class = 'cell-border stripe',
  #             options = list(
  #               dom = 't',
  #               pageLength = -1,
  #               ordering = FALSE,
  #               searching = FALSE,
  #               paging = FALSE,
  #               autoWidth = FALSE,
  #               scrollX = TRUE,
  #               columnDefs = list(list(width = '100px', targets = c(1, 2, 3, 4)),
  #                                 list(className = 'dt-center', targets = c(0, 1, 2, 3, 4)))
  #             ),
  #             selection = "none",
  #             escape = FALSE,
  #             filter = "none",) %>% 
  #     formatStyle(columns = c(0,4), #specify columns to format
  #                 fontWeight = 'bold') %>%
  #     formatStyle(columns = 1:4,
  #                 target = 'row',
  #                 fontWeight = styleRow(dim(chiSq3x3Totaled())[1], "bold")) %>%
  #     formatStyle(columns = c(0,3),
  #                 borderRight = styleRow(1:3,'2px solid #787878')) %>%
  #     formatStyle(columns = 1:3,
  #                 borderTop = styleRow(c(1),'2px solid #787878'),
  #                 borderBottom = styleRow(c(3),'2px solid #787878'))
  # })
  
  output$chiSqObs <- renderDT({
    chiSqData <- chiSqTotaled()
    
    CreateChiSqObserved(chiSqData)
  })
  
  output$chiSqExp <- renderDT({
    CreateChiSqExpected(chiSqResults()$Results$expected)
  })
  
  output$chiSqResultsMatrix <- renderDT({
    req(si_iv$is_valid())

    chiSqTest <- suppressWarnings(ChiSquareTest(chiSqActiveMatrix(), input$chiSquareYates))
    
    datatable(chiSqTest$Matrix,
              class = 'cell-border stripe',
              options = list(
                dom = 't',
                pageLength = -1,
                ordering = FALSE,
                searching = FALSE,
                paging = FALSE,
                autoWidth = FALSE,
                scrollX = TRUE,
                columnDefs = list(
                  list(width = '130px', targets = c(0, 1, 2, 3, 4, 5)),
                  list(className = 'dt-center', targets = c(0, 1, 2, 3, 4, 5)))
              ),
              selection = "none",
              escape = FALSE,
              filter = "none",
              rownames = FALSE) %>%
      formatStyle(columns = 0:ncol(chiSqTest$Matrix),
                  target = 'row',
                  fontWeight = styleRow(dim(chiSqTest$Matrix)[1], "bold")) 
  })
  
  
  output$chiSqTest <- renderUI({
    PrintChiSqTest()
  })
  
  output$chiSqPlot <- renderPlot({ ###### chisq plot ----
    data <- chiSqResults()
    chisq_df <- data$Results$parameter
    chisq_ts <- round(data$Results$statistic, 4)
    
    if(input$chisquareSigLvl == "10%") {
      sigLvl <- 0.1 
    } else if(input$chisquareSigLvl == "5%") {
      sigLvl <- 0.05
    } else {
      sigLvl <- 0.01
    }
    
    cv <- round(qchisq(1 - sigLvl, df = chisq_df), 4)
    # lower95 <- qchisq(.025, chisq_df)
    # upper95 <- qchisq(.975, chisq_df)
    
    xSeq <- c(seq(0, 15, length.out = 75), cv, chisq_ts)
    rrLabel <- c((cv + max(xSeq))/2)
    x_vector <- sort(c(xSeq, rrLabel))
    p_vector <- dchisq(x_vector, df = chisq_df)
    
    df <- distinct(data.frame(x = x_vector, y = p_vector))
    cvDF <- filter(df, x %in% cv)
    tsDF <- filter(df, x %in% chisq_ts)
    rrLabelDF <- filter(df, x %in% rrLabel)
    arLabelDF <- filter(df, y %in% max(p_vector))
    
    ggplot(df, 
           aes(x = x, y = y)) +
      stat_function(fun = dchisq, 
                    args = list(df = chisq_df),
                    geom = "Density",
                    fill = NA) +
      shadeHtArea(df, cv, "greater") +
      geom_segment(data = filter(df, y %in% max(p_vector)),
                   aes(x = 0, xend = 0, y = 0, yend = y, alpha = 0.5),
                   linetype = "solid",
                   linewidth = 0.75,
                   color='black',
                   show.legend = FALSE) +
      geom_text(data = filter(df, x %in% c(0)),
                aes(x = x, y = 0, label = "0"),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = -.03,
                check_overlap = TRUE) +
      geom_segment(data = cvDF,
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "solid",
                   lineend = 'butt',
                   linewidth = 1.5,
                   color='#023B70') +
      geom_text(data = cvDF,
                aes(x = x, y = 0, label = x),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = -.03,
                check_overlap = TRUE) +
      geom_segment(data = tsDF,
                   aes(x = x, xend = x, y = 0, yend = y + .055),
                   linetype = "solid",
                   linewidth = 1.25,
                   color='#BD130B') +
      geom_text(data = tsDF,
                aes(x = x, y = y, label = x),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = .075,
                check_overlap = TRUE) +
      geom_text(data = arLabelDF,
                aes(x = x, y = 0, label = "A R"),
                size = 16 / .pt,
                fontface = "bold",
                vjust = -4,
                check_overlap = TRUE) +
      geom_text(data = rrLabelDF,
                aes(x = x, y = y, label = "RR"),
                size = 16 / .pt,
                fontface = "bold",
                vjust = -4,
                check_overlap = TRUE) +
      theme_void() +
      ylab("") + 
      xlab(expression(bold(chi^2))) +
      scale_y_continuous(breaks = NULL) +
      theme(axis.title.x = element_text(size = 20))
      # coord_cartesian(clip="off")
      
  })
  
  output$fishersObs <- renderDT({
    req(si_iv$is_valid())
    
    CreateChiSqObserved(chiSqTotaled())
  })
  
  output$fishersTest <- renderUI({
    PrintFishersTest()
  })
  
  # --------------------------------------------------------------------- #
  
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  observeEvent(input$oneMeanUserData, priority = 5, {
    hide(id = "inferenceData")
    hide(id = "oneMeanVariable")
    fileInputs$oneMeanStatus <- 'uploaded'
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
    fileInputs$indMeansStatus <- 'uploaded'
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
    fileInputs$depMeansStatus <- 'uploaded'
    
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
  
  
  observeEvent(input$anovaUserData, {
    hide(id = "inferenceData")
    hide(id = "anovaUploadInputs")
    print("HO")
    # hide(id = "anovaResponse")
    # hide(id = "anovaFactors")
    
    fileInputs$anovaStatus <- 'uploaded'
    
    if(anovaupload_iv$is_valid())
    {
      freezeReactiveValue(input, "anovaMultiColumns")
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           "anovaMultiColumns",
                           choices = c(colnames(anovaUploadData()))
      )
      
      freezeReactiveValue(input, "anovaResponse")
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           "anovaResponse",
                           choices = c(colnames(anovaUploadData()))
      )
      
      freezeReactiveValue(input, "anovaFactors")
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           "anovaFactors",
                           choices = c(colnames(anovaUploadData()))
      )
      
      show(id = "anovaUploadInputs")
      print(length(input$anovaMultiColumns))
      print("HEY")
      # show(id = "anovaResponse")
      # show(id = "anovaFactors")
    }
  })
  
  observeEvent(input$chisquareDimension, {
    if( input$chisquareDimension != '2 x 2') {
      shinyjs::disable(selector = '#chisquareMethod input[value="Fisher"]')
      
      updatePrettyRadioButtons(
        inputId = "chisquareMethod",
        selected = "Chi-Square"
      )
    } else {
      shinyjs::enable(selector = '#chisquareMethod input[value="Fisher"]')
    }
  })
  
  observeEvent(input$goInference, {
    output$renderChiSqObs <- renderUI({
      DTOutput("chiSqObs", width = '500px')
    })
    
    output$renderChiSqExp <- renderUI({
      DTOutput("chiSqExp", width = '500px')
    })
    
    output$renderChiSqResults <- renderUI({
      DTOutput("chiSqResultsMatrix", width = "750px")
    })
    
    output$renderFishersObs <- renderUI({
      DTOutput("fishersObs", width = '500px')
    })
  })
  
  observeEvent(input$goInference, {
    #output$renderInference <- renderDataTable(

    if(si_iv$is_valid() && depmeansrawsd_iv$is_valid()) {
      show(id = "inferenceData")
      
    } else {
      hide(id = "inferenceData")
    }
    
    if(input$siMethod == '1'){

      if(input$popuParameter == 'Population Proportion') {
        req(input$numTrials && input$numSuccesses)
        if(input$numTrials < input$numSuccesses) {
          hide(id = "inferenceData")
        }
      } # input$popuParameter == 'Population Proportion'
    } # one sample
    
    else if(input$siMethod == '2') {
 
      if(input$popuParameters == 'Population Proportions') {
        req(!is.na(input$numSuccesses1) && !is.na(input$numTrials1))
        req(!is.na(input$numSuccesses2) && !is.na(input$numTrials2))

        if(input$numSuccesses1 > input$numTrials1 || input$numSuccesses2 > input$numTrials2) {
          print("amde it")
         hide(id = 'inferenceData') 
        }
        
      } else if(input$popuParameters == 'Dependent Population Means') {
        
        output$depMeansTable <- renderUI({
          DTOutput("depMeansData")
        })
      }
    } else if(input$siMethod == 'Categorical') {

      # output$render2x2ChiSq <- renderUI({
      #   tagList(
      #     
      #     titlePanel("Chi-Square Test for Independence"),
      #     hr(),
      #     br(),
      #     h3("Observed Frequencies"),
      #     DTOutput("chiSq2x2Obs", width = '500px'),
      #     br(),
      #     br(),
      #     h3("Expected Frequencies"),
      #     DTOutput("chiSq2x2", width = '500px'),
      #     br(),
      #   )
      # })
      # 
      # output$render2x3ChiSq <- renderUI({
      #   tagList(
      #     
      #     titlePanel("Chi-Square Test for Independence"),
      #     hr(),
      #     br(),
      #     DTOutput("chiSq2x3", width = '500px'),
      #     br(),
      #     br(),
      #     br(),
      #   )
      # })
      
      # output$render3x2ChiSq <- renderUI({
      #   tagList(
      #     
      #     titlePanel("Chi-Square Test for Independence"),
      #     hr(),
      #     br(),
      #     DTOutput("chiSq3x2", width = '500px'),
      #     br(),
      #     br(),
      #     br(),
      #   )
      # })
      # 
      # output$render3x3ChiSq <- renderUI({
      #   tagList(
      #     
      #     titlePanel("Chi-Square Test for Independence"),
      #     hr(),
      #     br(),
      #     DTOutput("chiSq3x3", width = '500px'),
      #     br(),
      #     br(),
      #     br(),
      #   )
      # })
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
    ext <- tolower(ext)
    
    switch(ext, 
           csv = read_csv(input$slrUserData$datapath, show_col_types = FALSE),
           xls = read_xls(input$slrUserData$datapath), 
           xlsx = read_xlsx(input$slrUserData$datapath),
           txt = read_tsv(input$slrUserData$datapath, show_col_types = FALSE),
           
           validate("Improper file format")
    )
  })
  
  sampleInfoRaw <- eventReactive({input$x
                                  input$y}, {
      
      dat <- list()
      datx <- createNumLst(input$x)
      daty <- createNumLst(input$y)
      
      dat$diff <- length(datx) - length(daty)
      dat$xSD <- sd(datx)
      dat$ySD <- sd(daty)
        
      return(dat)
    })
  
  explanatoryInfoUpload <- eventReactive(input$slrExplanatory, {
    dat <- list()
    datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
    
    dat$invalid <- any(!is.numeric(datx))
    dat$sd <- sd(datx, na.rm = TRUE)
    
    return(dat)
  })
  
  responseInfoUpload <- eventReactive(input$slrResponse, {
    dat <- list()
    daty <- as.data.frame(slrUploadData())[, input$slrResponse]
    
    dat$invalid <- any(!is.numeric(daty))
    dat$sd <- sd(daty, na.rm = TRUE)
    
    return(dat)
  })
  
  sampleDiffUpload <- eventReactive (c(input$slrExplanatory, 
                                       input$slrResponse), {
    if(input$slrResponse == "" | input$slrExplanatory == "") {
      return(0)
    } else {
      datx <- na.omit(as.data.frame(slrUploadData())[, input$slrExplanatory])
      daty <- na.omit(as.data.frame(slrUploadData())[, input$slrResponse])
                                           
      diff <- length(datx) - length(daty)
                                           
      return(diff)
    }
  })
  
  # --------------------------------------------------------------------- #
  
  
  ### Observers ----
  # --------------------------------------------------------------------- #
  
  observeEvent(input$slrUserData, {
    hide(id = "RegCorMP")
    hide(id = "slrResponse")
    hide(id = "slrExplanatory")
    fileInputs$slrStatus <- 'uploaded'
    
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
    
    if(input$simple_vs_multiple == 'SLR') {
      
      if(regcor_iv$is_valid()) {
        show(id = "SLRData")
      } else {
        hide(id = "SLRData")
      }
      
      output$slrValidation <- renderUI({
        
        if(!slrupload_iv$is_valid()) {
          
          if(is.null(input$slrUserData)) {
            validate("Please upload a file.")
          }
          
          validate(
            need(!is.null(fileInputs$slrStatus) && fileInputs$slrStatus == 'uploaded', "Please upload a file."),
            
            errorClass = "myClass"
          )
          
          validate(
            need(nrow(slrUploadData()) != 0, "File is empty."),
            need(ncol(slrUploadData()) > 1, "Data must include one response and (at least) one explanatory variable."),
            need(nrow(slrUploadData()) > 2, "Samples must include at least 2 observations."),
      
            errorClass = "myClass"
          )
        } 
        
        if(!slruploadvars_iv$is_valid()) {

          validate(
            need(input$slrExplanatory != "", "Please select an Explanatory Variable (x)."),
            need(input$slrResponse != "", "Please select a Response Variable (y).") %then%
              need(sampleDiffUpload() == 0, "The Explanatory (x) and Response (y) variables must have the same number of observations."),
            
            errorClass = "myClass"
          )
          
          validate(
            need(!explanatoryInfoUpload()$invalid, "The Explanatory Variable (x) contains non-numeric data.") %then%
              need(explanatoryInfoUpload()$sd != 0, "The data for the Explanatory Variable (x) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
            need(!responseInfoUpload()$invalid, "The Response Variable (y) contains non-numeric data.") %then%
              need(responseInfoUpload()$sd != 0, "The data for the Response Variable (y) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
            
            errorClass = "myClass"
          )
        } 
 
        if(input$dataRegCor == 'Upload Data') {
          req(slruploadvars_iv$is_valid())
          datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
          daty <- as.data.frame(slrUploadData())[, input$slrResponse]
        } else {
          
          validate(
            need(input$x, "Input required for the Independent Variable (x)."),
            need(input$y, "Input required for the Dependent Variable (y)."),
            
            errorClass = "myClass"
          )
          
          datx <- createNumLst(input$x)
          daty <- createNumLst(input$y)
        }
            
        validate(
          need(length(datx) >= 2, "Must have at least 2 observations for x."),
          need(length(daty) >= 2, "Must have at least 2 observations for y."),
          need(!anyNA(datx), "Data must be numeric."),
          need(!anyNA(daty), "Data must be numeric."),
          need(length(datx) == length(daty), "x and y must have the same number of observations."),
              
          errorClass = "myclass"
        )
        
        if(!slrraw_iv$is_valid()) {
          
          validate(
            need(sampleInfoRaw()$xSD != 0, "The data for the Independent Variable (x) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
            need(sampleInfoRaw()$ySD != 0, "The data for the Dependent Variable (y) must have a standard deviation greater than 0 to perform regression and correlation analysis."),
            
            errorClass = "myClass"
          )
        }
      }) #output$slrValidation
      
      if(regcor_iv$is_valid()) {
        
        if(input$dataRegCor == 'Upload Data') {
          datx <- as.data.frame(slrUploadData())[, input$slrExplanatory]
          daty <- as.data.frame(slrUploadData())[, input$slrResponse]
        } else {
          datx <- createNumLst(input$x)
          daty <- createNumLst(input$y)
        }
        
        model <- lm(daty ~ datx)
          
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
          ) %>% formatStyle(names(dfTotaled),
                            target = 'row',
                            fontWeight = styleRow(dim(dfTotaled)[1], "bold"))
        )
          
        output$scatterplot <- renderPlot({ # scatterplot ----
          
          RenderScatterplot(df, input$slrScatterTitle, input$slrScatterXlab, input$slrScatterYlab, input$slrRegLineColour, input$slrScatterColour)
          
        })
          
        if(summary(model)$coefficients["datx", "Estimate"] > 0){
          slopeDirection <- "increase"
          yHatOp <- "+"
          b0HatOp <- "-"
        } else {
          slopeDirection <- "decrease"
          yHatOp <- "-"
          b0HatOp <- "+"
        }
        
        interceptEstimate <- round(summary(model)$coefficients["(Intercept)", "Estimate"], 4)
        slopeEstimate <- round(summary(model)$coefficients["datx", "Estimate"], 4)
        
        output$regLineEquation <- renderUI({
          withMathJax()
          p(
            withMathJax(),
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
                    slopeEstimate),
            br(),
            br(),
            p("and"),
            sprintf("\\( \\qquad \\hat{\\beta}_{0} = \\bar{y} - \\hat{\\beta}_{1} \\bar{x}\\)"),
            sprintf("\\( \\, = \\, %g - (%0.4f) (%g) \\)",
                    mean(daty),
                    summary(model)$coefficients["datx", "Estimate"],
                    mean(datx)),
            sprintf("\\( \\, = \\, %g %s %0.4f\\)",
                    mean(daty),
                    b0HatOp,
                    abs(slopeEstimate) * mean(datx)),
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
          
        #----------------------------------#
        # Tests for normality of residuals #
        #----------------------------------#
        
        # Anderson-Darling Normality Test 
        # output$AndersonDarlingTest <- renderPrint({ 
        #   ad.test(model$residuals)
        # })
        #   
        # # Kolmogorov-Smirnov Normality Test 
        # output$KolmogorovSmirnovTest <- renderPrint({ 
        #   ks.test(model$residuals, "pnorm")
        # })
        #   
        # # Shapiro-Wilk Normality Test 
        # output$ShapiroTest <- renderPrint({ 
        #   shapiro.test(model$residuals) 
        # })
        #   
        # # Q-Q plot for residuals
        # output$qqplot <- renderPlot({
        #   #qqnorm(model$residuals, ylab = "Residuals", xlab = "Z Scores", main = "Q-Q plot of Standardized Residuals", pch = 19) #+
        #   #qqline(model$residuals)
        #   qqPlot(model$residuals, main = "Q-Q Plot", xlab = "Z Scores",  ylab = "Residuals", pch = 19) 
        # })
        #   
        # output$moreplots <- renderPlot({
        #   par(mfrow = c(2, 2))
        #   plot(model, which = 1:4, pch = 19)
        # })
          
          
        req(length(datx) > 1) ## correlation coefficient ----
        if(length(datx) > 2) {
          
          pearson <- cor.test(datx, daty, method = "pearson")
          
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
              
            if(length(datx) > 3) {
              output$PearsonConfInt <- renderPrint({ 
                pearson$conf.int
              })
            } else {
              output$PearsonConfInt <- renderPrint ({
                noquote("Computation of the Confidence Interval requires a minimum sample size of 4.")
              })
            }
          }
            
            
            # output$PearsonEstimate <- renderPrint({
            #   cat(noquote(paste(c("Pearson's r:", round(pearson$estimate[[1]], 4)))))
            # })
        }
        else
        {
          output$PearsonCorTest <- renderPrint ({
            noquote("Pearson's Product-Moment Correlation requires a minimum sample size of 3 for computation.")
          })
        }
          
        kendall <- cor.test(datx, daty, method = "kendall")
        spearman <- cor.test(datx, daty, method = "spearman")
        
        output$kendallEstimate <- renderUI({
          sprintf("\\( \\tau \\; = \\; %0.4f \\)", 
                  kendall$estimate)
        })
        
        output$spearmanEstimate <- renderUI({
          sprintf("\\( \\displaystyle r_{s} \\; = \\; 1 - \\dfrac{ 6 \\, \\sum\\limits_{i=1}^n d^2_{i}}{ n(n^2 - 1)} \\; = \\; %0.4f \\)", 
                  spearman$estimate)
        })
        
        output$slrViewUpload <- renderDT({
          req(slrupload_iv$is_valid())
          datatable(slrUploadData(),
                    options = list(pageLength = -1,
                                   lengthMenu = list(c(25, 50, 100, -1),
                                                     c("25", "50", "100", "all"))))
        })
          
      } #if regcor_iv is valid
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
  
  # observeEvent(!ds_iv$is_valid(), {
  #   hide(id = "descriptiveStatsMP")
  # })
  
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
    dsReset(TRUE)
    hide(id = 'descriptiveStatsMP')
    shinyjs::reset("descriptiveStatsPanel")
    fileInputs$dsStatus <- 'reset'
  })
  
  #  -------------------------------------------------------------------- #
  
  
  
  #### Probability Distributions ----
  #  -------------------------------------------------------------------- #
  
  observeEvent({input$cTableDimension
                input$cTableType
                input$cMatrix2x2
                input$cMatrix2x3
                input$cMatrix3x2
                input$cMatrix3x3}, {
    hide('probabilityMP')
  })
  
  observeEvent(input$gocTable, {
    show(id = 'probabilityMP')
  })
  
  #-----------------------#
  # Binomial Distribution #
  #-----------------------#
  
  observeEvent(input$goBinom, {
    show(id = 'probabilityMP')
  })
  
  observeEvent({input$numTrialsBinom
                input$successProbBinom
                input$numSuccessesBinom
                input$numSuccessesBinomx1
                input$numSuccessesBinomx2}, {
    hide(id = 'probabilityMP')
  })
  
  observeEvent(input$calcBinom, {
    if(input$calcBinom == 'between') {
      hide(id = "probabilityMP")
    }
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
  
  observeEvent({input$muPoisson
                input$xPoisson
                input$x1Poisson
                input$x2Poisson}, {
    hide(id = 'probabilityMP')
  })
  
  observeEvent(input$calcPoisson, {
    if(input$calcPoisson == 'between') {
      hide(id = "probabilityMP")
    }
  })
  
  observeEvent(input$resetPoisson, {
    hide(id = "probabilityMP")
    shinyjs::reset("poissonPanel")
  })
  
  #---------------------#
  # Normal Distribution #
  #---------------------#
  
  observeEvent(input$goNormalProb, {
    show(id = "probabilityMP")
  })
  
  observeEvent(input$goNormalQuan, {
    show(id = "probabilityMP")
  })
  
  observeEvent({input$popMean
                input$popSD
                input$xValue
                input$x1Value
                input$x2Value
                input$sampMeanDistr
                input$sampDistrxValue
                input$sampDistrx1Value
                input$sampDistrx2Value
                input$sampDistrSize
                input$calcQuantiles
                input$percentileValue}, {
    hide(id = 'probabilityMP')
  })
  
  observeEvent(input$calcQuartiles, {
    if(input$calcQuartiles == 'Percentile') {
      hide(id = "probabilityMP")
    }
  })
  
  observeEvent(input$calcNormal, {
    if(input$calcNormal == 'between') {
      hide(id = "probabilityMP")
    }
  })
  
  observeEvent(input$calcNormSampDistr, {
    if(input$calcNormSampDistr == 'between') {
      hide(id = "probabilityMP")
    }
  })
  
  observeEvent(input$resetNormalProb, {
    hide(id = "probabilityMP")
    shinyjs::reset("normalPanel")
  })
  
  observeEvent(input$resetNormalQuan, {
    hide(id = "probabilityMP")
    shinyjs::reset("normalPanel")
  })
  
  #  -------------------------------------------------------------------- #
  
  
  #### Sample Size Estimation ----
  #  -------------------------------------------------------------------- #
  
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
  
  #  -------------------------------------------------------------------- #
  
  
  #### Statistical Inference ----
  #  -------------------------------------------------------------------- #
  
  observeEvent(!si_iv$is_valid(), {
    hide(id = "inferenceMP")
    hide(id = "inferenceData")
  })
  
  observeEvent(!depmeansrawsd_iv$is_valid(), {
    hide(id = "inferenceMP")
    hide(id = "inferenceData")
  })
  
  observeEvent({input$siMethod
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
  
  observeEvent(!anovaupload_iv$is_valid(), {
    hide(id = "inferenceMP")
    hide(id = "inferenceData")
  })
  
  observeEvent(input$goInference, {
    show(id = "inferenceMP")
  })
  
  observeEvent(input$resetInference, {
    hide(id = "inferenceMP")
    hide(id = "anovaUploadInputs")
    shinyjs::reset("inferencePanel")
    fileInputs$oneMeanStatus <- 'reset'
    fileInputs$indMeansStatus <- 'reset'
    fileInputs$depMeansStatus <- 'reset'
    fileInputs$anovaStatus <- 'reset'
  })
  
  #  -------------------------------------------------------------------- #
  
  
  
  #### Regression and Correlation ----
  #  -------------------------------------------------------------------- #
  
  observeEvent(!regcor_iv$is_valid(), {
    hide(id = "RegCorMP")
    hide(id = "SLRData")
  })
  
  observeEvent(input$dataRegCor, {
    hide(id = "RegCorMP")
    hide(id = "slrResponse")
    hide(id = "slrExplanatory")
    updateTextInput(inputId = "xlab", value = "x")
    updateTextInput(inputId = "ylab", value = "y")
  })
  
  observe({
    
    if(input$dataRegCor == 'Enter Raw Data') {
      hideTab(inputId = "slrTabset", target = "Uploaded Data")
      updateTabsetPanel(inputId = 'slrTabset', selected = 'Simple Linear Regression')
    } else {
      showTab(inputId = "slrTabset", target = "Uploaded Data")
    }
  })
  
  observeEvent(input$resetRegCor, {
    # hideTab(inputId = 'tabSet', target = 'Simple Linear Regression')
    # hideTab(inputId = 'tabSet', target = 'Normality of Residuals')
    # hideTab(inputId = 'tabSet', target = 'Residual Plots')
    hide(id = "RegCorMP")
    shinyjs::reset("RegCorPanel")
    fileInputs$slrStatus <- 'reset'
    
  })
  
  #  -------------------------------------------------------------------- #
}