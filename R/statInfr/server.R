statInfrServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    {# Data Validation
      indmeanssumm_iv <- InputValidator$new()
      indmeansraw_iv <- InputValidator$new()
      indmeansupload_iv <- InputValidator$new()
      indmeansuploadvar_iv <- InputValidator$new()
      indmeanssdknown_iv <- InputValidator$new()
      indmeanssdunk_iv <- InputValidator$new()
      indmeansrawsd_iv <- InputValidator$new()
      indmeansrawsdunk_iv <- InputValidator$new()
      indmeansuploadsd_iv <- InputValidator$new()
      indmeansmunaught_iv <- InputValidator$new()

      wilcoxonUpload_iv <- InputValidator$new()
      wilcoxonraw_iv <- InputValidator$new()
      wilcoxonRanksuploadvars_iv <- InputValidator$new()
      wRankSumrawsd_iv <- InputValidator$new()

      depmeansraw_iv <- InputValidator$new()
      depmeansupload_iv <- InputValidator$new()
      depmeansuploadvars_iv <- InputValidator$new()
      depmeansrawsd_iv <- InputValidator$new()
      depmeansmunaught_iv <- InputValidator$new()

      signedRankUpload_iv <- InputValidator$new()
      signedRankRaw_iv <- InputValidator$new()
      signedRankUploadvars_iv <- InputValidator$new()
      signedRankrawsd_iv <- InputValidator$new()

      oneSD_iv <- InputValidator$new()
      oneSDht_iv <- InputValidator$new()

      oneprop_iv <- InputValidator$new()
      onepropht_iv <- InputValidator$new()

      twoprop_iv <- InputValidator$new()
      twopropht_iv <- InputValidator$new()
      twopropdiffnaught_iv <- InputValidator$new()

      twopopvarsum_iv <- InputValidator$new()
      twopopvar_iv <- InputValidator$new()
      twopopvarraw_iv <- InputValidator$new()

      kwupload_iv <- InputValidator$new()
      kwmulti_iv <- InputValidator$new()
      kwstacked_iv <- InputValidator$new()

      anovaupload_iv <- InputValidator$new()
      anovamulti_iv <- InputValidator$new()
      anovastacked_iv <- InputValidator$new()

      chiSq2x2_iv <- InputValidator$new()
      chiSq2x3_iv <- InputValidator$new()
      chiSq3x2_iv <- InputValidator$new()
      chiSq3x3_iv <- InputValidator$new()

      ## ------------ Rules -------------------------------------------------------

      ## sampleSize1
      indmeanssumm_iv$add_rule("sampleSize1", sv_required())
      indmeanssumm_iv$add_rule("sampleSize1", sv_integer())
      indmeanssumm_iv$add_rule("sampleSize1", sv_gt(1))

      ## sampleMean1
      indmeanssumm_iv$add_rule("sampleMean1", sv_required())

      ## sampleSize2
      indmeanssumm_iv$add_rule("sampleSize2", sv_required())
      indmeanssumm_iv$add_rule("sampleSize2", sv_integer())
      indmeanssumm_iv$add_rule("sampleSize2", sv_gt(1))

      ## sampleMean2
      indmeanssumm_iv$add_rule("sampleMean2", sv_required())

      ## popuSD1
      indmeanssdknown_iv$add_rule("popuSD1", sv_required())
      indmeanssdknown_iv$add_rule("popuSD1", sv_gt(0))

      ## popuSD2
      indmeanssdknown_iv$add_rule("popuSD2", sv_required())
      indmeanssdknown_iv$add_rule("popuSD2", sv_gt(0))

      ## sampSD1
      indmeanssdunk_iv$add_rule("sampSD1", sv_required())
      indmeanssdunk_iv$add_rule("sampSD1", sv_gt(0))

      ## sampSD2
      indmeanssdunk_iv$add_rule("sampSD2", sv_required())
      indmeanssdunk_iv$add_rule("sampSD2", sv_gt(0))

      ## raw_sample1
      indmeansraw_iv$add_rule("raw_sample1", sv_required())
      indmeansraw_iv$add_rule("raw_sample1", sv_regex(
                                               "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)"
                                             ))

      ## raw_sample2
      indmeansraw_iv$add_rule("raw_sample2", sv_required())
      indmeansraw_iv$add_rule("raw_sample2", sv_regex(
                                               "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."
                                             ))

      indmeansrawsd_iv$add_rule("popuSDRaw1", sv_required())
      indmeansrawsd_iv$add_rule("popuSDRaw1", sv_gt(0))

      indmeansrawsd_iv$add_rule("popuSDRaw2", sv_required())
      indmeansrawsd_iv$add_rule("popuSDRaw2", sv_gt(0))

      indmeansrawsdunk_iv$add_rule("raw_sample1", ~ if (sd(createNumLst(input$raw_sample1)) == 0 &&
                                                        sd(createNumLst(input$raw_sample2)) == 0 &&
                                                        input$inferenceType2 == "Hypothesis Testing") {
                                                      "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2."
                                                    })
      indmeansrawsdunk_iv$add_rule("raw_sample2", ~ if (sd(createNumLst(input$raw_sample1)) == 0 &&
                                                        sd(createNumLst(input$raw_sample2)) == 0 &&
                                                        input$inferenceType2 == "Hypothesis Testing") {
                                                      "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2."
                                                    })

                                        # indMeansUserData
      indmeansupload_iv$add_rule("indMeansUserData", sv_required())
      indmeansupload_iv$add_rule("indMeansUserData", ~ if (is.null(fileInputs$indMeansStatus) || fileInputs$indMeansStatus == "reset") "Required")
      ## indmeansupload_iv$add_rule("indMeansUserData", ~ if(!(tolower(tools::file_ext(input$indMeansUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
      indmeansupload_iv |> add_rule_accepted_file_formats("indMeansUserData")
      indmeansupload_iv$add_rule("indMeansUserData", ~ if (nrow(IndMeansUploadData()) == 0) "File is empty.")
      indmeansupload_iv$add_rule("indMeansUserData", ~ if (ncol(IndMeansUploadData()) < 2) "File must contain at least 2 distinct samples to choose from for analysis.")
      indmeansupload_iv$add_rule("indMeansUserData", ~ if (nrow(IndMeansUploadData()) < 3) "Samples must include at least 2 observations.")

      indmeansuploadsd_iv$add_rule("popuSDUpload1", sv_required())
      indmeansuploadsd_iv$add_rule("popuSDUpload1", sv_gt(0))

      indmeansuploadsd_iv$add_rule("popuSDUpload2", sv_required())
      indmeansuploadsd_iv$add_rule("popuSDUpload2", sv_gt(0))

      indmeansuploadvar_iv$add_rule("indMeansUplSample1", sv_required())
      indmeansuploadvar_iv$add_rule("indMeansUplSample2", sv_required())
      indmeansuploadvar_iv$add_rule("indMeansUplSample1", ~ {
        if (checkNumeric(IndMeansUploadData(), input$indMeansUplSample1)) {
          "Selected column contains non-numeric data."
        }
      })

      indmeansuploadvar_iv$add_rule("indMeansUplSample2", ~ {
        if (checkNumeric(IndMeansUploadData(), input$indMeansUplSample2)) {
          "Selected column contains non-numeric data."
        }
      })
      indmeansuploadvar_iv$add_rule("indMeansUplSample1", ~ {
        d <- IndMeansUploadData()
        col <- input$indMeansUplSample1
        if (is.null(col) || col == "" || !(col %in% names(d))) {
          return(NULL)
        }
        s1 <- na.omit(unlist(d[, col]))
        if (length(s1) < 2) "Sample 1 must have at least 2 observations"
      })

      indmeansuploadvar_iv$add_rule("indMeansUplSample2", ~ {
        d <- IndMeansUploadData()
        col <- input$indMeansUplSample2
        if (is.null(col) || col == "" || !(col %in% names(d))) {
          return(NULL)
        }
        s2 <- na.omit(unlist(d[, col]))
        if (length(s2) < 2) "Sample 2 must have at least 2 observations"
      })

      indmeansuploadvar_iv$add_rule("indMeansUplSample1", ~ {
        d <- IndMeansUploadData()
        if (input$indMeansUplSample1 == "" || input$indMeansUplSample2 == "" ||
            !(input$indMeansUplSample1 %in% names(d)) || !(input$indMeansUplSample2 %in% names(d))) {
          return(NULL)
        }
        s1 <- na.omit(unlist(d[, input$indMeansUplSample1]))
        s2 <- na.omit(unlist(d[, input$indMeansUplSample2]))
        if (input$bothsigmaKnownUpload == "bothUnknown" && input$inferenceType2 == "Hypothesis Testing" && sd(s1) == 0 && sd(s2) == 0) {
          "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2"
        }
      })

      indmeansuploadvar_iv$add_rule("indMeansUplSample2", ~ {
        d <- IndMeansUploadData()
        if (input$indMeansUplSample1 == "" || input$indMeansUplSample2 == "" ||
            !(input$indMeansUplSample1 %in% names(d)) || !(input$indMeansUplSample2 %in% names(d))) {
          return(NULL)
        }
        s1 <- na.omit(unlist(d[, input$indMeansUplSample1]))
        s2 <- na.omit(unlist(d[, input$indMeansUplSample2]))
        if (input$bothsigmaKnownUpload == "bothUnknown" && input$inferenceType2 == "Hypothesis Testing" && sd(s1) == 0 && sd(s2) == 0) {
          "Sample standard deviation cannot be 0 for both Sample 1 and Sample 2"
        }
      })

      wilcoxonUpload_iv$add_rule("wilcoxonUpl", sv_required())
      wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if (is.null(fileInputs$rankSumStatus) || fileInputs$rankSumStatus == "reset") "Required")
      ## wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if(!(tolower(tools::file_ext(input$wilcoxonUpl$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
      wilcoxonUpload_iv |> add_rule_accepted_file_formats("wilcoxonUpl")
      wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if (nrow(WilcoxonUploadData()) == 0) "File is empty.")
      wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if (ncol(WilcoxonUploadData()) < 2) "File must contain at least 2 distinct samples to choose from for analysis.")
      wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ if (nrow(WilcoxonUploadData()) < 3) "Samples must include at least 2 observations.")
      wilcoxonraw_iv$add_rule("rankSumRaw1", sv_required())
      wilcoxonraw_iv$add_rule("rankSumRaw1", sv_regex(
                                               "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)"
                                             ))
      wilcoxonraw_iv$add_rule("rankSumRaw2", sv_required())
      wilcoxonraw_iv$add_rule("rankSumRaw2", sv_regex(
                                               "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."
                                             ))
      wilcoxonRanksuploadvars_iv$add_rule("wilcoxonUpl1", sv_required())
      wilcoxonRanksuploadvars_iv$add_rule("wilcoxonUpl2", sv_required())
      wRankSumrawsd_iv$add_rule("rankSumRaw2", ~ {
        data <- GetwRankSumMeansData()
        if (is.null(data) ||
            length(unique(data$samp1)) <= 1 ||
            length(unique(data$samp2)) <= 1) {
          "Variance required in Sample 1 and Sample 2 data for hypothesis testing."
        }
      })
      wilcoxonUpload_iv$add_rule("wilcoxonUpl", ~ {
        data <- WilcoxonUploadData()
        if (!is.null(data) && nrow(data) > 0) {
          if (!all(sapply(data, is.numeric))) {
            "Uploaded data contains non-numeric values. Please ensure all columns are numeric."
          }
        }
      })
      ## ind means Mu Naught
      indmeansmunaught_iv$add_rule("indMeansMuNaught", sv_required())

      ## before
      depmeansraw_iv$add_rule("before", sv_required())
      depmeansraw_iv$add_rule("before", sv_regex(
                                          "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                          "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)"
                                        ))

      ## after
      depmeansraw_iv$add_rule("after", sv_required())
      depmeansraw_iv$add_rule("after", sv_regex(
                                         "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                         "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."
                                       ))


      depmeansraw_iv$add_rule("before", ~ if (length(createNumLst(input$before)) != length(createNumLst(input$after))) "Sample 1 and Sample 2 must have the same number of observations.")
      depmeansraw_iv$add_rule("after", ~ if (length(createNumLst(input$before)) != length(createNumLst(input$after))) "Sample 1 and Sample 2 must have the same number of observations.")

      depmeansupload_iv$add_rule("depMeansUserData", sv_required())
      depmeansupload_iv$add_rule("depMeansUserData", ~ if (is.null(fileInputs$depMeansStatus) || fileInputs$depMeansStatus == "reset") "Required")
      ## depmeansupload_iv$add_rule("depMeansUserData", ~ if(!(tolower(tools::file_ext(input$depMeansUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
      depmeansupload_iv |> add_rule_accepted_file_formats("depMeansUserData")
      depmeansupload_iv$add_rule("depMeansUserData", ~ if (nrow(DepMeansUploadData()) == 0) "File is empty.")
      depmeansupload_iv$add_rule("depMeansUserData", ~ if (ncol(DepMeansUploadData()) < 2) "File must contain at least 2 distinct 'Before' and 'After' sets of data to choose from for analysis.")
      depmeansupload_iv$add_rule("depMeansUserData", ~ if (nrow(DepMeansUploadData()) < 4) "Samples must include at least 3 observations.")


      depmeansuploadvars_iv$add_rule("depMeansUplSample1", sv_required())
      depmeansuploadvars_iv$add_rule("depMeansUplSample2", sv_required())
      depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ if (CheckDepUploadSamples() != 0) "Sample 1 and Sample 2 must have the same number of observations.")
      depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ if (CheckDepUploadSamples() != 0) "Sample 1 and Sample 2 must have the same number of observations.")
      depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ {
        if (checkNumeric(DepMeansUploadData(), input$depMeansUplSample1)) {
          "Selected column contains non-numeric data."
        }
      })
      depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ {
        if (checkNumeric(DepMeansUploadData(), input$depMeansUplSample2)) {
          "Selected column contains non-numeric data."
        }
      })

      depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ {
        d <- DepMeansUploadData()
        col <- input$depMeansUplSample1
        if (col == "" || !(col %in% names(d))) {
          return(NULL)
        }
        if (length(na.omit(unlist(d[, col]))) < 3) {
          "Sample 1 must have at least 3 observations."
        }
      })
      depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ {
        d <- DepMeansUploadData()
        col <- input$depMeansUplSample2
        if (col == "" || !(col %in% names(d))) {
          return(NULL)
        }
        if (length(na.omit(unlist(d[, col]))) < 3) {
          "Sample 2 must have at least 3 observations."
        }
      })

      depmeansuploadvars_iv$add_rule("depMeansUplSample1", ~ {
        d <- DepMeansUploadData()
        if (!(input$depMeansUplSample1 %in% names(d)) ||
            !(input$depMeansUplSample2 %in% names(d))) {
          return(NULL)
        }

        if (input$depMeansUplSample1 != "" &&
            input$depMeansUplSample2 != "" &&
            !checkNumeric(DepMeansUploadData(), input$depMeansUplSample1) &&
            !checkNumeric(DepMeansUploadData(), input$depMeansUplSample2) &&
            (input$depMeansUplSample1 == input$depMeansUplSample2 ||
             GetDepMeansData()$sd == 0)) {
          "Standard deviation of the difference (sd) is zero."
        }
      })
      depmeansuploadvars_iv$add_rule("depMeansUplSample2", ~ {
        d <- DepMeansUploadData()
        if (!(input$depMeansUplSample1 %in% names(d)) ||
            !(input$depMeansUplSample2 %in% names(d))) {
          return(NULL)
        }

        if (input$depMeansUplSample1 != "" &&
            input$depMeansUplSample2 != "" &&
            !checkNumeric(DepMeansUploadData(), input$depMeansUplSample1) &&
            !checkNumeric(DepMeansUploadData(), input$depMeansUplSample2) &&
            (input$depMeansUplSample1 == input$depMeansUplSample2 ||
             GetDepMeansData()$sd == 0)) {
          "Standard deviation of the difference (sd) is zero."
        }
      })

      depmeansraw_iv$add_rule("before", ~ if (GetDepMeansData()$sd == 0) "Standard deviation of the difference (sd) is zero.")
      depmeansraw_iv$add_rule("after", ~ if (GetDepMeansData()$sd == 0) "Standard deviation of the difference (sd) is zero.")

      depmeansmunaught_iv$add_rule("depMeansMuNaught", sv_required())
      ## signed Rank Test
      signedRankUpload_iv$add_rule("signedRankUpl", sv_required())
      signedRankUpload_iv$add_rule("signedRankUpl", ~ if (is.null(fileInputs$signedRankStatus) || fileInputs$signedRankStatus == "reset") "Required")
      ## signedRankUpload_iv$add_rule("signedRankUpl", ~ if(!(tolower(tools::file_ext(input$signedRankUpl$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
      signedRankUpload_iv |> add_rule_accepted_file_formats("signedRankUpl")
      signedRankUpload_iv$add_rule("signedRankUpl", ~ if (nrow(signedRankUploadData()) == 0) "File is empty.")
      signedRankUpload_iv$add_rule("signedRankUpl", ~ if (ncol(signedRankUploadData()) < 2) "File must contain at least 2 distinct samples to choose from for analysis.")
      signedRankUpload_iv$add_rule("signedRankUpl", ~ if (nrow(signedRankUploadData()) < 3) "Samples must include at least 2 observations.")
      signedRankRaw_iv$add_rule("signedRankRaw1", sv_required())
      signedRankRaw_iv$add_rule("signedRankRaw1", sv_regex(
                                                    "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)"
                                                  ))
      signedRankRaw_iv$add_rule("signedRankRaw2", sv_required())
      signedRankRaw_iv$add_rule("signedRankRaw2", sv_regex(
                                                    "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                                    "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."
                                                  ))
      signedRankRaw_iv$add_rule("signedRankRaw1", ~ if (length(createNumLst(input$signedRankRaw1)) != length(createNumLst(input$signedRankRaw2))) "Sample 1 and Sample 2 must have the same number of observations.")
      signedRankRaw_iv$add_rule("signedRankRaw2", ~ if (length(createNumLst(input$signedRankRaw1)) != length(createNumLst(input$signedRankRaw2))) "Sample 1 and Sample 2 must have the same number of observations.")

      signedRankUploadvars_iv$add_rule("signedRankUpl1", sv_required())
      signedRankUploadvars_iv$add_rule("signedRankUpl2", sv_required())
      signedRankUploadvars_iv$add_rule("signedRankUpl1", ~ if (CheckSignedRankUploadSamples() != 0) "Sample 1 and Sample 2 must have the same number of observations.")
      signedRankUploadvars_iv$add_rule("signedRankUpl2", ~ if (CheckSignedRankUploadSamples() != 0) "Sample 1 and Sample 2 must have the same number of observations.")


      signedRankUpload_iv$add_rule("signedRankUpl", ~ {
        data <- signedRankUploadData()
        if (!is.null(data) && nrow(data) > 0) {
          if (!all(sapply(data, is.numeric))) {
            "Uploaded data contains non-numeric values. Please ensure all columns are numeric."
          }
        }
      })
      signedRankRaw_iv$add_rule("signedRankRaw1", ~ {
        sample1 <- createNumLst(input$signedRankRaw1)
        sample2 <- createNumLst(input$signedRankRaw2)
        if (length(sample1) == length(sample2)) {
          differences <- sample1 - sample2
          if (all(differences == 0) || var(differences) == 0) {
            "'Sample 1’' and 'Sample 2' data are the same."
          }
        }
      })

      signedRankRaw_iv$add_rule("signedRankRaw2", ~ {
        sample1 <- createNumLst(input$signedRankRaw1)
        sample2 <- createNumLst(input$signedRankRaw2)
        if (length(sample1) == length(sample2)) {
          differences <- sample1 - sample2
          if (all(differences == 0) || var(differences) == 0) {
            "'Sample 1’' and 'Sample 2' data are the same."
          }
        }
      })

      signedRankUploadvars_iv$add_rule("signedRankUpl1", ~ {
        if (!rv$allowColumnValidation) {
          return(NULL)
        }

        if (input$signedRankUpl1 != "" && input$signedRankUpl2 != "") {
          data <- signedRankUploadData()
          if (!(input$signedRankUpl1 %in% colnames(data))) {
            return("Selected column for Sample 1 does not exist in the uploaded file.")
          }
          if (!(input$signedRankUpl2 %in% colnames(data))) {
            return("Selected column for Sample 2 does not exist in the uploaded file.")
          }
          sample1 <- na.omit(unlist(data[, input$signedRankUpl1]))
          sample2 <- na.omit(unlist(data[, input$signedRankUpl2]))
          min_length <- min(length(sample1), length(sample2))
          if (min_length > 0) {
            differences <- sample1[1:min_length] - sample2[1:min_length]
            if (all(differences == 0) || var(differences) == 0) {
              "'Sample 1’' and 'Sample 2' data are the same."
            }
          }
        }
      })

      signedRankUploadvars_iv$add_rule("signedRankUpl2", ~ {
        if (!rv$allowColumnValidation) {
          return(NULL)
        }

        if (input$signedRankUpl1 != "" && input$signedRankUpl2 != "") {
          data <- signedRankUploadData()
          if (!(input$signedRankUpl1 %in% colnames(data))) {
            return("Selected column for Sample 1 does not exist in the uploaded file.")
          }
          if (!(input$signedRankUpl2 %in% colnames(data))) {
            return("Selected column for Sample 2 does not exist in the uploaded file.")
          }
          sample1 <- na.omit(unlist(data[, input$signedRankUpl1]))
          sample2 <- na.omit(unlist(data[, input$signedRankUpl2]))
          min_length <- min(length(sample1), length(sample2))
          if (min_length > 0) {
            differences <- sample1[1:min_length] - sample2[1:min_length]
            if (all(differences == 0) || var(differences) == 0) {
              "'Sample 1’' and 'Sample 2' data are the same."
            }
          }
        }
      })

      ## sample standard deviation
      oneSD_iv$add_rule("SSDSampleSize", sv_required())
      oneSD_iv$add_rule("SSDSampleSize", sv_integer())
      oneSD_iv$add_rule("SSDSampleSize", sv_gt(1))
      oneSD_iv$add_rule("SSDStdDev", sv_required())
      oneSD_iv$add_rule("SSDStdDev", sv_gt(0))
      oneSDht_iv$add_rule("hypStdDeviation", sv_required())
      oneSDht_iv$add_rule("hypStdDeviation", sv_gt(0))

      ## numSuccessesProportion
      oneprop_iv$add_rule("numSuccesses", sv_required(message = "Numeric value required."))
      oneprop_iv$add_rule("numSuccesses", sv_integer())
      oneprop_iv$add_rule("numSuccesses", sv_gte(0))

      ## x1
      twoprop_iv$add_rule("numSuccesses1", sv_required())
      twoprop_iv$add_rule("numSuccesses1", sv_integer())
      twoprop_iv$add_rule("numSuccesses1", sv_gte(0))
      twopropht_iv$add_rule("numSuccesses1", ~ if (checkTwoProp() == 0) "At least one of (x1) and (x2) must be greater than 0.")

      ## x2
      twoprop_iv$add_rule("numSuccesses2", sv_required())
      twoprop_iv$add_rule("numSuccesses2", sv_integer())
      twoprop_iv$add_rule("numSuccesses2", sv_gte(0))
      twopropht_iv$add_rule("numSuccesses2", ~ if (checkTwoProp() == 0) "At least one of (x1) and (x2) must be greater than 0.")
      twopropht_iv$add_rule("numSuccesses1", ~ {
        if (input$numSuccesses1 == input$numTrials1 &&
            input$numSuccesses2 == input$numTrials2) {
          "Both sample proportions are equal to 1."
        }
      })
      twopropht_iv$add_rule("numSuccesses2", ~ {
        if (input$numSuccesses1 == input$numTrials1 &&
            input$numSuccesses2 == input$numTrials2) {
          "Both sample proportions are equal to 1."
        }
      })

      ## diff naught
      twopropdiffnaught_iv$add_rule("propDiffNaught", sv_required())
      twopropdiffnaught_iv$add_rule("propDiffNaught", sv_gte(-1, message = "Value must be between -1 and 1 (inclusive)."))
      twopropdiffnaught_iv$add_rule("propDiffNaught", sv_lte(1, message = "Value must be between -1 and 1 (inclusive)."))

      ## SDSampleSize1
      twopopvarsum_iv$add_rule("SDSampleSize1", sv_required())
      twopopvarsum_iv$add_rule("SDSampleSize1", sv_integer())
      twopopvarsum_iv$add_rule("SDSampleSize1", sv_gt(1))

      ## SDSampleSize2
      twopopvarsum_iv$add_rule("SDSampleSize2", sv_required())
      twopopvarsum_iv$add_rule("SDSampleSize2", sv_integer())
      twopopvarsum_iv$add_rule("SDSampleSize2", sv_gt(1))

      ## stdDev1
      twopopvarsum_iv$add_rule("stdDev1", sv_required())
      twopopvarsum_iv$add_rule("stdDev1", sv_gt(0))

      ## stdDev2
      twopopvarsum_iv$add_rule("stdDev2", sv_required())
      twopopvarsum_iv$add_rule("stdDev2", sv_gt(0))

      ## Two Std Dev n1
      twopopvar_iv$add_rule("n1", sv_required())
      twopopvar_iv$add_rule("n1", sv_integer())
      twopopvar_iv$add_rule("n1", sv_gt(1))

      ## Two Std Dev n2
      twopopvar_iv$add_rule("n2", sv_required())
      twopopvar_iv$add_rule("n2", sv_integer())
      twopopvar_iv$add_rule("n2", sv_gt(1))

      ## Two Std Dev s1^2
      twopopvar_iv$add_rule("s1sq", sv_required())
      twopopvar_iv$add_rule("s1sq", sv_gt(0))

      ## Two Std Dev s2^2
      twopopvar_iv$add_rule("s2sq", sv_required())
      twopopvar_iv$add_rule("s2sq", sv_gt(0))

      ## raw group 1
      twopopvarraw_iv$add_rule("rawSamp1SD", sv_required())
      twopopvarraw_iv$add_rule("rawSamp1SD", sv_regex(
                                               "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."
                                             ))
      twopopvarraw_iv$add_rule("rawSamp1SD", ~ if (sd(createNumLst(input$rawSamp1SD)) == 0) "No variance in sample data")

      ## raw group 2
      twopopvarraw_iv$add_rule("rawSamp2SD", sv_required())
      twopopvarraw_iv$add_rule("rawSamp2SD", sv_regex(
                                               "( )*^(-)?([0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)(,( )*(-)?[0-9]+(\\.[0-9]+)?)+([ \r\n])*$",
                                               "Data must be at least 3 numeric values separated by a comma (ie: 2,3,4)."
                                             ))
      twopopvarraw_iv$add_rule("rawSamp2SD", ~ if (sd(createNumLst(input$rawSamp2SD)) == 0) "No variance in sample data")


      ## numTrialsProportion
      oneprop_iv$add_rule("numTrials", sv_required(message = "Numeric value required."))
      oneprop_iv$add_rule("numTrials", sv_integer())
      oneprop_iv$add_rule("numTrials", sv_gt(0))

      ## n1
      twoprop_iv$add_rule("numTrials1", sv_required())
      twoprop_iv$add_rule("numTrials1", sv_integer())
      twoprop_iv$add_rule("numTrials1", sv_gt(0))

      ## n2
      twoprop_iv$add_rule("numTrials2", sv_required())
      twoprop_iv$add_rule("numTrials2", sv_integer())
      twoprop_iv$add_rule("numTrials2", sv_gt(0))

      ## hypMean
      onemeanht_iv$add_rule("hypMean", sv_required())

      ## hypProportion
      onepropht_iv$add_rule("hypProportion", sv_required())
      onepropht_iv$add_rule("hypProportion", sv_gt(0))
      onepropht_iv$add_rule("hypProportion", sv_lt(1))

      ## Anova
      anovaupload_iv$add_rule("anovaUserData", sv_required())
      anovaupload_iv$add_rule("anovaUserData", ~ if (is.null(fileInputs$anovaStatus) || fileInputs$anovaStatus == "reset") "Required")
      ## anovaupload_iv$add_rule("anovaUserData", ~ if(!(tolower(tools::file_ext(input$anovaUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
      anovaupload_iv |> add_rule_accepted_file_formats("anovaUserData")
      anovaupload_iv$add_rule("anovaUserData", ~ if (ncol(anovaUploadData()) < 2) "Data must include at least two columns")
      ## anovaupload_iv$add_rule("anovaUserData", ~ if(nrow(anovaUploadData()) < 2) "")

      ## anovamulti_iv$add_rule("anovaMultiColumns", sv_required())
      anovamulti_iv$add_rule("anovaMultiColumns", ~ if (length(input$anovaMultiColumns) < 2) "Select at least two columns")
      anovamulti_iv$add_rule("anovaMultiColumns", ~ {
        if (checkNumeric(anovaUploadData(), input$anovaMultiColumns)) {
          "Selected column(s) contain non-numeric data."
        }
      })

      anovastacked_iv$add_rule("anovaResponse", sv_required())
      anovastacked_iv$add_rule("anovaFactors", sv_required())
      anovastacked_iv$add_rule("anovaResponse", ~ if (anovaStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
      anovastacked_iv$add_rule("anovaFactors", ~ if (anovaStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
      anovastacked_iv$add_rule("anovaResponse", ~ {
        if (checkNumeric(anovaUploadData(), input$anovaResponse)) {
          "Response variable must be numeric."
        }
      })

      ## Kruskal-Wallis
      kwupload_iv$add_rule("kwUserData", sv_required())
      kwupload_iv$add_rule("kwUserData", ~ if (is.null(fileInputs$kwStatus) || fileInputs$kwStatus == "reset") "Required")
      ## kwupload_iv$add_rule("kwUserData", ~ if(!(tolower(tools::file_ext(input$kwUserData$name)) %in% c("csv", "txt", "xls", "xlsx"))) "File format not accepted.")
      kwupload_iv |> add_rule_accepted_file_formats("kwUserData")
      kwupload_iv$add_rule("kwUserData", ~ if (ncol(kwUploadData()) < 2) "Data must include at least two columns")
      kwmulti_iv$add_rule("kwMultiColumns", ~ if (length(input$kwMultiColumns) < 2) "Select at least two columns")
      kwmulti_iv$add_rule("kwMultiColumns", ~ {
        if (checkNumeric(kwUploadData(), input$kwMultiColumns)) {
          "Selected column(s) contain non-numeric data."
        }
      })

      kwstacked_iv$add_rule("kwResponse", sv_required())
      kwstacked_iv$add_rule("kwFactors", sv_required())
      kwstacked_iv$add_rule("kwResponse", ~ if (kwStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
      kwstacked_iv$add_rule("kwFactors", ~ if (kwStackedIsValid() == FALSE) "Response variable and factors column cannot be the same")
      kwstacked_iv$add_rule("kwResponse", ~ {
        if (checkNumeric(kwUploadData(), input$kwResponse)) {
          "Response variable must be numeric."
        }
      })
      ## Chi-Square
      ChiSqInputRules <- function(iv, inputID) {
        iv$add_rule(inputID, sv_required())
        iv$add_rule(inputID, ~ if (any(is.na(chiSqActiveData()$numeric))) "Fields must be positive integers.")
        iv$add_rule(inputID, ~ if (any(chiSqActiveData()$numeric < 0)) "Fields must be positive integers.")
        iv$add_rule(inputID, ~ if (any(chiSqActiveData()$numeric %% 1 != 0)) "Fields must be positive integers.")
        iv$add_rule(inputID, ~ if (all(chiSqActiveData()$numeric == 0)) "All cell values cannot be equal to zero.")
        iv$add_rule(inputID, ~ if (any(chiSqTotaled()[, "Total"] == 0)) "Row Totals must be greater than zero.")
        iv$add_rule(inputID, ~ if (any(chiSqTotaled()["Total", ] == 0)) "Column Totals must be greater than zero.")
      }

      ## 2 x 2
      ChiSqInputRules(chiSq2x2_iv, "chiSqInput2x2")

      ## 2 x 3
      ChiSqInputRules(chiSq2x3_iv, "chiSqInput2x3")

      ## 3 x 2
      ChiSqInputRules(chiSq3x2_iv, "chiSqInput3x2")

      ## 3 x 3
      ChiSqInputRules(chiSq3x3_iv, "chiSqInput3x3")

      ## ------------ Conditions --------------------------------------------------
      onemean_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                    input$popuParameter == "Population Mean" &&
                                    input$dataAvailability == "Summarized Data"))

      onemeansdknown_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                           input$popuParameter == "Population Mean" &&
                                           input$dataAvailability == "Summarized Data" &&
                                           input$sigmaKnown == "Known"))

      onemeansdunk_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                         input$popuParameter == "Population Mean" &&
                                         input$dataAvailability == "Summarized Data" &&
                                         input$sigmaKnown == "Unknown"))

      onemeanraw_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                       input$popuParameter == "Population Mean" &&
                                       input$dataAvailability == "Enter Raw Data"))

      onemeanupload_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                          input$popuParameter == "Population Mean" &&
                                          input$dataAvailability == "Upload Data"))

      onemeanuploadvar_iv$condition(function() {
        isTRUE(input$siMethod == "1" &&
               input$popuParameter == "Population Mean" &&
               input$dataAvailability == "Upload Data" &&
               onemeanupload_iv$is_valid())
      })

      onemeanuploadsd_iv$condition(function() {
        isTRUE(input$siMethod == "1" &&
               input$popuParameter == "Population Mean" &&
               input$dataAvailability == "Upload Data" &&
               input$sigmaKnownUpload == "Known")
      })

      onemeanht_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                      input$popuParameter == "Population Mean" &&
                                      input$inferenceType == "Hypothesis Testing"))

      indmeanssumm_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                         input$popuParameters == "Independent Population Means" &&
                                         input$dataAvailability2 == "Summarized Data"))

      indmeansraw_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                        input$popuParameters == "Independent Population Means" &&
                                        input$dataAvailability2 == "Enter Raw Data"))

      indmeanssdknown_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                            input$popuParameters == "Independent Population Means" &&
                                            input$dataAvailability2 == "Summarized Data" &&
                                            input$bothsigmaKnown == "bothKnown"))

      indmeanssdunk_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                          input$popuParameters == "Independent Population Means" &&
                                          input$dataAvailability2 == "Summarized Data" &&
                                          input$bothsigmaKnown == "bothUnknown"))

      indmeansrawsd_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                          input$popuParameters == "Independent Population Means" &&
                                          input$dataAvailability2 == "Enter Raw Data" &&
                                          input$bothsigmaKnownRaw == "bothKnown"))

      indmeansrawsdunk_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                             input$popuParameters == "Independent Population Means" &&
                                             input$dataAvailability2 == "Enter Raw Data" &&
                                             input$bothsigmaKnownRaw == "bothUnknown" &&
                                             input$inferenceType2 == "Hypothesis Testing" &&
                                             indmeansraw_iv$is_valid()))

      indmeansupload_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                           input$popuParameters == "Independent Population Means" &&
                                           input$dataAvailability2 == "Upload Data"))

      indmeansuploadvar_iv$condition(function() {
        isTRUE(input$siMethod == "2" &&
               input$popuParameters == "Independent Population Means" &&
               input$dataAvailability2 == "Upload Data" &&
               indmeansupload_iv$is_valid())
      })

      indmeansuploadsd_iv$condition(function() {
        isTRUE(input$siMethod == "2" &&
               input$popuParameters == "Independent Population Means" &&
               input$dataAvailability2 == "Upload Data" &&
               input$bothsigmaKnownUpload == "bothKnown")
      })


      wilcoxonraw_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                        input$popuParameters == "Wilcoxon rank sum test" &&
                                        input$wilcoxonRankSumTestData == "Enter Raw Data"))

      wilcoxonUpload_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                           input$popuParameters == "Wilcoxon rank sum test" &&
                                           input$wilcoxonRankSumTestData == "Upload Data"))

      wilcoxonRanksuploadvars_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                                    input$popuParameters == "Wilcoxon rank sum test" &&
                                                    input$wilcoxonRankSumTestData == "Upload Data" &&
                                                    wilcoxonUpload_iv$is_valid()))

      wRankSumrawsd_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                          input$popuParameters == "Wilcoxon rank sum test" &&
                                          input$wilcoxonRankSumTestData == "Enter Raw Data" &&
                                          wilcoxonraw_iv$is_valid()))

      signedRankRaw_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                          input$popuParameters == "Wilcoxon Signed Rank Test" &&
                                          input$signedRankTest == "Enter Raw Data"))

      signedRankUpload_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                             input$popuParameters == "Wilcoxon Signed Rank Test" &&
                                             input$signedRankTest == "Upload Data"))

      signedRankUploadvars_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                                 input$popuParameters == "Wilcoxon Signed Rank Test" &&
                                                 input$signedRankTest == "Upload Data" &&
                                                 signedRankUpload_iv$is_valid()))

      signedRankrawsd_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                            input$popuParameters == "Wilcoxon Signed Rank Test" &&
                                            input$signedRankTest == "Enter Raw Data" &&
                                            signedRankRaw_iv$is_valid()))


      indmeansmunaught_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                             input$popuParameters == "Independent Population Means" &&
                                             input$inferenceType2 == "Hypothesis Testing"))

      depmeansraw_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                        input$popuParameters == "Dependent Population Means" &&
                                        input$dataTypeDependent == "Enter Raw Data"))

      depmeansupload_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                           input$popuParameters == "Dependent Population Means" &&
                                           input$dataTypeDependent == "Upload Data"))

      depmeansuploadvars_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                               input$popuParameters == "Dependent Population Means" &&
                                               input$dataTypeDependent == "Upload Data" &&
                                               depmeansupload_iv$is_valid()))

      depmeansrawsd_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                          input$popuParameters == "Dependent Population Means" &&
                                          input$dataTypeDependent == "Enter Raw Data" &&
                                          depmeansraw_iv$is_valid()))

      depmeansmunaught_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                             input$popuParameters == "Dependent Population Means" &&
                                             input$inferenceType2 == "Hypothesis Testing"))

      oneSD_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                  input$popuParameter == "Population Standard Deviation"))

      ## See: https://rstudio.github.io/shinyvalidate/reference/InputValidator.html#method-InputValidator-condition.
      oneSDht_iv$condition(function() {
        return(all(
          input$siMethod == "1",
          input$popuParameter == "Population Standard Deviation",
          input$inferenceType == "Hypothesis Testing"
        ))
      })

      oneprop_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                    input$popuParameter == "Population Proportion"))

      onepropht_iv$condition(~ isTRUE(input$siMethod == "1" &&
                                      input$popuParameter == "Population Proportion" &&
                                      input$inferenceType == "Hypothesis Testing"))

      twoprop_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                    input$popuParameters == "Population Proportions"))

      twopropht_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                      input$popuParameters == "Population Proportions" &&
                                      input$inferenceType2 == "Hypothesis Testing"))

      twopopvarsum_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                         input$popuParameters == "Two Population Variances" &&
                                         input$dataAvailability3 == "Summary"))

      twopopvar_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                      input$popuParameters == "Two Population Variances" &&
                                      input$dataAvailability3 == "Variance"))

      twopopvarraw_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                         input$popuParameters == "Two Population Variances" &&
                                         input$dataAvailability3 == "Enter Raw Data"))

      twopropdiffnaught_iv$condition(~ isTRUE(input$siMethod == "2" &&
                                              input$popuParameters == "Population Proportions" &&
                                              input$inferenceType2 == "Hypothesis Testing"))

      kwupload_iv$condition(~ isTRUE(input$siMethod == "Multiple" &&
                                     input$multipleMethodChoice == "kw"))

      kwmulti_iv$condition(~ isTRUE(input$siMethod == "Multiple" &&
                                    input$kwFormat == "Multiple" &&
                                    input$multipleMethodChoice == "kw" &&
                                    kwupload_iv$is_valid()))

      kwstacked_iv$condition(~ isTRUE(input$siMethod == "Multiple" &&
                                      input$kwFormat == "Stacked" &&
                                      input$multipleMethodChoice == "kw" &&
                                      kwupload_iv$is_valid()))

      anovaupload_iv$condition(~ isTRUE(input$siMethod == "Multiple" &&
                                        input$multipleMethodChoice == "anova"))

      anovamulti_iv$condition(~ isTRUE(input$siMethod == "Multiple" &&
                                       input$anovaFormat == "Multiple" &&
                                       input$multipleMethodChoice == "anova" &&
                                       anovaupload_iv$is_valid()))

      anovastacked_iv$condition(~ isTRUE(input$siMethod == "Multiple" &&
                                         input$anovaFormat == "Stacked" &&
                                         input$multipleMethodChoice == "anova" &&
                                         anovaupload_iv$is_valid()))

      chiSq2x2_iv$condition(~ isTRUE(input$siMethod == "Categorical" &&
                                     input$chisquareDimension == "2 x 2"))

      chiSq2x3_iv$condition(~ isTRUE(input$siMethod == "Categorical" &&
                                     input$chisquareDimension == "2 x 3"))

      chiSq3x2_iv$condition(~ isTRUE(input$siMethod == "Categorical" &&
                                     input$chisquareDimension == "3 x 2"))

      chiSq3x3_iv$condition(~ isTRUE(input$siMethod == "Categorical" &&
                                     input$chisquareDimension == "3 x 3"))

      ## ------------ Dependencies ------------------------------------------------
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
      si_iv$add_validator(wilcoxonUpload_iv)
      si_iv$add_validator(wilcoxonraw_iv)
      si_iv$add_validator(wilcoxonRanksuploadvars_iv)
      si_iv$add_validator(signedRankUpload_iv)
      si_iv$add_validator(signedRankRaw_iv)
      si_iv$add_validator(signedRankUploadvars_iv)
      si_iv$add_validator(indmeansmunaught_iv)
      si_iv$add_validator(depmeansraw_iv)
      si_iv$add_validator(depmeansupload_iv)
      si_iv$add_validator(depmeansuploadvars_iv)
      si_iv$add_validator(depmeansmunaught_iv)
      si_iv$add_validator(oneSD_iv)
      si_iv$add_validator(oneSDht_iv)
      si_iv$add_validator(oneprop_iv)
      si_iv$add_validator(onepropht_iv)
      si_iv$add_validator(twoprop_iv)
      si_iv$add_validator(twopropht_iv)
      si_iv$add_validator(twopropdiffnaught_iv)
      si_iv$add_validator(twopopvarsum_iv)
      si_iv$add_validator(twopopvar_iv)
      si_iv$add_validator(twopopvarraw_iv)
      twoprop_iv$add_validator(twopropht_iv)
      si_iv$add_validator(kwupload_iv)
      si_iv$add_validator(kwmulti_iv)
      si_iv$add_validator(kwstacked_iv)
      si_iv$add_validator(anovaupload_iv)
      si_iv$add_validator(anovamulti_iv)
      si_iv$add_validator(anovastacked_iv)
      si_iv$add_validator(chiSq2x2_iv)
      si_iv$add_validator(chiSq2x3_iv)
      si_iv$add_validator(chiSq3x2_iv)
      si_iv$add_validator(chiSq3x3_iv)

      ## ------------ Activation --------------------------------------------------

      ## FIXME: If this validator object has been added to another validator
      ## object using InputValidator$add_validator, calls to enable() on this
      ## validator will be ignored. Don't rely on this behavior, if undefined, or
      ## if out-of-sync with the documentation. When child validators exist they
      ## are enabled or disabled recursively when parent is enabled or disabled,
      ## and actions on the child should be ignored.
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
      wilcoxonraw_iv$enable()
      wilcoxonUpload_iv$enable()
      indmeansmunaught_iv$enable()
      depmeansraw_iv$enable
      depmeansupload_iv$enable()
      depmeansuploadvars_iv$enable()
      depmeansrawsd_iv$enable()
      depmeansmunaught_iv$enable()
      signedRankRaw_iv$enable()
      signedRankUpload_iv$enable()
      ## signedRankUploadvars_iv$enable()
      oneSD_iv$enable()
      oneSDht_iv$enable()
      oneprop_iv$enable()
      onepropht_iv$enable()
      twoprop_iv$enable()
      twopropht_iv$enable()
      twopropdiffnaught_iv$enable()
      twopopvarsum_iv$enable()
      twopopvar_iv$enable()
      twopopvarraw_iv$enable()
      kwupload_iv$enable()
      kwmulti_iv$enable()
      kwstacked_iv$enable()
      anovaupload_iv$enable()
      anovamulti_iv$enable()
      anovastacked_iv$enable()
      chiSq2x2_iv$enable()
      chiSq2x3_iv$enable()
      chiSq3x2_iv$enable()
      chiSq3x3_iv$enable()
    }

    {# plot options menu servers
      plotOptionsMenuServer("oneMeanBoxplot")
      plotOptionsMenuServer("indMeansBoxplot")
      plotOptionsMenuServer("indMeansQQPlot")
      plotOptionsMenuServer("depMeansQQPlot")
      plotOptionsMenuServer("sidebysidewRankSum")
      plotOptionsMenuServer("anovaBoxplot")
      plotOptionsMenuServer("anovaHistogram")
      plotOptionsMenuServer("anovaQQplot")
      plotOptionsMenuServer("anovaMeanPlot")
    }

    ConfLvl <- reactive({
      if (input$siMethod == "1") {
        if (input$confidenceLevel == "90%") {
          confLvl <- 0.9
        } else if (input$confidenceLevel == "95%") {
          confLvl <- 0.95
        } else {
          confLvl <- 0.99
        }
      } else if (input$siMethod == "2") {
        if (input$confidenceLevel2 == "90%") {
          confLvl <- 0.9
        } else if (input$confidenceLevel2 == "95%") {
          confLvl <- 0.95
        } else {
          confLvl <- 0.99
        }
      } else {
        confLvl <- 0
      }

      ## B.C. NOTE: all of the above could be replaced with the following.
      ## req(input$siMethod != NULL)
      ## if (input$siMethod %in% c(1, 2))
      ##   confLvl <- switch(input$confidenceLevel,
      ##                     "90%" = 0.90,
      ##                     "95%" = 0.95,
      ##                     0.99)
      ## else
      ##   confLvl <- 0

      return(confLvl)
    })

    SigLvl <- reactive({
      if (input$siMethod == "1") {
        if (input$significanceLevel == "10%") {
          sigLvl <- 0.10
        } else if (input$significanceLevel == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }
      } else if (input$siMethod == "2") {
        if (input$significanceLevel2 == "10%") {
          sigLvl <- 0.10
        } else if (input$significanceLevel2 == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }
      } else {
        sigLvl <- 0.05
      }

      return(sigLvl)
    })

    ## TODO: All submodule servers must be started herein.
    ## TODO: All submodules must be revealed appropriately.
  })
}

## output$inferenceValidation <- renderUI({
##   if (!onemean_iv$is_valid()) {
##     validate(
##       need(input$sampleSize, "Sample size (n) must be an integer greater than 1.") %||%
##       need(input$sampleSize > 1 & input$sampleSize %% 1 == 0, "Sample size (n) must be an integer greater than 1."),
##       need(input$sampleMean, "Sample mean required."),
##       errorClass = "myClass"
##     )
##   }

##   if (!onemeanraw_iv$is_valid()) {
##     validate(
##       need(input$sample1, "Sample Data required.") %||%
##       need(length(createNumLst(input$sample1)) > 1, "Sample Data requires a minimum of 2 data points."),
##       if (input$sigmaKnownRaw == "rawKnown") {
##         need(input$popuSDRaw, "Population Standard Deviation is required.") %||%
##           need(input$popuSDRaw > 0, "Population Standard Deviation must be positive.")
##       },
##       errorClass = "myClass"
##     )

##     if (input$sigmaKnownRaw == "rawUnknown") {
##       sampleData <- createNumLst(input$sample1)
##       validate(
##         need(
##           sd(sampleData, na.rm = TRUE) != 0,
##           "When the sample standard deviation is 0, the test statistic (t) is undefined."
##         ),
##         errorClass = "myClass"
##       )
##     }
##   }

##   if (!onemeansdknown_iv$is_valid()) {
##     validate(
##       need(input$popuSD & input$popuSD > 0, "Population Standard Deviation must be positive."),
##                                         # need(input$popuSD > 0, "Population Standard Deviation must be greater than 0"),
##       errorClass = "myClass"
##     )
##   }

##   if (!onemeansdunk_iv$is_valid()) {
##     validate(
##       need(input$sampSD && input$sampSD > 0, "Sample Standard Deviation (s) must be positive."),
##       errorClass = "myClass"
##     )
##   }

##   if (!onemeanuploadvar_iv$is_valid()) {
##     validate(
##       need(input$oneMeanVariable != "", "Please select a column for analysis."),
##       errorClass = "myClass"
##     )
##     sampleData <- na.omit(unlist(Upload()[, input$oneMeanVariable]))
##     validate(
##       need(is.numeric(sampleData), "Selected column must be numeric."),
##       errorClass = "myClass"
##     )
##     validate(
##       need(length(sampleData) > 1, "Samples must include at least 2 observations."),
##       errorClass = "myClass"
##     )
##     validate(
##       need(
##         sd(sampleData, na.rm = TRUE) != 0,
##         "When the sample standard deviation is 0, the test statistic (t) is undefined."
##       ),
##       errorClass = "myClass"
##     )
##   }


##   if (!onemeanuploadsd_iv$is_valid()) {
##     validate(
##       need(input$popuSDUpload && input$popuSDUpload > 0, "Population Standard Deviation must be positive."),
##       errorClass = "myClass"
##     )
##   }

##   if (!onemeanht_iv$is_valid()) {
##     validate(
##       need(input$hypMean, "Hypothesized value of the Population Mean is required."),
##       errorClass = "myClass"
##     )
##   }

##   ## ---------------- One Standard Deviation Validation
##   if (!oneSD_iv$is_valid()) {
##     validate(
##       need(input$SSDSampleSize, "Sample size (n) is required.") %||%
##       need(input$SSDSampleSize > 1 & input$SSDSampleSize %% 1 == 0, "Sample size (n) must be an integer greater than 1."),
##       errorClass = "myClass"
##     )
##   }

##   if (!oneSD_iv$is_valid()) {
##     validate(
##       need(input$SSDStdDev, "Sample Standard Deviation (s) is required.") %||%
##       need(input$SSDStdDev > 0, "Sample Standard Deviation (s) must be positive."),
##       errorClass = "myClass"
##     )
##   }

##   ## DONE: these messages are for debugging purposes only.
##   ## message(sprintf("Should oneSDht_iv be testing? %s", oneSDht_iv$condition()()))
##   if (!oneSDht_iv$is_valid()) {
##     ## message("The one sample standard deviation hypothesis test InputValidator object is invalid!")
##     validate(
##       need(input$hypStdDeviation, "Hypothesized Population Standard Deviation (\u03C3\u2080) is required.") %||%
##       need(input$hypStdDeviation > 0 && input$hypStdDeviation < 1, "Hypothesized Population Standard Deviation (\u03C3\u2080) must be positive. (\u03C3\u2080 > 0)."),
##       errorClass = "myClass"
##     )
##   }

##   ## ---------------- One Prop Validation
##   if (!oneprop_iv$is_valid()) {
##     validate(
##       need(input$numSuccesses, "Numeric value for Number of Successes (x) required"),
##       need(input$numTrials, "Numeric value for Number of Trials (n) required"),
##       errorClass = "myClass"
##     )

##     validate(
##       need(input$numSuccesses %% 1 == 0, "Number of Successes (x) must be an integer"),
##       need(input$numSuccesses >= 0, "Number of Successes (x) cannot be negative"),
##       need(input$numTrials %% 1 == 0, "Number of Trials (n) must be an integer"),
##       need(input$numTrials > 0, "Number of Trials (n) must be greater than 0") %||%
##       need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),
##       errorClass = "myClass"
##     )
##   } else if (input$siMethod == "1" && input$popuParameter == "Population Proportion") {
##     req(input$numSuccesses >= 0 && input$numTrials)
##     validate(
##       need(input$numSuccesses <= input$numTrials, "Number of Successes (x) cannot be greater than Number of Trials (n)"),
##       errorClass = "myClass"
##     )
##   }

##   if (!onepropht_iv$is_valid()) {
##     validate(
##       need(input$hypProportion, "Hypothesized value of the Population Proportion must be between 0 and 1") %||%
##       need(input$hypProportion > 0 && input$hypProportion < 1, "Hypothesized value of the Population Proportion must be between 0 and 1"),
##       errorClass = "myClass"
##     )
##   }
## })
