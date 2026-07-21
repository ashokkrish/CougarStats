statInfrMethodMultipleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## ------------ Kruskal-Wallis Outputs ------------------------------------------
    output$kwHT <- kruskalWallisHT(kwResults, reactive({
      input$kwSigLvl
    }))
    output$kwUploadTable <- kruskalWallisUpload(kwUploadData, reactive({
      kwupload_iv$is_valid()
    }))
    output$kwInitialUploadTable <- kruskalWallisUploadInitial(kwUploadData)
    output$renderKWRM <- kwRankedTableOutput(kwResults()$data)
    output$kruskalWallisPlot <- kruskalWallisPlot(kwResults, reactive({
      input$kwSigLvl
    }))
    output$kwConclusionOutput <- kwConclusion(kwResults, reactive({
      input$kwSigLvl
    }))
    output$debug_kw_state <- renderText({
      paste(
        "multipleMethodChoice:", input$multipleMethodChoice, "|",
        "kwUserData exists:", !is.null(input$kwUserData), "|",
        "kwUserData name:", if (!is.null(input$kwUserData)) input$kwUserData$name else "NULL", "|",
        "siMethod:", input$siMethod
      )
    })


    ## ------------ ANOVA Reactives ---------------------------------------------
    anovaUploadData <- createFileInputEventReactive(input, "anovaUserData")

    anovaStackedIsValid <- eventReactive(
    {
      input$anovaResponse
      input$anovaFactors
    },
    {
      valid <- TRUE

      if (!is.null(input$anovaResponse) && !is.null(input$anovaFactors)) {
        if (input$anovaResponse == input$anovaFactors) {
          valid <- FALSE
        }
      }

      return(valid)
    }
    )

    anovaOneWayResults <- reactive({
      req(si_iv$is_valid)

      results <- list()

      if (input$anovaFormat == "Multiple") {
        anovaData <- stack(anovaUploadData()[, input$anovaMultiColumns])
        factorCol <- "ind"
        factorNames <- levels(anovaData[, factorCol])
      } else {
        anovaData <- anovaUploadData()
        colnames(anovaData)[colnames(anovaData) == input$anovaFactors] <- "ind"
        colnames(anovaData)[colnames(anovaData) == input$anovaResponse] <- "values"
        anovaData <- anovaData %>% dplyr::mutate(ind = factor(ind))
        factorCol <- "ind"
        factorNames <- levels(anovaData$ind)
      }

      anovaData <- na.omit(anovaData)
      totalCount <- nrow(anovaData)
      numFactors <- length(factorNames)
      anovaTest <- aov(formula = values ~ ind, data = anovaData)

      results$data <- anovaData
      results$count <- totalCount
      results$factorCol <- factorCol
      results$numFactors <- numFactors
      results$factorNames <- factorNames
      results$fit <- anovaTest
      results$residuals <- anovaTest$residuals
      results$test <- anova(anovaTest)

      return(results)
    })

    ## ------------ Kruskal-Wallis Reactives ------------------------------------
    kwUploadData <- eventReactive(input$kwUserData, {
      kwUploadData_func(input$kwUserData)
    })

    kwStackedIsValid <- eventReactive(
      list(input$kwResponse, input$kwFactors),
      {
        kwStackedIsValid_func(input$kwResponse, input$kwFactors)
      }
    )

    kwResults <- reactive({
      req(input$siMethod == "Multiple" && input$multipleMethodChoice == "kw")
      req(si_iv$is_valid())

      kwResults_func(
        input$kwFormat,
        input$kwMultiColumns,
        kwUploadData(),
        input$kwFactors,
        input$kwResponse
      )
    })

    ## ---------------- ANOVA Validation
      if (!anovaupload_iv$is_valid()) {
        if (is.null(input$anovaUserData)) {
          validate("Please upload a file.")
        }

        validate(
          need(!is.null(fileInputs$anovaStatus) && fileInputs$anovaStatus == "uploaded", "Please upload a file."),
          errorClass = "myClass"
        )

        validate(
          need(nrow(anovaUploadData()) > 0, "File is empty."),
          need(ncol(anovaUploadData()) >= 2, "File must contain at least 2 distinct columns of data to choose from for analysis."),
          errorClass = "myClass"
        )
      }

      if (!anovamulti_iv$is_valid()) {
        validate(
          need(length(input$anovaMultiColumns) >= 2, "Please select two or more columns to conduct analysis."),
          errorClass = "myClass"
        )

        validate(
          need(
            !checkNumeric(anovaUploadData(), input$anovaMultiColumns),
            "Selected columns must be numeric."
          ),
          errorClass = "myClass"
        )
      }

      if (!anovastacked_iv$is_valid()) {
        validate(
          need(!is.null(input$anovaResponse) && input$anovaResponse != "", "Please select a Response Variable."),
          need(!is.null(input$anovaFactors) && input$anovaFactors != "", "Please select a Factors column."),
          errorClass = "myClass"
        )

        validate(
          need(anovaStackedIsValid() == TRUE, "Please select distinct columns for the Response Variable and Factors."),
          errorClass = "myClass"
        )

        validate(
          need(
            !checkNumeric(anovaUploadData(), input$anovaResponse),
            "Response variable must be numeric."
          ),
          errorClass = "myClass"
        )
      }

      ## ---------------- Kruskal-Wallis Validation
      if (!kwupload_iv$is_valid()) {
        if (is.null(input$kwUserData)) {
          validate("Please upload a file.")
        }

        validate(
          need(!is.null(fileInputs$kwStatus) && fileInputs$kwStatus == "uploaded", "Please upload a file."),
          errorClass = "myClass"
        )

        validate(
          need(nrow(kwUploadData()) > 0, "File is empty."),
          need(ncol(kwUploadData()) >= 2, "File must contain at least 2 distinct columns of data to choose from for analysis."),
          errorClass = "myClass"
        )
      }

      if (!kwmulti_iv$is_valid()) {
        validate(
          need(length(input$kwMultiColumns) >= 2, "Please select two or more columns to conduct analysis."),
          errorClass = "myClass"
        )

        validate(
          need(
            !checkNumeric(kwUploadData(), input$kwMultiColumns),
            "Selected columns must be numeric."
          ),
          errorClass = "myClass"
        )
      }

      if (!kwstacked_iv$is_valid()) {
        validate(
          need(!is.null(input$kwResponse) && input$kwResponse != "", "Please select a Response Variable."),
          need(!is.null(input$kwFactors) && input$kwFactors != "", "Please select a Factors column."),
          errorClass = "myClass"
        )

        validate(
          need(kwStackedIsValid() == TRUE, "Please select distinct columns for Response Variable and Factors."),
          errorClass = "myClass"
        )
        validate(
          need(
            !checkNumeric(kwUploadData(), input$kwResponse),
            "Response variable must be numeric."
          ),
          errorClass = "myClass"
        )
      }

    ## ------------ ANOVA Outputs -----------------------------------------------
    output$anovaOutput <- renderUI({
      req(si_iv$is_valid())
      PrintANOVA()
    })

    ## ---------------- Factor Table ----
    output$oneWayFactorTable <- renderDT({
      req(si_iv$is_valid())
      PrintANOVAFactorTable()
    })

    ## ---------------- ANOVA Table ----
    output$oneWayAnovaTable <- renderDT({
      req(si_iv$is_valid())
      PrintANOVATable()
    })

    ## output$oneWayAnovaTableHTML <- renderUI({
    ##   req(si_iv$is_valid())
    ##   PrintANOVATableHTML()
    ## })

    ## ---------------- HT Plot ----
    output$oneWayAnovaPlot <- renderPlot({
      req(si_iv$is_valid())

      data <- anovaOneWayResults()$test
      anovaF <- round(data[1, "F value"], 4)

      if (input$anovaSigLvl == "10%") {
        sigLvl <- 0.1
      } else if (input$anovaSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }

      cv <- round(qf(1 - sigLvl, df1 = data[1, "Df"], df2 = data[2, "Df"]), 4)

      xSeq <- c(seq(0, 15, length.out = 75), cv, anovaF)
      rrLabel <- c((cv + max(xSeq)) / 2)
      x_vector <- sort(c(xSeq, rrLabel))
      p_vector <- df(x_vector, df1 = data[1, "Df"], df2 = data[2, "Df"])

      anova_dataframe <- distinct(data.frame(x = x_vector, y = p_vector))
      cv_dataframe <- filter(anova_dataframe, x %in% cv)
      ts_dataframe <- filter(anova_dataframe, x %in% anovaF)
      rrLabelDF <- filter(anova_dataframe, x %in% rrLabel)
      arLabelDF <- filter(anova_dataframe, y %in% max(p_vector))

      ggplot(
        anova_dataframe,
        aes(x = x, y = y)
      ) +
        stat_function(
          fun = df,
          args = list(df1 = data[1, "Df"], df2 = data[2, "Df"]),
          geom = "Density",
          fill = NA
        ) +
        shadeHtArea(anova_dataframe, cv, "greater") +
        geom_segment(
          data = filter(anova_dataframe, y %in% max(p_vector)),
          aes(x = 0, xend = 0, y = 0, yend = y, alpha = 0.5),
          linetype = "solid",
          linewidth = 0.75,
          color = "black",
          show.legend = FALSE
        ) +
        geom_text(
          data = filter(anova_dataframe, x %in% c(0)),
          aes(x = x, y = 0, label = "0"),
          size = 14 / .pt,
          fontface = "bold",
          nudge_y = -.03,
          check_overlap = TRUE
        ) +
        geom_segment(
          data = cv_dataframe,
          aes(x = x, xend = x, y = 0, yend = y),
          linetype = "solid",
          lineend = "butt",
          linewidth = 1.5,
          color = "#023B70"
        ) +
        geom_text(
          data = cv_dataframe,
          aes(x = x, y = 0, label = x),
          size = 14 / .pt,
          fontface = "bold",
          nudge_y = -.03,
          check_overlap = TRUE
        ) +
        geom_segment(
          data = ts_dataframe,
          aes(x = x, xend = x, y = 0, yend = y + .055),
          linetype = "solid",
          linewidth = 1.25,
          color = "#BD130B"
        ) +
        geom_text(
          data = ts_dataframe,
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
        xlab("F") +
        scale_y_continuous(breaks = NULL) +
        theme(axis.title.x = element_text(
                size = 20,
                family = "serif",
                face = "bold.italic"
              ))
    })

    ## ---------------- Post hoc analysis ----
    output$anovaPosthocAnalysis <- renderUI({
      if (input$anovaSigLvl == "10%") {
        sigLvl <- 0.1
      } else if (input$anovaSigLvl == "5%") {
        sigLvl <- 0.05
      } else {
        sigLvl <- 0.01
      }

      numComparisons <- anovaOneWayResults()$numFactors * (anovaOneWayResults()$numFactors - 1) / 2

      tagList(
        withMathJax(),
        titlePanel("Pairwise Comparisons"),
        hr(),
        br(),
        sprintf("The total number of independent pairwise comparisons is"),
        sprintf("\\( m = k(k-1)/2 \\), where \\(k\\) is the number of factors."),
        br(),
        sprintf(
          "In this exercise,  \\(m = %s(%s-1)/2 = %s.\\)",
          anovaOneWayResults()$numFactors,
          anovaOneWayResults()$numFactors,
          numComparisons
        ),
        br(),
        br(),
        p(tags$b("Bonferroni adjusted p-values using t tests with pooled SD")),
        DTOutput(session$ns("anovaBonfTable")),
        br(),
        sprintf("Note: The simple Bonferroni correction rejects only null hypotheses
              with a p-value less than"),
        sprintf(
          "\\( \\alpha^{*} = \\alpha / m = %s / %s = %s \\)",
          sigLvl,
          numComparisons,
          round(sigLvl / numComparisons, 4)
        ),
        br()
      )
    })

    ## ---------------- Bonf Table ----
    output$anovaBonfTable <- renderDT({
      data <- anovaOneWayResults()$data

      bonf_df <- as.data.frame(pairwise.t.test(data$values, data$ind, p.adjust.method = "bonf")$p.value)
      bonf_df <- mutate_if(bonf_df, is.numeric, round, digits = 4)
      bonf_df[bonf_df == 0] <- "< 0.0001"

      headers <- htmltools::withTags(table(
                              class = "display",
                              thead(
                                tr(
                                  th("",
                                     style = "border: 1px solid rgba(0, 0, 0, 0.15);
                        border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"
                        ),
                        lapply(colnames(bonf_df), th,
                               style = "border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);"
                          )
                        )
                        )
                        ))

      datatable(bonf_df,
                class = "cell-border stripe",
                container = headers,
                options = list(
                  dom = "t",
                  pageLength = -1,
                  ordering = FALSE,
                  searching = FALSE,
                  paging = FALSE,
                  autoWidth = TRUE,
                  scrollX = TRUE
                ),
                selection = "none",
                escape = FALSE,
                filter = "none"
                ) %>% formatStyle(
                        columns = c(0),
                        fontWeight = "bold"
                      )
    })

    ## ----------------- Boxplot ----
    output$anovaBoxplot <- renderPlot(
    {
      req(si_iv$is_valid())
      data <- anovaOneWayResults()$data

      df_boxplot <- data.frame(
        sample = c(data[, "ind"]),
        data = c(data[, "values"])
      )
      colnames(df_boxplot) <- c("sample", "data")

      RenderSideBySideBoxplot(
        df_boxplot[, "data"],
        df_boxplot,
        input[["anovaBoxplot-Colour"]],
        input[["anovaBoxplot-Title"]],
        input[["anovaBoxplot-Xlab"]],
        input[["anovaBoxplot-Ylab"]],
        input[["anovaBoxplot-BoxWidth"]] / 10,
        input[["anovaBoxplot-Gridlines"]],
        input[["anovaBoxplot-Flip"]],
        input[["anovaBoxplot-OutlierLabels"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["anovaBoxplot-Height"]], input[["anovaBoxplot-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["anovaBoxplot-Width"]], input[["anovaBoxplot-WidthPx"]], ui = FALSE)
    }
    )

    ## ---------------- Histogram of Residuals ----
    output$anovaHistogram <- renderPlot(
    {
      req(si_iv$is_valid())
      data <- anovaOneWayResults()$residuals

      RenderHistogram(
        data,
        input[["anovaHistogram-Colour"]],
        input[["anovaHistogram-Title"]],
        input[["anovaHistogram-Xlab"]],
        input[["anovaHistogram-Ylab"]],
        input[["anovaHistogram-Gridlines"]],
        input[["anovaHistogram-Flip"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["anovaHistogram-Height"]], input[["anovaHistogram-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["anovaHistogram-Width"]], input[["anovaHistogram-WidthPx"]], ui = FALSE)
    }
    )

    ## ---------------- QQ Plot of Residuals ----
    output$anovaQQplot <- renderPlot(
    {
      req(si_iv$is_valid())
      data <- anovaOneWayResults()$residuals

      qqplot_df <- data.frame(values = data)

      RenderQQPlot(
        qqplot_df,
        input[["anovaQQplot-Colour"]],
        input[["anovaQQplot-Title"]],
        input[["anovaQQplot-Xlab"]],
        input[["anovaQQplot-Ylab"]],
        input[["anovaQQplot-Gridlines"]],
        input[["anovaQQplot-Flip"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["anovaQQplot-Height"]], input[["anovaQQplot-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["anovaQQplot-Width"]], input[["anovaQQplot-WidthPx"]], ui = FALSE)
    }
    )

    ## ---------------- Group Means Plot ----
    output$anovaMeanPlot <- renderPlot(
    {
      req(si_iv$is_valid())
      data <- as.data.frame(anovaOneWayResults()$data)
      groups <- anovaOneWayResults()$factornames

      RenderMeanPlot(
        data,
        groups,
        input[["anovaMeanPlot-Colour"]],
        input[["anovaMeanPlot-Title"]],
        input[["anovaMeanPlot-Xlab"]],
        input[["anovaMeanPlot-Ylab"]],
        input[["anovaMeanPlot-Gridlines"]],
        input[["anovaMeanPlot-Flip"]]
      )
    },
    height = function() {
      GetPlotHeight(input[["anovaMeanPlot-Height"]], input[["anovaMeanPlot-HeightPx"]], ui = FALSE)
    },
    width = function() {
      GetPlotWidth(input[["anovaMeanPlot-Width"]], input[["anovaMeanPlot-WidthPx"]], ui = FALSE)
    }
    )

    ## ---------------- Uploaded Data Table ----
    output$anovaUploadTable <- renderDT({
      req(anovaupload_iv$is_valid())
      datatable(anovaUploadData(),
                options = list(
                  pageLength = -1,
                  lengthMenu = list(
                    c(25, 50, 100, -1),
                    c("25", "50", "100", "all")
                  ),
                  columnDefs = list(list(
                    className = "dt-center",
                    targets = 0:ncol(anovaUploadData())
                  ))
                )
                )
    })

    observeEvent(input$resetInference, {
      reset("inputPanel")

      ## Kruskal-Wallis inputs.
      list(c(input = "kwMultiColumns", placeholder = "Select two or more columns"),
           c(input = "kwResponse", placeholder = "Select a variable"),
           c(input = "kwFactors", placeholder = "Select a factor")) |>
        lapply(function(args) {
          updateSelectizeInput(
            session,
            args$input,
            selected = "",
            choices = c(""),
            options = list(placeholder = args$placeholder)
          )
        })
    })

    observeEvent(input$goInference, {
      output$renderAnovaDataView <- renderUI({
        tagList(
          div(DTOutput(session$ns("anovaUploadTable")), style = "width: 75%")
        )
      })

      output$renderAnovaBoxplot <- renderUI({
        tagList(
          plotOutput(session$ns("anovaBoxplot"),
                     height = GetPlotHeight(input[["anovaBoxplot-Height"]], input[["anovaBoxplot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaBoxplot-Width"]], input[["anovaBoxplot-WidthPx"]], ui = TRUE)
                     ),
          br(),
          br(),
          hr()
        )
      })

      output$renderAnovaHistogram <- renderUI({
        tagList(
          plotOutput(session$ns("anovaHistogram"),
                     height = GetPlotHeight(input[["anovaBoxplot-Height"]], input[["anovaBoxplot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaBoxplot-Width"]], input[["anovaBoxplot-WidthPx"]], ui = TRUE)
                     ),
          br(),
          br(),
          hr()
        )
      })

      output$renderAnovaQQplot <- renderUI({
        tagList(
          plotOutput(session$ns("anovaQQplot"),
                     height = GetPlotHeight(input[["anovaQQplot-Height"]], input[["anovaQQplot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaQQplot-Width"]], input[["anovaQQplot-WidthPx"]], ui = TRUE)
                     ),
          br(),
          br(),
          hr()
        )
      })

      output$renderAnovaMeanPlot <- renderUI({
        tagList(
          plotOutput(session$ns("anovaMeanPlot"),
                     height = GetPlotHeight(input[["anovaMeanPlot-Height"]], input[["anovaMeanPlot-HeightPx"]], ui = TRUE),
                     width = GetPlotWidth(input[["anovaMeanPlot-Width"]], input[["anovaMeanPlot-WidthPx"]], ui = TRUE)
                     ),
          br(),
          br(),
          hr()
        )
      })

      output$renderChiSqObs <- renderUI({
        DTOutput(session$ns("chiSqObs"), width = "500px")
      })

      output$renderChiSqResults <- renderUI({
        if (input$chiSquareYates) {
          DTOutput(session$ns("chiSqResultsMatrix"), width = "850px")
        } else {
          DTOutput(session$ns("chiSqResultsMatrix"), width = "750px")
        }
      })

      output$renderFishersObs <- renderUI({
        DTOutput(session$ns("fishersObs"), width = "500px")
      })
    })

    observe({
      req(kwResults())

      results <- NULL
      tryCatch(
      {
        results <- kwResults()
      },
      error = function(e) {
        cat("Error in kwResults():", e$message, "\n")
        return(NULL)
      }
      )

      if (is.null(results)) {
        output$analysisContent <- renderUI({
          tagList(
            p("Unable to calculate results. Please check your data and selections.",
              style = "color: red; font-weight: bold; font-size: 16px;"
              )
          )
        })
        return()
      }

      if (!is.null(results$validation_error)) {
        output$analysisContent <- renderUI({
          tagList(
            p(results$validation_error, style = "color: red; font-weight: bold; font-size: 16px;")
          )
        })
      } else {
        output$analysisContent <- renderUI({
          tagList(
            kruskalWallisHT(kwResults, reactive(input$kwSigLvl))(),
            br(),
            kruskalWallisPlot(kwResults, reactive(input$kwSigLvl))(),
            br(),
            kwConclusion(kwResults, reactive(input$kwSigLvl))()
          )
        })
      }
    })

    observe({
      req(kwResults())

      results <- NULL
      tryCatch(
      {
        results <- kwResults()
      },
      error = function(e) {
        cat("Error in kwResults() for ranking:", e$message, "\n")
        return(NULL)
      }
      )

      if (is.null(results)) {
        output$renderKWRM <- renderUI({
          tagList(
            p("Unable to generate ranking table.",
              style = "color: #666; font-style: italic; text-align: center; padding: 20px;"
              )
          )
        })
        return()
      }

      if (!is.null(results$data)) {
        output$renderKWRM <- kwRankedTableOutput(results$data)
      } else {
        output$renderKWRM <- renderUI({
          tagList(
            p("No data available for ranking.",
              style = "color: #666; font-style: italic; text-align: center; padding: 20px;"
              )
          )
        })
      }
    })


    observeEvent(input$goInference, {
      req(kwUploadData())
      req(kwupload_iv$is_valid())

      output$renderKWData <- renderUI({
        tagList(
          div(DTOutput(session$ns("kwUploadTable")), style = "width: 75%")
        )
      })
    })

    observeEvent(input$kwUserData, {

      hide(id = "kwUploadInputs")

      fileInputs$kwStatus <- "uploaded"
      output$kwInitialUploadTable <- kruskalWallisUploadInitial(kwUploadData)

      if (kwupload_iv$is_valid()) {
        updateRadioButtons(session, "kwFormat", selected = "Multiple")

        freezeReactiveValue(input, "kwMultiColumns")
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "kwMultiColumns",
          choices = c(colnames(kwUploadData())),
          selected = character(0)
        )

        freezeReactiveValue(input, "kwResponse")
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "kwResponse",
          choices = c(colnames(kwUploadData())),
          selected = character(0)
        )
        freezeReactiveValue(input, "kwFactors")
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "kwFactors",
          choices = c(colnames(kwUploadData())),
          selected = character(0)
        )

        shinyjs::show(id = "kwUploadInputs")
      }
    })
    
    output$kwInitialUploadTable <- renderDT({
      datatable(kwUploadData(), width = "75%")
    })

    observeEvent(input$anovaUserData, priority = 10, {
      hide(id = "anovaUploadInputs")

      fileInputs$anovaStatus <- "uploaded"

      if (anovaupload_iv$is_valid()) {
        freezeReactiveValue(input, "anovaMultiColumns")
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "anovaMultiColumns",
          choices = c(colnames(anovaUploadData()))
        )

        freezeReactiveValue(input, "anovaResponse")
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "anovaResponse",
          choices = c(colnames(anovaUploadData()))
        )

        freezeReactiveValue(input, "anovaFactors")
        updateSelectizeInput(
          session = getDefaultReactiveDomain(),
          "anovaFactors",
          choices = c(colnames(anovaUploadData()))
        )

        shinyjs::show(id = "anovaUploadInputs")
      }
    })

    observeEvent(input$calculate, {
      do.call(showResultTabs,
              switch(input$multipleMethodChoice,
                     "anova" = list(tabset = "anovaTabset", tabs = c("Analysis", "Graphs")),
                     "kw" = list(tabset = "kwTabset", tabs = c("Analysis", "Data with Ranks"))))
    })


    output$fishersObs <- renderDT({
      req(si_iv$is_valid())

      CreateChiSqObserved(chiSqTotaled())
    })

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

        ## choose columns based on whether Yates' correction applied or not
        yates_applied <- input$chiSquareYates
        dimension <- input$chisquareDimension
        if (yates_applied && dimension == "2 x 2") {
          selected_cols <- c("O", "E", "(O - E)", "(|O - E| - 0.5)<sup>2</sup>", "(|O - E| - 0.5)<sup>2</sup> / E", "Standardized Residuals")
        } else {
          selected_cols <- c("O", "E", "(O - E)", "(O - E)<sup>2</sup>", "(O - E)<sup>2</sup> / E", "Standardized Residuals")
        }

        display_matrix <- chiSqTest$Matrix[, selected_cols, drop = FALSE]

        headers <- htmltools::withTags(table(
                                class = "display",
                                thead(
                                  tr(lapply(selected_cols, function(colname) {
                                    th(HTML(colname),
                                       class = "dt-center",
                                       style = "border-right: 1px solid rgba(0, 0, 0, 0.15); border-top: 1px solid rgba(0, 0, 0, 0.15);"
                                       )
                                  }))
                                )
                              ))

        datatable(display_matrix,
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
                      list(width = "130px", targets = c(0, 1, 2, 3, 5)),
                      list(width = "160px", targets = 4),
                      list(className = "dt-center", targets = c(0, 1, 2, 3, 4, 5))
                    )
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none",
                  rownames = FALSE
                  ) %>%
          formatStyle(
            columns = 0:ncol(display_matrix),
            target = "row",
            fontWeight = styleRow(dim(display_matrix)[1], "bold")
          )
      })

      CreateChiSqObserved <- function(chiSqData) {
        headers <- GetChiSqHeaders(chiSqData)

        observedTable <- datatable(chiSqData,
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
                                       list(
                                         width = "100px",
                                         targets = 0:ncol(chiSqData)
                                       ),
                                       list(
                                         className = "dt-center",
                                         targets = 0:ncol(chiSqData)
                                       )
                                     )
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
        headers <- GetChiSqHeaders(totaledData)

        expectedTable <- datatable(totaledData,
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
                                       list(
                                         width = "100px",
                                         targets = 0:ncol(totaledData)
                                       ),
                                       list(
                                         className = "dt-center",
                                         targets = 0:ncol(totaledData)
                                       )
                                     )
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
        formatStyle(
          columns = c(0, numCol),
          fontWeight = "bold"
        ) %>%
        formatStyle(
          columns = 1:numCol,
          target = "row",
          fontWeight = styleRow(dim(chiSqTotaled())[1], "bold")
        ) %>%
        formatStyle(
          columns = c(0, numCol - 1),
          borderRight = styleRow(c(1:(numRow - 1)), "2px solid #787878")
        ) %>%
        formatStyle(
          columns = c(1:(numCol - 1)),
          borderTop = styleRow(c(1), "2px solid #787878"),
          borderBottom = styleRow(c(numRow - 1), "2px solid #787878")
        )
    }

    GetChiSqHeaders <- function(chiSqData) {
        rowTitle <- input$chiSquareRowHeader
        colTitle <- input$chiSquareColHeader

        if (rowTitle == "" && colTitle == "") {
          headers <- htmltools::withTags(table(
                                  class = "display",
                                  thead(
                                    tr(
                                      th("",
                                         style = "border: 1px solid rgba(0, 0, 0, 0.15);
                        border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"
                        ),
                        lapply(colnames(chiSqData), th,
                               style = "border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);"
                          )
                        )
                        )
                        ))
        } else {
          headers <- htmltools::withTags(table(
                                  class = "display",
                                  thead(
                                    tr(
                                      th(
                                        rowspan = 2, colspan = 1, rowTitle,
                                        class = "dt-center",
                                        style = "border: 1px solid rgba(0, 0, 0, 0.15);"
                                      ),
                                      th(
                                        colspan = ncol(chiSqData), colTitle,
                                        class = "dt-center",
                                        style = "border: 1px solid rgba(0, 0, 0, 0.15);
                      border-left: none;"
                      )
                      ),
                      tr(
                        lapply(colnames(chiSqData), th,
                               style = "border-right: 1px solid rgba(0, 0, 0, 0.15);"
                               )
                      )
                      )
                      ))
        }

        return(headers)
      }

      PrintANOVA <- function() {
        data <- anovaOneWayResults()$test

        if (input$anovaSigLvl == "10%") {
          sigLvl <- 0.1
        } else if (input$anovaSigLvl == "5%") {
          sigLvl <- 0.05
        } else {
          sigLvl <- 0.01
        }

        critVal <- round(qf(1 - sigLvl, df1 = data[1, "Df"], df2 = data[2, "Df"]), 4)

        if (data[1, "Pr(>F)"] < sigLvl) {
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

        hypothesis <- PrintANOVAHyp(sigLvl)
        testStat <- PrintANOVAFormula()
        pValue <- PrintANOVAPValue(pValSymbol, sigLvl, reject)
        anovaCV <- PrintANOVACV(critVal, data[1, "Df"], data[2, "Df"], reject, region, sigLvl)
        conclusion <- PrintANOVAConclusion(sigLvl, reject)
        tagAppendChildren(hypothesis, testStat, pValue, anovaCV, conclusion)
      }

    PrintANOVAHyp <- function(sigLvl) {
        anovaData <- anovaOneWayResults()$data
        numGroups <- anovaOneWayResults()$numFactors
        groupCol <- anovaOneWayResults()$factorCol
        groupNames <- anovaOneWayResults()$factorNames

        nullHyp <- "H_{0} : "
        groupCounts <- tagList()

        for (group in 1:(numGroups - 1)) {
          nullHyp <- paste0(nullHyp, "\\mu_{\\textit{", groupNames[group], "}} = ")
        }

        nullHyp <- paste0(nullHyp, "\\mu_{\\textit{", groupNames[numGroups], "}}")

        hypothesis <- tagList(
          withMathJax(),
          sprintf(
            "\\( %s \\) ",
            nullHyp
          ),
          br(),
          sprintf("\\( H_{a}: \\) At least two means differ"),
          br(),
          br(),
          sprintf(
            "\\( \\alpha = %s \\)",
            sigLvl
          ),
          br(),
          br(),
          sprintf(
            "\\( n = %s \\)",
            anovaOneWayResults()$count
          ),
          br(),
          sprintf(
            "\\( k = %s \\)",
            numGroups
          ),
          br(),
          br()
        )

        return(hypothesis)
      }

      PrintANOVAFormula <- function() {
        tagList(
          br(),
          p(tags$b("Numerical Summaries:")),
          DTOutput(session$ns("oneWayFactorTable"), width = "600px"),
          br(),
          br(),
          p(tags$b("ANOVA Table:")),
          DTOutput(session$ns("oneWayAnovaTable"), width = "900px"),
          ## uiOutput(session$ns("oneWayAnovaTableHTML"), width = '900px'),
          br(),
          br(),
          p(tags$b("Test Statistic:")),
          formatLaTeXBigOrSmallNumbers(
            sprintf(
              "\\( F = \\dfrac{MSB}{MSE} = \\dfrac{%0.4g}{%0.4g} = %0.4g \\)",
              anovaOneWayResults()$test[1, "Mean Sq"],
              anovaOneWayResults()$test[2, "Mean Sq"],
              anovaOneWayResults()$test[1, "F value"]
            )
          ),
          br(),
          br(),
          br()
        )
      }

      PrintANOVAFactorTable <- function() {
        anovaData <- anovaOneWayResults()$data
        numGroups <- anovaOneWayResults()$numFactors
        groupCol <- anovaOneWayResults()$factorCol
        groupNames <- anovaOneWayResults()$factorNames
        colNames <- c("Factor", "Sample Size", "Sample Mean", "Sample Standard Deviation")

        headers <- htmltools::withTags(table(
                                class = "display",
                                thead(
                                  tr(
                                    th(colNames[1],
                                       style = "border: 1px solid rgba(0, 0, 0, 0.15);
                      border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"
                      ),
                      lapply(colNames[2:4], th,
                             style = "border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);"
                          )
                      )
                      )
                      ))

        factor_df <- data.frame()

        for (group in 1:numGroups) {
          groupData <- as.data.frame(anovaData[anovaData$ind == groupNames[group], ])
          factor_df <- rbind(factor_df, data.frame(
                                          "Sample Size" = length(groupData[, groupCol]),
                                          "Sample Mean" = mean(groupData[, "values"]),
                                          "Sample Standard Deviation" = sd(groupData[, "values"])
                                        ))
        }

        rownames(factor_df) <- groupNames

        ftable <- datatable(factor_df,
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
                                list(
                                  className = "dt-center",
                                  targets = 0:3
                                ),
                                list(
                                  width = "150px",
                                  targets = 0:3
                                )
                              )
                            ),
                            selection = "none",
                            escape = FALSE,
                            filter = "none"
                            ) %>%
          formatRound(
            columns = 1,
            digits = 0
          ) %>%
          formatRound(
            columns = 2:3,
            digits = 4
          ) %>%
          formatStyle(
            columns = c(0),
            fontWeight = "bold"
          )

        return(ftable)
      }

      PrintANOVATable <- function() {
        data <- anovaOneWayResults()$test

        if (data[1, "Pr(>F)"] < 0.0001 && data[1, "Pr(>F)"] > 0) {
          data[1, "Pr(>F)"] <- "P < 0.0001"
        } else {
          data[1, "Pr(>F)"] <- paste(round(data[1, "Pr(>F)"], 4))
        }

        data <- rbind(data, c(sum(data[, "Df"]), sum(data[, "Sum Sq"]), NA, NA, NA))
        rownames(data) <- c("Between Groups (Model)", "Within Groups (Error)", "Total")
        colNames <- c("df", "Sum of Squares (SS)", "Mean Sum of Squares (MS)", "F-ratio", "P-Value")

        style <- function(...) {
          args <- rlang::list2(...)
          paste(names(args), args, sep = ": ", collapse = "; ")
        }
        rule <- function(p) sprintf("1px solid rgba(0, 0, 0, %0.02f)", p)
        style_a <- style(border = rule(0.15), `border-bottom` = rule(0.30))
        style_b <- style(`border-right` = rule(0.15), `border-top` = rule(0.15))

        headers <- withTags(table(
          class = "display",
          thead(tr(
            th("Sources of Variation", style = style_a),
            lapply(colNames, th, style = style_b)
          ))
        ))

        ## FIXME: data table is heavy handed for what is created. Using standard
        ## CSS with a standard HTML table is probably a lot easier!
        datatable(data[, 0:5],
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
                      list(className = "dt-center", targets = 0:5),
                      list(width = "150px", targets = 2:5)
                    )
                  ),
                  selection = "none",
                  escape = FALSE,
                  filter = "none"
                  ) |>
          formatRound(columns = 1, digits = 0) |>
          ## FIXME: the use of formatRound here is not appropriate to the numbers
          ## when rare, so we'll instead use character values after formatting
          ## numbers like in the LaTeX equations.
          formatRound(columns = 2:4, digits = 4) |>
          formatStyle(columns = c(0, 4), fontWeight = "bold") %>%
          formatStyle(columns = 1:5, target = "row", fontWeight = styleRow(3, "bold"))
      }

      PrintANOVATableHTML <- function() {
        data <- anovaOneWayResults()$test
        if (data[1, "Pr(>F)"] < 0.0001 && data[1, "Pr(>F)"] > 0) {
          data[1, "Pr(>F)"] <- "P < 0.0001"
        } else {
          data[1, "Pr(>F)"] <- paste(round(data[1, "Pr(>F)"], 4))
        }
        data <- rbind(data, c(sum(data[, "Df"]), sum(data[, "Sum Sq"]), NA, NA, NA))

        d <- sapply(as.data.frame(data), function(x) sprintf("%0.4g", as.numeric(x)))
        d[which(d == "NA")] <- ""
        reformat_indices <- which(grepl("e", d))
        d[reformat_indices] <- formatLaTeXBigOrSmallNumbers(d[reformat_indices])
        d[seq(d)] <- sprintf("\\(%s\\)", d)

        withMathJax(
          tags$style("table.plain th, table.plain tr { padding: 10px; }"),
          tags$table(
                 class = "plain",
                 tags$thead(
                        tags$tr(
                               tags$th("Sources of Variation"),
                               tags$th("df"),
                               tags$th("Sum of Squares (SS)"),
                               tags$th("Mean Sum of Squares (MS)"),
                               tags$th("F-ratio"),
                               tags$th("P-Value")
                             )
                      ),
                 tags$tbody(
                        tags$tr(
                               tags$td(style = "font-weight: bold;", "Between Groups (Model)"),
                               tags$td(d[1, 1]),
                               tags$td(d[1, 2]),
                               tags$td(d[1, 3]),
                               tags$td(style = "font-weight: bold;", d[1, 4]),
                               tags$td(d[1, 5])
                             ),
                        tags$tr(
                               tags$td(style = "font-weight: bold;", "Within Groups (Error)"),
                               tags$td(d[2, 1]),
                               tags$td(d[2, 2]),
                               tags$td(d[2, 3]),
                               tags$td(d[2, 4]),
                               tags$td(d[2, 5])
                             ),
                        tags$tr(
                               style = "font-weight: bold;",
                               tags$td("Total"),
                               tags$td(d[3, 1]),
                               tags$td(d[3, 2]),
                               tags$td(d[3, 3]),
                               tags$td(d[3, 4]),
                               tags$td(d[3, 5])
                             )
                      )
               )
        )
      }

      PrintANOVAPValue <- function(pValSymbol, sigLvl, reject) {
        tsValue <- anovaOneWayResults()$test[1, "F value"]
        pValue <- anovaOneWayResults()$test[1, "Pr(>F)"]
        pvalCalc <- paste("P(\\, F \\, \\gt \\,", round(tsValue, 4), ")")

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
      }

      PrintANOVACV <- function(critVal, df1, df2, reject, region, alpha) {
        cvOutput <- tagList(
          p(tags$b("Using Critical Value Method:")),
          sprintf(
            "Critical Value \\( = F_{\\alpha, \\, (k - 1), \\, (n - k)} = F_{%s, \\, %s, \\, %s} = %s \\)",
            alpha,
            df1,
            df2,
            critVal
          ),
          br(),
          br(),
          sprintf(
            "Since the test statistic \\( F \\) falls within the %s region, %s \\( H_{0}\\).",
            region,
            reject
          ),
          br(),
          br(),
          br(),
          plotOutput(session$ns("oneWayAnovaPlot"), width = "50%", height = "400px"),
          br(),
          br()
        )
      }

      PrintANOVAConclusion <- function(sigLvl, reject) {
        if (reject == "reject") {
          result <- "there is sufficient statistical evidence in support of the alternative
                 hypothesis \\( (H_{a}) \\) that at least two means differ and post
                 hoc tests are warranted."
        } else {
          result <- "there is not enough statistical evidence in support of the alternative
                  hypothesis \\( (H_{a}) \\) that at least two means differ."
        }

        tagList(
          p(tags$b("Conclusion:")),
          p(
            sprintf(
              "At the %1.0f%% significance level, %s",
              sigLvl * 100,
              result
            ),
            br()
          )
        )
      }

    formatLaTeXBigOrSmallNumbers <- function(string) {
      gsub("[eE](-?)\\+?0?([0-9]+)", "^{\\1\\2}", string)
    }

    getTotaledMatrix <- function(cMatrix, matrixData) {
        colnames(cMatrix) <- colnames(matrixData)
        rownames(cMatrix) <- rownames(matrixData)
        cMatrix <- cbind(cMatrix, Total = round(rowSums(cMatrix), 4))
        cMatrix <- rbind(cMatrix, Total = round(colSums(cMatrix), 4))

        return(cMatrix)
      }
  })
}

