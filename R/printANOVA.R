library(shiny)

PrintANOVA <- function() {
  data <- anovaOneWayResults()$test
  
  if(input$anovaSigLvl == "10%") {
    sigLvl <- 0.1 
  } else if(input$anovaSigLvl == "5%") {
    sigLvl <- 0.05
  } else {
    sigLvl <- 0.01
  }
  
  critVal <- round(qf(1 - sigLvl, df1 = data[1,"Df"], df2 = data[2,"Df"]), 4)
  
  if(data[1,"Pr(>F)"] < sigLvl) {
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
  anovaCV <- PrintANOVACV(critVal, data[1,"Df"], data[2,"Df"], reject, region, sigLvl)
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
  
  for(group in 1:(numGroups - 1)) {
    nullHyp <- paste0(nullHyp, "\\mu_{\\textit{", groupNames[group], "}} = ")
  }
  
  nullHyp <- paste0(nullHyp, "\\mu_{\\textit{", groupNames[numGroups], "}}")
  
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
    sprintf("\\( k = %s \\)",
            numGroups),
    br(),
    br(),
  )
  
  return(hypothesis)
}

PrintANOVAFormula <- function() {
  tagList(
    br(),
    p(tags$b("Numerical Summaries:")),
    DTOutput("oneWayFactorTable", width = '600px'),
    br(),
    br(),
    p(tags$b("ANOVA Table:")),
    DTOutput("oneWayAnovaTable", width = '900px'),
    br(),
    br(),
    p(tags$b("Test Statistic:")),
    sprintf("\\( F = \\dfrac{MSB}{MSE} = \\dfrac{%0.4f}{%0.4f} = %0.4f \\)",
            anovaOneWayResults()$test[1,"Mean Sq"],
            anovaOneWayResults()$test[2,"Mean Sq"],
            anovaOneWayResults()$test[1,"F value"]),
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
  
  headers = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colNames[1],
           style = "border: 1px solid rgba(0, 0, 0, 0.15);
                      border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"),
        lapply(colNames[2:4], th,
               style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);')
      )
    )
  ))
  
  factor_df <- data.frame()
  
  for(group in 1:numGroups) {
    groupData <- as.data.frame(anovaData[anovaData$ind == groupNames[group],])
    factor_df <- rbind(factor_df, data.frame("Sample Size" = length(groupData[,groupCol]),
                                             "Sample Mean" = mean(groupData[,"values"]),
                                             "Sample Standard Deviation" = sd(groupData[,"values"])))
  }
  
  rownames(factor_df) <- groupNames
  
  ftable <- datatable(factor_df,
                      class = 'cell-border stripe',
                      container = headers,
                      options = list(
                        dom = 't',
                        pageLength = -1,
                        ordering = FALSE,
                        searching = FALSE,
                        paging = FALSE,
                        autoWidth = FALSE,
                        scrollX = TRUE,
                        columnDefs = list(list(className = 'dt-center',
                                               targets = 0:3),
                                          list(width = '150px', 
                                               targets = 0:3))
                      ),
                      selection = "none",
                      escape = FALSE,
                      filter = "none"
  ) %>% formatRound(columns = 1,
                    digits = 0
  ) %>% formatRound(columns = 2:3,
                    digits = 4
  ) %>% formatStyle(columns = c(0),
                    fontWeight = 'bold'
  ) 
  
  return(ftable)
}

PrintANOVATable <- function() {
  data <- anovaOneWayResults()$test
  
  if(data[1,"Pr(>F)"] < 0.0001 && data[1,"Pr(>F)"] > 0) {
    data[1,"Pr(>F)"] <- "P < 0.0001"
  } else {
    data[1,"Pr(>F)"] <- paste(round(data[1,"Pr(>F)"], 4))
  }
  
  data <- rbind(data, c(sum(data[,"Df"]), sum(data[,"Sum Sq"]), NA, NA, NA))
  # print(data[,"Df"])
  rownames(data) <- c("Between", "Error", "Total")
  colNames <- c("df", "Sum of Squares (SS)", "Mean Sum of Squares (MS)", "F-ratio", "P-Value")
  
  headers = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th("Sources of Variation", 
           style = "border: 1px solid rgba(0, 0, 0, 0.15);
                      border-bottom: 1px solid  rgba(0, 0, 0, 0.3);"),
        lapply(colNames, th, 
               style = 'border-right: 1px solid rgba(0, 0, 0, 0.15);
                          border-top: 1px solid rgba(0, 0, 0, 0.15);')
      )
    )
  ))
  
  datatable(data[,0:5],
            class = 'cell-border stripe',
            container = headers,
            options = list(
              dom = 't',
              pageLength = -1,
              ordering = FALSE,
              searching = FALSE,
              paging = FALSE,
              autoWidth = FALSE,
              scrollX = TRUE,
              columnDefs = list(list(className = 'dt-center',
                                     targets = 0:5),
                                list(width = '150px', 
                                     targets = 2:5))
            ),
            selection = "none",
            escape = FALSE,
            filter = "none"
  ) %>% formatRound(columns = 1,
                    digits = 0
  ) %>% formatRound(columns = 2:4,
                    digits = 4
  ) %>% formatStyle(columns = c(0,4),
                    fontWeight = 'bold'
  ) %>% formatStyle(columns = 1:5,
                    target = 'row',
                    fontWeight = styleRow(3, "bold"))
}

PrintANOVAPValue <- function(pValSymbol, sigLvl, reject) {
  tsValue <- anovaOneWayResults()$test[1,"F value"]
  pValue <- anovaOneWayResults()$test[1,"Pr(>F)"]
  pvalCalc <- paste("P(\\, F \\, \\gt \\,", round(tsValue,4), ")")
  
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
}

PrintANOVACV <- function(critVal, df1, df2, reject, region, alpha) {
  
  cvOutput <- tagList(
    p(tags$b("Using Critical Value Method:")),
    sprintf("Critical Value \\( = F_{\\alpha, \\, (k - 1), \\, (n - k)} = F_{%s, \\, %s, \\, %s} = %s \\)",
            alpha,
            df1,
            df2,
            critVal),
    br(),
    br(),
    sprintf("Since the test statistic \\( F \\) falls within the %s region, %s \\( H_{0}\\).",
            region,
            reject),
    br(),
    br(),
    br(),
    plotOutput("oneWayAnovaPlot", width = "50%", height = "400px"),
    br(),
    br()
  )
}

PrintANOVAConclusion <- function(sigLvl, reject) {
  if(reject == "reject") {
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
      sprintf("At the %1.0f%% significance level, %s",
              sigLvl*100,
              result),
      br(),
    )
  )
}