library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)

### ------------ Kruskal-Wallis Reactives ------------------------------------   
kwUploadData_func <- function(kwUserData) {
  ext <- tools::file_ext(kwUserData$name)
  ext <- tolower(ext)
  
  switch(ext,
         csv = read_csv(kwUserData$datapath, show_col_types = FALSE),
         xls = read_xls(kwUserData$datapath),
         xlsx = read_xlsx(kwUserData$datapath),
         txt = read_tsv(kwUserData$datapath, show_col_types = FALSE),
         validate("Improper file format.")
  )
}

kwStackedIsValid_func <- function(kwResponse, kwFactors) {
  valid <- TRUE
  
  if(!is.null(kwResponse) && !is.null(kwFactors)) {
    if(kwResponse == kwFactors) {
      valid <- FALSE
    }
  }
  
  return(valid)
}

kwResults_func <- function(si_iv_is_valid, kwFormat, kwMultiColumns, kwUploadData_output, kwFactors, kwResponse) {
  req(si_iv_is_valid)
  
  results <- list()
  
  if (kwFormat == "Multiple"){
    req(kwMultiColumns)
    kwData <- stack(kwUploadData_output[,kwMultiColumns])
    factorCol <- "ind"
    factorNames <- levels(kwData$ind)
  } else {
    req(kwFactors, kwResponse)
    kwData <- kwUploadData_output
    colnames(kwData)[colnames(kwData) == kwFactors] <- "ind"
    colnames(kwData)[colnames(kwData) == kwResponse] <- "values"
    kwData <- kwData %>% dplyr::mutate(ind = factor(ind)) 
    factorCol <- "ind"
    factorNames <- levels(kwData$ind)
    
  }
  
  kwData <- na.omit(kwData)
  
  kwData <- kwData %>%
    dplyr::mutate(Rank = rank(values, na.last = "keep", ties.method = "average"))
  
  
  totalCount <- nrow(kwData)    # n for Kruskal-Wallis
  numFactors <- length(factorNames)   # r for Kruskal-Wallis
  
  kwTest <- kruskal.test(formula = values ~ ind, data = kwData)
  
  results$data <- kwData
  results$count <-totalCount
  results$factorCol <- factorCol
  results$numFactor <- numFactors
  results$factorNames <- factorNames
  results$test <- kwTest
  
  return(results)
}

### ---------------- Kruskal-Wallis Validation ------------------------------------
validateKWInputs <- function(kwupload_iv_is_valid, kwUserData, fileInputs_kwStatus,
                             kwUploadData_output, kwmulti_iv_is_valid, input_kwMultiColumns, kwstacked_iv_is_valid,
                             input_kwResponse, input_kwFactors, kwStackedIsValid_output) 
{
  if(!kwupload_iv_is_valid()) {
    if(is.null(kwUserData)) {
      validate("Please upload a file.")
    }
    
    validate(
      need(!is.null(fileInputs_kwStatus), "Please upload a file."),
      errorClass = "myClass"
    )
    
    validate(
      need(nrow(kwUploadData_output) > 0, "File is empty."),
      need(ncol(kwUploadData_output) >= 2, "File must contain at least 2 distinct columns of data to choose from for analysis."),
      errorClass = "myClass"
    )
  }
  
  if(!kwmulti_iv_is_valid()) {
    validate(
      need(length(input_kwMultiColumns) >= 2, "Please select two or more columns to conduct analysis."),
      errorClass = "myClass"
    )
  }
  
  if(!kwstacked_iv_is_valid()) {
    validate(
      need(!is.null(input_kwResponse) && input_kwResponse != '', "Please select a Response Variable."),
      need(!is.null(input_kwFactors) && input_kwFactors != '', "Please select a Factors column."),
      errorClass = "myClass"
    )
    
    validate(
      need(kwStackedIsValid_output == TRUE, "Please select distinct columns for Response Variable and Factors."),
      errorClass = "myClass"
    )
  }
}

### ---------------- Kruskal-Wallis Outputs------------------------------------
kruskalWallisHT <- function(kwResults_output, kwSigLvl_input) {

  renderUI({
    req(kwResults_output())
    
    results <- kwResults_output()
    kwTest <- results$test
    kwData <- results$data
    totalCount <- results$count
    factorNames <- results$factorNames
    
    # conditions for hypothesis testing 
    kw_pv <- kwTest$p.value
    kw_sl <- as.numeric(substring(kwSigLvl_input(), 1, nchar(kwSigLvl_input()) - 1))/100
    
    global_ranks <- kwData$Rank
    group_sums <- tapply(global_ranks, kwData$ind, sum)
    group_n <- tapply(global_ranks, kwData$ind, length)
    
    # used for calculating the Test Statistic
    sum_R2_n <- sum(group_sums^2 / group_n)
    step1 <- 12 / (totalCount * (totalCount + 1))
    step2 <- step1 * sum_R2_n
    step3 <- 3 * (totalCount + 1)
    kwTstat <- step2-step3
    
    # used in P-Value calculations
    data <- kwResults_output()$test
    kw_pv <- data$p.value
    kw_pv_rounded <- as.numeric(round(kw_pv, 4))
    kw_test_rounded <- as.character(round(kwTstat, 4))
    kw_test_rounded_comparison <- as.character(round(data$statistic, 4))
    
    #to be used in the rejection/acceptance graph
    kw_test <- as.numeric(round(data$statistic, 4))
    
    # Degrees of Freedom 
    kw_df <- length(factorNames) -1
    kw_k <- length(factorNames)
    
    # Chi Square CV
    alph <-(1-(as.numeric(substring(kwSigLvl_input(), 1, nchar(kwSigLvl_input()) - 1))/100))
    kw_chi<- (qchisq(alph,kw_df))
    
    # the sum of the inner part of the T Statistic calculation
    sum_parts <- sapply(seq_along(factorNames), function(i) {
      sprintf("\\frac{(%.1f)^2}{%d}", group_sums[i], group_n[i])
    })
    
    withMathJax(
      tagList(
        p(
          sprintf("\\( H_{0}:\\) The distributions of the groups are identical, and their medians are equal."), br(),
          sprintf("\\( H_{a}:\\) At least one group differs in median from the others."), br(),
          br(),
          sprintf("\\( \\alpha = %s \\)", kw_sl),
          br(), br(),
          sprintf("\\( n = %s \\)", totalCount),
          br(),
          sprintf("\\( k = %s \\)", kw_k),
          
          br(), br(),
          p(tags$b("Test Statistic:")),
          
          sprintf("\\( H = \\frac{12}{n(n + 1)}\\sum_{j = 1}^{k}\\frac{R_j^2}{n_j} - 3(n + 1) \\)"), 
          sprintf("\\( = \\frac{12}{%d(%d + 1)}\\left(%s\\right) - 3(%d + 1) \\)",
                  totalCount, totalCount, paste(sum_parts, collapse = " + "), totalCount), 
          sprintf("\\( = %s \\)", kw_test_rounded_comparison),
          br(), 
            if (kw_test_rounded_comparison != kw_test_rounded){
              helpText("* Note: The average of data points was used in the case of a tie while calculating the rank score.")
            },
          
          br(), 
          
          p(tags$b("Using P-value method: ")),
          sprintf("\\(P = P(\\chi^2 \\geq %s) = %s\\)", kw_test_rounded, kw_pv_rounded),
          
          br(), br(),
          
          if (kw_pv <= kw_sl){
            sprintf("Since \\(P \\leq %s\\), reject \\(H_{0}.\\)", kw_sl)
          } else {
            sprintf("Since \\(P > %s\\), do not reject \\(H_{0}.\\)", kw_sl)
          },
          
          br(), br(),
          
          # Trying to use Chi-Square CV method
          p(tags$b("Using the Critical Value Method: ")),
          sprintf("Critical Value  = \\(\\chi^2 _{\\alpha, \\, df} = \\chi^2 _{\\alpha, \\, (k - 1)} = \\chi^2_{\\, %s, \\, %s} = %.4f \\)", kw_sl, kw_df, kw_chi),
          br(), br(), 
          
          if (kw_chi <= as.numeric(kw_test_rounded_comparison)){
            sprintf("Since the test statistic \\(\\chi^2\\) falls within the rejection region, reject \\(H_{0}.\\)")
                    #\\(%.4f \\leq %s\\), reject \\(H_{0}.\\)", kw_chi, kw_test_rounded_comparison)
          } else if (kw_chi > as.numeric(kw_test_rounded_comparison)) {
            sprintf("Since \\(%.4f > %s\\), do not reject \\(H_{0}.\\)", kw_chi, kw_test_rounded_comparison)
          },
        )
      )
    )
  })
}
kwConclusion <- function(kwResults, kwSigLvl_input) {
  renderUI({
    req(kwResults())
    
    results <- kwResults()
    kwTest <- results$test
    kw_pv <- kwTest$p.value
    kw_pv_rounded <- as.character(round(kw_pv, 4))
    
    if(kwSigLvl_input() == "10%") {
      kw_sl <- 10
    } else if(kwSigLvl_input() == "5%") {
      kw_sl <- 5
    } else {
      kw_sl <- 1
    }
    
    tagList(
      p(tags$b("Conclusion: ")),
      if (kw_pv <= kw_sl) {
        p(sprintf("At the %1.0f%% significance level, there is sufficient statistical evidence in support of the alternative hypothesis \\( (H_{a})\\)
that at least one group differs in median from the others.", kw_sl))
      } else {
        p(sprintf("At the %1.0f%% significance level, there is not enough statistical evidence in support of the alternative
                  hypothesis \\( (H_{a}) \\) that at least one group differs in median from the others."), kw_sl)
      }
    )
  })
}

kruskalWallisPlot <- function(kwResults_output, kwSigLvl_input) {
  renderPlot({
    req(kwResults_output())
    
    results <- kwResults_output()
    kwTest <- results$test
    kw_df <- length(results$factorNames) - 1 
    kwTstat <- round(kwTest$statistic, 4)
    
    if(kwSigLvl_input() == "10%") {
      sigLvl <- 0.1
    } else if(kwSigLvl_input() == "5%") {
      sigLvl <- 0.05
    } else {
      sigLvl <- 0.01
    }
    
    cv <- round(qchisq(1 - sigLvl, df = kw_df), 4)
    
    xMax <- max(10, cv * 1.5, kwTstat * 1.5)
    xSeq <- c(seq(0, xMax, length.out = 75), cv, kwTstat)
    rrLabel <- c((cv + max(xSeq))/2)
    x_vector <- sort(c(xSeq, rrLabel))
    p_vector <- dchisq(x_vector, df = kw_df)
    
    kw_dataframe <- distinct(data.frame(x = x_vector, y = p_vector))
    cv_dataframe <- filter(kw_dataframe, x %in% cv)
    ts_dataframe <- filter(kw_dataframe, x %in% kwTstat)
    rrLabelDF <- filter(kw_dataframe, x %in% rrLabel)
    arLabelDF <- filter(kw_dataframe, y %in% max(p_vector))
    
    ggplot(kw_dataframe,
           aes(x = x, y = y)) +
      stat_function(fun = dchisq,
                    args = list(df = kw_df),
                    geom = "Density",
                    fill = NA) +
      shadeHtArea2(kw_dataframe, cv, "greater") +
      geom_segment(data = filter(kw_dataframe, y %in% max(p_vector)),
                   aes(x = 0, xend = 0, y = 0, yend = y, alpha = 0.5),
                   linetype = "solid",
                   linewidth = 0.75,
                   color='black',
                   show.legend = FALSE) +
      geom_text(data = filter(kw_dataframe, x %in% c(0)),
                aes(x = x, y = 0, label = "0"),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = -.03,
                check_overlap = TRUE) +
      geom_segment(data = cv_dataframe,
                   aes(x = x, xend = x, y = 0, yend = y),
                   linetype = "solid",
                   lineend = 'butt',
                   linewidth = 1.5,
                   color='#023B70') +
      geom_text(data = cv_dataframe,
                aes(x = x, y = 0, label = x),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = -.03,
                check_overlap = TRUE) +
      geom_segment(data = ts_dataframe,
                   aes(x = x, xend = x, y = 0, yend = y + .055),
                   linetype = "solid",
                   linewidth = 1.25,
                   
                   color='#BD130B') +
      geom_text(data = ts_dataframe,
                aes(x = x, y = y, label = x),
                size = 14 / .pt,
                fontface = "bold",
                nudge_y = .075,
                check_overlap = TRUE) +
      geom_text(data = arLabelDF,
                aes(x = x, y = 0, label = "A R"),
                size = 16 / .pt,
                fontface = "bold",
                vjust = -5,
                check_overlap = TRUE) +
      geom_text(data = rrLabelDF,
                aes(x = x, y = y, label = "R R"),
                size = 16 / .pt,
                fontface = "bold",
                vjust = -5,
                check_overlap = TRUE) +
      theme_void() +
      ylab("") +
      xlab(expression(chi^2)) + 
      scale_y_continuous(breaks = NULL) +
      theme(axis.title.x = element_text(size = 20,
                                        family = "serif",
                                        face = "bold.italic"))
  })
}

shadeHtArea2 <- function(data, cv, direction = "greater") {
  if(direction == "greater") {
    subset <- data[data$x >= cv, ]
    x <- c(cv, subset$x, max(data$x))
    y <- c(0, subset$y, 0)
  } else if(direction == "less") {
    subset <- data[data$x <= cv, ]
    x <- c(min(data$x), subset$x, cv)
    y <- c(0, subset$y, 0)
  } else { # two-tailed (?)
    subset1 <- data[data$x <= cv[1], ]
    subset2 <- data[data$x >= cv[2], ]
    x <- c(min(data$x), subset1$x, cv[1], cv[2], subset2$x, max(data$x))
    y <- c(0, subset1$y, 0, 0, subset2$y, 0)
  }
  
  geom_polygon(data = data.frame(x = x, y = y), 
               aes(x = x, y = y), 
               fill = "#023B70", 
               alpha = 0.2)
}

kruskalWallisUpload <- function(kwUploadData_output, kwupload_iv_is_valid) {
  renderDT({
    req(kwupload_iv_is_valid())
    datatable(kwUploadData_output(),
              options = list(pageLength = -1,
                             lengthMenu = list(c(25, 50, 100, -1),
                                               c("25", "50", "100", "all")),
                             columnDefs = list(list(className = 'dt-center',
                                                    targets = 0:ncol(kwUploadData_output())))))
  })
}
# For the output in the second tabe in KW tab
kwRankedTableOutput <- function(data) {
  renderUI({
    req(data)
    
    df <- if (is.reactive(data)) data() else data
    
    ranked_data_wide <- df %>%
      dplyr::select(Group = ind, Value = values, Rank = Rank) %>%
      dplyr::arrange(Group, Rank) %>%
      dplyr::group_by(Group) %>%
      dplyr::mutate(ObsID = row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(
        id_cols = ObsID,
        names_from = Group,
        values_from = c(Value, Rank),
        names_sep = " "  
      ) %>%
      dplyr::select(-ObsID) %>%
      dplyr::select(
        order(
          match(
            gsub("(Value|Rank) (.*)", "\\2", names(.)), 
            unique(gsub("(Value|Rank) (.*)", "\\2", names(.)))
          ),
          match(
            gsub("(Value|Rank) (.*)", "\\1", names(.)), 
            c("Value", "Rank")
          )
        )
      ) %>%
      
      dplyr::rename_with(~gsub("Value (.*)", "\\1 Value", .)) %>%
      dplyr::rename_with(~gsub("Rank (.*)", "\\1 Rank", .))
    
    tagList(
      titlePanel("Ranked Results by Group"),
      br(),
      br(),
      div(
        DT::datatable(
          ranked_data_wide,
          rownames = FALSE,
          options = list(
            pageLength = 10,
            lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "all")),
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(width = '120px', targets = "_all")  
            )
          )
        ), 
        style = "width: 95%"
      ),
      br(),
      br()
    )
  })
}