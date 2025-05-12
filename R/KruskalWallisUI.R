library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(DT)

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
    kwData <- stack(kwUploadData_output[,kwMultiColumns])
    factorCol <- "ind"
    factorNames <- levels(kwData$ind)
  } else {
    kwData <- kwUploadData_output
    colnames(kwData)[colnames(kwData) == kwFactors] <- "ind"
    colnames(kwData)[colnames(kwData) == kwResponse] <- "values"
    kwData <- mutate(kwData, ind = factor(ind))
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
    data <- kwResults_output()$test
    kw_test_rounded <- as.character(round(data$statistic, 4))
    kw_pv <- data$p.value
    kw_pv_rounded <- as.character(round(kw_pv, 4))
    kw_sl <- as.numeric(substring(kwSigLvl_input(), 1, nchar(kwSigLvl_input()) - 1))/100
    
    withMathJax(
      tagList(
        p(
          sprintf("\\( H_{0}:\\) The distributions of the groups are identical, and their medians are equal."), br(),
          sprintf("\\( H_{a}:\\) At least one group differs in median from the others."), br(),
          br(),
          sprintf("\\( \\alpha = %s \\)", kw_sl),
          br(), br(),
          p(tags$b("Test Statistic:")),
          sprintf("\\( K = \\frac{12}{n(n + 1)}\\sum_{j = 1}^{k}\\frac{R_j^2}{n_j} - 3(n + 1) = %s\\)", kw_test_rounded),
          br(), br(),
          p(tags$b("Using P-value method: ")),
          sprintf("\\(P = P(\\chi^2 \\geq %s) = %s\\)", kw_test_rounded, kw_pv_rounded),
          br(), br(),
          if (kw_pv <= kw_sl){
            sprintf("Since \\(P \\leq %s\\), reject \\(H_{0}.\\)", kw_sl)
          } else {
            sprintf("Since \\(P > %s\\), do not reject \\(H_{0}.\\)", kw_sl)
          },
          br(), br(),
          p(tags$b("Conclusion: ")),
          sprintf(
            if (kw_pv <= kw_sl){
              sprintf("Since the p-value is less than the significance level (%s \\(\\leq %.2f\\)), we reject the null hypothesis.
                     There is sufficient evidence to conclude that at least one group's median is significantly different from the others.", kw_pv_rounded, kw_sl)
            } else {
              sprintf("Since the p-value is greater than the significance level (%s \\(> %.2f\\)), we fail to reject the null hypothesis.
                     There is insufficient evidence to conclude that the medians of the groups are significantly different.", kw_pv_rounded, kw_sl)
            }
          )
        )
      )
    )
  })
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
        names_sep = " "  # Space separator for "groupname value" and "groupname rank"
      ) %>%
      dplyr::select(-ObsID) %>%
      # Reorder columns to put Value before Rank for each group
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
      # Clean up column names to match exact requested format
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
              list(width = '120px', targets = "_all")  # Slightly wider columns for the names
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