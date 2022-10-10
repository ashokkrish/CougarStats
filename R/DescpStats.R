library(shiny)

createNumLst <- function(text) {
  text <- gsub("","", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}

dat <- createNumLst(input$descriptiveStat)

withMathJax(
  paste0("\\(\\bar{x}=\\)", mean(dat)),
  #paste0("Mode = ", Modes(dat)),
  br(),
  paste0("Median (Q2) =", median(dat)),
  br(),
  paste("Five Number Summary:"),
  br(),
  #paste0(min(dat), " ", quantile(dat,0.25)," ", median(dat), " ", quantile(dat, 0.75)," ", max(dat)),
  br(),
  paste0("Q1: ", quantile(dat,0.25)),
  br(),
  paste0("Q3: ", quantile(dat,0.75)),
  br(),
  paste0("IQR:",IQR(dat)),
  br(),
  paste0("Standard Deviation: ",sd(dat)),
  br(),
  paste0("Variance: ",var(dat)),
  br(),
  paste0("Range",range(dat)[2]-range(dat)[1]),
  br()
  #paste0("Boxplot", boxplot(c(dat)))
)
