library(dplyr)
ChiSquareTest <- function(tableData, correction = FALSE) {
  
  # print(typeof(tableData))
  # print(tableData)
  
  results <- chisq.test(x = tableData, correct = correction)
  observed <- t(t(as.vector(results$observed)))
  expected <- round(t(t(as.vector(results$expected))), 4)
  ominusE <- round(observed - expected, 4)
  ominusE2 <- round(ominusE^2, 4)
  ominusE2byE <- round(ominusE2/expected, 4)
  residuals <- round(t(t(as.vector(results$residuals))), 4)
  
  # Yates'-corrected values
  ominusE2Yates <- round((abs(ominusE) - 0.5)^2, 4)
  ominusE2byEYates <- round(((abs(ominusE) - 0.5)^2) / expected, 4)
  
  # always produce matrix with both yates' and non-yates' columns
  resultMatrix <- cbind(observed, expected, ominusE, ominusE2, ominusE2byE, ominusE2Yates, ominusE2byEYates, residuals)
  resultMatrix <- rbind(resultMatrix, Total = round(colSums(resultMatrix), 4))
  colnames(resultMatrix) <- c("O", "E", "(O - E)", "(O - E)<sup>2</sup>", "(O - E)<sup>2</sup> / E", "(|O - E| - 0.5)<sup>2</sup>", "(|O - E| - 0.5)<sup>2</sup> / E", "Standardized Residuals")
  
  fullResults <- list(results, resultMatrix)
  names(fullResults) <- c("Results", "Matrix")
  
  return(fullResults)
}