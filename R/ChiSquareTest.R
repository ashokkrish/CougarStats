library(dplyr)
ChiSquareTest <- function(tableData, correction = FALSE) {
  
  # print(typeof(tableData))
  # print(tableData)
  
  results <- chisq.test(x = tableData, correct = correction)

  # Matrix without Yates correction
  observed <- t(t(as.vector(results$observed)))
  expected <- round(t(t(as.vector(results$expected))), 4)
  ominusE <- round(observed - expected, 4)
  ominusE2 <- round(ominusE^2, 4)
  ominusE2byE <- round(ominusE2/expected, 4)
  residuals <- round(t(t(as.vector(results$residuals))), 4)
  
  resultMatrix <- cbind(observed, expected, ominusE, ominusE2, ominusE2byE, residuals)
  colnames(resultMatrix) <- c("O", "E", "(O - E)", "(O - E)<sup>2</sup>", "(O - E)<sup>2</sup> / E", "Standardized Residuals")
  
  # Matrix with Yates correction
  if (correction){
    oMinusE2Yates <- round((abs(ominusE) - 0.5)^2, 4)
    oMinusE2byEYates <- round(oMinusE2Yates / expected, 4)
    
    resultMatrix <- cbind(observed, expected, ominusE, oMinusE2Yates, oMinusE2byEYates, ominusE2, ominusE2byE, residuals)
    colnames(resultMatrix) <- c("O", "E", "(O - E)", "(|O - E| - 0.5)<sup>2</sup>", "(|O - E| - 0.5)<sup>2</sup> / E", "(O - E)<sup>2</sup>", "(O - E)<sup>2</sup> / E", "Standardized Residuals")
  }
  
  resultMatrix <- rbind(resultMatrix, Total = round(colSums(resultMatrix), 4))
  fullResults <- list(results, resultMatrix)
  names(fullResults) <- c("Results", "Matrix")
  
  return(fullResults)
}



