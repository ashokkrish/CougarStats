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
  
  resultMatrix <- cbind(observed, expected, ominusE, ominusE2, ominusE2byE)
  resultMatrix <- rbind(resultMatrix, Total = round(colSums(resultMatrix), 4))
  colnames(resultMatrix) <- c("O", "E", "(O - E)", "(O - E)<sup>2</sup>", "(O - E)<sup>2</sup> / E")
  
  fullResults <- list(results, resultMatrix)
  names(fullResults) <- c("Results", "Matrix")
  
  return(fullResults)
}



