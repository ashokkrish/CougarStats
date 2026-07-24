confidence_coefficient_mean <- function(n, 
                                        sigma,
                                        margin.error = 0.01,
                                        width = NULL) {
  if (!is.null(width)) {
    margin.error <- width / 2
  }
  
  z <- margin.error * sqrt(n) / sigma
  
  # Confidence coefficient
  conf.level <- 2 * pnorm(z) - 1
  
  return(conf.level)
}

# #Example 1:
#confidence_coefficient_mean(n = 21, sigma = 11, margin.error = 2)