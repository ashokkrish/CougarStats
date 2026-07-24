confidence_coefficient_proportion <- function(n,
                                              p0 = 0.5,
                                              margin.error = 0.01,
                                              width = NULL) {
  if (!is.null(width)) {
    margin.error <- width / 2
  }
  
  z <- margin.error * sqrt(n / (p0 * (1 - p0)))
  
  # Confidence coefficient
  conf.level <- 2 * pnorm(z) - 1
  
  return(conf.level)
}

# # Example 1:
# confidence_coefficient_proportion(n = 400, margin.error = 0.05)
# 
# # Example 2:
# confidence_coefficient_proportion(n = 500, p0 = 0.30,  width = 0.08)