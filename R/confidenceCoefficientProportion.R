confidence_coefficient_proportion <- function(n,
                                              p0 = 0.5,
                                              margin.error = 0.01,
                                              width = NULL) {
  # Convert width to margin of error
  if (!is.null(width)) {
    margin.error <- width / 2
  }
  # Input checks
  if (n <= 0) {
    stop("'n' must be positive.")
  }
  if (p0 <= 0 || p0 >= 1) {
    stop("'p0' must lie strictly between 0 and 1.")
  }
  if (margin.error <= 0) {
    stop("'margin.error' must be positive.")
  }
  # Compute z-value
  z <- margin.error * sqrt(n / (p0 * (1 - p0)))
  # Confidence coefficient
  conf.level <- 2 * pnorm(z) - 1
  return(conf.level)
} 
# Example 1:
# confidence_coefficient_proportion(
#   n = 400,
#   margin.error = 0.05
# )
# # Example 2:
# confidence_coefficient_proportion(
#   n = 500,
#   p0 = 0.30,
#   width = 0.08
# )
