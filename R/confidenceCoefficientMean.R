confidence_coefficient_mean <- function(n, 
                                        sigma,
                                        margin.error = 0.01,
                                        width = NULL) {
  if (!is.null(width)) {
    margin.error <- width / 2
  }
  if (n <= 0)
    stop("'n' must be positive.")
  if (sigma <= 0)
    stop("'sigma' must be positive.")
  if (margin.error <= 0)
    stop("'margin.error' must be positive.")
  z <- margin.error * sqrt(n) / sigma
  conf.level <- 2 * pnorm(z) - 1
  return(conf.level)
}

# Example: fixed n = 100, sigma = 15, want margin error = 2
#confidence_coefficient_mean(n = 21, sigma = 11, margin.error = 2)


