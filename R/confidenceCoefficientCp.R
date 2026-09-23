confidence_coefficient_cp <- function(n,
                                      p0 = 0.5,
                                      margin.error = 0.01) {
  
  target_width <- 2 * margin.error
  
  f <- function(alpha) {
    x <- floor(n * p0)
    lower <- if (x == 0) 0 else qbeta(alpha / 2, x, n - x + 1)
    upper <- if (x == n) 1 else qbeta(1 - alpha / 2, x + 1, n - x)
    (upper - lower) - target_width
  }
  
  lower_alpha <- 1e-10
  upper_alpha <- 0.4
  
  f_lower <- f(lower_alpha)
  f_upper <- f(upper_alpha)
  
  # No confidence coefficient can achieve the requested width
  if (f_lower * f_upper > 0) {
    return(NULL)
  }
  # No confidence coefficient can achieve the requested width
  if (f_lower * f_upper > 0) {
    return(NULL)
  }
  
  # Search for alpha such that interval width = target_width
  res <- uniroot(f, interval = c(1e-10, 0.4))   # alpha up to 0.4 (i.e., 60% CI)
  alpha <- res$root
  
  # Confidence coefficient
  conf.coef <- 1 - alpha # Consider changing this to conf.level and return it in the next line
  
  return(conf.coef)
}

# # Example 1:
# confidence_coefficient_cp(n = 500, margin.error = 0.05)