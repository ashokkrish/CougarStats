library(stats)
confidence_coefficient_cp <- function(n, p0 = 0.5, margin.error = 0.01) {
  target_width <- 2 * margin.error
  f <- function(alpha) {
    x <- floor(n * p0)
    lower <- if (x == 0) 0 else qbeta(alpha / 2, x, n - x + 1)
    upper <- if (x == n) 1 else qbeta(1 - alpha / 2, x + 1, n - x)
    (upper - lower) - target_width
  }
  # Search for alpha such that interval width = target_width
  res <- uniroot(f, interval = c(1e-10, 0.4))   # alpha up to 0.4 (i.e., 60% CI)
  alpha <- res$root
  conf.coef <- 1 - alpha
  return(conf.coef)
}

# Example: fixed n = 500, want margin error = 0.05
confidence_coefficient_cp(n = 500, margin.error = 0.05)
