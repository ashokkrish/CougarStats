library(stats)

sample_size_clopper_pearson <- function(p0 = 0.5, conf.level = 0.95, margin.error = 0.01, width = NULL) {
  # Input validation
  if (!is.null(width) && !missing(margin.error)) {
    return(NA)
  }
  
  # Use width if provided
  if (!is.null(width)) {
    if (width <= 0) return(NA)
    margin.error <- width / 2
  }
  
  if (!is.null(margin.error) && margin.error <= 0) return(NA)
  
  alpha <- 1 - conf.level
  E <- margin.error
  n <- 1
  
  repeat {
    x <- floor(n * p0)
    
    lower <- if (x == 0) 0 else qbeta(alpha / 2, x, n - x + 1)
    upper <- if (x == n) 1 else qbeta(1 - alpha / 2, x + 1, n - x)
    
    interval_width <- upper - lower
    
    if (interval_width <= 2 * E) {
      return(n)
    }
    
    n <- n + 1
    if (n > 1e5) return(NA)
  }
}


# Use default margin.error = 0.01
sample_size_clopper_pearson()

# Specify margin of error explicitly
sample_size_clopper_pearson(p0 = 0.5, conf.level = 0.95, margin.error = 0.05)

# Specify width instead
sample_size_clopper_pearson(p0 = 0.5, conf.level = 0.95, width = 0.10)
