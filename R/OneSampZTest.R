ZTest <- function(n, xbar, sigma, mu = 0, alternative = c("two.sided", "less", "greater"),  s_level = 0.05)
{
  if(alternative == 'two.sided'){
    z.crit <- qnorm((1 - s_level) + s_level/2)
  }
  else if(alternative == 'less') {
    z.crit <- -qnorm(1 - s_level)
  }
  else if(alternative == 'greater') {
    z.crit <- qnorm(1 - s_level)
  }
  
  se <- sigma/sqrt(n)
  
  zstat <- (xbar - mu)/se
  
  if(alternative == 'two.sided'){
    p_value <- 2*pnorm(abs(zstat), lower.tail = FALSE)
  }
  else if(alternative == 'less') {
    p_value <- pnorm(zstat, lower.tail = TRUE)
  }
  else if(alternative == 'greater') {
    p_value <- pnorm(zstat, lower.tail = TRUE)
  }
  
  dat <- round(c(n, xbar, sigma, z.crit, se, zstat, p_value), 4)
  
  names(dat) <- c("Sample Size", "Sample Mean", "Population SD", "Z Critical", "Std Error", "TS", "P-Value")
  
  return(dat)
}

# Example usage:

ZTest(36, 6.05, 3.8, 5.1, "greater", 0.10)

ZTest(24, 15.17, 5, 12.5, "greater", 0.10)

ZTest(20, 27.1, 2.4, 25.3, "greater", 0.01)

ZTest(35, 0.66, 0.12, 0.70, "less", 0.05)

ZTest(23, 3071, 761, 3000, "less", 0.01)

ZTest(20, 262.3, 3, 264, "less", 0.01)

ZTest(10, 8.179, 0.02, 8.20, "two.sided", 0.10)
