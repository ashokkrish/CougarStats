OnePropZTest <- function(X, n, p0, alternative = c("two.sided", "less", "greater"),  s_level = 0.05)
{
  phat <- X/n
  
  se <- sqrt(p0*(1 - p0)/n)
  
  zstat <- (phat - p0)/se
  
  if(alternative == 'two.sided'){
    p_value <- 2*pnorm(abs(zstat), lower.tail = FALSE)
    z.crit <- qnorm((1 - s_level) + s_level/2)
    # z.crit1 <- -qnorm((1 - s_level) + s_level/2)
    # z.crit2 <- qnorm((1 - s_level) + s_level/2)
    # print(c(z.crit1, z.crit2))
  }
  else if(alternative == 'less') {
    p_value <- pnorm(zstat, lower.tail = TRUE)
    z.crit <- -qnorm(1 - s_level)
  }
  else if(alternative == 'greater') {
    p_value <- pnorm(zstat, lower.tail = FALSE)
    z.crit <- qnorm(1 - s_level)
  }
  
  dat <- round(c(X, n, phat, z.crit, se, zstat, p_value), 4)
  
  names(dat) <- c("X", "n", "Sample Proportion", "Z Critical", "Std Error", "Test Statistic", "P-Value")
  
  return(dat)
}

# Example usage:

OnePropZTest(16, 250, 0.12, "two.sided", 0.05)
