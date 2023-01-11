# x1 and n1
# X2 and n2
# s_level: Significance Level   

TwoPropZTest <- function(X1, n1, X2, n2, hyp_diff, alternative = c("two.sided", "less", "greater"),  s_level = 0.05)
{
  phat1 <- X1/n1
  
  phat2 <- X2/n2
  
  pooled_p <- (X1 + X2)/(n1 + n2)
    
  se <- sqrt(pooled_p*(1 - pooled_p)*((1/n1)+(1/n2)))
  
  zstat <- ((phat1 - phat2) - hyp_diff)/se
  
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
  
  dat <- round(c(phat1, phat2, pooled_p, z.crit, se, zstat, p_value), 4)
  
  names(dat) <- c("Sample Proportion 1", "Sample Proportion 1", "Pooled Proportion", "Z Critical", "Std Error", "Test Statistic", "P-Value")
  
  return(dat)
}

# Example usage:
TwoPropZTest(86, 173, 68, 115, 0, "two.sided", 0.05)

TwoPropZTest(861, 1055, 417, 974, 0, "two.sided", 0.05)
