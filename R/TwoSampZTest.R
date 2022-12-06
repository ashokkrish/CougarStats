# sample mean, population standard deviation and sample size
# xbar1, sigma1, and n1
# xbar2, sigma2, and n2
# s_level: Significance Level   

TwoSampZTest <- function(xbar1, sigma1, n1, xbar2, sigma2, n2, alternative = c("two.sided", "less", "greater"),  s_level = 0.05)
{
  se <- sqrt((sigma1^2/n1) + (sigma2^2/n2))
  
  zstat <- (xbar1 - xbar2)/se
  
  if(alternative == 'two.sided'){
    p_value <- 2*pnorm(abs(zstat), lower.tail = FALSE)
    z.crit <- qnorm((1 - s_level) + s_level/2)
    #z.crit<- paste0("±",  z.crit, sep = "")
  }
  else if(alternative == 'less') {
    p_value <- pnorm(zstat, lower.tail = TRUE)
    z.crit <- -qnorm(1 - s_level)
  }
  else if(alternative == 'greater') {
    p_value <- pnorm(zstat, lower.tail = FALSE)
    z.crit <- qnorm(1 - s_level)
  }
  
  dat <- round(c((xbar1 - xbar2), z.crit, se, zstat, p_value), 4)
  
  # if(alternative == 'two.sided'){
  #   names(dat) <- c("Difference of means", "± Z Critical", "Std Error", "TS", "P-Value")
  # }
  # else
  # {
    names(dat) <- c("Difference of means", "Z Critical", "Std Error", "Test Statistic", "P-Value")
  # }
  return(dat) 
}

# Example usage:

TwoSampZTest(10, 2.1, 15, 12, 2.3, 15, "two.sided", 0.05)

