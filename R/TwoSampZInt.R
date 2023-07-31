# sample mean, population standard deviation and sample size
# xbar1, sigma1, and n1
# xbar2, sigma2, and n2
# c_level: Confidence Level   

TwoSampZInt <- function(xbar1, sigma1, n1, xbar2, sigma2, n2, c_level = 0.95)
{
  z.crit <- qnorm(c_level + (1-c_level)/2)
  
  se <- sqrt((sigma1^2/n1) + (sigma2^2/n2))
  
  margin_of_error <- z.crit*se
  
  LCL <- (xbar1 - xbar2) - margin_of_error
  
  UCL <- (xbar1 - xbar2) + margin_of_error
  
  dat <- round(c((xbar1 - xbar2), z.crit, se, margin_of_error, LCL, UCL), 4)
  
  names(dat) <- c("Difference of means", "Z Critical", "Std Error", "ME", "LCL", "UCL")
  
  return(dat) 
}

# Example usage:

TwoSampZInt(10, 2.1, 15, 12, 2.3, 15, 0.95)

TwoSampZInt(29.6, 5.36, 21, 33.9, 5.97, 21, 0.99)

TwoSampZInt(14.6, 0.43, 19, 15.36, 0.50, 17, 0.90)
