ZInterval <- function(n, xbar, sigma, c_level = 0.95)
{
  z.crit <- qnorm(c_level + (1-c_level)/2)
  
  se <- sigma/sqrt(n)
  
  margin_of_error <- z.crit*se
  
  LCL_z <- xbar - margin_of_error
  
  UCL_z <- xbar + margin_of_error
  
  dat <- sapply(c(n, xbar, sigma, z.crit, se, margin_of_error, LCL_z, UCL_z), function(x){ if(x < 0.0001 && x > 0) {signif(x,1)} else {round(x, 4)}})
  
  names(dat) <- c("Sample Size", "Sample Mean", "Population SD", "Z Critical", "Std Error", "ME", "LCL", "UCL")
  
  return(dat)
}

# Example usage:

ZInterval(18, 103.5375, 8.78, 0.90)

ZInterval(18, 103.5375, 8.78, 0.95)

ZInterval(18, 103.5375, 8.78, 0.99)
