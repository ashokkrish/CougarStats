ZInterval <- function(n, xbar, sigma, c_level = 0.95)
{
  z.crit <- qnorm(c_level + (1-c_level)/2)
  
  se <- sigma/sqrt(n)
  
  margin_of_error <- z.crit*se
  
  LCL_z <- xbar - margin_of_error
  
  UCL_z <- xbar + margin_of_error
  
  dat <- round(c(xbar, z.crit, se, LCL_z, UCL_z), 4)
  
  names(dat) <- c("Sample Mean", "Z Critical", "Std Error", "LCL", "UCL")
  
  return(dat)
}

# Example usage:

ZInterval(18, 103.5375, 8.78, 0.90)

ZInterval(18, 103.5375, 8.78, 0.95)

ZInterval(18, 103.5375, 8.78, 0.99)
