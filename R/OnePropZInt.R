OnePropZInterval <- function(x, n, c_level = 0.95)
{
  phat <- x/n  
  
  z.crit <- qnorm(c_level + (1-c_level)/2)
  
  se <- sqrt((phat*(1 - phat))/n)
  
  margin_of_error <- z.crit*se
  
  LCL_z <- phat - margin_of_error
  
  UCL_z <- phat + margin_of_error
  
  dat <- sapply(c(x, n, phat, z.crit, se, LCL_z, UCL_z), function(x){ if(x < 0.0001 && x > 0) {signif(x,1)} else {round(x, 4)}})
  
  names(dat) <- c("x", "n", "phat", "Z Critical", "Std Error", "LCL", "UCL")
  
  return(dat)
}

# Example usage:
#
# OnePropZInterval(348, 1268, 0.90)
# 
# OnePropZInterval(1087, 1430, 0.95)
# 
# OnePropZInterval(615, 1430, 0.95)
