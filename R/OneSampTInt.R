TInterval <- function(n, xbar, s, c_level = 0.95)
{
  df <- n - 1
  
  t.crit <- qt((c_level + (1-c_level)/2), df)
  
  se <- s/sqrt(n)
    
  margin_of_error <- t.crit*se
  
  LCL_t <- xbar - margin_of_error
  
  UCL_t <- xbar + margin_of_error

  dat <- sapply(c(n, xbar, s, t.crit, se, margin_of_error, LCL_t, UCL_t), function(x){ if(x < 0.0001 && x > 0) {signif(x,1)} else {round(x, 4)}})
  
  names(dat) <- c("Sample Size", "Sample Mean", "Sample SD", "T Critical", "Std Error", "ME", "LCL", "UCL")
  
  return(dat)
}

# Example usage:

TInterval(18, 103.5375, 4.78, 0.95)
