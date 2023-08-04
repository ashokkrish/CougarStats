# sample mean, sample standard deviation and sample size
# xbar1, s1, and n1
# xbar2, s2, and n2
# var.equal: whether or not population variances can be assumed equal. Default is TRUE.
# c_level: Confidence Level   

TwoSampTInt <- function(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, c_level = 0.95)
{
  if(var.equal == TRUE)
  {
    # sp is the pooled standard deviation
    sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
    #print(sp)
    se <- sp*sqrt((1/n1 + 1/n2)) 
    
    df <- n1 + n2 - 2
  } else
  {
    se <- sqrt((s1^2/n1) + (s2^2/n2))
    
    # Welch-Satterthwaite df
    df <- ((s1^2/n1 + s2^2/n2)^2)/((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
  }  
  
  t.crit <- qt((1 - c_level)/2, df = df, lower.tail = FALSE)
  
  margin_of_error <- t.crit*se
  
  LCL <- (xbar1 - xbar2) - margin_of_error
  
  UCL <- (xbar1 - xbar2) + margin_of_error
  
  dat <- sapply(c((xbar1 - xbar2), t.crit, df, se, margin_of_error, LCL, UCL), function(x){ if(x < 0.0001 && x > 0) {signif(x,1)} else {round(x, 4)}})
  
  names(dat) <- c("Difference of means", "T Critical", "df", "Std Error", "ME", "LCL", "UCL")
  
  return(dat) 
}

# Example usage:

# TwoSampTInt(10, 2.1, 15, 12, 2.3, 15, TRUE, 0.95)
# 
# TwoSampTInt(0.0297, 0.01, 10, 0.0313, 0.01, 10, TRUE, 0.95)
# 
# TwoSampTInt(29.6, 5.36, 21, 33.9, 5.97, 21, TRUE, 0.95)
# 
# TwoSampTInt(14.6, 0.43, 19, 15.36, 0.50, 17, TRUE, 0.90)
