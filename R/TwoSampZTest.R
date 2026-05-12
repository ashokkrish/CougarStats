# sample mean, population standard deviation and sample size
# xbar1, sigma1, and n1
# xbar2, sigma2, and n2
# s_level: Significance Level   

TwoSampZTest <- function(xbar1, sigma1, n1, xbar2, sigma2, n2, alternative = c("two.sided", "less", "greater"),  s_level = 0.05, muNaught = 0)
{
  # try to resolve the alternative; match.arg will error if the actual argument is invalid
  alternative_resolved <- tryCatch(
    match.arg(alternative),
    error = function(e) {
      stop("`alternative` must be one of: \"two.sided\", \"less\", \"greater\" (received: ",
        paste0(alternative, collapse = ", "), ").", 
        call. = FALSE)
    }
  )
  se <- sqrt((sigma1^2/n1) + (sigma2^2/n2))
  
  zstat <- ((xbar1 - xbar2) - muNaught)/se
  
  if(alternative_resolved == 'two.sided'){
    p_value <- 2*pnorm(abs(zstat), lower.tail = FALSE)
    z.crit <- qnorm((1 - s_level) + s_level/2)
    #z.crit<- paste0("±",  z.crit, sep = "")
  }
  else if(alternative_resolved == 'less') {
    p_value <- pnorm(zstat, lower.tail = TRUE)
    z.crit <- -qnorm(1 - s_level)
  }
  else if(alternative_resolved == 'greater') {
    p_value <- pnorm(zstat, lower.tail = FALSE)
    z.crit <- qnorm(1 - s_level)
  }
  
  dat <- sapply(c(((xbar1 - xbar2) - muNaught), z.crit, se, zstat, p_value), function(x){ if(x < 0.0001 && x > -1e-2) {signif(x,1)} else {round(x, 4)}})
  
  names(dat) <- c("Difference of means", "Z Critical", "Std Error", "Test Statistic", "P-Value")
  
  return(dat) 
}

# Example usage:

# TwoSampZTest(10, 2.1, 15, 12, 2.3, 15, "two.sided", 0.05)

