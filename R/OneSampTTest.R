TTest <- function(n, xbar, s, mu = 0, alternative = c("two.sided", "less", "greater"),  s_level = 0.05)
{
  if (is.null(mu) || is.na(mu)) mu <- 0
  
  se <- s/sqrt(n)
  
  tstat <- (xbar - mu)/se
  
  df <- n - 1
  
  if(alternative == 'two.sided'){
    p_value <- 2*pt(abs(tstat), df, lower.tail = FALSE)
    t.crit <- qt((1 - s_level) + s_level/2, df)
  }
  else if(alternative == 'less') {
    p_value <- pt(tstat, df, lower.tail = TRUE)
    t.crit <- -qt(1 - s_level, df)
  }
  else if(alternative == 'greater') {
    p_value <- pt(tstat, df, lower.tail = FALSE)
    t.crit <- qt(1 - s_level, df)
  }
  
  dat <- sapply(c(n, xbar, (xbar - mu), s, t.crit, se, tstat, p_value, df), function(x){ if(x < 0.0001 && x > 0) {signif(x,1)} else {round(x, 4)}})
  
  names(dat) <- c("Sample Size", "Sample Mean", "Numerator", "Sample SD", "T Critical", "Std Error", "Test Statistic", "P-Value", "df")
  
  return(dat)
}

# Example usage:

TTest(25, 73, 16, 70, "greater", 0.05)

TTest(18, 237.10, 11.28, 240, "less", 0.05)

TTest(16, 1600, 700, 2000, "less", 0.10)

TTest(8, 160.6, 50, 150, "greater", 0.05)

TTest(48, 21, 11, 30, "less", 0.05)

TTest(9, 6.5, 0.6, 6, "greater", 0.05)

TTest(15, 45, 5, 40, "greater", 0.05)

TTest(30, 2.481, 1.616, 3, "two.sided", 0.10)
