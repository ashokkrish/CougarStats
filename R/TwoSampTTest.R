#' T-test with two samples
#'
#' TODO: further description.
#'
#' TODO: details.
#' @param xbar1 Sample one mean.
#' @param s1 Sample one standard deviation.
#' @param n1 Sample one size.
#' @param xbar2 Sample two mean.
#' @param s2 Sample two standard deviation.
#' @param n2 Sample two size.
#' @param var.equal whether or not population variances can be assumed equal. Default is TRUE.
#' @param alternative One of "two.sided", "less", or "greater".
#' @param s_level The significance level.
#' @param muNaught hypothesized difference between means under H0.
#' @examples
#' TwoSampTTest(98, 3.76, 16, 96.3, 3.85, 15, TRUE, "greater", 0.05)
#' TwoSampTTest(29.6, 4.36, 25, 33.9, 4.97, 25, TRUE, "less", 0.01)
#' TwoSampTTest(29.6, 5.36, 21, 33.9, 5.97, 21, TRUE, "two.sided", 0.05)
#' TwoSampTTest(145.75, 24.3706, 8, 146.5, 21.2939, 8, TRUE, "two.sided", 0.01)
TwoSampTTest <- function(xbar1, s1, n1, xbar2, s2, n2, var.equal = TRUE, alternative = c("two.sided", "less", "greater"),  s_level = 0.05, muNaught = 0) {
  if (!(is.null(alternative) || missing(alternative))) alternative <- match.arg(alternative) else stop("Argument 'alternative' is missing or NULL!")

  if(var.equal == TRUE) {
    ## sp is the pooled standard deviation
    sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
    se <- sp * sqrt((1 / n1 + 1 / n2))

    df <- n1 + n2 - 2
  } else if(var.equal == FALSE) {
    se <- sqrt((s1^2 / n1) + (s2^2 / n2))

    ## Welch-Satterthwaite df
    df <- ((s1^2 / n1 + s2^2 / n2)^2) / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
  }

  tstat <- ((xbar1 - xbar2) - muNaught) / se

  if (alternative == "two.sided") {
    p_value <- 2 * pt(abs(tstat), df, lower.tail = FALSE)
    t.crit <- qt((1 - s_level) + s_level / 2, df)
  } else if (alternative == "less") {
    p_value <- pt(tstat, df, lower.tail = TRUE)
    t.crit <- -qt(1 - s_level, df)
  } else if (alternative == "greater") {
    p_value <- pt(tstat, df, lower.tail = FALSE)
    t.crit <- qt(1 - s_level, df)
  }

  ##If var.equal == TRUE then sp must be printed in dat: PENDING

  dat <- sapply(c(((xbar1 - xbar2) - muNaught), t.crit, df, se, tstat, p_value), function(x) {
    if (x < 0.0001 && x > -1e-2) {
      signif(x, 1)
    } else {
      round(x, 4)
    }
  })
  names(dat) <- c("Difference of means", "T Critical", "df", "Std Error", "Test Statistic", "P-Value")

  return(dat)
}
