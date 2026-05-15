#' Z-test with two samples
#'
#' TODO: further description.
#'
#' TODO: details.
#' @param xbar1 Sample one mean.
#' @param sigma1 Population one standard deviation.
#' @param n1 Sample one size.
#' @param xbar2 Sample two mean.
#' @param sigma2 Population two standard deviation.
#' @param n2 Sample two size.
#' @param alternative One of "two.sided", "less", or "greater".
#' @param s_level The significance level.
#' @param muNaught hypothesized difference between means under H0.
#' @example
#' TwoSampZTest(10, 2.1, 15, 12, 2.3, 15, "two.sided", 0.05)
TwoSampZTest <- function(xbar1, sigma1, n1, xbar2, sigma2, n2, alternative = c("two.sided", "less", "greater"),  s_level = 0.05, muNaught = 0) {
  if (!(is.null(alternative) || missing(alternative))) alternative <- match.arg(alternative) else stop("Argument 'alternative' is missing or NULL!")

  se <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))

  zstat <- ((xbar1 - xbar2) - muNaught) / se

  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(abs(zstat), lower.tail = FALSE)
    z.crit <- qnorm((1 - s_level) + s_level / 2)
    ## z.crit<- paste0("±",  z.crit, sep = "")
  } else if (alternative == "less") {
    p_value <- pnorm(zstat, lower.tail = TRUE)
    z.crit <- -qnorm(1 - s_level)
  } else if(alternative == "greater") {
    p_value <- pnorm(zstat, lower.tail = FALSE)
    z.crit <- qnorm(1 - s_level)
  }

  dat <- sapply(c(((xbar1 - xbar2) - muNaught), z.crit, se, zstat, p_value), function(x) {
    if (x < 0.0001 && x > -1e-2) {
      signif(x, 1)
    } else {
      round(x, 4)
    }
  })

  names(dat) <- c("Difference of means", "Z Critical", "Std Error", "Test Statistic", "P-Value")

  return(dat)
}
