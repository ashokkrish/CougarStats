#' Z-test with two proportions.
#'
#' TODO: futher description.
#'
#' TODO: details.
#' @param X1 TODO: document.
#' @param n1 TODO: document.
#' @param X2 TODO: document.
#' @param n2 TODO: document.
#' @param p0 TODO: document.
#' @param hyp_diff TODO: document.
#' @param alternative One of "two.sided", "less", or "greater".
#' @param s_level The significance level.
TwoPropZTest <- function(X1, n1, X2, n2, hyp_diff = 0, alternative = c("two.sided", "less", "greater"),  s_level = 0.05) {
  if (!(is.null(alternative) || missing(alternative))) alternative <- match.arg(alternative) else stop("Argument 'alternative' is missing or NULL!")

  phat1 <- X1 / n1

  phat2 <- X2 / n2

  pooled_p <- (X1 + X2) / (n1 + n2)

  se <- sqrt(pooled_p * (1 - pooled_p) * ((1 / n1) + (1 / n2)))

  zstat <- ((phat1 - phat2) - hyp_diff) / se

  if (alternative == "two.sided") {
    p_value <- 2 * pnorm(abs(zstat), lower.tail = FALSE)
    z.crit <- qnorm((1 - s_level) + s_level / 2)
  } else if (alternative == "less") {
    p_value <- pnorm(zstat, lower.tail = TRUE)
    z.crit <- -qnorm(1 - s_level)
  } else if (alternative == "greater") {
    p_value <- pnorm(zstat, lower.tail = FALSE)
    z.crit <- qnorm(1 - s_level)
  }

  # a pooled proportion of 0 or 1 gives a standard error of 0 (0/0), so the test statistic does not exist
  validate(
    need(!anyNA(c(z.crit, zstat, p_value)), "Unable to calculate the test statistic. The standard error is zero (the pooled proportion is 0 or 1) or a sample size is zero."),
    errorClass = "myClass")

  dat <- sapply(c(phat1, phat2, pooled_p, z.crit, se, zstat, p_value), function(x) {
    if (!is.na(x) && x < 0.0001 && x > 0) {
      signif(x,1)
    } else {
      round(x, 4)
    }
  })

  names(dat) <- c("Sample Proportion 1", "Sample Proportion 2", "Pooled Proportion", "Z Critical", "Std Error", "Test Statistic", "P-Value")

  return(dat)
}
