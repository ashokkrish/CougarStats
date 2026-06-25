#' Z-test with one proportion.
#'
#' TODO: futher description.
#'
#' TODO: details.
#' @param x TODO: document.
#' @param n TODO: document.
#' @param p0 TODO: document.
#' @param alternative One of "two.sided", "less", or "greater".
#' @param s_level The significance level.
OnePropZTest <- function(x, n, p0, alternative = c("two.sided", "less", "greater"),  s_level = 0.05) {
  if (!(is.null(alternative) || missing(alternative))) alternative <- match.arg(alternative) else stop("Argument 'alternative' is missing or NULL!")

  phat <- x / n

  se <- sqrt((p0 * (1 - p0)) / n)

  zstat <- (phat - p0) / se

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

  dat <- sapply(c(x, n, phat, z.crit, se, zstat, p_value), function(x) {
    if (is.null(x) || is.na(x) || is.nan(x)) {
      return(NA)
    } else if (x < 0.0001 && x > 0) {
      return(signif(x, 1))
    } else {
      return(round(x, 4))
    }
  })

  names(dat) <- c("x", "n", "phat", "Z Critical", "Std Error", "Test Statistic", "P-Value")

  return(dat)
}
