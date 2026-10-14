# Smallest sample size n (searching n = 1, 2, ..., max.n) for which the exact
# Clopper-Pearson interval, with x = floor(n * p0) successes, is no wider than
# 2 * margin.error. Returns NA when no n up to max.n is large enough.
sample_size_clopper_pearson <- function(p0 = 0.5, conf.level = 0.95, margin.error = 0.01, max.n = 1e5) {
  if (is.na(margin.error) || margin.error <= 0) return(NA)

  alpha  <- 1 - conf.level
  target <- 2 * margin.error

  # Width of the exact interval for every n in the vector `n`
  cp_width <- function(n) {
    x     <- floor(n * p0)
    lower <- numeric(length(n))
    upper <- rep(1, length(n))

    has_lower <- x > 0
    has_upper <- x != n
    lower[has_lower] <- qbeta(alpha / 2, x[has_lower], n[has_lower] - x[has_lower] + 1)
    upper[has_upper] <- qbeta(1 - alpha / 2, x[has_upper] + 1, n[has_upper] - x[has_upper])

    upper - lower
  }

  # Lower bound for the interval width over every n in [n1, n2]. Both limits of
  # the exact interval increase with x and decrease with n - x, and x = floor(n * p0)
  # and n - x never decrease as n grows. So no n in the block can be narrower than
  # upper(x at n1, n2 - x at n2) - lower(x at n2, n1 - x at n1).
  min_width_in_block <- function(n1, n2) {
    x1 <- floor(n1 * p0)
    x2 <- floor(n2 * p0)
    if (n2 - x2 <= 0) return(-Inf)
    
    lower <- if (x2 == 0) 0 else qbeta(alpha / 2, x2, n1 - x1 + 1)
    upper <- qbeta(1 - alpha / 2, x1 + 1, n2 - x2)
    upper - lower
  }
  
  # Blocks that provably contain no valid n are skipped (the block size grows
  # while skipping and shrinks near the answer); the remaining n are checked
  # exactly, in increasing order, so the first valid n is always the one found.
  min_block <- 8
  max_block <- 8192
  tolerance <- target * (1 + 1e-9) + 1e-9

  n     <- 1
  block <- 64

  while (n <= max.n) {
    n_end <- min(n + block - 1, max.n)

    if (isTRUE(min_width_in_block(n, n_end) > tolerance)) {
      n     <- n_end + 1
      block <- min(block * 2, max_block)
    } else if (block > min_block) {
      block <- block %/% 2
    } else {
      candidates <- n:n_end
      hit        <- which(cp_width(candidates) <= target)

      if (length(hit) > 0) return(as.numeric(candidates[hit[1]]))

      n <- n_end + 1
    }
  }

  NA
}
