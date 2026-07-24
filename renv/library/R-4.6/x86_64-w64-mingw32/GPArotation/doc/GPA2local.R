### R code from vignette source 'GPA2local.Stex'

###################################################
### code chunk number 1: GPA2local.Stex:19-21
###################################################
options(continue="  ")
pdf.options(pointsize = 8)


###################################################
### code chunk number 2: GPA2local.Stex:57-163
###################################################
library("GPArotation")

GPFallMinima <- function(A, method = "quartimin", orthogonal = FALSE,
                         randomStarts = 100, eps = 1e-5, maxit = 1000,
                         normalize = FALSE, methodArgs = NULL,
                         minimumInclusion = 2) {
  # Runs multiple random starts and returns ALL distinct local minima,
  # not just the global minimum. Non-converged starts are discarded.
  # Minima found fewer than minimumInclusion times are excluded.
  # Minima are sorted by frequency (most common first).

  engine <- if (orthogonal) GPForth else GPFoblq

  Qvalues    <- numeric(randomStarts)
  Qconverged <- logical(randomStarts)
  all_res    <- vector("list", randomStarts)

  for (i in 1:randomStarts) {
    res <- engine(A, Tmat      = Random.Start(ncol(A)),
                     normalize  = normalize,
                     eps        = eps,
                     maxit      = maxit,
                     method     = method,
                     methodArgs = methodArgs)
    Qvalues[i]    <- res$Table[nrow(res$Table), 2]
    Qconverged[i] <- res$convergence
    all_res[[i]]  <- res
  }

  # Discard non-converged starts
  converged_idx <- which(Qconverged)
  nConverged    <- length(converged_idx)

  if (nConverged == 0)
    stop("No starts converged. Consider increasing maxit or relaxing eps.")

  Qvalues_conv <- Qvalues[converged_idx]
  all_res_conv <- all_res[converged_idx]

  # Bin converged criterion values into equivalence classes
  Q_round  <- round(Qvalues_conv / eps) * eps
  Q_unique <- unique(Q_round)

  # Build one representative solution per unique minimum
  minima <- vector("list", length(Q_unique))
  for (j in seq_along(Q_unique)) {
    idx        <- which(Q_round == Q_unique[j])
    count      <- length(idx)
    proportion <- count / nConverged
    minima[[j]] <- list(
      result     = all_res_conv[[idx[1]]],
      f          = Q_unique[j],
      count      = count,
      proportion = proportion
    )
  }

  # Sort by count (most common first)
  ord    <- order(sapply(minima, `[[`, "count"), decreasing = TRUE)
  minima <- minima[ord]

  # Filter out minima found fewer than minimumInclusion times
  keep   <- sapply(minima, `[[`, "count") >= minimumInclusion
  minima <- minima[keep]

  if (length(minima) == 0)
    stop("No minima found with count >= minimumInclusion (", minimumInclusion,
         "). Consider reducing minimumInclusion or increasing randomStarts.")

  # Summary data frame
  f_values <- sapply(minima, `[[`, "f")
  f_global <- min(f_values)

  summary_df <- data.frame(
    minimum    = seq_along(minima),
    f          = round(f_values, 6),
    deltaF     = round(f_values - f_global, 6),
    count      = sapply(minima, `[[`, "count"),
    proportion = round(sapply(minima, `[[`, "proportion"), 3),
    isGlobal   = f_values == f_global
  )

  result <- list(
    minima           = minima,
    summary          = summary_df,
    Qvalues          = Qvalues_conv,
    nConverged       = nConverged,
    nStarts          = randomStarts,
    method           = method,
    orthogonal       = orthogonal,
    minimumInclusion = minimumInclusion
  )
  class(result) <- "GPFallMinima"
  result
}

print.GPFallMinima <- function(x, ...) {
  cat("Random start analysis:", x$nConverged, "of", x$nStarts,
      "starts converged\n")
  cat("Distinct minima found:", nrow(x$summary),
      "(minimumInclusion =", x$minimumInclusion, ")\n\n")
  print(x$summary, row.names = FALSE)
  cat("\nGlobal minimum: f =", min(x$summary$f), "\n")
  cat("Access full solutions via $minima[[i]]$result\n")
  invisible(x)
}


###################################################
### code chunk number 3: GPA2local.Stex:212-230
###################################################
library("GPArotation")
data("CCAI", package = "GPArotation")
fa_unrotated <- factanal(factors = 3, covmat = CCAI_R, 
      n.obs = 461, rotation = "none")
A <- loadings(fa_unrotated)

# Oblimin: highly stable
res_oblimin <- oblimin(A, normalize = TRUE, randomStarts = 200)
cat("Oblimin random start diagnostics:\n")
res_oblimin$randStartChar

# Simplimax: complex landscape
set.seed(42)
res_ccai <- GPFallMinima(A, method = "simplimax",
                         randomStarts = 200,
                         normalize = TRUE,
                         minimumInclusion = 2)
res_ccai


###################################################
### code chunk number 4: GPA2local.Stex:254-256
###################################################
# Print the most common solution
print(res_ccai$minima[[1]]$result)


###################################################
### code chunk number 5: GPA2local.Stex:262-264
###################################################
# Print solution in row 3 of the summary
print(res_ccai$minima[[3]]$result, digits = 2)


###################################################
### code chunk number 6: GPA2local.Stex:269-271
###################################################
i <- 3
print(res_ccai$minima[[i]]$result, digits = 2)


###################################################
### code chunk number 7: GPA2local.Stex:278-281
###################################################
# Print the global minimum using the GPArotation S3 print method
global <- which(res_ccai$summary$isGlobal)
print(res_ccai$minima[[global]]$result, digits = 2)


###################################################
### code chunk number 8: GPA2local.Stex:284-286
###################################################
# Summary with structure matrix for oblique solution
summary(res_ccai$minima[[global]]$result, Structure = TRUE, digits = 2)


###################################################
### code chunk number 9: GPA2local.Stex:308-344
###################################################
plotSortedLoadings <- function(..., labels = NULL, col = NULL,
                                main = "Sorted Absolute Loadings",
                                ylab = "Absolute loading",
                                xlab = "Rank") {
  solutions <- list(...)
  for (i in seq_along(solutions)) {
    if (!inherits(solutions[[i]], "GPArotation"))
      stop("Argument ", i, " is not a GPArotation object.")
  }
  n <- length(solutions)
  if (is.null(labels))
    labels <- paste("Solution", seq_len(n))
  if (is.null(col))
    col <- palette.colors(n, palette = "Okabe-Ito")
  sorted_loadings <- lapply(solutions, function(x)
    sort(abs(as.vector(x$loadings)), decreasing = FALSE))
  all_values <- unlist(sorted_loadings)
  max_len    <- max(sapply(sorted_loadings, length))
  plot(NULL,
       xlim = c(1, max_len),
       ylim = c(0, max(all_values)),
       main = main,
       xlab = xlab,
       ylab = ylab,
       las  = 1)
  abline(h = seq(0, 1, by = 0.1), col = "grey90", lty = 1)
  for (i in seq_len(n)) {
    lines(seq_along(sorted_loadings[[i]]), sorted_loadings[[i]],
          col = col[i], lwd = 2)
    points(seq_along(sorted_loadings[[i]]), sorted_loadings[[i]],
           col = col[i], pch = 19, cex = 0.6)
  }
  legend("topleft", legend = labels, col = col, lwd = 2, pch = 19,
         bty = "n")
  invisible(sorted_loadings)
}


###################################################
### code chunk number 10: GPA2local.Stex:349-354
###################################################
do.call(plotSortedLoadings,
        c(lapply(res_ccai$minima, function(x) x$result),
          list(labels = paste0("Min ", res_ccai$summary$minimum,
                               " (f=", round(res_ccai$summary$f, 4),
                               ", n=", res_ccai$summary$count, ")"))))


