### R code from vignette source 'GPA1guide.Stex'

###################################################
### code chunk number 1: GPA1guide.Stex:22-24
###################################################
 options(continue="  ")
pdf.options(pointsize = 8)


###################################################
### code chunk number 2: GPA1guide.Stex:44-45
###################################################
library("GPArotation")


###################################################
### code chunk number 3: GPA1guide.Stex:89-100
###################################################
data("Harman", package = "GPArotation")

# Calling a rotation directly
qHarman <- quartimax(Harman8)

# Equivalently, via the wrapper function
qHarman <- GPFRSorth(Harman8, method = "quartimax")

# Two equivalent ways to access the rotated loadings
loadings(qHarman)       # via extractor function (recommended)
qHarman$loadings        # via direct list access


###################################################
### code chunk number 4: GPA1guide.Stex:112-120
###################################################
data("CCAI", package = "GPArotation")
y <- factanal(factors = 3, covmat = CCAI_R, n.obs = 461, rotation = "none")
y.quart <- quartimax(y$loadings)
max(loadings(y.quart) %*% t(y.quart$Th) - loadings(y))
y.obli <- oblimin(y$loadings, normalize = TRUE, randomStarts = 15)
max(loadings(y.obli) %*% t(y.obli$Th) - loadings(y))
# last equation on Page 678
max(loadings(y.obli) - loadings(y) %*% solve(t(y.obli$Th)))


###################################################
### code chunk number 5: GPA1guide.Stex:126-129
###################################################
y <- factanal(factors = 3, covmat = CCAI_R, n.obs = 461, rotation = "none")
y.obli <- oblimin(y$loadings, normalize = TRUE, randomStarts = 15)
max(abs(y.obli$Phi - t(y.obli$Th) %*% y.obli$Th))


###################################################
### code chunk number 6: GPA1guide.Stex:143-154
###################################################
set.seed(334)
res <- quartimin(Harman8, normalize = TRUE, randomStarts = 100)

# Raw unsorted loadings
loadings(res)

# Sorted loadings via print (factors reordered and signs adjusted)
res.sorted <- print(res)

# Once sorted, repeated calls to print are stable
max(abs(print(res.sorted)$loadings - res.sorted$loadings)) == 0  # TRUE


###################################################
### code chunk number 7: GPA1guide.Stex:193-198
###################################################
res.obli <- oblimin(Harman8, normalize = TRUE, randomStarts = 100)
# Pattern matrix (unsorted)
summary(res.obli, Structure = FALSE)
# Structure matrix
summary(res.obli, Structure = TRUE)


###################################################
### code chunk number 8: GPA1guide.Stex:241-261
###################################################
plotPatternStructure <- function(pattern, structure,
                                  labels = NULL,
                                  main = "Pattern vs Structure") {
  k   <- ncol(pattern)
  col <- palette.colors(k, palette = "Okabe-Ito")
  par(mfrow = c(1, k))
  for (j in 1:k) {
    lims <- range(c(pattern[, j], structure[, j]))
    plot(pattern[, j], structure[, j],
         xlim = lims, ylim = lims,
         xlab = "Pattern loading",
         ylab = "Structure loading",
         main = paste(if (!is.null(labels)) labels[j]
                      else paste("Factor", j)),
         pch  = 19, col = col[j])
    abline(0, 1, lty = 2, col = "grey60")
    abline(h = 0, col = "grey80")
    abline(v = 0, col = "grey80")
  }
}


###################################################
### code chunk number 9: GPA1guide.Stex:264-270
###################################################
res.obli.s  <- print(res.obli)
Pattern     <- loadings(res.obli.s)
Structure   <- Pattern %*% res.obli.s$Phi

plotPatternStructure(Pattern, Structure,
                     labels = c("Factor 1", "Factor 2"))


###################################################
### code chunk number 10: GPA1guide.Stex:295-299
###################################################
data(Thurstone, package = "GPArotation")
infomaxQ(box26, randomStarts = 100)         # 100 random starts
infomaxQ(box26, Tmat = Random.Start(3))     # single random start
infomaxQ(box26, randomStarts = 1)           # also a single random start


###################################################
### code chunk number 11: GPA1guide.Stex:328-332
###################################################
res <- geominQ(box26, normalize = TRUE, randomStarts = 100)
res$randStartChar
# Criterion value at best solution
res$Table[nrow(res$Table), "f"]


###################################################
### code chunk number 12: GPA1guide.Stex:357-413
###################################################
plotRotationLandscape <- function(A, method = "quartimax", n = 1000,
                                   main = NULL, ...) {
  # Plot the objective function landscape for 2-factor orthogonal rotation.
  # For 2 factors, all orthogonal rotations are parameterized by a single
  # angle theta in [0, 2*pi), giving a clean 1D landscape.
  #
  # Args:
  #   A      : a 2-factor unrotated loading matrix
  #   method : rotation criterion (default "quartimax")
  #   n      : number of angles to evaluate (default 1000)
  #   main   : plot title (default: "Rotation landscape: <method>")
  #   ...    : additional arguments passed to the vgQ criterion function

  if (ncol(A) != 2)
    stop("plotRotationLandscape only works for 2-factor solutions.")

  vgQfun_fn <- get(paste("vgQ", method, sep = "."),
                   envir = asNamespace("GPArotation"))

  if (is.null(main))
    main <- paste("Rotation landscape:", method)

  theta  <- seq(0, 2 * pi, length.out = n)
  f_vals <- numeric(n)

  for (i in seq_along(theta)) {
    Tmat      <- matrix(c( cos(theta[i]), sin(theta[i]),
                          -sin(theta[i]), cos(theta[i])), 2, 2)
    L         <- A %*% Tmat
    VgQ       <- do.call(vgQfun_fn, append(list(L), list(...)))
    f_vals[i] <- VgQ$f
  }

  # Find global minimum
  min_idx <- which.min(f_vals)

  plot(theta, f_vals,
       type  = "l",
       lwd   = 2,
       main  = main,
       xlab  = expression(theta ~ "(radians)"),
       ylab  = "f",
       xaxt  = "n")
  axis(1, at     = c(0, pi/2, pi, 3*pi/2, 2*pi),
           labels = c("0", expression(pi/2), expression(pi),
                      expression(3*pi/2), expression(2*pi)))
  abline(h   = f_vals[min_idx], col = "grey80", lty = 2)
  points(theta[min_idx], f_vals[min_idx],
         col = "tomato", pch = 19, cex = 1.5)
  text(theta[min_idx], f_vals[min_idx],
       labels = paste0("min f = ", round(f_vals[min_idx], 4)),
       pos    = 4,
       cex    = 0.8)

  invisible(data.frame(theta = theta, f = f_vals))
}


###################################################
### code chunk number 13: GPA1guide.Stex:419-420
###################################################
data(Harman, package = "GPArotation")


###################################################
### code chunk number 14: GPA1guide.Stex:423-428
###################################################
par(mfrow = c(2, 2))
plotRotationLandscape(Harman8, method = "quartimax")
plotRotationLandscape(Harman8, method = "varimax")
plotRotationLandscape(Harman8, method = "bentler")
plotRotationLandscape(Harman8, method = "entropy")


###################################################
### code chunk number 15: GPA1guide.Stex:455-468
###################################################
res <- plotRotationLandscape(Harman8, method = "simplimax", k = 4)

# Find all local minima by sign changes in the discrete derivative
df         <- diff(res$f)
local_mins <- which(df[-length(df)] < 0 & df[-1] > 0)

# Mark all local minima on the existing plot
points(res$theta[local_mins], res$f[local_mins],
       col = "steelblue", pch = 19, cex = 0.8)
legend("topright",
       legend = c("global minimum", "local minima"),
       col    = c("tomato", "steelblue"),
       pch    = 19, bty = "n", cex = 0.8)


###################################################
### code chunk number 16: GPA1guide.Stex:471-474
###################################################
cat("Total minima found:              ", length(local_mins), "\n")
cat("Expected due to symmetry:        ", 4, "\n")
cat("Approximate genuine local minima:", max(0, length(local_mins) - 4), "\n")


###################################################
### code chunk number 17: GPA1guide.Stex:505-536
###################################################
data(Harman, package = "GPArotation")
res.quart   <- quartimax(Harman8)
res.oblimin <- oblimin(Harman8)

L.quart   <- abs(loadings(res.quart))
L.oblimin <- abs(loadings(res.oblimin))

ord.quart   <- order(L.quart[, 1],   decreasing = TRUE)
ord.oblimin <- order(L.oblimin[, 1], decreasing = TRUE)

par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

barplot(t(L.quart[ord.quart, ]),
        beside      = TRUE,
        ylim        = c(0, 1),
        main        = "Quartimax",
        ylab        = "Absolute loading",
        xlab        = "Variable (sorted by Factor 1)",
        legend.text = c("Factor 1", "Factor 2"),
        args.legend = list(x = "topright"),
        col         = c("steelblue", "tomato"))

barplot(t(L.oblimin[ord.oblimin, ]),
        beside      = TRUE,
        ylim        = c(0, 1),
        main        = "Oblimin",
        ylab        = "Absolute loading",
        xlab        = "Variable (sorted by Factor 1)",
        legend.text = c("Factor 1", "Factor 2"),
        args.legend = list(x = "topright"),
        col         = c("steelblue", "tomato"))


###################################################
### code chunk number 18: GPA1guide.Stex:556-621
###################################################
plotSortedLoadings <- function(..., labels = NULL, col = NULL, 
                                main = "Sorted Absolute Loadings",
                                ylab = "Absolute loading", 
                                xlab = "Rank") {
  # Plot sorted absolute loadings for one or more GPArotation objects.
  # Multiple solutions are overlaid on a single plot for comparison.
  # Loadings are sorted from smallest to largest (left to right).
  #
  # Args:
  #   ...    : one or more GPArotation objects
  #   labels : character vector of legend labels (default: "Solution 1", etc.)
  #   col    : character vector of colors (default: auto-assigned)
  #   main   : plot title
  #   ylab   : y-axis label
  #   xlab   : x-axis label

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

# Example
data(Harman, package = "GPArotation")
res.quart   <- quartimax(Harman8)
res.oblimin <- oblimin(Harman8)
res.geomin  <- geominT(Harman8)



###################################################
### code chunk number 19: GPA1guide.Stex:626-628
###################################################
plotSortedLoadings(res.quart, res.oblimin, res.geomin,
                   labels = c("Quartimax", "Oblimin", "Geomin"))


###################################################
### code chunk number 20: GPA1guide.Stex:685-716
###################################################
origdigits <- options("digits")
options(digits = 2)
trBritain <- matrix(c(.783,-.163,.811,.202,.724,.209,.850,.064,
  -.031,.592,-.028,.723,.388,.434,.141,.808,.215,.709),
  byrow = TRUE, ncol = 2)
trGermany <- matrix(c(.778,-.066,.875,.081,.751,.079,.739,.092,
  .195,.574,-.030,.807,-.135,.717,.125,.738,.060,.691),
  byrow = TRUE, ncol = 2)
# orthogonal rotation of trGermany towards trBritain
trx <- targetT(trGermany, Target = trBritain)
# Factor loadings after target rotation
trx
# Differences between loadings matrices after rotation
y <- trx$loadings - trBritain
print(y, digits = 1)
# Square root of the mean squared difference per item
sqrt(apply((y^2), 1, mean))
# Square root of the mean squared difference per factor
sqrt(apply((y^2), 2, mean))
# Identity coefficient per factor after rotation
2 * colSums(trx$loadings * trBritain) /
  (colSums(trx$loadings^2) + colSums(trBritain^2))
# Additivity coefficient per factor after rotation
diag(2 * cov(trx$loadings, trBritain)) /
  diag(var(trx$loadings) + var(trBritain))
# Proportionality coefficient per factor after rotation
colSums(trBritain * trx$loadings) /
  sqrt(colSums(trBritain^2) * colSums(trx$loadings^2))
# Correlation for each factor after rotation
diag(cor(trBritain, trx$loadings))
options(digits = origdigits$digits)


###################################################
### code chunk number 21: GPA1guide.Stex:728-751
###################################################
plot(trBritain[, 1], trBritain[, 2],
     xlim = c(-0.3, 1.0), ylim = c(-0.3, 1.0),
     xlab = "Factor 1", ylab = "Factor 2",
     main = "Target Rotation: Germany towards Britain",
     pch = 19, col = "steelblue", cex = 1.2)
abline(h = 0, lty = 2, col = "grey70")
abline(v = 0, lty = 2, col = "grey70")
points(trGermany[, 1], trGermany[, 2],
       pch = 17, col = "tomato", cex = 1.2)
points(loadings(trx)[, 1], loadings(trx)[, 2],
       pch = 15, col = "orange", cex = 1.2)
for (i in 1:nrow(trGermany)) {
  arrows(trGermany[i, 1], trGermany[i, 2],
         loadings(trx)[i, 1], loadings(trx)[i, 2],
         length = 0.08, col = "grey60")
}
legend("topright",
       legend = c("Britain (varimax rotated)", 
       		"East Germany (varimax rotated)", 
		"East Germany (rotated towards Britain)"),
       col    = c("steelblue", "tomato", "orange"),
       pch    = c(19, 17, 15),
       bty    = "n")


###################################################
### code chunk number 22: GPA1guide.Stex:778-792
###################################################
A <- matrix(c(.664, .688, .492, .837, .705, .82, .661, .457, .765, .322,
  .248, .304, -0.291, -0.314, -0.377, .397, .294, .428, -0.075, .192, .224,
  .037, .155, -.104, .077, -.488, .009), ncol = 3)
# using targetT
SPA <- matrix(c(rep(NA, 6), .7, .0, .7, rep(0, 3), rep(NA, 7),
  0, 0, NA, 0, rep(NA, 4)), ncol = 3)
xt <- targetT(A, Target = SPA)
# using pstT
SPApst <- matrix(c(rep(0, 6), .7, .0, .7, rep(0, 3), rep(0, 7),
  0, 0, 0, 0, rep(0, 4)), ncol = 3)
SPAW <- matrix(c(rep(0, 6), rep(1, 6), rep(0, 7), 1, 1, 0, 1,
  rep(0, 4)), ncol = 3)
xpst <- pstT(A, Target = SPApst, W = SPAW)
max(abs(loadings(xt) - loadings(xpst)))


###################################################
### code chunk number 23: GPA1guide.Stex:810-814
###################################################
data("CCAI", package = "GPArotation")
factanal(factors = 3, covmat = CCAI_R, n.obs = 461, rotation = "infomaxT")
factanal(factors = 3, covmat = CCAI_R, n.obs = 461, rotation = "infomaxT",
  control = list(rotate = list(normalize = TRUE, eps = 1e-6)))


###################################################
### code chunk number 24: GPA1guide.Stex:822-826
###################################################
data("WansbeekMeijer", package = "GPArotation")
fa.unrotated <- factanal(factors = 3, covmat = NetherlandsTV,
                         normalize = TRUE, rotation = "none")
quartimin(loadings(fa.unrotated), normalize = TRUE)


###################################################
### code chunk number 25: GPA1guide.Stex:836-859
###################################################
data("WansbeekMeijer", package = "GPArotation")
fa.unrotated <- factanal(factors = 3, covmat = NetherlandsTV,
                         normalize = TRUE, rotation = "none")
# Two-step procedure (always correct)
set.seed(42)
fa.cf <- cfQ(loadings(fa.unrotated), kappa = 0.3, normalize = TRUE,
             randomStarts = 100)
fa.cf

if (getRversion() >= "4.5.1") {
  # Single-step via factanal (correct in R >= 4.5.1)
  set.seed(42)
  fa.factanal <- factanal(factors = 3, covmat = NetherlandsTV,
                          normalize = TRUE, rotation = "cfQ",
                          control = list(rotate = list(kappa = 0.3,
                                                       randomStarts = 100)))
  fa.sorted <- print(fa.cf, sortLoadings = TRUE)
  cat("Maximum difference in loadings:\n")
  print(max(abs(abs(fa.sorted$loadings) - abs(fa.factanal$loadings))))
} else {
  cat("Single-step factanal oblique rotation requires R >= 4.5.1.\n")
  cat("Use the two-step procedure above for correct results.\n")
}


###################################################
### code chunk number 26: GPA1guide.Stex:874-952
###################################################
  factanal_fit <- function(fa, R_obs, n) {
  # Compute common factor model fit indices from factanal output.
  # For MLE extraction only (factanal). For other estimators (minres,
  # principal axis, WLS) the chi-square statistic is not defined in
  # the same way and this function should not be used.
  #
  # Args:
  #   fa    : a factanal object (rotation = "none" recommended)
  #   R_obs : observed correlation or covariance matrix passed to factanal
  #   n     : sample size (n.obs passed to factanal)

  if (is.null(fa$STATISTIC))
    stop("factanal did not compute a chi-square statistic. ",
         "Ensure n.obs is specified when passing a covariance matrix.")

  # Ensure we work with a correlation matrix
  R_obs <- cov2cor(as.matrix(R_obs))
  p     <- nrow(R_obs) 
  L     <- loadings(fa)
  k     <- ncol(L)           # number of factors extracted
 
  # Model-implied correlation matrix using factanal uniquenesses
  # fa$uniquenesses are the MLE estimated unique variances
  R_hat <- L %*% t(L) + diag(fa$uniquenesses)
  R_hat <- cov2cor(R_hat)
  
  # Residual matrix --- off-diagonal only
  Resid <- R_obs - R_hat
  diag(Resid) <- 0

  # Test of the hypothesis that k factors are sufficient. Identical to factanal
  F_ml <- log(det(R_hat)) - log(det(R_obs)) + sum(diag(R_obs %*% solve(R_hat))) - p
  chi2 <- (n - 1 - (2 * p + 5)/6 - (2 * k)/3) * F_ml  
  df <- ((p - k)^2 - (p + k)) / 2
  pval <- pchisq(chi2, df, lower.tail = FALSE)

  # SRMR: standardized root mean square residual
  rstar.off <- sum(Resid^2) / 2
  srmr <- sqrt(rstar.off / (p * (p - 1)))

  # RMSEA: root mean square error of approximation
  rmsea <- sqrt(max(0, chi2 / (df * n) - 1 / (n - 1)))

  # Null model: all items uncorrelated
  chi2_null <- (n - 1) * sum(R_obs[lower.tri(R_obs)]^2)
  df_null   <- p * (p - 1) / 2

  # CFI: comparative fit index
  cfi <- (max(chi2_null - df_null, 0) - max(chi2 - df, 0)) /
          max(chi2_null - df_null, 0)

  # TLI: Tucker-Lewis index
  tli <- (chi2_null / df_null - chi2 / df) /
         (chi2_null / df_null - 1)

  # AIC and BIC
  aic <- chi2 - 2 * df
  bic <- chi2 - log(n) * df

  # Print results
  cat("Factor Model Fit Indices (MLE only)\n")
  cat("------------------------------------\n")
  cat(sprintf("Chi-square (df = %d):  %.3f  p = %.4f\n", df, chi2, pval))
  cat(sprintf("RMSEA:                 %.4f\n", rmsea))
  cat(sprintf("SRMR:                  %.4f\n", srmr))
  cat(sprintf("CFI:                   %.4f\n", cfi))
  cat(sprintf("TLI:                   %.4f\n", tli))
  cat(sprintf("AIC:                   %.3f\n", aic))
  cat(sprintf("BIC:                   %.3f\n", bic))
  cat("\nTop 5 absolute residuals:\n")
  resid_vals <- Resid[lower.tri(Resid)]
  print(round(sort(abs(resid_vals), decreasing = TRUE)[1:5], 4))

  invisible(c(chi2  = chi2,  df   = df,   pval = pval,
              rmsea = rmsea, srmr = srmr,
              cfi   = cfi,   tli  = tli,
              aic   = aic,   bic  = bic))
}


###################################################
### code chunk number 27: GPA1guide.Stex:958-966
###################################################
data("WansbeekMeijer", package = "GPArotation")
fa2 <- factanal(factors = 2, covmat = NetherlandsTV, rotation = "none")
fa3 <- factanal(factors = 3, covmat = NetherlandsTV, rotation = "none")
cat("=== 2 factors ===\n")
fit2 <- factanal_fit(fa2, cov2cor(NetherlandsTV$cov), n = 2154)

cat("\n=== 3 factors ===\n")
fit3 <- factanal_fit(fa3, cov2cor(NetherlandsTV$cov), n = 2154)


###################################################
### code chunk number 28: GPA1guide.Stex:969-977
###################################################
cat("Model comparison:\n")
cat(sprintf("           2-factor  3-factor\n"))
cat(sprintf("RMSEA:     %8.4f  %8.4f\n", fit2["rmsea"],  fit3["rmsea"]))
cat(sprintf("SRMR:      %8.4f  %8.4f\n", fit2["srmr"],   fit3["srmr"]))
cat(sprintf("CFI:       %8.4f  %8.4f\n", fit2["cfi"],    fit3["cfi"]))
cat(sprintf("TLI:       %8.4f  %8.4f\n", fit2["tli"],    fit3["tli"]))
cat(sprintf("AIC:       %8.2f  %8.2f\n", fit2["aic"],    fit3["aic"]))
cat(sprintf("BIC:       %8.2f  %8.2f\n", fit2["bic"],    fit3["bic"]))


