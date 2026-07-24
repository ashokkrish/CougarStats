### R code from vignette source 'GPA3bifactor.Stex'

###################################################
### code chunk number 1: GPA3bifactor.Stex:20-23
###################################################
options(continue="  ")
pdf.options(pointsize = 8)
library("GPArotation")


###################################################
### code chunk number 2: GPA3bifactor.Stex:69-83
###################################################
data("WansbeekMeijer", package = "GPArotation")
fa.unrotated <- factanal(factors = 3, covmat = NetherlandsTV,
                         rotation = "none")

# Orthogonal bifactor rotation
res.bifT <- bifactorT(loadings(fa.unrotated))
print(res.bifT, sortLoadings = FALSE, digits = 3)

# Oblique bifactor rotation
res.bifQ <- bifactorQ(loadings(fa.unrotated))
print(res.bifQ, sortLoadings = FALSE, digits = 3)

# Structure matrix for oblique solution
summary(res.bifQ, Structure = TRUE)


###################################################
### code chunk number 3: GPA3bifactor.Stex:116-140
###################################################
omega_h <- function(bifactor_result) {
  # Compute omega hierarchical from a GPArotation bifactor solution.
  # Args:
  #   bifactor_result : a GPArotation object from bifactorT or bifactorQ
  L     <- loadings(bifactor_result)
  lg    <- L[, 1]       # general factor loadings
  Ls    <- L[, -1]      # group factor loadings
  theta <- 1 - rowSums(L^2)  # model-implied unique variances
  sum(lg)^2 / (sum(lg)^2 + sum(Ls^2) + sum(theta))
}

# Omega total: proportion of total score variance due to all common factors.
# Requires the observed or population correlation matrix R.
omega_t <- function(bifactor_result, R) {
  L     <- loadings(bifactor_result)
  R_hat <- L %*% t(L)
  sum(R_hat) / sum(R)
}

# Coefficient alpha from a correlation matrix
alpha_coef <- function(R) {
  k <- nrow(R)
  k / (k - 1) * (1 - sum(diag(R)) / sum(R))
}


###################################################
### code chunk number 4: GPA3bifactor.Stex:166-191
###################################################
# Example 1: Strong general factor (loadings .7), weak group factors (.2)
lambda_g1  <- rep(0.7, 12)
lambda_s1a <- c(rep(0.2, 4), rep(0.0, 8))
lambda_s1b <- c(rep(0.0, 4), rep(0.2, 4), rep(0.0, 4))
lambda_s1c <- c(rep(0.0, 8), rep(0.2, 4))

L1 <- cbind(lambda_g1, lambda_s1a, lambda_s1b, lambda_s1c)
R1 <- L1 %*% t(L1)
diag(R1) <- 1

fa1  <- factanal(factors = 4, covmat = R1, rotation = "none")
bif1 <- bifactorT(loadings(fa1))

# Example 2: Weaker general factor (loadings .3), stronger group factors (.6)
lambda_g2  <- rep(0.3, 12)
lambda_s2a <- c(rep(0.6, 4), rep(0.0, 8))
lambda_s2b <- c(rep(0.0, 4), rep(0.6, 4), rep(0.0, 4))
lambda_s2c <- c(rep(0.0, 8), rep(0.6, 4))

L2 <- cbind(lambda_g2, lambda_s2a, lambda_s2b, lambda_s2c)
R2 <- L2 %*% t(L2)
diag(R2) <- 1

fa2  <- factanal(factors = 4, covmat = R2, rotation = "none")
bif2 <- bifactorT(loadings(fa2))


###################################################
### code chunk number 5: GPA3bifactor.Stex:194-203
###################################################
cat("Example 1 - Strong general factor:\n")
cat("  alpha   =", round(alpha_coef(R1), 3), "\n")
cat("  omega_t =", round(omega_t(bif1, R1), 3), "\n")
cat("  omega_h =", round(omega_h(bif1), 3), "\n\n")

cat("Example 2 - Weaker general factor:\n")
cat("  alpha   =", round(alpha_coef(R2), 3), "\n")
cat("  omega_t =", round(omega_t(bif2, R2), 3), "\n")
cat("  omega_h =", round(omega_h(bif2), 3), "\n")


###################################################
### code chunk number 6: GPA3bifactor.Stex:268-269
###################################################
data("CCAI", package = "GPArotation")


###################################################
### code chunk number 7: GPA3bifactor.Stex:272-275
###################################################
cat("Range of observed correlations:\n")
cat("  Min:", round(min(CCAI_R[lower.tri(CCAI_R)]), 3), "\n")
cat("  Max:", round(max(CCAI_R[lower.tri(CCAI_R)]), 3), "\n")


###################################################
### code chunk number 8: GPA3bifactor.Stex:283-287
###################################################
fa_unrotated <- factanal(factors = 3, covmat = CCAI_R, 
         n.obs = 461, rotation = "none")
bif <- bifactorT(loadings(fa_unrotated))
print(bif, sortLoadings = FALSE, digits = 3)


###################################################
### code chunk number 9: GPA3bifactor.Stex:306-311
###################################################
cat("alpha   =", round(alpha_coef(CCAI_R), 3), "\n")
cat("omega_t =", round(omega_t(bif, CCAI_R), 3), "\n")
cat("omega_h =", round(omega_h(bif), 3), "\n")
cat("gap (omega_t - omega_h) =",
    round(omega_t(bif, CCAI_R) - omega_h(bif), 3), "\n")


###################################################
### code chunk number 10: GPA3bifactor.Stex:321-336
###################################################
omega_h_by_group <- function(bifactor_result, R) {
  L     <- loadings(bifactor_result)
  lg    <- L[, 1]
  Ls    <- L[, -1]
  theta <- 1 - rowSums(L^2)
  denom <- sum(lg)^2 + sum(Ls^2) + sum(theta)
  cat("Variance partition:\n")
  cat("  General factor:    ", round(sum(lg)^2 / denom, 3), "\n")
  for (j in 1:ncol(Ls))
    cat("  Group factor", j + 1, ":     ",
        round(sum(Ls[, j]^2) / denom, 3), "\n")
  cat("  Measurement error: ", round(sum(theta) / denom, 3), "\n")
  cat("  Total (omega_t):   ",
      round(omega_t(bifactor_result, R), 3), "\n")
}


###################################################
### code chunk number 11: GPA3bifactor.Stex:339-340
###################################################
omega_h_by_group(bif, CCAI_R)


