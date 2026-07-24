library(quadprogXT)

Amat <- cbind(diag(3), rep(11, 3))
diag(Amat) <- 1:3

Anorm <- quadprogXT:::normalizeConstraints(Amat, bvec = rep(3, 4))
expect_true(
    all.equal(colSums(Anorm$Amat ^ 2), rep(1, ncol(Amat))),
    info = "column sums of A ^ 2 are equal to 1"
)

Amat[ , 1] <- 0
expect_error(
    quadprogXT:::normalizeConstraints(Amat, bvec = rep(3, 4)),
    info = "column sums of A ^ 2 are equal to 1"
)

