library(quadprogXT)

Amat <- matrix(c(-4,-3,0,2,1,0,0,-2,1),3,3)
AmatSparse <- rbind(Amat[1,], 0, 0, 0, 0, Amat[2:3,])
AindCompact <- rbind(c(2,2,2),c(1,1,2),c(2,2,3))
AmatCompact <- rbind(c(-4,2,-2),c(-3,1,1))
target <- list(Amat = AmatCompact, Aind = AindCompact)

expect_equal(
  current = convertToCompact(Amat),
  target = target,
  info = "Conversion to compact program yields same results as quadprog example."
)

expect_error(
  current = convertToCompact(Amat * 0),
  info = "Conversion to compact program fails when there is > 0 columns of all 0s."
)

expect_equal(
  current = convertToCompact(AmatSparse)$Amat,
  target = target$Amat,
  info = "Conversion to compact program ignores unconstrained variables in Amat."
)
