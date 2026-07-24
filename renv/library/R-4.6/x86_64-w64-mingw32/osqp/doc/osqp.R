## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(osqp)
library(Matrix)

## ----define-problem-----------------------------------------------------------
P <- Matrix(c(4., 1.,
              1., 2.), 2, 2, sparse = TRUE)
q <- c(1., 1.)
A <- Matrix(c(1., 1., 0.,
              1., 0., 1.), 3, 2, sparse = TRUE)
l <- c(1., 0., 0.)
u <- c(1., 0.7, 0.7)

## ----solve--------------------------------------------------------------------
model <- osqp(P, q, A, l, u, pars = osqpSettings(verbose = FALSE))
result <- model@Solve()

## ----inspect-result-----------------------------------------------------------
result$x
result$y
result$info$status
result$info$obj_val

## ----solve-osqp---------------------------------------------------------------
result <- solve_osqp(P, q, A, l, u, pars = osqpSettings(verbose = FALSE))
result$x

## ----update-vectors-----------------------------------------------------------
q_new <- c(2., 3.)
l_new <- c(2., -1., -1.)
u_new <- c(2., 2.5, 2.5)
model@Update(q = q_new, l = l_new, u = u_new)
result <- model@Solve()
result$x
result$info$status

## ----update-matrices----------------------------------------------------------
# Create new matrices with same sparsity pattern
P_new <- Matrix(c(5., 1.5,
                  1.5, 1.), 2, 2, sparse = TRUE)
A_new <- Matrix(c(1.2, 1.5, 0.,
                  1.1, 0., 0.8), 3, 2, sparse = TRUE)

# Update only the nonzero values
model@Update(Px = triu(P_new)@x, Ax = A_new@x)
result <- model@Solve()
result$x

## ----warm-start---------------------------------------------------------------
# Solve original problem
model <- osqp(P, q, A, l, u, pars = osqpSettings(verbose = FALSE,
                                                   warm_starting = TRUE))
res1 <- model@Solve()
cold_iters <- res1$info$iter

# Warm start with previous solution
model@WarmStart(x = res1$x, y = res1$y)
res2 <- model@Solve()
warm_iters <- res2$info$iter

cat("Cold start iterations:", cold_iters, "\n")
cat("Warm start iterations:", warm_iters, "\n")

## ----cold-start---------------------------------------------------------------
model@ColdStart()
res3 <- model@Solve()
cat("After cold start:", res3$info$iter, "iterations\n")

## ----settings-----------------------------------------------------------------
# Tighter tolerances and solution polishing
settings <- osqpSettings(
  eps_abs   = 1e-06,
  eps_rel   = 1e-06,
  polishing = TRUE,
  verbose   = FALSE
)
model <- osqp(P, q, A, l, u, pars = settings)
result <- model@Solve()
result$info$obj_val

## ----update-settings----------------------------------------------------------
model@UpdateSettings(osqpSettings(max_iter = 2000L))
pars <- model@GetParams()
pars$max_iter

## ----get-dims-----------------------------------------------------------------
dims <- model@GetDims()
cat("Variables:", dims[["n"]], " Constraints:", dims[["m"]], "\n")

## ----lasso--------------------------------------------------------------------
set.seed(1)
n <- 10   # number of features
m <- 100  # number of observations

# Generate random data
Ad <- Matrix(rnorm(m * n), m, n, sparse = FALSE)
Ad <- as(Ad, "CsparseMatrix")
x_true <- rnorm(n) * (runif(n) > 0.8) / sqrt(n)
b <- as.numeric(Ad %*% x_true + 0.5 * rnorm(m))

# Auxiliary matrices
In <- Diagonal(n)
Im <- Diagonal(m)
On <- Matrix(0, n, n, sparse = TRUE)
Onm <- Matrix(0, n, m, sparse = TRUE)

# QP formulation: variables are (x, y, t)
P <- bdiag(On, Im, On)
q <- rep(0, 2 * n + m)
A <- rbind(
  cbind(Ad, -Im, Matrix(0, m, n, sparse = TRUE)),
  cbind(In, Onm, -In),
  cbind(In, Onm,  In)
)
l <- c(b, rep(-Inf, n), rep(0, n))
u <- c(b, rep(0, n), rep(Inf, n))

# Setup model once
model <- osqp(P, q, A, l, u,
              pars = osqpSettings(warm_starting = TRUE, verbose = FALSE))

# Solve for a range of gamma values
gammas <- seq(0.1, 2.0, length.out = 10)
results <- data.frame(gamma = gammas, nnz = integer(10), obj = numeric(10))

for (i in seq_along(gammas)) {
  gamma <- gammas[i]
  q_new <- c(rep(0, n + m), rep(gamma, n))
  model@Update(q = q_new)
  res <- model@Solve()
  x_hat <- res$x[1:n]
  results$nnz[i] <- sum(abs(x_hat) > 1e-4)
  results$obj[i] <- res$info$obj_val
}

results

## ----portfolio----------------------------------------------------------------
set.seed(42)
n <- 20   # assets
k <- 5    # factors
gamma <- 1

# Random factor model
F_mat <- Matrix(rnorm(n * k) * (runif(n * k) > 0.3), n, k, sparse = TRUE)
D <- Diagonal(n, runif(n) * sqrt(k))
mu <- rnorm(n)

# QP data: variables are (x, y)
P <- bdiag(D, Diagonal(k))
q <- c(-mu / (2 * gamma), rep(0, k))
A <- rbind(
  cbind(t(F_mat), -Diagonal(k)),                          # y = F'x
  cbind(Matrix(1, 1, n, sparse = TRUE), Matrix(0, 1, k, sparse = TRUE)),  # 1'x = 1
  cbind(Diagonal(n), Matrix(0, n, k, sparse = TRUE))       # x >= 0
)
l <- c(rep(0, k), 1, rep(0, n))
u <- c(rep(0, k), 1, rep(1, n))

result <- solve_osqp(P, q, A, l, u,
                     pars = osqpSettings(verbose = FALSE, polishing = TRUE))
cat("Status:", result$info$status, "\n")

# Portfolio weights (first n variables)
weights <- result$x[1:n]
cat("Invested assets:", sum(weights > 1e-4), "of", n, "\n")
cat("Sum of weights:", round(sum(weights), 6), "\n")

## ----sparse-input-------------------------------------------------------------
# Dense matrix input works fine
P_dense <- matrix(c(4, 1, 1, 2), 2, 2)
q <- c(1, 1)
A_dense <- matrix(c(1, 1, 0, 1, 0, 1), 3, 2)
l <- c(1, 0, 0)
u <- c(1, 0.7, 0.7)

result <- solve_osqp(P_dense, q, A_dense, l, u,
                     pars = osqpSettings(verbose = FALSE))
result$x

