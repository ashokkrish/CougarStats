## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----cs-fit-------------------------------------------------------------------
library(lme4)
fm1.cs <- lmer(Reaction ~ Days + cs(1 + Days | Subject), sleepstudy)

## ----getReCovs----------------------------------------------------------------
print(fm1.cs_cov <- getReCovs(fm1.cs))

## ----getInfo------------------------------------------------------------------
getME(fm1.cs, "par")
getME(fm1.cs, "theta")

## ----lambda-mat---------------------------------------------------------------
library(Matrix)
image(getME(fm1.cs, "Lambda"))

## ----varcorr------------------------------------------------------------------
vc_mat <- VarCorr(fm1.cs)
vc_mat$Subject

## ----comparison-setup---------------------------------------------------------
if (!requireNamespace("glmmTMB", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(glmmTMB)
}

## Often want to ignore attributes and class.
## Set a fairly large tolerance for convenience.
all.equal.nocheck <- function(x, y, tolerance = 3e-5, ..., check.attributes = FALSE, check.class = FALSE) {
  require("Matrix", quietly = TRUE)
  ## working around mode-matching headaches
  if (is(x, "Matrix")) x <- matrix(x)
  if (is(y, "Matrix")) y <- matrix(y)
  all.equal(x, y, ..., tolerance = tolerance, check.attributes = check.attributes, check.class = check.class)
}

get.cor1 <- function(x) {
  v <- VarCorr(x)
  vv <- if (inherits(x, "merMod")) v$group else v$cond$group
  attr(vv, "correlation")[1,2]
}

## ----unstruc-sim--------------------------------------------------------------
n_groups <- 20
n_per_group <- 20
n <- n_groups * n_per_group

set.seed(1)
dat.us <- data.frame(
  group = rep(1:n_groups, each = n_per_group),
  x1 = rnorm(n),
  x2 = rnorm(n)
)
## Constructing a similar dataset for the other examples
gdat.us <- dat.diag <- gdat.diag <- dat.us

form <- y ~ 1 + x1 * x2 + us(1 + x1|group)
dat.us$y <- simulate(form[-2], 
                    newdata = dat.us,
                    family = gaussian,
                    newparams = list(beta = c(-7, 5, -100, 20),
                                     theta = c(2.5, 1.4, 6.3),
                                     sigma = 2))[[1]]

form2 <- y ~ 1 + x1 + us(1 + x1|group)
gdat.us$y <- simulate(
  form2[-2],
  newdata = gdat.us,
  family = binomial,
  newparams = list(
    beta  = c(-1.5, 0.5),     
    theta = c(0.3, 0.1, 0.3)
  ))[[1]]

## ----compare-lmm--------------------------------------------------------------
lme4.us <- lmer(form, data = dat.us, REML = "FALSE")
glmmTMB.us <- glmmTMB(form, dat = dat.us)

## Fixed effects
fixef(lme4.us); fixef(glmmTMB.us)$cond
all.equal.nocheck(fixef(lme4.us), fixef(glmmTMB.us)$cond)

## Sigma
sigma(lme4.us); sigma(glmmTMB.us)
all.equal.nocheck(sigma(lme4.us), sigma(glmmTMB.us))

## Log likelihoods
logLik(lme4.us); logLik(glmmTMB.us)
all.equal.nocheck(logLik(lme4.us), logLik(glmmTMB.us))

## ----compare-cov--------------------------------------------------------------
## Variance-Covariance Matrix
vcov(lme4.us); vcov(glmmTMB.us)$cond
all.equal.nocheck(vcov(lme4.us), vcov(glmmTMB.us)$cond)

## Variance and Covariance Components
all.equal.nocheck(VarCorr(lme4.us)$group,
          VarCorr(glmmTMB.us)$cond$group)

## Conditional Modes of the Random Effects
all.equal.nocheck(ranef(lme4.us)$group, ranef(glmmTMB.us)$cond$group)

## ----compare-glmm-------------------------------------------------------------
glme4.us <- glmer(form2, data = gdat.us, family = binomial)
gglmmTMB.us <- glmmTMB(form2, dat = gdat.us, family = binomial)

## Fixed effects
fixef(glme4.us); fixef(gglmmTMB.us)$cond
all.equal.nocheck(fixef(glme4.us), fixef(gglmmTMB.us)$cond)

## Sigma
all.equal.nocheck(sigma(glme4.us), sigma(gglmmTMB.us))

## Log likelihoods
logLik(glme4.us); logLik(gglmmTMB.us)
all.equal.nocheck(logLik(glme4.us), logLik(gglmmTMB.us))

## ----compare-glmm-cov---------------------------------------------------------
## Variance-Covariance Matrix
vcov(glme4.us); vcov(gglmmTMB.us)$cond
all.equal.nocheck(vcov(glme4.us), vcov(gglmmTMB.us)$cond)

## Variance and Covariance Components
all.equal.nocheck(VarCorr(glme4.us)$group,
          VarCorr(gglmmTMB.us)$cond$group)

## Conditional Modes of the Random Effects
all.equal.nocheck(ranef(glme4.us)$group, ranef(gglmmTMB.us)$cond$group)

## ----fit-homog-diag, eval = FALSE---------------------------------------------
# lme4.us <- lmer(Reaction ~ Days + diag(Days | Subject, hom = TRUE), sleepstudy)
# glmmTMB.us <- glmmTMB(Reaction ~ Days + homdiag(Days | Subject), sleepstudy)

## ----sim-het-diag-------------------------------------------------------------
form <- y ~ 1 + x1 * x2 + diag(1|group)
dat.diag$y <- simulate(form[-2], 
                       newdata = dat.diag,
                       family = gaussian,
                       newparams = list(beta = c(-7, 5, -100, 20),
                                        theta = c(2.5),
                                        sigma = 2))[[1]]

## ----fit-het-diag-------------------------------------------------------------
lme4.diag <- lmer(form, data = dat.diag, REML = "FALSE")
glmmTMB.diag <- glmmTMB(form, dat = dat.diag)

## Fixed effects
fixef(lme4.diag); fixef(glmmTMB.diag)$cond
all.equal.nocheck(fixef(lme4.diag), fixef(glmmTMB.diag)$cond)

## Sigma
sigma(lme4.diag); sigma(glmmTMB.diag)
all.equal.nocheck(sigma(lme4.diag), sigma(glmmTMB.diag))

## Log likelihoods
logLik(lme4.diag); logLik(glmmTMB.diag)
all.equal.nocheck(logLik(lme4.diag), logLik(glmmTMB.diag))

## Variance-Covariance Matrix
vcov(lme4.diag); vcov(glmmTMB.diag)$cond
all.equal.nocheck(vcov(lme4.diag), vcov(glmmTMB.diag)$cond)

## Variance and Covariance Components
all.equal.nocheck(VarCorr(lme4.diag)[[1]], 
          VarCorr(glmmTMB.diag)$cond$group)

## Conditional Modes of the Random Effects
all.equal.nocheck(ranef(lme4.diag)$group, ranef(glmmTMB.diag)$cond$group)

## ----cs-fit-2, eval = FALSE---------------------------------------------------
# lme4.us <- lmer(Reaction ~ Days + cs(Days | Subject, hom = TRUE), sleepstudy)
# glmmTMB.us <- glmmTMB(Reaction ~ Days + cs(Days | Subject), sleepstudy)

## ----simgroup-----------------------------------------------------------------
simGroup <- function(g, n=6, phi=0.6) {
  x <- MASS::mvrnorm(mu = rep(0,n),
                     Sigma = phi^as.matrix(dist(1:n)) )  
  y <- x + rnorm(n)                              
  times <- factor(1:n)
  group <- factor(rep(g,n))
  data.frame(y, times, group)
}

set.seed(1)
dat.cs <- do.call("rbind", lapply(1:2000, simGroup))

## ----more-comp, cache=TRUE----------------------------------------------------
lme4.cs <- lmer(y ~ times + cs(0 + times | group, hom = TRUE), data = dat.cs, REML = FALSE)
glmmTMB.cs <- glmmTMB(y ~ times + homcs(0 + times | group), data = dat.cs)

## Fixed effects
fixef(lme4.cs); fixef(glmmTMB.cs)$cond
all.equal.nocheck(fixef(lme4.cs), fixef(glmmTMB.cs)$cond)

## Sigma
sigma(lme4.cs); sigma(glmmTMB.cs)
all.equal.nocheck(sigma(lme4.cs), sigma(glmmTMB.cs))

## Log likelihoods
logLik(lme4.cs); logLik(glmmTMB.cs)
all.equal.nocheck(logLik(lme4.cs), logLik(glmmTMB.cs))

## Variance-Covariance Matrix
all.equal.nocheck(vcov(lme4.cs), vcov(glmmTMB.cs)$cond)

## Variance and Covariance Components
all.equal.nocheck(VarCorr(lme4.cs)[[1]], 
          VarCorr(glmmTMB.cs)$cond$group)

## Conditional Modes of the Random Effects
all.equal.nocheck(ranef(lme4.cs)$group, ranef(glmmTMB.cs)$cond$group)

## Comparing against the predicted rho value
lme4.rho <- get.cor1(lme4.cs)
glmmTMB.rho <- get.cor1(glmmTMB.cs)
lme4.rho; glmmTMB.rho
all.equal.nocheck(lme4.rho, glmmTMB.rho)

## ----ar1-sim------------------------------------------------------------------
set.seed(1)
dat.ar1 <- do.call("rbind", lapply(1:2000, function(g) simGroup(g, phi = 0.7)))

## ----het-ar1-fit, eval = FALSE------------------------------------------------
# lme4.ar1 <- lmer(y ~ times + ar1(0 + times | group), data = dat.ar1, REML = FALSE)
# glmmTMB.ar1 <- glmmTMB(y ~ times + hetar1(0 + times | group), data = dat.ar1)

## ----ar1-fit, cache=TRUE------------------------------------------------------
lme4.ar1 <- lmer(y ~ times + ar1(0 + times | group, hom = TRUE), data = dat.ar1, REML = FALSE)
glmmTMB.ar1 <- glmmTMB(y ~ times + ar1(0 + times | group), data = dat.ar1)

## Fixed effects
fixef(lme4.ar1); fixef(glmmTMB.ar1)$cond
all.equal.nocheck(fixef(lme4.ar1), fixef(glmmTMB.ar1)$cond)

## Sigma
sigma(lme4.ar1); sigma(glmmTMB.ar1)
all.equal.nocheck(sigma(lme4.ar1), sigma(glmmTMB.ar1))

## Log likelihoods
logLik(lme4.ar1); logLik(glmmTMB.ar1)
all.equal.nocheck(logLik(lme4.ar1), logLik(glmmTMB.ar1))

## Variance-Covariance Matrix
all.equal.nocheck(vcov(lme4.ar1), vcov(glmmTMB.ar1)$cond)

## Variance and Covariance Components
all.equal.nocheck(VarCorr(lme4.ar1)$group, 
                  VarCorr(glmmTMB.ar1)$cond$group)

## Conditional Modes of the Random Effects
all.equal.nocheck(ranef(lme4.ar1)$group, ranef(glmmTMB.ar1)$cond$group)

## Comparing against the predicted rho value
lme4.ar1.rho <- get.cor1(lme4.ar1)
glmmTMB.ar1.rho <- get.cor1(glmmTMB.ar1)
lme4.ar1.rho; glmmTMB.ar1.rho
all.equal.nocheck(lme4.ar1.rho, glmmTMB.ar1.rho)

