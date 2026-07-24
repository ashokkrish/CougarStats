## ----setup,include=FALSE------------------------------------------------------
library("lava")
knitr::opts_chunk$set(
  collapse = TRUE,
  cache = TRUE,
  comment = "#>",
  dev = "svg",
  fig.ext = "svg"
  )
has_mets <- lava:::versioncheck('mets', 1)
has_geepack <- lava:::versioncheck('geepack', 1)

## ----estimate.syntax, eval=FALSE----------------------------------------------
# estimate(x=, ...)
# estimate(coef=, IC=, ...)
# estimate(coef=, vcov=, ...)

## ----estimate.examples, eval=FALSE--------------------------------------------
# merge(subset(estimate(x), 1), estimate(coef=p, IC=ic, id=id)) |>
#   transform(function(x) c(exp(x), exp(x[1]))) |> # parameter transformation
#   labels(c("a", "b")) # rename parameters

## ----estimate.direct, eval=FALSE----------------------------------------------
# c(exp(b)^0.5, exp(b * a) / (1 + exp(-b)))

## ----sim_model----------------------------------------------------------------
m <- lvm() |>
  regression(y1 ~ x1 + a + w) |>
  regression(y2 ~ x2 + a + w) |>
  regression(y3 ~ x3 + a + w) |>
  regression(y4 ~ x4 + a + w) |>
  regression(a ~ w) |>
  distribution(~ y1 + y2 + y3 + y4 + a, value = binomial.lvm()) |>
  distribution(~id, value = Sequence.lvm(integer = TRUE))

## ----simulate-----------------------------------------------------------------
n <- 4e2
dw <- sim(m, n, seed = 1) |>
  transform(y3 = y3 * ifelse(id > n / 2, NA, 1))
Print(dw)
## Data in long format
dl <- reshape(dw,
        varying = list(paste0("y",1:4),
                       paste0("x",1:4)),
        v.names=c("y", "x"), direction="long") |>
  na.omit()
dl <- dl[order(dl$id), ]
## dl <- mets::fast.reshape(dw, varying = c("y", "x")) |> na.omit()
Print(dl)

## ----inp1---------------------------------------------------------------------
inp <- as.matrix(dw[, c("y1", "y2")])
e <- estimate(inp[, 1, drop = FALSE], type="mean")
class(e)
e

## ----ic1----------------------------------------------------------------------
IC(e) |> Print()

## ----inp2---------------------------------------------------------------------
estimate(inp)

## ----mlm----------------------------------------------------------------------
e <- lm(cbind(y1, y2) ~ 1, data = dw) |>
  estimate()
IC(e) |> head()

## ----estimatemethods----------------------------------------------------------
summary(e)
## extract parameter coefficients
coef(e)
## ## Asymptotic (robust) variance estimate
vcov(e)
## Matrix with estimates and confidence limits
estimate(e, null=0, level = 0.99) |> parameter()
## Influence curve
IC(e) |> head()
## Join estimates
ee <- merge(e, e)
ee
## Forest plots
plot(ee, null=0.5, digits=2)

## ----glm----------------------------------------------------------------------
g <- glm(y1 ~ a + x1, data = dw, family = binomial)
estimate(g)

## ----glm.std------------------------------------------------------------------
estimate(g, vcov = vcov(g))
# estimate(g, vcov = TRUE) alternative syntax to obtain model-based SEs

## ----ifglm--------------------------------------------------------------------
IC(g) |> head()

## ----ordreg-------------------------------------------------------------------
ordreg(y1 ~ a + x1, dw, family=binomial(logit)) |> estimate()

## ----mets---------------------------------------------------------------------
library("survival")
data(pbc, package="survival")

## ----phreg, eval=has_mets-----------------------------------------------------
fit.phreg <- mets::phreg(Surv(time, status > 0) ~ age + sex, data = pbc)
fit.phreg
IC(fit.phreg) |> head()

## ----phreg-baseline, eval=has_mets--------------------------------------------
baseline <- function(object, time, ...) {
  ic <- mets::IC(object, baseline = TRUE, time = time, ...)
  est <- mets::predictCumhaz(object$cumhaz, new.time = time)[1, 2]
  estimate(NULL, coef = est, IC = ic, labels = paste0("chaz:", time))
}
tt <- 2000
baseline(fit.phreg, tt)

## ----survreg, eval=has_mets---------------------------------------------------
survival::survreg(Surv(time, status > 0) ~ age + sex, data = pbc, dist="weibull") |>
  estimate()

## ----semfit, eval=has_mets----------------------------------------------------
sem <- lvm(y1 + y2 ~ 1 * u + w) |>
  latent(~ u) |>
  ordinal(K=2, ~ y1 + y2)
semfit <- estimate(sem, data = dw)

## Robust standard errors
estimate(semfit)

## ----quantiles----------------------------------------------------------------
eq <- estimate(dw[, c("w", "x1")], type = "quantile", probs = c(0.25, 0.5, 0.75))
eq
IC(eq) |> head()

## ----glmmarg------------------------------------------------------------------
g1 <- glm(y1 ~ a, family=binomial, data=dw)
g2 <- glm(y2 ~ a, family=binomial, data=dw)
e <- merge(g1, g2)
summary(e)

## ----hypo1--------------------------------------------------------------------
estimate(e, cbind(0,1,0,-1), null=0)

## ----glmmargmis---------------------------------------------------------------
g2 <- glm(y2 ~ 1, family = binomial, data = dw)
summary(g2)
dwc <- na.omit(dw)
g3 <- glm(y3 ~ 1, family = binomial, data = dwc)
summary(g3)

e2 <- estimate(g2, id = dw$id)
e3 <- estimate(g3, id = "id", data=dwc)

ecomb <- merge(e2, e3)
IC(ecomb) |> Print()
vcov(ecomb)
## Same marginals as
list(vcov(e2), vcov(e3))

## ----merge--------------------------------------------------------------------
merge(e2, e3, id = list(dw$id, dwc$id))

## ----estimatenoid-------------------------------------------------------------
estimate(g2) |>
  IC() |> head()
vcov(merge(estimate(g2), estimate(g3)))
merge(estimate(g2), estimate(g3)) |>
  (rownames %++% head %++% IC)()

## ----merge_idnull-------------------------------------------------------------
merge(g1, g2, id = NULL) |> (Print %++% IC)()
merge(g1, g2, id = NULL) |> vcov()

## ----mergekeep----------------------------------------------------------------
merge(g1, g2, keep = c("(Intercept)", "(Intercept).1"))

## ----merge2-------------------------------------------------------------------
merge(g1, g2, keep=c(1, 3))

## ----merge3-------------------------------------------------------------------
merge(g1, g2, keep = "cept", regex = TRUE)
merge(g1, g2, keep = c("\\)$", "^a$"), regex = TRUE, ignore.case = TRUE)

## ----merge4-------------------------------------------------------------------
merge(g1, g2, labels = c("a", "b", "c")) |> estimate(keep = c("a", "c"))
merge(g1, g2,
      labels = c("a", "b", "c"),
      keep = c("a", "c")
)
estimate(g1, labels=c("a", "b"))

## ----merge5-------------------------------------------------------------------
merge(g1, g2, subset="(Intercept)")

## ----cluster1-----------------------------------------------------------------
g0 <- glm(y ~ a + w + x, data = dl, family = binomial())

## ----cluster2-----------------------------------------------------------------
estimate(g0, id=dl$id)

## ----geepack, eval=has_geepack------------------------------------------------
gee0 <- geepack::geeglm(y ~ a + w + x, data = dl, id = dl$id, family=binomial)
summary(gee0)

## ----aggregate----------------------------------------------------------------
set.seed(1)
y <- cbind(rnorm(1e5))
N <- 2e2 ## Number of aggregated groups, the number of observations in the new IF
id <- foldr(nrow(y), N, list=FALSE)
Print(cbind(table(id)))

## Aggregated IF
e <- estimate(cbind(y), id = id)
object.size(e)
e

## ----delta1-------------------------------------------------------------------
estimate(g1, sum)
estimate(g1, function(p) list(a = sum(p))) # named list
## Multiple parameters
estimate(g1, function(x) c(x, x[1] + exp(x[2]), inv = 1 / x[2]))
estimate(g1, exp)

## ----estimate_calculus_models-------------------------------------------------
a <- estimate(coef=c("a"=0.5), IC=scale(rnorm(10)), id=1:10)
b <- estimate(coef=c("b"=0.8), IC=scale(rnorm(10)), id=1:10)

## ----estimate_calc_ex---------------------------------------------------------
a * b
(3 * cos(a) / sqrt(b) + 1) / a
c(sum=sum(e), sum2=a+b,
  prod=prod(e), prod2=a*b) # sum and prod function
e <- c(a,b) # merge
e %*% e # inner prod.
c(1, 2) %*% e
c(pow = a^b) # power-function, rename parameter
a^c(0.5, 2)
c(e["a"] * e["b"] / a, e["b"])

## ----estimate-contrast--------------------------------------------------------
B <- rbind(c(1,-1), c(1,0), c(0,1))
B %*% e
B %*% e == c(1,1,0)

## -----------------------------------------------------------------------------
lava::logit
logit(b)
expit(c(a,b))

## ----estimate-pipe------------------------------------------------------------
merge(a, b) |>  # merges the two `estimate` objects
  transform(prod) |> # calculates product of parameter estimates
  subset(1) |> # nothing happens here as the result was already 1-dim.
  labels("prod") # rename parameter

## ----estimate-with------------------------------------------------------------
e <- c("e1"=a, "e2"=b)
with(e, c(est = e1*e2))

## ----cov----------------------------------------------------------------------
Cov <- function(x, y, ...) {
  est <- mean(x * y)-mean(x)*mean(y)
    estimate(
      coef = est,
      IC = (x - mean(x)) * (y - mean(y)) - est,
      ...
    )
}
with(dw, Cov(x1, x2))

## ----cov2---------------------------------------------------------------------
est <- lm(cbind(x1, x2, x1 * x2) ~ 1, data = dw) |>
  estimate(labels = c("E1", "E2", "E12"))
est

est["E12"] - est["E2"]*est["E1"]
# transform(e1, function(x) c(x, cov=with(as.list(x), E12 - E2* E1))) # Same result

## ----rho----------------------------------------------------------------------
v12 <- with(dw, Cov(x1, x2, id = id))
v1  <- with(dw, Cov(x1, x1, id = id))
v2  <- with(dw, Cov(x2, x2, id = id))

rho <- c(rho = v12 / sqrt(v1 * v2))
rho

## ----tanh---------------------------------------------------------------------
estimate(atanh(rho), back.transform = tanh)

## ----estglmmerge--------------------------------------------------------------
g <- lapply(
  list(y1 ~ a, y2 ~ a, y3 ~ a), #, y4 ~ a+x4),
  function(f) glm(f, family = binomial, data = dw)
)
gg <- Reduce(merge, g)
gg

## ----contrast1----------------------------------------------------------------
B <- cbind(0,1, 0,-1, 0,0)
estimate(gg, B)

## ----estcontrast1-------------------------------------------------------------
estimate(gg, B, null=1)

## ----estcontrast2-------------------------------------------------------------
B <- rbind(cbind(0,1, 0,-1, 0,0),
           cbind(0,1, 0,0, 0,-1))
estimate(gg, B)

## -----------------------------------------------------------------------------
estimate(gg, a + a.1, 2*a - a.2, a, null=c(2,1,1))

## ----contr--------------------------------------------------------------------
contr(list(1, c(1, 2), c(1, 4)), n = 5)

## ----pairwise.diff------------------------------------------------------------
pairwise.diff(3)
estimate(gg, pairwise.diff(3), null=c(1,1,1), use=c(2,4,6))

## ----pcorrect, eval=has_mets--------------------------------------------------
gg0 <- estimate(gg, use="^a", regex=TRUE, null=rep(.8, 3))
alpha_zmax(gg0)

## ----closedtesting, eval=has_mets---------------------------------------------
closed_testing(gg0, test = test_wald)

## ----estpred------------------------------------------------------------------
g <- glm(y1 ~ a + x1 + w, data=dw, family=binomial)
pr <- function(p, data, ...)
  with(data, expit(p[1] + p["a"] + p["x1"]*x1 + p["w"]*w))
pr(coef(g), dw) |> head()

## ----average------------------------------------------------------------------
id <- foldr(NROW(dw), 100, list=FALSE)
ea <- estimate(g, pr, average=TRUE, id=id)
ea
IC(ea) |> head()

## ----ate-nuisancemodels-------------------------------------------------------
qmod <- glm(y1 ~ a * w, family = binomial, data = dw) # E(Y|W,A) := Q(W,A)
amod <- glm(a ~ w, family = binomial, data = dw) # P(A=1|W) = Pi_1(W)
q0 <- predict(qmod, transform(dw, a=0), type="response") # Q(W,0)
q1 <- predict(qmod, transform(dw, a=1), type="response") # Q(W,1)
p1 <- predict(amod, dw, type="response") # P(A=1|W)
e0 <- with(dw, (1-a) / (1-p1) * (y1 - q0) + q0)
e1 <- with(dw, a / p1 * (y1 - q1) + q1)
head(cbind(e0, e1))

## ----est-potentialoutcomes----------------------------------------------------
est0 <- estimate(coef=mean(e0), IC=e0-mean(e0)) # E[Y(0)]
est1 <- estimate(coef=mean(e1), IC=e1-mean(e1)) # E[Y(1)]

potential_outcomes <- merge(est0, est1, paired=TRUE, labels=c("y(0)", "y(1)"))
potential_outcomes
head(IC(potential_outcomes))
vcov(potential_outcomes)

## ----est-ate------------------------------------------------------------------
estimate(potential_outcomes, cbind(-1, 1), labels="ate")

## ----est-ate-or---------------------------------------------------------------
est <- with(potential_outcomes, logit(`y(1)`)-logit(`y(0)`))
est
transform(est, labels="OR", back.transform=exp)
#logor <- function(p) logit(p[2]) - logit(p[1])
#transform(potential_outcomes, logor, labels="logOR")
#transform(potential_outcomes, logor, labels="OR", back.transform=exp)

## ----est-cate-----------------------------------------------------------------
# amod <- targeted::learner_glm(a ~ w, family=binomial)
# qmod <- targeted::learner_glm(y1 ~ a * w, family=binomial)
# est <- targeted::cate(qmod, amod, data=dw, second.order = FALSE)
# #>             Estimate Std.Err    2.5%  97.5%   P-value
# #> E[y1(1)]      0.7050 0.03721 0.63207 0.7779 4.934e-80
# #> E[y1(0)]      0.5359 0.03535 0.46666 0.6052 6.290e-52
# #> ───────────
# #> (Intercept)   0.1691 0.04996 0.07115 0.2670 7.138e-04

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

