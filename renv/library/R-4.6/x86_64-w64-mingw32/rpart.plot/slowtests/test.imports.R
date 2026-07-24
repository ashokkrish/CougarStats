# test.imports.R: test packages that import or suggest rpart.plot

source("test.prolog.R")

library(DStree)
par(mfrow=c(3,3))
example(plot.DStree)
plot(fit, prob="surv", box.palette="auto", main="DStree\nbox.palette=\"auto\"")
plot(fit, prob="surv", type=1, nn=TRUE, yesno=2, box.palette="Oranges",
     main="DStree\ntype=1, nn=TRUE, yesno=2\nbox.palette=\"Oranges\"", cex.main=.9)
fit$method <- "anova"; class(fit) <- "rpart"  # hacks from DStree.plot.R
rpart.plot(fit, trace=1)
print(rpart.rules(fit))

par(old.par)

source("test.epilog.R")
