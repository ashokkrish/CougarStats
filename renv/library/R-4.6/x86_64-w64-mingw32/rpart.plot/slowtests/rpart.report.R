# rpart.report.R: test code from rpart package vignette with rpart.plot package

source("test.prolog.R")

USE.PRP <- TRUE # global option

set.seed(1924)

# Not all the data in the rpart reports is (easily) available
# so sometimes we use alternative data below

cat("### Section 3\n")

# get the stagec data (you will find it in library/rpart/tests/data.stagec)
stagec <- read.table("data.stagec",
                     col.names=c("pgtime", "pgstat", "age", "eet",
                                 "g2", "grade", "gleason", "ploidy"))

progstat <- factor(stagec$pgstat, levels=0:1, labels=c("No", "Prog"))
cfit <- rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,
              data=stagec, method='class')
print(cfit)
print(rpart.rules(cfit))
par(mfrow=c(2,3))
if (USE.PRP) {
    prp(cfit, main="Section 3", uniform=F, branch=1, roundint=FALSE)
} else {
    plot(cfit, main="Section 3")
    text(cfit)
}
par(old.par)

cat("### Section 4\n")

n <- 200
temp <- c(1,1,1,0,1,1,1,
          0,0,1,0,0,1,0,
          1,0,1,1,1,0,1,
          1,0,1,1,0,1,1,
          0,1,1,1,0,1,0,
          1,1,0,1,0,1,1,
          0,1,0,1,1,1,1,
          1,0,1,0,0,1,0,
          1,1,1,1,1,1,1,
          1,1,1,1,0,1,0)
lights <- matrix(temp, 10, 7, byrow=T) # The true light pattern 0-9
set.seed(1) # For reproducibility
temp1 <- matrix(rbinom(n*7, 1, .9), n, 7) # Noisy lights
y <- rep(0:9, length=200)
temp1 <- ifelse(lights[y+1, ]==1, temp1, 1-temp1)
temp2 <- matrix(rbinom(n*17, 1, .5), n, 17) #Random lights
x <- cbind(temp1, temp2) #x is the matrix of predictors

temp3 <- rpart.control(xval=10, minbucket=2, minsplit=4, cp=0)
dfit <- rpart(y ~ x, method='class', control=temp3)
printcp(dfit)
print(rpart.rules(dfit))

fit9 <- prune(dfit, cp=.02)
print(rpart.rules(fit9, style='tall'))
print(rpart.rules(fit9, trace=1, roundint=FALSE)) # message Variable not in the model.frame

df <- data.frame(y, x)
dfit.df <- rpart(y ~ ., data=df, method='class', control=temp3)
fit9.df <- prune(dfit.df, cp=.02)
print(rpart.rules(fit9.df, cover=TRUE))
print(rpart.rules(fit9.df, cover=TRUE, roundint=FALSE))
print(rpart.rules(fit9.df, cover=TRUE, clip.facs=TRUE))
print(rpart.rules(fit9.df, cover=TRUE, roundint=FALSE, clip.facs=TRUE))

df.logical <- data.frame(y, x == 1)
dfit.df.logical <- rpart(y ~ ., data=df.logical, method='class', control=temp3)
fit9.df.logical <- prune(dfit.df.logical, cp=.02)
print(rpart.rules(fit9.df.logical, cover=TRUE))
print(rpart.rules(fit9.df.logical, cover=TRUE, roundint=FALSE))
print(rpart.rules(fit9.df.logical, cover=TRUE, clip.facs=TRUE))
print(rpart.rules(fit9.df.logical, cover=TRUE, roundint=FALSE, clip.facs=TRUE))

# trace=1 below so we can see message: Variable name 'x1' is not in splits in terms$dataClasses
# This message is printed only when trace>0
par(mfrow=c(2,4))
if (USE.PRP) {
    prp(fit9,            branch=.3, compress=T, main="Section 4 dfit\ntrace=0 roundint=FALSE",             trace=0, roundint=FALSE) # silent but isbinary() fails
    prp(fit9,            branch=.3, compress=T, main="Section 4 dfit\ntrace=1 roundint=FALSE",             trace=1, roundint=FALSE) # message Variable not in the model.frame
    prp(fit9.df,         branch=.3, compress=T, main="Section 4 dfit.df\ntrace=1, roundint=FALSE",         trace=1, roundint=FALSE) # ok
    prp(fit9.df.logical, branch=.3, compress=T, main="Section 4 dfit.df.logical\ntrace=1, roundint=FALSE", trace=1, roundint=FALSE) # ok

    prp(fit9,            branch=.3, compress=T, main="Section 4 dfit trace=0\nroundint=TRUE",     roundint=TRUE)
    prp(fit9,            branch=.3, compress=T, main="Section 4 dfit trace=1\nroundint=TRUE",     roundint=TRUE)
    prp(fit9.df,         branch=.3, compress=T, main="Section 4 dfit.df\nroundint=TRUE",          roundint=TRUE)
    prp(fit9.df.logical, branch=.3, compress=T, main="Section 4 dfit.df.logical\nroundint=TRUE",  roundint=TRUE)
} else {
    plot(fit9, branch=.3, compress=T, main="Section 4 trace=0")
    text(fit9)
}
par(old.par)

cat("### Section 5\n")

printcp(cfit)
summary(cfit, cp=.06)

cat("### Section 6\n")

fit1 <- rpart(Reliability ~ Price + Country + Mileage + Type,
              data=cu.summary, parms=list(split='gini'))
print(rpart.rules(fit1, style='tall', cover=TRUE))
fit2 <- rpart(Reliability ~ Price + Country + Mileage + Type,
              data=cu.summary, parms=list(split='information'))
print(rpart.rules(fit2))
par(mfrow=c(1,2))
if (USE.PRP) {
    prp(fit1, extra=T, main="Section 6\ngini", uniform=F, branch=1, xsep="/", under=T, roundint=FALSE)
    prp(fit2, extra=T, main="nunif", uniform=F, branch=1, under=T, roundint=FALSE)
} else {
    plot(fit1, main="Section 6\ngini"); text(fit1, use.n=T, cex=.8, xpd=NA)
    plot(fit2, main="\nunif"); text(fit2, use.n=T, cex=.8, xpd=NA)
}
par(old.par)

lmat <- matrix(c(0,4,3,0), nrow=2, ncol=2, byrow=F)
fit1 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis)
print(rpart.rules(fit1, style='wide'))
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
              parms=list(prior=c(.65,.35)))
print(rpart.rules(fit2, style='wide'))
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
              parms=list(loss=lmat))
print(rpart.rules(fit3, style='tall'))
par(mfrow=c(1,3))
if (USE.PRP) {
    prp(fit1, main="Section 6a", extra=T, type=1, uniform=F, branch=1, roundint=FALSE)
    # TODO this gives very small text, can improve by using minbranch=.5:
    # prp(fit1, main="Section 6a", extra=T, type=4, uniform=F, branch=1, roundint=FALSE)
    prp(fit2, extra=T, type=4, uniform=F, branch=1, roundint=FALSE)
    prp(fit3, extra=T, type=4, uniform=F, branch=1, yesno.yshift=-.5, roundint=FALSE) # yesno.yshift just to test
} else {
    plot(fit1, main="Section 6a"); text(fit1, use.n=T, fancy=T, xpd=NA)
    plot(fit2); text(fit2, use.n=T, fancy=T, xpd=NA)
    plot(fit3); text(fit3, use.n=T, fancy=T, xpd=NA)
}
par(old.par)

cat("### Section 7\n")

# was
# cars <- car.all[, c(1:12, 15:17, 21, 28, 32:36)]
# cars$Eng.Rev <- as.numeric(as.character(car.all$Eng.Rev2))
# now (data slightly different, results will not be identical to the document)
data(car.test.frame); cars <- car.test.frame

fit3 <- rpart(Price ~ ., data=cars)
fit3
print(rpart.rules(fit3, style='wide'))
printcp(fit3)
print(fit3, cp=.10)

summary(fit3, cp=.10)

par(mfrow=c(1,2))
if (USE.PRP) {
    prp(fit3, extra=T, main="Section 7", uniform=F, under=T, roundint=FALSE)
} else {
    plot(fit3)
    text(fit3, use.n=T, main="Section 7", xpd=NA)
}
# test negative digits (so will use standard format function)
prp(fit3, extra=T, main="Section 7 (negative digits)", uniform=F, under=T, digits=-6, roundint=FALSE)
print(rpart.rules(fit3, digits=-6)) # will treat digits-6 the same as digits=6
par(old.par)

# plot(predict(fit3), resid(fit3))
# axis(3, at=fit3$frame$yval[fit3$frame$var=='<leaf>'],
# labels=row.names(fit3$frame)[fit3$frame$var=='<leaf>'])
# mtext('leaf number', side=3, line=3)
# abline(h=0)

cfit2 <- rpart(pgstat ~ age + eet + g2 + grade + gleason + ploidy, data=stagec)
print(rpart.rules(cfit2, style='wide'))
printcp(cfit2)
print(cfit2, cp=.03)

cat("### Section 8\n")

fit <- rpart(skips ~ Opening + Solder + Mask + PadType + Panel, data=solder,
             method='poisson', control=rpart.control(cp=.05, maxcompete=2))
summary(fit, cp=.10)
par(mfrow=c(2,2))
if (USE.PRP) {
    plot(fit, main="Section 8 plot.rpart")
    text(fit, use.n=T, xpd=NA)
    rpart.plot(fit, main="Section 8 rpart.plot", under=TRUE, digits=4)
    print(rpart.rules(fit, digits=4))
} else {
    plot(fit, main="Section 8")
    text(fit, use.n=T, xpd=NA)
}

fit.prune <- prune(fit, cp=.15)
if (USE.PRP) {
    plot(fit.prune, main="Section 8a plot.rpart")
    text(fit.prune, use.n=T, xpd=NA)
    rpart.plot(fit.prune, main="Section 8a rpart.plot", under=TRUE, digits=4)
    print(rpart.rules(fit.prune))
} else {
    plot(fit.prune)
    text(fit.prune, use.n=T, xpd=NA, main="Section 8a")
}

library(survival)
fit <- rpart(Surv(stagec$pgtime, stagec$pgstat) ~ age + eet + g2 + grade +
             gleason + ploidy, data=stagec)
print(fit)
if (USE.PRP) {
    plot(fit, uniform=T, branch=.4, compress=T, main="Section 8b plot.rpart")
    text(fit, use.n=T)
    rpart.plot(fit, main="Section 8b rpart.plot", under=TRUE, digits=4)
    print(rpart.rules(fit))
} else {
    plot(fit, uniform=T, branch=.4, compress=T, main="Section 8b")
    text(fit, use.n=T)
}
summary(fit, cp=.02)

# modified for running in a script
fit2 <- prune(fit, cp=.015) # was fit2 <- snip.rpart(fit)
if (USE.PRP) {
    plot(fit2, main="Section 8c plot.rpart")
    text(fit2, use.n=T)
    rpart.plot(fit2, branch=.4, main="Section 8c rpart.plot", under=TRUE, digits=4)
    print(rpart.rules(fit2))
} else {
    plot(fit2)
    text(fit2, use.n=T, main="Section 8c")
}
par(old.par)

newgrp <- fit2$where
plot(survfit(Surv(pgtime, pgstat) ~ newgrp, data=stagec), mark.time=F, lty=1:4)
title(xlab='Time to Progression', ylab='Prob Progression')
legend(.2, .2, legend=paste('node', c(4,5,6,7)), lty=1:4)

cat("### Section 9\n")

fit <- rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy, stagec,
             control=rpart.control(cp=.025))
print(rpart.rules(fit, style='tall'))
par(mfrow=c(2,3))
if (USE.PRP) {
    prp(fit, main="Section 9", uniform=F, branch=1, roundint=FALSE)
    prp(fit, uniform=T, extra=T, type=1, main="Section 9a", roundint=FALSE)
    prp(fit, branch=0, extra=T, uniform=F, main="Section 9b", roundint=FALSE)
    prp(fit, branch=.4, uniform=T, compress=T, type=1, extra=T, main="Section 9c", roundint=FALSE)
    # post.rpart is essentially:
    prp(fit, uniform=T, branch=0.2, compress=T, Margin=0.1, type=4, extra=T, under=T, main="Section 9d", roundint=FALSE)
} else {
    plot(fit, main="Section 9")
    text(fit)

    plot(fit, uniform=T, main="Section 9a")
    text(fit, use.n=T, fancy=T)

    plot(fit, branch=0, main="Section 9b")
    text(fit, use.n=T)

    plot(fit, branch=.4, uniform=T, compress=T, main="Section 9c")
    text(fit, fancy=T, use.n=T)

    # post.rpart is essentially:
    plot(fit, uniform=T, branch=0.2, compress=T, margin=0.1, main="Section 9d")
    text(fit, fancy=T, use.n=T)
}
par(old.par)

cat("### Section 10\n")

fit <- rpart(pgtime ~ age + eet + g2 + grade + gleason + ploidy, stagec)
print(rpart.rules(fit, style='wide'))
fit$cptable
temp <- xpred.rpart(fit)
err <- (stagec$pgtime - temp)^2
sum.err <- apply(err, 2, sum)
sum.err / (fit$frame)$dev[1]

par(old.par)

source("test.epilog.R")
