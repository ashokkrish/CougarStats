# test.rpart.rules.R (also tests rpart.predict and roundint argument)

source("test.prolog.R")
old.width <- options("width")

example(rpart.rules)
title("example(rpart.rules)")
example(print.rpart.rules)
title("example(print.rpart.rules)")

# test rpart.model.frame with good and bad data
par(mfrow=c(2, 2))
data(trees)
trees1 <- trees * 10 # so all values integral
Volume <- rpart(formula=Volume~Girth, data=trees1, cp=.001)
rules <- rpart.rules(Volume, digits=4)
rpart.plot(Volume, digits=4, main="Volume\ndata still available")
print(rules)
# can't use digits in print.rpart.rules (instead must specify digits in rpart.rules)
expect.err(try(print(rpart.rules(Volume), digits=4)), "specify 'digits' in rpart.rules (not in print.rpart.rules)")
expect.err(try(print(rpart.rules(Volume), digit=4)),  "specify 'digits' in rpart.rules (not in print.rpart.rules)")
expect.err(try(print(rpart.rules(Volume), dig=4)),    "specify 'digits' in rpart.rules (not in print.rpart.rules)")
trees1 <- "bad data"
options(warn=2) # treat warnings as errors
expect.err(try(rpart.rules(Volume, digits=4)), "(converted from warning) Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).")
expect.err(try(rpart.plot(Volume, digits=4, main="Volume\ntrees not available")), "Cannot retrieve the data used to build the model")
expect.err(try(prp(Volume, digits=4, main="Volume\ntrees not available")), "Cannot retrieve the data used to build the model")
rpart.plot(Volume, digits=4, roundint=FALSE, main="Volume roundint=FALSE\ndata not available")
trees1 <- trees[,1:2] # only part of the data
expect.err(try(rpart.rules(Volume, digits=4)), "(converted from warning) Cannot retrieve the data used to build the model (model.frame: invalid type (list) for variable 'Volume').")
print(rpart.rules(Volume, digits=4, roundint=FALSE, trace=.5)) # trace print: Cannot retrieve the data used to build the model (model.frame: invalid type (list) for variable 'Volume')
trees1 <- trees[,2:3] # only part of the data
print(rpart.rules(Volume, digits=4, roundint=FALSE, trace=.5)) # trace print: Cannot retrieve the data used to build the model (model.frame: object 'Girth' not found)

# test with rpart argument model=TRUE (also use space in predictor name for testing node label parsing)
trees1 <- trees * 10 # so all values integral
trees1[["Girth Around"]] <- trees1$Girth # space in predictor name
trees1$Girth <- NULL
options(warn=1) # print warnings as they occur (rpart gives a warning partial argument match of 'length' to 'length.out')
Volume.modTRUE <- rpart(formula=Volume~., data=trees1, cp=.001, model=TRUE)
options(warn=2) # treat warnings as errors
rules <- rpart.rules(Volume.modTRUE, digits=4)
print(rules)
trees1 <- "bad data"
print(rpart.rules(Volume.modTRUE, digits=4)) # ok, trees1 is ignored
rpart.plot(Volume.modTRUE, digits=4, main="Volume.modTRUE\ndata not available")
rpart.plot(Volume.modTRUE, digits=4, roundint=FALSE, main="Volume.modTRUE roundint=FALSE\ndata not available")
par(old.par)

cat0("\n=== test digits, varlen, faclen, trace ===\n\n")

options(warn=1) # print warnings as they occur (rpart gives a warning: partial argument match of 'length' to 'length.out')
mileage <- rpart(Mileage ~ ., data=cu.summary)
options(warn=2) # treat warnings as errors

options(warn=1234) # for testing that rpart.rules doesn't mess up options(warn)
print(rpart.rules(mileage))
stopifnot(options("warn") == 1234) # make sure rpart.rules didn't mess up options(warn)
options(warn=1) # print warnings as they occur
print(rpart.rules(mileage, digits=0, trace=1)) # this also tests print(rules)
print(rpart.rules(mileage, digits=2, trace=2))
print(rpart.rules(mileage, digits=3))
print(rpart.rules(mileage, digits=10))
print(rpart.rules(mileage, digits=-10)) # should be same as digits=10
stopifnot(identical(rpart.rules(mileage, digits=-10), rpart.rules(mileage, digits=10)))
expect.err(try(rpart.rules(mileage, digits=99)), "digits=99 but it should be between -22 and 22")

print(rpart.rules(mileage, faclen=1))
print(rpart.rules(mileage, faclen=2))
print(rpart.rules(mileage, faclen=-1))
print(rpart.rules(mileage, faclen=-2, trace=TRUE))
print(rpart.rules(mileage, faclen=-4))
print(rpart.rules(mileage, faclen=-4, clip.facs=TRUE))

# rpart.predict (anova model)
stopifnot(identical(options("width"), old.width))
owidth <- options("width")
options(width=1e3)
print(predict(mileage)[1:5])
print(rpart.predict(mileage)[1:5])
print(head(rpart.predict(mileage, rules=TRUE, when="")))
print(head(rpart.predict(mileage, rules=TRUE, when="WHEN")))
print(head(rpart.predict(mileage, rules=TRUE, when="", because="reason:")))
print(head(rpart.predict(mileage, rules=TRUE, because="")))
stopifnot(identical(predict(mileage), rpart.predict(mileage)))
print(rpart.predict(mileage, rules=TRUE)[12:14,])
# use max and not identical because rpart.predict(mileage, rules=TRUE)[,1] doesn't have names
stopifnot(max(abs(predict(mileage) - rpart.predict(mileage, rules=TRUE)[,1])) == 0)
stopifnot(identical(as.numeric(predict(mileage)), rpart.predict(mileage, rules=TRUE)[,1]))
print(rpart.predict(mileage, nn=TRUE)[12:14,])
stopifnot(max(abs(predict(mileage) - rpart.predict(mileage, nn=TRUE)[,1])) == 0)
print(rpart.predict(mileage, nn=TRUE, rules=TRUE)[12:14,])
stopifnot(max(abs(predict(mileage) - rpart.predict(mileage, nn=TRUE, rules=TRUE)[,1])) == 0)
options(width=owidth$width)
stopifnot(identical(options("width"), old.width))

# test negative response and predictor values
cu.summary2 <- cu.summary
cu.summary2$Mileage <- -cu.summary2$Mileage
cu.summary2$Price   <- -cu.summary2$Price
mileage.negative <- rpart(Mileage ~ ., data=cu.summary2)
cat0("mileage.negative (negative response and predictor values)\n")
print(rpart.rules(mileage.negative))
print(rpart.rules(mileage.negative, varlen=2, faclen=-3, digits=3))

data(ptitanic)
age <- rpart(age ~ ., data=ptitanic)

print(rpart.rules(age, varlen=0))
print(rpart.rules(age, varlen=1))
print(rpart.rules(age, varlen=2))
print(rpart.rules(age, varlen=3))
print(rpart.rules(age, varlen=-1))
print(rpart.rules(age, varlen=-2))
print(rpart.rules(age, varlen=-3))

print(rpart.rules(age, varlen=-3, faclen=1))
print(rpart.rules(age, varlen=-3, faclen=2))

print(rpart.rules(age, varlen=-3, faclen=2, digits=1))

# test various types of predictor with roundint and cover

cat0("\n=== test various types of predictor with roundint and cover ===\n")

data <- "nonesuch"

build.models <- function(use.prp, clip.facs, roundint)
{
    form <- Volume~.-Height

    # conventional model with data argument and continuous predictors
    main <- paste0("clip.facs=", clip.facs, " roundint=", roundint, "\ncontinuous predictors")
    cat0(main, "\n")
    trees.internal <- trees
    mod <- rpart(formula=Volume~Girth, data=trees.internal, cp=.001)
    if(use.prp)
        prp(mod, clip.facs=clip.facs, roundint=roundint, main=main,
               type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
    else {
        rpart.plot(mod, clip.facs=clip.facs, roundint=roundint, main=main,
                   type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
        rules <- rpart.rules(mod, clip.facs=clip.facs, roundint=roundint, digits=3)
        print(rules)
    }
    main <- "logical and factor"
    trees.internal <- trees
    trees.internal$Girth <- ceiling(trees.internal$Girth)
    trees.internal$logical12 <- trees.internal$Girth > 12
    trees.internal$fac16     <- factor(ifelse(trees.internal$Girth > 16, "big", "small"))
    trees.internal$Girth <- NULL
    mod <- rpart(formula=Volume~., data=trees.internal, cp=.001)
    if(use.prp)
        prp(mod, clip.facs=clip.facs, roundint=roundint, main=main,
               type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
    else {
        rpart.plot(mod, clip.facs=clip.facs, roundint=roundint, main=main,
                   type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
        rules <- rpart.rules(mod, clip.facs=clip.facs, roundint=roundint, cover=TRUE, digits=3)
        print(rules)
    }
    main <- "logical and numeric binary"
    trees.internal <- trees
    trees.internal$logical12 <- trees.internal$Girth > 12
    trees.internal$numeric16 <- as.numeric(trees.internal$Girth > 16)
    trees.internal$Girth <- NULL
    mod <- rpart(formula=Volume~., data=trees.internal, cp=.001)
    if(use.prp)
        prp(mod, clip.facs=clip.facs, roundint=roundint, main=main,
               type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
    else {
        rpart.plot(mod, clip.facs=clip.facs, roundint=roundint, main=main,
                   type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
        rules <- rpart.rules(mod, clip.facs=clip.facs, roundint=roundint, digits=3)
        print(rules)
    }
    main <- "discrete Girth"
    cat0(main, "\n")
    trees.internal <- trees
    g <- trees.internal$Girth
    trees.internal$Girth <- ifelse(g < 12.4, 1, ifelse(g < 16.1, 2, 3))
    mod.discrete.girth <- rpart(formula=Volume~Girth, data=trees.internal, cp=.001)
    if(use.prp)
        prp(mod.discrete.girth, clip.facs=clip.facs, roundint=roundint, main=main,
               type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
    else {
        rpart.plot(mod.discrete.girth, clip.facs=clip.facs, roundint=roundint, main=main,
                   type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
        rules <- rpart.rules(mod.discrete.girth, clip.facs=clip.facs, roundint=roundint, cover=TRUE, digits=3)
        print(rules)
    }
    main <- "model without data arg"
    cat0(main, "\n")
    trees.internal <- trees
    Volume <- 10 * trees.internal$Volume
    Girth  <- 10 * trees.internal$Girth
    mod.without.data.arg <- rpart(formula=Volume~Girth, cp=.001)
    if(use.prp)
        prp(mod.without.data.arg, clip.facs=clip.facs, roundint=roundint, main=main,
               type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
    else {
        rpart.plot(mod.without.data.arg, clip.facs=clip.facs, roundint=roundint, main=main,
                   type=4, clip.right.labs=FALSE, digits=3, cex.main=1)
        rules <- rpart.rules(mod.without.data.arg, clip.facs=clip.facs, roundint=roundint, digits=3)
        print(rules)
    }
    list(mod=mod, mod.without.data.arg=mod.without.data.arg)
}
for(use.prp in c(FALSE, TRUE)) {
    for(clip.facs in c(FALSE, TRUE))
        for(roundint in c(FALSE, TRUE)) {
            par(mfrow=c(3, 3))
            ret <- build.models(use.prp, clip.facs, roundint)
        }
}
par(old.par)

mod                  <- ret$mod
mod.without.data.arg <- ret$mod.without.data.arg

# check if we can still access the data used to build the model in build.models
print(rpart.rules(mod, roundint=FALSE, trace=.5)) # ok
options(warn=2) # treat warnings as errors
expect.err(try(rpart.rules(mod)), "Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).")
print(rpart.rules(mod.without.data.arg, roundint=FALSE, trace=.5)) # ok
print(rpart.rules(mod.without.data.arg, roundint=TRUE, trace=.5))  # ok, because environment saved with formula in model
options(warn=1) # print warnings as they occur

cat0("\n=== fit.oz, digits=4 ===\n")
library(earth)
data(ozone1)
fit.oz <- rpart(O3~., data=ozone1)
cat("rpart.rules(fit.oz)\n")
print(rpart.rules(fit.oz, digits=4))
rpart.plot(fit.oz, digits=4, main="fit.oz\ndigits=4")
cat("rpart.rules(fit.oz), roundint=FALSE\n")
print(rpart.rules(fit.oz, roundint=FALSE, digits=4))
rpart.plot(fit.oz, roundint=FALSE, digits=4, main="fit.oz\ndigits=4, roundint=FALSE")

cat0("\n=== oz.vis55 ===\n")

oz <- ozone1
oz$vis55 <- oz$vis >= 55 # replace vis with a logical variable
oz$vis <- NULL
oz.vis55 <- rpart(O3~., data=oz)
cat("oz.vis55:\n")
print(rpart.rules(oz.vis55))

# test handling of malformed rpart object
cat("oz.vis55.badmodel:\n")
oz.vis55.badmodel <- oz.vis55
oz.vis55.badmodel$model <- "bad.model.frame"
print(rpart.rules(oz.vis55.badmodel)) # should give a warning
print(rpart.rules(oz.vis55.badmodel, roundint=FALSE)) # ok

cat("oz.vis55.noterms.nocall:\n")
oz.vis55.noterms.nocall <- oz.vis55
oz.vis55.noterms.nocall$terms <- NULL
oz.vis55.noterms.nocall$call <- NULL
print(rpart.rules(oz.vis55.noterms.nocall)) # should give a warning
print(rpart.rules(oz.vis55.noterms.nocall, roundint=FALSE)) # ok

oz.vis55.model <- rpart(O3~., data=oz, model=TRUE)
oz.vis55.model.noterms.nocall <- oz.vis55.model
oz.vis55.model.noterms.nocall$terms <- NULL
oz.vis55.model.noterms.nocall$call <- NULL
print(rpart.rules(oz.vis55.model.noterms.nocall)) # ok (but cannot get response name)

cat("oz.vis55.badformula:\n")
oz.vis55.badformula <- oz.vis55
oz.vis55.badformula$formula <- "bad.formula"
print(rpart.rules(oz.vis55.badformula)) # should give a warning
print(rpart.rules(oz.vis55.badformula, roundint=FALSE)) # ok

cat("oz.vis55.badcall:\n")
oz.vis55.badcall <- oz.vis55
oz.vis55.badcall$call <- "bad.call"
print(rpart.rules(oz.vis55.badcall)) # should give a warning
print(rpart.rules(oz.vis55.badcall, roundint=FALSE)) # ok

cat("oz.vis55.badterms:\n")
oz.vis55.badterms <- oz.vis55
oz.vis55.badterms$terms <- "bad.terms"
print(rpart.rules(oz.vis55.badterms)) # should give a warning
print(rpart.rules(oz.vis55.badterms, roundint=FALSE)) # ok

cat("oz.vis55.baddataClasses:\n")
oz.vis55.baddataClasses <- oz.vis55
attr(oz.vis55.baddataClasses$terms, "dataClasses") <- "bad.dataClasses"
print(rpart.rules(oz.vis55.baddataClasses)) # should give a warning
print(rpart.rules(oz.vis55.baddataClasses, roundint=FALSE)) # ok

# test handling of malformed rpart object, such as package semtree passes
# no terms component nor attribute (therefore cannot determine is.binary for the variables)
cat("oz.vis55.noterms:\n")
oz.vis55.noterms <- oz.vis55
oz.vis55.noterms$terms <- NULL
print(rpart.rules(oz.vis55.noterms, trace=1)) # trace message says "cannot determine is.binary"
print(rpart.rules(oz.vis55.noterms)) # silent (but still cannot determine is.binary)

cat0("\n=== Country ===\n")
Country <- rpart(Country~., data=cu.summary) # factor with 10 levels
par(mfrow=c(1, 2))
rpart.plot(Country, type=3, clip.right.labs=FALSE, branch=.3, legend.x=-0.12, legend.y=.97)
print(rpart.rules(Country, cover=TRUE))
print(rpart.rules(Country, nn=TRUE))
print(rpart.rules(Country, cover=TRUE, nn=TRUE))

print(rpart.rules(Country, style="tall"))
print(rpart.rules(Country, style="tall", cover=TRUE))
print(rpart.rules(Country, style="tall", nn=TRUE))
print(rpart.rules(Country, style="tall", cover=TRUE, nn=TRUE))

print(rpart.rules(Country, style="tallw"))
print(rpart.rules(Country, style="tallw", cover=TRUE))
print(rpart.rules(Country, style="tallw", nn=TRUE))
print(rpart.rules(Country, style="tallw", cover=TRUE, nn=TRUE))

expect.err(try(rpart.rules(Country, nonesuch="nonesuch")), 'unused argument (nonesuch = "nonesuch")')
expect.err(try(print(rpart.rules(Country), nonesuch2="nonesuch2")), 'unused argument (nonesuch2 = "nonesuch2")')
expect.err(try(rpart.rules(99)), "Not an rpart object")
expect.err(try(rpart.rules(Country, ylim=c(0,1))), 'unused argument (ylim = c(0, 1))')

# rpart.predict (factor response, 10 levels)
stopifnot(identical(options("width"), old.width))
owidth <- options("width")
options(width=1e3)
print(predict(Country)[1:5,])
print(head(rpart.predict(Country, rules=TRUE)))
print(head(rpart.predict(Country, rules=TRUE, when="")))
print(head(rpart.predict(Country, rules=TRUE, when="WHEN")))
print(head(rpart.predict(Country, rules=TRUE, when="", because="reason:")))
print(head(rpart.predict(Country, rules=TRUE, because="")))
stopifnot(all.equal(predict(Country), as.matrix(rpart.predict(Country, rules=TRUE)[,1:10])))
stopifnot(max(abs(predict(Country) - rpart.predict(Country, rules=TRUE)[,1:10])) == 0)
print(rpart.predict(Country, nn=TRUE)[1:5,])
stopifnot(all.equal(predict(Country), as.matrix(rpart.predict(Country, nn=TRUE)[,1:10])))
print(rpart.predict(Country, nn=TRUE, rules=TRUE)[1:5,], digits=2)
stopifnot(all.equal(predict(Country), as.matrix(rpart.predict(Country, nn=TRUE, rules=TRUE)[,1:10])))

print(predict(Country, type="class")[1:5])
print(rpart.predict(Country, type="class", rules=TRUE, because="reason:")[1:5,])
print(rpart.predict(Country, type="class", rules=TRUE, because="")[1:5,])
pred <- rpart.predict(Country, type="class", rules=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(Country, type="class"), pred))
print(rpart.predict(Country, type="class", rules=TRUE, because="\n")[1:5,])
print(rpart.predict(Country, type="class", rules=TRUE, nn=TRUE)[1:5,])
pred <- rpart.predict(Country, type="class", rules=TRUE, nn=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(Country, type="class"), pred))

print(rpart.predict(Country, newdata=cu.summary[5:8,]))
print(rpart.predict(Country, newdata=cu.summary[5:8,], nn=TRUE))
print(rpart.predict(Country, newdata=cu.summary[5:8,], rules=TRUE))
print(rpart.predict(Country, newdata=cu.summary[5:8,], nn=TRUE, rules=TRUE))

options(width=owidth$width)
stopifnot(identical(options("width"), old.width))

cat0("\n=== \"style\" argument ===\n")

Species <- rpart(Species ~ ., data=iris)
rpart.plot(Species, main="rpart(Species ~ ., data=iris)")
par(old.par)
print(rpart.rules(Species, style="wide"))
print(rpart.rules(Species, style="wide", cover=TRUE))
print(rpart.rules(Species, style="tall"))
print(rpart.rules(Species, style="tall", cover=TRUE))
print(rpart.rules(Species, style="tallw"))
print(rpart.rules(Species, style="tallw", cover=TRUE))

Mileage <- rpart(Mileage ~ ., data=cu.summary)
rules <- rpart.rules(Mileage)
print(rules)
print(rules, style='tall')
print(rules, style='tallw')
rules.nn <- rpart.rules(Mileage, nn=TRUE)
print(rules.nn)
print(rules.nn, style='tall')
print(rules.nn, style='tallw')

data(ptitanic)
survived <- rpart(survived ~ ., data=ptitanic)
print(rpart.rules(survived, style="wide", varlen=-3, faclen=2))
print(rpart.rules(survived, style="tall", varlen=-3, faclen=2))
print(rpart.rules(survived, style="wide", clip.facs=TRUE, varlen=-3, faclen=2))
print(rpart.rules(survived, style="tall", cover=TRUE, clip.facs=TRUE, varlen=-3, faclen=2))

print(rpart.rules(survived, varlen=-3, faclen=2, nn=TRUE))
print(rpart.rules(survived, varlen=-3, faclen=2, nn=TRUE, cover=TRUE))
print(rpart.rules(survived, style="wide", varlen=-3, faclen=2, nn=TRUE))
print(rpart.rules(survived, style="tall", varlen=-3, faclen=2, nn=TRUE))
print(rpart.rules(survived, style="wide", clip.facs=TRUE, varlen=-3, faclen=2, nn=TRUE))
print(rpart.rules(survived, style="tall", cover=TRUE, clip.facs=TRUE, varlen=-3, faclen=2, nn=TRUE))

# rpart.predict (binomial response)
stopifnot(identical(options("width"), old.width))
owidth <- options("width")
options(width=1e3)
print(predict(survived)[1:5,])
print(head(rpart.predict(survived, rules=TRUE)))
print(head(rpart.predict(survived, rules=TRUE, when="")))
print(head(rpart.predict(survived, rules=TRUE, when="WHEN")))
print(head(rpart.predict(survived, rules=TRUE, when="", because="reason:")))
print(head(rpart.predict(survived, rules=TRUE, because="")))
options(width=owidth$width)
stopifnot(identical(options("width"), old.width))
stopifnot(all.equal(predict(survived), as.matrix(rpart.predict(survived, rules=TRUE)[,1:2])))
stopifnot(max(abs(predict(survived) - rpart.predict(survived, rules=TRUE)[,1:2])) == 0)
print(rpart.predict(survived, nn=TRUE)[1:5,])
stopifnot(all.equal(predict(survived), as.matrix(rpart.predict(survived, nn=TRUE)[,1:2])))
print(rpart.predict(survived, nn=TRUE, rules=TRUE)[1:5,])
stopifnot(all.equal(predict(survived), as.matrix(rpart.predict(survived, nn=TRUE, rules=TRUE)[,1:2])))

print(predict(survived, type="class")[1:5])
print(rpart.predict(survived, type="class", rules=TRUE)[1:5,])
pred <- rpart.predict(survived, type="class", rules=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(survived, type="class"), pred))
print(rpart.predict(survived, type="class", rules=TRUE)[1:5,])
print(rpart.predict(survived, type="class", rules=TRUE, nn=TRUE)[1:5,])
pred <- rpart.predict(survived, type="class", rules=TRUE, nn=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(survived, type="class"), pred))

print(rpart.predict(survived, newdata=ptitanic[5:8,]))
print(rpart.predict(survived, newdata=ptitanic[5:8,], nn=TRUE))
print(rpart.predict(survived, newdata=ptitanic[5:8,], rules=TRUE))
print(rpart.predict(survived, newdata=ptitanic[5:8,], nn=TRUE, rules=TRUE))

Reliability <- rpart(Reliability ~ ., data=cu.summary)
print(rpart.rules(Reliability, style='wide'))
print(rpart.rules(Reliability, style='tall'))
print(rpart.rules(Reliability, style='tallw'))
print(rpart.rules(Reliability, style='wide', clip.facs=TRUE))
print(rpart.rules(Reliability, style='tall', clip.facs=TRUE))
print(rpart.rules(Reliability, style='tallw', clip.facs=TRUE))

cat0("\n=== \"extra\" argument, binomial model ===\n")

options(warn=2) # treat warnings as errors
cat0("extra=\"auto\"\n")
print(rpart.rules(survived, extra="auto"))
cat0("extra=\"A\"\n")
print(rpart.rules(survived, extra="A"))
stopifnot(identical(rpart.rules(survived), rpart.rules(survived, extra=0))) # extra=0 same as "auto"
cat0("extra=1\n")
expect.err(try(rpart.rules(survived, extra=1)), "extra=1 is not supported by rpart.rules")
cat0("extra=2\n")
expect.err(try(rpart.rules(survived, extra=2)), "extra=2 is not supported by rpart.rules")
cat0("extra=3\n")
expect.err(try(rpart.rules(survived, extra=3)), "extra=3 is not supported by rpart.rules")
cat0("extra=4\n")
print(rpart.rules(survived, extra=4))
cat0("extra=5\n")
print(rpart.rules(survived, extra=5))
cat0("extra=6\n")
print(rpart.rules(survived, extra=6))
cat0("extra=7\n")
print(rpart.rules(survived, extra=7))
cat0("extra=8\n")
print(rpart.rules(survived, extra=8))
cat0("extra=9\n")
print(rpart.rules(survived, extra=9))
cat0("extra=10\n")
print(rpart.rules(survived, extra=10))
cat0("extra=11\n")
print(rpart.rules(survived, extra=11))
cat0("extra=123\n")
expect.err(try(rpart.rules(survived, extra=123)), "extra=23 is illegal")
options(warn=1) # print warnings as they occur

cat0("\n=== \"extra\" argument, multiclass model ===\n")

Species <- rpart(Species ~ ., data=iris)
options(warn=2) # treat warnings as errors
cat0("default extra\n")
print(rpart.rules(Species))
cat0("extra=0\n")
print(rpart.rules(Species, extra=0))
cat0("extra=1\n")
expect.err(try(print(rpart.rules(Species, extra=1))), "extra=1 is not supported by rpart.rules")
cat0("extra=2\n")
expect.err(try(print(rpart.rules(Species, extra=2))), "extra=2 is not supported by rpart.rules")
cat0("extra=3\n")
expect.err(try(print(rpart.rules(Species, extra=3))), "extra=3 is not supported by rpart.rules")
cat0("extra=4\n")
print(rpart.rules(Species, extra=4))
cat0("extra=5\n")
print(rpart.rules(Species, extra=5))
cat0("extra=6\n")
expect.err(try(print(rpart.rules(Species, extra=6))), "extra=106 but the response has 3 levels (only the 2nd level is displayed)")
cat0("extra=7\n")
expect.err(try(print(rpart.rules(Species, extra=7))), "extra=106 but the response has 3 levels (only the 2nd level is displayed)")
cat0("extra=8\n")
print(rpart.rules(Species, extra=8))
cat0("extra=9\n")
print(rpart.rules(Species, extra=9))
cat0("extra=10\n")
expect.err(try(print(rpart.rules(Species, extra=10))), "extra=110 but the response has 3 levels (only the 2nd level is displayed)")
cat0("extra=11\n")
expect.err(try(print(rpart.rules(Species, extra=11))), "extra=110 but the response has 3 levels (only the 2nd level is displayed)")
cat0("extra=12\n")
expect.err(try(print(rpart.rules(Species, extra=12))), "extra=12 is illegal")
options(warn=1) # print warnings as they occur

cat0("\n=== \"and\" and related arguments ===\n")

print(rpart.rules(survived))
print(rpart.rules(survived, and="AND"))
print(rpart.rules(survived, and="AND",   eq="=", style="tall"))  # 'and' isn't displayed for style="tall"
print(rpart.rules(survived, and=" AND ", eq="=", style="tallw")) # 'and' isn't displayed for style="tallw"

print(rpart.rules(survived, response.name="survived probability", eq="=", lt="<", ge=">=", and="&&", facsep=","))

print(rpart.rules(survived, style="tall",
                  response.name="survived probability", facsep=" or ",
                  eq="is", lt="is less than", ge="is greater than", and="and"))

expect.err(try(rpart.rules(survived, and=99)), "'and' is not a character variable")
expect.err(try(rpart.rules(survived, response.name=99)), "'response.name' is not a character variable")
expect.err(try(rpart.rules(survived, response.name=c("a", "b"))), "'response.name' has more than one element")
expect.err(try(rpart.rules(survived, eq=NULL)), "'eq' is NULL (it should be a string)")
rpart.rules(survived, lt="")
rpart.rules(survived, ge="")
rpart.rules(survived, and="")
rpart.rules(survived, when="if")
rpart.rules(survived, when=" if ")
rpart.rules(survived, when="     when")
rpart.rules(survived, when="")
rpart.rules(survived, extra=4)
rpart.rules(survived, extra=4, when="reason:")
rpart.rules(survived, extra=4, style="tall", when="\nreason:")
rpart.rules(survived, extra=4, style="tall", when="\n\n")
rpart.rules(survived, extra=4, style="tall", when="")
print.data.frame(rpart.rules(survived))
print.default(rpart.rules(survived))

cat0("\n=== null model ===\n")
data(ptitanic)
null.model <- rpart(survived~sibsp, data=ptitanic)
print(rpart.rules(null.model))
print(rpart.rules(null.model, style='wide'))
print(rpart.rules(null.model, style='tall'))
print(rpart.rules(null.model, null.model="NO RULES"))
print(rpart.rules(null.model, style='wide', null.model="NO RULES"))
print(rpart.rules(null.model, style='tall', null.model="NO RULES"))

# rpart.predict (null.model)
print(predict(null.model)[1:5,])
print(head(rpart.predict(null.model, rules=TRUE)))
print(head(rpart.predict(null.model, rules=TRUE, null.model="NO RULES")))
print(head(rpart.predict(null.model, rules=TRUE, when="")))
print(head(rpart.predict(null.model, rules=TRUE, when="WHEN")))
print(head(rpart.predict(null.model, rules=TRUE, when="", because="reason:")))
print(head(rpart.predict(null.model, rules=TRUE, because="")))
stopifnot(all.equal(predict(null.model), as.matrix(rpart.predict(null.model, rules=TRUE)[,1:2])))
stopifnot(max(abs(predict(null.model) - rpart.predict(null.model, rules=TRUE)[,1:2])) == 0)
print(rpart.predict(null.model, nn=TRUE)[1:5,])
stopifnot(all.equal(predict(null.model), as.matrix(rpart.predict(null.model, nn=TRUE)[,1:2])))
print(rpart.predict(null.model, nn=TRUE, rules=TRUE)[1:5,])
stopifnot(all.equal(predict(null.model), as.matrix(rpart.predict(null.model, nn=TRUE, rules=TRUE)[,1:2])))

print(predict(null.model, type="class")[1:5])
print(rpart.predict(null.model, type="class", rules=TRUE, because="     reason: ")[1:5,])
pred <- rpart.predict(null.model, type="class", rules=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(null.model, type="class"), pred))
print(rpart.predict(null.model, type="class", rules=TRUE)[1:5,])
print(rpart.predict(null.model, type="class", rules=TRUE, nn=TRUE)[1:5,])
pred <- rpart.predict(null.model, type="class", rules=TRUE, nn=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(null.model, type="class"), pred))

print(rpart.predict(null.model, newdata=ptitanic[5:8,]))
print(rpart.predict(null.model, newdata=ptitanic[5:8,], nn=TRUE))
print(rpart.predict(null.model, newdata=ptitanic[5:8,], rules=TRUE))
print(rpart.predict(null.model, newdata=ptitanic[5:8,], nn=TRUE, rules=TRUE))

cat0("\n=== \"varorder\" argument ===\n")
data(ptitanic)
survived <- rpart(survived ~ ., data=ptitanic)
print(rpart.rules(survived))
print(rpart.rules(survived, varorder="pclass"))
print(rpart.rules(survived, varorder=c('pclass', 'sibsp')))
print(rpart.rules(survived, varorder=c('pclass', 'sex', 'sibsp')))
print(rpart.rules(survived, varorder="si")) # partial match on sibsp
stopifnot(identical(rpart.rules(survived, varorder=c('pclass', 'sex', 'sibsp')),
                    rpart.rules(survived, varorder=c('pc',     'se',  'sib'))))
cat0("\n===varorder error handling ===\n")
options(warn=2) # treat warnings as errors
expect.err(try(rpart.rules(survived, varorder='nonesuch')),      'varorder="nonesuch" does not uniquely match one of: "sex" "pclass" "parch" "age" "sibsp"')
expect.err(try(rpart.rules(survived, varorder='p')),             'varorder="p" does not uniquely match one of: "sex" "pclass" "parch" "age" "sibsp"')
expect.err(try(rpart.rules(survived, varorder='')),              'varorder="" does not uniquely match one of: "sex" "pclass" "parch" "age" "sibsp"')
expect.err(try(rpart.rules(survived, varorder=c('pclass', ''))), 'varorder="" does not uniquely match one of: "sex" "pclass" "parch" "age" "sibsp"')
expect.err(try(rpart.rules(survived, varorder=99)),              'is.character(varorder) is not TRUE')
expect.err(try(rpart.rules(survived, shadow.col="gray")),        "rpart.rules: ignoring argument 'shadow.col'")
options(warn=1) # print warnings as they occur
rpart.rules(survived, varorder=c('nonesuch1','pcl','nonesuch2')) # will give warnings but still print the rules
rpart.rules(survived, clip.facs=TRUE, cover=1, varorder=c('nonesuch1','pcl','nonesuch2')) # will give warnings but still print the rules

par(old.par)

stopifnot(identical(options("width"), old.width))

# Survival model

library('rpart.plot')
library('survival')
rpart.surv <- rpart(Surv(age, sibsp == 1) ~ ., ptitanic)
rpart.plot(rpart.surv, under=TRUE, main="survival model")
print(rpart.rules(rpart.surv))
print(rpart.rules(rpart.surv, cover=TRUE, clip.facs=TRUE, digits=4))
options(warn=2) # treat warnings as errors
expect.err(try(rpart.rules(rpart.surv, extra=1)), "(converted from warning) extra=1 is not supported by rpart.rules")
expect.err(try(rpart.rules(rpart.surv, extra=2)), "(converted from warning) extra=2 is not supported by rpart.rules")
options(warn=1) # print warnings as they occur

owidth <- options("width")
options(width=1e3)
# rpart.predict (Survival model)
print(predict(rpart.surv)[1:5])
print(rpart.predict(rpart.surv)[1:5])
stopifnot(identical(predict(rpart.surv), rpart.predict(rpart.surv)))
print(rpart.predict(rpart.surv, rules=TRUE)[12:14,])
print(head(rpart.predict(rpart.surv, rules=TRUE, when="")))
print(head(rpart.predict(rpart.surv, rules=TRUE, when="WHEN")))
print(head(rpart.predict(rpart.surv, rules=TRUE, when="", because="reason:")))
print(head(rpart.predict(rpart.surv, rules=TRUE, because="")))
# use max and not identical because rpart.predict(rpart.surv, rules=TRUE)[,1] doesn't have names
stopifnot(max(abs(predict(rpart.surv) - rpart.predict(rpart.surv, rules=TRUE)[,1])) == 0)
stopifnot(identical(as.numeric(predict(rpart.surv)), rpart.predict(rpart.surv, rules=TRUE)[,1]))
print(rpart.predict(rpart.surv, nn=TRUE)[12:14,])
stopifnot(max(abs(predict(rpart.surv) - rpart.predict(rpart.surv, nn=TRUE)[,1])) == 0)
print(rpart.predict(rpart.surv, nn=TRUE, rules=TRUE)[12:14,])
stopifnot(max(abs(predict(rpart.surv) - rpart.predict(rpart.surv, nn=TRUE, rules=TRUE)[,1])) == 0)
print(rpart.predict(rpart.surv, newdata=ptitanic[5:8,]))
print(rpart.predict(rpart.surv, newdata=ptitanic[5:8,], nn=TRUE))
print(rpart.predict(rpart.surv, newdata=ptitanic[5:8,], rules=TRUE))
print(rpart.predict(rpart.surv, newdata=ptitanic[5:8,], nn=TRUE, rules=TRUE))
options(width=owidth$width)
stopifnot(identical(options("width"), old.width))

#--- extra tests for rpart.predict ---

survived <- rpart(survived ~ ., data=ptitanic)
print(rpart.rules(survived))
owidth <- options(width=1e3)$width
pred <- rpart.predict(survived, rules=TRUE)[2:4,]
rownames(pred) <- NULL
print(pred)
expect.err(try(rpart.predict(survived, rules=TRUE, style="tall")), "style = \"tall\" is not supported by rpart.predict")
print(rpart.predict(survived, rules=TRUE, cover=TRUE)[2:4,])
print(rpart.predict(survived, rules=TRUE, roundint=FALSE)[2:4,])
print(rpart.predict(survived, rules=TRUE, roundint=FALSE, clip.facs=TRUE)[2:4,])
print(rpart.predict(survived, rules=TRUE, varorder="pclass")[2:4,])
print(rpart.predict(survived, rules=TRUE, faclen=-2)[2:4,])
print(rpart.predict(survived, rules=TRUE, varlen=2)[2:4,])
print(rpart.predict(survived, rules=TRUE, varlen=2)[2:4,], digits=2)
options(width=owidth)

set.seed(2020)
ptit <- ptitanic
ptit <- ptit[sample.int(nrow(ptitanic)),]
ptit <- ptit[1:50,]
pclass <- rpart(pclass~., data=ptit, control=list(cp=.05))
print(rpart.rules(pclass))
options(digits=2)

print(predict(pclass)[1:5,])
stopifnot(all.equal(predict(pclass), as.matrix(rpart.predict(pclass, rules=TRUE)[,1:3])))
stopifnot(max(abs(predict(pclass) - rpart.predict(pclass, rules=TRUE)[,1:3])) == 0)
print(rpart.predict(pclass, nn=TRUE)[1:5,])
stopifnot(all.equal(predict(pclass), as.matrix(rpart.predict(pclass, nn=TRUE)[,1:3])))
print(rpart.predict(pclass, nn=TRUE, rules=TRUE)[1:5,])
stopifnot(all.equal(predict(pclass), as.matrix(rpart.predict(pclass, nn=TRUE, rules=TRUE)[,1:3])))

print(predict(pclass, type="class")[1:5])
print(rpart.predict(pclass, type="class", rules=TRUE)[1:5,])
pred <- rpart.predict(pclass, type="class", rules=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(pclass, type="class"), pred))
print(rpart.predict(pclass, type="class", rules=TRUE)[1:5,])
print(rpart.predict(pclass, type="class", rules=TRUE, nn=TRUE)[1:5,])
pred <- rpart.predict(pclass, type="class", rules=TRUE, nn=TRUE)
names <- rownames(pred)
pred <- pred[,1]
names(pred) <- names
stopifnot(all.equal(predict(pclass, type="class"), pred))

print(rpart.predict(pclass, newdata=ptit[5:8,]))
print(rpart.predict(pclass, newdata=ptit[5:8,], nn=TRUE))
print(rpart.predict(pclass, newdata=ptit[5:8,], rules=TRUE))
print(rpart.predict(pclass, newdata=ptit[5:8,], nn=TRUE, rules=TRUE))

# caret package

library(caret)
data(iris)
Petal.Length <- rpart(Petal.Length ~ ., data=iris)
print(rpart.rules(Petal.Length))

# caret train x,y interface

set.seed(2020)
# note that caret converts factors to indicator columns before invoking rpart
iPetal.Length <- 3 # index of Sepal.Length column
caret.xy <- train(iris[,-iPetal.Length], iris[,iPetal.Length], method="rpart", tuneLength=4)
rpart.rules(caret.xy$finalModel)
rpart.rules(caret.xy$finalModel, roundint=FALSE)
plotmo(Petal.Length, method="apartdep", do.par=2)
rpart.plot(Petal.Length)

owidth <- options(width=1e3)$width
head(rpart.predict(caret.xy$finalModel, rules=TRUE))
# TODO fails: Error in eval(predvars, data, env) : object 'Speciesversicolor' not found
# rpart.predict(caret.xy$finalModel, rules=TRUE, newdata=iris[50:52,])
options(width=owidth)

plotmo(caret.xy, all2=TRUE, method="apartdep", do.par=2) # Warning: NA in singles, will plot all variables (as if all1=TRUE)
rpart.plot(caret.xy$finalModel)

plotmo(caret.xy$finalModel, method="apartdep", do.par=2, all2=TRUE)
rpart.plot(caret.xy$finalModel)

# caret train formula interface
set.seed(2020)
# note that caret converts factors to indicator columns before invoking rpart
caret.form <- train(Petal.Length ~ ., data=iris, method="rpart", tuneLength=4)
rpart.rules(caret.form$finalModel)
rpart.rules(caret.form$finalModel, roundint=FALSE)
plotmo(Petal.Length, method="apartdep", do.par=2)
rpart.plot(Petal.Length)

owidth <- options(width=1e3)$width
head(rpart.predict(caret.form$finalModel, rules=TRUE))
# TODO fails: Error in eval(predvars, data, env) : object 'Speciesversicolor' not found
# rpart.predict(caret.form$finalModel, rules=TRUE, newdata=iris[50:52,])
options(width=owidth)

plotmo(caret.form, all2=TRUE, method="apartdep", do.par=2) # Warning: NA in singles, will plot all variables (as if all1=TRUE)
rpart.plot(caret.form$finalModel)

plotmo(caret.form$finalModel, method="apartdep", do.par=2, all2=TRUE)
rpart.plot(caret.form$finalModel)

# Dates

par(mfrow=c(2, 2))
set.seed(2020)
xx <- 5 * rnorm(60)
data <- data.frame(
    yy  = c(1:30, 30:1) + xx,
    xx  = xx,
    dd = c(as.Date(paste0("2018-08-", 1:30)),
           as.Date(paste0("2018-09-", 1:30))))
print(head(data))
datemod <- rpart(yy ~ ., data = data)
print(rpart.rules(datemod))
plotmo(datemod, pt.col=2, do.par=FALSE)
rpart.plot(datemod)
par(old.par)

stopifnot(identical(options("width"), old.width))
source("test.epilog.R")
