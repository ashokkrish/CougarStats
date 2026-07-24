# test.palette.R

source("test.prolog.R")
library(rattle, quietly=TRUE)

example(show.prp.palettes)

# binomial model

data(ptitanic)
mod.survived <- rpart(survived~., data=ptitanic, control=list(cp=.02))
# test built-in palettes
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot     (mod.survived, type=2, fallen.leaves=TRUE, main="test.palette.R\ntwo class, box.palette=\"auto\"")
library(rattle, quietly=TRUE)
fancyRpartPlot(mod.survived, main="fancyRpartPlot", sub="")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Blues",    box.palette="Blues", trace=1)
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Blues",   box.palette="-Blues", trace=2)
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Browns",   box.palette="Browns", trace=1)
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Browns",  box.palette="-Browns", trace=2)
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Grays",    box.palette="Grays")
rpart.plot(mod.survived, type=2, extra="au",   fallen.leaves=TRUE, main="-Grays",   box.pal="-Grays")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Greys",    box.pa="Greys")
rpart.plot(mod.survived, type=2, extra="AU",   fallen.leaves=TRUE, main="-Greys",   box.p="-Greys")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Greens",   box.palette="Greens")
prp                (mod.survived, type=2, extra="AUTO", fallen.leaves=TRUE, main="-Greens",  box.palette="-Greens")
rpart.plot(mod.survived, type=2, extra="AUTO", fallen.leaves=TRUE, main="Oranges",  box.palette="Oranges")
rpart.plot(mod.survived, type=2, extra="Auto", fallen.leaves=TRUE, main="-Oranges", box.palette="-Oranges")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Reds",     box.palette="Reds")
rpart.plot(mod.survived, type=2, extra="aUTO", fallen.leaves=TRUE, main="-Reds",    box.palette="-Reds")
rpart.plot(mod.survived, type=2, extra="Auto", fallen.leaves=TRUE, main="Purples",  box.palette="Purples")
prp                (mod.survived, type=2, extra="autO", fallen.leaves=TRUE, main="-Purples", box.palette="-Purples")
par(old.par)

par(mfrow = c(4, 3), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Blues",    box.palette="Blues", trace=1)
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Bu",       box.palette="Bu", trace=1)
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Bu",      box.palette="-Bu", trace=2)
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Browns",   box.palette="Browns", trace=1)
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Br",       box.palette="Br", trace=1)
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Br",      box.palette="-Br", trace=2)
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Grays",    box.palette="Grays")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Gy",       box.palette="Gy")
rpart.plot(mod.survived, type=2, extra="au",   fallen.leaves=TRUE, main="-Gy",      box.pal="-Gy")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Greys",    box.pa="Greys")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Gy",       box.pa="Gy")
rpart.plot(mod.survived, type=2, extra="AU",   fallen.leaves=TRUE, main="-Gy",      box.p="-Gy")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Greens",   box.palette="Greens")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Gn",       box.palette="Gn")
prp                (mod.survived, type=2, extra="AUTO", fallen.leaves=TRUE, main="-Gn",      box.palette="-Gn")
rpart.plot(mod.survived, type=2, extra="AUTO", fallen.leaves=TRUE, main="Oranges",  box.palette="Oranges")
rpart.plot(mod.survived, type=2, extra="AUTO", fallen.leaves=TRUE, main="Or",       box.palette="Or")
rpart.plot(mod.survived, type=2, extra="Auto", fallen.leaves=TRUE, main="-Or",      box.palette="-Or")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Reds",     box.palette="Reds")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="Re",       box.palette="Re")
rpart.plot(mod.survived, type=2, extra="aUTO", fallen.leaves=TRUE, main="-Re",      box.palette="-Re")
rpart.plot(mod.survived, type=2, extra="Auto", fallen.leaves=TRUE, main="Purples",  box.palette="Purples")
rpart.plot(mod.survived, type=2, extra="Auto", fallen.leaves=TRUE, main="Pu",       box.palette="Pu")
prp                (mod.survived, type=2, extra="autO", fallen.leaves=TRUE, main="-Pu",      box.palette="-Pu")
par(old.par)

par(mfrow = c(4, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="RdYlGn",   box.palette="RdYlGn")
rpart.plot(mod.survived, type=2, extra="aU",   fallen.leaves=TRUE, main="-RdYlGn",  box.palette="-RdYlGn")
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="BuGn",     box.palette="BuGn")
rpart.plot(mod.survived, type=2, extra="Au",   fallen.leaves=TRUE, main="-BuGn",    box.palette="-BuGn")
rpart.plot(mod.survived, type=2, extra="aut",  fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot(mod.survived, type=2, extra="AUT",  fallen.leaves=TRUE, main="-BuRd",    box.palette="-BuRd")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdGn",    box.palette="-RdGn")
# rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="pal1",   box.palette="pal1")

rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="BuGn",     box.palette="BuGn")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GnBu",     box.palette="GnBu")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGy",     box.palette="RdGy")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GyRd",     box.palette="GyRd")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuBu",     box.palette="BuBu")
rpart.plot(mod.survived, box.palette=0, main="mod.survived\nbox.palette=0")
rpart.plot(mod.survived, box.palette=NA, main="mod.survived\nbox.palette=NA")
par(old.par)

# test list palette for binomial response
par(mfrow = c(2, 2))
rpart.plot(mod.survived, type=2,               fallen.leaves=TRUE, main="test list palette for binomial response\nlist(Blues, Greens)", box.palette=list("Blues", "Greens"))
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="list(Blues, Purples)", box.palette=list("Blues", "Purples"))
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuGn", box.palette="BuGn")
par(old.par)

# test custom palettes
par(mfrow = c(2, 2))
box.palette=gray(seq(.95, .6, length.out=100))
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main=paste(length(box.palette), " grays\n(custom palette)"), box.palette=box.palette)

box.palette=gray(seq(.9, .6, length.out=3))
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main=paste(length(box.palette), " grays"), box.palette=box.palette)

box.palette=gray(seq(.9, .6, length.out=2))
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main=paste(length(box.palette), " grays"), box.palette=box.palette)

rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="single pink", box.palette="pink")

rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="wheat", box.palette="wheat")

box.palette=c("pink", "palegreen")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="pink palegreen", box.palette=box.palette)

box.palette=c("pink", "wheat", "palegreen")
rpart.plot(mod.survived, type=2, extra="auto", fallen.leaves=TRUE, main="pink wheat palegreen", box.palette=box.palette)
par(old.par)

# continuous response (anova tree)
itit <- ptitanic
itit$survived <- as.integer(itit$survived == "survived")
mod.continuous.survived <- rpart(survived~., data=itit, control=list(cp=.02))
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot(mod.continuous.survived, type=2, extra="auto", main="continuous (survived)")
fancyRpartPlot(mod.continuous.survived, main="fancyRpartPlot", sub="")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Greens",  box.palette="-Greens")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Greens",   box.palette="Greens")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Blues",    box.palette="Blues")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Blues",   box.palette="-Blues")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Oranges",  box.palette="Oranges")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Oranges", box.palette="-Oranges")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Purples",  box.palette="Purples")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Purples", box.palette="-Purples")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Reds",     box.palette="Reds")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Reds",    box.palette="-Reds")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Grays",    box.palette="Grays")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Grays",   box.palette="-Grays")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="Greys",    box.palette="Greys")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-Greys",   box.palette="-Greys")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdYlGn",   box.palette="RdYlGn")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdYlGn",  box.palette="-RdYlGn")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuGn",     box.palette="BuGn")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-BuGn",    box.palette="-BuGn")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-BuRd",    box.palette="-BuRd")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdGn",    box.palette="-RdGn")

rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GnBu",     box.palette="GnBu")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-BuGn",    box.palette="-BuGn")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="BuRd",     box.palette="BuRd")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdBu",    box.palette="-RdBu")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="RdGn",     box.palette="RdGn")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-GnRd",    box.palette="-GnRd")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="GnYlRd",   box.palette="GnYlRd")
rpart.plot(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="-RdYlGn",  box.palette="-RdYlGn")

rpart.plot(mod.continuous.survived, box.palette=0,  main="box.palette=0")
rpart.plot(mod.continuous.survived, box.palette=NA, main="box.palette=NA")
par(old.par)

par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
mod.age <- rpart(age ~ ., data=ptitanic)
rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto", main="age\nbox.palette=\"auto\"")
fancyRpartPlot(mod.age, main="fancyRpartPlot", sub="")
rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="Greens", yesno=2, main="Greens yesno=2")
rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="RdYlGn", main="RdYlGn")
rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="Reds",   main="Reds")
rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="BuGn",   main="BuGn")
rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="notacolor")),
           "box.palette: \"notacolor\" is neither a color nor a palette")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="#1234XX")),
           "box.palette: \"#1234XX\" is neither a color nor a palette")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="#1234XX")),
           "box.palette: \"#1234XX\" is neither a color nor a palette")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="B")),
           "box.palette=\"B\" is ambiguous")

# TODO the error message below could be improved to say pal="-B" is ambiguous (note the minus)
expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="-B")),
           "box.palette=\"B\" is ambiguous")

# expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette="NA")),
#            "box.palette=\"NA\" is not allowed")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette=rpart.plot)),
           "box.palette: illegal value")

expect.err(try(rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette=c(0, 1, 2), main="box.palette=c(0,1,2)")),
           "box.palette: 0 is neither a color nor a palette")

rpart.plot(mod.age, type=2, extra="auto", fallen.leaves=TRUE, box.palette=c(2,3,4), main="box.palette=c(2,3,4)")
par(old.par)

# multiclass responses
mod.class <- rpart(pclass~., data=ptitanic, control=list(cp=.02))
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
rpart.plot(mod.class, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto",
                    main="multiclass\n(2 of 3 classes predicted)", trace=1)
fancyRpartPlot(mod.class, main="fancyRpartPlot", sub="")
rpart.plot(mod.class, type=2, extra="auto", fallen.leaves=TRUE, box.palette=list("Blues", "Reds", "Greens"),
           main="list(Blues, Reds, Greens)", trace=2)
# expect warning: The box palettes have been reassigned
# Actually, I removed the warning (in the rpart.plot source code) because it causes confusion
rpart.plot(mod.class, type=2, extra="auto", fallen.leaves=TRUE, box.palette=list("Blues", "Reds"), main="list(Blues, Reds)")

data(iris)
mod.species <- rpart(Species~., data=iris, cp=1e-10)
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass\n(3 of 4 classes predicted)")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette="auto", main="multiclass, tweak=1.1", tweak=1.1)
# expect warning: all boxes will be white
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette=list("Reds", "Grays"), main="list(Reds, Grays)\nall boxes white")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette=0, main="box.palette=0")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.col=2:4, main="mod.species\nbox.col=2:4")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, trace=1,
           box.palette=list("Reds", "Blues", "Grays"),
           main='mod.species\nbox.palette=list("Reds", "Blues", "Grays")')
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette="Blues", main="box.palette=\"Blues\"")
rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette="lightpink", main="box.palette=\"lightpink\"")
par(old.par)

par(mfrow = c(2, 2))
prp(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="mod.continuous.survived\nc(\"Red\", \"Yellow\", \"Green\")",  box.palette=c("red", "yellow", "green"))
prp(mod.continuous.survived, type=2, extra="auto", fallen.leaves=TRUE, main="mod.continuous.survived\nc(\"Green\", \"Yellow\", \"Red\")",  box.palette=c("green", "yellow", "red"))
expect.err(try(rpart.plot(mod.species, type=2, extra="auto", fallen.leaves=TRUE, box.palette=c("Blues", "Reds"), main="box.palette=c(\"Blues\", \"Reds\")")),
           "The rpart model has a multiclass response (not a continuous or binary response)")

par(old.par)

par(mfrow = c(1, 2))
mod.country <- rpart(Country ~ ., cu.summary)
# expect warning: The box palettes have been reassigned
# Actually, I removed the warning (in the rpart.plot source code) because it causes confusion
rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass\n(4 of 10 classes predicted)")
fancyRpartPlot(mod.country, main="fancyRpartPlot", sub="") # fancyRpartPlot gets colors wrong
rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass\nbox.palette=\"auto\"", box.palette="auto")
rpart.plot(mod.country,
    type=2, extra="auto", fallen.leaves=TRUE,
    main="box.palette=\nlist(\"Greens\", \"Reds\", \"Grays\", \"Purples\")",
    box.palette=list("Greens", "Reds", "Grays", "Purples"))

rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass, trace=-1", box.palette="auto", trace=-1)
rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE, main="multiclass, box.palette=0", box.palette=0)
rv <- rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=FALSE, box.palette="auto", main="multiclass, fallen.leaves=FALSE", trace=-1)
expect.err(try(rpart.plot(mod.country, type=2, extra="auto", fallen.leaves=TRUE,
               box.palette=c("lightblue2", "pink"))),
               "The rpart model has a multiclass response (not a continuous or binary response)")
par(old.par)

# poisson model
library(earth)
data(ozone1)
ozone2 <- ozone1
set.seed(8)
ozone2$O3a <- round(runif(330, 1, 10))
y <- cbind(ozone2$O3, ozone2$O3a)
mod.poisson <- rpart(y~.-O3-O3a, data=ozone2, control=list(cp=.04))
par(mfrow=c(2, 2))
# plot(mod.poisson, unif=TRUE, branch=.3, main="poisson\n"); text(mod.poisson, use.n=TRUE, all=T, digits=3, xpd=NA, cex=1.1)
rpart.plot    (mod.poisson, type=2, extra="auto", fallen.leaves=TRUE, main="poisson\ndefault args", box.palette="auto")
fancyRpartPlot(mod.poisson, main="fancyRpartPlot", sub="")
rpart.plot    (mod.poisson, type=2, extra="auto", fallen.leaves=TRUE, main="poisson\nbox.palette=RdYlGn", box.palette="RdYlGn")
rpart.plot    (mod.poisson, type=2, extra="auto", fallen.leaves=TRUE, main="poisson\nbox.palette=BuGn", box.palette="BuGn")
par(old.par)

# compare "auto" to "-auto"

par(mfrow=c(2, 2))
rpart.plot(mod.continuous.survived, type=2, extra="auto",  main="auto\nmod.continuous.survived",  box.palette="auto")
rpart.plot(mod.continuous.survived, type=2, extra="auto", main="-auto\nmod.continuous.survived", box.palette="-auto")

rpart.plot(mod.age,     type=2, extra="a",  main="auto\nmod.age",  box.palette="auto")
rpart.plot(mod.age,     type=2, extra="a", main="-auto\nmod.age", box.palette="-auto")
par(old.par)

par(mfrow=c(2, 2))
rpart.plot(mod.class,   type=2, extra="au",  main="auto\nmod.class",  box.palette="auto")
rpart.plot(mod.class,   type=2, extra="au", main="-auto\nmod.class", box.palette="-auto")

rpart.plot(mod.poisson, type=2, extra="AUTO",  main="auto\nmod.poisson",  box.palette="auto")
rpart.plot(mod.poisson, type=2, extra="AUTO", main="-auto\nmod.poisson", box.palette="-auto")
par(old.par)

par(mfrow=c(2, 2))
rpart.plot(mod.species, type=2, extra="A",  main="auto\nmod.species",  box.palette="auto")
rpart.plot(mod.species, type=2, extra="A", main="-auto\nmod.species", box.palette="-auto")

# TODO we shouldn't really accept extra="axxx" as "auto" (not serious)
rpart.plot(mod.country, type=2, extra="axxx",  main="auto\nmod.country",  box.palette="auto")
rpart.plot(mod.country, type=2, extra="axxx", main="-auto\nmod.country", box.palette="-auto")
par(old.par)

# test legend.x and legend.y and legend.cex
par(mfrow=c(2,2))
multi.class.model <- rpart(Reliability ~ ., data=cu.summary)
rpart.plot(multi.class.model, legend.x=NULL, main="legend.x=NULL (default)")
rpart.plot(multi.class.model, legend.x=NA, main="legend.x=NA (no legend)")
rpart.plot(multi.class.model, fallen.leaves=TRUE, legend.x=.8, main="legend.x=.8")
rpart.plot(multi.class.model, legend.x=-.1, legend.cex=1.4, main="legend.x=-.1, legend.cex=1.4")
par(old.par)

par(mfrow=c(2,2))
rpart.plot(multi.class.model, legend.y=NULL, main="legend.y=NULL (default)")
rpart.plot(multi.class.model, fallen.leaves=FALSE, legend.y=NA, main="legend.y=NA (no legend)")
rpart.plot(multi.class.model, fallen.leaves=FALSE, legend.y=.16, main="legend.y=.16")
rpart.plot(multi.class.model, legend.x=0, legend.y=1.1, legend.cex=.8,
           main="legend.x=0 legend.y=1.1 legend.cex=.8")
par(old.par)

cat("palette handling for node.fun, pal.thresh, and pal.node.fun\n")

get.class.stats    <- rpart.plot:::get.class.stats
internal.node.labs <- rpart.plot:::internal.node.labs
paste.trunc        <- rpart.plot:::paste.trunc
is.try.err         <- rpart.plot:::is.try.err

plot1 <- function(object, node.fun=NULL, pal.node.fun=FALSE, optional.msg="", ...)
{
    rpart.plot(object,
               node.fun=node.fun,
               pal.node.fun=pal.node.fun,
                 main=paste.trunc(optional.msg,
                        "pal.node.fun=", pal.node.fun,
                        "\nnode.fun=",
                        if(is.null(node.fun))
                            "NULL"
                        else
                            paste(strwrap(deparse(body(node.fun)), width=40), collapse="\n"),
                        sep="", maxlen=100),
               cex.main=.9, ...)
}
plot2 <- function(object, node.fun=NULL,  optional.msg="", ...)
{
    try <- try(plot1(object, node.fun=node.fun, pal.node.fun=FALSE, optional.msg=optional.msg, ...))
    if(is.try.err(try)) {
        plot(c(0,1), c(0,1), col=0,
             bty="n", xlab="", xaxt="n", ylab="", yaxt="n",
             main=paste.trunc(optional.msg, "pal.node.fun=FALSE",
                    "\nnode.fun=",
                    paste(strwrap(deparse(body(node.fun)), width=40), collapse="\n"),
                    maxlen=100),
             cex.main=.9)
        text(.5, .3,
             paste.trunc(paste(strwrap(attr(try,"condition"), width=40), collapse="\n"), maxlen=150),
             cex=.9, xpd=NA)
    }
    try <- try(plot1(object, node.fun=node.fun, pal.node.fun=TRUE, optional.msg=optional.msg, ...))
    if(is.try.err(try)) {
        plot(c(0,1), c(0,1), col=0,
             bty="n", xlab="", xaxt="n", ylab="", yaxt="n",
             main=paste.trunc(optional.msg, "pal.node.fun=TRUE",
                    "\nnode.fun=",
                    paste(strwrap(deparse(body(node.fun)), width=40), collapse="\n"),
                    maxlen=100),
             cex.main=.9)
        text(.5, .3,
             paste.trunc(paste(strwrap(attr(try,"condition"), width=40), collapse="\n"), maxlen=150),
             cex=.9, xpd=NA)
    }
}
#--- anova model ---

data(ptitanic)
ptitanic.numeric.survived <- ptitanic
ptitanic.numeric.survived$survived <- as.numeric(ptitanic$survived)
anova.mod <- rpart(survived~., data=ptitanic.numeric.survived, cp=.02)

old.par <- par(no.readonly=TRUE)
par(mfrow=c(3, 2))
par(mar=c(0, 0, 4, 0))
rpart.plot(anova.mod, main="no node.fun")
title("node.fun (anova model)\n\n")

empty.plot()

# test that we can extract the numeric values of the node labels from the labels
plot2(anova.mod, node.fun=function(x, labs, digits, varlen) paste(labs))
plot2(anova.mod, node.fun=function(x, labs, digits, varlen) paste("surv=\n", labs))
plot2(anova.mod, extra=0, node.fun=function(x, labs, digits, varlen) paste("surv=\n", -10 * as.numeric(labs)))
plot2(anova.mod, extra=0, node.fun=function(x, labs, digits, varlen) paste("surv=\n", -1e8 * as.numeric(labs)))
plot2(anova.mod, box.palette="Gy")
par(old.par)
par(mfrow=c(4, 2), mar=c(0, 0, 3, 0))
plot2(anova.mod, box.palette="RdGy")
plot2(anova.mod, node.fun=function(x, labs, digits, varlen) c(1,2,3,4,"nonesuch",6,7,8,9)) # expect try error
plot2(anova.mod, node.fun=function(x, labs, digits, varlen) 1:nrow(x$frame))
plot2(anova.mod, node.fun=function(x, labs, digits, varlen) paste("prefix", 1:nrow(x$frame), "suffix"))
par(old.par)

# test pal.thresh
par(mfrow=c(3, 2), oma=c(0, 0, 3, 0), mar=c(0, 0, 3, 0))
rpart.plot(anova.mod, extra=0, box.palette="RdGn", main="pal.thresh=default", trace=0)
title("pal.thresh (anova model)\n\n\n\n", xpd=NA)
rpart.plot(anova.mod, extra=0, box.palette="RdGn", main="pal.thresh=1.2",  pal.thresh=1.2)
rpart.plot(anova.mod, extra=0, box.palette="RdGn", main="pal.thresh=100",  pal.thresh=100)
rpart.plot(anova.mod, extra=0, box.palette="RdGn", main="pal.thresh=-100",  pal.thresh=-100)
par(old.par)
# test James Hedge's example
par(mfrow=c(3, 2), mar=c(0, 0, 4, 0))
plot2(anova.mod, node.fun=function(x, labs, digits, varlen) paste(x$frame$yval * x$frame$n),
      optional.msg="box.palette=default\n")
plot2(anova.mod, node.fun=function(x, labs, digits, varlen) paste(x$frame$yval * x$frame$n),
      pal.thresh=80, box.palette="OrGy",
      optional.msg="pal.thresh=80, box.palette=\"OrGy\"\n")
rpart.plot(anova.mod, box.palette="BuGn", pal.thresh=1.8,
           main="box.palette=\"BuGn\", pal.thresh=1.8")
rpart.plot(anova.mod, box.palette=c("lightblue", "lightgreen"), pal.thresh=1.8,
           main="box.palette=c(\"lightblue\", \"lightgreen\"), pal.thresh=1.8")
par(old.par)

# test expand.palette
paste.c <- rpart.plot:::paste.c
test.expand.palette <- function(box.palette)
{
    cat(paste0("\nbox.palette=", paste.c(box.palette)), "\n")
    rpart.plot(anova.mod, trace=1, box.palette=box.palette,
               main=paste0("box.palette=", paste.c(box.palette)))
}
par(mfrow=c(3,2))
test.expand.palette("auto")
test.expand.palette("Reds")
test.expand.palette(c("-Reds", "Blues"))
test.expand.palette(c("tan", "tan2", "tan4"))
test.expand.palette(c("tan2", "tan", "Blues"))
test.expand.palette(c("auto", "-Reds"))
expect.err(try(test.expand.palette("")), "box.palette: \"\" is neither a color nor a palette")
expect.err(try(test.expand.palette("nonesuch")), "box.palette: \"nonesuch\" is neither a color nor a palette")
expect.err(try(test.expand.palette("Nonesuch")), "box.palette=\"Nonesuch\" is not allowed")
expect.err(try(test.expand.palette(c("tan1", "huh?"))), "box.palette: \"huh?\" is neither a color nor a palette")
cat("\n")
par(old.par)

#--- binomial model ---

binom.mod <- rpart(survived~., data=ptitanic, cp=.02)

par(mfrow=c(3, 2))
par(mar=c(0, 0, 4, 0))
rpart.plot(binom.mod, main="no node.fun")
title("node.fun (binomial model)\n\n")

empty.plot()

# test that we can extract the numeric values of the node labels from the labels
plot2(binom.mod, node.fun=function(x, labs, digits, varlen) paste(labs))

plot2(binom.mod, node.fun=function(x, labs, digits, varlen) round(get.class.stats(x)$prob.per.lev[,2], 3))
par(old.par)

par(mfrow=c(4, 2), mar=c(0, 0, 4, 0))

plot2(binom.mod, node.fun=function(x, labs, digits, varlen) letters[1:nrow(x$frame)]) # expect try error

plot2(binom.mod, node.fun=function(x, labs, digits, varlen) c(1,2,3,4,"nonesuch",6,7)) # expect try error

plot2(binom.mod, node.fun=function(x, labs, digits, varlen) 1:nrow(x$frame))

plot2(binom.mod, node.fun=function(x, labs, digits, varlen) paste("prefix", 1:nrow(x$frame), "suffix"))
par(old.par)

# test pal.thresh
par(mfrow=c(4, 2), oma=c(0, 0, 3, 0), mar=c(0, 0, 3, 0))
rpart.plot(binom.mod)
title("pal.thresh (binom mod)\n\n\n\n", xpd=NA)
rpart.plot(binom.mod, main="pal.thresh=.5",  pal.thresh=.5, trace=0)

rpart.plot(binom.mod, main="pal.thresh=0",   pal.thresh=0, trace=0)
rpart.plot(binom.mod, main="pal.thresh=1",   pal.thresh=1, trace=0)

rpart.plot(binom.mod, main="pal.thresh=-10", pal.thresh=-10, trace=0)
rpart.plot(binom.mod, main="pal.thresh=10",  pal.thresh=10, trace=0)

rpart.plot(binom.mod, main="pal.thresh=.2",  pal.thresh=.2, trace=0)
par(old.par)

# test pal.thresh with node.fun
par(mfrow=c(4, 2), mar=c(0, 0, 3, 0))
rpart.plot(binom.mod)
title("pal.thresh with node.fun (binom mod)\n\n")
empty.plot()

plot2(binom.mod, node.fun=function(x, labs, digits, varlen)
                   round(2*(get.class.stats(x)$prob.per.lev[,2] - .5), 2))

plot2(binom.mod, node.fun=function(x, labs, digits, varlen)
                   round(2*(get.class.stats(x)$prob.per.lev[,2] - .5), 2),
         pal.thresh=.45,
         optional.msg="pal.thresh=.45\n")

# binomial response, list palette

mod.survived <- rpart(survived~., data=ptitanic, control=list(cp=.02))

rpart.plot(mod.survived, type=2, fallen.leaves=TRUE,
  main="test list palette for binomial response\nlist(Blues, Greens)",
  box.palette=list("Blues", "Greens"))
par(old.par)

#--- poisson model ---

library(earth)
data(ozone1)
ozone2 <- ozone1
set.seed(8)
ozone2$O3a <- round(runif(330, 1, 10))
y <- cbind(ozone2$O3, ozone2$O3a)
mod.poisson <- rpart(y~.-O3-O3a, data=ozone2, control=list(cp=.04))
par(mfrow=c(2, 2))

rpart.plot(mod.poisson, extra="auto", fallen.leaves=TRUE,
           box.palette="BnGn",
           main="mod.poisson\nbox.palette=\"BnGn\"")

rpart.plot(mod.poisson, extra=0, fallen.leaves=TRUE,
           main="box.palette=\"BnGn\"\npal.thresh=.35",
           box.palette="BnGn", pal.thresh=.35)

rpart.plot(mod.poisson, extra=0, fallen.leaves=TRUE,
           main="box.palette=\"BnGn\"\npal.thresh=.7\nnode.fun",
           box.palette="BnGn", pal.thresh=.7,
           node.fun=function(x, labs, digits, varlen)
                    sprint("now %.2f\nwas %.2f", 10*as.numeric(labs)-5, as.numeric(labs)))

rpart.plot(mod.poisson, extra=0, fallen.leaves=TRUE,
           main="box.palette=\"BnGn\"\npal.thresh=-1\nnode.fun\npal.node.fun=TRUE",
           box.palette="BnGn", pal.thresh=-1,
           pal.node.fun=TRUE,
           node.fun=function(x, labs, digits, varlen)
                    sprint("now %.2f\nwas %.2f", 10*as.numeric(labs)-5, as.numeric(labs)))
par(old.par)

#--- multiclass model ---

mod.multiclass <- rpart(pclass~., data=ptitanic, control=list(cp=.02))
par(mfrow=c(3, 2))
plot2(mod.multiclass, optional.msg="mod.multiclass\n", trace=1,
      node.fun=function(x, labs, digits, varlen) substring(labs, 1, 1))
plot2(mod.multiclass, box.palette=list("Bu","Or","Gy"), trace=1,
      optional.msg="box.palette=list(\"Bu\",\"Or\",\"Gy\")\n",
      node.fun=function(x, labs, digits, varlen) substring(labs, 1, 1))
plot2(mod.multiclass, box.palette="BuOr", trace=1,
      optional.msg="box.palette=\"BuOr\"\n",
      node.fun=function(x, labs, digits, varlen) substring(labs, 1, 1))
plot2(mod.multiclass, box.palette="BuOr", trace=1,
      optional.msg="box.palette=\"BuOr\"\n",
      node.fun=function(x, labs, digits, varlen) -as.numeric(substring(labs, 1, 1)))
plot2(mod.multiclass, box.palette="OrGn", pal.thresh=20, trace=1,
      optional.msg="box.palette=\"OrGn\" pal.thresh=20\n",
      node.fun=function(x, labs, digits, varlen) 10 * as.numeric(substring(labs, 1, 1)))

expect.err(try(rpart.plot(mod.multiclass, type=2, extra="auto", trace=2,
               fallen.leaves=TRUE, box.palette=c("blues", "reds"),
               main="box.palette=c(\"blues\", \"reds\")")),
           "The rpart model has a multiclass response (not a continuous or binary response)")

plot2(mod.multiclass, box.palette=list("pink", "lightblue", "lightgray"),
      optional.msg='list("pink", "lightblue", "lightgray")\n',
      node.fun=function(x, labs, digits, varlen) 10 * as.numeric(substring(labs, 1, 1)))

par(old.par)

# multiclass model: test handling of response classes that aren't used in the training data

par(mfrow = c(2, 2), mar = c(3, 3, 3, 1), mgp = c(1.5, 0.5, 0))
data(cu.summary)
# reference model
dat <- cu.summary
aref <- rpart(Reliability~., data=dat)
print(rpart.rules(aref))

dat <- cu.summary
# add an extra level to Reliability that is unused in the data
# in this example, unusedlev is the last level (it won't appear in yval2)
levs <- c("muchworse", "worse", "average",
          "better", "muchbetter", "unusedlev1")
asnum <- as.numeric(cu.summary$Reliability)
dat$Reliability <- factor(levs[asnum], levels=levs, labels=levs, ordered=TRUE)
# print(head(dat, 50))
a1 <- rpart(Reliability~., data=dat)
rpart.plot(a1, main="test response classes that aren't used in the training data\na1", do.par=FALSE)
print(rpart.rules(a1))
aref$cptable <- a1$cptable <- NULL
attr(aref, "ylevels") <- attr(a1, "ylevels") <- NULL
stopifnot(identical(aref, a1))

dat <- cu.summary
# add an extra level to Reliability that is unused in the data
# in this example, unused level is not the last level (it will appear in yval2)
levs <- c("muchworse", "worse", "average",
          "better", "unusedlev", "muchbetter")
asnum <- as.numeric(cu.summary$Reliability)
asnum[asnum == 5] <- 6 # skip unused level
dat$Reliability <- factor(levs[asnum], levels=levs, labels=levs, ordered=TRUE)
a2 <- rpart(Reliability~., data=dat)
rpart.plot(a2, main="a2", do.par=FALSE)
print(rpart.rules(a2))

dat <- cu.summary
# add two extra levels to Reliability that are unused in the data
levs <- c("muchworse", "worse", "average",
          "better", "unusedlev1", "muchbetter", "unusedlev2")
asnum <- as.numeric(cu.summary$Reliability)
asnum[asnum == 5] <- 6 # skip unused level
dat$Reliability <- factor(levs[asnum], levels=levs, labels=levs, ordered=TRUE)
a3 <- rpart(Reliability~., data=dat)
rpart.plot(a3, main="a3", do.par=FALSE)
print(rpart.rules(a3))
par(old.par)

source("test.epilog.R")
