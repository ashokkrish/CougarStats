# vignette.R: figures and code in the rpart.plot vignette

source("test.prolog.R")
data(ptitanic)

#--- front.R ---

library(earth)
data(ozone1)
par(mfrow=c(3,3), mgp = c(1.5, .5, 0))
a <- rpart(O3~., data=ozone1, cp=.024)
y <- a$frame$yval
cols <- c("lightcoral", "khaki2", "palegreen2")
cols <- ifelse(y > 20, cols[1], ifelse(y < 15, cols[3], cols[2]))
prp(a, # old vignette before prp supported box.palette
main="[front] An Example\nmanual palette=\nc(\"lightcoral\", \"khaki2\", \"palegreen2\")",
    box.col=cols, type=4, fallen=T, branch=.3, round=0, leaf.round=9,
    clip.right.labs=F, under.cex=1,
    prefix="ozone\n", branch.col="gray", branch.lwd=2,
    extra=101, under=T, lt=" < ", ge=" >= ", cex.main=1.5)

prp(a,
main="[front] An Example\nbox.palette=\nc(\"palegreen3\", \"khaki2\", \"lightcoral\")",
    type=4, fallen=T, branch=.3, round=0, leaf.round=9,
    clip.right.labs=F, under.cex=1,
    box.palette=c("palegreen3", "khaki2", "lightcoral"),
    prefix="ozone\n", branch.col="gray", branch.lwd=2,
    extra=101, under=T, lt=" < ", ge=" >= ", cex.main=1.5)

prp(a,
    type = 4,                # left and right split labels (see Figure 2)
    clip.right.labs = FALSE, # full right split labels
    extra = 101,             # show nbr of obs and percentages (see Figure 3)
    under = TRUE,            # position extra info _under_ the boxes
    under.cex = 1,           # size of text under the boxes (default is .8)
    fallen.leaves = TRUE,    # put leaves at the bottom of plot
    box.palette = "GnYlRd",  # color of the boxes
    branch = .3,             # branch lines with narrow shoulders and down slopes
    round = 0,               # no rounding of nodes (sharp edges)
    leaf.round = 9,          # round leaf nodes (for leaves, supersedes round argument)
    prefix = "ozone\n",      # prepend this string to the node labels
    main = "An Example",     # main title
    cex.main = 1.5,          # big main title
    branch.col = "gray",     # color of branch lines
    branch.lwd = 2)          # line width of branch lines

par(old.par)

#--- example.R ---

par(mfrow=c(2,2))
#---------------------------------------------------------------------------
binary.model <- rpart(survived ~ ., data=ptitanic, cp=.02)
                                        # cp=.02 for small demo tree

rpart.plot(binary.model, tweak=.9, cex.main=.9,
           main="\ntitanic survived\n(binary response)")

#---------------------------------------------------------------------------
anova.model <- rpart(Mileage ~ ., data=cu.summary)

rpart.plot(anova.model, tweak=.9, cex.main=.9,
           main="\n\n\nmiles per gallon\n(continuous response)\n")

#---------------------------------------------------------------------------
multi.class.model <- rpart(Reliability ~ ., data=cu.summary)

rpart.plot(multi.class.model, tweak=1, cex.main=.9,
           legend.cex=1.3, legend.y=1.25,
           main="\nvehicle reliability\n(multi class response)")

#--- compare to the plotting functions in the rpart package ---

par(old.par)
par(mfrow=c(3,3))
tree <- rpart(survived ~ ., data=ptitanic, cp=.02)
                         # cp=.02 because want small tree for demo
plot(tree, uniform=TRUE, compress=TRUE, branch=.2)
text(tree, use.n=TRUE, cex=.6, xpd=NA) # cex is a guess, depends on your window size
title("compare to the plotting functions\nin the rpart package", cex.sub=.8)

#--- type.R ---

a <- rpart(survived~., data=ptitanic, control=list(cp=.02))
col.main <- rgb(80, 100, 130, maxColorValue=255)
par(mfrow=c(2,3))
prp(a, faclen=0, type=0, main="type = 0\n(default)\n", col=1, col.main=col.main)
prp(a, faclen=0, type=1, main="type = 1\nlabel all nodes\n(like text.rpart all=TRUE)", col=1, col.main=col.main)
prp(a, faclen=0, type=2, main="type = 2\nsplit labels below node labels\n", col=1, col.main=col.main)
prp(a, faclen=0, type=3, main="type = 3\nleft and right split labels\n", col=1, col.main=col.main)
prp(a, faclen=0, type=4, main="type = 4\nlike type=3 but with interior labels\n(like text.rpart fancy=TRUE)", col=1, col.main=col.main)
prp(a, faclen=0, type=5, main="type = 5\nvariable name in interior nodes", col=1, col.main=col.main)
par(old.par)

#--- anova-extra.R ---

a <- rpart(Volume~., data=trees, cp=.5)
par(mfrow=c(3,4))
par(mar = c(3, 3, 5, 1))
par(mgp = c(1.5, .5, 0))
col.main <- rgb(80, 100, 130, maxColorValue=255)
prp(a, clip.right.labs=F, split.cex=.8, tweak=1.3, type=4, extra=0,   yesno=F, faclen=0, under=T, cex.main=1.3, main="[anova-extra] extra = 0\ndefault\n", col=1, col.main=col.main)
prp(a, clip.right.labs=F, split.cex=.8, tweak=1.3, type=4, extra=1,   yesno=F, faclen=0, under=T, cex.main=1.3, main="extra = 1\nnbr of obs\n", col=1, col.main=col.main)
prp(a, clip.right.labs=F, split.cex=.8, tweak=1.3, type=4, extra=100, yesno=F, faclen=0, under=T, cex.main=1.3, main="extra = 100\npercentage of obs\n", col=1, col.main=col.main)
prp(a, clip.right.labs=F, split.cex=.8, tweak=1.3, type=4, extra=101, yesno=F, faclen=0, under=T, cex.main=1.3, main="extra = 101\nnbr and percentage\nof obs", col=1, col.main=col.main)
par(old.par)

#--- class-extra.R ---

par(mfrow=c(4,5))
par(mar = c(3, 3, 5, 1))
par(mgp = c(1.5, .5, 0))
col.main <- rgb(80, 100, 130, maxColorValue=255)
a <- rpart(survived~., data=ptitanic, cp=.03)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=0,   cex.main=1, main="[class-extra] extra = 0\ndefault\n", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=1,   cex.main=1, main="extra = 1\nnbr of obs per class\n", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=2,   cex.main=1, main="extra = 2\nclass rate\n", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=3,   cex.main=1, main="extra = 3\nmisclass rate\n", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=4,   cex.main=1, main="extra = 4\nprob per class\n(sum across a node is 1)", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=5,   cex.main=1, main="extra = 5\nprob per class,\nfitted class not displayed", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=6,   cex.main=1, main="extra = 6\nprob of 2nd class\n(useful for\nbinary responses)", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=7,   cex.main=1, main="extra = 7\nprob of 2nd class,\nfitted class not displayed", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=8,   cex.main=1, main="extra = 8\nprob of fitted class\n", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=9,   cex.main=1, main="extra = 9\noverall prob\n(sum over all leaves is 1)", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=10,  cex.main=1, main="extra = 10\noverall prob of 2nd class\n", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=11,  cex.main=1, main="extra = 11\noverall prob of 2nd class\nfitted class not displayed", col=1, col.main=col.main)
empty.plot()
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=100, cex.main=1, main="extra = 100\npercent of obs\n", col=1, col.main=col.main)
prp(a, under.cex=1, type=4, faclen=0, under=T, extra=106, cex.main=1, main="extra = 106\nprob of 2nd class and\npercent of obs", col=1, col.main=col.main)
par(old.par)

#--- fraction.R ---

par(mfrow=c(3,3))
tree <- rpart(survived~., data=ptitanic, cp=.02)
                         # cp=.02 because want small tree for demo
prp(tree, extra=7, prefix="fraction\n", tweak=1.2, main="[fraction]")


#--- dev.R ---

tree <- rpart(survived~., data=ptitanic, cp=.02)
                         # cp=.02 because want small tree for demo
my.node.fun <- function(x, labs, digits, varlen)
{
    paste("dev", x$frame$dev)
}
prp(tree, node.fun=my.node.fun, tweak=1.2, main="[dev]")


#--- standard-plus-dev.R ---

tree <- rpart(survived~., data=ptitanic, cp=.02)
my.node.fun2 <- function(x, labs, digits, varlen)
{
    paste(labs, "\ndev", x$frame$dev)
}
prp(tree, extra=6, node.fun=my.node.fun2, tweak=1.2, main="[standard-plus-dev]")


#--- double-newline-dev.R ---

tree <- rpart(survived~., data=ptitanic, cp=.02)
my.node.fun3 <- function(x, labs, digits, varlen)
{
    # identical to my.node.fun2 above but use \n\n instead of \n
    paste(labs, "\n\ndev", x$frame$dev)
}
prp(tree, extra=6, node.fun=my.node.fun3, tweak=1.2, main="[double-newline-dev]")

#--- prefix-col.R ---

tree <- rpart(survived~., data=ptitanic, cp=.02)
prp(tree, extra=6, tweak=1.2, main="prefix-col",
    box.col=ifelse(tree$frame$yval == 2, "palegreen3", "pink"))
par(old.par)

#--- movie.R ---

# omitted, tested in test.prp.R

#--- depth-first-movie.R ---

par(mfrow=c(4,5))
data(ptitanic)
tree1 <- rpart(survived~., data=ptitanic, cp=.012)
par(mfrow=c(4,3), mar = c(0, 0, 2, 0))
for(iframe in 1:nrow(tree1$frame)) {
    cols <- ifelse(1:nrow(tree1$frame) <= iframe, "black", "gray")
    dev.hold()     # hold screen output to prevent flashing
    prp(tree1, compress=FALSE, faclen=3, varlen=-3,
       col=cols, branch.col=cols, split.col=cols,
       main=sprint("\n%d", iframe), yesno=FALSE)
    dev.flush()
    # Sys.sleep(1) # wait one second
}
par(old.par)

#--- path-to-root.R ---

par(mfrow=c(2,2))
tree <- rpart(survived~., data=ptitanic, cp=0.02)

# return the given node and all its ancestors (a vector of node numbers)
path.to.root <- function(node)
{
    if(node == 1)   # root?
        node
    else            # recurse, %/% 2 gives the parent of node
        c(node, path.to.root(node %/% 2))
}

node <- 11          # 11 is our chosen node, arbitrary for this example
nodes <- as.numeric(row.names(tree$frame))
cols <- ifelse(nodes %in% path.to.root(node), "sienna", "gray")
prp(tree, nn=TRUE,
    col=cols, branch.col=cols, split.col=cols, nn.col=cols, yesno=F, tweak=1.2,
    main="[path-to-root]")

#--- gray-background.R ---

old.bg <- par(bg="gray50")
iris.tree <- rpart(Species~., data=iris)
prp(iris.tree, type=0, extra=8, main="[gray-background]",
    under=TRUE, yesno=FALSE,
    varlen=0, faclen=0, tweak=1.4,
    col=c("orangered", "orange", "wheat")[iris.tree$frame$yval])
par(bg=old.bg)

#--- snip.R ---

tree <- rpart(survived~., data=ptitanic, cp=.012)
new.tree <- prp(tree, main="[snip-part1]")$obj # interactively trim the tree
prp(new.tree, main="[snip-part2]")             # display the new tree

#--- heat-tree.R ---

heat.tree <- function (tree, low.is.green=FALSE, ...) {
    y <- tree$frame$yval
    if(low.is.green)
        y <- -y
    max <- max(y)
    min <- min(y)
    cols <- rainbow(99, end=.36)[
                ifelse(y >  y[1], (y-y[1]) * (99-50) / (max-y[1])+50,
                                  (y-min)  * (50-1)  / (y[1]-min)+1)]
    prp(tree, branch.col=cols, box.col=cols, ...)
}
data(ptitanic)
tree <- rpart(age ~ ., data=ptitanic)
heat.tree(tree, type=4, varlen=0, faclen=0, fallen.leaves=TRUE)
heat.tree(tree, low.is.green=TRUE)
par(old.par)

#--- compress.R ---

par(mfrow=c(2,2))
a <- rpart(survived~., data=ptitanic, cp=.01)
col.main <- rgb(80, 100, 130, maxColorValue=255)
prp(a, compress=FALSE, ycompress=FALSE, main="[compress]\ncompress=FALSE\nycompress=FALSE", cex.main=.9, trace=1, col=1, col.main=col.main)
text(.2, -.12, "calculated cex:\n0.44", cex=1, xpd=NA, font=3, col=rgb(.1,.2,.4))
prp(a, compress=TRUE, ycompress=FALSE, main="\ncompress=TRUE (default)\nycompress=FALSE", cex.main=.9, trace=1, col=1, col.main=col.main)
text(.2, .15, "calculated cex:\n0.69", cex=1, xpd=NA,  font=3, col=rgb(.1,.2,.4))
prp(a, compress=TRUE, ycompress=TRUE, main="\ncompress=TRUE (default)\nycompress=TRUE (default)", cex.main=.9, trace=1, col=1, col.main=col.main)
text(.2, .2, "calculated cex:\n0.82", cex=1, xpd=NA,   font=3,  col=rgb(.1,.2,.4))
par(old.par)

#--- compact.R ---

par(mfrow=c(3,3))
empty.plot()
empty.plot()
empty.plot()

a <- rpart(survived~., data=ptitanic, cp=.1)
col.main <- rgb(80, 100, 130, maxColorValue=255)
prp(a, ycompact=FALSE, xcompact=FALSE, faclen=0, trace=1, extra=6,
    main="[compact] xcompact=FALSE\nycompact=FALSE", cex.main=1, split.border.col=0, tweak=1.2)
rect(-.05, -.1, 1.05, 1.2, xpd=NA)
# par(mfg=c(1,1)) # for plotting on top of each other to see alignment
prp(a, faclen=0, trace=1, extra=6,
    main="\ndefault:\nxcompact=TRUE\nycompact=TRUE", cex.main=1, split.border.col=0, tweak=1.2)
rect(-.76, -1.96, 1.76, 3.35, xpd=NA) # , border=2)
par(old.par)

#--- compatibility example ---

par(mfrow=c(2,2))
fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis) # from example(rpart)
oldpar <- par(mfrow=c(1,2), xpd=NA) # side by side comparison
plot(fit)
text(fit, use.n=TRUE)
prp(fit, extra=1, uniform=F, branch=1, yesno=F, border.col=0, xsep="/")

#--- split.fun example ---

a9 <- rpart(Price/1000 ~ Mileage + Type + Country, cu.summary)
split.fun <- function(x, labs, digits, varlen, faclen)
{
    gsub(" = ", ":\n", labs)
}
prp(a9, extra=100, under=T, yesno=F, split.fun=split.fun, main="a9 (split.fun example)")

#--- utilities.R ---

tree <- rpart(survived~., data=ptitanic, cp=.02)
print(as.numeric(row.names(tree$frame))) # node numbers in the order they appear in frame

node <- 2
print(node %/% 2)                               # parent of node

print(inode <- match(node, nodes))              # row index of node in frame

print(is.leaf <- tree$frame$var == "<leaf>")    # logical vec, indexed on row in frame

print(nodes[is.leaf])                           # the leaf node numbers

print(is.left <- nodes %% 2 == 0)               # logical vec, indexed on row in frame

print(ifelse(is.left, nodes+1, nodes-1))        # siblings of nodes

get.children <- function(node)          # node and all its children
    if(is.leaf[match(node, nodes)]) {
        node
    } else
        c(node,
          get.children(2 * node),       # left child
          get.children(2 * node + 1))   # right child

print(get.children(2))

#--- split-label12.R ---

tree.split.label12 <- rpart(Price/1000 ~ Mileage + Type + Country, cu.summary)
split.fun <- function(x, labs, digits, varlen, faclen)
{
    gsub(" = ", ":\n", labs)
}
prp(tree.split.label12, extra=1, branch=1, split.border.col=1, split.yspace=1.5, main="split-label12")
prp(tree.split.label12, extra=1, branch=1, split.border.col=1, split.yspace=1.5, split.fun=split.fun)
prp(tree.split.label12, extra=100, under=T, yesno=F, split.fun=split.fun, tweak=.8, branch.lwd=.7)

#--- split-label13.R ---

par(mfrow=c(2,3), mar=c(5, 4, 2, 2))
tree.split.label13 <- rpart(Price/1000 ~ Mileage + Type + Country, cu.summary)
split.fun <- function(x, labs, digits, varlen, faclen)
{
    # replace commas with spaces (needed for strwrap)
    labs <- gsub(",", " ", labs)
    for(i in 1:length(labs)) {
        # split labs[i] into multiple lines
        labs[i] <- paste(strwrap(labs[i], width=25), collapse="\n")
    }
    labs
}
prp(tree, cex=1.05)
prp(tree.split.label13, split.fun=split.fun, main="split-label13")
split.fun.width15 <- function(x, labs, digits, varlen, faclen)
{
    # replace commas with spaces (needed for strwrap)
    labs <- gsub(",", " ", labs)
    for(i in 1:length(labs)) {
        # split labs[i] into multiple lines
        labs[i] <- paste(strwrap(labs[i], width=15), collapse="\n")
    }
    labs
}
prp(tree.split.label13, clip.facs=TRUE, split.fun=split.fun.width15, main="split-label13\nclip.facs=TRUE")

#--- fancy-rpart-plot.R --------------------------------

par(mfrow=c(3,3), mar=c(5, 4, 2, 2))
binary.model <- rpart(survived ~ ., data=ptitanic, cp=.02)
rpart.plot(binary.model, tweak=1.15, cex.main=1.1,
           main="rpart.plot (default)\n")
rpart.plot(binary.model, tweak=1, cex.main=1.1,
           extra=104, box.palette="GnBu", nn=TRUE,
           branch.lty=3, shadow.col="gray",
           main="rpart.plot (with args)\n")
library(rattle, quietly=TRUE)
fancyRpartPlot(binary.model, tweak=.8, cex.main=.9,
           main="\nfancyRpartPlot", sub="")
par(old.par)

#--- plotmo-examples.R ---------

kyphosis1 <- kyphosis
kyphosis1$Age <- kyphosis1$Age / 12
tree <- rpart(Kyphosis ~ ., data=kyphosis1)
par(mfrow=c(3, 3))
par(mar=c(.5, .5, 2, .5)) # mar is b l t r
par(mgp=c(1.6, 0.6, 0))
prp(tree, extra=7)
par(mar=c(.5, 1, 2, .5)) # mar is b l t r
set.seed(2016)
plotmo(tree, degree1=NA, do.par=F, theta=220-80, expand=.5,
       type="prob", nresponse="present", main="", ticktype="d", ntick=3)
par(mar=c(4, 4, 4, .5))
set.seed(2016)
plotmo(tree, degree1=NA, do.par=F, main="", type2="image",
       pt.col=ifelse(kyphosis1$Kyphosis=="present", "red", "lightblue"),
       pt.pch=20, cex.response=1, col.image=grey(10:4/10), ngrid2=300,
       type="prob", nresponse="present", yflip=T, xflip=T)
par(old.par)

#--- plotmo-ozone.R ---------

data(ozone1)
par(mfrow=c(3,3),
    mar=c(.5, 0.5, 2.5, .5), cex=.6, mgp = c(1.6, 0.6, 0))  # mar is b l t r
a1 <- rpart(O3~., data=ozone1)
prp(a1, type=1, cex=1, main="ozone level         \n", Margin=-.07)
col.persp <- rgb(220, 255, 255, maxColorValue=255)
theta <- -35
degree2 <- c("ibh", "temp")
set.seed(2016)
plotmo(a1, do.par=F, degree1=0, degree2=degree2, persp.theta=theta,
       ngrid2=20, col.persp=col.persp, ylim=c(0, 30))

degree2 <- c("ibt", "temp")
set.seed(2016)
plotmo(a1, do.par=F, degree1=0, degree2=degree2, persp.theta=theta,
       ngrid2=20, col.persp=col.persp)

a <- lm(O3~., data=ozone1)
set.seed(2016)
plotmo(a, degree1=0, do.par=F, degree2=degree2, persp.theta=theta,
       main="linear model", clip=F, col.persp=col.persp)

a <- earth(O3~., data=ozone1, degree=2)
set.seed(2016)
plotmo(a, degree1=0, do.par=F, degree2=degree2, persp.theta=theta,
       main="MARS", col.persp=col.persp)

library(randomForest)
set.seed(2018)
a <- randomForest(O3~., data=ozone1)
plotmo(a, degree1=0, do.par=F, degree2=degree2, persp.theta=theta,
       main="random forest", col.persp=col.persp)
par(old.par)

#--- cleartree.R ---

library(rpart.plot)
data(ptitanic)
survived <- rpart(survived ~ ., data=ptitanic, cp=.02)
par(mfrow=c(4,4))
rpart.plot(survived, main="default\n\n", cex.main=1)
rpart.plot(survived, yesno=2, main="yesno=2\n\n", cex.main=1)
rpart.plot(survived, type=3, clip.right.labs=FALSE, branch=.3, cex.main=1,
           main="type=3\nclip.right.labs=FALSE\nbranch=.3\n")
# points(.65, 1.4, pch=13 * 16 + 6, font=5, xpd=NA, cex=.9) # checkmark
rpart.plot(survived, type=4, clip.right.labs=FALSE, branch=.3, cex.main=1,
           main="type=4\nclip.right.labs=FALSE\nbranch=.3\n")
par(old.par)

#--- rules.R ---

old.par <- par(no.readonly=TRUE)
par(mfrow=c(5,3))
data(trees)
par(mar=c(0, 2, 5, 2))
Volume <- rpart(Volume ~ ., data=trees)
empty.plot()
rpart.plot(Volume, type=3, yspace=.4,
           clip.right.labs=FALSE,
           branch=.3, under=TRUE,
           cex=1.1, split.cex=.8)
empty.plot()
cat("rpart.rules(Volume):\n")
print(rpart.rules(Volume))

data(ptitanic)
survived <- rpart(survived ~ ., data=ptitanic, cp=.02)
par(mar=c(0, 0, 3, 0))
empty.plot()
rpart.plot(survived, type=3, clip.right.labs=FALSE, yspace=.2,
           branch=.3, under=TRUE,
           cex=.9, split.cex=1)
cat("rpart.rules(survived, cover=TRUE):\n")
print(rpart.rules(survived, cover=TRUE))
cat("rpart.rules(survived, roundint=FALSE):\n")
print(rpart.rules(survived, roundint=FALSE))
cat("rpart.rules(survived, clip.facs=TRUE):\n")
print(rpart.rules(survived, clip.facs=TRUE))
cat("rpart.rules(survived, extra=4, cover=TRUE):\n")
print(rpart.rules(survived, extra=4, cover=TRUE))
cat("rpart.rules(survived, extra=9, cover=TRUE):\n")
print(rpart.rules(survived, extra=9, cover=TRUE))
par(old.par)

source("test.epilog.R")
