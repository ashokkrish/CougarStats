
suppressMessages({
    library(zigg)
    library(microbenchmark)
    library(ggplot2)
    library(patchwork)
})

N <- 1e6

nr <- microbenchmark(ziggN=zrnorm(N), baseRN=rnorm(N))
nr

ne <- microbenchmark(ziggE=zrexp(N), baseRE=rexp(N))
ne

nu <- microbenchmark(ziggU=zrunif(N), baseRU=runif(N))
nu

p1 <- autoplot(nr) + ggtitle("rnorm")
p2 <- autoplot(ne) + ggtitle("rexp")
p3 <- autoplot(nu) + ggtitle("runif")
p1 + p2 + p3

renamedDF <- function(res, txt, char) {
    DF <- as.data.frame(res)
    DF$expr <- as.factor(gsub(char, "", as.character(DF$expr)))
    DF$type <- as.factor(txt)
    DF
}

dat <- rbind(renamedDF(nr, "normal", "N"),
             renamedDF(ne, "exponential", "E"),
             renamedDF(nu, "uniform", "U"))

## next few lines borrowed from autoplot.microbenchmark
unit <- microbenchmark:::determine_unit(dat, NULL)
dat$ntime <- microbenchmark:::convert_to_unit(dat, unit)
y_label <- sprintf("Time in %s for neval=%d draws of size N=%d",
                   attr(dat$ntime, "unit"), nrow(dat)/length(levels(dat$expr))/3, N)

p <- ggplot(dat) +
    aes(y=ntime, x=expr, z=type) +
    stat_ydensity() +
    coord_flip(ylim=c(0,max(dat$ntime))) +
    scale_y_continuous(name = y_label) +
    scale_x_discrete(name = "") +
    facet_grid(~ type) +
    labs(title="Timing comparison of the Ziggurat PRNG over base R",
         subtitle="See script demo/timings.R in package 'zigg' for code",
         caption=paste("Updated on", format(trunc(Sys.time())))) +
    tinythemes::theme_ipsum_rc()

if (FALSE) {
    filename <- file.path("ziggurat_base_R_comparison.png")
    png(filename, 1200, 600)
    p
    ignoreme <- dev.off()
}
