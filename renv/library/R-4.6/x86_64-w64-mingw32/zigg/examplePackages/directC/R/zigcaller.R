
ziggrnorm <- function(n) {
    .Call(ziggrnorm_impl, n)
}

ziggsetseed <- function(s) {
    invisible(.Call(ziggsetseed_impl, s))
}
