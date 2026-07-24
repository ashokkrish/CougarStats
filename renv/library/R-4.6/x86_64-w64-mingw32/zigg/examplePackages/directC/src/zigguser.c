
#include <zigg/api/C>

/* declaration of external functions exported by package zigg */
/* see file src/init.c for the R_GetCCallable() call to assign these */
extern SEXP (*zigg_zrnorm)(SEXP);
extern SEXP (*zigg_zsetseed)(SEXP);

SEXP ziggrnorm_impl(SEXP ns) {
    return zigg_zrnorm(ns);
}

SEXP ziggsetseed_impl(SEXP ns) {
    return zigg_zsetseed(ns);
}
