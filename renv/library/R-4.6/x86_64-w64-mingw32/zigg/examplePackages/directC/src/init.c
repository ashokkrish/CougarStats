#include <R.h>
#include <Rinternals.h>         /* for SEXP */
#include <R_ext/Rdynload.h>

SEXP ziggrnorm_impl(SEXP ns);
SEXP ziggsetseed_impl(SEXP ns);

/* exporting the twp C level functions in this package */
static const R_CallMethodDef CallEntries[] = {
    {"ziggrnorm_impl",   (DL_FUNC) &ziggrnorm_impl,   1},
    {"ziggsetseed_impl", (DL_FUNC) &ziggsetseed_impl, 1},
    {NULL, NULL, 0}
};

/* declaration of functions exported by package zigg */
/* both are used by our two functions */
SEXP (*zigg_zrnorm)(SEXP);
SEXP (*zigg_zsetseed)(SEXP);

void R_init_zigguserDirectC(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    /* obtain C function pointers to functions called from zigg */
    zigg_zrnorm = (SEXP(*)(SEXP)) R_GetCCallable("zigg", "zrnorm");
    zigg_zsetseed = (SEXP(*)(SEXP)) R_GetCCallable("zigg", "zsetseed");
}
