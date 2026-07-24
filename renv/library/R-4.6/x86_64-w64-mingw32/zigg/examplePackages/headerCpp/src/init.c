#include <R.h>
#include <Rinternals.h>         /* for SEXP */
#include <R_ext/Rdynload.h>

SEXP ziggrnorm_impl(SEXP ns);
SEXP ziggsetseed_impl(SEXP ns);

static const R_CallMethodDef CallEntries[] = {
    {"ziggrnorm_impl",   (DL_FUNC) &ziggrnorm_impl,   1},
    {"ziggsetseed_impl", (DL_FUNC) &ziggsetseed_impl, 1},
    {NULL, NULL, 0}
};

void R_init_zigguserHeaderCpp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
