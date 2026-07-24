#include <zigg/header>             					// provided by the zigg package

#include <R.h>
#include <Rinternals.h>         					// for SEXP

static zigg::Ziggurat ziggurat; 					// static instance ensures initialization

extern "C" {

    SEXP ziggrnorm_impl(SEXP ns) {
        int n = Rf_asInteger(ns);
        SEXP v = PROTECT(Rf_allocVector(REALSXP, n));
        double* p = REAL(v);
        for (int i = 0; i < n; i++) {
            p[i] = ziggurat.rnorm();
        }
        UNPROTECT(1);
        return v;
    }

    SEXP ziggsetseed_impl(SEXP ss) {
        double sd = Rf_asReal(ss);
        uint32_t su = static_cast<uint32_t>(sd);
        ziggurat.setSeed(su);
        return R_NilValue;
    }

}
