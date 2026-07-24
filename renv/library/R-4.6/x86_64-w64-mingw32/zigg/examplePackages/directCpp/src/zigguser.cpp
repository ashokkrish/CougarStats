
#include <zigg/api/Cpp>

extern "C" {
    SEXP ziggrnorm_impl(SEXP ns) {
        return R::zrnorm(ns);
    }

    SEXP ziggsetseed_impl(SEXP ns) {
        return R::zsetseed(ns);
    }
}
