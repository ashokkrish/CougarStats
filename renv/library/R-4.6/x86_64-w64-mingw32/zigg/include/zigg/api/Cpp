/*
   This header file provides the interface used by other packages,
   and should be included once per package.
*/


#ifndef _r_api_zigg_h_
#define _r_api_zigg_h_

/* number of R header files (possibly listing too many) */
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <R_ext/Rdynload.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
    # define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
    # define attribute_hidden
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* provided the interface for the function exported 	*/
/* in ../src/init.c via R_RegisterCCallable()		*/

SEXP attribute_hidden zrnorm(SEXP n) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("zigg", "zrnorm");
    return fun(n);
}

SEXP attribute_hidden zrexp(SEXP n) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("zigg", "zrexp");
    return fun(n);
}

SEXP attribute_hidden zrunif(SEXP n) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("zigg", "zrunif");
    return fun(n);
}

SEXP attribute_hidden zsetseed(SEXP n) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("zigg", "zsetseed");
    return fun(n);
}

#ifdef __cplusplus
}

/* add a namespace for C++ use */
namespace R {
    inline SEXP zrnorm(SEXP n) {
        return ::zrnorm(n);
    }
    inline SEXP zsetseed(SEXP n) {
        return ::zsetseed(n);
    }
}

#endif

#endif /* _r_api_zigg_h_ */
