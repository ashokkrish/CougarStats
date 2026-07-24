#include <zigg/header>             					// provided by the zigg package
#include <Rcpp/Lightest>

static zigg::Ziggurat ziggurat; 					// static instance ensures initialization

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector ziggrnorm(int n) {
    Rcpp::NumericVector v(n);
    for (int i = 0; i < n; i++) {
        v[i] = ziggurat.rnorm();
    }
    return v;
}

// [[Rcpp::export(rng = false)]]
void ziggsetseed(double s) {
    uint32_t su = static_cast<uint32_t>(s);
    ziggurat.setSeed(su);
}
