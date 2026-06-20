// [[Rcpp::plugins(cpp17)]]
#include "rsynthea.h"

// Trivial exported function to verify the toolchain compiles end-to-end.
// [[Rcpp::export]]
int rcpp_hello(int x) {
    return x + 1;
}
