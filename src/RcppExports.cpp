// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// bin_value
double bin_value(NumericMatrix bin, double r, int left, int right);
RcppExport SEXP _tidySpectR_bin_value(SEXP binSEXP, SEXP rSEXP, SEXP leftSEXP, SEXP rightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type bin(binSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< int >::type left(leftSEXP);
    Rcpp::traits::input_parameter< int >::type right(rightSEXP);
    rcpp_result_gen = Rcpp::wrap(bin_value(bin, r, left, right));
    return rcpp_result_gen;
END_RCPP
}
// recc_split
IntegerVector recc_split(NumericMatrix bin, double r, double noise, int left, int right, IntegerVector indices);
RcppExport SEXP _tidySpectR_recc_split(SEXP binSEXP, SEXP rSEXP, SEXP noiseSEXP, SEXP leftSEXP, SEXP rightSEXP, SEXP indicesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type bin(binSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type noise(noiseSEXP);
    Rcpp::traits::input_parameter< int >::type left(leftSEXP);
    Rcpp::traits::input_parameter< int >::type right(rightSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indices(indicesSEXP);
    rcpp_result_gen = Rcpp::wrap(recc_split(bin, r, noise, left, right, indices));
    return rcpp_result_gen;
END_RCPP
}
// vnoise
double vnoise(NumericMatrix bin, double r);
RcppExport SEXP _tidySpectR_vnoise(SEXP binSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type bin(binSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(vnoise(bin, r));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tidySpectR_bin_value", (DL_FUNC) &_tidySpectR_bin_value, 4},
    {"_tidySpectR_recc_split", (DL_FUNC) &_tidySpectR_recc_split, 6},
    {"_tidySpectR_vnoise", (DL_FUNC) &_tidySpectR_vnoise, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_tidySpectR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
