#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::IntegerMatrix drop_na_nn_idx(Rcpp::IntegerMatrix mat, int k) {
  
  int ncol = mat.ncol();
  int nrow = mat.nrow();
  
  Rcpp::IntegerVector v(nrow * k, NA_INTEGER);
  v.attr("dim") = Rcpp::Dimension(nrow, k);
  Rcpp::IntegerMatrix out = Rcpp::as<Rcpp::IntegerMatrix>(v);
  
  // Filling output matrix
  int col_out = 0;
  for (int row = 0; row < nrow; row++) {
    for (int col_in = 0; col_in < ncol; col_in++) {
      if (mat(row, col_in) != NA_INTEGER) {
        out(row, col_out) = mat(row, col_in);
        col_out++;
        if (col_out == k) break;
      }
    }   
    col_out = 0;
  }
  return out;
}

/*** R
# drop_na_nn_idx(matrix(c(1, 2, NA, 2, NA, 4), ncol = 2), 3)
*/



// [[Rcpp::export]]
Rcpp::NumericMatrix drop_na_nn_dists(Rcpp::NumericMatrix mat, int k) {
  
  int ncol = mat.ncol();
  int nrow = mat.nrow();
  
  Rcpp::NumericVector v(nrow * k, NA_REAL);
  v.attr("dim") = Rcpp::Dimension(nrow, k);
  Rcpp::NumericMatrix out = Rcpp::as<Rcpp::NumericMatrix>(v);
  
  // Filling output matrix
  int col_out = 0;
  for (int row = 0; row < nrow; row++) {
    for (int col_in = 0; col_in < ncol; col_in++) {
      if (!Rcpp::NumericVector::is_na(mat(row, col_in))) {
        out(row, col_out) = mat(row, col_in);
        col_out++;
        if (col_out == k) break;
      }
    }   
    col_out = 0;
  }
  return out;
}

/*** R
# drop_na_nn_dists(matrix(c(1.23, 2.34, NA, 2.4, NA, 4.0), ncol = 2), 3)
*/
