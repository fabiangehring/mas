// [[Rcpp::plugins(cpp11)]]   
#include <random>
#include <Rcpp.h>
// [[Rcpp::depends(dqrng)]]

#include <boost/random/binomial_distribution.hpp>
#include <xoshiro.h>

using binomial = boost::random::binomial_distribution<int>;

// [[Rcpp::export]]
Rcpp::IntegerMatrix bootstrap_nn_idx(Rcpp::IntegerMatrix mat, Rcpp::IntegerVector size, int k, int seed) {
  
  dqrng::xoroshiro128plus rng(seed);
  boost::random::binomial_distribution<int> dist;
  
  int ncol = mat.ncol();
  int nrow = mat.nrow();
  
  // Initialitzing output matrix
  Rcpp::IntegerVector v(nrow * k, NA_INTEGER);
  v.attr("dim") = Rcpp::Dimension(nrow, k);
  Rcpp::IntegerMatrix out = Rcpp::as<Rcpp::IntegerMatrix>(v);
  
  // Filling output matrix
  int col_out = 0;
  for (int row = 0; row < nrow; row++) {
    for (int col_in = 0; col_in < ncol; col_in++) {
      if (mat(row, col_in) != NA_INTEGER) {
        int n_copy = dist(rng, binomial::param_type(size[row], 1.0/size[row]));
        for (int copy = 0; copy < n_copy; copy++) {
          out(row, col_out) = mat(row, col_in);
          col_out++;
          if (col_out == k) break;
        }
        if (col_out == k) break;
      }    
    }
    col_out = 0;
  }

  return out;
}


/*** R
# bootstrap_nn_idx(matrix(c(1, NA, 3, NA, 2, 4, NA, 8, NA), ncol = 3), 1:3, 2, 123456)
*/
