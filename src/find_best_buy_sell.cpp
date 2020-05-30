#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector find_best_buy_sell(NumericMatrix low_prob, NumericMatrix high_prob, NumericMatrix close_prob, NumericMatrix payoffs) {
  const int n_data = low_prob.nrow();
  const int n_buy_sell = payoffs.ncol();
  const int n_group_low = low_prob.ncol();
  const int n_group_high = high_prob.ncol();
  const int n_group_close = close_prob.ncol();
  
  NumericVector out = NumericVector(n_data);
  
  for (int i = 0; i<n_data; i++) {
    double best_payoff = 0.0;
    for (int buy_sell_id = 0; buy_sell_id < n_buy_sell; buy_sell_id++) {
      double curr_payoff = 0.0;
      int price_id = 0;
      for (int group_low = 0; group_low < n_group_low; group_low++) {
        for (int group_high = 0; group_high < n_group_high; group_high++) {
          for (int group_close = 0; group_close < n_group_close; group_close++) {
            curr_payoff += low_prob(i, group_low) * high_prob(i, group_high) * close_prob(i, group_close) * payoffs(price_id, buy_sell_id);
            price_id++;
          }
        }
      }
      if (curr_payoff > best_payoff) {
        best_payoff = curr_payoff;
      }
    }
    out[i] = best_payoff;
  }
  return out;
}

/*** R
low_prob <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.4, 0.3, 0.2, 0.1), ncol = 4, byrow = TRUE)
high_prob <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.4, 0.3, 0.2, 0.1), ncol = 4, byrow = TRUE)
close_prob <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.4, 0.3, 0.2, 0.1), ncol = 4, byrow = TRUE)
payoffs <- matrix(rep(1:64, 3), ncol = 3)

find_best_buy_sell(low_prob, high_prob, close_prob, payoffs)
*/
