#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_best_buy_sell(NumericMatrix low_prob, NumericMatrix high_prob, NumericMatrix close_prob, NumericMatrix payoffs) {
  const int n_data = low_prob.nrow();
  const int n_buy_sell = payoffs.ncol();
  const int n_group_low = low_prob.ncol();
  const int n_group_high = high_prob.ncol();
  const int n_group_close = close_prob.ncol();
  
  IntegerVector out = IntegerVector(n_data);
  
  for (int i = 0; i<n_data; i++) {
  
    if (i % 1000 == 0) {
      Rprintf("%i\n", i);
    }

    double best_payoff = 0.0;
    int best_buy_sell_id = -1;
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
        best_buy_sell_id = buy_sell_id;
      }
    }
    out[i] = best_buy_sell_id + 1;
  }
  return out;
}

/*** R

low_prob <- matrix(rep(0.1, 30 * 100000), ncol = 30)
high_prob <- matrix(rep(0.1, 30 * 100000), ncol = 30)
close_prob <- matrix(rep(0.1, 30 * 100000), ncol = 30)
payoffs <- matrix(rep(1:10000, 750), ncol = 750)

a <- system.time({find_best_buy_sell(low_prob, high_prob, close_prob, payoffs)})
*/
