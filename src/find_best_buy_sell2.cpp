#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_best_buy_sell2(NumericMatrix low_prob, NumericMatrix high_prob, NumericMatrix close_prob, NumericMatrix payoffs) {
  
  const int n_data = low_prob.nrow();
  const int n_buy_sell = payoffs.ncol();
  const int n_group_low = low_prob.ncol();
  const int n_group_high = high_prob.ncol();
  const int n_group_close = close_prob.ncol();
  
  const int n_sceanrio = n_group_low * n_group_high * n_group_close;
  
  IntegerVector out = IntegerVector(n_data);
  
  for (int i = 0; i<n_data; i++) {
    NumericVector prob = NumericVector(n_group_low*n_group_high*n_group_close);
    
    int counter = 0;
    for (int group_low = 0; group_low < n_group_low; group_low++) {
      for (int group_high = 0; group_high < n_group_high; group_high++) {
        for (int group_close = 0; group_close < n_group_close; group_close++) {
          prob[counter] = low_prob(i, group_low) * high_prob(i, group_high) * close_prob(i, group_close);
          counter++;
        }
      }
    }
    
    double best_payoff = 0.0;
    int best_buy_sell_id = -1;
    for (int buy_sell_id = 0; buy_sell_id < n_buy_sell; buy_sell_id++) {
      double curr_payoff = 0.0;
      for (int scenario_id = 0; scenario_id < n_sceanrio; scenario_id++) {
        curr_payoff += prob[scenario_id] * payoffs(scenario_id, buy_sell_id);
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
# set.seed(123)
# n <- 1000
# low_prob <- matrix(rnorm(30*n), ncol = 30)
# high_prob <- matrix(rnorm(30*n), ncol = 30)
# close_prob <- matrix(rnorm(30*n), ncol = 30)
# payoffs <- matrix(rnorm(30^3 * 750), ncol = 750)
# 
# find_best_buy_sell(low_prob, high_prob, close_prob, payoffs)
*/
