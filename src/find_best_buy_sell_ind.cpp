#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_best_buy_sell_ind(NumericMatrix low_prob, NumericMatrix high_prob, NumericMatrix close_prob, NumericMatrix buy_first_payoffs, NumericMatrix sell_first_payoffs, CharacterVector both_first) {
  
  const int n_data = low_prob.nrow();
  const int n_buy_sell = buy_first_payoffs.ncol();
  
  const int n_group_low = low_prob.ncol();
  const int n_group_high = high_prob.ncol();
  const int n_group_close = close_prob.ncol();
  
  IntegerVector out = IntegerVector(n_data);
  
  for (int i = 0; i<n_data; i++) {
    NumericMatrix payoffs;
    if (both_first[i] == "buy") {
      payoffs = buy_first_payoffs;
    } else if (both_first[i] == "sell") {
      payoffs = sell_first_payoffs;
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
# set.seed(1)
# n <- 1000
# both_first <- c("buy", "sell")[sample(c(1, 2), n, replace = TRUE)]
# low_prob <- matrix(rnorm(30*n), ncol = 30)
# high_prob <- matrix(rnorm(30*n), ncol = 30)
# close_prob <- matrix(rnorm(30*n), ncol = 30)
# buy_first_payoffs <- matrix(rnorm(30^3 * 750), ncol = 750)
# sell_first_payoffs <- matrix(rnorm(30^3 * 750), ncol = 750)
# find_best_buy_sell(low_prob, high_prob, close_prob, buy_first_payoffs, sell_first_payoffs, both_first)

### Validation (for element 1)
# element_id <- 1
# prob <- expand_grid(
#   low_prob = as.vector(low_prob[element_id, ]),
#   high_prob = as.vector(high_prob[element_id, ]),
#   close_prob = as.vector(close_prob[element_id, ])
# ) %>% mutate(prob = low_prob * high_prob*close_prob) %$% prob
# which.max(map_dbl(as_tibble(payoffs) %>% as.list(), ~sum(.*prob)))

*/
