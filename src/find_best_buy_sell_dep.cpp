#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector find_best_buy_sell_dep(NumericMatrix prob, NumericMatrix buy_first_payoffs, NumericMatrix sell_first_payoffs, CharacterVector both_first) {
  
  const int n_data = prob.nrow();
  const int n_buy_sell = buy_first_payoffs.ncol();
  const int n_group = prob.ncol();
  
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
      for (int group = 0; group < n_group; group++) {
        curr_payoff += prob(i, group) * payoffs(price_id, buy_sell_id);
        price_id++;
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
# n <- 100
# both_first <- c("buy", "sell")[sample(c(1, 2), n, replace = TRUE)]
# prob <- matrix(rnorm(1000*n), ncol = 1000)
# buy_first_payoffs <- matrix(rnorm(1000 * 750), ncol = 750)
# sell_first_payoffs <- matrix(rnorm(1000 * 750), ncol = 750)
# find_best_buy_sell_dep(prob, buy_first_payoffs, sell_first_payoffs, both_first)
# 
# ### Validation (for element 1)
# find_best_buy_sell_dep(prob, buy_first_payoffs, buy_first_payoffs, both_first)
# element_id <- 1
# which.max(map_dbl(as_tibble(buy_first_payoffs) %>% as.list(), ~sum(.*prob[1, ])))
*/
