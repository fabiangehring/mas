
// [[Rcpp::plugins(cpp11)]]   
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double calc_payoff_per_title_internal(double close_1, double close_0, double first, double second, double low, double high, double gamma, String return_type) {
  
  double prev_price = close_1;
  
  double r_close_0_close_1;
  double r_first_close_1;
  double r_close_0_first;
  double r_second_close_1;
  double r_second_first;
  double r_close_0_second;
  
  double gamma_cash_1;
  double gamma_cash_first;
  double gamma_cash_second;
  
  
  // transaction is done if 
  //   a) prices is hit and 
  //   b) price is coming from "correct" side (from a lower prices to sell, from an upper prices to buy)
  
  // check if first price is executed
  int I_first = false;    
  if (first < second and first >= low and prev_price > first) {
    I_first = true;
    prev_price = first;
  } else if (first >= second and first <= high and prev_price < first) {
    I_first = true;
    prev_price = first;
  }
  
  // check if second price is executed
  int I_second = false;    
  if (first >= second and second >= low and prev_price > second) {
    I_second = true;
  } else if (first < second and second <= high and prev_price < second) {
    I_second = true;
  }
  
  // calc returns
  if (return_type == "cont") {
    r_first_close_1 = log(first/close_1);
    r_close_0_first = log(close_0/first);
    r_second_close_1 = log(second/close_1);
    r_second_first = log(second/first);
    r_close_0_second = log(close_0/second);
    r_close_0_close_1 = log(close_0/close_1);
  } else if (return_type == "disc") {
    r_first_close_1 = first/close_1-1;
    r_close_0_first = close_0/first-1;
    r_second_close_1 = second/close_1-1;
    r_second_first = second/first-1;
    r_close_0_second = close_0/second-1;
    r_close_0_close_1 = close_0/close_1-1;
  }
  
  gamma_cash_1 = gamma * pow(close_1, 2)/100;
  gamma_cash_first = gamma * pow(first, 2)/100;
  gamma_cash_second = gamma * pow(second, 2)/100;
  
  double out;
  if (!I_first and !I_second) {
    out = gamma_cash_1 * pow(r_close_0_close_1, 2);
  } else if (I_first and !I_second) {
    out = gamma_cash_1 * pow(r_first_close_1, 2) + gamma_cash_first * pow(r_close_0_first, 2);
  } else  if (!I_first and I_second) {
    out = gamma_cash_1 * pow(r_second_close_1, 2) + gamma_cash_second * pow(r_close_0_second, 2);
  } else {
    out = gamma_cash_1 * pow(r_first_close_1, 2) + gamma_cash_first * pow(r_second_first, 2) + gamma_cash_second * pow(r_close_0_second, 2);
  }
  return out * 0.5 * 100;
}
/*** R

calc_payoff_per_title_internal(100, 102, 99, 102, 98, 105, 0.2, "disc")
calc_payoff_per_title(100, 102, 99, 102, 98, 105, 0.2, "disc")

*/


// // [[Rcpp::plugins(cpp11)]]   
// #include <Rcpp.h>
// using namespace Rcpp;
// 
// // [[Rcpp::export]]
// NumericVector calc_payoff_per_title_old(NumericVector close_1, NumericVector close_0, NumericVector first, NumericVector second, NumericVector low, NumericVector high, double gamma, String return_type) {
//   
//   int n = first.length();
//   NumericVector prev_price = clone(close_1);
//   NumericVector out = NumericVector(n);
//   
//   double r_close_0_close_1;
//   double r_first_close_1;
//   double r_close_0_first;
//   double r_second_close_1;
//   double r_second_first;
//   double r_close_0_second;
//   
//   double gamma_cash_1;
//   double gamma_cash_first;
//   double gamma_cash_second;
//   
//   for (int i = 0; i < n; i++) {
//     
//     // transaction is done if 
//     //   a) prices is hit and 
//     //   b) price is coming from "correct" side (from a lower prices to sell, from an upper prices to buy)
//     
//     // check if first price is executed
//     int I_first = false;    
//     if (first[i] < second[i] and first[i] >= low[i] and prev_price[i] > first[i]) {
//       I_first = true;
//       prev_price[i] = first[i];
//     } else if (first[i] >= second[i] and first[i] <= high[i] and prev_price[i] < first[i]) {
//       I_first = true;
//       prev_price[i] = first[i];
//     }
//     
//     // check if second price is executed
//     int I_second = false;    
//     if (first[i] >= second[i] and second[i] >= low[i] and prev_price[i] > second[i]) {
//       I_second = true;
//     } else if (first[i] < second[i] and second[i] <= high[i] and prev_price[i] < second[i]) {
//       I_second = true;
//     }
//     
//     // calc returns
//     if (return_type == "cont") {
//       r_first_close_1 = log(first[i]/close_1[i]);
//       r_close_0_first = log(close_0[i]/first[i]);
//       r_second_close_1 = log(second[i]/close_1[i]);
//       r_second_first = log(second[i]/first[i]);
//       r_close_0_second = log(close_0[i]/second[i]);
//       r_close_0_close_1 = log(close_0[i]/close_1[i]);
//     } else if (return_type == "disc") {
//       r_first_close_1 = first[i]/close_1[i]-1;
//       r_close_0_first = close_0[i]/first[i]-1;
//       r_second_close_1 = second[i]/close_1[i]-1;
//       r_second_first = second[i]/first[i]-1;
//       r_close_0_second = close_0[i]/second[i]-1;
//       r_close_0_close_1 = close_0[i]/close_1[i]-1;
//     }
//     
//     gamma_cash_1 = gamma * pow(close_1[i], 2)/100;
//     gamma_cash_first = gamma * pow(first[i], 2)/100;
//     gamma_cash_second = gamma * pow(second[i], 2)/100;
//     
//     if (!I_first and !I_second) {
//       out[i] = gamma_cash_1 * pow(r_close_0_close_1, 2);
//     } else if (I_first and !I_second) {
//       out[i] = gamma_cash_1 * pow(r_first_close_1, 2) + gamma_cash_first * pow(r_close_0_first, 2);
//     } else  if (!I_first and I_second) {
//       out[i] = gamma_cash_1 * pow(r_second_close_1, 2) + gamma_cash_second * pow(r_close_0_second, 2);
//     } else {
//       out[i] = gamma_cash_1 * pow(r_first_close_1, 2) + gamma_cash_first * pow(r_second_first, 2) + gamma_cash_second * pow(r_close_0_second, 2);
//     }
//   }
//   
//   return 0.5 * 100 * out;
// }


// [[Rcpp::plugins(cpp11)]]   
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_payoff_per_title(NumericVector close_1, NumericVector close_0, NumericVector first, NumericVector second, NumericVector low, NumericVector high, double gamma, String return_type) {
  int n = first.length();
  NumericVector out = NumericVector(n);
  for (int i = 0; i < n; i++) {
    out[i] = calc_payoff_per_title_internal(close_1[i], close_0[i], first[i], second[i], low[i], high[i], gamma, return_type);
  }
  return out;
}


/*** R
# quotes_line_test_1 <- tribble(
#   ~Close_1, ~Low, ~High, ~Close_0,
#         10,    8,    12,        9
# )
# 
# 
# calc_payoff_per_title(
#   close_1 = quotes_line_test_1$Close_1,
#   close_0 = quotes_line_test_1$Close_0,
#   first = quotes_line_test_1$Low,
#   second  = quotes_line_test_1$High,
#   low = quotes_line_test_1$Low,
#   high = quotes_line_test_1$High,
#   gamma = 0.2,
#   return_type = "disc"
# )
# 
# calc_payoff_per_title(
#   close_1 = c(100, 100, 100),
#   close_0 = c(100, 102, 104),
#   first = c(100, 101, 102),
#   second  = c(102, 103, 104),
#   low = c(100, 101, 102),
#   high = c(102, 103, 104),
#   gamma = 0.2,
#   return_type = "disc"
# )
*/



// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List calc_best_buy_sell(NumericVector probs, double min_prob, NumericVector low, NumericVector high, NumericVector close, String both_first) {
  
  int n = probs.length();
  
  int best_payoff_idx = -1;
  double best_payoff = -1;
  for (int i=0; i<n; i++) {
    if (probs[i] < min_prob) continue;
    
    double first;
    double second;
    if (both_first == "buy") {
      first = low[i];
      second = high[i];
    } else if (both_first == "sell") {
      second = low[i];
      first = high[i];
    }
    
    double curr_payoff = 0.0;
    double total_prob = 0.0;
    for (int j = 0; j<n; j++){
      if (probs[j] < min_prob) continue;
      curr_payoff += probs[j] * calc_payoff_per_title_internal(100.0, close[j], first, second, low[j], high[j], 0.2, "disc");
      total_prob += probs[j];
    }
    
    double scaled_payoff = curr_payoff / total_prob;
    if (scaled_payoff > best_payoff) {
      best_payoff = scaled_payoff;
      best_payoff_idx = i;
    }
  }
  
  return List::create(Named("buy") = low[best_payoff_idx], Named("sell") = high[best_payoff_idx]);
}

/*** R
# calc_best_buy_sell(c(0.5, 0.5), 0.3, c(100, 100), c(102, 103), c(100, 105), "buy")

# calc_best_buy_sell(probs, 0.001, low, high, close, "buy")


# calc_payoff_per_title(100, 100, 102, 102, 100, 102, 0.2, "disc");
*/



