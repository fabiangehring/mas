// [[Rcpp::plugins(cpp11)]]   
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_payoff_per_title(NumericVector close_1, NumericVector close_0, NumericVector first, NumericVector second, NumericVector low, NumericVector high, double gamma, String return_type) {
  
  int n = first.length();
  NumericVector prev_price = clone(close_1);
  NumericVector out = NumericVector(n);
  
  double r_close_0_close_1;
  double r_first_close_1;
  double r_first_close_0;
  double r_second_close_1;
  double r_second_first;
  double r_close_0_second;
  
  double gamma_cash_1;
  double gamma_cash_first;
  double gamma_cash_second;
  
  for (int i = 0; i < n; i++) {
    
    // transaction is done if 
    //   a) prices is hit and 
    //   b) price is coming from "correct" side (from a lower prices to sell, from an upper prices to buy)
    
    // check if first price is executed
    int I_first = false;    
    if (first[i] < second[i] and first[i] >= low[i] and prev_price[i] > first[i]) {
      I_first = true;
      prev_price[i] = first[i];
    } else if (first[i] >= second[i] and first[i] <= high[i] and prev_price[i] < first[i]) {
      I_first = true;
      prev_price[i] = first[i];
    }
    
    // check if second price is executed
    int I_second = false;    
    if (first[i] >= second[i] and second[i] >= low[i] and prev_price[i] > second[i]) {
      I_second = true;
    } else if (first[i] < second[i] and second[i] <= high[i] and prev_price[i] < second[i]) {
      I_second = true;
    }
    
    // calc returns
    if (return_type == "cont") {
      r_first_close_1 = log(first[i]/close_1[i]);
      r_first_close_0 = log(first[i]/close_0[i]);
      r_second_close_1 = log(second[i]/close_1[i]);
      r_second_first = log(second[i]/first[i]);
      r_close_0_second = log(close_0[i]/second[i]);
      r_close_0_close_1 = log(close_0[i]/close_1[i]);
    } else if (return_type == "disc") {
      r_first_close_1 = first[i]/close_1[i]-1;
      r_first_close_0 = first[i]/close_0[i]-1;
      r_second_close_1 = second[i]/close_1[i]-1;
      r_second_first = second[i]/first[i]-1;
      r_close_0_second = close_0[i]/second[i]-1;
      r_close_0_close_1 = close_0[i]/close_1[i]-1;
    }
  
    gamma_cash_1 = gamma * pow(close_1[i], 2)/100;
    gamma_cash_first = gamma * pow(first[i], 2)/100;
    gamma_cash_second = gamma * pow(second[i], 2)/100;
    
    if (!I_first and !I_second) {
      out[i] = gamma_cash_1 * pow(r_close_0_close_1, 2);
    } else if (I_first and !I_second) {
      out[i] = gamma_cash_1 * pow(r_first_close_1, 2) + gamma_cash_first * pow(r_first_close_0, 2);
    } else  if (!I_first and I_second) {
      out[i] = gamma_cash_1 * pow(r_second_close_1, 2) + gamma_cash_second * pow(r_close_0_second, 2);
    } else {
      out[i] = gamma_cash_1 * pow(r_first_close_1, 2) + gamma_cash_first * pow(r_second_first, 2) + gamma_cash_second * pow(r_close_0_second, 2);
    }
  }
  
  return 0.5 * 100 * out;
}


/*** R
# quotes_line_test_1 <- tribble(
#   ~Close_1, ~Low, ~High, ~Close_0,
#         10,    8,    12,        9
# )
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
*/
