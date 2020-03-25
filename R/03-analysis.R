
calc_payoff <- function(quotes_line, sell = quotes$High, buy = quotes$Low, gamma_cash = 1) {
  
  I_sell <- quotes$High >= sell
  I_buy <- quotes$Low >= buy
  
  sell_to <- log(sell/quotes$close)
  
}