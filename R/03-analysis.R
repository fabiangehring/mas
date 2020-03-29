
#' Calculate the payoff from prices moves
#'
#' @param quotes_line A data.frame with at least the following columns: "Adjusted_t-1", "Low", "High", "Adjusted_t"
#' @param sell The upper price at which the the delta hedge should be "rebalanced"
#' @param buy The lower price at which the the delta hedge should be "rebalanced"
#' @param gamma_cash The value used for gamma cash
#' @param both_first Character indicating what price movement is assumed first in cases where both, the buy and sell prices are reaches. One of 
#' c("ind", "buy", "sell", "min", "max"). "ind" is used to take into account p- and down moves independently. Economically this is equal to a situation
#' where hedges are not rebalanced at "buy" or "sell" levels but also at the  Adjusted_t-1. "buy" indicates that the buy event was reached first. "sell" 
#' indicates that the sell event was reached first. "min" conservativly considers the move first that leaves to the least pnl. "max" uses the event first
#' that least to highest pnl.
#'
#' @return The pnl
#' @export
#'
#' @examples
#' quotes_line <- tribble(
#'    ~`Adjusted_t-1`, ~Low, ~High, ~Adjusted_t,
#'                 10,     8,    12,           9
#'  )
#' move <- c(0.03, 0.03)
#' calc_payoff_const_gamma_cash(quotes_line, both_first = "ind")
#' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "sell")
#' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "buy")
#' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "min")
#' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "max")
calc_payoff_const_gamma_cash <- function(quotes_line, buy = NULL, sell = NULL, gamma_cash = 1,  both_first = "ind") {
  
  stopifnot(is.null(sell) || is.null(buy) || all(buy<=sell))
  
  I_sell <- if (!is.null(sell)) quotes_line$High >= sell else rep(FALSE, nrow(quotes_line))
  I_buy <- if (!is.null(buy))  quotes_line$Low <= buy else rep(FALSE, nrow(quotes_line))
  
  # sell only
  I_sell_only <- (I_sell == TRUE) * (I_buy == FALSE)
  sell_part_1 <- log(sell[I_sell_only] / quotes_line$`Adjusted_t-1`[I_sell_only])^2
  sell_part_2 <- log(quotes_line$Adjusted_t[I_sell_only] / sell[I_sell_only])^2
  pnl_sell <- sum(sell_part_1, sell_part_2)
  
  # buy only
  I_buy_only <- (I_sell == FALSE) * (I_buy == TRUE)
  buy_part_1 <- log(buy[I_buy_only] / quotes_line$`Adjusted_t-1`[I_buy_only])^2
  buy_part_2 <- log(quotes_line$Adjusted_t[I_buy_only] / buy[I_buy_only])^2
  pnl_buy <- sum(buy_part_1, buy_part_2)
  
  # none
  I_none <- (I_sell == FALSE) * (I_buy == FALSE)
  none <- log(quotes_line$`Adjusted_t`[I_none] / quotes_line$`Adjusted_t-1`[I_none])^2
  pnl_none <- sum(none)
  
  # both
  I_both <- (I_sell == TRUE) * (I_buy == TRUE)
  if (both_first == "ind") {
    both_sell_part_1 <- log(sell[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
    both_sell_part_2 <- log(quotes_line$Adjusted_t[I_both] / sell[I_both])^2
    both_buy_part_1 <- log(buy[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
    both_buy_part_2 <- log(quotes_line$Adjusted_t[I_both] / buy[I_both])^2
    pnl_both <- sum(both_sell_part_1, both_sell_part_2, both_buy_part_1, both_buy_part_2)
  } else if (both_first %in% c("buy", "sell")) {
    
    if (both_first == "buy") {
      first_price <- buy
      second_price <- sell
    } else {
      first_price <- sell
      second_price <- buy
    }
    
    both_part_1 <- log(first_price[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
    both_part_2 <- log(second_price[I_both] / first_price[I_both])^2
    both_part_3 <- log(quotes_line$`Adjusted_t`[I_both] / second_price[I_both])^2
    pnl_both <- sum(both_part_1, both_part_2, both_part_3)
    
  } else if (both_first %in% c("min", "max")) {
    
    both_sell_first_part_1 <- log(sell[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
    both_sell_first_part_2 <- log(buy[I_both] / sell[I_both])^2
    both_sell_first_part_3 <- log(quotes_line$`Adjusted_t`[I_both] / buy[I_both])^2
    both_sell_first <- both_sell_first_part_1 + both_sell_first_part_2 + both_sell_first_part_3
    
    both_buy_first_part_1 <- log(buy[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
    both_buy_first_part_2 <- log(sell[I_both] / buy[I_both])^2
    both_buy_first_part_3 <- log(quotes_line$`Adjusted_t`[I_both] / sell[I_both])^2
    both_buy_first <- both_buy_first_part_1 + both_buy_first_part_2 + both_buy_first_part_3
    
    if (both_first == "min") {
      pnl_both <- sum(pmin(both_sell_first, both_buy_first))
    } else {
      pnl_both <- sum(pmax(both_sell_first, both_buy_first))
    }
  }
  
  return(sum(1/2 * gamma_cash * (pnl_sell + pnl_buy + pnl_none + pnl_both)))
}


move <- c(0.03, 0.03)
calc_payoff_const_gamma_cash(quotes_line, both_first = "ind")
calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "sell")
calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "buy")
calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "min")
calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "max")


optimize_payoff_to_move  <- function(move, quotes_line, both_first) {
  calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = both_first)
}

optim(c(0.03, 0.03), optimize_payoff_to_move, quotes_line = quotes_line, both_first = "sell", control = list(fnscale = -1))


