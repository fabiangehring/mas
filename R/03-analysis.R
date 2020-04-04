#' #' Calculate the payoff from prices moves
#' #'
#' #' @param quotes_line A data.frame with at least the following columns: "Adjusted_t_1", "Low", "High", "Adjusted_t"
#' #' @param sell The upper price at which the the delta hedge should be "rebalanced"
#' #' @param buy The lower price at which the the delta hedge should be "rebalanced"
#' #' @param gamma_cash The value used for gamma cash
#' #' @param both_first Character indicating what price movement is assumed first in cases where both, the buy and sell prices are reaches. One of 
#' #' c("ind", "buy", "sell", "min", "max"). "ind" is used to take into account p- and down moves independently. Economically this is equal to a situation
#' #' where hedges are not rebalanced at "buy" or "sell" levels but also at the  Adjusted_t-1. "buy" indicates that the buy event was reached first. "sell" 
#' #' indicates that the sell event was reached first. "min" conservativly considers the move first that leaves to the least pnl. "max" uses the event first
#' #' that least to highest pnl.
#' #'
#' #' @return The pnl
#' #' @export
#' #'
#' #' @examples
#' #' quotes_line <- tribble(
#' #'    ~Adjusted_t_1, ~Low, ~High, ~Adjusted_t,
#' #'                 10,     8,    12,           9
#' #'  )
#' #' move <- c(0.03, 0.03)
#' #' calc_payoff_const_gamma_cash(quotes_line, both_first = "ind")
#' #' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "sell")
#' #' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "buy")
#' #' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "min")
#' #' calc_payoff_const_gamma_cash(quotes_line, buy = quotes_line$`Adjusted_t-1` * (1 - move[1]), sell = quotes_line$`Adjusted_t-1` * (1 + move[2]), both_first = "max")
#' calc_payoff_const_gamma_cash <- function(quotes_line, buy = NULL, sell = NULL, gamma_cash = 1,  both_first = "ind") {
#'   
#'   stopifnot(is.null(sell) || is.null(buy) || all(buy<=sell))
#'   
#'   I_sell <- if (!is.null(sell)) quotes_line$High >= sell else rep(FALSE, nrow(quotes_line))
#'   I_buy <- if (!is.null(buy))  quotes_line$Low <= buy else rep(FALSE, nrow(quotes_line))
#'   
#'   # sell only
#'   I_sell_only <- (I_sell == TRUE) * (I_buy == FALSE)
#'   sell_part_1 <- log(sell[I_sell_only] / quotes_line$`Adjusted_t-1`[I_sell_only])^2
#'   sell_part_2 <- log(quotes_line$Adjusted_t[I_sell_only] / sell[I_sell_only])^2
#'   pnl_sell <- sum(sell_part_1, sell_part_2)
#'   
#'   # buy only
#'   I_buy_only <- (I_sell == FALSE) * (I_buy == TRUE)
#'   buy_part_1 <- log(buy[I_buy_only] / quotes_line$`Adjusted_t-1`[I_buy_only])^2
#'   buy_part_2 <- log(quotes_line$Adjusted_t[I_buy_only] / buy[I_buy_only])^2
#'   pnl_buy <- sum(buy_part_1, buy_part_2)
#'   
#'   # none
#'   I_none <- (I_sell == FALSE) * (I_buy == FALSE)
#'   none <- log(quotes_line$`Adjusted_t`[I_none] / quotes_line$`Adjusted_t-1`[I_none])^2
#'   pnl_none <- sum(none)
#'   
#'   # both
#'   I_both <- (I_sell == TRUE) * (I_buy == TRUE)
#'   if (both_first == "ind") {
#'     both_sell_part_1 <- log(sell[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
#'     both_sell_part_2 <- log(quotes_line$Adjusted_t[I_both] / sell[I_both])^2
#'     both_buy_part_1 <- log(buy[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
#'     both_buy_part_2 <- log(quotes_line$Adjusted_t[I_both] / buy[I_both])^2
#'     pnl_both <- sum(both_sell_part_1, both_sell_part_2, both_buy_part_1, both_buy_part_2)
#'   } else if (both_first %in% c("buy", "sell")) {
#'     
#'     if (both_first == "buy") {
#'       first_price <- buy
#'       second_price <- sell
#'     } else {
#'       first_price <- sell
#'       second_price <- buy
#'     }
#'     
#'     both_part_1 <- log(first_price[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
#'     both_part_2 <- log(second_price[I_both] / first_price[I_both])^2
#'     both_part_3 <- log(quotes_line$`Adjusted_t`[I_both] / second_price[I_both])^2
#'     pnl_both <- sum(both_part_1, both_part_2, both_part_3)
#'     
#'   } else if (both_first %in% c("min", "max")) {
#'     
#'     both_sell_first_part_1 <- log(sell[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
#'     both_sell_first_part_2 <- log(buy[I_both] / sell[I_both])^2
#'     both_sell_first_part_3 <- log(quotes_line$`Adjusted_t`[I_both] / buy[I_both])^2
#'     both_sell_first <- both_sell_first_part_1 + both_sell_first_part_2 + both_sell_first_part_3
#'     
#'     both_buy_first_part_1 <- log(buy[I_both] / quotes_line$`Adjusted_t-1`[I_both])^2
#'     both_buy_first_part_2 <- log(sell[I_both] / buy[I_both])^2
#'     both_buy_first_part_3 <- log(quotes_line$`Adjusted_t`[I_both] / sell[I_both])^2
#'     both_buy_first <- both_buy_first_part_1 + both_buy_first_part_2 + both_buy_first_part_3
#'     
#'     if (both_first == "min") {
#'       pnl_both <- sum(pmin(both_sell_first, both_buy_first))
#'     } else {
#'       pnl_both <- sum(pmax(both_sell_first, both_buy_first))
#'     }
#'   }
#'   
#'   return(sum(1/2 * gamma_cash * (pnl_sell + pnl_buy + pnl_none + pnl_both)))
#' }


#' Calulate Gamma Cash
#'
#' @param price The price of the underlying
#' @param gamma The options Gamma
#'
#' @return
#' @export
#'
#' @examples
#' gamma_cash(13, 0.2)
gamma_cash <- function(price, gamma) {
  gamma * price^2/100
}


#' Calculate the payoff from prices moves
#'
#' @param quotes_line A data.frame with at least the following columns: "Adjusted_t_1", "Low", "High", "Adjusted_t"
#' @param sell The upper price at which the the delta hedge should be "rebalanced"
#' @param buy The lower price at which the the delta hedge should be "rebalanced"
#' @param gamma The gamma value (assumed constant)
#' @param both_first Character indicating what price movement is assumed first in cases where both, the buy and sell prices are reaches. One of 
#' c("buy", "sell", "min", "max"). "buy" indicates that the buy event was reached first. "sell" indicates that the sell event was reached first.
#' "min" conservativly considers the move first that leaves to the least pnl. "max" uses the event first that least to highest pnl.
#'
#' @return The pnl for every row
#' @export
#'
#' @examples
#' quotes_line_test_1 <- tribble(
#'    ~Adjusted_t_1, ~Low, ~High, ~Adjusted_t,
#'                 10,     8,    12,           9
#'  )
#' calc_payoff_const_gamma(quotes_line_test_1)
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High)
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "buy")
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "sell")
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "max")
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "min")
#' 
#' quotes_line_test_2 <- tribble(
#'    ~Adjusted_t_1, ~Low, ~High, ~Adjusted_t,
#'                 9,     8,    12,           10
#'  )
#' calc_payoff_const_gamma(quotes_line_test_2)
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "buy")
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "sell")
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "max")
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "min")
calc_payoff_const_gamma <- function(quotes_line, buy = quotes_line$Adjusted_t, sell = quotes_line$Adjusted_t, gamma = 0.2, both_first = "buy", return = "disc") {

  stopifnot(all(buy <= sell))
  
  low <- quotes_line$Low
  high <- quotes_line$High
  close_t_1 <- quotes_line$Adjusted_t_1
  close_t <- quotes_line$Adjusted_t

  calc_payoff_per_title <- function(first, second, low, high) {
    
    buy_first_sell_second <- first < second
    sell_first_buy_second <- !buy_first_sell_second
    
    n <- nrow(quotes_line)
    
    I_first <- as.logical(rep(NA, n))
    I_first[buy_first_sell_second] <- (low[buy_first_sell_second] <= first[buy_first_sell_second])
    I_first[sell_first_buy_second] <- (high[sell_first_buy_second] >= first[sell_first_buy_second])
    
    I_second <- as.logical(rep(NA, n))
    I_second[buy_first_sell_second] <- (high[buy_first_sell_second] >= second[buy_first_sell_second])
    I_second[sell_first_buy_second] <- (low[sell_first_buy_second] <= second[sell_first_buy_second])
    
    # calc returns
    if (return == "cont") {
      r_first_close_t_1 <- log(first/close_t_1)
      r_first_close_t <- log(first/close_t)
      r_second_close_t_1 <- log(second/close_t_1)
      r_second_first <- log(second/first)
      r_close_t_second <- log(close_t/second)
      r_close_t_close_t_1 <- log(close_t/close_t_1)
    } else if (return == "disc") {
      r_first_close_t_1 <- first/close_t_1-1
      r_first_close_t <- first/close_t-1
      r_second_close_t_1 <- second/close_t_1-1
      r_second_first <- second/first-1
      r_close_t_second <- close_t/second-1
      r_close_t_close_t_1 <- close_t/close_t_1-1
    }
    
    # calc gains
    start_to_first <- gamma_cash(close_t_1, gamma) * r_first_close_t_1 ^ 2 * I_first * (1 - I_second)
    first_to_end <- gamma_cash(close_t, gamma) * r_first_close_t ^2 * I_first * (1 - I_second)
    
    start_to_second <- gamma_cash(close_t_1, gamma) * r_second_close_t_1 ^2 * I_second * (1 - I_first)
    second_to_end <- gamma_cash(second, gamma) * r_close_t_second ^2 * I_second * (1 - I_first)
    
    start_to_first_both <- gamma_cash(close_t_1, gamma) * r_first_close_t_1 ^2 * I_first * I_second
    first_to_second <- gamma_cash(first, gamma) * r_second_first^2 * I_second * I_first
    second_to_end_both <- gamma_cash(second, gamma) * r_close_t_second^2 * I_second * I_first
    
    start_to_end <- gamma_cash(close_t_1, gamma) * r_close_t_close_t_1^2 * (1 - I_second) * (1 - I_first)
    
    0.5 *100 * (start_to_first + first_to_end + start_to_second + second_to_end + start_to_first_both + first_to_second + second_to_end_both + start_to_end)
  }
  
  # evaluate first and second
  if (both_first %in% "buy") {
    first <- buy
    second <- sell
  } else if (both_first == "sell") {
    first <- sell
    second <- buy
  } else if (both_first %in% c("min", "max")) {
    buy_first <- calc_payoff_per_title(buy, sell, low, high)
    sell_first <- calc_payoff_per_title(sell, buy, low, high)
    
    first <- buy
    second <- sell
    if (both_first == "min") {
      first[sell_first < buy_first] <- sell[sell_first < buy_first]
      second[sell_first < buy_first] <- buy[sell_first < buy_first]
    } else  if (both_first == "max") {
      first[sell_first > buy_first] <- sell[sell_first > buy_first]
      second[sell_first > buy_first] <- buy[sell_first > buy_first]
    }
  }
  
  calc_payoff_per_title(first, second, low, high)
}


