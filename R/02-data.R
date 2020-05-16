
# TODO: Needed anymore?
#' #' Keep data only when unbroken. We consider a timeseries as broken if one of the values is 0 or below or NA.
#' #'
#' #' @param quotes_ticker A sorted data.frame with at least the columns defined in cols of numeric type
#' #' @param cols The column names to check
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' quotes_ticker <- tibble(Low = c(1,2,NA,1,2), High = c(3,3,3,0,3), Adjusted = c(2,2,2,2,2))
#' #' filter_unbroken_history(quotes_ticker)
#' filter_unbroken_history <- function(quotes_ticker, cols = c("Low", "High", "Adjusted")) {
#'   
#'   bod_col_bool <- map(quotes_ticker[, cols], ~ .<=0 | is.na(.))
#'   bad_bool <- Reduce(`|`, bod_col_bool)
#'   
#'   if (sum(bad_bool) > 0) {
#'     first_good_idx <- max(which(bad_bool > 0)) + 1
#'     if (first_good_idx > nrow(quotes_ticker)) return(data.frame())
#'     quotes_ticker <- quotes_ticker[seq.int(first_good_idx, nrow(quotes_ticker)), ]
#'   }
#'   return(quotes_ticker)
#' }


#' Set values where not at least one of the columns are always changing from day to day in absolute value to NA.
#'
#' @param quotes_ticker A sorted data.frame with at least the columns defined in cols of numeric type
#' @param cols The column names to check 
#'
#' @return The filtered data.frame
#' @export 
#' 
#' @examples
#' quotes_ticker <- tibble(Date = Sys.Date() + 1:5, Low = c(1,2,1,1,2), High = c(3,3,3,3,3), Adjusted = c(2,2,2,2,2))
#' na_no_daily_changes(quotes_ticker)
na_no_daily_changes <- function(quotes_ticker, cols = c("Open", "Low", "High", "Adjusted")) {
  if (nrow(quotes_ticker) < 2) return(quotes_ticker)
  bad_bool <- c(FALSE, diff(as.matrix(quotes_ticker[, cols])) %>% abs() %>% rowSums() == 0)
  mutate_at(quotes_ticker, cols, ~ifelse(bad_bool, NA, .))
}


#' Set Adjusted values to NA, where the huge daily return must considered as data error
#'
#' @param quotes_ticker A data.frame with at least the column "Adjusted"
#' @param max_daily_return The maximum accepted (adjusted) daily return
#'
#' @return
#' @export
#' A date.frame where unreasonable values for adjusted are replaced by NA
#'
#' @examples
#' na_too_large_daily_changes(tibble(Adjusted = c(1, 100, 1)), 2)
na_too_large_daily_changes <- function(quotes_ticker, max_ratio) {
  too_large_daily_changes_up_bool <- c(FALSE, tidyr::replace_na(tail(quotes_ticker$Adjusted, -1) / head(quotes_ticker$Adjusted, -1) > max_ratio, FALSE))
  too_large_daily_changes_down_bool <- c(FALSE, tidyr::replace_na(head(quotes_ticker$Adjusted, -1) / tail(quotes_ticker$Adjusted, -1) > max_ratio, FALSE))
  
  quotes_ticker$Adjusted[too_large_daily_changes_up_bool | too_large_daily_changes_down_bool] <- NA
  quotes_ticker
}


#' Filter out data with tickers that have an adjusted value of very close to 0 (0.001) or below somewhere in the data. Since the adjustment propagates through the data over time such 
#' tickers are set to na completely.
#' 
#' Notice that this filter should be used early in the data cleanup process since otherwise it might not be possible to detect the negative adjusted prices
#' anymoire (when excluded with other filters).
#'
#' @param quotes_ticker A data.frame with at least the columns "Ticker" and "Adjusted"
#'
#' @return The filtered data.frame
#' @export
#'
#' @examples
#' quotes_ticker <- tibble(Ticker = "SIKA.SW", Date = as.Date("2005-12-06"), Open = 17.5, Low = 17.4, High = 17.7, Clos = 17.7, Adjusted = -0.000001, Volume = 528480)
#' na_wrong_corporate_actions(quotes_ticker)
na_wrong_corporate_actions <- function(quotes_ticker, threshold) {
  ticker_with_neg_adj <- quotes_ticker %>% filter(Adjusted < threshold) %>% .[["Ticker"]] %>% unique()
  quotes_ticker$Adjusted[quotes_ticker$Ticker %in% ticker_with_neg_adj] <- NA
  quotes_ticker
}


#' Keep data only from where columns differ. (i.e. "Low" and "High", to guarantee that intraday moves are recorded)
#'
#' @param quotes_ticker A sorted data.frame with at least the columns defined in cols of numeric type
#'
#' @return The filtered data.frame
#' @export 
#' 
#' @examples
#' quotes_ticker <- tibble(Low = c(1,2,3,1,2), High = c(3,3,3,3,3))
#' na_no_intraday_moves(quotes_ticker)
na_no_intraday_moves <- function(quotes_ticker, cols = c("Low", "High")) {
  abs_diff <- t(quotes_ticker[, cols]) %>% diff() %>% abs() %>% t() %>% rowSums()
  bad_bool <- abs_diff == 0
  mutate_at(quotes_ticker, cols, ~ifelse(bad_bool, NA, .))
}

# TODO: Needed anymore?
#' #' Set negative values to NA
#' #'
#' #' @param quotes_ticker A data.frame containing at least the columns spezified in cols
#' #' @param cols The cols that need to be positive
#' #'
#' #' @return The filtered data.frame
#' #' @export
#' #'
#' #' @examples
#' #' quotes_ticker <- tibble(Low = c(1), Open = c(1), High = c(3), Close = c(3), Adjusted = -1)
#' #' na_negative_values(quotes_ticker)
#' na_negative_values <- function(quotes_ticker, cols = c("Open", "Low", "High", "Close", "Adjusted")) {
#'   mutate_at(quotes_ticker, cols, ~ ifelse(. < 0, NA, .))
#' }


#' Keep data only if the order Low <= Open | Close <= High is fulfilled
#'
#' @param quotes_ticker A data.frame with at least the column "Open", "Low", "High", "Close"
#'
#' @return The filtered data.frame
#' @export
#'
#' @examples
#' na_unreasonable_measure_order(tibble(Open = 2, Low = 2, High = 3, Close = 3))
#' na_unreasonable_measure_order(tibble(Open = 2, Low = 3, High = 6, Close = 4))
#' na_unreasonable_measure_order(tibble(Open = 2, Low = 3, High = 6, Close = 7))
#' na_unreasonable_measure_order(tibble(Open = 2, Low = 3, High = 1, Close = 7))
na_unreasonable_measure_order <- function(quotes_ticker) {
  
  # Check low
  unreasonable_low_open <- tidyr::replace_na(quotes_ticker$Low > quotes_ticker$Open, FALSE)
  quotes_ticker$Low[unreasonable_low_open] <- NA
  
  unreasonable_low_high <- tidyr::replace_na(quotes_ticker$Low > quotes_ticker$High, FALSE)
  quotes_ticker$Low[unreasonable_low_high] <- NA
  
  unreasonable_low_close <- tidyr::replace_na(quotes_ticker$Low > quotes_ticker$Close, FALSE)
  quotes_ticker$Low[unreasonable_low_close] <- NA
  
  # Check high
  unreasonable_high_open <- tidyr::replace_na(quotes_ticker$High < quotes_ticker$Open, FALSE)
  quotes_ticker$High[unreasonable_high_open] <- NA                                                                                                            
  
  unreasonable_high_low <- tidyr::replace_na(quotes_ticker$High < quotes_ticker$Low, FALSE)
  quotes_ticker$High[unreasonable_high_low] <- NA 
  
  unreasonable_high_close <- tidyr::replace_na(quotes_ticker$High < quotes_ticker$Close, FALSE)
  quotes_ticker$High[unreasonable_high_close] <- NA 
  
  quotes_ticker
}


#' Set values with unreasonable rations for "Low", "High", "Close" compared to "Open" to NA. This removes obvious typos like 1.02 instead of 102 for one 
#' measure.
#'
#' @param quotes_ticker data.frame with at least the column "Open", "Low", "High", "Close"
#' @param max_ratio The maximum indiviudal ratio allowed between "Open", "Low", "High" to "Close" and vice versa.
#'
#' @return The filtered data.frame
#' @export
#'
#' @examples
#' quotes_ticker_1 <- tibble(Ticker = "AV.L", Date = as.Date("2019-08-09"), Open = 387, Low = 3.87, High = 388, Close = 383, Adjusted = 373, Volume = 22813950)
#' na_typos(quotes_ticker_1)
#' 
#' quotes_ticker_2 <- tibble(Ticker = "VOD.L", Date = as.Date("2019-09-16"), Open = 160, Low = 158, High = 1602, Close = 159, Adjusted = 155, Volume = 203315444)
#' na_typos(quotes_ticker_2)
na_typos <- function(quotes_ticker, max_ratio) {
  
  unreasonable_open_low <- tidyr::replace_na(quotes_ticker$Open / quotes_ticker$Low > max_ratio, FALSE)
  unreasonable_low_open <- tidyr::replace_na(quotes_ticker$Low / quotes_ticker$Open > max_ratio, FALSE)
  
  unreasonable_open_high <- tidyr::replace_na(quotes_ticker$Open / quotes_ticker$High > max_ratio, FALSE)
  unreasonable_high_open <- tidyr::replace_na(quotes_ticker$High / quotes_ticker$Open > max_ratio, FALSE)
  
  unreasonable_open_close <- tidyr::replace_na(quotes_ticker$Open / quotes_ticker$Close > max_ratio, FALSE)
  unreasonable_close_open <- tidyr::replace_na(quotes_ticker$Close / quotes_ticker$Open > max_ratio, FALSE)
  
  
  unreasonable_low_high <- tidyr::replace_na(quotes_ticker$Low / quotes_ticker$High > max_ratio, FALSE)
  unreasonable_high_low <- tidyr::replace_na(quotes_ticker$High / quotes_ticker$Low > max_ratio, FALSE)
  
  unreasonable_low_close <- tidyr::replace_na(quotes_ticker$Low / quotes_ticker$Close > max_ratio, FALSE)
  unreasonable_close_low <- tidyr::replace_na(quotes_ticker$Close / quotes_ticker$Low > max_ratio, FALSE)
  
  
  unreasonable_high_close <- tidyr::replace_na(quotes_ticker$High / quotes_ticker$Close > max_ratio, FALSE)
  unreasonable_close_high <- tidyr::replace_na(quotes_ticker$Close / quotes_ticker$High > max_ratio, FALSE)
  
  # open
  unreasonable_open <- (unreasonable_open_low + unreasonable_low_open + unreasonable_open_high + unreasonable_high_open + unreasonable_open_close + unreasonable_close_open) > 1
  quotes_ticker$Open[unreasonable_open] <- NA
  
  unreasonable_low <- (unreasonable_open_low + unreasonable_low_open + unreasonable_low_high + unreasonable_high_low + unreasonable_low_close + unreasonable_close_low) > 1
  quotes_ticker$Low[unreasonable_low] <- NA
  
  unreasonable_high <- (unreasonable_open_high + unreasonable_high_open + unreasonable_low_high + unreasonable_high_low + unreasonable_high_close + unreasonable_close_high) > 1
  quotes_ticker$High[unreasonable_high] <- NA
  
  unreasonable_close <- (unreasonable_open_close + unreasonable_close_open + unreasonable_open_close + unreasonable_close_open + unreasonable_high_close + unreasonable_close_high) > 1
  quotes_ticker$Close[unreasonable_close] <- NA
  
  quotes_ticker
}


#' Set entries to NA where values are in the tails for one of the respective column.
#' 
#' @param data A data.frame with at least the columns defined in col
#' @param col The column names that need to be processed
#' @param tail_prc The proportion of values to filter out on both ends. i.e. a value of 0.01 will set the 1% smallest and the 1% largest values to NA
#'
#' @return The processed data.frame
#' @export
#'
#' @examples
#' summary(na_extremes(quotes_line, col = c("Open", "Low", "High", "Close", "Adjusted_t")))
na_extremes <- function(data, col, tail) {
  for (curr_col in col) {
    extremes <- quantile(data[[curr_col]], na.rm = TRUE, probs = c(tail, 1 - tail))
    extreme_lower_bool <- tidyr::replace_na(data[[curr_col]] < extremes[1], FALSE)
    extreme_upper_bool <- tidyr::replace_na(data[[curr_col]] > extremes[2], FALSE)
    data[[curr_col]][extreme_lower_bool | extreme_upper_bool] <- NA
  }  
  data
}


# TODO: Remove, use widen instead
#' #' Move Close Adjusted of previous day to same line
#' #'
#' #' @param quotes_ticker A data.frame with at least the column "Adjusted"
#' #'
#' #' @return A data.frame where previous entry of adjusted is added on line as "Adjusted_t_1" and "Adjusted" is renamed to "Adjusted_t". The first entry is 
#' #' removed, since no previous adjusted price is available.
#' #' @export
#' #'
#' #' @examples
#' reorganize_to_one_line <- function(quotes_ticker) {
#'   slice(quotes_ticker, -1) %>% mutate(`Adjusted_t_1` = head(quotes_ticker$Adjusted, -1)) %>% rename(Adjusted_t = Adjusted)
#' }


#' Apply adjustment factor to all price and volume information
#'
#' @param quotes A data.frame with at least the columns Open, Low, High, Close, Adjusted und Volume
#'
#' @return
#' The same dateframe with adjusted columns for the mentioned columns above.
#' @export
#'
#' @examples
#' quotes <- tibble(Open = 2, Low = 1, High = 3, Close = 2, Adjusted = 4)
#' adjust_prices(quotes)
adjust_quotes <- function(quotes) {
  adj_factor <- quotes$Adjusted / quotes$Close 
  quotes %>% mutate_at(c("Open", "Low", "High", "Close", "Volume"), ~ .*adj_factor)
}


#' Normalize values of a dataframe rowwise to 100
#'
#' @param quotes A data.frame containing at least all columns mentoned in "base_col" and "target_cols"
#' @param base_col The name of the column to normalize to 100
#' @param target_cols The names of the columns to normalize
#'
#' @return The normalized data.frame
#' @export
#'
#' @examples
#' normalize_quotes(tibble(a = c(50, 150), b = c(80, 200), c = c(50, 200)), base_col = "a", target_cols = c("b"))
normalize_quotes <- function(quotes, base_col = "Open", target_cols = c("Low", "High", "Close", "Adjusted")) {
  index_factor <- 100 / quotes[[base_col]] 
  mutate_at(quotes, unique(c(base_col, target_cols)), ~.*index_factor)
}


