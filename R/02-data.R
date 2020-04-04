#' Keep data only when unbroken. We consider a timeseries as broken if one of the values is 0 or below or NA.
#'
#' @param quotes_ticker A sorted data.frame with at least the columns defined in cols of numeric type
#' @param cols The column names to check
#'
#' @return
#' @export
#'
#' @examples
#' quotes_ticker <- tibble(Low = c(1,2,NA,1,2), High = c(3,3,3,0,3), Adjusted = c(2,2,2,2,2))
#' filter_unbroken_history(quotes_ticker)
filter_unbroken_history <- function(quotes_ticker, cols = c("Low", "High", "Adjusted")) {

  bod_col_bool <- map(quotes_ticker[, cols], ~ .<=0 | is.na(.))
  bad_bool <- Reduce(`|`, bod_col_bool)
  
  if (sum(bad_bool) > 0) {
    first_good_idx <- max(which(bad_bool > 0)) + 1
    if (first_good_idx > nrow(quotes_ticker)) return(data.frame())
    quotes_ticker <- quotes_ticker[seq.int(first_good_idx, nrow(quotes_ticker)), ]
  }
  return(quotes_ticker)
}


#' Keep data only from where at least one of the columns are always changing from day to day in absolute value.
#'
#' @param quotes_ticker A sorted data.frame with at least the columns defined in cols of numeric type
#' @param cols The column names to check 
#'
#' @return The filtered data.frame
#' @export 
#' 
#' @examples
#' quotes_ticker <- tibble(Low = c(1,2,1,1,2), High = c(3,3,3,3,3), Adjusted = c(2,2,2,2,2))
#' filter_daily_changes(quotes_ticker)
filter_daily_changes <- function(quotes_ticker, cols = c("Low", "High", "Adjusted")) {
  if (nrow(quotes_ticker) < 2) return(quotes_ticker)
  bad_bool <- diff(as.matrix(quotes_ticker[, cols])) %>% abs() %>% rowSums() == 0
  if (sum(bad_bool) > 0) {
    first_good_idx <- max(which(bad_bool)) + 2
    if (first_good_idx > nrow(quotes_ticker)) return(data.frame())
    quotes_ticker <- quotes_ticker[seq.int(first_good_idx, nrow(quotes_ticker)), ]
  }
  return(quotes_ticker)
}

#' Filter out data with tickers that have a negative adjusted value somewhere in the data. Since the adjustment propagates through the data over time such 
#' tickers are removed completely.
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
#' filter_wrong_corporate_actions(quotes_ticker)
filter_wrong_corporate_actions <- function(quotes_ticker) {
  ticker_with_neg_adj <- quotes_ticker %>% filter(Adjusted < 0) %>% .[["Ticker"]] %>% unique()
  filter(quotes_ticker, !(Ticker %in% ticker_with_neg_adj))
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
#' filter_intraday_changes(quotes_ticker)
filter_intraday_changes <- function(quotes_ticker, cols = c("Low", "High")) {
  abs_diff <- t(quotes_ticker[, cols]) %>% diff() %>% abs() %>% t() %>% rowSums()
  bad_bool <- abs_diff == 0
  if (sum(bad_bool) > 0) {
    first_good_idx <- max(which(bad_bool)) + 1
    if (first_good_idx > nrow(quotes_ticker)) return(data.frame())
    quotes_ticker <- quotes_ticker[seq.int(first_good_idx, nrow(quotes_ticker)), ]
  }
  return(filter(quotes_ticker, ))
}

#' Keep positive values only
#'
#' @param quotes_ticker A data.frame containing at least the columns spezified in cols
#' @param cols The cols that (cumulatively) need to be positive
#'
#' @return The filtered data.frame
#' @export
#'
#' @examples
#' quotes_ticker <- tibble(Low = c(1), Open = c(1), High = c(3), Close = c(3), Adjusted = -1)
#' filter_positive_values(quotes_ticker)
filter_positive_values <- function(quotes_ticker, cols = c("Open", "Low", "High", "Close", "Adjusted")) {
  filter_at(quotes_ticker, cols, ~. > 0)
}


#' Keep data only if the order Low <= Open | Close <= High is fulfilled
#'
#' @param quotes_ticker A data.frame with at least the column "Open", "Low", "High", "Close"
#'
#' @return The filtered data.frame
#' @export
#'
#' @examples
#' filter_unreasonable_measure_order(tibble(Open = 2, Low = 2, High = 3, Close = 3))
#' filter_unreasonable_measure_order(tibble(Open = 2, Low = 3, High = 6, Close = 4))
#' filter_unreasonable_measure_order(tibble(Open = 2, Low = 3, High = 6, Close = 7))
#' filter_unreasonable_measure_order(tibble(Open = 2, Low = 3, High = 1, Close = 7))
filter_unreasonable_measure_order <- function(quotes_ticker) {
  quotes_ticker %>%
    filter(Low <= pmin(Open, High, Close)) %>%
    filter(High >= pmax(Open, Low, Close))
}


#' Filter out entries with unreasonable rations for "Low", "High", "Close" compared to "Open". This removes obvious typos like 1.02 instead of 102 for one measure.
#'
#' @param quotes_ticker data.frame with at least the column "Open", "Low", "High", "Close"
#' @param max_ratio The maximum indiviudal ratio allowed between "Open", "Low", "High" to "Close" and vice versa.
#'
#' @return The filtered data.frame
#' @export
#'
#' @examples
#' quotes_ticker_1 <- tibble(Ticker = "AV.L", Date = as.Date("2019-08-09"), Open = 387, Low = 3.87, High = 388, Close = 383, Adjusted = 373, Volume = 22813950)
#' filter_typos(quotes_ticker_1)
#' 
#' quotes_ticker_2 <- tibble(Ticker = "VOD.L", Date = as.Date("2019-09-16"), Open = 160, Low = 158, High = 1602, Close = 159, Adjusten = 155, Volume = 203315444)
#' filter_typos(quotes_ticker_2)
filter_typos <- function(quotes_ticker, max_ratio = 8) {
  quotes_ticker %>% 
    filter(Open / Low <= max_ratio & Low / Open <= max_ratio) %>%
    filter(Open / High <= max_ratio & High / Open <= max_ratio) %>%
    filter(Open / Close <= max_ratio & High / Close <= max_ratio)
}


#' Move Close Adjusted of previous day to same line
#'
#' @param quotes_ticker A data.frame with at least the column "Adjusted"
#'
#' @return A data.frame where previous entry of adjusted is added on line as "Adjusted_t_1" and "Adjusted" is renamed to "Adjusted_t". The first entry is 
#' removed, since no previous adjusted price is available.
#' @export
#'
#' @examples
#' reorganize_to_one_line(tibble(Adjusted = 1:3))
reorganize_to_one_line <- function(quotes_ticker) {
  slice(quotes_ticker, -1) %>% mutate(`Adjusted_t_1` = head(quotes_ticker$Adjusted, -1)) %>% rename(Adjusted_t = Adjusted)
}


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


