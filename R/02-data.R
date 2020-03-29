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


#' Move Close Adjusted of previous day to same line
#'
#' @param quotes_ticker A data.frame with at least the column "Adjusted"
#'
#' @return A data.frame where previous entry of adjusted is added on line as "Adjusted_t-1" and "Adjusted" is renamed to "Adjusted_t". The first entry is 
#' removed, since no previous adjusted price is available.
#' @export
#'
#' @examples
#' reorganize_to_one_line(tibble(Adjusted = 1:3))
reorganize_to_one_line <- function(quotes_ticker) {
  slice(quotes_ticker, -1) %>% mutate(`Adjusted_t-1` = head(quotes_ticker$Adjusted, -1)) %>% rename(Adjusted_t = Adjusted)
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


# test <- quotes %>% 
#   adjust_quotes() %>%
#   group_by(Ticker) %>%
#   dplyr::group_modify(~filter_unbroken_history(.)) %>%
#   dplyr::group_modify(~filter_daily_changes(.)) %>%
#   dplyr::group_modify(~filter_intraday_changes(.)) %>%
#   dplyr::group_modify(~reorganize_to_one_line(.)) %>%
#   ungroup()

