
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
