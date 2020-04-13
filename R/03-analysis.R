
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
#' @param both_first Character or numeric indicating what price movement is assumed first in cases where both, the buy and sell prices are reaches. One of 
#' c("buy", "sell", "min", "max") or a integer value. "buy" indicates that the buy event was reached first. "sell" indicates that the sell event was reached first.
#' "min" conservativly considers the move first that leaves to the least pnl. "max" uses the event first that least to highest pnl. If an numeric is given
#' a random number (with seed of that number) is used to evaluate if there was a buy or sell event first with equal probability of 50%.
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
calc_payoff_const_gamma <- function(quotes_line, buy = quotes_line$Adjusted_t, sell = quotes_line$Adjusted_t, gamma = 0.2, both_first = 123456, return = "disc") {
  
  stopifnot(all(buy <= sell))
  
  low <- quotes_line$Low
  high <- quotes_line$High
  close_t_1 <- quotes_line$Adjusted_t_1
  close_t <- quotes_line$Adjusted_t
  
  calc_payoff_per_title <- function(first, second, low, high) {
    
    buy_first <- sell_second <- first < second
    sell_first <- buy_second <- !buy_first
    
    n <- nrow(quotes_line)
    
    # transaction is done if 
    #   a) prices is hit and 
    #   b) price is coming from "correct" side (from a lower prices to sell, from an upper prices to buy)
    first_prev_price <- quotes_line$Adjusted_t_1
    I_first <- as.logical(rep(FALSE, n))
    I_first[buy_first & first >= low & first_prev_price > first] <- TRUE
    I_first[sell_first & first <= high & first_prev_price < first] <- TRUE

    second_prev_price <- ifelse(I_first, first, quotes_line$Adjusted_t_1)
    I_second <- as.logical(rep(FALSE, n))
    I_second[buy_second & second >= low & second_prev_price > second] <- TRUE
    I_second[sell_second & second <= high & second_prev_price < second] <- TRUE
    
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
  if (is.numeric(both_first))  {
    old <- .Random.seed
    set.seed(both_first)
    buy_first_bool <- sample(c(TRUE, FALSE), nrow(quotes_line), TRUE)
    .Random.seed <<- old
    first <- ifelse(buy_first_bool, buy, 0) + ifelse(!buy_first_bool, sell, 0)
    second <- ifelse(!buy_first_bool, buy, 0) + ifelse(buy_first_bool, sell, 0)
  } else if (both_first %in% "buy") {
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
  } else {
    stop("Expected input for argument both_first.")
  }
  
  calc_payoff_per_title(first, second, low, high)
}


#' Get history of current element
#'
#' @param data A sorted data.frame
#' @param window An integer indicating the length of the history
#' @param col The column names of the values for which the history should be widened
#'
#' @return A data.frame of same length and order like data with the history for each row for the requestes columns. The number at the end of the column tells
#' their order. 1 is the oldest.
#' @export
#'
#' @examples
#' data <- tibble(a = 1:10000, b = 1:10000)
#' test <- widen(quotes_line, 7, "Low")
widen <- function(data, window, col = names(data)) {
  
  # fill first rows such that even first row has predecessor
  data_fill <- data[seq_len(window), ] 
  data_fill[seq_len(dim(data_fill)[1]), seq_len(dim(data_fill)[2])] <- NA
  data <- bind_rows(data_fill, data)
  
  # widen data
  row_idx <- unlist(map(seq_len(nrow(data) - window), ~seq_len(window) + . - 1))
  values_fn <- map(col, ~list) %>% set_names(col)
  data_widened <- data[row_idx, col] %>%
    mutate(group_id = rep(seq_len(window), length(row_idx) / window)) %>%
    tidyr::pivot_wider(
      names_from = group_id, 
      values_from = all_of(col), 
      values_fn = values_fn
    ) %>% 
    unnest(cols = names(.)) 
  
  # name columns in case of single feature
  if (length(col) == 1) names(data_widened) <- paste0(col, "_", names(data_widened))
  
  data_widened
}


#' Split data into a query part (the values to search for) and the data part (where to find the data) for a given date.
#'
#' @param curr_date The date of the split
#' @param all_dates The dates for teh rows of data_wide
#' @param data_wide A data.frame from where query and data should be extracted
#'
#' @return
#' @export
#' A list of the two data.frames "query" and "data"
#'
#' @examples
split_data_query <- function(curr_date, all_dates, data_wide) {
  query <- data_wide[all_dates %in% curr_date, ]
  data_wide <- data_wide[all_dates < max(curr_date), ]
  return(list(data_wide = data_wide, query = query))
}


#' Find quote histories in the past that ressembles the current the most
#'
#' @param quotes_line A data.frame with quote information
#' @param cols The colum names of the values that should be compared their historical counteroparts
#' @param window The size of the window that should be compared
#' @param k The number of closest neighbors that should be looked for
#' @param chunk_size The size of dates that should be looked at at once. A higher number is faster but if >1 it is not guaranteed that all neigbors found are
#' from the past. Dates with same date as query or later will be returned as NA.
#' @param treetype 
#'
#' @return
#' @export
#'
#' @examples
find_nn <- function(quotes_line, cols, window, k = 51, chunk_size = 20, norm = c("euclidean", "manhattan")) {
  
  data_wide <- quotes_line %>% 
    group_by(Ticker) %>% 
    dplyr::group_modify(~widen(data = ., window = window, cols = cols)) %>% 
    ungroup() %>%
    select(-Ticker)
  
  all_dates <- quotes_line$Date
  
  # remove NA values
  na_free_row_bool <- rowSums(is.na(data_wide)) == 0
  data_wide <- data_wide[na_free_row_bool, ]
  all_dates <- all_dates[na_free_row_bool]
  
  # order dates such that indizes are strictly increasing
  date_order <- order(all_dates)
  all_dates <- all_dates[date_order]
  data_wide <- data_wide[date_order, ]
  
  unique_dates <- sort(unique(all_dates))
  
  chunk_size <- chunk_size
  n_chunks <- ceiling(length(unique_dates) / chunk_size)
  date_chunks <- split(unique_dates, head(rep(seq_len(n_chunks), each = chunk_size), length(unique_dates)))
  
  #  choose correct nearest neighbor search distance
  if (norm == "euclidean") {
    nn_fct <- RANN::nn2(data_query$data_wide, query = data_query$query, min(k, nrow(data_query$data_wide)), treetype = "kd")
  } else if (norm == "manhattan") {
    nn_fct <- RANN.L1::nn2(data_query$data_wide, query = data_query$query, min(k, nrow(data_query$data_wide)), treetype = "kd")
  } else {
    stop("Unexpected value for function argument norm.")
  }
  
  knn_output <- pbmcapply::pbmclapply(date_chunks, function(curr_date_chunk) {
    data_query <- split_data_query(curr_date_chunk, all_dates, data_wide)
    
    out <- nn_fct(data_query$data_wide, query = data_query$query, min(k, nrow(data_query$data_wide)), treetype = "kd")
    
    if (nrow(data_query$data_wide) < k) {
      tmp <- matrix(rep(NA, k * nrow(data_query$query)), ncol = k)
      nn.idx <- tmp
      nn.idx[, seq_len(nrow(data_query$data_wide))] <- out[["nn.idx"]]
      nn.dists <- tmp
      nn.dists[, seq_len(nrow(data_query$data_wide))] <- out[["nn.dists"]]
      out <- list(nn.idx = nn.idx, nn.dists = nn.dists)
    }
    return(out)
  }, mc.cores = parallel::detectCores() - 1)
  
  nn <- list()
  nn[["nn.idx"]] <- purrr::map(knn_output,  ~.[["nn.idx"]]) %>% do.call(rbind, .)
  nn[["nn.dists"]] <- purrr::map(knn_output,  ~.[["nn.dists"]]) %>% do.call(rbind, .)
  
  # make sure all neighbors are from previous dates
  neighbor_date_minus_self_date <- matrix(all_dates[nn[["nn.idx"]]], ncol = k) - as.integer(all_dates)
  nn[["nn.idx"]][neighbor_date_minus_self_date >= 0] <- NA
  nn[["nn.dists"]][neighbor_date_minus_self_date >= 0] <- NA
  
  nn
}


# suppressPackageStartupMessages({
#   library(dplyr)
#   library(stringr)
#   library(purrr)
#   library(pbmcapply)
#   library(kknn)
#   library(tidyr)
# })
# nn <- find_nn(quotes_line, "Low", 5, chunk_size = 1)

y <- 12324156

