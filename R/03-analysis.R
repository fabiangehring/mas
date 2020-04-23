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
  
  print("hier")
  stopifnot(all(buy <= sell, na.rm = TRUE))
  
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
#' @param data A sorted data.frame sortest by Ticker and desc(Date)
#' @param window An integer indicating the length of the history
#' @param col The column names of the values for which the history should be widened
#' @param include_current A logical indicating if the current date (t = 0) should be included in the output

#' @return A data.frame of same length and order like data with the history for each row for the requestes columns. The number at the end of the column tells
#' their order and indicates the distance to the current data (i.e. *_7 meaning t-7).
#' @export
#'
#' @examples
#' data <- tibble(a = 1:100, b = 1:100)
#' widen(data, 7, c("a", "b"))
#' widen(data, 7, c("a", "b"), include_current = TRUE)
widen <- function(data, window, cols = names(data), include_current = FALSE) {
  
  # fill first rows such that even first row has predecessor
  data_fill <- data[seq_len(window), ] 
  data_fill[seq_len(dim(data_fill)[1]), seq_len(dim(data_fill)[2])] <- NA
  data <- bind_rows(data_fill, data)
  
  # widen data
  row_idx <- map(seq_len(nrow(data) - window), function(x) {
    curr_row_idx <- seq_len(window) + x - 1
    if (include_current) {
      curr_row_idx <- c(curr_row_idx, window + x)
    }
    curr_row_idx
  }) %>% unlist()
  
  values_fn <- map(cols, ~list) %>% set_names(cols)
  data_widened <- data[row_idx, cols] %>%
    mutate(group_id = rep(rev(seq_len(window + include_current) - include_current), length(row_idx) / (window + include_current))) %>%
    tidyr::pivot_wider(
      names_from = group_id, 
      values_from = all_of(cols), 
      values_fn = values_fn
    ) %>% 
    unnest(cols = names(.)) 
  
  # name columns in case of single feature
  if (length(cols) == 1) names(data_widened) <- paste0(cols, "_", names(data_widened))
  
  data_widened
}


#' Split data into a query part (the values to search for) and the data part (where to find the data) for a given date.
#'
#' @param curr_date The date of the split
#' @param all_dates The dates for the rows of data_wide
#' @param data_wide A data.frame from where query and data should be extracted
#'
#' @return
#' @export
#' A list of the two data.frames "query" and "data"
#'
#' @examples
split_data_query <- function(curr_date, all_dates, data_wide) {
  query <- data_wide[all_dates == curr_date, ]
  data_wide <- data_wide[all_dates < curr_date, ]
  return(list(data_wide = data_wide, query = query))
}


#' #' Find quote histories in the past that resembles the current the most
#' #'
#' #' @param quotes_line A data.frame with quote information
#' #' @param cols The colum names of the values that should be compared their historical counteroparts
#' #' @param window The size of the window that should be compared
#' #' @param k The number of closest neighbors that should be looked for
#' #' @param chunk_size The size of dates that should be looked at at once. A higher number is faster but if >1 it is not guaranteed that all neigbors found are
#' #' from the past. Dates with same date as query or later will be returned as NA.
#' #' @param One of "euclidean" or "manhattan".
#' #' @param treetype 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' data_wide_curr <- tibble(Open_2 = 94, Open_1 = 99, Open_0 = 104, Close_2 = 101, Close_1 = 102, Close_0 = 99)
#' #' data_wide_nn <- tibble(Open_2 = c(93, 103), Open_1 = c(99, 98), Open_0 = c(103, 98), Close_2 = c(102, 105), Close_1 = c(102, 103), Close_0 = c(98, 99))
#' find_nn <- function(quotes_line, cols, window, k = 50, chunk_size = 1, norm = "euclidean", mc.cores = parallel::detectCores() - 1) {
#'   
#'   data_wide <- quotes_line %>%
#'     group_by(Ticker) %>% 
#'     dplyr::group_modify(~widen(data = ., window = window, cols = cols)) %>% 
#'     ungroup() %>%
#'     select(-Ticker)
#'   
#'   all_dates <- quotes_line$Date
#'   
#'   # remove NA values
#'   na_free_row_bool <- rowSums(is.na(data_wide)) == 0
#'   data_wide <- data_wide[na_free_row_bool, ]
#'   all_dates <- all_dates[na_free_row_bool]
#'   
#'   
#'   # order dates such that indizes are strictly increasing
#'   date_order <- order(all_dates)
#'   all_dates <- all_dates[date_order]
#'   data_wide <- data_wide[date_order, ]
#'   
#'   unique_dates <- sort(unique(all_dates))
#'   
#'   chunk_size <- chunk_size
#'   n_chunks <- ceiling(length(unique_dates) / chunk_size)
#'   date_chunks <- split(unique_dates, head(rep(seq_len(n_chunks), each = chunk_size), length(unique_dates)))
#'   
#'   #  choose correct nearest neighbor search distance
#'   if (norm == "euclidean") {
#'     nn_fct <- RANN::nn2
#'   } else if (norm == "manhattan") {
#'     nn_fct <- RANN.L1::nn2
#'   } else {
#'     stop("Unexpected value for function argument norm.")
#'   }
#'   
#'   create_dummy_return <- function(width, height) {
#'     out <- list()
#'     tmp <- matrix(rep(NA, width * height), ncol = width)
#'     out$nn.idx <- tmp
#'     out$nn.dists <- tmp
#'     out
#'   }
#'   
#'   knn_output <- pbmcapply::pbmclapply(seq_along(date_chunks), function(i) {
#'     
#'     curr_date_chunk <- date_chunks[[i]]
#'     data_query <- split_data_query(curr_date_chunk, all_dates, data_wide)
#'     
#'     if (chunk_size == 1 && i == 1) {
#'       out <- create_dummy_return(k, nrow(data_query$query))
#'       return(out)
#'     }
#'     
#'     out <- nn_fct(data_query$data_wide, query = data_query$query, min(k, nrow(data_query$data_wide)), treetype = "kd")
#'     if (nrow(data_query$data_wide) < k) {
#'       tmp <- create_dummy_return(k, nrow(data_query$query))
#'       tmp$nn.idx[, seq_len(nrow(data_query$data_wide))] <- out[["nn.idx"]]
#'       tmp$nn.dists[, seq_len(nrow(data_query$data_wide))] <- out[["nn.dists"]]
#'       out <- tmp
#'     }
#'     return(out)
#'   }, mc.cores = mc.cores)
#'   
#'   nn <- list()
#'   nn[["nn.idx"]] <- purrr::map(knn_output,  ~.[["nn.idx"]]) %>% do.call(rbind, .)
#'   nn[["nn.dists"]] <- purrr::map(knn_output,  ~.[["nn.dists"]]) %>% do.call(rbind, .)
#'   
#'   # make sure all neighbors are from previous dates
#'   neighbor_date_minus_self_date <- matrix(all_dates[nn[["nn.idx"]]], ncol = k) - as.integer(all_dates)
#'   nn[["nn.idx"]][neighbor_date_minus_self_date >= 0] <- NA
#'   nn[["nn.dists"]][neighbor_date_minus_self_date >= 0] <- NA
#'   
#'   nn
#' }
#' 


#' Find quote histories in the past that resembles the current the most
#'
#' @param data_wide A data.frame with all columns that need to be used to find the nearest neighbors
#' @param dates The dates for each row of data_wide
#' @param k The number of closest neighbors that should be looked for
#' @param norm The distance measure to use, of "euclidean" or "manhattan".
#' @param mc.cores The number of cores to use for the calculation. 
#'
#' @return
#' @export
#'
#' @examples
#' dates <- as.Date(c("2020-01-03", "2020-01-02", "2020-01-02"))
#' data_wide <- tibble(
#'   Open_2 = c(93, 103, 104), 
#'   Open_1 = c(99, NA, 100), 
#'   Open_0 = c(103, 98, 100), 
#'   Close_2 = c(102, 105, 98), 
#'   Close_1 = c(102, 103, 101), 
#'   Close_0 = c(98, 99, 103)
#' )
#' find_nn(data_wide, dates)
find_nn <- function(data_wide, dates, k = 50, norm = "euclidean", mc.cores = parallel::detectCores() - 1) {
  
  n <- nrow(data_wide)
  
  # order dates such that indizes are strictly increasing
  date_order <- order(dates)
  dates_ordered <- dates[date_order]
  data_wide_ordered <- data_wide[date_order, ]
  rm(data_wide)
  
  
  #  choose correct nearest neighbor search distance
  if (norm == "euclidean") {
    nn_fct <- RANN::nn2
  } else if (norm == "manhattan") {
    nn_fct <- RANN.L1::nn2
  } else {
    stop("Unexpected value for function argument norm.")
  }
  
  create_dummy_return <- function(width, height) {
    out <- list()
    tmp <- matrix(rep(NA, width * height), ncol = width)
    out$nn.idx <- tmp
    out$nn.dists <- tmp
    out
  }
  
  na_row_bool <- rowSums(is.na(data_wide_ordered)) > 0
  data_wide_filtered <- data_wide_ordered[!na_row_bool, ]
  rm(data_wide_ordered)
  
  dates_filtered <- dates_ordered[!na_row_bool]
  rm(dates_ordered)
  
  unique_dates <- sort(unique(dates_filtered))
  knn_output <- pbmcapply::pbmclapply(seq_along(unique_dates), function(i) {
    curr_date <- unique_dates[i]
    data_query <- split_data_query(curr_date, dates_filtered, data_wide_filtered)
    rm(data_wide_filtered)
    
    if (i == 1) {
      out <- create_dummy_return(k, nrow(data_query$query))
      return(out)
    }
    
    out <- nn_fct(data_query$data_wide, query = data_query$query, min(k, nrow(data_query$data_wide)), treetype = "kd")
    
    if (nrow(data_query$data_wide) < k) {
      tmp <- create_dummy_return(k, nrow(data_query$query))
      tmp$nn.idx[, seq_len(nrow(data_query$data_wide))] <- out[["nn.idx"]]
      tmp$nn.dists[, seq_len(nrow(data_query$data_wide))] <- out[["nn.dists"]]
      out <- tmp
    }
    rm(data_query)
    return(out)
  }, mc.cores = mc.cores)
  
  nn <- list()
  
  # make sure also excluded rows are in the result (as NA rows)
  nn[["nn.idx"]] <- matrix(rep(NA_integer_, k * n), ncol = k)
  nn[["nn.idx"]][!na_row_bool, ] <- purrr::map(knn_output,  ~.[["nn.idx"]]) %>% do.call(rbind, .)
  
  # take into account in indices that there were excluded rows
  nn[["nn.idx"]] <- matrix(seq_len(n)[!na_row_bool][nn[["nn.idx"]]], ncol = k)
  
  nn[["nn.dists"]] <- matrix(rep(NA_real_, k * n), ncol = k)
  nn[["nn.dists"]][!na_row_bool, ] <- purrr::map(knn_output,  ~.[["nn.dists"]]) %>% do.call(rbind, .)
  
  # order result in the same way as inputed data
  nn[["nn.idx"]] <- nn[["nn.idx"]][order(date_order), ]
  nn[["nn.idx"]] <- matrix(date_order[nn[["nn.idx"]]], ncol = k)
  
  nn[["nn.dists"]] <- nn[["nn.dists"]][order(date_order), ]
  
  nn
}


#' Plot price movements for window and current time frame inkl. prediction
#'
#' @param data_wide_curr A data.frame with the history of quotes for the Ticker and Date of interest. Includes at least the columns given in cols followed by 
#' a window suffix (i.e "Close_3")
#' @param data_wide_nn A data.frame with the history of quotes for the nearest neighboars. Includes at least the columns given in cols followed by a window 
#' suffix (i.e "Close_3")
#' @param method Methodology to predict value, see nn_apply
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' data_wide_curr <- tibble(Open_2 = 94, Open_1 = 99, Open_0 = 104, Close_2 = 101, Close_1 = 102, Close_0 = 99)
#' data_wide_nn <- tibble(Open_2 = c(93, 103), Open_1 = c(99, 98), Open_0 = c(103, 98), Close_2 = c(102, 105), Close_1 = c(102, 103), Close_0 = c(98, 99))
#' plot_nn(data_wide_curr, data_wide_nn, title = "Some Test")
#' plot_nn(select(data_wide_curr, !ends_with("_0")), select(data_wide_nn, !ends_with("_0")))
plot_nn <- function(data_wide_curr, data_wide_nn, title = NULL, method = "mean") {  
  
  stopifnot(sort(names(data_wide_curr)) == sort(names(data_wide_nn)))
  col_types <- unique(stringr::str_extract(names(data_wide_curr), ".*(?=_[0-9]+$)"))
  
  # predict values
  data_wide_pred <- pred_nn(select(data_wide_nn, ends_with("_0")), method = method)
  
  plot_data <- mutate(data_wide_nn, src = "nn") %>%
    bind_rows(mutate(data_wide_curr, src = "curr")) %>%
    bind_rows(mutate(data_wide_pred, src = "pred")) %>%
    mutate(entry = seq_len(n())) %>%
    tidyr::pivot_longer(-c("entry", "src"), names_to = "type") %>%
    separate(type, sep = "_", into = c("type", "window"))
  
  # factor to ensure order
  window_string <- ifelse(plot_data$window == 0, "t", paste0("t-", plot_data$window))
  window_levels <- ifelse(sort(unique(plot_data$window), decreasing = TRUE) == 0, "t", paste0("t-", sort(unique(plot_data$window), decreasing = TRUE)))
  
  plot_data <- plot_data %>%
    mutate_at("window", ~factor(window_string, window_levels)) %>%
    mutate_at("type", ~factor(., levels = col_types))
  
  plot_data_nn <- plot_data %>% 
    filter(src == "nn") %>%
    mutate_at("entry", ~factor(., levels = sort(unique(entry))))
  
  plot_data_curr <- plot_data %>% filter(src == "curr")
  plot_data_pred <- plot_data %>% filter(src == "pred" & !is.na(value))
  
  ggplot2::ggplot(mapping = ggplot2::aes(x = type, y = value, group = entry)) +
    
    # t-x
    ggplot2::geom_point(data = plot_data_nn) +
    ggplot2::geom_line(data = plot_data_nn) +
    
    # t
    ggplot2::geom_point(data = plot_data_curr, color = "red", size = 1.5) +
    ggplot2::geom_line(data = plot_data_curr, color = "red", linetype = "solid") +
    
    # pred t
    ggplot2::geom_point(data = plot_data_pred, color = "red", alpha = 1) + 
    ggplot2::geom_line(data = plot_data_pred, color = "red", linetype = "dashed", alpha = 1) + 
    
    ggplot2::theme_bw() +
    ggplot2::facet_grid(rows = ~window) +
    ggplot2::ggtitle(title) + 
    ggplot2::xlab(NULL) + 
    ggplot2::ylab(NULL)
}


#' Predict a value based on the values of the nearest neoighbors in the past
#'
#' @param data_wide_pred A data.frame with the columns to predict (including at least all row indices included in nn_idx)
#' @param nn_idx A matrix of indizes of the nearest neighbors, if left empty as default the method is applied to all entries in the column, this is handy if
#' already all neighbors of an entry are given in data_wide
#' @param method Methodology to predict value, see nn_apply 
#'
#' @return A data.frame with same dimensionality and cols as data_wide_pred with the prediced values
#' @export
#'
#' @examples
#' data_wide <- data.frame(Low = c(100, 101, 104, 105), High = c(102, 103, 105, 106))
#' nn_pred(data_wide, method = "mean")
#' 
#' nn_pred(data_wide[0, ], method = "mean")
#' 
#' nn_idx <- matrix(c(2, 3, 4, 1, 3, 4, 1, 2, 4, 1, 2, 3), ncol = 3, byrow = TRUE)
#' nn_pred(data_wide, nn_idx, method = "mean")
pred_nn <- function(data_wide, nn_idx = matrix(seq_len(nrow(data_wide)), ncol = nrow(data_wide)), method = "mean") {
  map_dfc(names(data_wide), function(col) {
    data_wide_nn <- matrix(data_wide[[col]][nn_idx], ncol = ncol(nn_idx))
    if (method == "mean") {
      return(apply(data_wide_nn, 1, mean))
    }
  }) %>% set_names(names(data_wide))
}


plot_ratio_history <- function(quotes_line, data_pred, title = NULL, both_first = 1234) {
  
  stopifnot(nrow(quotes_line) == nrow(data_pred))
  data <- bind_cols(quotes_line, data_pred)
  
  payoff_base <- sum(calc_payoff_const_gamma(data, both_first = both_first))
  
  unique_sorted_dates <- sort(unique(data$Date))
  
  data_per_date <- arrange(data, Date) %>% split(data$Date)
  
  payoffs <- map_df(seq_along(data_per_date), function(i) {
    curr_data <- data_per_date[[i]]
    payoff_base_curr <- sum(calc_payoff_const_gamma(curr_data, both_first = both_first))
    payoff_curr <- sum(calc_payoff_const_gamma(curr_data, buy = curr_data$Buy, sell = curr_data$Sell, both_first = both_first))
    list(date = unique_sorted_dates[i], payoff_curr = payoff_curr, payoff_base = payoff_base_curr, n = nrow(curr_data))
  })
  
  payoffs$ratio <- payoffs$payoff_curr / payoffs$payoff_base
  payoffs$ratio_cum <- cumsum(payoffs$payoff_curr) / cumsum(payoffs$payoff_base)
  
  ggplot2::ggplot(payoffs, aes(x = date, y = ratio_cum)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Zeit") +
    ggplot2::ylab("Verhältnis Payoff Passives / Aktives Szenario") +
    ggplot2::ggtitle(title) + 
    ggplot2::theme_bw()
  
}


#' Sort nearest neighbor index matrix according to another variable. This functions also guarantees the correctness of the self references of the content.
#'
#' @param nn_idx A matrix of integers self row refercences (i.e. nearest neighbor indizes)
#' @param x A vector for which nn_idx needs to be sorted
#' @param reverse A logical. If FALSE nn_idx is sorted accoding to x. If TRUE it is assumed that nn_idx is already sorted accoriding to x and should be 
#' reverted to the order of x.
#'
#' @return
#' @export
#'
#' @examples
#' nn_idx <- matrix(c(2, 1, 1, 1, 3, 3, 2, 2), ncol = 2)
#' x <- c(5, 3, 4, 6)
#' sort_nn_idx(nn_idx, x, TRUE)
#' test_values <- c(10, 20, 30, 40)
#' all(matrix(test_values[nn_idx], ncol = ncol(nn_idx)) == matrix(test_values[order(x)][sort_nn_idx(nn_idx, x, FALSE)], ncol = ncol(nn_idx))[order(order(x)), ])
#' all(sort_nn_idx(sort_nn_idx(nn_idx, x, FALSE), x, TRUE) == nn_idx)
#' 
#' # Do Not Run
#' # all(sort_nn_idx(sort_nn_idx(nn$nn.idx, quotes_line$Date, FALSE), quotes_line$Date, TRUE) == nn$nn.idx, na.rm = TRUE)
sort_nn_idx <- function(nn_idx, x, reverse = FALSE) {
  stopifnot(nrow(nn_idx) == length(x))
  orig_order <- order(x)
  if (reverse) {
    return(matrix(orig_order[nn_idx], ncol = ncol(nn_idx))[order(orig_order), ])
  } else {
    # nothing to do
    if (all(orig_order == seq_along(x))) return(nn_idx)
    return(matrix(order(orig_order)[nn_idx], ncol = ncol(nn_idx))[orig_order, ])
  }
}

# dates <- quotes_line$Date
# nn_idx <- nn$nn.idx
# na_row_bool <- rowSums(nn_idx, is.na(nn_idx)) == 0
# nn_idx[!na_row_bool, ]
# 
# which(rowSums(is.na(a)) > 30 & rowSums(is.na(a)) < 50)
# a[10689, ]
# nn_idx[4418184, ]
# quotes_line[4418184, ]

#' a <- bootstrap_nn(dates, nn_idx)
bootstrap_nn <- function(dates, nn_idx, k = ncol(nn_idx)) {
  
  k <- min(k, ncol(nn_idx))
  already_sorted <- all(order(dates) == seq_len(length(dates)))
  
  # sort accoring to dates
  if (!already_sorted) {
    date_order <- order(dates)
    nn_idx <- sort_nn_idx(nn_idx, dates)
    dates_ordered <- dates[date_order]
  } else {
    dates_ordered <- dates
  }
  
  unique_dates <- unique(dates_ordered)
  
  # fast way to count dates (faster than table)
  date_count <- dates_ordered %>%
    as.integer() %>%
    split(., .) %>%
    map_int(~length(.)) %>%  
    unname() %>%
    tibble(Date = unique_dates, Freq = ., Cum_Freq = cumsum(.))
  
  bootstrap_idx <- map(seq_along(unique_dates), function(i) {
    
    cum_sum <- date_count$Cum_Freq[i]
    size <- date_count$Freq[i]
    
    if (cum_sum == size) {
      return(matrix(rep(NA, k * size), ncol = k))
    } 
    
    curr_nn_idx <- nn_idx[seq(cum_sum - size + 1, cum_sum), ]
    idx_count <- rbinom(sum(!is.na(curr_nn_idx)), cum_sum - size, 1 / (cum_sum - size))
    idx <- which(!is.na(curr_nn_idx), arr.ind = TRUE)

    if (is.null(dim(idx))) {
      dim(idx) <- c(length(idx)/2, 2)
    }

    # create new matrix indizes to insert values
    rows <- rep(idx[, 1], idx_count)
    cols <- ave(rep(1, length(rows)), rows, FUN = cumsum)

    idx_new <- c(rows, cols)
    dim(idx_new) <- c(length(rows), 2)
    idx_new <- idx_new[cols <= k , ]

    # create new matrix and fill
    curr_nn_idx_boot <- matrix(rep(NA, size * k), ncol = k)
    curr_nn_idx_boot[idx_new] <- rep(curr_nn_idx[!is.na(curr_nn_idx)], idx_count)[cols <= k]
    curr_nn_idx_boot
  })
  
  nn_idx_boot <- do.call(rbind, bootstrap_idx)
  if (!already_sorted) {
    date_order <- order(dates)
    nn_idx_boot <- sort_nn_idx(nn_idx_boot, dates, reverse = TRUE)
  }
  
  nn_idx_boot
}



# nn <- readRDS("data/nn_eucl_olhc_w3_38a896430298c738055505dc89e042ac.rds")
# na_row_bool <- rowSums(is.na( nn$nn.idx)) > 0
# nn_idx_sample <-  nn$nn.idx[!na_row_bool, ]
# rm(na_row_bool)
# 
# smpls <- sample_bootstrap(1, nrow(nn_idx_sample), 123456)
# bootstrap_idx <- smpls[[1]]
# rm(smpls)
# gc()
# 
# 

bootstrap_nn_idx <- function(bootstrap_idx, nn_idx) {
  curr_nn_idx <- nn_idx_sample[bootstrap_idx, ]
  curr_nn_idx[!curr_nn_idx %in% bootstrap_idx] <- NA
  bootstraped_nn_idx <- matrix(rep(NA, length(curr_nn_idx)), ncol = ncol(curr_nn_idx))
  idx <- which(!is.na(curr_nn_idx), arr.ind = TRUE)
  orig_order <- order(idx[,1])
  idx <- idx[orig_order, ]
  idx[, 2] <- ave(rep(1, nrow(idx)), idx[, 1], FUN = cumsum)
  idx <- idx[order(orig_order), ]
  bootstraped_nn_idx[idx] <- curr_nn_idx[!is.na(curr_nn_idx)]
  bootstraped_nn_idx
}

# 
# test <- bootstrap_nn_idx(bootstrap_idx, nn_idx_sample)
# pred_nn(data_wide = , nn_idx = test[, seq_len(k)])
# 
# 
# 
