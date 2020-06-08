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
#' @param quotes_line A data.frame with at least the following columns: "Close_1", "Low", "High", "Close_0"
#' @param sell The upper price at which the the delta hedge should be "rebalanced"
#' @param buy The lower price at which the the delta hedge should be "rebalanced"
#' @param gamma The gamma value (assumed constant)
#' @param both_first Character indicating what price movement is assumed first in cases where both, the buy and sell prices are reaches. One of 
#' c("buy", "sell", "min", "max") or a vector with "buy" and "sell" of same length as quotes_line. "buy" indicates that the buy event was reached first. 
#' "sell" indicates that the sell event was reached first. "min" conservativly considers the move first that leaves to the least pnl. "max" uses the event 
#' first that least to highest pnl.
#'
#' @return The pnl for every row
#' @export
#'
#' @examples
#' quotes_line_test_1 <- tribble(
#'    ~Close_1,   ~Low,   ~High, ~Close_0,
#'          10,      8,      12,        9
#'  )
#' calc_payoff_const_gamma(quotes_line_test_1)
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High)
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "buy")
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "sell")
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "max")
#' calc_payoff_const_gamma(quotes_line_test_1, buy = quotes_line_test_1$Low, sell = quotes_line_test_1$High, both_first = "min")
#' 
#' quotes_line_test_2 <- tribble(
#'    ~Close_1, ~Low, ~High, ~Close_0,
#'           9,    8,    12,       10
#'  )
#' calc_payoff_const_gamma(quotes_line_test_2)
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "buy")
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "sell")
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "max")
#' calc_payoff_const_gamma(quotes_line_test_2, buy = quotes_line_test_2$Low, sell = quotes_line_test_2$High, both_first = "min")
calc_payoff_const_gamma <- function(quotes_line, buy = quotes_line$Close_0, sell = quotes_line$Close_0, both_first = "min", gamma = 0.2, return_type = "disc") {
  
  stopifnot(all(buy <= sell, na.rm = TRUE))
  
  if (length(buy) == 1L && nrow(quotes_line) > 1) buy <- rep(buy, nrow(quotes_line))
  if (length(sell) == 1L && nrow(quotes_line) > 1) sell <- rep(sell, nrow(quotes_line))
  
  if (length(both_first) == 1L && both_first %in% c("min", "max")) {
    
    buy_first <- calc_payoff_per_title(
      close_1 = quotes_line$Close_1, 
      close_0 = quotes_line$Close_0,
      first = buy,
      second  = sell, 
      low = quotes_line$Low, 
      high = quotes_line$High, 
      gamma = gamma, 
      return_type = return_type
    )
    
    sell_first <- calc_payoff_per_title(
      close_1 = quotes_line$Close_1, 
      close_0 = quotes_line$Close_0,
      first = sell,
      second  = buy, 
      low = quotes_line$Low, 
      high = quotes_line$High, 
      gamma = gamma, 
      return_type = return_type
    )
    
    first <- buy
    second <- sell
    if (both_first == "min") {
      first[sell_first < buy_first] <- sell[sell_first < buy_first]
      second[sell_first < buy_first] <- buy[sell_first < buy_first]
    } else  if (both_first == "max") {
      first[sell_first > buy_first] <- sell[sell_first > buy_first]
      second[sell_first > buy_first] <- buy[sell_first > buy_first]
    }
  } else if (all(both_first %in% c("buy", "sell"))) {
    if (length(both_first) == 1 && nrow(quotes_line) > 1) {
      both_first <- rep(both_first, nrow(quotes_line))
    }
    
    buy_first_bool <- both_first == "buy"
    first <- buy
    second <- sell
    if (sum(!buy_first_bool) > 0) {
      first[!buy_first_bool] <- sell[!buy_first_bool]
      second[!buy_first_bool] <- buy[!buy_first_bool]
    }
    
  } else {
    stop("Expected input for argument both_first.")
  }
  
  calc_payoff_per_title(
    close_1 = quotes_line$Close_1, 
    close_0 = quotes_line$Close_0,
    first = first,
    second  = second, 
    low = quotes_line$Low, 
    high = quotes_line$High, 
    gamma = gamma, 
    return_type = return_type
  )
  
}


#' #' Get history of current element
#' #'
#' #' @param data A sorted data.frame sortest by Ticker and desc(Date)
#' #' @param window An integer indicating the length of the history
#' #' @param col The column names of the values for which the history should be widened
#' #' @param include_current A logical indicating if the current date (t = 0) should be included in the output
#' 
#' #' @return A data.frame of same length and order like data with the history for each row for the requestes columns. The number at the end of the column tells
#' #' their order and indicates the distance to the current data (i.e. *_7 meaning t-7).
#' #' @export
#' #'
#' #' @examples
#' #' data <- tibble(a = 1:100, b = 1:100)
#' #' widen(data, 7, c("a", "b"))
#' #' widen(data, 7, c("a", "b"), include_current = TRUE)
#' #' widen(data, 7, c("a"))
#' #' widen(data, 7, c("a"), include_current = TRUE)
#' widen <- function(data, window, cols = names(data), include_current = FALSE) {
#'   
#'   # fill first rows such that even first row has predecessor
#'   data_fill <- data[seq_len(window), ] 
#'   data_fill[seq_len(dim(data_fill)[1]), seq_len(dim(data_fill)[2])] <- NA
#'   data <- bind_rows(data_fill, data)
#'   
#'   # widen data
#'   row_idx <- map(seq_len(nrow(data) - window), function(x) {
#'     curr_row_idx <- seq_len(window) + x - 1
#'     if (include_current) {
#'       curr_row_idx <- c(curr_row_idx, window + x)
#'     }
#'     curr_row_idx
#'   }) %>% unlist()
#'   
#'   values_fn <- map(cols, ~list) %>% set_names(cols)
#'   data_widened <- data[row_idx, cols] %>%
#'     mutate(group_id = rep(rev(seq_len(window + include_current) - include_current), length(row_idx) / (window + include_current))) %>%
#'     tidyr::pivot_wider(
#'       names_from = group_id, 
#'       values_from = all_of(cols), 
#'       values_fn = values_fn
#'     )
#'   
#'   # name columns in case of single feature
#'   if (length(cols) == 1) names(data_widened) <- paste0(cols, "_", names(data_widened))
#'   
#'   unnest(data_widened, cols = names(data_widened)) 
#' }


#' Get history of current element
#'
#' @param data A sorted data.frame sortest by Ticker and desc(Date)
#' @param window An integer indicating the length of the history
#' @param col The column names of the values for which the history should be widened
#' @param keep A character vector indicating what existing columns should be kept

#' @return A data.frame of same length and order like data with the history for each row for the requestes columns. The number at the end of the column tells
#' their order and indicates the distance to the current data (i.e. *_7 meaning t-7).
#' @export
#'
#' @examples
#' data <- tibble(a = 1:100, b = 1:100)
#' widen(data, 7, c("a", "b"))
#' widen(data, 7, c("a", "b"), keep = c("a", "b"))
#' widen(data, 7, c("a"))
#' widen(data, 7, c("a"), keep = "b")
widen <- function(data, window, cols = names(data), keep = NULL) {
  
  # fill first rows such that even first row has predecessor
  data_fill <- data[seq_len(window), ] 
  data_fill[seq_len(dim(data_fill)[1]), seq_len(dim(data_fill)[2])] <- NA
  data_all <- bind_rows(data_fill, data)
  
  # widen data  
  row_idx <- rep(seq_len(window), nrow(data)) + rep(seq_len(nrow(data)) - 1, each = window)
  
  values_fn <- map(cols, ~list) %>% set_names(cols)
  data_widened <- data_all[row_idx, cols] %>%
    mutate(group_id = rep(rev(seq_len(window )), length(row_idx) / (window))) %>%
    tidyr::pivot_wider(
      names_from = group_id, 
      values_from = all_of(cols), 
      values_fn = values_fn
    )
  
  # name columns in case of single feature
  if (length(cols) == 1) names(data_widened) <- paste0(cols, "_", names(data_widened))
  bind_cols(unnest(data_widened, cols = names(data_widened)), data[, keep])
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


#' # output is sorted in arrange(data, Date, Ticker)
#' #' Find nearest neighbors chunkwise. Invalid indices (because they point to a later point in time) are replaced with NA. Additionally the output matrices are
#' #' organized such that NA values are at the very right.
#' #'
#' #' @param data A data.frame with coluns Date and Ticker and other numeric columns that should be compared. Data.frame must be sorted for "Date" and "Ticker", 
#' #' otherwise an error will be thrown.
#' #' @param distance One of c("euclidean", "manhattan"
#' #' @param k The number of nearest neigbours to find. Notice that these are not guaranteed since invalid indices (since pointing to late pint in time) are
#' #' replaced by NA.
#' #' @param n_chunks Number of chunks to split the data into.  
#' #' @param mc.cores The number of cores for parallel execution
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' data <- data.frame(
#' #'   Date = as.Date(c("2001-01-01", "2001-01-02", "2001-01-02")), 
#' #'   Ticker = c("A", "A", "A"), 
#' #'   value = c(1, 3, 1.2)
#' #' )
#' #' unkwise(data = data, n_chunks = 1, k = 3)
#' find_nn_chunkwise <- function(data, distance = "euclidean", k = 75, n_chunks = 500, mc.cores = parallel::detectCores()) {
#'   
#'   # check if data is ordered correctly
#'   if (!all(order(data$Date, data$Ticker) == seq_len(nrow(data)))) stop("data must be sorted for Date and Ticker")
#'   stopifnot(all(order(data$Date, data$Ticker) == seq_len(nrow(data))))
#'   
#'   counts <- data %>% select(Date) %>% group_by(Date) %>% summarise(cnt = n()) %>% ungroup() %>% mutate(cum_sum = cumsum(cnt))
#'   
#'   breaks <- nrow(data) / n_chunks * seq_len(n_chunks)
#'   split_dates <- map(breaks, ~counts$Date[[min(which(counts$cum_sum>=.))]])
#'   
#'   # check if all chunk sizes >= k
#'   chunk_sizes <- diff(map_int(split_dates, ~sum(filter(counts, Date <= .)$cnt)))
#'   if (any(chunk_sizes < k)) stop("chunk size smaller than k, decrease number of chunks")
#'   
#'   
#'   rann_fct <- if (distance == "euclidean") {RANN::nn2} else if (distance == "manhattan") {RANN.L1::nn2} else stop("distance not supported")
#'   
#'   knn_list <- pbmcapply::pbmclapply(rev(seq_along(split_dates)), function(i) {
#'     prev_split_Date <- ifelse(i > 1, split_dates[[i - 1]], -Inf)
#'     curr_split_Date <- split_dates[[i]]
#'     curr_data <- as.matrix(select(filter(data, Date <= curr_split_Date), -Ticker, -Date))
#'     curr_query <- as.matrix(select(filter(data, Date <= curr_split_Date & Date > prev_split_Date) , -Ticker, -Date))
#'     rann_fct(curr_data, curr_query, k = k)
#'   },  mc.cores =  mc.cores)
#'   
#'   knn <- purrr::transpose(knn_list) %>% map(~do.call(rbind, .[rev(seq_along(.))]))
#'   
#'   bad_idx <- knn$nn.idx > rep(head(c(0, counts$cum_sum), -1), counts$cnt)
#'   knn$nn.idx[bad_idx] <- NA
#'   knn$nn.idx <- drop_na_nn_idx(knn$nn.idx, k)
#'   
#'   knn$nn.dists[bad_idx] <- NA
#'   knn$nn.dists <- drop_na_nn_dists(knn$nn.dists, k)
#'   
#'   knn
#' }
#' 
#' 
#' 
#' #' Find quote histories in the past that resembles the current the most
#' #'
#' #' @param data_wide A data.frame with all columns that need to be used to find the nearest neighbors
#' #' @param dates The dates for each row of data_wide
#' #' @param k The number of closest neighbors that should be looked for
#' #' @param norm The distance measure to use, of "euclidean" or "manhattan".
#' #' @param mc.cores The number of cores to use for the calculation. 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' dates <- as.Date(c("2020-01-03", "2020-01-02", "2020-01-02"))
#' #' data_wide <- tibble(
#' #'   Open_2 = c(93, 103, 104), 
#' #'   Open_1 = c(99, NA, 100), 
#' #'   Open_0 = c(103, 98, 100), 
#' #'   Close_2 = c(102, 105, 98), 
#' #'   Close_1 = c(102, 103, 101), 
#' #'   Close_0 = c(98, 99, 103)
#' #' )
#' #' find_nn(data_wide, dates)
#' find_nn <- function(data_wide, dates, k = 50, norm = "euclidean", mc.cores = parallel::detectCores() - 1) {
#'   
#'   n <- nrow(data_wide)
#'   
#'   # order dates such that indizes are strictly increasing
#'   date_order <- order(dates)
#'   dates_ordered <- dates[date_order]
#'   data_wide_ordered <- data_wide[date_order, ]
#'   rm(data_wide)
#'   
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
#'   na_row_bool <- rowSums(is.na(data_wide_ordered)) > 0
#'   data_wide_filtered <- data_wide_ordered[!na_row_bool, ]
#'   rm(data_wide_ordered)
#'   
#'   dates_filtered <- dates_ordered[!na_row_bool]
#'   rm(dates_ordered)
#'   
#'   unique_dates <- sort(unique(dates_filtered))
#'   knn_output <- pbmcapply::pbmclapply(seq_along(unique_dates), function(i) {
#'     curr_date <- unique_dates[i]
#'     data_query <- split_data_query(curr_date, dates_filtered, data_wide_filtered)
#'     rm(data_wide_filtered)
#'     
#'     if (i == 1) {
#'       out <- create_dummy_return(k, nrow(data_query$query))
#'       return(out)
#'     }
#'     
#'     out <- nn_fct(data_query$data_wide, query = data_query$query, min(k, nrow(data_query$data_wide)), treetype = "kd")
#'     
#'     if (nrow(data_query$data_wide) < k) {
#'       tmp <- create_dummy_return(k, nrow(data_query$query))
#'       tmp$nn.idx[, seq_len(nrow(data_query$data_wide))] <- out[["nn.idx"]]
#'       tmp$nn.dists[, seq_len(nrow(data_query$data_wide))] <- out[["nn.dists"]]
#'       out <- tmp
#'     }
#'     rm(data_query)
#'     return(out)
#'   }, mc.cores = mc.cores)
#'   
#'   nn <- list()
#'   
#'   # make sure also excluded rows are in the result (as NA rows)
#'   nn[["nn.idx"]] <- matrix(rep(NA_integer_, k * n), ncol = k)
#'   nn[["nn.idx"]][!na_row_bool, ] <- purrr::map(knn_output,  ~.[["nn.idx"]]) %>% do.call(rbind, .)
#'   
#'   # take into account in indices that there were excluded rows
#'   nn[["nn.idx"]] <- matrix(seq_len(n)[!na_row_bool][nn[["nn.idx"]]], ncol = k)
#'   
#'   nn[["nn.dists"]] <- matrix(rep(NA_real_, k * n), ncol = k)
#'   nn[["nn.dists"]][!na_row_bool, ] <- purrr::map(knn_output,  ~.[["nn.dists"]]) %>% do.call(rbind, .)
#'   
#'   # order result in the same way as inputed data
#'   nn[["nn.idx"]] <- nn[["nn.idx"]][order(date_order), ]
#'   nn[["nn.idx"]] <- matrix(date_order[nn[["nn.idx"]]], ncol = k)
#'   
#'   nn[["nn.dists"]] <- nn[["nn.dists"]][order(date_order), ]
#'   
#'   nn
#' }




#' Find nearest neighbors in the past
#'
#' @param data A data. frame with numeric columns used for comparison and column "Date"
#' @param distance One of "euclidean" and "mahnattan"
#' @param k The number of neighbors to look for
#' @param mc.cores The number of cores to use for parallel computation
#'
#' @return
#' A list of RANN outputs. One element for each date. 
#' @export
#'
#' @examples
#' 
#' data <- tibble(Date = as.Date(c("2020-01-03", "2020-01-02", "2020-01-03", "2020-01-01")), Close_1 = c(100, 101, 102, 103), Close_0 = c(102, 103, 104, 105)) %>%
#'   arrange(Date)
#'
#' find_nn(
#'   data = data,
#'   distance = "euclidean",
#'   k = 5
#' )
find_nn <- function(data, distance, k, mc.cores = getOption("mc.cores", 2L)) {
  
  if (any(order(data$Date) != seq_len(nrow(data)))) stop("Data must be sorted by date")
  unique_dates <- sort(unique(data$Date), decreasing = TRUE)
  
  fct <- if (distance == "euclidean") {
    RANN::nn2
  } else if (distance == "manhattan"){
    RANN.L1::nn2
  } else {
    stop("Unexpected distance measure.")
  }
  
  nn_list <- pbmclapply(unique_dates, function(curr_date) {
    curr_data <- select(filter(data, Date < curr_date), -Date)
    curr_query <- select(filter(data, Date == curr_date), -Date)
    
    if (nrow(curr_data) == 0) {
      out <- list(
        nn.idx = matrix(rep(NA_integer_, k * nrow(curr_query)), ncol = k),
        nn.dists =  matrix(rep(NA_real_, k * nrow(curr_query)), ncol = k)
      )
    } else {
      out <- fct(
        data = curr_data,
        query = curr_query,
        k = min(k, nrow(curr_data))
      ) 
      
      if (nrow(out$nn.idx) < k) {
        nn.idx  <- matrix(rep(NA_integer_, k * nrow(curr_query)), ncol = k)
        nn.idx[, seq_len(ncol(out$nn.idx))] <- out$nn.idx
        out$nn.idx <- nn.idx
        
        nn.dists <- matrix(rep(NA_real_, k * nrow(curr_query)), ncol = k)
        nn.dists[, seq_len(ncol(out$nn.dists))] <- out$nn.dists
        out$nn.dists <- nn.dists 
      }
    }
    out
  }, mc.cores = mc.cores)
  
  purrr::transpose(nn_list) %>% map(~do.call(rbind, .[rev(seq_along(.))]))
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
plot_nn <- function(data_wide_curr, data_wide_nn, title = NULL, fct = function(x) mean(x, na.rm = TRUE)) {  
  
  stopifnot(sort(names(data_wide_curr)) == sort(names(data_wide_nn)))
  col_types <- unique(stringr::str_extract(names(data_wide_curr), ".*(?=_[0-9]+$)"))
  
  # predict values
  data_wide_pred <- pred_nn(select(data_wide_nn, ends_with("_0")), fct = fct)
  
  plot_data <- mutate(data_wide_nn, src = "nn") %>%
    bind_rows(mutate(data_wide_curr, src = "curr")) %>%
    bind_rows(mutate(data_wide_pred, src = "pred")) %>%
    select(c(stringr::str_subset(names(.), "_[0-9]+$"), "src")) %>%
    mutate(entry = seq_len(n())) %>%
    tidyr::pivot_longer(-c("entry", "src"), names_to = "type") %>%
    separate(type, sep = "_", into = c("type", "window"))
  
  # factor to ensure order
  window_string <- dplyr::if_else(plot_data$window == 0, "t", paste0("t-", plot_data$window))
  window_levels <- dplyr::if_else(sort(unique(plot_data$window), decreasing = TRUE) == 0, "t", paste0("t-", sort(unique(plot_data$window), decreasing = TRUE)))
  
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
#' @param fct A function with one argument defining how the prediction should be calulated
#'
#' @return A data.frame with same dimensionality and cols as data_wide_pred with the prediced values
#' @export
#'
#' @examples
#' data_wide <- data.frame(Low = c(100, 101, 104, 105), High = c(102, 103, 105, 106))
#' pred_nn(data_wide, method = "mean")
#' 
#' pred_nn(data_wide[0, ], method = "mean")
#' 
#' nn_idx <- matrix(c(2, 3, 4, 1, 3, 4, 1, 2, 4, 1, 2, 3), ncol = 3, byrow = TRUE)
#' pred_nn(data_wide, nn_idx, method = "mean")
pred_nn <- function(data_wide, nn_idx = matrix(seq_len(nrow(data_wide)), ncol = nrow(data_wide)), fct = function(x) mean(x, na.rm = TRUE)) {
  map_dfc(names(data_wide), function(col) {
    data_wide_nn <- matrix(data_wide[[col]][nn_idx], ncol = ncol(nn_idx))
    return(apply(data_wide_nn, 1, fct))
  }) %>% set_names(names(data_wide))
}




#' Calculate payoff factors based on nearest neighbors compared to reference strategy
#'
#' @param data A data.frame with at least columns "Close_1", "Open_0", "Low_0", "High_0" and "Close_0"
#' @param nn_idx A data.frame containing the nearest neighbor indecies. Must be of same order as data
#' @param both_first Vector of "buy" and "sell", indicating if buy or sell action is done first.
#' @param fct A function with one argument defining how the prediction given the neigbores should be made
#' @param use_spread A boolean defining if the predicted Low and High spreads added to the actual OPen Price should be used instead of the predicted Low and High Prices
#'
#' @return
#' @export
#'
#' @examples
calc_nn_payoff_factor <- function(data, nn_idx, both_first, fct = function(x) mean(x, na.rm = TRUE), use_spread = FALSE) {
  
  scale_fct_test <- sum(calc_payoff_const_gamma(select(data, Close_1, Low = "Low_0", High = "High_0", Close_0), both_first = both_first))
  pred <- pred_nn(
    data_wide = data %>% select(Open_0, Low_0, High_0), 
    nn_idx = as.matrix(nn_idx), 
    fct = fct
  )
  pred$Low_0[is.na(pred$Low_0)] <- -Inf
  pred$High_0[is.na(pred$High_0)] <- Inf
  
  if (use_spread) {
    buy <- data$Open_0 + (pred$Low_0 - pred$Open_0)
    sell <- data$Open_0 + (pred$High_0 - pred$Open_0)
  } else {
    buy <- pred$Low_0
    sell <- pred$High_0
  }
  
  payoff <- sum(calc_payoff_const_gamma(
    quotes_line = select(data, Close_1, Low = "Low_0", High = "High_0", Close_0),
    buy = buy,
    sell = sell,
    both_first = both_first)
  )
  payoff / scale_fct_test
}



plot_ratio_history <- function(quotes_line, data_pred, title = NULL, both_first = 1234) {
  
  stopifnot(nrow(quotes_line) == nrow(data_pred))
  data <- bind_cols(quotes_line, data_pred)
  
  payoff_base <- sum(calc_payoff_const_gamma(data, both_first = both_first))
  
  unique_sorted_dates <- sort(unique(data$Date))
  
  data_per_date <- arrange(data, Date) %>% split(data$Date)
  
  browser()
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


#' Calculate payoff variation factors including bootstrap for different symmetric price moves
#'
#' @param data A data.frame with at least the following columns: "Close_1", "Low", "High", "Close_0"
#' @param both_first Character indicating what price movement is assumed first in cases where both, the buy and sell prices are reaches. One of 
#' c("buy", "sell", "min", "max") or a vector with "buy" and "sell" of same length as quotes_line. "buy" indicates that the buy event was reached first. 
#' "sell" indicates that the sell event was reached first. "min" conservativly considers the move first that leaves to the least pnl. "max" uses the event 
#' first that least to highest pnl.
#' @param move The moves (up and down) to be calculated
#' @param R The number of bootstrap samples
#' @param col The columns which should be moved
#' @param mc.cores The number of cores used for parallelization
#'
#' @return A data.frame with columns "move", "original", "bias", "std_error", "lower" and "upper"
#' @export
#'
#' @examples
#' data <- quotes_line_test_1 <- tribble(
#'    ~Close_1, ~Open,  ~Low, ~High, ~Close_0,
#'          10,     9,   8,    12,        9,
#'           9,     7,   5,    17,        10
#'  )
#'  bootstrap_variation_factor(data = data, both_first = "buy", move = seq(0, 0.1, 0.01), R = 10)
bootstrap_variation_factor <- function(data, both_first, move, R, col = "Open", mc.cores = getOption("mc.cores", 2L)) {
  purrr::map_df(move, function(curr_move) {
    print(curr_move)
    bootstr <- boot::boot(data, function(data, index) {
      if (length(both_first) == 1 & nrow(data) > 1) {
        both_first <- rep(both_first, nrow(data))
      }
      scale_fct <- sum(calc_payoff_const_gamma(data[index, ], both_first = both_first[index]))
      opt_payoff_sym(data = data[index, ], move = curr_move, col = col, both_first = both_first[index], scale_fct = scale_fct)
    }, R = R, parallel = "multicore", ncpus = mc.cores)
    ci <- boot::boot.ci(bootstr, conf = 0.95, type = "perc")
    out <- list(move = curr_move, original = bootstr$t0, bias = mean(bootstr$t) - bootstr$t0, std_error = sd(bootstr$t))
    if (is.null(ci)) {
      out <- c(out, list(lower = bootstr$t0, upper = bootstr$t0))
    } else {
      out <- c(out, list(lower = as.vector(ci$percent[1, 4]), upper = as.vector(ci$percent[1, 5])))
    }
  })
}


#' Plot payoff ratio for factor variation with confidence ribbon.
#'
#' @param data A data.frame with at least the columns "move", "original", "lower" and "upper"
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' plot_variation_factor(tibble(move = c(0.01, 0.02), original = c(1, 2), lower = c(0, 1.5), upper = c(3, 4.5)))
plot_variation_factor <- function(data, title = "Payoffvergleich bei symmetrischer Abwechung vom Eröffnungspreis") {
  p <- ggplot2::ggplot(data, aes(x = move)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
    ggplot2::geom_line(aes(y = original)) +
    ggplot2::xlab("Abweichung vom Eröffnungspreis") + 
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::ylab( "Payoffverhältnis zur Referenzstrategie") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw()
  
  if ("group" %in% names(data)) {
    p <- p + facet_wrap(~group)
  }
  
  p
}



#' Discretizize data into buckets of same size based on training data. The columns are processed sequentially. Lowest and Highets buckets start from -Inf and 
#' Inf respectively, such that out of box test data gets classified as well.
#'
#' @param data A data.frame with at least the columns given in cols
#' @param train_idx An integer vector holding the indices of the trainingset
#' @param test_idx An integer vector holding the indices of the testset
#' @param cols The column names to use for bucketing, the processing is done in order of mentioning
#' @param n_groups_per_col Number of buckets per column
#'
#' @return A list with elements "groups" and "borders". "Groups" is an integer vector of the groups for the data (train and testset), starting at 0. "Borders" is a data.frame
#' including the borders for every bucket.
#' @export
#'
#' @examples
#' multivariate_discretization(data, train_idx, test_idx, c("Low_0", "High_0", "Close_0"), 3)
# out <- multivariate_discretization(data_wide_3_all, train_idx, test_idx, c("Low_0", "High_0", "Close_0"), 10)
# out$groups %>% tibble(groups = .) %>% group_by(groups) %>% summarize(n = n()) %>% ungroup() %>% arrange(desc(n))
multivariate_discretization <- function(data, train_idx, test_idx, cols, n_groups_per_col) {
  
  data_train <- dplyr::slice(data, train_idx)
  data_test <- dplyr::slice(data, test_idx)
  
  groups_prev_train <- rep(0L, length(train_idx))
  groups_prev_test <- rep(0L, length(test_idx))
  
  # init border table
  if (length(cols) == 1) {
    border_names <- c("lower", "upper")
  } else {
    border_names <- map(cols, ~paste0(., c("_lower", "_upper"))) %>% unlist()
  }
  borders <- map(seq_len(length(cols) * 2), ~rep(NA_real_, n_groups_per_col^length(cols))) %>% 
    set_names(border_names) %>% 
    tibble::as_tibble() %>%
    cbind(tibble(Bucket = seq_len(n_groups_per_col^length(cols)) - 1), .)
  
  for (col_id in seq_along(cols)) {
    
    curr_col <- cols[[col_id]]
    groups_curr_train <- rep(0L, length(train_idx))
    groups_curr_test <- rep(0L, length(test_idx))
    print(paste0("col_id: ", col_id))
    
    for (curr_group in seq_len(n_groups_per_col^(col_id - 1)) - 1) {
      
      print(paste0("curr_group: ", curr_group))
      curr_train_group_idx <- which(groups_prev_train == curr_group)
      curr_train_values <- data_train[[curr_col]][curr_train_group_idx]
      curr_breaks <- quantile(curr_train_values, probs = seq(0, 1, length  = n_groups_per_col + 1))
      curr_breaks[1] <- -Inf
      curr_breaks[length(curr_breaks)] <- Inf
      curr_breaks <- as.vector(curr_breaks)
      
      # write lower and upper limits
      start_idx <- curr_group * n_groups_per_col^(length(cols) - col_id + 1) + 1
      curr_idx <- seq_len(n_groups_per_col * n_groups_per_col^(length(cols) - col_id)) + start_idx - 1
      
      curr_lower <- head(curr_breaks, -1) %>% rep(each = n_groups_per_col^(length(cols) - col_id))
      borders[curr_idx, 1 + (col_id - 1) * 2 + 1] <- curr_lower
      
      curr_upper <- tail(curr_breaks, -1) %>% rep(each = n_groups_per_col^(length(cols) - col_id))
      borders[curr_idx, 1 + (col_id - 1) * 2 + 2] <- curr_upper
      
      if (col_id == 1) {
        group_offset <- 0
      } else {
        group_offset <- curr_group * n_groups_per_col
      }
      
      curr_breaks[duplicated(curr_breaks)] <- NA
      groups_curr_train[curr_train_group_idx] <- group_offset + as.integer(cut(curr_train_values, breaks = curr_breaks)) - 1
      curr_test_group_idx <- which(groups_prev_test == curr_group)
      curr_test_values <- data_test[[curr_col]][curr_test_group_idx]
      
      groups_curr_test[curr_test_group_idx] <- group_offset + as.integer(cut(curr_test_values, breaks = curr_breaks)) - 1
    }
    groups_prev_train <- groups_curr_train
    groups_prev_test <- groups_curr_test
  }
  
  groups <- rep(NA_integer_, nrow(data))
  groups[train_idx] <- groups_curr_train
  groups[test_idx] <- groups_curr_test
  
  list(groups = groups, borders = borders)
}

#' Load neural model
#' If the model is available on disk it will be loaded and returned. If not all necessary files can be found, an error
#' is thrown. In such situations the input files for the model should be written to disk and the python scripts for
#' training should be run.
#'
#' @param data_wide A data.frame containing the "wide" data history of the model
#' @param type One of "ind" (indipendent) or "dep" (dependent)
#' @param crossentropy One of "categorical" (non-ordinal) or "binary" (ordinal)
#'
#' @return
#' A list with elements "discretization" and "pred" for the testing set.
#' @export
#'
#' @examples
#' get_neural_model_ind(data_wide_3_all)
get_neural_model_ind <- function(data_wide, architecture, crossentropy = "categorical") {
  
  type <- "ind"
  path <- paste0("data/models/", type, "/", crossentropy, "/", architecture, "/")
  n_groups_per_col <- 30
  
  ### model for close
  model_names <- c("low_pred_prob", "high_pred_prob", "close_pred_prob")
  model_paths <- paste0(path, model_names, ".feather")
  
  nn = NULL
  nn$discretization <- list(
    low = multivariate_discretization(data_wide, train_idx, test_idx, "Low_0", n_groups_per_col),
    high = multivariate_discretization(data_wide, train_idx, test_idx, "High_0", n_groups_per_col),
    close = multivariate_discretization(data_wide, train_idx, test_idx, "Close_0", n_groups_per_col)
  )
  
  if (all(map_lgl(model_paths, file.exists))) {
    nn$pred$low = read_feather(model_paths[1])
    nn$pred$high = read_feather(model_paths[2])
    nn$pred$close = read_feather(model_paths[3])
    
    if (crossentropy == "binary") {
      
      translate_cum_prob <- function(cum_prob) {
        col_names <- names(cum_prob)
        n_col <- ncol(cum_prob)
        n_row <- nrow(cum_prob)
        for (i in tail(seq_len(n_col), -1)) {
          cum_prob[[i]] <- pmin(cum_prob[[i]], cum_prob[[i-1]])
        }
        cum_prob - set_names(bind_cols(cum_prob[, head(1 + seq_len(n_col), -1)], tmp = rep(0, n_row)), col_names)
      }
      
      nn$pred$low <- translate_cum_prob(nn$pred$low)
      nn$pred$high <- translate_cum_prob(nn$pred$high)
      nn$pred$close <- translate_cum_prob(nn$pred$close)
    }
    
    
    
    return(nn)
  } else {
    stop(paste0("At least one neural network model does not exist. Please execute python code first."))
    
    labels <- tibble(
      low = nn$discretization$low %$% groups,
      high = nn$discretization$high %$% groups, 
      close = nn$discretization$close %$% groups
    )
    
    write_feather(labels[train_idx, ], paste0(path, "labels_train.feather"))
    
    data_short <- shorten_data(data_wide)
    write_feather(data_short[train_idx, ], paste0(path, "data_train.feather"))
    write_feather(select(data_wide, train_cols)[test_idx, ], paste0(path, "data_test.feather"))
    
    # Execute jupyter notebook: py/*.ipynb
  }
}

get_neural_model_dep <- function(data_wide, architecture, crossentropy, n_groups_per_col) {
  
  type <- "dep"
  path <- paste0("data/models/", type, "/", crossentropy, "/", architecture, "/")
  
  ### model for close
  model_names <- c("pred_prob")
  model_paths <- paste0(path, model_names, ".feather")
  
  nn = NULL
  nn$discretization <- multivariate_discretization(data_wide, train_idx, test_idx, c("Low_0", "High_0", "Close_0"), n_groups_per_col)
  
  if (all(map_lgl(model_paths, file.exists))) {
    nn$pred = read_feather(model_paths)
    
    if (crossentropy == "binary") {
      
      translate_cum_prob <- function(cum_prob) {
        col_names <- names(cum_prob)
        n_col <- ncol(cum_prob)
        n_row <- nrow(cum_prob)
        for (i in tail(seq_len(n_col), -1)) {
          cum_prob[[i]] <- pmin(cum_prob[[i]], cum_prob[[i-1]])
        }
        cum_prob - set_names(bind_cols(cum_prob[, head(1 + seq_len(n_col), -1)], tmp = rep(0, n_row)), col_names)
      }
      
      seq_1 <- seq(0 * n_groups_per_col + 1, by = 1, length.out = n_groups_per_col)
      cum_prob[, seq_1] <- translate_cum_prob(cum_prob[, seq_1])
      
      seq_2 <- seq(0 * n_groups_per_col + 1, by = 1, length.out = n_groups_per_col)
      cum_prob[, seq_2] <- translate_cum_prob(cum_prob[, seq_2])
      
      seq_3 <- seq(0 * n_groups_per_col + 1, by = 1, length.out = n_groups_per_col)
      cum_prob[, seq_3] <- translate_cum_prob(cum_prob[, seq_3])
      
      nn$pred <- cum_prob
    }
    
    return(nn)
  } else {
    stop(paste0("At least one neural network model does not exist. Please execute python code first."))
    
    labels <- tibble(labels = nn$discretization %$% groups)
    write_feather(labels[train_idx, ], paste0(path, "labels_train.feather"))
    
    data_short <- shorten_data(data_wide)
    write_feather(data_short[train_idx, ], paste0(path, "data_train.feather"))
    write_feather(data_short[test_idx, ], paste0(path, "data_test.feather"))
    
    # Execute jupyter notebook: py/*.ipynb
    
    pred_prob_1 <- read_feather(paste0(path, "pred_prob_1.feather"))
    pred_prob_2 <- read_feather(paste0(path, "pred_prob_2.feather"))
    pred_prob_3 <- read_feather(paste0(path, "pred_prob_3.feather"))
    
    pred_prob <- bind_rows(pred_prob_1, pred_prob_2, pred_prob_3)
    write_feather(pred_prob, paste0(path, "pred_prob.feather"))
    rm(pred_prob_1, pred_prob_2, pred_prob_3)
    
  }
}


#' Remove columns from wide data that should not be used for model estimation
#'
#' @param data_wide A data.frame containing columns not usable for training or estimaton (i.e Ticker, date, etc.) plus
#' all data from window lying mostly back and the current data.
#'
#' @return
#' A data.frame only containing data necessary for training and estimation.
#' @export
#'
#' @examples
#' shorten_data(tibble(Date = 1, Ticker = "ABC", Close_3 = 100, Close_2 = 100, Open_2 = 100, Low_1 = 100, High_0 = 200))
shorten_data <- function(data_wide) {
  train_cols <- setdiff(names(data_wide), c("Ticker", "Date", "Close_0", "Low_0", "High_0"))
  max_window <- max(as.integer(str_extract(train_cols, "[0-9]+$")))
  train_cols <- train_cols[!str_detect(train_cols, paste0(max_window, "$"))]
  select(data_wide, all_of(train_cols))
}

#' Prepare data to create histogram
#'
#' @param borders A data.frame with columns ending with "lower" and "upper"
#' @param prob A vector of prbabilities for every bucket in borders.
#' @param group The name that should be added in columns group of the output
#'
#' @return A data.frame with columns "lower", "upper" and "prob". If group is given this is added in a column "group"
#' @export
#'
#' @examples
eval_hist_data <- function(borders, prob, group = NULL) {
  stopifnot(nrow(borders) == length(prob))
  borders %>%
    mutate(prob = prob) %>%
    rename_at(dplyr::vars(tidyselect::ends_with("lower")), ~"lower") %>%
    rename_at(dplyr::vars(tidyselect::ends_with("upper")), ~"upper") %>%
    select("lower", "upper", "prob") %>%
    mutate(group = group)
}


#' Load neural model
#' If the model is available on disk it will be loaded and returned. If not all necessary files can be found, an error
#' is thrown. In such situations the input files for the model should be written to disk and the python scripts for
#' training should be run.
#'
#' @param data_wide A data.frame containing the "wide" data history of the model
#' @param type One of "ind" (indipendent) or "dep" (dependent)
#' @param crossentropy One of "categorical" (non-ordinal) or "binary" (ordinal)
#'
#' @return
#' A list with elements "discretization" and "pred" for the testing set.
#' @export
#'
#' @examples
#' get_neural_model(data_wide_3_all)
get_neural_model <- function(data_wide, type = "ind", crossentropy = "categorical") {
  
  path <- paste0("data/models/", type, "/", crossentropy, "/")
  n_groups_per_col <- 30
  
  ### model for close
  model_names <- c("low_pred_prob", "high_pred_prob", "close_pred_prob")
  model_paths <- paste0(path, model_names, ".feather")
  
  nn = NULL
  nn$discretization <- list(
    low = multivariate_discretization(data_wide, train_idx, test_idx, "Low_0", n_groups_per_col),
    high = multivariate_discretization(data_wide, train_idx, test_idx, "High_0", n_groups_per_col),
    close = multivariate_discretization(data_wide, train_idx, test_idx, "Close_0", n_groups_per_col)
  )
  
  if (all(map_lgl(model_paths, file.exists))) {
    nn$pred$low = read_feather(model_paths[1])
    nn$pred$high = read_feather(model_paths[2])
    nn$pred$close = read_feather(model_paths[3])
    
    if (crossentropy == "binary") {
      
      translate_cum_prob <- function(cum_prob) {
        col_names <- names(cum_prob)
        n_col <- ncol(cum_prob)
        n_row <- nrow(cum_prob)
        for (i in tail(seq_len(n_col), -1)) {
          cum_prob[[i]] <- pmin(cum_prob[[i]], cum_prob[[i-1]])
        }
        cum_prob - set_names(bind_cols(cum_prob[, head(1 + seq_len(n_col), -1)], tmp = rep(0, n_row)), col_names)
      }
      
      nn$pred$low <- translate_cum_prob(nn$pred$low)
      nn$pred$high <- translate_cum_prob(nn$pred$high)
      nn$pred$close <- translate_cum_prob(nn$pred$close)
    }
    
    
    
    return(nn)
  } else {
    stop(paste0("At least one neural network model does not exist. Please execute python code first."))
    
    labels <- tibble(
      low = nn$discretization$low %$% groups,
      high = nn$discretization$high %$% groups, 
      close = nn$discretization$close %$% groups
    )
    
    write_feather(labels[train_idx, ], paste0(path, "labels_train.feather"))
    
    data_short <- shorten_data(data_wide)
    write_feather(data_short[train_idx, ], paste0(path, "data_train.feather"))
    write_feather(select(data_wide, train_cols)[test_idx, ], paste0(path, "data_test.feather"))
    
    # Execute jupyter notebook: py/*.ipynb
  }
}


#' Remove columns from wide data that should not be used for model estimation
#'
#' @param data_wide A data.frame containing columns not usable for training or estimaton (i.e Ticker, date, etc.) plus
#' all data from window lying mostly back and the current data.
#'
#' @return
#' A data.frame only containing data necessary for training and estimation.
#' @export
#'
#' @examples
#' shorten_data(tibble(Date = 1, Ticker = "ABC", Close_3 = 100, Close_2 = 100, Open_2 = 100, Low_1 = 100, High_0 = 200))
shorten_data <- function(data_wide) {
  train_cols <- setdiff(names(data_wide), c("Ticker", "Date", "Close_0", "Low_0", "High_0"))
  max_window <- max(as.integer(str_extract(train_cols, "[0-9]+$")))
  train_cols <- train_cols[!str_detect(train_cols, paste0(max_window, "$"))]
  select(data_wide, train_cols)
}

#' Prepare data to create histogram
#'
#' @param borders A data.frame with columns ending with "lower" and "upper"
#' @param prob A vector of prbabilities for every bucket in borders.
#' @param group The name that should be added in columns group of the output
#'
#' @return A data.frame with columns "lower", "upper" and "prob". If group is given this is added in a column "group"
#' @export
#'
#' @examples
eval_hist_data <- function(borders, prob, group = NULL) {
  stopifnot(nrow(borders) == length(prob))
  borders %>%
    mutate(prob = prob) %>%
    rename_at(dplyr::vars(tidyselect::ends_with("lower")), ~"lower") %>%
    rename_at(dplyr::vars(tidyselect::ends_with("upper")), ~"upper") %>%
    select("lower", "upper", "prob") %>%
    mutate(group = group)
}


#' Plot price histogram
#' Negative and positive infinity values are replaced by extrapolating the closest bin.
#' 
#' @param data A data.frame with columns "lower", "upper" and "prob". If also an optional column "group" is included
#' several overlapping historgrams are plotted
#' @param title A character with the title
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' data <- tibble(
#'   lower = c(c(-Inf, 100, 98), c(-Inf, 100.2, 98.2)),
#'   upper = c(c(98, Inf, 100), c(98.2, Inf, 100.2)), 
#'   prob = c(c(0.3, 0.3, 0.4), c(0.2, 0.6, 0.2)),
#'   group = rep(1:2, each = 3)
#' )
#' plot_price_histogram(data, title = "Verteilung Sell-Preise")
plot_price_histogram <- function(data, title = NULL) {
  
  if ("group" %in% names(data)) {
    data <- data %>% group_by(group) %>% arrange(lower, .by_group = TRUE)
  } else {
    data <- data %>% arrange(lower)
  }
  
  lower <- data$lower
  neg_inf_idx <- which(lower == -Inf)
  lower[neg_inf_idx] <- lower[neg_inf_idx + 1] - (lower[neg_inf_idx + 2] - lower[neg_inf_idx + 1])
  data$lower <- lower
  
  upper <- data$upper
  pos_inf_idx <- which(upper == Inf)
  upper[pos_inf_idx] <- upper[pos_inf_idx - 1] + (upper[pos_inf_idx - 1] - upper[pos_inf_idx - 2])
  data$upper <- upper
  
  ## use standard breaks and labels
  breaks <- seq(from = floor(min(data$lower)), to = ceiling(max(data$lower)))
  labels <- breaks
  
  p <- ggplot(data, aes(ymin = 0))
  if ("group" %in% names(data)) {
    p <- p + geom_rect(aes(xmin = lower, xmax = upper, ymax = prob, fill = group), alpha = 0.5)
  } else {
    p <- p + geom_rect(aes(xmin = lower, xmax = upper, ymax = prob), alpha = 0.5, show.legend = FALSE)
  }
  
  p + 
    xlab("number of obs") + 
    ylab("value") +
    scale_x_continuous(breaks = breaks, labels = labels) + 
    scale_y_continuous(labels = scales::percent) + 
    ggplot2::ggtitle(title) + 
    ggplot2::xlab("Preis") +
    ggplot2::ylab("Wahrscheinlichkeit") + 
    ggplot2::scale_fill_discrete(name = "Typ") +
    theme_bw()
}


#' Plot a histogram for specific neural model prediction (typically test sample).
#'
#' @param eval_id An integer value specifying the id to plot
#' @param neural_model A neural model object with at least the elements discretization and pred.
#' @param title A character specifying the title of the plot
#'
#' @return
#' @export
#'
#' @examples
#' discretization_low <- list(borders = tibble(Bucket = c(1, 2), lower = c(101, 102), upper = c(102, 103)))
#' discretization_high <- list(borders = tibble(Bucket = c(1, 2), lower = c(103, 104), upper = c(104, 105)))
#' discretization_close <- list(borders = tibble(Bucket = c(1, 2), lower = c(102, 103), upper = c(103, 104)))
#' discretization <- list(low = discretization_low, high = discretization_high, close = discretization_close)
#' 
#' pred <- list(
#'   low = as_tibble(t(c(0.4, 0.6))), 
#'   high = as_tibble(t(c(0.3, 0.7))),
#'   close = as_tibble(t(c(0.8, 0.2)))
#' )
#' neural_model <- list(discretization = discretization, pred = pred)
#' plot_neural_sample_histogram(1, neural_model)
plot_neural_sample_histogram <- function(eval_id, neural_model, title = "Histogramm prognostizierter Tiefst-, Höchst- und Schlusspreise") {
  
  hist_data_low <- eval_hist_data(
    borders = neural_model$discretization$low$borders, 
    prob = unlist(neural_model$pred$low[eval_id, ], use.names = FALSE), 
    group = "Tief"
  )
  
  hist_data_high <- eval_hist_data(
    borders = neural_model$discretization$high$borders, 
    prob = unlist(neural_model$pred$high[eval_id, ], use.names = FALSE), 
    group = "Hoch"
  )
  
  hist_data_close <- eval_hist_data(
    borders = neural_model$discretization$close$borders, 
    prob = unlist(neural_model$pred$close[eval_id, ], use.names = FALSE), 
    group = "Schluss"
  )
  
  plot_price_histogram(
    data = bind_rows(hist_data_low, hist_data_close, hist_data_high) %>% mutate(group = factor(group, levels = unique(group))), 
    title = title
  )
}


#' Eval mid prices for discretization borders
#'
#' @param discretization A list with elements "low", "high" and "close". Each element is a list itself and has element "borders" with the data.frame with "Bucket", "lower" and "upper" column.
#'
#' @export A data.frame with columns "Bucket", "Low", "High" and "Close" with the buckets mit prices
#' @export
#'
#' @examples
#' discretization_low <- list(borders = tibble(Bucket = c(1, 2), lower = c(101, 102), upper = c(102, 103)))
#' discretization_high <- list(borders = tibble(Bucket = c(1, 2), lower = c(100, 105), upper = c(101, 106)))
#' discretization_close <- list(borders = tibble(Bucket = c(1, 2), lower = c(-Inf, 103), upper = c(300, 105)))
#' discretization <- list(low = discretization_low, high = discretization_high, close = discretization_close)
#' eval_mid_prices(discretization)
eval_mid_prices <- function(discretization) {
  bind_rows(
    mutate(discretization$low$borders, type = "Low"),
    mutate(discretization$high$borders, type = "High"),
    mutate(discretization$close$borders, type = "Close")
  ) %>% mutate(mid = (lower + upper) / 2) %>%
    select(Bucket, mid, type) %>%
    pivot_wider(names_from = type, values_from = mid)
}


#' Evaluate all crossjoin price scenarios. For not plausible values (order of values not as expected
#' Low <= Close <= High or one of the values is not finite) the whole row is set to 100. Additionally a column "Close_1" with value 100 is added.
#' 
#' @param discretization A data.frame with columns "Bucket", "Low", "High" and "Close".
#'
#' @return A data.frame with columns "Low", "High", "Close_0" and "Close_1"
#' @export 
#'
#' @examples
#' mid_prices <- tibble(Bucket = 1:2, Low = c(102, 103), High = c(100, 106), Close = c(-Inf, 104))
#' eval_price_scenarios_ind(mid_prices)
eval_price_scenarios_ind <- function(mid_prices) {
  
  # find all prices scenarios
  price_scenarios <- expand_grid(
    Low = mid_prices$Low,
    High = mid_prices$High, 
    Close = mid_prices$Close
  ) %>%
    rename(Close_0 = "Close") %>%
    mutate(Close_1 = 100)
  
  implausible_price_scenarios_idx <- which(
    price_scenarios$Close_0 < price_scenarios$Low | 
      price_scenarios$Close_0 > price_scenarios$High |
      !is.finite(price_scenarios$Low) | 
      !is.finite(price_scenarios$High) | 
      !is.finite(price_scenarios$Close_0)
  )
  plausible_price_scenarios_idx <- setdiff(seq_len(nrow(price_scenarios)), implausible_price_scenarios_idx)
  price_scenarios[implausible_price_scenarios_idx, tail(seq_len(ncol(price_scenarios)), -1)] <- 100
  price_scenarios
}

#' Evaluate all crossjoin price scenarios. For not plausible values (order of values not as expected
#' Low <= Close <= High or one of the values is not finite) the whole row is set to 100. Additionally a column "Close_1" with value 100 is added.
#' 
#' @param discretization A list of borders and groups for test data
#'
#' @return A data.frame with columns "Low", "High", "Close_0" and "Close_1"
#' @export 
#'
#' @examples
#' borders <- tibble(
#'   Bucket = 1:2, 
#'   Low_0_lower = c(102, 103),
#'   Low_0_upper = c(103, 104),
#'   High_0_lower = c(103, 106), 
#'   High_0_upper = c(104, 107),
#'   Close_0_lower = c(102, -Inf),
#'   Close_0_upper = c(103, 105)
#' )
#' eval_price_scenarios_dep(borders)
eval_price_scenarios_dep <- function(borders) {
  
  price_scenarios <- borders %>%
    transmute(
      Bucket = Bucket, 
      Low = (Low_0_lower + Low_0_upper) / 2,
      High = (High_0_lower + High_0_upper) / 2,
      Close_0 = (Close_0_lower + Close_0_upper) / 2, 
      Close_1 = 100
    )
  
  implausible_price_scenarios_idx <- which(
    price_scenarios$Close_0 < price_scenarios$Low | 
      price_scenarios$Close_0 > price_scenarios$High |
      !is.finite(price_scenarios$Low) | 
      !is.finite(price_scenarios$High) | 
      !is.finite(price_scenarios$Close_0)
  )
  plausible_price_scenarios_idx <- setdiff(seq_len(nrow(price_scenarios)), implausible_price_scenarios_idx)
  price_scenarios[implausible_price_scenarios_idx, tail(seq_len(ncol(price_scenarios)), -1)] <- 100
  price_scenarios
}


#' Eval buy sell scenarios. Implausible values (Low > High) or one of both not finite, both values of that row 
#' are set to -Inf (buy) and Inf (sell).
#'
#' @param mid_prices A data.frame with at least columns "Low" and "High"
#'
#' @return A data.frame with "buy" and "sell"
#' @export
#'
#' @examples
#' mid_prices <- tibble(Bucket = 1:2, Low = c(102, 103), High = c(100, 106), Close = c(-Inf, 104))
#' eval_buy_sell_scenarios_ind(mid_prices)
eval_buy_sell_scenarios_ind <- function(mid_prices) {
  buy_sell_combinations <- expand_grid(buy = mid_prices$Low, sell = mid_prices$High)
  implausible_buy_sell_combinations_idx <- which(
    (buy_sell_combinations$buy > buy_sell_combinations$sell) | !is.finite(buy_sell_combinations$buy) | !is.finite(buy_sell_combinations$sell)
  )
  buy_sell_combinations$buy[implausible_buy_sell_combinations_idx] <- -Inf
  buy_sell_combinations$sell[implausible_buy_sell_combinations_idx] <- Inf
  buy_sell_combinations
}


#' Eval buy sell scenarios. Implausible values (Low > High) or one of both not finite, both values of that row 
#' are set to -Inf (buy) and Inf (sell).
#'
#' @param mid_prices A data.frame with at least columns "Low" and "High"
#'
#' @return A data.frame with at least columns "Low" and "High"
#' @export
#'
#' @examples
#' mid_prices <- tibble(Bucket = 1:2, Low = c(102, 103), High = c(100, 106), Close = c(-Inf, 104))
#' eval_buy_sell_scenarios_dep(mid_prices)
eval_buy_sell_scenarios_dep <- function(price_scenarios) {
  
  buy_sell_combinations <- expand_grid(
    buy = sort(unique(price_scenarios$Low)),
    sell = sort(unique(price_scenarios$High))
  )
  
  implausible_buy_sell_combinations_idx <- which(
    (buy_sell_combinations$buy > buy_sell_combinations$sell) | !is.finite(buy_sell_combinations$buy) | !is.finite(buy_sell_combinations$sell)
  )
  buy_sell_combinations$buy[implausible_buy_sell_combinations_idx] <- -Inf
  buy_sell_combinations$sell[implausible_buy_sell_combinations_idx] <- Inf
  buy_sell_combinations
}


#' Find optimal buy sell prices for given probability distributions of price scenarios
#'
#' @param low_pred_prob A data.frame (n_entries, n_probabilites) with predicted probabilities for every low bucket
#' @param high_pred_prob A data.frame (n_entries, n_probabilites) with predicted probabilities for every high bucket
#' @param close_pred_prob A data.frame (n_entries, n_probabilites) with predicted probabilities for every close bucket
#' @param buy_first_payoffs A data.frame(n_price_scenarios = n_probabilites^3, n_buy_sell_scenarios) with payoffs for every price and bus-sell scenarios if buy is executed first
#' @param sell_first_payoffs A data.frame(n_price_scenarios = n_probabilites^3, n_buy_sell_scenarios) with payoffs for every price and bus-sell scenarios if sell is executed first
#' @param both_first A character vector with entries "buy" or "sell" indicating what actions should be performed first
#' @param mc.cores Integer with the number of cores to use in parallel computations
#'
#' @return A integer vector of optimal bus sell scenario for every entry
#' @export
#'
#' @examples
#' set.seed(1)
#' low_pred_prob <- as_tibble(t(c(0.1, 0.2, 0.3, 0.4)))
#' high_pred_prob <- as_tibble(t(c(0.2, 0.3, 0.1, 0.4)))
#' close_pred_prob <- as_tibble(t(c(0.3, 0.4, 0.2, 0.1)))
#' buy_first_payoffs <- map_dfc(seq_len(10), ~rnorm(ncol(low_pred_prob)^3))
#' sell_first_payoffs <-map_dfc(seq_len(10), ~rnorm(ncol(low_pred_prob)^3))
#' both_first <- "buy"
#' find_optimal_buy_sell_idx_ind(low_pred_prob, high_pred_prob, close_pred_prob, buy_first_payoffs, sell_first_payoffs, both_first, 1)
find_optimal_buy_sell_idx_ind <- function(low_pred_prob, high_pred_prob, close_pred_prob, buy_first_payoffs, sell_first_payoffs, both_first, mc.cores = getOption("mc.cores", 2L)) {
  
  stopifnot(length(unique(c(nrow(low_pred_prob), nrow(high_pred_prob), nrow(close_pred_prob)))) == 1)
  stopifnot(length(both_first) == nrow(low_pred_prob))
  
  find_best_buy_sell_parallel <- function(probs, buy_first_payoffs_mat, sell_first_payoffs_mat) {
    find_best_buy_sell_ind(
      low_prob = probs$low_pred_prob_mat,
      high_prob = probs$high_pred_prob_mat,
      close_prob = probs$close_pred_prob_mat,
      buy_first_payoffs = buy_first_payoffs_mat, 
      sell_first_payoffs = sell_first_payoffs_mat, 
      both_first = both_first
    )
  }
  
  chunk2 <- function(x,n) {
    if (n == 1) return(x)
    split(x, cut(seq_along(x), min(n, length(x)), labels = FALSE))
  }
  
  low_pred_prob_mat <- as.matrix(low_pred_prob)
  high_pred_prob_mat <- as.matrix(high_pred_prob)
  close_pred_prob_mat <- as.matrix(close_pred_prob)
  
  split_probs <- map(
    .x = chunk2(seq_len(nrow(low_pred_prob)), mc.cores), 
    .f = ~list(
      low_pred_prob_mat = low_pred_prob_mat[., , drop = FALSE], 
      high_pred_prob_mat = high_pred_prob_mat[., ,drop = FALSE], 
      close_pred_prob_mat = close_pred_prob_mat[., ,drop = FALSE]
    )
  )
  pbmclapply(
    X = split_probs,
    FUN = find_best_buy_sell_parallel, 
    buy_first_payoffs_mat = as.matrix(buy_first_payoffs), 
    sell_first_payoffs_mat = as.matrix(sell_first_payoffs),
    mc.cores = mc.cores
  ) %>% unlist(use.names = FALSE)
}



find_optimal_buy_sell_idx_dep <- function(pred_prob, buy_first_payoffs, sell_first_payoffs, both_first, mc.cores = getOption("mc.cores", 2L)) {
  
  # TODO: comment in
  # stopifnot(length(both_first) == nrow(pred_prob))
  
  find_best_buy_sell_parallel <- function(probs_mat, buy_first_payoffs_mat, sell_first_payoffs_mat) {
    find_best_buy_sell_dep(
      probs_mat,
      buy_first_payoffs = buy_first_payoffs_mat, 
      sell_first_payoffs = sell_first_payoffs_mat, 
      both_first = both_first
    )
  }
  
  chunk2 <- function(x,n) {
    if (n == 1) return(x)
    split(x, cut(seq_along(x), min(n, length(x)), labels = FALSE))
  }
  
  pred_prob_mat <- as.matrix(pred_prob)
  
  split_probs <- map(
    .x = chunk2(seq_len(nrow(pred_prob)), mc.cores), 
    .f = ~pred_prob_mat[., , drop = FALSE] 
  )
  
  pbmclapply(
    X = split_probs,
    FUN = find_best_buy_sell_parallel, 
    buy_first_payoffs_mat = as.matrix(buy_first_payoffs), 
    sell_first_payoffs_mat = as.matrix(sell_first_payoffs),
    mc.cores = mc.cores
  ) %>% unlist(use.names = FALSE)
}



#' Predict optimal buy sell prices for test set
#'
#' @param neural_model A neural model object with the elements discretization and pred.
#' @param data_wide A data.frame with wide data
#' @param both_first A character vector with entries "buy" or "sell" indication which action should be daone first
#' @param test_idx A vector of indices for the test set
#' @param sample_idx A vecor of sub indices of the test set. This is useful if for testing purposes only some entries
#' should be evaluated
#' @param mc.cores A integer indicating how many cores should be used during parallel computation
#'
#' @return
#' A data.frame with columns buy and sell representing the optimal buy and sell prices for each entry in the test set.
#' @export
#'
#' @examples
find_optimal_buy_sell_ind <- function(neural_model, data_wide, both_first, test_idx, sample_idx = seq_along(test_idx), mc.cores = getOption("mc.cores", 2L)) {
  
  stopifnot(length(unique(c(
    length(test_idx), 
    nrow(neural_model$pred$low),
    nrow(neural_model$pred$high),
    nrow(neural_model$pred$close)
  ))) == 1)
  
  mid_prices <- eval_mid_prices(neural_model$discretization)
  price_scenarios <- eval_price_scenarios_ind(mid_prices)
  buy_sell_scenarios <- eval_buy_sell_scenarios_ind(mid_prices)
  
  buy_first_payoffs <- map2_dfc(
    .x = buy_sell_scenarios$buy, 
    .y = buy_sell_scenarios$sell, 
    .f = ~calc_payoff_const_gamma(price_scenarios, buy = .x, sell = .y, both_first = "buy")
  )
  
  sell_first_payoffs <- map2_dfc(
    .x = buy_sell_scenarios$buy, 
    .y = buy_sell_scenarios$sell, 
    .f = ~calc_payoff_const_gamma(price_scenarios, buy = .x, sell = .y, both_first = "sell")
  )
  
  optimal_buy_sell_idx <- find_optimal_buy_sell_idx_ind(
    low_pred_prob = neural_model$pred$low[sample_idx, ],
    high_pred_prob = neural_model$pred$high[sample_idx, ],
    close_pred_prob = neural_model$pred$close[sample_idx, ],
    buy_first_payoffs = buy_first_payoffs,
    sell_first_payoffs = sell_first_payoffs,
    both_first = both_first[test_idx][sample_idx],
    mc.cores = 3
  )
  
  buy_sell_scenarios[optimal_buy_sell_idx, ]
}

#' Predict optimal buy sell prices for test set
#'
#' @param neural_model A neural model object with the elements discretization and pred.
#' @param data_wide A data.frame with wide data
#' @param both_first A character vector with entries "buy" or "sell" indication which action should be daone first
#' @param test_idx A vector of indices for the test set
#' @param sample_idx A vecor of sub indices of the test set. This is useful if for testing purposes only some entries
#' should be evaluated
#' @param mc.cores A integer indicating how many cores should be used during parallel computation
#'
#' @return
#' A data.frame with columns buy and sell representing the optimal buy and sell prices for each entry in the test set.
#' @export
#'
#' @examples
find_optimal_buy_sell_dep <- function(neural_model, data_wide, both_first, test_idx, sample_idx = seq_along(test_idx), mc.cores = getOption("mc.cores", 2L)) {
  
  # stopifnot(length(unique(c(
  #   length(test_idx), 
  #   nrow(neural_model$pred)
  # ))) == 1)
  
  price_scenarios <- eval_price_scenarios_dep(neural_model$discretization$borders)
  buy_sell_scenarios <- eval_buy_sell_scenarios_dep(price_scenarios)
  
  buy_first_payoffs <- map2_dfc(
    .x = buy_sell_scenarios$buy, 
    .y = buy_sell_scenarios$sell, 
    .f = ~calc_payoff_const_gamma(price_scenarios, buy = .x, sell = .y, both_first = "buy")
  )
  
  sell_first_payoffs <- map2_dfc(
    .x = buy_sell_scenarios$buy, 
    .y = buy_sell_scenarios$sell, 
    .f = ~calc_payoff_const_gamma(price_scenarios, buy = .x, sell = .y, both_first = "sell")
  )
  
  optimal_buy_sell_idx <- find_optimal_buy_sell_idx_dep(
    pred_prob = neural_model$pred,
    buy_first_payoffs = buy_first_payoffs,
    sell_first_payoffs = sell_first_payoffs,
    both_first = both_first[test_idx][sample_idx],
    mc.cores = 3
  )
  
  buy_sell_scenarios[optimal_buy_sell_idx, ]
}

