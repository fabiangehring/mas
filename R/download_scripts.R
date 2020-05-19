#' Download stock data
#'
#' @return
#' Function does not return a value but is used because of its sideeffect of saving a dataframe "stocks" into the 
#' data folder containing all listed equity constitutents of the iShares MSCI World ETF as of 02.03.2020.
#' @export
#'
#' @examples
#' download_stocks()
download_stocks <- function() {
  
  # get listed equity constiutents of big msci world etf
  url <- "https://www.ishares.com/ch/individual/en/products/251881/ishares-msci-world-ucits-etf-inc-fund/1495092304805.ajax?tab=all&fileType=json&asOfDate=20200302&_=1584800705312"
  raw_json <- jsonlite::fromJSON(url)
  col_names <- c("Ticker", "Name", "Asset Class", "Weight (%)", "Price", "Shares", "Market Value", "Notional Value", "Sector", "ISIN", "Exchange", "Location", "Market Currency")
  constituents <- raw_json %$% aaData %>% purrr::map(~rlang::set_names(purrr::map(., ~last(.)), col_names)) %>% bind_rows()
  stocks <- dplyr::filter(constituents, `Asset Class` == "Equity" & Exchange != "NO MARKET (E.G. UNLISTED)")
  return(stocks)
}


#' Download stock data
#'
#' @param stocks A data.frame with the columns "Ticker" and "Exchange" of the stocks to download.
#'
#' @return
#' Downloads the history of daily available quote for the given stocks from yahoo finance until 02.03.2020 and safes them to the data folder.
#' @export
#'
#' @examples
#' # Not Run
#' # download_quotes(tibble(Ticker = "AAPL", Exchange = "NASDAQ"))
download_quotes <- function(stocks) {
  
  # fetch prices from yahoo finance
  quotes <- new.env()
  Symbols <- unique(eval_yahoo_ticker(stocks$Ticker, stocks$Exchange))
  quantmod::getSymbols(Symbols, env = quotes, auto.assign = TRUE, src = 'yahoo', from = '1950-01-01', to = as.Date("2020-03-02"), verbose=TRUE)
  
  # consolidate data into one single data.frame
  quotes <- as.list(quotes) %>%
    purrr::map2(., names(.), function(x, y) {
      tibble::as_tibble(x) %>%
        set_names(c("Open", "High", "Low", "Close", "Volume", "Adjusted")) %>%
        mutate(Date = zoo::index(x), Ticker = y)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::select("Ticker", "Date", "Open", "Low", "High", "Close", "Adjusted", "Volume")
  
  return(quotes)
}


#' Evaluate correct Yahoo Finance ticker given iShares ticker and exchange information
#'
#' @param ishares_ticker 
#' @param exchange 
#'
#' @return
#' @export
#'
#' @examples
#' eval_yahoo_ticker("NESN", "Six Swiss Exchange Ag")
eval_yahoo_ticker <- function(ishares_ticker, exchange) {
  
  # Remove dots at the end of ticker
  ticker <- str_remove(ishares_ticker, "\\.$")
  
  # Replace dots in the middle of ticker by dash
  ticker <- str_replace_all(ticker, "\\.", "-")
  
  # numeric tickers must be at least 4 characters long (i.e. Hong Kong tickers)
  short_number_bool <- str_detect(ticker, "^[0-9]{0,3}$")
  ticker[short_number_bool] <- str_pad(ticker[short_number_bool], 4, pad = "0")
  
  # add exchange ending if needed
  paste0(str_replace_all(ticker, " ", "-"), get_ticker_ending(exchange))
}


#' Find ticker ending for specific exchange
#'
#' @param exchange 
#'
#' @return
#' @export
#'
#' @examples
#' get_ticker_ending(location = constituents$Exchange)
get_ticker_ending <- function(exchange) {
  market_endings <- list(
    `Asx - All Markets` = ".AX",
    `Bolsa De Madrid` = ".MC",
    `Borsa Italiana` = ".MI",
    `Cboe BZX formerly known as BATS` = "",
    `Euronext Amsterdam` = ".AS",
    `Hong Kong Exchanges And Clearing Ltd` = ".HK",
    `Irish Stock Exchange - All Market` = ".IR",
    `London Stock Exchange` = ".L",      
    `NASDAQ` = "",                              
    `Nasdaq Omx Helsinki Ltd.` = ".HE",           
    `Nasdaq Omx Nordic` = ".ST",
    `New York Stock Exchange Inc.` = "",
    `New Zealand Exchange Ltd` = ".NZ",      
    `Nyse Euronext - Euronext Brussels` = ".BR",   
    `Nyse Euronext - Euronext Lisbon` = ".LS",     
    `Nyse Euronext - Euronext Paris`= ".PA",      
    `Nyse Mkt Llc` = "",                        
    `Omx Nordic Exchange Copenhagen A/S` = ".CO",
    `Oslo Bors Asa` = ".OL",                    
    `Singapore Exchange` = ".SI",              
    `SIX Swiss Exchange` = ".SW",
    `Six Swiss Exchange Ag` = ".SW",         
    `Tel Aviv Stock Exchange` = ".TA",          
    `Tokyo Stock Exchange` = ".T",            
    `Toronto Stock Exchange` = ".TO",         
    `Wiener Boerse Ag` = ".VI",      
    `Xetra` = ".DE"
  )
  
  if (any(!exchange %in% names(market_endings))) {
    stop("Unknown exchange detected.")
  }
  
  unlist(market_endings[exchange], use.names = FALSE)
}


