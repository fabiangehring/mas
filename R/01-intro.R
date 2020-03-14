#' Visualize the incompleteness of a delta hedge for a at the money (atm) european option.
#'
#' @param t initial or current time t (in years).
#' @param S stock price at time t.
#' @param r risk-free annual interest rate.
#' @param sigma annual volatility (standard deviation).
#' @param K strike.
#' @param T maturity (in years).
#' @param move percentage move of stock price.
#' @param type character string indicating whether a call (the default) or a put option is considered.
#'
#' @return
#' @export
#'
#' @examples
#' par(mfrow = c(1, 2))
#' plot_call_price(t = 0, S = seq(5, 15, 0.01), r = 0.01, sigma = 0.5, K = 10, T = 0.5, move = 0.2, type = "call")
#' plot_call_price(t = 0, S = seq(5, 15, 0.01), r = 0.01, sigma = 0.5, K = 10, T = 0.5, move = 0.2, type = "put")
#' par(mfrow = c(1, 1))
plot_call_price <- function(t, S, r, sigma, K, T, move, type = c("call", "put")) {
  option_price <- qrmtools::Black_Scholes(t, S, r, sigma, K, T, type)
  
  plot(
    x = S,
    y = option_price,
    main = paste0(stringr::str_to_title(type,), " Option"),
    type = "l",
    xlab = "Underlying Price",
    ylab = "Option Price",
    lty = "solid"
  )
  points(K, qrmtools::Black_Scholes(t, K, r, sigma, K, T, type), pch = 4)
  # lines(S, pmax(0, S-K), lty  = "dashed")
  
  # tangent of atm option
  atm_price <- qrmtools::Black_Scholes(t, K, r, sigma, K, T, type)
  slope <- qrmtools::Black_Scholes_Greeks(t, K, r, sigma, K, T, type)["delta"]
  intercept <- atm_price - slope * K
  tangent_x <- S
  tangent_y <- intercept + tangent_x * slope
  
  in_plot <- tangent_y >= min(option_price) & tangent_y <= max(option_price)
  lines(tangent_x[in_plot], tangent_y[in_plot], lty = "dotted")
  
  # price deviation from tangent
  price_down <- qrmtools::Black_Scholes(t, K * (1 - move), r, sigma, K, T, type)
  tangent_down <- intercept + K * (1 - move) * slope
  lines(rep(K * (1 - move), 2), c(price_down, tangent_down), col = "red", lwd = 2)
  
  price_up <- qrmtools::Black_Scholes(t, K * (1 + move), r, sigma, K, T, type)
  tangent_up <- intercept + K * (1 + move) * slope
  lines(rep(K * (1 + move), 2), c(price_up, tangent_up), col = "red", lwd = 2)
}



#' 15min intraday prices of UBSG on 2020-03-13, source swissquote.ch (previous_day_close: 7.902)
#'
#' @return A plot of the intraday prices
#' @export
#'
#' @examples
#' ubsg_20200313_intraday()
ubsg_20200313_intraday <- function() {
intraday_prices <- tribble(
    ~Time, ~Price,
    "09:00", 8.25,
    "09:15", 8.06,
    "09:30", 8.03,
    "09:45", 7.92,
    "10:00", 7.93,
    "10:15", 7.92,
    "10:30", 7.97,
    "10:45", 8.06,
    "11:00", 8.15,
    "11:15", 8.21,
    "11:30", 8.16,
    "11:45", 8.22,
    "12:00", 8.26,
    "12:15", 8.46,
    "12:30", 8.44,
    "12:45", 8.53,
    "13:00", 8.54,
    "13:15", 8.63,
    "13:30", 8.84,
    "13:45", 8.77,
    "14:00", 8.73,
    "14:15", 8.74,
    "14:30", 8.66,
    "14:45", 8.50,
    "15:00", 8.38,
    "15:15", 8.47,
    "15:30", 8.51,
    "15:45", 8.36,
    "16:00", 8.20,
    "16:15", 8.17,
    "16:30", 8.02,
    "16:45", 7.97,
    "17:00", 8.05,
    "17:15", 7.99,
    "17:30", 8.05
  ) %>% mutate(Time = paste("2020-03-13", Time) %>% as.POSIXct())
  plot(intraday_prices, main = "Aktienkurs UBS am 13.03.2020", type = "o", xlab = "Zeit", ylab = "Preis (in CHF)")
}

