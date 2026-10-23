# Functions to load stock data and calculate basic statistics.

library(quantmod)
library(TTR)

# Reads portfolio.txt and returns the stock symbols as a character vector.
read_portfolio <- function(file = "portfolio.txt") {
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols[symbols != ""]
}
# Returns a named list with one data frame per stock.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(file)
  stock_list <- list()

  for (sym in symbols) {
    df <- tryCatch({
      xts_data <- getSymbols(sym, src = "yahoo", from = from, to = to,
                             auto.assign = FALSE)
      out <- data.frame(Date = index(xts_data), coredata(xts_data))
      colnames(out) <- c("Date", "Open", "High", "Low", "Close",
                         "Volume", "Adjusted")
      out
    }, error = function(e) {
      message("Could not load ", sym, ": ", conditionMessage(e))
      NULL
    })

    if (!is.null(df)) {
      stock_list[[sym]] <- df
    }
  }

  stock_list
}

# Mode = the value that shows up most often.
# Closing prices almost never repeat exactly, so I round them to whole
# dollars first. Otherwise every price would appear once.
get_mode <- function(x) {
  x <- round(x[!is.na(x)])
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Takes ONE stock data frame and calculates the basic statistics.
# Returns a list with:
#   summary = one-row data frame (mean, median, mode, sd, last moving average)
#   data    = the original data frame plus a moving average column
calculate_statistics <- function(df, ma_days = 20) {
  close_prices <- df$Close

  # if there are fewer rows than ma_days, shrink the window so SMA() does not fail
  if (nrow(df) < ma_days) {
    ma_days <- nrow(df)
  }

  moving_avg <- SMA(close_prices, n = ma_days)
  df$MovingAvg <- moving_avg

  summary_row <- data.frame(
    Mean      = mean(close_prices, na.rm = TRUE),
    Median    = median(close_prices, na.rm = TRUE),
    Mode      = get_mode(close_prices),
    StdDev    = sd(close_prices, na.rm = TRUE),
    LastMA    = tail(moving_avg[!is.na(moving_avg)], 1),
    MA_Days   = ma_days
  )

  list(summary = summary_row, data = df)
}

# Runs calculate_statistics() on every stock in the list.
# Returns a list with:
#   table = one row per stock (easy to read)
#   data  = list of data frames that now include the moving average
calculate_all_statistics <- function(stock_list, ma_days = 20) {
  results <- lapply(stock_list, calculate_statistics, ma_days = ma_days)

  stats_table <- do.call(rbind, lapply(results, function(r) r$summary))
  stats_table <- cbind(Symbol = names(results), stats_table)
  rownames(stats_table) <- NULL

  list(table = stats_table,
       data = lapply(results, function(r) r$data))
}
