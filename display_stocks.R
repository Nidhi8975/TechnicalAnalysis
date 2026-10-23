
# Set the working directory to this folder before running.

source("technical_analysis_functions.R")

stocks  <- load_stock_data("portfolio.txt")
results <- calculate_all_statistics(stocks, ma_days = 20)

display_data_frames <- function(stock_list, n = 5) {
  for (sym in names(stock_list)) {
    cat("\n=====", sym, "=====\n")
    cat("Rows:", nrow(stock_list[[sym]]), "\n")
    cat("First", n, "rows:\n")
    print(head(stock_list[[sym]], n))
    cat("Last", n, "rows:\n")
    print(tail(stock_list[[sym]], n))
  }
}

display_quote <- function(stock_list) {
  for (sym in names(stock_list)) {
    df <- stock_list[[sym]]
    last_row <- df[nrow(df), ]
    prev_row <- df[nrow(df) - 1, ]
    change <- last_row$Close - prev_row$Close
    pct <- change / prev_row$Close * 100
    cat(sprintf("%-5s  Close: %8.2f   Change: %+7.2f (%+.2f%%)   Volume: %s\n",
                sym, last_row$Close, change, pct,
                format(last_row$Volume, big.mark = ",")))
  }
}

# Way 3: line chart of closing price with the moving average on top
plot_stock <- function(df, symbol) {
  plot(df$Date, df$Close, type = "l", col = "steelblue", lwd = 2,
       main = paste(symbol, "- Closing Price and Moving Average"),
       xlab = "Date", ylab = "Price (USD)")
  lines(df$Date, df$MovingAvg, col = "red", lwd = 2)
  legend("topleft", legend = c("Close", "Moving average"),
         col = c("steelblue", "red"), lty = 1, lwd = 2, bty = "n")
}

# Way 4: candlestick chart (the style Yahoo Finance uses)
plot_candles <- function(symbol) {
  xts_data <- getSymbols(symbol, src = "yahoo", from = Sys.Date() - 120,
                         auto.assign = FALSE)
  chartSeries(xts_data, name = paste(symbol, "Candlestick (last 120 days)"),
              theme = chartTheme("white"), TA = "addSMA(n=20)")
}

# ---- 3. Show the results ----
display_data_frames(stocks)

cat("\n===== Quick quotes =====\n")
display_quote(stocks)

cat("\n===== Statistics (closing price) =====\n")
print(results$table, digits = 4, row.names = FALSE)

# one chart per stock
for (sym in names(results$data)) {
  plot_stock(results$data[[sym]], sym)
}

# one candlestick chart (change the symbol if you want)
plot_candles("AAPL")
