# Portfolio dashboard built with R Shiny
# STEP 1: DATA COLLECTION AND SETUP
# Packages (install once with install.packages(c("shiny", "ggplot2", "quantmod")))
# quantmod also loads TTR, which I use for SMA(), RSI() and MACD().
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# Data source: Yahoo Finance through quantmod. I also added two backup sources
# so the app still works if Yahoo is blocked or the symbol fails:
#   - Upload CSV file
#   - Demo data (offline)
data_sources <- c("Yahoo Finance", "Upload CSV file", "Demo data (offline)")

# Stock symbols for the dropdown. Reads portfolio.txt 
portfolio <- c("AAPL", "MSFT", "GOOG", "AMZN", "NVDA")
if (file.exists("portfolio.txt")) {
  from_file <- trimws(readLines("portfolio.txt", warn = FALSE))
  from_file <- from_file[from_file != ""]
  if (length(from_file) > 0) portfolio <- from_file
}
# Stops with a clear message if something is missing.
standardize_ohlc <- function(df) {
  names(df) <- tolower(trimws(names(df)))
  needed <- c("date", "open", "high", "low", "close")
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("The data is missing these columns: ", paste(missing_cols, collapse = ", "))
  }
  if (!("volume" %in% names(df))) df$volume <- 0

  out <- data.frame(
    Date   = as.Date(df$date),
    Open   = as.numeric(df$open),
    High   = as.numeric(df$high),
    Low    = as.numeric(df$low),
    Close  = as.numeric(df$close),
    Volume = as.numeric(df$volume)
  )
  out <- out[!is.na(out$Date) & !is.na(out$Close) & !is.na(out$Open) &
             !is.na(out$High) & !is.na(out$Low), ]
  if (nrow(out) < 2) stop("Not enough valid rows in the data.")
  out[order(out$Date), ]
}

# Download data from Yahoo Finance with quantmod.
fetch_yahoo <- function(symbol, start_date, end_date) {
  symbol <- toupper(trimws(symbol))
  if (symbol == "") stop("Please enter a stock symbol.")
  raw <- tryCatch(
    getSymbols(symbol, src = "yahoo", from = start_date - 1500, to = end_date,
               auto.assign = FALSE),
    error = function(e) {
      stop("Could not download ", symbol, " from Yahoo Finance (", conditionMessage(e),
           "). Check the symbol and your internet, or try another data source.")
    }
  )
  df <- data.frame(Date = index(raw), coredata(raw))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  standardize_ohlc(df)
}

# Made-up price data that does not need the internet.
# It is a formula (a trend plus two waves), so it is the same every time.
make_demo <- function() {
  d <- seq(as.Date("2023-01-02"), as.Date("2024-06-28"), by = "day")
  d <- d[!(as.POSIXlt(d)$wday %in% c(0, 6))]            # keep weekdays only
  i <- seq_along(d)
  close  <- 100 + 0.04 * i + 8 * sin(i / 12) + 3 * sin(i / 3.3)
  open   <- c(close[1] - 0.5, close[-length(close)])
  high   <- pmax(open, close) + 1 + 0.4 * abs(sin(i))
  low    <- pmin(open, close) - 1 - 0.4 * abs(sin(i / 2))
  volume <- round(1e6 + 2e5 * sin(i / 5))
  data.frame(Date = d, Open = open, High = high, Low = low, Close = close, Volume = volume)
}

# Turn daily data into weekly or monthly bars.
# Open = first open, High = highest high, Low = lowest low,
# Close = last close, Volume = total volume.
aggregate_ohlc <- function(df, time_frame) {
  if (time_frame == "Daily") return(df)
  key <- as.Date(cut(df$Date, ifelse(time_frame == "Weekly", "week", "month")))
  pieces <- split(df, key)
  out <- do.call(rbind, lapply(pieces, function(p) {
    data.frame(Date = p$Date[nrow(p)], Open = p$Open[1], High = max(p$High),
               Low = min(p$Low), Close = p$Close[nrow(p)], Volume = sum(p$Volume))
  }))
  rownames(out) <- NULL
  out
}
# STEP 3: TECHNICAL INDICATORS
# Adds the indicator columns to the data frame:
#   RSI                    relative strength index (0 to 100)
#   MACD, MACDSignal, MACDHist
# If an indicator cannot be calculated (for example not enough rows) it is
# filled with NA instead of crashing the app.
add_indicators <- function(df, short_n, long_n, rsi_n, macd_fast, macd_slow, macd_sig) {
  n <- nrow(df)
  safe <- function(expr) tryCatch(as.numeric(expr), error = function(e) rep(NA_real_, n))

  df$ShortMA <- safe(SMA(df$Close, n = short_n))
  df$LongMA  <- safe(SMA(df$Close, n = long_n))
  df$RSI     <- safe(RSI(df$Close, n = rsi_n))

  m <- tryCatch(
    MACD(df$Close, nFast = macd_fast, nSlow = macd_slow, nSig = macd_sig,
         maType = "EMA", percent = FALSE),     # percent = FALSE gives the usual MACD
    error = function(e) NULL
  )
  if (is.null(m)) {
    df$MACD <- NA_real_
    df$MACDSignal <- NA_real_
  } else {
    df$MACD <- as.numeric(m[, "macd"])
    df$MACDSignal <- as.numeric(m[, "signal"])
  }
  df$MACDHist <- df$MACD - df$MACDSignal
  df
}
# STEP 4 : TRADING RULES

# Moving average crossover rule:
#   Buy  = the short MA crosses ABOVE the long MA on this bar
#   Sell = the short MA crosses BELOW the long MA on this bar
#   Hold = anything else
# Optional RSI filter: ignore a Buy if RSI is already at or above rsi_limit.
make_signals <- function(df, use_rsi_filter = FALSE, rsi_limit = 70) {
  n <- nrow(df)
  signal <- rep("Hold", n)

  if (n >= 2) {
    for (i in 2:n) {
      s_prev <- df$ShortMA[i - 1]; l_prev <- df$LongMA[i - 1]
      s_now  <- df$ShortMA[i];     l_now  <- df$LongMA[i]
      if (anyNA(c(s_prev, l_prev, s_now, l_now))) next      # not enough history yet

      if (s_now > l_now && s_prev <= l_prev) {
        too_hot <- use_rsi_filter && !is.na(df$RSI[i]) && df$RSI[i] >= rsi_limit
        if (!too_hot) signal[i] <- "Buy"
      } else if (s_now < l_now && s_prev >= l_prev) {
        signal[i] <- "Sell"
      }
    }
  }

  df$Signal <- signal
  df
}
# STEP 2: SHINY APP - USER INTERFACE AND WIDGETS

ui <- fluidPage(
  titlePanel("Portfolio Dashboard - Technical Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Data"),
      selectInput("source", "Data source:", choices = data_sources),
      conditionalPanel(
        condition = "input.source == 'Yahoo Finance'",
        selectizeInput("symbol", "Stock symbol (pick one or type your own):",
                       choices = portfolio, selected = portfolio[1],
                       options = list(create = TRUE))
      ),
      conditionalPanel(
        condition = "input.source == 'Upload CSV file'",
        fileInput("csv_file", "Choose a CSV file", accept = ".csv"),
        helpText("Needs columns: Date, Open, High, Low, Close (Volume is optional).")
      ),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart type:",
                  choices = c("Candlestick", "Line", "Area")),

      hr(),
      h4("Technical indicators"),
      checkboxGroupInput("indicators", "Show (turn each on or off):",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      sliderInput("short_n", "Short moving average (periods):", min = 5, max = 50, value = 20),
      sliderInput("long_n",  "Long moving average (periods):", min = 20, max = 100, value = 50),
      numericInput("rsi_n", "RSI period:", value = 14, min = 2, max = 50),
      fluidRow(
        column(4, numericInput("macd_fast", "MACD fast", value = 12, min = 2)),
        column(4, numericInput("macd_slow", "MACD slow", value = 26, min = 3)),
        column(4, numericInput("macd_sig",  "MACD signal", value = 9, min = 2))
      ),

      hr(),
      h4("Trading rule"),
      checkboxInput("show_signals", "Show Buy / Sell annotations", value = TRUE),
      checkboxInput("show_hold", "Also label Hold bars", value = FALSE),
      checkboxInput("use_rsi_filter", "Skip Buy signals when RSI is high", value = FALSE),
      numericInput("rsi_limit", "RSI limit:", value = 70, min = 50, max = 100)
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Chart",
          plotOutput("stock_chart", height = "420px"),
          conditionalPanel(
            condition = "input.indicators && input.indicators.indexOf('RSI') > -1",
            plotOutput("rsi_chart", height = "180px")
          ),
          conditionalPanel(
            condition = "input.indicators && input.indicators.indexOf('MACD') > -1",
            plotOutput("macd_chart", height = "200px")
          )
        ),
        tabPanel("Signals",
          br(),
          textOutput("signal_summary"),
          br(),
          tableOutput("signal_table")
        ),
        tabPanel("About the rules",
          br(),
          p("Moving average crossover rule:"),
          tags$ul(
            tags$li("Buy: the short moving average crosses above the long moving average."),
            tags$li("Sell: the short moving average crosses below the long moving average."),
            tags$li("Hold: no cross on that bar.")
          ),
          p("You can change both moving average lengths and turn on the RSI filter, which skips a Buy when RSI is at or above the limit."),
          p("RSI and MACD use a different scale from price, so they are shown in their own panels under the price chart and share the same dates."),
          p("This is a class project and not investment advice.")
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {

  # Step 1: get the data 

  # CSV file the user uploaded
  csv_data <- reactive({
    req(input$csv_file)
    standardize_ohlc(read.csv(input$csv_file$datapath, stringsAsFactors = FALSE))
  })

  # When the source changes, move the date range to something that makes sense
  observeEvent(input$source, {
    if (input$source == "Demo data (offline)") {
      updateDateRangeInput(session, "date_range",
                           start = as.Date("2023-01-02"), end = as.Date("2024-06-28"))
    } else if (input$source == "Yahoo Finance") {
      updateDateRangeInput(session, "date_range",
                           start = Sys.Date() - 365, end = Sys.Date())
    }
  }, ignoreInit = TRUE)

  # When a CSV is uploaded, show the dates that are in the file
  observeEvent(input$csv_file, {
    df <- tryCatch(csv_data(), error = function(e) NULL)
    if (!is.null(df)) {
      updateDateRangeInput(session, "date_range", start = min(df$Date), end = max(df$Date))
    }
  })

  # The raw daily data from whichever source is selected.
  # Errors are caught and shown as a friendly message on the page.
  raw_data <- reactive({
    result <- tryCatch({
      switch(input$source,
        "Yahoo Finance"       = fetch_yahoo(input$symbol, input$date_range[1], input$date_range[2]),
        "Upload CSV file"     = csv_data(),
        "Demo data (offline)" = make_demo()
      )
    }, error = function(e) e)

    validate(need(!inherits(result, "error"),
                  if (inherits(result, "error")) conditionMessage(result) else ""))
    result
  })
  # Aggregate to the chosen time frame, add indicators, then add signals.
  # This is done on ALL the downloaded data first so the moving averages are
  # ready on the first day of the chart.
  processed <- reactive({
    validate(
      need(input$short_n < input$long_n, "The short moving average must be smaller than the long one."),
      need(input$macd_fast < input$macd_slow, "MACD fast must be smaller than MACD slow."),
      need(!is.na(input$rsi_n) && !is.na(input$macd_fast) && !is.na(input$macd_slow) &&
           !is.na(input$macd_sig) && !is.na(input$rsi_limit), "Please fill in every indicator number.")
    )
    df <- aggregate_ohlc(raw_data(), input$time_frame)
    df <- add_indicators(df, input$short_n, input$long_n, input$rsi_n,
                         input$macd_fast, input$macd_slow, input$macd_sig)
    make_signals(df, input$use_rsi_filter, input$rsi_limit)
  })

  # Only the dates the user asked to see
  view <- reactive({
    df <- processed()
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) >= 2, "No data in this date range. Try a different range or data source."))
    df
  })

  title_text <- reactive({
    name <- switch(input$source,
                   "Yahoo Finance" = toupper(input$symbol),
                   "Upload CSV file" = "Uploaded file",
                   "Demo data (offline)" = "Demo data")
    paste(name, "-", input$time_frame, input$chart_type, "chart")
  })

  # Step 2 + 3 + 4: price chart

  output$stock_chart <- renderPlot({
    df <- view()
    show_ma <- "Moving Averages" %in% input$indicators

    # work out the y range first, so Buy/Sell labels fit inside the chart
    y_values <- c(df$Low, df$High)
    if (show_ma) y_values <- c(y_values, df$ShortMA, df$LongMA)
    y_range <- range(y_values, na.rm = TRUE)
    off <- 0.04 * diff(y_range)                      # distance used for labels

    p <- ggplot(df, aes(x = Date))

    #Step 2: chart types
    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), color = "steelblue", linewidth = 0.8)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "steelblue", alpha = 0.25) +
        geom_line(aes(y = Close), color = "steelblue", linewidth = 0.8)
    } else {
      df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
      gap <- median(as.numeric(diff(df$Date)))
      half <- 0.3 * gap                                 # half the width of a candle
      p <- p +
        geom_segment(data = df, aes(x = Date, xend = Date, y = Low, yend = High, color = Direction)) +
        geom_rect(data = df, aes(xmin = Date - half, xmax = Date + half,
                                 ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                                 fill = Direction)) +
        scale_color_manual(values = c(Up = "darkgreen", Down = "red"), guide = "none") +
        scale_fill_manual(values = c(Up = "darkgreen", Down = "red"), guide = "none")
    }

    # Step 3: moving averages are drawn on top of the price chart
    subtitle_text <- NULL
    if (show_ma) {
      p <- p +
        geom_line(aes(y = ShortMA), color = "orange", linewidth = 0.9, na.rm = TRUE) +
        geom_line(aes(y = LongMA),  color = "purple", linewidth = 0.9, na.rm = TRUE)
      subtitle_text <- paste0("Orange = short MA (", input$short_n, "), purple = long MA (",
                              input$long_n, ")")
    }

    #Step 4: annotate the bars with the signals
    if (input$show_signals) {
      buys  <- df[df$Signal == "Buy", ]
      sells <- df[df$Signal == "Sell", ]
      if (nrow(buys) > 0) {
        p <- p +
          geom_point(data = buys, aes(x = Date, y = Low - off), shape = 17,
                     color = "darkgreen", size = 3.5) +
          geom_text(data = buys, aes(x = Date, y = Low - 2.2 * off, label = Signal),
                    color = "darkgreen", fontface = "bold", size = 3.5)
      }
      if (nrow(sells) > 0) {
        p <- p +
          geom_point(data = sells, aes(x = Date, y = High + off), shape = 25,
                     fill = "red", color = "red", size = 3.5) +
          geom_text(data = sells, aes(x = Date, y = High + 2.2 * off, label = Signal),
                    color = "red", fontface = "bold", size = 3.5)
      }
      if (input$show_hold) {
        holds <- df[df$Signal == "Hold", ]
        p <- p + geom_text(data = holds, aes(x = Date, y = Close, label = Signal),
                           color = "grey50", size = 2, vjust = -1)
      }
    }

    p +
      coord_cartesian(ylim = c(y_range[1] - 3.5 * off, y_range[2] + 3.5 * off)) +
      scale_x_date(date_labels = "%b %Y") +
      labs(title = title_text(), subtitle = subtitle_text, x = NULL, y = "Price") +
      theme_minimal()
  })

  # Step 3: RSI panel (shown only when "RSI" is ticked)

  output$rsi_chart <- renderPlot({
    df <- view()
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "darkorange", linewidth = 0.8, na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "grey40") +
      coord_cartesian(ylim = c(0, 100)) +
      scale_x_date(date_labels = "%b %Y") +
      labs(title = paste0("RSI (", input$rsi_n, ")  -  above 70 is overbought, below 30 is oversold"),
           x = NULL, y = "RSI") +
      theme_minimal()
  })

  #Step 3: MACD panel (shown only when "MACD" is ticked)
  output$macd_chart <- renderPlot({
    df <- view()
    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACDHist, fill = MACDHist >= 0), na.rm = TRUE, show.legend = FALSE) +
      geom_line(aes(y = MACD), color = "blue", linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = MACDSignal), color = "red", linewidth = 0.8, na.rm = TRUE) +
      scale_fill_manual(values = c("TRUE" = "#66BB6A", "FALSE" = "#EF5350")) +
      scale_x_date(date_labels = "%b %Y") +
      labs(title = paste0("MACD (", input$macd_fast, ", ", input$macd_slow, ", ", input$macd_sig,
                          ")  -  blue = MACD, red = signal, bars = histogram"),
           x = NULL, y = "MACD") +
      theme_minimal()
  })

  # Step 4: signals tab

  output$signal_summary <- renderText({
    df <- view()
    n_buy  <- sum(df$Signal == "Buy")
    n_sell <- sum(df$Signal == "Sell")
    n_hold <- sum(df$Signal == "Hold")
    acted  <- df[df$Signal != "Hold", ]
    latest <- if (nrow(acted) > 0) {
      paste0(acted$Signal[nrow(acted)], " on ", format(acted$Date[nrow(acted)]))
    } else {
      "none in this range"
    }
    paste0("Buy signals: ", n_buy, "  |  Sell signals: ", n_sell,
           "  |  Hold bars: ", n_hold, "  |  Latest signal: ", latest)
  })

  output$signal_table <- renderTable({
    df <- view()
    acted <- df[df$Signal != "Hold", ]
    validate(need(nrow(acted) > 0, "No Buy or Sell signals in this range."))
    data.frame(Date = format(acted$Date), Signal = acted$Signal,
               Close = round(acted$Close, 2),
               ShortMA = round(acted$ShortMA, 2), LongMA = round(acted$LongMA, 2))
  })
}


# RUN THE APP
shinyApp(ui, server)
