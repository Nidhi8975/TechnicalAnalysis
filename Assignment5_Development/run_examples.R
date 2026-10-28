# run_examples.R
# Runs every indicator on the sample data from the assignment.
# HOW TO RUN: in RStudio, open this file, set the working directory to this folder
# (Session > Set Working Directory > To Source File Location), then click Source.
# Expected results are in the comments. R prints about 7 digits; I rounded to 4.

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

cat("--- SMA, period 3 ---\n")
print(sma(data, period = 3))
# 12.3333 15.6667 17.6667 20.0000 21.6667 23.6667 23.3333   (7 values)

cat("--- EMA, period 3 ---\n")
print(ema(data, period = 3))
# 10.0000 11.0000 13.0000 16.5000 17.2500 19.6250 22.3125 23.1562 22.0781

cat("--- MACD (3, 5, 2) ---\n")
macd_result <- macd(c(100, 105, 110, 115, 120, 125, 130),
                    short_period = 3, long_period = 5, signal_period = 2)
print(macd_result)
# macd_line:   0.0000 0.8333 1.8056 2.6620 3.3372 3.8394 4.2002
# signal_line: 0.0000 0.5556 1.3889 2.2377 2.9707 3.5498 3.9834
# histogram:   0.0000 0.2778 0.4167 0.4244 0.3665 0.2896 0.2168

cat("--- Standard deviation ---\n")
print(stdev(data))


cat("--- Linear regression (my own data) ---\n")
reg_data <- c(2, 4, 5, 4, 5, 7, 8, 9, 10, 12)
reg <- linreg(reg_data, regressionLength = 5, regressionOffset = 0)
print(reg)

# predicted_values: 5.2857 6.5714 7.8571 9.1429 10.4286 11.7143 
# Check against R's built-in: coef(lm(reg_data[5:10] ~ seq_along(reg_data[5:10])))

cat("--- RSI, period 5 ---\n")
price <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
print(rsi(price, period = 5))
# NA NA NA NA NA 50.5263 68.9256 71.8352 78.2108 66.8597

cat("--- StochRSI (RSI period 5, K 3, D 3) ---\n")
print(stoch_rsi(price, period = 5, k_period = 3, d_period = 3))
# d_line: NA NA NA NA NA 0.6920                    (6 values)

cat("--- Crossover and crossunder (assignment data) ---\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
print(crossover(arr1, arr2))
# "None" "None" "None" "Up" "None" "None" "None" "None" "None"
print(crossunder(arr1, arr2))
# "None" "False" "False" "False" "False" "False" "False" "False" "False"

cat("--- Crossover and crossunder (my own data with a cross each way) ---\n")
a <- c(1, 3, 5, 3, 1)
b <- c(2, 2, 2, 2, 2)
print(crossover(a, b))
# "None" "Up" "None" "None" "Down"
print(crossunder(a, b))
# "None" "False" "False" "False" "True"

cat("--- Error checks (each should print an error message) ---\n")
print(tryCatch(sma(c(1, 2), 5), error = function(e) conditionMessage(e)))
# "Data length should be greater than or equal to the period"
print(tryCatch(crossover(c(1, 2, 3), c(1, 2)), error = function(e) conditionMessage(e)))
# "Both arrays should have the same length"
print(tryCatch(linreg(1:5, 9, 0), error = function(e) conditionMessage(e)))
# "regressionLength cannot be greater than the number of elements in regressionSource"
