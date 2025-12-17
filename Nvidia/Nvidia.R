# NVIDIA Stock Moving Average Analysis (Tidyverse Style)

# Load libraries
library(quantmod)
library(dplyr)
library(knitr)

# Get NVIDIA stock data
getSymbols("NVDA", from = "2020-01-01", to = Sys.Date())

# Calculate 50-day and 200-day moving averages
NVDA$SMA50 <- SMA(Cl(NVDA), n = 50)
NVDA$SMA200 <- SMA(Cl(NVDA), n = 200)

# Create a tidy data frame for analysis
nvda_df <- data.frame(
  Date = index(NVDA),
  Price = as.numeric(Cl(NVDA)),
  SMA_50_day = as.numeric(NVDA$SMA50),
  SMA_200_day = as.numeric(NVDA$SMA200)
) %>%
  na.omit() %>%
  mutate(Status = ifelse(SMA_50_day > SMA_200_day, "Bullish", "Bearish"))

# TABLE 1: Sample of Data Over Time (Every 50 days)

sample_table <- nvda_df %>%
  slice(seq(1, n(), by = 50)) %>%
  mutate(
    Price = round(Price, 2),
    SMA_50_day = round(SMA_50_day, 2),
    SMA_200_day = round(SMA_200_day, 2)
  )

kable(sample_table,
      col.names = c("Date", "Close Price ($)", "50-Day SMA ($)", 
                    "200-Day SMA ($)", "Market Status"),
      caption = "Table 1: NVIDIA Stock Prices and Moving Averages (Sample)",
      align = c("l", "r", "r", "r", "c"),
      row.names = FALSE)

# TABLE 2: Most Recent 10 Trading Days

recent_table <- nvda_df %>%
  tail(10) %>%
  mutate(
    Price = round(Price, 2),
    SMA_50_day = round(SMA_50_day, 2),
    SMA_200_day = round(SMA_200_day, 2)
  )

kable(recent_table,
      col.names = c("Date", "Close Price ($)", "50-Day SMA ($)", 
                    "200-Day SMA ($)", "Market Status"),
      caption = "Table 2: NVIDIA Most Recent Trading Days",
      align = c("l", "r", "r", "r", "c"),
      row.names = FALSE)

# CHART: Stock Price with Moving Averages
chartSeries(NVDA, 
            subset = "2020::", 
            theme = "white",
            name = "NVIDIA Stock Price with Moving Averages (2020-Present)")
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")

cat("\nData Summary:\n")
cat("From", as.character(min(nvda_df$Date)), "to", as.character(max(nvda_df$Date)), "\n")
cat("Trading days:", nrow(nvda_df), "\n")
cat("Latest price: $", round(tail(nvda_df$Price, 1), 2), "\n")
cat("50-day average: $", round(tail(nvda_df$SMA_50_day, 1), 2), "\n")
cat("200-day average: $", round(tail(nvda_df$SMA_200_day, 1), 2), "\n")
cat("Current trend:", tail(nvda_df$Status, 1), "\n")

