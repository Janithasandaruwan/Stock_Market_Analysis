# get stock prices; useful stock analysis functions
library(quantmod) 
library(xts)
# web scraping
library(rvest)
# ggplot2, purrr, dplyr, tidyr, readr, tibble
library(tidyverse)
# working with strings
library(stringr) 
library(forcats)
# working with dates 
library(lubridate)
# interactive plots
library(plotly) 
library(corrplot)
library(dplyr)
library(readxl)
library(TTR)
# evaluating the performance and  risk  characteristics  of  financial  assets  or  funds
library(PerformanceAnalytics) 
################################################################################


# define the excel file path
file_path = "/Users/janithasandaruwan/Desktop/RA-Illias/stock_market/Historical prices.xlsx"
# loads the company stock data
AAPL <- read_xlsx(file_path, sheet = 1)
MSFT <- read_xlsx(file_path, sheet = 2)
INTC <- read_xlsx(file_path, sheet = 3)
PEP <- read_xlsx(file_path,  sheet = 4)
WMT <- read_xlsx(file_path,  sheet = 5)
COST <- read_xlsx(file_path, sheet = 6)
NVDA <- read_xlsx(file_path,  sheet = 7)
################################################################################


# function for filter data between given date range and sort data by date
filter_and_sort_data <- function(stock_data, start_date, end_date){
  # convert the 'date_column' to Date type
  stock_data$Date <- as.Date(stock_data$Date, format = "%Y-%m-%d")
  # filter data between start_date and end_date
  stock_data <- stock_data %>% filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))
  # sort the filtered data by date (ascending)
  stock_data <- stock_data %>% arrange(Date)
  # return the sorted and filtered data
  return(stock_data)
}
################################################################################


# cleaned stock data for "2014-10-09" and "2021-09-30" range
AAPL <- filter_and_sort_data(AAPL, "2014-10-09", "2021-09-30")
MSFT <- filter_and_sort_data(MSFT, "2014-10-09", "2021-09-30")
INTC <- filter_and_sort_data(INTC, "2014-10-09", "2021-09-30")
PEP <- filter_and_sort_data(PEP, "2014-10-09", "2021-09-30")
WMT <- filter_and_sort_data(WMT, "2014-10-09", "2021-09-30")
COST <- filter_and_sort_data(COST, "2014-10-09", "2021-09-30")
NVDA <- filter_and_sort_data(NVDA, "2014-10-09", "2021-09-30")

# daily log returns of the companies
AAPL_log_returns<-ROC(AAPL$Adjusted)
MSFT_log_returns<-ROC(MSFT$Adjusted)
INTC_log_returns<-ROC(INTC$Adjusted)
PEP_log_returns<-ROC(PEP$Adjusted)
WMT_log_returns<-ROC(WMT$Adjusted)
COST_log_returns<-ROC(COST$Adjusted)
NVDA_log_returns<-ROC(NVDA$Adjusted)
################################################################################


# mean of log stock returns
# na.rm = TRUE ignore the NA values
AAPL_mean_log<-mean(AAPL_log_returns, na.rm=TRUE)
MSFT_mean_log<-mean(MSFT_log_returns, na.rm=TRUE)
INTC_mean_log<-mean(INTC_log_returns, na.rm=TRUE)
PEP_mean_log<-mean(PEP_log_returns, na.rm=TRUE)
WMT_mean_log<-mean(WMT_log_returns, na.rm=TRUE)
COST_mean_log<-mean(COST_log_returns, na.rm=TRUE)
NVDA_mean_log<-mean(NVDA_log_returns, na.rm=TRUE)
################################################################################


# standard deviation of log stock returns
# na.rm = TRUE ignore the NA values
AAPL_sd_log<-sd(AAPL_log_returns, na.rm=TRUE)
MSFT_sd_log<-sd(MSFT_log_returns, na.rm=TRUE)
INTC_sd_log<-sd(INTC_log_returns, na.rm=TRUE)
PEP_sd_log<-sd(PEP_log_returns, na.rm=TRUE)
WMT_sd_log<-sd(WMT_log_returns, na.rm=TRUE)
COST_sd_log<-sd(COST_log_returns, na.rm=TRUE)
NVDA_sd_log<-sd(NVDA_log_returns, na.rm=TRUE)
################################################################################


# create a data frame for mean and sd of log stock returns
log_stock_df <- data.frame(rbind(c("AAPL",AAPL_mean_log,AAPL_sd_log),c("MSFT",MSFT_mean_log,MSFT_sd_log),
                                   c("INTC",INTC_mean_log,INTC_sd_log),c("PEP",PEP_mean_log,PEP_sd_log),
                                   c("WMT",WMT_mean_log,WMT_sd_log),c("COST",COST_mean_log,COST_sd_log),
                                   c("NVDA",NVDA_mean_log,NVDA_sd_log)),stringsAsFactors = FALSE)
# Assign column titles
colnames(log_stock_df) <- c("Stock", "Mean_of_Log_Return", "SD_of_Log_Return")

################################################################################


# create bar plot for mean of log stock returns of the companies
# convert Mean_of_Log_Return to numeric
log_stock_df$Mean_of_Log_Return <- as.numeric(log_stock_df$Mean_of_Log_Return)

# sort data frame by Mean_of_Log_Return in decreasing order
sort_mean_log_stock <- log_stock_df[order(log_stock_df$Mean_of_Log_Return, decreasing = TRUE),]

# plot using ggplot2
ggplot(sort_mean_log_stock, 
       aes(x = reorder(Stock, Mean_of_Log_Return), y = Mean_of_Log_Return)) + geom_bar(stat = "identity", fill = "skyblue") +
       labs(title = "Mean of Log Returns of the Companies", 
       y = "Mean of Log Return", 
       x = "Stock") +
       theme_minimal()
################################################################################

# create bar plot for sd of log stock returns of the companies
# convert SD_of_Log_Return to numeric
log_stock_df$SD_of_Log_Return <- as.numeric(log_stock_df$SD_of_Log_Return)

# sort data frame by SD_of_Log_Return in decreasing order
sort_sd_log_stock <- log_stock_df[order(log_stock_df$SD_of_Log_Return, decreasing = TRUE),]

# plot using ggplot2
ggplot(sort_sd_log_stock, 
       aes(x = reorder(Stock, SD_of_Log_Return), y = SD_of_Log_Return)) + geom_bar(stat = "identity", fill = "burlywood") +
       labs(title = "SD of Log Returns of the Companies", 
       y = "SD Log Return", 
       x = "Stock") +
  theme_minimal()
################################################################################


# graph of Sd_Log_Return (dependent) vs Mean_Log_Return (independent)
# create data frame
log_stock_new_df<-data.frame(rbind(c("AAPL", round(AAPL_mean_log,5), round(AAPL_sd_log, 5)),
                                   c("MSFT", round(MSFT_mean_log, 5), round(MSFT_sd_log, 5)),
                                   c("INTC", round(INTC_mean_log, 5), round(INTC_sd_log, 5)),
                                   c("PEP", round(PEP_mean_log, 5), round(PEP_sd_log, 5)),
                                   c("WMT", round(WMT_mean_log, 5), round(WMT_sd_log, 5)),
                                   c("COST", round(COST_mean_log, 5), round(COST_sd_log, 5)),
                                   c("NVDA", round(NVDA_mean_log, 5), round(NVDA_sd_log, 5))))
# column names
colnames(log_stock_new_df)<-c("Share","Mean_Log_Return", "Sd_Log_Return")
ggplot(log_stock_new_df, aes(x = Mean_Log_Return, y = Sd_Log_Return, label = Share)) +
  geom_point() +  # Create points for the plot
  geom_text(vjust = -0.5, hjust = 1.5, size = 3.5) +  # Add company names with adjustments
  labs(title = "Risk vs. Return for Companies",
       x = "Mean Log Return (Return)",
       y = "SD of Log Return (Risk)")
################################################################################


# function to plot the candlestick chart with Bollinger Bands
plot_stock_with_bbands <- function(title, stock_data, start_date, end_date){
  # filter the data based on the provided date range
  data_2020 <- filter_and_sort_data(stock_data, start_date, end_date)
  # create an xts time-series object
  stock_data_xts <- xts(data_2020[, c("Open", "High", "Low", "Close")], order.by = data_2020$Date)
  # calculate Bollinger Bands (20-period moving average and 2 standard deviations)
  bbands <<- BBands(HLC(stock_data_xts), n = 20, sd = 2)
  # plot the candlestick chart
  chartSeries(stock_data_xts, type = "candlesticks", theme = chartTheme("black"), name = as.character(title))
  # draw the moving average band
  addTA(bbands$mavg, on = 1, col = "yellow", border = NA) # Moving average
}

# AAPL
plot_stock_with_bbands("AAPL Stock Prices", AAPL, "2020-01-01", "2020-12-31")
addTA(bbands$up, on = 1, col = "red", border = NA)  # Upper band
addTA(bbands$dn, on = 1, col = "green", border = NA)  # Lower band

# MSFT
plot_stock_with_bbands("MSFT Stock Prices", MSFT, "2020-01-01", "2020-12-31")
addTA(bbands$up, on = 1, col = "red", border = NA)  # Upper band
addTA(bbands$dn, on = 1, col = "green", border = NA)  # Lower band

# INTC
plot_stock_with_bbands("INTC Stock Prices", INTC, "2020-01-01", "2020-12-31")
addTA(bbands$up, on = 1, col = "red", border = NA)  # Upper band
addTA(bbands$dn, on = 1, col = "green", border = NA)  # Lower band

# PEP
plot_stock_with_bbands("PEP Stock Prices", PEP, "2020-01-01", "2020-12-31")
addTA(bbands$up, on = 1, col = "red", border = NA)  # Upper band
addTA(bbands$dn, on = 1, col = "green", border = NA)  # Lower band

# WMT
plot_stock_with_bbands("WMT Stock Prices", WMT, "2020-01-01", "2020-12-31")
addTA(bbands$up, on = 1, col = "red", border = NA)  # Upper band
addTA(bbands$dn, on = 1, col = "green", border = NA)  # Lower band

# COST
plot_stock_with_bbands("COST Stock Prices", COST, "2020-01-01", "2020-12-31")
addTA(bbands$up, on = 1, col = "red", border = NA)  # Upper band
addTA(bbands$dn, on = 1, col = "green", border = NA)  # Lower band

# NVDA
plot_stock_with_bbands("NVDA Stock Prices", NVDA, "2020-01-01", "2020-12-31")
addTA(bbands$up, on = 1, col = "red", border = NA)  # Upper band
addTA(bbands$dn, on = 1, col = "green", border = NA)  # Lower band
################################################################################


# check correlation of different companies
correlation_data<-cbind(diff(log(AAPL$Close)),diff(log(MSFT$Close)),diff(log(INTC$Close)),diff(log(PEP$Close)),
             diff(log(WMT$Close)),diff(log(COST$Close)),diff(log(NVDA$Close)))

# assign column titles
colnames(correlation_data) <- c("AAPL(Tec)", "MSFT(Tec)", "INTC(Tec)", "PEP(Beverage)", "WMT(Retail Stores)", "COST(Retail Stores)", "NVDA(Tec)")
# show the corrplot
corrplot(cor(correlation_data), method = "pie", type = "full")
################################################################################















