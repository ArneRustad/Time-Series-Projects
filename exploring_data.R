# Import libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
# Fetch data
data= read.csv("bitcoin_data.csv")
colnames(data) = c("Date", "Price")

# Change column type and format of missing values from . to NA
data$Date = as.Date(data$Date)
data$Price[which(data$Price == ".")] = NA
data$Price = as.numeric(as.character(data$Price))
View(data)

# Check class of each column in data frame
lapply(data,class)

# Plot data
ggplot(data, aes(x = Date, y = Price)) + geom_line()

acf(data$Price, na.action = na.pass)

# Plot data with log transformation
ggplot(data, aes(x = Date, y = log(Price))) + geom_line()

acf(log(data$Price), na.action = na.pass)

# Fit and subtract trend. Check acf and plot trend with time series
fit.log1 = lm(log(Price) ~ Date, data = data)
fit.log2 = lm(log(Price) ~ poly(Date,2), data = data)
log.trend.poly1 = predict(fit.log1, newdata = data)
log.trend.poly2 = predict(fit.log2, newdata = data)

df.log.detrended = data.frame(Date = data$Date, Price = log(data$Price), Trend.poly1 = log.trend.poly1,
                              Trend.poly2 = log.trend.poly2)
df.log.detrended.long = pivot_longer(df.log.detrended, -Date)

ggplot(df.log.detrended.long, aes(x = Date, y = value, col = name)) + geom_line()

acf(log(data$Price) - trend.log, na.action = na.pass)
plot(data$Date, log(data$Price) - trend.log, type = "l")


# Differencing the time series and plotting
acf(diff(data$Price), na.action = na.pass)
plot(diff(data$Price, lag = 1, differences = 3), type = "l")

# Differencing the log transformed time series and plotting
acf(diff(log(data$Price)), na.action = na.pass)
plot(diff(log(data$Price), lag = 1, differences = 3), type = "l")
