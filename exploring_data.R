# Import libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)

# Fetch data
data= read.csv("bitcoin_data.csv")

# Change column type and format of missing values from . to NA
colnames(data) = c("Date", "Price")
data$Date = as.Date(data$Date)
data$Price[which(data$Price == ".")] = NA
data$Price = as.numeric(as.character(data$Price))
#View(data)

# Check class of each column in data frame
lapply(data,class)

# Plot data
par(mfrow = c(1,1))
ggplot(data, aes(x = Date, y = Price)) + geom_line() + ggtitle("Bitcoin time series")
## ggsave
acf(data$Price, na.action = na.pass)
pacf(data$Price, na.action = na.pass)

# Plot data with log transformation
ggplot(data, aes(x = Date, y = log(Price))) + geom_line() + ggtitle("Bitcoin time series with log transformation")
## ggsave

acf(log(data$Price), na.action = na.pass)
pacf(log(data$Price), na.action = na.pass)

df.aic.log = data.table(expand.grid(ar = 1:4, ma = 1:4), aic = NULL)
for (i in 1:nrow(df.aic.log)) {
  df.aic.log[i, "aic"] = arima(ts(log(data$Price)),
                                         order=c(df.aic.log$ar[i], 0, df.aic.log$ma[i]))$aic
}
df.aic.log = df.aic.log[order(df.aic.log$aic, decreasing = TRUE)]
df.aic.log

mod.arma.log.best = arima(ts(log(data$Price)), order = c(1,1,2))
mod.arma.log.best

# Fit and subtract trend. Check acf and plot trend with time series
fit.log1 = lm(log(Price) ~ Date, data = data)
fit.log2 = lm(log(Price) ~ poly(Date,2), data = data)
log.trend.poly1 = predict(fit.log1, newdata = data)
log.trend.poly2 = predict(fit.log2, newdata = data)

df.log.trend = data.frame(Date = data$Date, Price = log(data$Price), Trend.poly1 = log.trend.poly1,
                          Trend.poly2 = log.trend.poly2)
df.log.trend.long = pivot_longer(df.log.trend, -Date)

ggplot(df.log.trend.long, aes(x = Date, y = value, col = name)) + geom_line()

par(mfrow=c(1,1))
acf(df.log.trend$Price - df.log.trend$Trend.poly1, na.action = na.pass)
pacf(df.log.trend$Price - df.log.trend$Trend.poly1, na.action = na.pass)
plot(data$Date, df.log.trend$Price - df.log.trend$Trend.poly1, type = "l", main = "Log transformed then detrended")

mod.ar.log.detrended = arima(ts(df.log.trend$Price - df.log.trend$Trend.poly1), order=c(2, 0, 0))

df.aic.log.detrended = data.table(expand.grid(ar = 1:4, ma = 1:4), aic = NULL)
for (i in 1:nrow(df.aic.log.detrended)) {
  df.aic.log.detrended[i, "aic"] = arima(ts(df.log.trend$Price - df.log.trend$Trend.poly1),
                                         order=c(df.aic.log.detrended$ar[i], 0, df.aic.log.detrended$ma[i]))$aic
}
df.aic.log.detrended = df.aic.log.detrended[order(df.aic.log.detrended$aic, decreasing = TRUE),]

mod.arima.log.detrended.best = arima(ts(df.log.trend$Price - df.log.trend$Trend.poly1),
                                     order=c(df.aic.log.detrended$ar[1], 0, df.aic.log.detrended$ma[1]))
mod.arima.log.detrended.best


# Differencing the time series and plotting
acf(diff(data$Price), na.action = na.pass)
plot(diff(data$Price, lag = 1, differences = 3), type = "l")

ggplot(data.frame(Date = data$Date[-1], Price = diff(data$Price)), aes(x = Date, y = Price)) +
  geom_line() + ggtitle("Differenced Bitcoin time series") + ylab("Diff(Price)")
## ggsave

# Differencing the log transformed time series and plotting
acf(diff(log(data$Price)), na.action = na.pass)
pacf(diff(log(data$Price)), na.action = na.pass)

ggplot(data.frame(Date = data$Date[-1], Price = diff(log(data$Price))), aes(x = Date, y = Price)) +
  geom_line() + ggtitle("Differencing log transformed Bitcoin time series") + ylab("Diff(log(Price))")
## ggsave

# Choosing ARMA model for differenced log transformed time series
df.aic.log.differenced = data.table(expand.grid(ar = 1:4, ma = 1:4, incl.mean = c(FALSE)), aic = NULL, bic = NULL)
for (i in 1:nrow(df.aic.log.differenced)) {
  fit = arima(ts(log(data$Price)), order=c(df.aic.log.differenced$ar[i], 1, df.aic.log.differenced$ma[i]))
  df.aic.log.differenced[i, "aic"] = fit$aic
  df.aic.log.differenced[i, "bic"] = BIC(fit)
}
df.aic.log.differenced = df.aic.log.differenced[order(df.aic.log.differenced$aic, decreasing = TRUE),]
df.aic.log.differenced

mod.arima.log.differenced.best = arima(ts(log(data$Price)),
                                     order=c(1, 1, 2))
mod.arima.log.differenced.best

# Diagnosting if residuals for ARMA model for differenced log transformed time series appear as white noise
mean(mod.arima.log.differenced.best$residuals, na.rm = TRUE)
acf(mod.arima.log.differenced.best$residuals, na.action = na.pass)
## ggsave

Box.test(mod.arima.log.differenced.best$residuals)
ggplot(data.frame(Residual = mod.arima.log.differenced.best$residuals),
       aes( x= seq_along(Residual), y = Residual)) +
  geom_line() + ggtitle("Residuals for ARMA(1,2) model on differenced log transformed Bitcoin time series") +
  xlab("Residual no.") + ylab("Value")
## ggsave

