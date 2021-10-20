source("Project1/libraries_dirs_and_functions.R")
width = 6
height = 4

# Check class of each column in data frame
lapply(data,class)

# Plot data
par(mfrow = c(1,1))
ggplot(data, aes(x = Date, y = Price)) + geom_line() + 
  ggtitle("Bitcoin time series") + 
  theme(plot.margin = unit(c(0.5,0.5,0,0), "cm")) + 
  ylab("Price (USD)")
ggsave("plot_ts_bitcoin.jpg", path = image.dir, width = width, height = height)

#Plotting ACF and PACF for the untransformed time series
acf(data$Price, na.action = na.pass)
pacf(data$Price, na.action = na.pass)

# Plot data with log transformation
ggplot(data, aes(x = Date, y = log(Price))) + geom_line() + 
  ggtitle("Bitcoin time series with log transformation") + 
  theme(plot.margin = unit(c(0.5,0.5,0,0), "cm")) + 
  ylab("log(Price) (USD)")
ggsave("plot_ts_bitcoin_logscale.jpg", path = image.dir, width = width, 
       height = height)

#Plotting ACF and PACF for the log-transformed time series
acf(log(data$Price), na.action = na.pass)
pacf(log(data$Price), na.action = na.pass)

#Finding the best model according to AIC, we did not use this result in the report
df.aic.log = data.table(expand.grid(ar = 1:4, ma = 1:4), aic = NULL)
for (i in 1:nrow(df.aic.log)) {
  df.aic.log[i, "aic"] = arima(ts(log(data$Price)),
                                         order=c(df.aic.log$ar[i], 0, 
                                                 df.aic.log$ma[i]))$aic
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

df.log.trend = data.frame(Date = data$Date, Price = log(data$Price), 
                          Trend.poly1 = log.trend.poly1,
                          Trend.poly2 = log.trend.poly2)
df.log.trend.long = pivot_longer(df.log.trend, -Date)

ggplot(df.log.trend.long, aes(x = Date, y = value, col = name)) + geom_line()

par(mfrow=c(1,1))
acf(df.log.trend$Price - df.log.trend$Trend.poly1, na.action = na.pass)
pacf(df.log.trend$Price - df.log.trend$Trend.poly1, na.action = na.pass)
plot(data$Date, df.log.trend$Price - df.log.trend$Trend.poly1, type = "l",
     main = "Log transformed then detrended")

ggplot(df.log.trend, aes(x = Date, y = Price-Trend.poly1)) + geom_line() + 
  ggtitle("Log transformed then detrended Bitcoin time series") + 
  ylab("Detrend(log(Price))") + 
  theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
ggsave("detrended-log-ts.jpg", path = image.dir, width = width, height = height)

mod.ar.log.detrended = arima(ts(df.log.trend$Price - df.log.trend$Trend.poly1),
                             order=c(2, 0, 0))

df.aic.log.detrended = data.table(expand.grid(ar = 1:4, ma = 1:4), aic = NULL)
for (i in 1:nrow(df.aic.log.detrended)) {
  df.aic.log.detrended[i, "aic"] = arima(ts(df.log.trend$Price - 
                                              df.log.trend$Trend.poly1),
                                         order=c(df.aic.log.detrended$ar[i], 0, 
                                                 df.aic.log.detrended$ma[i]))$aic
}
df.aic.log.detrended = df.aic.log.detrended[order(df.aic.log.detrended$aic,
                                                  decreasing = TRUE),]

mod.arima.log.detrended.best = arima(ts(df.log.trend$Price - 
                                          df.log.trend$Trend.poly1),
                                     order=c(df.aic.log.detrended$ar[1], 0, 
                                             df.aic.log.detrended$ma[1]))
mod.arima.log.detrended.best


# Differencing the time series and plotting
acf(diff(data$Price), na.action = na.pass)
plot(diff(data$Price, lag = 1, differences = 3), type = "l")

ggplot(data.frame(Date = data$Date[-1], Price = diff(data$Price)),
       aes(x = Date, y = Price)) +
  geom_line() + ggtitle("Differenced Bitcoin time series") + ylab("Diff(Price)") + 
  theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
ggsave("plot_differenced_ts.jpg", path = image.dir, width = width, height = height)


# Differencing the log transformed time series and plotting
#save plot
jpeg(file=paste0(image.dir,"acf-diff-log.jpeg"))
acf(diff(log(data$Price)), na.action = na.pass, main = 
      "ACF for differenced log tranformed series")
dev.off()


jpeg(file=paste0(image.dir,"pacf-diff-log.jpeg"))
pacf(diff(log(data$Price)), na.action = na.pass, main = 
       "Partial ACF for differenced log tranformed series")
dev.off()


ggplot(data.frame(Date = data$Date[-1], Price = diff(log(data$Price))), 
       aes(x = Date, y = Price)) +
  geom_line() + ggtitle("Differenced log transformed Bitcoin time series") + 
  ylab("Diff(log(Price))") + 
  theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
ggsave("plot_differenced_log_ts.jpg", path = image.dir, width = width, 
       height = height)
