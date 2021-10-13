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
plot(diff(log(data$Price), lag = 1, differences = 1), type = "l")

par(mfrow=c(1,2))
plot(arima.sim(list(order=c(0,0,1), ma=.9), n=100))

?arima.sim


mod.arma <- arima(ts(log(data$Price)), order=c(2,1,0), include.mean = TRUE)
mean(diff(log(data$Price)), na.rm=TRUE)

mod.arma.diff <- arima(ts(diff(log(data$Price))), order=c(2,0,0), include.mean = FALSE)

predict(mod.arma, n.ahead = 10)
predict(mod.arma.diff, n.ahead = 10)
?predict.Arima

<<<<<<< HEAD

      
=======
df.aic.log.differenced = data.table(expand.grid(ar = 1:4, ma = 1:4, incl.mean = c(FALSE)), aic = NULL)
for (i in 1:nrow(df.aic.log.differenced)) {
  df.aic.log.differenced[i, "aic"] = arima(ts(log(data$Price)),
                                         order=c(df.aic.log.differenced$ar[i], 1, df.aic.log.differenced$ma[i]))$aic
}
df.aic.log.differenced = df.aic.log.differenced[order(df.aic.log.differenced$aic, decreasing = TRUE),]
df.aic.log.differenced

mod.arima.log.differenced.best = arima(ts(log(data$Price)),
                                     order=c(1, 1, 2))

sim.arima = function(data, start.date, end.date, ar = 0, ma = NULL, differencing = 0, plot = TRUE, sd = 1) {
  start.date = as.Date(start.date)
  end.date = as.Date(end.date)
  n = as.numeric(end.date - start.date) + 1
  
  order = c(length(ar), differencing, length(ma))
  ar.order = order[1]
  x.init = data$Price[data$Date < start.date]
  x.init = c(rep(0, max(ar.order, order[3])), x.init)
  innov = x.init
  #innov[1:ar.order] = x.init[1:ar.order]
  
  for(i in (ar.order+1):length(innov)) {
    ma.order = min(i-1, order[3])
    if (ma.order) {
      ma.term = sum(ma[1:ma.order] * innov[(i-ma.order):(i-1)])
    } else {ma.term = 0}
    print(paste(x.init[i], sum(ar * x.init[(i-ar.order):(i-1)]),  ma.term))
    innov[i] = x.init[i] - sum(ar * x.init[(i-ar.order):(i-1)]) + ma.term
  }
  print(innov)
  sim = arima.sim(model = list(order = order, ar = ar, ma = ma), n = n,
                  n.start = length(innov)-2, start.innov = innov[-c(1:2)], sd = sd(innov))
  df = data.table(Date = as.Date(min(data$Date):end.date, origin = "1970-01-01"),
                  Price = c(data$Price[data$Date < start.date], sim))
  df$Simulated = df$Date >= start.date
  if (plot) {
    ggplot(df, aes(x = Date, y = Price, col = Simulated)) + geom_line()
  }
}

sim.arima(data, "2018-01-01", "2019-01-01", ar = c(0.975), ma = 0.2, sd = 1)
?arima.sim

exp(predict(mod.arima.log.differenced.best, n.ahead = 10)$pred)
length(df.log.detrended$Trend.poly1)
plot(df.log.detrended$Trend.poly1 + arima.sim(model = list(ar = mod.arima.log.detrended.best$model$phi, ma = mod.arima.log.detrended.best$model$theta),
          n = 1827))
?arima.sim

mod.arima.log.detrended.best$model

?filter

