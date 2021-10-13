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
ggplot(data, aes(x = Date, y = Price)) + geom_line()

acf(data$Price, na.action = na.pass)

# Plot data with log transformation
ggplot(data, aes(x = Date, y = log(Price))) + geom_line()

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

# Differencing the log transformed time series and plotting
acf(diff(log(data$Price)), na.action = na.pass)
plot(diff(log(data$Price), lag = 1, differences = 1), type = "l")

par(mfrow=c(1,2))
plot(arima.sim(list(order=c(0,0,1), ma=.9), n=100))


mod.arma <- arima(ts(log(data$Price)), order=c(2,1,0), include.mean = TRUE)
mean(diff(log(data$Price)), na.rm=TRUE)

mod.arma.diff <- arima(ts(diff(log(data$Price))), order=c(2,0,0), include.mean = FALSE)

predict(mod.arma, n.ahead = 10)
predict(mod.arma.diff, n.ahead = 10)

df.aic.log.differenced = data.table(expand.grid(ar = 1:4, ma = 1:4, incl.mean = c(FALSE)), aic = NULL)
for (i in 1:nrow(df.aic.log.differenced)) {
  df.aic.log.differenced[i, "aic"] = arima(ts(log(data$Price)),
                                         order=c(df.aic.log.differenced$ar[i], 1, df.aic.log.differenced$ma[i]))$aic
}
df.aic.log.differenced = df.aic.log.differenced[order(df.aic.log.differenced$aic, decreasing = TRUE),]
df.aic.log.differenced

mod.arima.log.differenced.best = arima(ts(log(data$Price)),
                                     order=c(1, 1, 2))
mod.arima.log.differenced.best

View(data)
sim.arima = function(data, start.date, end.date, n.sim = 1, ar = 0, ma = NULL, differencing = 0, plot = TRUE, sd.innov = "estimate",
                     plot.truth.against.sim = FALSE,
                     transformation = function(x) return(x),
                     plot.transformed = FALSE,
                     inv.transformation = function(x) return(x)) {
  
  if (differencing >= 2) stop("Differencing >= 2 not yet implemented")
  
  start.date = as.Date(start.date)
  end.date = as.Date(end.date)
  dates.to.sim = data$Date[data$Date>= start.date & data$Date <= end.date]

  n = as.numeric(end.date - start.date) + 1
  
  order = c(length(ar), differencing, length(ma))
  ar.order = order[1]
  x.init = transformation(data$Price[data$Date < start.date])
  if (differencing > 0) x.init = diff(x.init, differencing)
  x.init = c(rep(0, max(order[c(1,3)])), x.init)
  innov = x.init
  #innov[1:ar.order] = x.init[1:ar.order]
  
  for(i in (ar.order+1):length(innov)) {
    ma.order = min(i-1, order[3])
    if (ma.order) {
      ma.term = sum(ma[1:ma.order] * innov[(i-ma.order):(i-1)])
    } else {ma.term = 0}
    innov[i] = x.init[i] - sum(ar * x.init[(i-ar.order):(i-1)]) - ma.term
  }
  
  if (as.character(sd.innov) == "estimate") sd.innov = sd(innov)
  
  df.sim = data.table(Date = rep(dates.to.sim, times = n.sim), n.sim = rep(1:n.sim, each = n), Price = as.numeric(NA),
                      Simulated = TRUE)
  curr.rows = 1:n
  for (i in seq_len(n.sim)) {
    sim = arima.sim(model = list(order = order, ar = ar, ma = ma), n = (n-differencing),
                    n.start = length(innov), start.innov = innov, sd = sd.innov)
    df.sim[curr.rows, Price := sim]
    curr.rows = curr.rows + n
  }
  
  if(differencing == 1) {df.sim$Price = df.sim$Price + transformation(data$Price[data$Date == start.date])}
  if (!plot.transformed) df.sim$Price = inv.transformation(df.sim$Price)

  if (plot) {
    p = ggplot(df.sim, aes(x = Date, y = Price, col = as.factor(n.sim), linetype = Simulated)) + geom_line() +
      guides(col = guide_legend(title = "Sim no."))
    if (plot.truth.against.sim) {
      dt.truth = as.data.table(data[data$Date <= end.date,])
      dt.truth$Simulated = FALSE
      if (plot.transformed) dt.truth$Price = transformation(dt.truth$Price)
      p = p + geom_line(aes(x = Date, y = Price, linetype = Simulated), col = "black", data = dt.truth)
    }
  }
  print(p)
}

sim.arima(data, "2019-10-01", "2021-01-01", n.sim = 1, ar = c(0.99), ma = c(0.8), differencing = 0,
          sd.innov = "estimate",
          plot.truth.against.sim = TRUE)

mod.arima.log.differenced.best
sim.arima(data, "2020-01-01", "2021-01-01", n.sim = 5, ar = c(0.0399), ma = c(-0.0870, 0.471), differencing = 1,
          sd.innov = sqrt(0.001805),
          plot.truth.against.sim = TRUE,
          transformation = log,
          inv.transformation = exp,
          plot.transformed = TRUE)

plot(arima.sim(list(order = c(1,1,2), ar = c(0.0399), ma = c(-0.0870, 0.471)), n = 100, sd = sqrt(0.001805)))

sim.arima(data, "2020-01-01", "2021-01-01", n.sim = 1, ar = c(0.0399), ma = c(-0.0870, 0.0471), differencing = 0,
          sd.innov = sqrt(0.001805),
          plot.truth.against.sim = TRUE,
          transformation = log,
          inv.transformation = exp,
          plot.transformed = TRUE)

exp(predict(mod.arima.log.differenced.best, n.ahead = 10)$pred)
length(df.log.detrended$Trend.poly1)
plot(df.log.detrended$Trend.poly1 + arima.sim(model = list(ar = mod.arima.log.detrended.best$model$phi, ma = mod.arima.log.detrended.best$model$theta),
          n = 1827))

?diffinv

diffinv(c(1,2,3,4))

