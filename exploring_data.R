# Import libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(stringr)
# Fetch data
image.dir = "Project1/Images"
width = 6
height = 4

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
ggplot(data, aes(x = Date, y = Price)) + geom_line() + ggtitle("Bitcoin time series") + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm")) + 
  ylab("Price (USD)")
ggsave("plot_ts_bitcoin.jpg", path = image.dir, width = width, height = height)

acf(data$Price, na.action = na.pass)
pacf(data$Price, na.action = na.pass)

# Plot data with log transformation
ggplot(data, aes(x = Date, y = log(Price))) + geom_line() + ggtitle("Bitcoin time series with log transformation") + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm")) + 
  ylab("log(Price) (USD)")
ggsave("plot_ts_bitcoin_logscale.jpg", path = image.dir, width = width, height = height)


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
  geom_line() + ggtitle("Differenced Bitcoin time series") + ylab("Diff(Price)") + theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
ggsave("plot_differenced_ts.jpg", path = image.dir, width = width, height = height)


# Differencing the log transformed time series and plotting
acf(diff(log(data$Price)), na.action = na.pass)
pacf(diff(log(data$Price)), na.action = na.pass)

ggplot(data.frame(Date = data$Date[-1], Price = diff(log(data$Price))), aes(x = Date, y = Price)) +
  geom_line() + ggtitle("Differenced log transformed Bitcoin time series") + ylab("Diff(log(Price))") + 
  theme(plot.margin = unit(c(0.5,0.5,0,0), "cm"))
ggsave("plot_differenced_log_ts.jpg", path = image.dir, width = width, height = height)

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
acf(mod.arima.log.differenced.best$residuals, na.action = na.pass, main = "Autocorrelation of residuals from ARMA model")

ggsave("plot_acf_residuals_differenced_log_ts.jpg", path = image.dir, width = width, height = height)

Box.test(mod.arima.log.differenced.best$residuals)
ggplot(data.frame(Residual = mod.arima.log.differenced.best$residuals),
       aes( x= seq_along(Residual), y = Residual)) +
  geom_line() + ggtitle("Residuals from model on differenced log transformed time series") +
  xlab("Residual no.") + ylab("Value")
ggsave("plot_residuals_differenced_log_ts.jpg", path = image.dir, width = width, height = height)

order = c(1,1,2)
start.date.test = "2019-01-01"
transformation = log
inv.transformation = exp
n.pred.ahead = 5
plot.pred = 1

test.preds.arima = function(data, order, start.date.test, transformation = NULL, inv.transformation = NULL, n.pred.ahead = 1,
                            plot.pred = NA) {
  start.date.test = as.Date(start.date.test)
  if (sum(is.null(c(transformation, inv.transformation))) == 1) {
    stop("Either both or neither of the transformation and inv.transformation parameter must be entered")
  }
  if(!is.null(transformation)) data = data %>% mutate(Price = transformation(Price))
  data.test = data[data$Date >= start.date.test,]
  data.test[, paste0("Pred", 1:n.pred.ahead)] = NA
  pred.cols = str_detect(colnames(data.test), "^Pred")
  curr.date.test = start.date.test
  for (i in seq_len(nrow(data.test))) {
    fit = arima(data$Price[data$Date < curr.date.test], order = order)
    data.test[i, pred.cols] = predict(fit, n.ahead = n.pred.ahead)$pred
    curr.date.test = curr.date.test + 1
  }
  cols.pred.and.price = str_detect(colnames(data.test), "^Pred|^Price")
  
  for(i in 1:n.pred.ahead) {
    pred.col = paste0("Pred", i)
    pred.i = data.test[, pred.col]
    rows.to.remove = length(pred.i) + 1 - seq_len(i)
    data.test[, pred.col] = c(rep(NA, i), pred.i[-rows.to.remove])
  }
  
  if(!is.null(inv.transformation)) {
    data.test[, cols.pred.and.price] = inv.transformation(data.test[, cols.pred.and.price])
    data = data %>% mutate(Price = inv.transformation(Price)) 
  }
  
  if(!is.na(plot.pred)) {
    y.column = sym(paste0("Pred", plot.pred))
    p = ggplot(data.test, aes(x = Date, y = !!y.column, col = "Pred")) + geom_line() +
      guides(col = guide_legend(title = "Line")) + ylab(paste0("Pred", plot.pred)) +
      ggtitle(sprintf("Prediction %d days ahead for ARMA model plotted against true Bitcoin price", plot.pred))
    p = p + geom_line(data = data, aes(x = Date, y = Price, col = "Truth"))
    print(p)
  }
  
  df.error = data.frame("Pred_Days_Ahead" = 1:n.pred.ahead,
                        mse = apply(data.test[,pred.cols], 2,
                                    function(x) return(mean((x - data.test$Price)^2, na.rm = TRUE))),
                        mae = apply(data.test[,pred.cols], 2,
                                    function(x) return(mean(abs(x - data.test$Price), na.rm = TRUE))))
  
  true.price.change = diff(data.test$Price)
  pred.price.change = data$Price[data$Date <= (start.date.test-1)]
  df.direction = data.frame("Pred_Days_Ahead" = 1:n.pred.ahead, Correct_direction = NA, True_direction_upwards = NA)
  df.investment = data.frame("Pred_Days_Ahead" = 1:n.pred.ahead, Passive = NA, Active = NA)
  df.investment[, "Passive"] = tail(data.test$Price, 1) / head(data.test$Price, 1)
  for (i in 1:nrow(df.direction)) {
    pred.direction = sign(head(data.test$Price, -i) - tail(data.test[, paste0("Pred", i)], -i))
    true.difference = diff(data.test$Price, lag = i)
    true.direction = sign(true.difference)
    correct.direction.pred = true.direction == pred.direction
    
    df.direction[i, "Correct_direction"] = mean(correct.direction.pred, na.rm = TRUE)
    df.direction[i, "True_direction_upwards"] = mean(true.direction == 1, na.rm = TRUE)
    
    true.difference.percent = true.difference / head(data.test$Price, -i)
    if (i == 1) {
      true.difference.percent.1.day.ahead = true.difference.percent
      df.investment[i, "Active"] =  prod(1 + ifelse(pred.direction == 1, true.difference.percent, 0), na.rm = TRUE)
    } else {
      df.investment[i, "Active"] =  prod(1 + ifelse(pred.direction == 1, head(true.difference.percent.1.day.ahead, -(i-1)), 0), na.rm = TRUE)
    }
  }
  df.investment
  df.direction
  return(list(df.error = df.error, df.direction = df.direction, df.investment))
}

test.preds.arima(data, order = c(2,1,3), start.date.test = "2019-01-01",
                 transformation = log, inv.transformation = exp,
                 n.pred.ahead = 2, plot.pred = 1)
is.null(exp)
sum(is.na(c(log, exp)))

