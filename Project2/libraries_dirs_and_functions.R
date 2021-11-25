# Import libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(stringr)
library(progress)
library(xtable)
library(tensorflow)
#install.packages('Rcpp')
library(Rcpp)
library(tseries)
library(fGarch)
library(rugarch)

aicc = function(model){
  n = model$nobs
  p.and.q = length(model$coef)
  aicc = -2 * model$loglik + 2*(p.and.q + 1)*n / (n - p.and.q - 2)
  return(aicc)
}

test_preds = function(preds.log.diff, data, data.log.diff) {
  data = na.omit(dplyr::filter(data, Date >= (start.date - 1)))
  data.log.diff = na.omit(dplyr::filter(data.log.diff, Date >= start.date))
  if (length(preds.log.diff) != nrow(data) - 1) stop("Length of preds.log.diff must be two less than nrow of data")
  if (length(preds.log.diff) != nrow(data.log.diff)) stop("Length of preds.log.diff must be the same as nrow data.log.diff")
  
  correct.direction = mean(sign(preds.log.diff) == sign(data.log.diff$Price))
  multipliers = diff(data$Price) / data$Price[-nrow(data)] + 1
  active.investment = prod(ifelse(preds.log.diff >= 0, multipliers, 1))
  
  preds.log = log(data$Price)[-nrow(data)] + preds.log.diff
  preds = exp(preds.log)
  
  mean.absolute.error = mean(abs(preds - data$Price[-1]))
  mean.squared.error = mean((preds - data$Price[-1])^2)
  
  return(list(mean.squared.error = mean.squared.error, mean.absolute.error = mean.absolute.error,
              correct.direction = correct.direction, active.investment = active.investment))
}

image.dir = "Project2/Images/"
dir.create(image.dir, showWarnings = FALSE)
result.dir = "Project2/Results/"

img.height = 6*0.8
img.width = 8*0.8


# Fetch data
data = read.csv("Project1/bitcoin_data.csv")

# Change column type and format of missing values from . to NA
colnames(data) = c("Date", "Price")
data$Date = as.Date(data$Date)
data$Price[which(data$Price == ".")] = NA
data$Price = as.numeric(as.character(data$Price))

data = na.omit(data)
#View(data)


# Creating differenced data set
data.log.diff = na.omit(data.frame(Date = data$Date[-1], Price = diff(log(data$Price))))
