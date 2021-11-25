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
