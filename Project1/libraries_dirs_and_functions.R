# Import libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(stringr)
library(progress)

aicc = function(model){
  n = model$nobs
  p = length(model$coef)
  aicc = model$aic + 2*p*(p+1)/(n-p-1)
  return(aicc)
}

image.dir = "Project1/Images/"
result.dir = "Project1/Results/"


# Fetch data
data= read.csv("bitcoin_data.csv")


# Change column type and format of missing values from . to NA
colnames(data) = c("Date", "Price")
data$Date = as.Date(data$Date)
data$Price[which(data$Price == ".")] = NA
data$Price = as.numeric(as.character(data$Price))
#View(data)