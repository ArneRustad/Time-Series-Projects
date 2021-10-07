# Import libraries
library(ggplot2)

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

# Plot data with log transformation
ggplot(data, aes(x = Date, y = log(Price))) + geom_line()


