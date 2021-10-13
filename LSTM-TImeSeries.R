library(keras)
library(tensorflow)
#install.packages('Rcpp')
library(Rcpp)
#install_keras()
#install_tensorflow(version = "nightly")


# Fetch data
data= read.csv("bitcoin_data.csv")
colnames(data) = c("Date", "Price")

# Change column type and format of missing values from . to NA
data$Date = as.Date(data$Date)
data$Price[which(data$Price == ".")] 
data=data[-which(data$Price == "."),]
data$Price = as.numeric(as.character(data$Price))



scale_factors <- c(mean(data$Price, na.rm=TRUE), sd(data$Price, na.rm=TRUE))
scale_factors



scaled_price <- data %>%
  dplyr::select(Price) %>%
  dplyr::mutate(Price = (Price - scale_factors[1]) / scale_factors[2])

scaled_train <- scaled_price[data$Date<as.Date("2018-01-01"),]

scaled_test <- scaled_price[data$Date>=as.Date("2018-01-01"),]

prediction <- 12
lag <- 12




# we lag the data 11 times and arrange that into columns
get_x_data <- function(scaled_data){
  scaled_data <- as.matrix(scaled_data)
  x_data <- t(sapply(
    1:(length(scaled_data) - lag - prediction + 1),
    function(x) scaled_data[x:(x + lag - 1), 1]
  ))
  # now we transform it into 3D form
  x_arr <- array(
    data = as.numeric(unlist(x_data)),
    dim = c(
      nrow(x_data),
      lag,
      1
    )
  )
  
  return(x_arr)
} 

get_y_data <- function(scaled_data){
  scaled_data <- as.matrix(scaled_data)

  y_data <- t(sapply(
    (1 + lag):(length(scaled_data) - prediction + 1),
    function(x) scaled_data[x:(x + prediction - 1)]
  ))
  
  y_arr <- array(
    data = as.numeric(unlist(y_data)),
    dim = c(
      nrow(y_data),
      prediction,
      1
    )
  )
  return(y_arr)
} 

x_train_arr = get_x_data(scaled_train)
y_train_arr = get_y_data(scaled_train)

class(scaled_test)
class(scaled_train)
x_pred_arr = get_x_data(scaled_test)
y_pred_arr_truth = get_y_data(scaled_test)




lstm_model <- keras_model_sequential()


lstm_model %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, 12, 1), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  #layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  #layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'mae')

summary(lstm_model)

# #LSTM en gang til
# lstm_model_pred <- keras_model_sequential()
# 
# lstm_model_pred %>%
#   layer_lstm(units = 50, # size of the layer
#              batch_input_shape = c(1, 12, 1), # batch size, timesteps, features
#              return_sequences = TRUE,
#              stateful = TRUE) %>%
#   # fraction of the units to drop for the linear transformation of the inputs
#   #layer_dropout(rate = 0.5) %>%
#   layer_lstm(units = 50,
#              return_sequences = TRUE,
#              stateful = TRUE) %>%
#   #layer_dropout(rate = 0.5) %>%
#   time_distributed(keras::layer_dense(units = 1))
# 
# lstm_model_pred %>%
#   compile(loss = 'mae', optimizer = 'adam', metrics = 'accuracy')

#Fit
lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE
)

#lstm_model_pred$set_weights = lstm_model$get_weights

#summary(lstm_model_pred)

lstm_forecast <- lstm_model_pred %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

# we need to rescale the data to restore the original values
lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]
lstm_forecast
 
apply(abs(lstm_forecast-(y_pred_arr_truth[,,1]* scale_factors[2] + scale_factors[1])),2,mean)
plot(lstm_forecast[,1], type="l")
plot(y_pred_arr_truth[,1,1]* scale_factors[2] + scale_factors[1], type = "l")
lines(lstm_forecast[,1])
# lstm_forecast2 <- lstm_model %>%
#   predict(x_pred_arr2, batch_size = 601) %>%
#   .[, , 1]
# 
# # we need to rescale the data to restore the original values
# lstm_forecast2 <- lstm_forecast2* scale_factors[2] + scale_factors[1]
# lstm_forecast2[1,]

