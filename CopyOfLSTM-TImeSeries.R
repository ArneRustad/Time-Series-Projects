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



scaled_train <- data %>%
  dplyr::select(Price) %>%
  dplyr::mutate(Price = (Price - scale_factors[1]) / scale_factors[2])



prediction <- 12
lag <- 12


scaled_train <- as.matrix(scaled_train)
scaled_train

# we lag the data 11 times and arrange that into columns
x_train_data <- t(sapply(
  1:(length(scaled_train) - lag - prediction + 1),
  function(x) scaled_train[x:(x + lag - 1), 1]
))
x_train_data
# now we transform it into 3D form
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(
    nrow(x_train_data),
    lag,
    1
  )
)
x_train_arr


y_train_data <- t(sapply(
  (1 + lag):(length(scaled_train) - prediction + 1),
  function(x) scaled_train[x:(x + prediction - 1)]
))

y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(
    nrow(y_train_data),
    prediction,
    1
  )
)
y_train_data

x_test <- data$Price[(nrow(scaled_train) - prediction + 1):nrow(scaled_train)]
x_test

# scale the data with same scaling factors as for training
x_test_scaled <- (x_test - scale_factors[1]) / scale_factors[2]

# this time our array just has one sample, as we intend to perform one 12-months prediction
x_pred_arr <- array(
  data = x_test_scaled,
  dim = c(
    1,
    lag,
    1
  )
)

x_pred_arr2 <- array(
  data = rep(x_test_scaled,each=601),
  dim = c(
    601,
    lag,
    1
  )
)





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
 

# lstm_forecast2 <- lstm_model %>%
#   predict(x_pred_arr2, batch_size = 601) %>%
#   .[, , 1]
# 
# # we need to rescale the data to restore the original values
# lstm_forecast2 <- lstm_forecast2* scale_factors[2] + scale_factors[1]
# lstm_forecast2[1,]

