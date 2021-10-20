source("Project1/libraries_dirs_and_functions.R")
#Removing missing values from the data set
data=data[-which(is.na(data$Price)),]

#Creating a data set with log-transfirmation and differencing
data.log.diff = data.frame(Date = data$Date[-1], Price = diff(log(data$Price)))

prediction <- 10
lag <- prediction
batch_size = 50

#Finding the mean and the standard deviation on the log-transformed differenced 
#data
scale_factors <- c(mean(data.log.diff$Price, na.rm=TRUE),
                   sd(data.log.diff$Price, na.rm=TRUE))
scale_factors

#Scaling the price
scaled_price <- data.log.diff %>%
  dplyr::select(Price) %>%
  dplyr::mutate(Price = (Price - scale_factors[1]) / scale_factors[2])

#Defining the training, validation and test sets
start.date.test = as.Date("2019-10-06")
end.date.test = max(data$Date)
start.date.validation = as.Date("2019-10-06")
end.date.validation = start.date.test
scaled_train <- scaled_price[data.log.diff$Date < start.date.validation - lag,]
if(start.date.test!=start.date.validation){
  scaled_validation <- scaled_price[data.log.diff$Date < start.date.test & 
                                      data.log.diff$Date >=start.date.validation,]
}
scaled_test <- scaled_price[data.log.diff$Date >= start.date.test - lag,]

length(scaled_train)
length(scaled_test)
length(scaled_validation)

# we lag the data 11 times and arrange that into columns
get_x_data <- function(scaled_data, lag, prediction){
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

get_y_data <- function(scaled_data, lag, prediction){
  scaled_data <- as.matrix(scaled_data)

  y_data <- sapply(
    (1 + lag):(length(scaled_data) - prediction + 1),
    function(x) scaled_data[x:(x + prediction - 1)]
  )
  
  if(prediction > 1) {
    y_data = t(y_data)
    nrow.data = nrow(y_data)
  } else {
    nrow.data = length(y_data)
  }
  
  y_arr <- array(
    data = as.numeric(unlist(y_data)),
    dim = c(
      nrow.data,
      prediction,
      1
    )
  )
  return(y_arr)
}

#Creating the training set
x_train_arr = get_x_data(scaled_train, lag = lag, prediction = prediction)
y_train_arr = get_y_data(scaled_train, lag = lag, prediction = prediction)

if(start.date.test!=start.date.validation){
  x_validation_arr = get_x_data(scaled_validation, lag = lag, prediction = 
                                  prediction)
  y_validation_arr = get_y_data(scaled_validation, lag = lag, prediction = 
                                  prediction)
}

#Creating the test set
x_pred_arr = get_x_data(scaled_test, lag = lag, prediction = 1)
y_pred_arr_truth = get_y_data(scaled_test, lag = lag, prediction = 1)
#Actual  value from the original data set
y_pred_one_ahead_truth = data$Price[data$Date >= start.date.test & data$Date <= 
                                      end.date.test]


#Function for creating a LSTM model
create_lstm_model = function(batch_size) {
  lstm_model <- keras_model_sequential()
  
  lstm_model %>%
    layer_lstm(units = 50, # size of the layer
               batch_input_shape = c(batch_size, lag, 1), # batch size, timesteps, features
               return_sequences = TRUE,
               stateful = TRUE) %>%
    # fraction of the units to drop for the linear transformation of the inputs
    layer_dropout(rate = 0.5) %>%
    layer_lstm(units = 50,
               return_sequences = TRUE,
               stateful = TRUE) %>%
    layer_dropout(rate = 0.5) %>%
    time_distributed(keras::layer_dense(units = 1))
  
  lstm_model %>%
    compile(loss = 'mae', optimizer = 'adam', metrics = 'mse')
  
  return(lstm_model)
}

#Creating our LSTM model
lstm_model = create_lstm_model(batch_size)

train.indices.to.use = (dim(x_train_arr)[[1]]-batch_size * 
                          floor(dim(x_train_arr)[[1]] / batch_size)+1):
  (dim(x_train_arr)[[1]])
validation.indices.to.use = (dim(x_validation_arr)[[1]]-batch_size * 
                               floor(dim(x_validation_arr)[[1]] / batch_size)+1):
  (dim(x_validation_arr)[[1]])
length(validation.indices.to.use)

#Fitting the model
lstm_model %>% fit(
  x = array(x_train_arr[train.indices.to.use,,], dim = 
              c(length(train.indices.to.use),lag,1)),
  y = array(y_train_arr[train.indices.to.use,,], dim = 
              c(length(train.indices.to.use),lag,1)),
  batch_size = batch_size,
  epochs = 20,
  verbose = 1,
  shuffle = FALSE,
  #validation_data=list(array(x_validation_arr[validation.indices.to.use,,],
  #dim=c(length(validation.indices.to.use),lag,1)),
                     # 3 array(y_validation_arr[validation.indices.to.use,,],
  #dim=c(length(validation.indices.to.use),lag,1)))
)



#lstm_model$save("Project1/Models/model")

lstm_model_pred = create_lstm_model(1)
lstm_model_pred$set_weights(lstm_model$get_weights())
#lstm_model_pred$weights[[2]] - lstm_model$weights[[2]]

#lstm_model_pred$save("Project1/Models/model_pred")

#lstm_model_pred = load_model_tf("Project1/Models/insane_model_pred")

# Predicting and rescaling to restore the original values of the log differenced
lstm_forecast <- lstm_model_pred %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1] * scale_factors[2] + scale_factors[1]
lstm_forecast

dates.to.pred = data$Date[data$Date >= start.date.test & data$Date <= end.date.test]
plot(y_pred_arr_truth[,1,1] * scale_factors[2] + scale_factors[1], type = "l")
lines(lstm_forecast[,1], col = "red")

pred1 = exp(lstm_forecast[,1] + log(data$Price[data$Date >= start.date.test-1 &
                                                 data$Date <= end.date.test-1]))
plot(dates.to.pred, pred1, type = "l", col = "red")
lines(dates.to.pred, y_pred_one_ahead_truth)

#Plotting the predictions against the true values
df.plot.lstm = data.frame(pred = pred1, dates = dates.to.pred, 
                          pred.one.day.ahead = y_pred_one_ahead_truth)
ggplot(data = df.plot.lstm, aes(x = dates, y = pred)) + 
  geom_line(aes(col = "Pred")) + 
  geom_line(aes(x = dates, y = pred.one.day.ahead, col = "Truth")) + 
  guides(col = guide_legend(title = "Line")) +
  ggtitle("Prediction 1 day ahead for LSTM model") +
  ylab("Price") + xlab("Date")
ggsave("pred_lstm_one_day.jpg", path = image.dir, width = width, height = height)


#Calculating MSE, MAE, correct direction, passive and active investment for
#comparison with other models
df.eval.pred1 = data.frame(mae = mean(abs(pred1 - y_pred_one_ahead_truth)),
                           mse = mean((pred1 - y_pred_one_ahead_truth)^2))

y_last_step = data$Price[data$Date >= (start.date.test - 1) & data$Date <= 
                           (end.date.test - 1)]
data$Date[data$Date >= (start.date.test - 1) & data$Date <= (end.date.test - 1)]

df.eval.pred1$correct.direction = mean(sign(pred1 - y_last_step) == 
                                         sign(y_pred_one_ahead_truth - 
                                                y_last_step))
df.eval.pred1

df.eval.pred1$passive = data$Price[data$Date == end.date.test] / 
  data$Price[data$Date == start.date.test]
true.difference.percent = (y_pred_one_ahead_truth - y_last_step) / y_last_step
df.eval.pred1$active = prod(ifelse(sign(pred1 - y_last_step) == 1, 1 + 
                                     true.difference.percent, 1))
prod(ifelse(rep(1, length(y_pred_one_ahead_truth))[-1] == 1, 1 + 
              true.difference.percent[-1], 1))
df.eval.pred1

# lstm_forecast <- lstm_model_pred %>%
#   predict(x_train_arr, batch_size = 1) %>%
#   .[, , 1]
# # we need to rescale the data to restore the original values
# lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]
# lstm_forecast
# 
# apply(abs(lstm_forecast-(y_train_arr[,,1]* scale_factors[2] + 
#scale_factors[1])),2,mean)
# plot(lstm_forecast[,1], type="l")
# plot(y_train_arr[,1,1]* scale_factors[2] + scale_factors[1], type = "l")
# lines(lstm_forecast[,1])

