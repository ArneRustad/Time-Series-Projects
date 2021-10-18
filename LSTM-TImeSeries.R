source("Project1/libraries_dirs_and_functions.R")
data=data[-which(is.na(data$Price)),]


data.log.diff = data.frame(Date = data$Date[-1], Price = diff(log(data$Price)))


scale_factors <- c(mean(data.log.diff$Price, na.rm=TRUE), sd(data.log.diff$Price, na.rm=TRUE))
scale_factors

scaled_price <- data.log.diff %>%
  dplyr::select(Price) %>%
  dplyr::mutate(Price = (Price - scale_factors[1]) / scale_factors[2])

start.date.test = as.Date("2019-01-01")
scaled_train <- scaled_price[data.log.diff$Date < start.date.test,]
scaled_test <- scaled_price[data.log.diff$Date >= start.date.test,]

length(scaled_train)
length(scaled_test)

prediction <- 10
lag <- 10
batch_size = 50

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

x_pred_arr = get_x_data(scaled_test)
y_pred_arr_truth = get_y_data(scaled_test)

x_arr = get_x_data(scaled_price)
y_arr = get_x_data(scaled_price)


create_lstm_model = function(batch_size) {
  lstm_model <- keras_model_sequential()
  
  lstm_model %>%
    layer_lstm(units = 50, # size of the layer
               batch_input_shape = c(batch_size, lag, 1), # batch size, timesteps, features
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
  
  return(lstm_model)
}

lstm_model = create_lstm_model(batch_size)

train.indices.to.use = 1:(batch_size * floor(dim(x_train_arr)[[1]] / batch_size))

lstm_model %>% fit(
  x = array(x_train_arr[train.indices.to.use,,], dim = c(length(train.indices.to.use),lag,1)),
  y = array(y_train_arr[train.indices.to.use,,], dim = c(length(train.indices.to.use),lag,1)),
  batch_size = batch_size,
  epochs = 40,
  verbose = 1,
  shuffle = FALSE
)

lstm_model_pred = lstm_model

lstm_model_pred = create_lstm_model(1)
lstm_model_pred$set_weights(lstm_model$get_weights())
lstm_model_pred$weights[[2]] - lstm_model$weights[[2]]

lstm_forecast <- lstm_model_pred %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

# we need to rescale the data to restore the original values
lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]
lstm_forecast
 
apply(abs(lstm_forecast-(y_pred_arr_truth[,,1]* scale_factors[2] + scale_factors[1])),2,mean)

plot(y_pred_arr_truth[,1,1]* scale_factors[2] + scale_factors[1], type = "l")
lines(lstm_forecast[,1], col = "red")

lstm_forecast[,1]
# lstm_forecast <- lstm_model_pred %>%
#   predict(x_train_arr, batch_size = 1) %>%
#   .[, , 1]
# # we need to rescale the data to restore the original values
# lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]
# lstm_forecast
# 
# apply(abs(lstm_forecast-(y_train_arr[,,1]* scale_factors[2] + scale_factors[1])),2,mean)
# plot(lstm_forecast[,1], type="l")
# plot(y_train_arr[,1,1]* scale_factors[2] + scale_factors[1], type = "l")
# lines(lstm_forecast[,1])



