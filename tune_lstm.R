df_eval_all = data.frame()

for (n_epoch in c(seq(10,100, 10))) {
  
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
  
  lstm_model = create_lstm_model(batch_size)
  
  train.indices.to.use = 1:(batch_size * floor(dim(x_train_arr)[[1]] / batch_size))
  
  lstm_model %>% fit(
    x = array(x_train_arr[train.indices.to.use,,], dim = c(length(train.indices.to.use),lag,1)),
    y = array(y_train_arr[train.indices.to.use,,], dim = c(length(train.indices.to.use),lag,1)),
    batch_size = batch_size,
    epochs = n_epoch,
    verbose = 1,
    shuffle = FALSE
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
  
  pred1 = exp(lstm_forecast[,1] + log(data$Price[data$Date >= start.date.test-1 & data$Date <= end.date.test-1]))
  plot(dates.to.pred, pred1, type = "l", col = "red")
  lines(dates.to.pred, y_pred_one_ahead_truth)
  
  df.eval.pred1 = data.frame(mae = mean(abs(pred1 - y_pred_one_ahead_truth)),
                             mse = mean((pred1 - y_pred_one_ahead_truth)^2))
  
  y_last_step = data$Price[data$Date >= (start.date.test - 1) & data$Date <= (end.date.test - 1)]
  data$Date[data$Date >= (start.date.test - 1) & data$Date <= (end.date.test - 1)]
  
  df.eval.pred1$correct.direction = mean(sign(pred1 - y_last_step) == sign(y_pred_one_ahead_truth - y_last_step))
  df.eval.pred1
  
  df.eval.pred1$passive = data$Price[data$Date == end.date.test] / data$Price[data$Date == start.date.test]
  true.difference.percent = (y_pred_one_ahead_truth - y_last_step) / y_last_step
  df.eval.pred1$active = prod(ifelse(sign(pred1 - y_last_step) == 1, 1 + true.difference.percent, 1))
  prod(ifelse(rep(1, length(y_pred_one_ahead_truth))[-1] == 1, 1 + true.difference.percent[-1], 1))
  
  print(df.eval.pred1)
  df.eval.pred1$n_epoch = n_epoch
  df_eval_all = rbind(df_eval_all, df.eval.pred1)
  
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
}

print(df_eval_all)