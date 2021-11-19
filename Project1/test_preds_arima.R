source("Project1/libraries_dirs_and_functions.R")

#Specifying dimensions of plots
width = 6
height = 4

#Function that predicts one day ahead using the ARMA model
test.preds.arima = function(data, order, start.date.test, transformation = NULL,
                            inv.transformation = NULL, n.pred.ahead = 1,
                            plot.pred = NA) {
  start.date.test = as.Date(start.date.test)
  if (sum(is.null(c(transformation, inv.transformation))) == 1) {
    stop("Either both or neither of the transformation and inv.transformation 
         parameter must be entered")
  }
  if(!is.null(transformation)) data = data %>% mutate(Price = transformation(Price))
  data.test = data[data$Date >= start.date.test,]
  data.test[, paste0("Pred", 1:n.pred.ahead)] = NA
  pred.cols = str_detect(colnames(data.test), "^Pred")
  curr.date.test = start.date.test
  
  pb = progress_bar$new(format = 
  "(:spin) [:bar] :percent [Elapsed time: :elapsedfull ||
  Estimated time remaining: :eta]",
                        total = nrow(data.test), complete = "=", incomplete =
    "-", current = ">", clear = FALSE)
  for (i in seq_len(nrow(data.test))) {
    fit = arima(data$Price[data$Date < curr.date.test], order = order)
    data.test[i, pred.cols] = predict(fit, n.ahead = n.pred.ahead)$pred
    curr.date.test = curr.date.test + 1
    pb$tick()
  }
   cols.pred.and.price = str_detect(colnames(data.test), "^Pred|^Price")
  
  for(i in 1:n.pred.ahead) {
    pred.col = paste0("Pred", i)
    pred.i = data.test[, pred.col]
    rows.to.remove = length(pred.i) + 1 - seq_len(i)
    data.test[, pred.col] = c(rep(NA, i), pred.i[-rows.to.remove])
  }
  
  if(!is.null(inv.transformation)) {
    data.test[, cols.pred.and.price] =
      inv.transformation(data.test[, cols.pred.and.price])
    data = data %>% mutate(Price = inv.transformation(Price)) 
  }
  
  if(!is.na(plot.pred)) {
    y.column = sym(paste0("Pred", plot.pred))
    p = ggplot(data.test, aes(x = Date, y = !!y.column, col = "Pred")) + geom_line() +
      guides(col = guide_legend(title = "Line")) + ylab(paste0("Pred", plot.pred)) +
      ggtitle(sprintf("Prediction %d day(s) ahead for ARMA model plotted against 
                      true Bitcoin price", plot.pred)) +
      ylab("Price")
    p = p + geom_line(data = data, aes(x = Date, y = Price, col = "Truth"))
    print(p)
  }
  
  df.error = data.frame("Pred_Days_Ahead" = 1:n.pred.ahead,
                        mse = apply(data.test[,pred.cols], 2,
                                    function(x) return(mean((x - 
                                            data.test$Price)^2, na.rm = TRUE))),
                        mae = apply(data.test[,pred.cols], 2,
                                    function(x) return(mean(abs(x - 
                                            data.test$Price), na.rm = TRUE))))
  
  true.price.change = diff(data.test$Price)
  pred.price.change = data$Price[data$Date <= (start.date.test-1)]
  df.direction = data.frame("Pred_Days_Ahead" = 1:n.pred.ahead, 
                            Correct_direction = NA, True_direction_upwards = NA)
  df.investment = data.frame("Pred_Days_Ahead" = 1:n.pred.ahead, Passive = NA,
                             Active = NA)
  df.investment[, "Passive"] = tail(data.test$Price, 1) / head(data.test$Price, 1)
  for (i in 1:nrow(df.direction)) {
    pred.direction = sign(head(data.test$Price, -i) - 
                            tail(data.test[, paste0("Pred", i)], -i))
    true.difference = diff(data.test$Price, lag = i)
    true.direction = sign(true.difference)
    correct.direction.pred = true.direction == pred.direction
    
    df.direction[i, "Correct_direction"] = mean(correct.direction.pred, na.rm = TRUE)
    df.direction[i, "True_direction_upwards"] = mean(true.direction == 1, na.rm = TRUE)
    
    true.difference.percent = true.difference / head(data.test$Price, -i)
    if (i == 1) {
      true.difference.percent.1.day.ahead = true.difference.percent
      df.investment[i, "Active"] =  prod(1 + ifelse(pred.direction == 1, 
                                    true.difference.percent, 0), na.rm = TRUE)
    } else {
      df.investment[i, "Active"] =  prod(1 + ifelse(pred.direction == 1,
            head(true.difference.percent.1.day.ahead, -(i-1)), 0), na.rm = TRUE)
    }
  }
  df.investment
  df.direction
  return(list(df.error = df.error, df.direction = df.direction, df.investment))
}

#Running the prediction for our choice of ARMA model
test.preds.arima(data, order = c(2,1,0), start.date.test = "2019-10-06",
                 transformation = log, inv.transformation = exp,
                 n.pred.ahead = 2, plot.pred = 1)

p = last_plot()
p
ggsave("pred_for_ARMA_model.jpg", path = image.dir, width = width, height = height)
