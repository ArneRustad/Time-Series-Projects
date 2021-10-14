
sim.arima = function(data, start.date, end.date, n.sim = 1, ar = 0, ma = NULL, differencing = 0, plot = TRUE, sd.innov = "estimate",
                     plot.truth.against.sim = FALSE,
                     transformation = function(x) return(x),
                     plot.transformed = FALSE,
                     inv.transformation = function(x) return(x)) {
  
  if (differencing >= 2) stop("Differencing >= 2 not yet implemented")
  
  start.date = as.Date(start.date)
  end.date = as.Date(end.date)
  dates.to.sim = data$Date[data$Date>= start.date & data$Date <= end.date]
  
  n = as.numeric(end.date - start.date) + 1
  
  order = c(length(ar), differencing, length(ma))
  ar.order = order[1]
  x.init = transformation(data$Price[data$Date < start.date])
  if (differencing > 0) x.init = diff(x.init, differencing)
  x.init = c(rep(0, max(order[c(1,3)])), x.init)
  innov = x.init
  #innov[1:ar.order] = x.init[1:ar.order]
  
  for(i in (ar.order+1):length(innov)) {
    ma.order = min(i-1, order[3])
    if (ma.order) {
      ma.term = sum(ma[1:ma.order] * innov[(i-ma.order):(i-1)])
    } else {ma.term = 0}
    innov[i] = x.init[i] - sum(ar * x.init[(i-ar.order):(i-1)]) - ma.term
  }
  
  if (as.character(sd.innov) == "estimate") sd.innov = sd(innov)
  
  df.sim = data.table(Date = rep(dates.to.sim, times = n.sim), n.sim = rep(1:n.sim, each = n), Price = as.numeric(NA),
                      Simulated = TRUE)
  curr.rows = 1:n
  for (i in seq_len(n.sim)) {
    sim = arima.sim(model = list(order = order, ar = ar, ma = ma), n = (n-differencing),
                    n.start = length(innov), start.innov = innov, sd = sd.innov)
    df.sim[curr.rows, Price := sim]
    curr.rows = curr.rows + n
  }
  
  if(differencing == 1) {df.sim$Price = df.sim$Price + transformation(data$Price[data$Date == start.date])}
  if (!plot.transformed) df.sim$Price = inv.transformation(df.sim$Price)
  
  if (plot) {
    p = ggplot(df.sim, aes(x = Date, y = Price, col = as.factor(n.sim), linetype = Simulated)) + geom_line() +
      guides(col = guide_legend(title = "Sim no."))
    if (plot.truth.against.sim) {
      dt.truth = as.data.table(data[data$Date <= end.date,])
      dt.truth$Simulated = FALSE
      if (plot.transformed) dt.truth$Price = transformation(dt.truth$Price)
      p = p + geom_line(aes(x = Date, y = Price, linetype = Simulated), col = "black", data = dt.truth)
    }
  }
  print(p)
}

sim.arima(data, "2019-10-01", "2021-01-01", n.sim = 1, ar = c(0.99), ma = c(0.8), differencing = 0,
          sd.innov = "estimate",
          plot.truth.against.sim = TRUE)

mod.arima.log.differenced.best
sim.arima(data, "2020-01-01", "2021-01-01", n.sim = 5, ar = c(0.0399), ma = c(-0.0870, 0.471), differencing = 1,
          sd.innov = sqrt(0.001805),
          plot.truth.against.sim = TRUE,
          transformation = log,
          inv.transformation = exp,
          plot.transformed = TRUE)

plot(arima.sim(list(order = c(1,1,2), ar = c(0.0399), ma = c(-0.0870, 0.471)), n = 100, sd = sqrt(0.001805)))

sim.arima(data, "2020-01-01", "2021-01-01", n.sim = 1, ar = c(0.0399), ma = c(-0.0870, 0.0471), differencing = 0,
          sd.innov = sqrt(0.001805),
          plot.truth.against.sim = TRUE,
          transformation = log,
          inv.transformation = exp,
          plot.transformed = TRUE)
