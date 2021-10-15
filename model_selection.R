
# Choosing ARMA model for differenced log transformed time series
max.p = 12
max.q = 12
df.aic.log.differenced = data.table(expand.grid(ar = 0:max.p, ma = 0:max.q, incl.mean = c(FALSE)), aic = NULL, bic = NULL, aicc = NULL)
pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                      total = (max.p+1)*(max.q+1), complete = "=", incomplete = "-", current = ">", clear = FALSE)
for (i in 1:nrow(df.aic.log.differenced)) {
  fit = arima(ts(log(data$Price)), order=c(df.aic.log.differenced$ar[i], 1, df.aic.log.differenced$ma[i]))
  df.aic.log.differenced[i, "aic"] = fit$aic
  df.aic.log.differenced[i, "bic"] = BIC(fit)
  df.aic.log.differenced[i, "aicc"] = aicc(fit)
  pb$tick()
}
df.aic.log.differenced = df.aic.log.differenced[order(df.aic.log.differenced$aicc, decreasing = TRUE),]
df.aic.log.differenced
fwrite(df.aic.log.differenced, paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced = fread(paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced
mod.arima.log.differenced.best = arima(ts(log(data$Price)),
                                       order=c(4, 1, 12))
mod.arima.log.differenced.best
aicc(mod.arima.log.differenced.best)