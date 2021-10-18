source("Project1/libraries_dirs_and_functions.R")

# Choosing ARMA model for differenced log transformed time series
max.p = 12
max.q = 12
df.aic.log.differenced = data.table(expand.grid(ar = 0:max.p, ma = 0:max.q, incl.mean = c(FALSE)),
                                    aic = NULL, bic = NULL, AICc = NULL,
                                    nan.in.se = NULL)
pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                      total = (max.p+1)*(max.q+1), complete = "=", incomplete = "-", current = ">", clear = FALSE)
for (i in 1:nrow(df.aic.log.differenced)) {
  fit = arima(ts(log(data$Price)), order=c(df.aic.log.differenced$ar[i], 1, df.aic.log.differenced$ma[i]))
  df.aic.log.differenced[i, "aic"] = fit$aic
  df.aic.log.differenced[i, "bic"] = BIC(fit)
  df.aic.log.differenced[i, "AICc"] = aicc(fit)
  df.aic.log.differenced[i, "nan.in.se"] = sum(diag(fit$var.coef) < 0) != 0
  pb$tick()
}
 df.aic.log.differenced = df.aic.log.differenced[order(df.aic.log.differenced$AICc, decreasing = TRUE),]
df.aic.log.differenced
fwrite(df.aic.log.differenced, paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced = fread(paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced

head(df.aic.log.differenced %>% subset(nan.in.se == FALSE), 10)

xtable(df.aic.log.differenced %>% select(ar, ma, AICc) %>% slice_head(n = 10))

df.aic.log.differenced[order(df.aic.log.differenced$bic, decreasing = TRUE),]

mod.arima.log.differenced.best = arima(ts(log(data$Price)),
                                       order=c(4, 1, 12))

mod.arima.log.differenced.best$coef

diag(mod.arima.log.differenced.best$var.coef)
mod.arima.log.differenced.best
aicc(mod.arima.log.differenced.best)

arima(ts(log(data$Price)),
      order=c(1, 1, 12))
