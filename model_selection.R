source("Project1/libraries_dirs_and_functions.R")

# Choosing ARMA model for differenced log transformed time series
max.p = 12
max.q = 12
df.aic.log.differenced = data.table(expand.grid(ar = 0:max.p, ma = 0:max.q,
                                                incl.mean = c(FALSE)),
                                    aic = NULL, bic = NULL, AICc = NULL,
                                    nan.in.se = NULL)
pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: 
                      :elapsedfull || Estimated time remaining: :eta]",
                      total = (max.p+1)*(max.q+1), complete = "=", incomplete =
                        "-", current = ">", clear = FALSE)

#Finding AIC, BIC and AICc for different values of p, q in the ARMA model
for (i in 1:nrow(df.aic.log.differenced)) {
  fit = arima(ts(log(data$Price)), order=c(df.aic.log.differenced$ar[i], 
                                           1, df.aic.log.differenced$ma[i]))
  df.aic.log.differenced[i, "aic"] = fit$aic
  df.aic.log.differenced[i, "bic"] = BIC(fit)
  df.aic.log.differenced[i, "AICc"] = aicc(fit)
  df.aic.log.differenced[i, "nan.in.se"] = sum(diag(fit$var.coef) < 0) != 0
  pb$tick()
}
 df.aic.log.differenced = 
   df.aic.log.differenced[order(df.aic.log.differenced$AICc, decreasing = TRUE),]
df.aic.log.differenced
fwrite(df.aic.log.differenced, paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced = fread(paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced

head(df.aic.log.differenced %>% subset(nan.in.se == FALSE), 10)

xtable(df.aic.log.differenced %>% select(ar, ma, AICc) %>% slice_head(n = 10))

df.aic.log.differenced[order(df.aic.log.differenced$bic, decreasing = TRUE),]

#Saving the ARMA model with best p and q
mod.arima.log.differenced.best = arima(ts(log(data$Price)),
                                       order=c(6, 1, 10))



# Diagnosting if residuals for ARMA model for differenced log transformed time
#series appear as white noise
mean.residuals = mean(mod.arima.log.differenced.best$residuals, na.rm = TRUE)
se.residuals = sd(mod.arima.log.differenced.best$residuals, na.rm = TRUE)
df.residuals = data.frame(residuals = mod.arima.log.differenced.best$residuals)

# Checking for normality in the residuals
hist(mod.arima.log.differenced.best$residuals)
ggplot(data = df.residuals) + geom_histogram(mapping = 
                                  aes(x = residuals, y = ..density..)) + 
  stat_function(fun = dnorm, 
    args = list(mean = mean.residuals, sd = se.residuals),  color="red", lwd=1)

qqnorm(mod.arima.log.differenced.best$residuals, pch = 1, frame = FALSE)
qqline(mod.arima.log.differenced.best$residuals, lwd = 2)



jpeg(file=paste0(image.dir,"plot_acf_residuals_differenced_log_ts.jpg"))
acf(mod.arima.log.differenced.best$residuals, na.action = na.pass,
    main = "Autocorrelation of residuals from ARMA(6,10) model")
dev.off()

#Performing Box test for the residuals
Box.test(mod.arima.log.differenced.best$residuals)
ggplot(data.frame(Residual = mod.arima.log.differenced.best$residuals),
       aes( x= seq_along(Residual), y = Residual)) +
  geom_line() + 
  ggtitle("Residuals from model on differenced log transformed time series") +
  xlab("Residual no.") + ylab("Value")
ggsave("plot_residuals_differenced_log_ts.jpg", 
       path = image.dir, width = width, height = height)




