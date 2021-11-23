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

#Finding AIC, BIC  for different values of p, q in the ARMA model
for (i in 1:nrow(df.aic.log.differenced)) {
  model.spec <- ugarchspec(variance.model = list(garchOrder=c(df.aic.log.differenced$ar[i], df.aic.log.differenced$ma[i])), 
                           model="fGARCH", submodel="GARCH"),mean.model = list(armaOrder = c(0, 0),include.mean=FALSE)
  model=ugarchfit(spec=model.spec, data=diff(log(na.omit(data$Price))))
  
  df.aic.log.differenced[i, "aic"] = fit$aic
  df.aic.log.differenced[i, "bic"] = BIC(fit)
  df.aic.log.differenced[i, "AICc"] = aicc(fit)
  df.aic.log.differenced[i, "nan.in.se"] = sum(diag(fit$var.coef) < 0) != 0
  pb$tick()
}
df.aic.log.differenced = 
  df.aic.log.differenced[order(df.aic.log.differenced$AICc, decreasing = FALSE),]
df.aic.log.differenced
fwrite(df.aic.log.differenced, paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced = fread(paste0(result.dir, "df_aicc_log_diff.csv"))
df.aic.log.differenced

head(df.aic.log.differenced %>% subse
     
     
     
     model3.spec <- ugarchspec(variance.model = list(garchOrder=c(1,1), model="fGARCH", submodel="GARCH"),mean.model = list(armaOrder = c(0, 0),include.mean=FALSE))
     model3=ugarchfit(spec=model3.spec, data=diff(log(na.omit(data$Price))))
     #summary(model2)
     infocriteria(model3)
     getMethod("infocriteria", "uGARCHfit")
     