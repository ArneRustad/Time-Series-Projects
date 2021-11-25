source("Project2/libraries_dirs_and_functions.R")

# Choosing GARCH model for differenced log transformed time series
max.p = 4
max.q = 4
max.garch.p = 4
max.garch.q = 4
df.aic.log.differenced = data.table(expand.grid(ar = 0:max.p, ma = 0:max.q, garch.p = 0:max.garch.p, garch.q = 0:max.garch.q))
View(df.aic.log.differenced)

pb = progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: 
                      :elapsedfull || Estimated time remaining: :eta]",
                      total = (max.p+1)*(max.q+1)*((max.garch.p+1)*(max.garch.q+1)-1), complete = "=", incomplete =
                        "-", current = ">", clear = TRUE)

df.aic.log.differenced = dplyr::filter(df.aic.log.differenced, ! (garch.p == 0 & garch.q == 0))

#Finding AIC, BIC  for different values of p, q in the ARMA model

for (i in 1:nrow(df.aic.log.differenced)) {
  tryCatch(
    {
      model.spec <- ugarchspec(variance.model = list(garchOrder=c(df.aic.log.differenced$garch.p[i], df.aic.log.differenced$garch.q[i]), 
                                                     model="fGARCH", submodel="GARCH"),
                               mean.model = list(armaOrder = c(df.aic.log.differenced$ar[i], df.aic.log.differenced$ma[i]),include.mean=FALSE),
                               distribution.model = "std")
      model=ugarchfit(spec=model.spec, data=diff(log(na.omit(data$Price))))
      
      df.aic.log.differenced[i, "AIC"] = infocriteria(model)[1]
      df.aic.log.differenced[i, "BIC"] = infocriteria(model)[2]
    },
    warning=function(cond) {
      df.aic.log.differenced[i, "AIC"] = NA
      df.aic.log.differenced[i, "BIC"] = NA
    }
  )
  
  pb$tick()
}

df.aic.log.differenced = df.aic.log.differenced[order(df.aic.log.differenced$BIC, decreasing = FALSE),]
df.aic.log.differenced
fwrite(df.aic.log.differenced, paste0(result.dir, "df_aic_log_diff_arma-garch_tdist.csv"))
df.aic.log.differenced = fread(paste0(result.dir, "df_aic_log_diff_arma-garch_tdist.csv"))
df.aic.log.differenced

xtable(df.aic.log.differenced %>% select(ar, ma, garch.p,garch.q,AIC,BIC) %>% slice_head(n = 10))



# Fitting best GARCH model
model.spec.best.garch <- ugarchspec(variance.model = list(garchOrder=c(df.aic.log.differenced$garch.p[1], df.aic.log.differenced$garch.q[1]), 
                                                          model="fGARCH", submodel="GARCH"),
                                    mean.model = list(armaOrder = c(df.aic.log.differenced$ar[1] ,df.aic.log.differenced$ma[1]), include.mean=FALSE),
                                    distribution.model = "std")

model.best.garch=ugarchfit(spec=model.spec.best.garch, data = data.log.diff$Price)
model.best.garch

start.date = as.Date("2019-10-06")
start.date.nr = (1:nrow(data.log.diff))[data.log.diff$Date == (start.date - 1)]
end.date.nr = nrow(data.log.diff)

roll.garch = ugarchroll(model.spec.best.garch, data = data.log.diff$Price,
                        n.ahead = 1, forecast.length = end.date.nr - start.date.nr, n.start = start.date.nr)
head(roll.garch@forecast$density)
head(roll.garch@forecast$VaR)

alpha = 0.05
df = model.best.garch@fit$coef[names(model.best.garch@fit$coef) == "shape"]
half.confint.length = qt(alpha / 2, df , lower.tail = FALSE) * roll.garch@forecast$density$Sigma * sqrt((df - 2) / df)
pred = roll.garch@forecast$density$Mu


df.log.diff.garch = data.frame(Date = dplyr::filter(data.log.diff, Date >= start.date)$Date,
                               Pred = pred,
                               lower.confint = pred -half.confint.length,
                               upper.confint = pred + half.confint.length)
df.log.diff.garch.long = pivot_longer(df.log.diff.garch, -Date, names_to = "Line_all")
df.log.diff.garch.long$Line = str_replace(df.log.diff.garch.long$Line_all, "lower.confint|upper.confint", paste(1 - alpha, "confint"))
fwrite(df.log.diff.garch.long, paste0(result.dir, "df_log_diff_arma-garch_long.csv"))


ggplot(df.log.diff.garch.long, aes(x = Date)) + geom_line(aes(y = value, col = Line, group = Line_all)) +
  geom_line(aes(y = Price, col = "Truth"), data = data.log.diff, alpha = 0.5) +
  ggtitle("ARMA-GARCH 0.95 prediction intervals for diff log price") + ylab("Diff(log(Price))")


print(model.best.garch@fit$coef)
ggsave("arma-garch_log_diff_pred_confint.jpg", path = image.dir, width = img.width, height = img.height)

log.price.for.test.obs = na.remove(log(data$Price))[(start.date.nr-1):(end.date.nr - 1)]
df.log.garch = data.frame(Date = df.log.diff.garch$Date,
                          Pred = df.log.diff.garch$Pred + log.price.for.test.obs) 

