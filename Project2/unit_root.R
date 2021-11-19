source("Project2/libraries_dirs_and_functions.R")

adf.test(log(na.omit(data$Price)), k=0)
adf.test(log(na.omit(data$Price)))
pp.test(log(na.omit(data$Price)))

fit.garch<-garchFit(~garch(1,0), diff(log(na.omit(data$Price))))
summary(garch)

model2.spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder = c(1, 1),include.mean=FALSE))
model2=ugarchfit(spec=model2.spec, data=diff(log(na.omit(data$Price))))
summary(model2)
infocriteria(model2)

?ugarchspec

fit.garch@fit
