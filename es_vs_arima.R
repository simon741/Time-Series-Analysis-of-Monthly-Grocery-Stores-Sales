library(forecast)
library(zoo)
library(astsa)

mape <- function(y, yhat)
  mean(abs((y - yhat)/y))

ts.data <- ts(scan("./dataset/dataset.txt"),start = c(1992,1),deltat = 1/12)

Sys.setlocale("LC_TIME", "English") #because of english name of days
ts.data.df <- data.frame(date = as.Date(as.yearmon(time(ts.data))),sales = as.numeric(ts.data))

#whole time series chart
#basic chart
plot(ts.data,xlab="", ylab="Sales($)",main = "Grocery Stores Sales in the U.S")

#part of the time series
#basic chart
par(mfrow=c(2,2))
#year 1992
plot(ts.data.df$date[1:12],ts.data.df$sales[1:12],type = "l",xlab="", ylab="Sales($)", main="Year 1992")

#year 1993
plot(ts.data.df$date[13:24],ts.data.df$sales[13:24],type = "l",xlab="", ylab="Sales($)", main="Year 1993")

#year 2002
plot(ts.data.df$date[109:120],ts.data.df$sales[109:120],type = "l",xlab="", ylab="Sales($)", main="Year 2002")
#year 2003
plot(ts.data.df$date[121:132],ts.data.df$sales[121:132],type = "l",xlab="", ylab="Sales($)", main="Year 2003")

par(mfrow=c(1))

#boxplot for months
ts.data.df$month <- factor(format(ts.data.df$date, format = "%b"),month.abb, ordered = T)

#basic chart
boxplot(ts.data.df$sales~ts.data.df$month,ylab="Sales($)")

#decomposition
ts.components <- decompose(ts.data,"additive")
ts.components
plot(ts.components)

#removed seasonality
plot(ts.data-ts.components$seasonal,xlab="", ylab="",main = "Adjusted Time Series without Seasonality")

#training/validation set split
ts.train <- window(ts.data,c(1992,1),c(2016,12))
ts.validation <- window(ts.data,c(2017,1),c(2017,12))

# #ES model
es.model <- ets(ts.train, model = "AAA")
es.forecast <- forecast(es.model, h=12, level =c(95))

#fitted values
plot(ts.train,ylab="Sales($)",main = "Fitted Time Series for ETS(A,Ad,A)")
lines(es.model$fitted,col="red")

#train set error
accuracy(es.model)

# Normality of residuals
par(mfrow=c(2,1))
hist(es.model$residuals,xlab="Residuals", main="Histogram of Residuals")
qqnorm(es.model$residuals,main="Normal Q-Q plot of Residuals")

#plot forecast with 95% prediction intervals - only last 10 years values
plot(es.forecast, showgap = F, include = 108, fcol = "red", ylab="Sales($)", main = "Forecast from ETS(A,Ad,A) for Year 2017 with 95% Prediction Interval")

#forecast vs reality validation set
plot(es.forecast, include = 1, fcol = "red", ylab="Sales($)", main = "Forecast vs. Reality Year 2017")
lines(ts.validation, col="black")

#Validation set error
mape(ts.validation,es.forecast$mean)


#--------------------------------------------------
#ARIMA

#ACF plot for stationarity check stationary
acf2(ts.train)

# order of differencing needed
nsdiffs(ts.train, test = c("seas"))

acf2(diff(ts.train,12),144)

#help to estimate the best parametres for arima
auto.arima(ts.train)

#Arima
arima.model <- arima(ts.train,order = c(2,1,0), seasonal = list(order = c(2,1,2), period = 12))

  
#fitted values
plot(ts.train,ylab="Sales($)",main = "Fitted Time Series for ARIMA(2,1,0)(2,1,2)[12]")
lines(fitted(arima.model),col="red")

#train set error
accuracy(arima.model)

par(mfrow=c(2,1))
hist(arima.model$residuals,xlab="Residuals", main="Histogram of Residuals")
qqnorm(arima.model$residuals,main="Normal Q-Q plot of Residuals")

arima.forecast <- forecast(arima.model, h=12, level =c(95))

#plot forecast with 95% prediction intervals - only last 100 values
plot(arima.forecast, showgap = F, include = 108, fcol = "red", ylab="Sales($)", main = "Forecast from ARIMA(2,1,0)(2,1,2)[12] for Year 2017 with 95% Prediction Interval")

#forecast vs reality validation set
plot(arima.forecast, include = 1, fcol = "red", ylab="Sales($)", main = "Forecast vs. Reality Year 2017")
lines(ts.validation, col="black")

mape(ts.validation,arima.forecast$mean) 

par(mfrow=c(1,2))
plot(es.forecast, include = 1, fcol = "red", ylab="Sales($)", main = "ES")
lines(ts.validation, col="black")
plot(arima.forecast, include = 1, fcol = "red", ylab="Sales($)", main = "ARIMA")
lines(ts.validation, col="black")
