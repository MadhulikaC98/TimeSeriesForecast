library(forecast)           
library(zoo)
library(tseries)

returns <- read.csv("C:/Users/Madhulika98/Downloads/return_data_final.csv")
View(returns)
return = subset(returns, select=c("date","return_rate"))
return.ts <- ts(return$return_rate, start = c(2014, 1), end = c(2018, 1), freq = 12)
plot(return.ts,main="Return rate for entire time series",xlab='Year',ylab="%ReturnRate")
Acf(return.ts,lag.max=12,main="ACF Plot of entire timeseries")
#Decompose
decompose_returns<-decompose(return.ts)
plot(decompose_returns)

returns.tslm<-tslm(return.ts ~ trend + season)
summary(returns.tslm)

#ADF
adf.test(return.ts)

nValid <- 6  # set holdout size          
nTrain <- length(return.ts) - nValid # set training size
train.ts <- window(return.ts, start = c(2014, 1), end = c(2017, 6))  
valid.ts <- window(return.ts, start = c(2017,7), end = c(2017,12))

options(scipen = 1) #to display numbers in scientific format or fixed
mod <- auto.arima(train.ts) # training model
summary(mod)
Acf(auto.fit$residuals, lag.max=24,main='auto.fit ACF')
Pacf(auto.fit$residuals, lag.max=24,main='auto.fit PACF')
# qpred<-forecast(train.ts, h=nValid, level=0) # forecast on holdout
# accuracy(qpred,valid) #check RMSE

acf(auto.fit$residuals, lag.max=24,main='auto.fit ACF')
pacf(auto.fit$residuals,lag.max=24, main='auto.fit PACF')
# 3. significant lag-1 PACF implies p=1;
# start to fit ARIMA models with chosen d=1, D=1, and p=1. 
# check the residuals ACF and PACF
fit1<- Arima(return.ts, order=c(0,0,0), seasonal=c(0,0,0))
acf(fit1$residuals, lag.max=24,main='ARIMA(1,1,0)(0,1,0)[12] ACF')


## Holt-Winters Validation
# use ets() with option model = "AAA" to fit Holt-Winter's smoothing 
# with additive error, trend, seasonality, and default paramenters 
hwin <- ets(train.ts, model = "ANA",alpha = 0.2)
hwin.pred<-forecast(hwin, h = nValid)
hwin
accuracy(hwin.pred,valid.ts)
hwin1 <- ets(return.ts, model = "ANA")
hwin1.pred<-forecast(hwin1, h = nValid, level=0)
hwin1.pred
# if you do not want confidence intervals (shaded part in the previous plot) make level = 0
plot(c(2014, 2019), c(0, 0.8), type = "n", xlab = "Year", ylab = "% Return Rate", bty = "l", xaxt = "n", yaxt = "n", main = "% Return Rate AES(AAA) Model")
axis(1, at = seq(2014, 2019, 1))
axis(2, at = seq(0, 0.8, 0.1), labels = format(seq(0, 0.8, 0.1)))
lines(train.ts)
lines(hwin$fitted,col="red")
lines(valid.ts)
lines(hwin.pred$mean,col="green")
lines(hwin1.pred$mean, col="blue")
legend(2017, 0.7, legend=c("Actuals", "Fitted values", "Validation values", "Forecast values"),
       col=c("black", "red", "green", "blue"), lty=1, cex=0.8)

hwin1.pred

## MA validation
# in rollmean(), use 'align = right' to calculate a trailing MA.
ma <- rollmean(return.ts, k = 3, align = "right")
plot(ma)

# obtain the last MA values and assign it holdout forecast

last.ma <- tail(ma, 1)
ma.pred <- ts(rep(last.ma, nValid), start = c(2014, 1), 
              end = c(2017,12), freq = 12)
# plot the series and fit
plot(return.ts, main = "Simple Moving Average",xlab="Year",ylab="%Return value")
lines(ma, lwd = 2, col = "blue") 
lines(ma.pred, lwd = 2, col = "green", lty = 2)
ma.pred
accuracy(ma.pred,valid.ts)

## SES(0.2) validation
# use ets() with model = "ANN" (additive error(A), no trend(N), no 
# seasonality (N)) and alpha = 0.2 to fit simple exponential smoothing.
ses<- ets(train.ts, model = "ANN", alpha = 0.2)
pred1<-forecast(ses, h = nValid, level=0)
plot(forecast(ses, h = nValid, level=0),main = "Returns SES(0.2) Validation",xlab = "Year", ylab = "% Return Rate")
lines(ses$fitted, lwd = 2, col = "blue") 
lines(valid.ts,col="green")
accuracy(pred1,valid.ts)
