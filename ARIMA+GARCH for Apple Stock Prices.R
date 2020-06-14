appl=read.csv("D:/Desktop/DirectMarketing/AAPL17.csv",header=T)
# wbclust=read.csv("D:/Desktop/DirectMarketing/WBClust2013.csv",header=T)
appl.close=appl$Adj.Close #read and store adj close price in original file
tail(appl)
plot(appl.close,type='l',main='Apple Stock Price')
diff.appl=diff(appl.close)

plot(diff.appl,type='l',main='Difference Apple')

library(forecast)
# adf.test(diff.appl, alternative = "stationary")


log.appl=log(appl.close)
plot(log.appl,type='l',main='Log Apple')

difflog.appl=diff(log.appl)

adf.test(difflog.appl, alternative = "stationary")
plot(difflog.appl,type='l',main='Difference Log Apple')

acf.appl=acf(log.appl,main='ACF Apple',lag.max=100,ylim=c(-
                                                            0.5,1))

pacf.appl=pacf(log.appl,main='PACF
               Apple',lag.max=100,ylim=c(-0.5,1))

acf.appl=acf(difflog.appl,main='ACF Difference Log
Apple',lag.max=100,ylim=c(-0.5,1))
pacf.appl=pacf(difflog.appl,main='PACF Difference Log
Apple',lag.max=100,ylim=c(-0.5,1))

library(forecast)

arima212 = auto.arima(log.appl,ic="aic",allowdrift = FALSE,trace=TRUE)
# arima212=arima(log.appl,order=c(2,1,2))
# arima212 = auto.arima(log.appl)
# arima212=arima(log.appl,order=c(1,1,0))
summary(arima212)
arima212

forecast010step1<-forecast(arima212,1,level=95)

forecast010<-forecast(arima212,100,level=95) 
plot(forecast010)



res.arima212=arima212$res

squared.res.arima212=res.arima212^2
par(mfcol=c(1,1))
plot(squared.res.arima212,main='Squared Residuals')
acf.squared212=acf(squared.res.arima212,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.arima212,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))


# install.packages("tseries")
library("tseries")
library("forecast")

arch05=garch(res.arima212,order=c(0,5),trace=F)
AIC(arch05)
loglik05=logLik(arch05)
summary(arch05)



forecast212step1=forecast(arima212,1,level=95)
forecast212=forecast(arima212,100,level=95) 
plot(forecast212)

arch05

ht.arch08=arch05$fit[,1]^2 #use 1st column of fit
plot(ht.arch08,main='Conditional variances')

fit212=fitted.values(arima212)
low=fit212-1.96*sqrt(ht.arch08)
high=fit212+1.96*sqrt(ht.arch08)
plot(log.appl,type='l',main='Log Apple,Low,High')
lines(low,col='red')
lines(high,col='blue')


archres=res.arima212/sqrt(ht.arch08)
qqnorm(archres,main='ARIMA-ARCH Residuals')
qqline(archres)

qqnorm(res.arima212,main='ARIMA Residuals')



