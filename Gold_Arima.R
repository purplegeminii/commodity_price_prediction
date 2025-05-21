## R Codes for Building ARIMA model

library(tseries)
library(ggplot2)
library(ggfortify)
library(reshape2)
library(fUnitRoots)
library(forecast)
library(sarima)
library(lmtest)
library(hwwntest)
library(nortest)
library(nortestARMA)
library(bbmle)
library(seasonal)
library(car)
library(sarima)
library(stats)
library(stats4)
library(TTR)
library(vars)
library(stats)
library(seasonal)
library(forecast)
library(seastests)
library(forecastHybrid)

RTA <- read.csv('/Users/purplegeminii/Desktop/Undergraduate Thesis/commodity_historical.csv')

tail(RTA)
summary(RTA)

# colnames(RTA)[colnames(RTA) == "Crude.Oil.Brent.Price"] <- "COBPrice"
# colnames(RTA)[colnames(RTA) == "Cocoa.Price"] <- "CocoaPrice"
colnames(RTA)[colnames(RTA) == "Gold.Price"] <- "GoldPrice"

#make the data a time series data
Tseries<-ts(RTA$GoldPrice, frequency=12, start=c(1980,1))
class(Tseries)

## Plotting the time series of Gold price data
autoplot(Tseries, col="green", xlab = "Year", ylab = "Gold Price")


###Test for seasonality
# If p-value < 0.05, we reject the null hypothesis, meaning there is significant seasonality.
kw(Tseries, freq = 12, diff = T, residuals = F, autoarima = T)

# Splitting the data into two parts
GoldTrain <- head(RTA, 618)
tail(GoldTrain)
GoldTest <- tail(RTA, 162)
tail(GoldTest)

### Convert Train cases to time series
TseriesTrain<-ts(GoldTrain$GoldPrice, frequency=12, start=c(1980,1))
TseriesTest<-ts(GoldTest$GoldPrice, frequency = 12)


## Decomposing the time series
DecompTseries<-decompose(TseriesTrain)
ts.stl<-stl(TseriesTrain,"periodic")  # decompose the TS
ts.sa<-seasadj(ts.stl) #de-seasonalize
acf(DecompTseries$seasonal)

## Plotting the time series of train part of the crudeoil price data
autoplot(TseriesTrain, col="green", xlab = "Year", ylab = "Gold Price")

# both acf() and pacf() generates plots by default
ACFED<- acf(TseriesTrain) # autocorrelation
PACFED<- pacf(TseriesTrain)  # partial autocorrelation

### KPSS and ADF test for stationarity (have opposite hypotheses)
# If the KPSS test p-value > 0.05 → Fail to reject null hypothesis → Series is stationary.
# If the ADF test p-value < 0.05 → Reject null hypothesis → Series is stationary.
kpss.test(TseriesTrain)
adf.test(TseriesTrain)

# Seasonal Difference
ndiffs(TseriesTrain)  # number for seasonal difference needed

## Difference to make it stationary 
GoldPrice_seasdiff <- diff(TseriesTrain, differences=1)  # seasonal differencing
plot(GoldPrice_seasdiff, type="l", main="Seasonally Differenced",col="red",xlab="Year")  # stationary!
autoplot(GoldPrice_seasdiff, col="red")
# GoldPrice_seasdiff

### test for stationary using Difference series
### KPSS and ADF have opposite hypotheses
# If the KPSS test p-value > 0.05 → Fail to reject null hypothesis → Series is stationary.
# If the ADF test p-value < 0.05 → Reject null hypothesis → Series is stationary.
kpss.test(GoldPrice_seasdiff)
adf.test(GoldPrice_seasdiff)

# both acf() and pacf() generates plots by default for Differenced series
ACFSEA<- acf(GoldPrice_seasdiff) # ACF plot
PACFSEA<- pacf(GoldPrice_seasdiff)  # PACF plot


## Arima Function to select the best model
BestArima<-auto.arima(GoldPrice_seasdiff, stepwise = FALSE, seasonal = TRUE, trace = TRUE)

## Checking various ARIMA models

# ARIMA 1
fit1<-Arima(TseriesTrain, order = c(0,1,4))  # ARIMA(0,1,4)
summary(fit1)

GoldForecast <- forecast(fit1, h=length(TseriesTest))  # Forecast for the same period as the test set
accuracy(GoldForecast, GoldTest$GoldPrice)
autoplot(GoldForecast)

#?arima

# if there is a seasonal component, then the code used is
SARIMA1<-Arima(
  TseriesTrain, 
  order = c(0,1,4), 
  seasonal = list(order = c(0,0,0), period = 12), 
  include.mean = FALSE, 
  include.drift = FALSE
)

# Coefficient test
coeftest(SARIMA1)
## Goodness of fit
summary(SARIMA1) # accuracy test

# forecast
GoldForecast <- forecast(SARIMA1, h=length(TseriesTest))  # Forecast for the same period as the test set
accuracy(GoldForecast, GoldTest$GoldPrice)
autoplot(GoldForecast)
