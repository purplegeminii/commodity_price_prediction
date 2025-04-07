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

DATA <- read.csv('/Users/purplegeminii/Desktop/Undergraduate Thesis/commodity_historical.csv')

tail(DATA)
summary(DATA)

colnames(DATA)[colnames(DATA) == "Crude.Oil.Brent.Price"] <- "COBPrice"
# colnames(DATA)[colnames(DATA) == "Cocoa.Price"] <- "CocoaPrice"
# colnames(DATA)[colnames(DATA) == "Gold.Price"] <- "GoldPrice"

#make the data a time series data
Tseries<-ts(DATA$COBPrice, frequency=12, start=c(1980,1))
class(Tseries)

## Plotting the time series of crudeoil price data
autoplot(Tseries, col="green", xlab = "Year", ylab = "Crude Oil Bent Price")


###Test for seasonality
# If p-value < 0.05, we reject the null hypothesis, meaning there is significant seasonality.
kw(Tseries, freq = 12, diff = T, residuals = F, autoarima = T)

# Splitting the data into two parts
COBTrain <- head(DATA, 618)
COBTest <- tail(DATA, 162)

### Convert Train cases to time series
TseriesTrain<-ts(COBTrain$COBPrice, frequency=12, start=c(1980,1))
TseriesTest<-ts(COBTest$COBPrice, frequency = 12)

## Decomposing the time series
DecompTseries<-decompose(TseriesTrain)
ts.stl<-stl(TseriesTrain,"periodic")  # decompose the TS
ts.sa<-seasadj(ts.stl) #de-seasonalize
acf(DecompTseries$seasonal)

## Plotting the time series of train part of the crudeoil price data
autoplot(TseriesTrain, col="green", xlab = "Year", ylab = "Crude Oil Bent Price")

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
COBPrice_seasdiff <- diff(TseriesTrain, differences=1)  # seasonal differencing
plot(COBPrice_seasdiff, type="l", main="Seasonally Differenced",col="red",xlab="Year")  # still not stationary!
autoplot(COBPrice_seasdiff, col="red")
# COBPrice_seasdiff

### test for stationary using Difference series
### KPSS and ADF have opposite hypotheses
# If the KPSS test p-value > 0.05 → Fail to reject null hypothesis → Series is stationary.
# If the ADF test p-value < 0.05 → Reject null hypothesis → Series is stationary.
kpss.test(COBPrice_seasdiff)
adf.test(COBPrice_seasdiff)

# both acf() and pacf() generates plots by default for Differenced series
ACFSEA<- acf(COBPrice_seasdiff) # ACF plot
PACFSEA<- pacf(COBPrice_seasdiff)  # PACF plot


## Arima Function to select the best model
# Best model: ARIMA(2,0,2)(0,0,1)[12] with non-zero mean
BestArima<-auto.arima(COBPrice_seasdiff, stepwise = FALSE, seasonal = TRUE, trace = TRUE)

## Checking various ARIMA models

# ARIMA 1
fit1<-Arima(TseriesTrain, order = c(2,0,2))
summary(fit1)
?arima

# if there is a seasonal component, then the code used is
SARIMA1<-Arima(
    TseriesTrain, 
    order = c(2,0,2), 
    seasonal = list(order = c(0,0,1), 
    period = 12), 
    include.mean = TRUE, 
    include.drift = FALSE
)

# Coefficient test
coeftest(SARIMA1)
## Goodness of fit
summary(SARIMA1) # accuracy test

# forecast
COBForecast <- forecast(SARIMA1, h=length(COBTest$COBPrice))  # Forecast for the same period as the test set
accuracy(COBForecast, COBTest$COBPrice)
autoplot(COBForecast)
