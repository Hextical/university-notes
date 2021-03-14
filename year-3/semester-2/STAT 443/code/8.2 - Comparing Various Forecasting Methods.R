#install.packages("forecast")
#install.packages("fpp2")
library("fpp2")
library("forecast")

cmort2 = ts(cmort2, frequency = 13)

x = forecast(fit, h = 30)

fitAR = auto.arima(cmort2)
#fcast <- forecast(fitAR, PI=TRUE, h=30,bootstrap = TRUE)
fcast <- forecast(fitAR, PI = TRUE, h = 30)

autoplot(fcast)

fit <- nnetar(cmort2, lambda = 0)
autoplot(forecast(fit, h = 30))
fcast <- forecast(fit, PI = TRUE, h = 30)
autoplot(fcast)

fitET = ets(cmort2)
fcast <- forecast(fitET, h = 30, PI = TRUE)
autoplot(fcast)



tr = round(length(cmort2) * 0.75)
cvSARIMA = 1:length(tr:(length(cmort2) - 1))
cvAUTOAR = 1:length(tr:(length(cmort2) - 1))

cvETS = 1:length(tr:(length(cmort2) - 1))
cvNNAR = 1:length(tr:(length(cmort2) - 1))


for (j in (tr:(length(cmort2) - 1))) {
  print(j)
  xSARIMA = tryCatch(
    sarima.for(cmort2[1:j], 1, 2, 0, 2, 2, 0, 1, 13),
    error = function(e) {
      x = list()
      x$pred = cmort2[j]
      return(x)
    }
  )
  cmort2j = ts(cmort2[1:j], frequency = 13)
  xAAR = forecast(auto.arima(cmort2j), h = 1)$mean
  xETS = forecast(ets(cmort2j), h = 1)$mean
  autoplot(forecast(ets(cmort2j), h = 1))
  xNNAR = forecast(nnetar(cmort2j, lambda = 0), h = 1)$mean
  cvSARIMA[j - tr + 1] = xSARIMA$pred - cmort2[j + 1]
  cvAUTOAR[j - tr + 1] = xAAR - cmort2[j + 1]
  cvETS[j - tr + 1] = xETS - cmort2[j + 1]
  cvNNAR[j - tr + 1] = xNNAR - cmort2[j + 1]
  print(xETS - cmort2[j + 1])
  print(xNNAR - cmort2[j + 1])
}

sum(cvSARIMA^2)
sum(cvETS^2)
sum(cvNNAR^2)
sum(cvAUTOAR^2)

# Personal Analysis

fets <- function(x, h) {
  forecast(ets(x), h = h)
}
fnn <- function(x, h) {
  forecast(nnetar(x, lambda = 0), h = h)
}
far <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

cvETSat <- tsCV(cmort2, fets, h = 32)
cvNNARat <- tsCV(cmort2, fnn, h = 32)
cvAUTOARat <- tsCV(cmort2, far, h = 32)

sqrt(mean(cvETSat ^ 2, na.rm = TRUE))
sqrt(mean(cvNNARat ^ 2, na.rm = TRUE))
sqrt(mean(cvAUTOARat ^ 2, na.rm = TRUE))
