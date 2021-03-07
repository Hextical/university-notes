###Bootstrapped PI's

plot(cmort2)

fitAR = auto.arima(cmort2)

fitAR

checkresiduals(fitAR)

fcast <- forecast(fitAR, PI = TRUE, h = 20)
autoplot(fcast)

fcast <- forecast(fitAR,
                  PI = TRUE,
                  h = 20,
                  bootstrap = TRUE)
autoplot(fcast)
