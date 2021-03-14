library(astsa)
library(fpp2)
cmort2 = cmort[seq(1, 508, by = 4)]

cmort2 = ts(cmort2, frequency = 13)

plot(cmort2)



fit <- nnetar(cmort2, lambda = 0)
autoplot(forecast(fit, h = 13))
fcast <- forecast(fit, PI = TRUE, h = 13)
autoplot(fcast)
