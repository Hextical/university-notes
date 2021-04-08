library(astsa)
library(fpp2)
library(urca)
cmort2 = ts(cmort[seq(1, 508, by = 4)], frequency = 13)
plot(cmort2)

#KPSS Test

x = ts(rnorm(100))
plot(x)
ur.kpss(x)
summary(ur.kpss(x))


summary(ur.kpss(cmort2))
summary(ur.kpss(cmort2, type = "tau"))

plot(diff(cmort2))

acf(diff(cmort2))

summary(ur.kpss(diff(cmort2)))

plot(diff(cmort2, lag = 13))

acf(diff(cmort2, lag = 13))

summary(ur.kpss(diff(cmort2, lag = 13)))

auto.arima(cmort2)
