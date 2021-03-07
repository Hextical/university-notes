library(astsa)

cmort2 = cmort[seq(1, 508, by = 4)]

cmort2 = ts(cmort2, frequency = 13)

plot(cmort2)


num = 3
aic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))
bic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))

for (ar in 0:num) {
  for (ma in 0:num) {
    for (AR in 0:num) {
      for (MA in 0:num) {
        x = tryCatch(
          sarima(cmort2, ar, 0, ma, AR, 0, MA, 13),
          error = function(e) {
            x = list()
            x$AIC = 10 ^ 100
            x$BIC = 10 ^ 100
            return(x)
          }
        )
        aic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$AIC
        bic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$BIC
      }
    }
  }
}
y.aic = which(aic.mat == min(aic.mat), arr.ind = TRUE) - 1
y.bic = which(bic.mat == min(bic.mat), arr.ind = TRUE) - 1

sarima(cmort2, y.aic[1], 0, y.aic[2], y.aic[3], 0, y.aic[4], 13)
sarima.for(cmort2, 20, y.aic[1], 0, y.aic[2], y.aic[3], 0, y.aic[4], 13)

y.aic00 = y.aic

library(astsa)
cmort2 = cmort[seq(1, 508, by = 4)]
num = 3
aic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))
bic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))

for (ar in 0:num) {
  for (ma in 0:num) {
    for (AR in 0:num) {
      for (MA in 0:num) {
        x = tryCatch(
          sarima(cmort2, ar, 1, ma, AR, 1, MA, 13),
          error = function(e) {
            x = list()
            x$AIC = 10 ^ 100
            x$BIC = 10 ^ 100
            return(x)
          }
        )
        aic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$AIC
        bic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$BIC
      }
    }
  }
}
y.aic = which(aic.mat == min(aic.mat), arr.ind = TRUE) - 1
y.bic = which(bic.mat == min(bic.mat), arr.ind = TRUE) - 1
sarima(cmort2, y.aic[1], 1, y.aic[2], y.aic[3], 1, y.aic[4], 13)
sarima.for(cmort2, 20, y.aic[1], 1, y.aic[2], y.aic[3], 1, y.aic[4], 13)

y.aic11 = y.aic

aic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))
bic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))


for (ar in 0:num) {
  for (ma in 0:num) {
    for (AR in 0:num) {
      for (MA in 0:num) {
        x = tryCatch(
          sarima(cmort2, ar, 1, ma, AR, 0, MA, 13),
          error = function(e) {
            x = list()
            x$AIC = 10 ^ 100
            x$BIC = 10 ^ 100
            return(x)
          }
        )
        aic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$AIC
        bic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$BIC
      }
    }
  }
}
y.aic = which(aic.mat == min(aic.mat), arr.ind = TRUE) - 1
y.bic = which(bic.mat == min(bic.mat), arr.ind = TRUE) - 1
sarima(cmort2, y.aic[1], 1, y.aic[2], y.aic[3], 0, y.aic[4], 13)
sarima.for(cmort2, 20, y.aic[1], 1, y.aic[2], y.aic[3], 0, y.aic[4], 13)


y.aic10 = y.aic


library(astsa)
cmort2 = cmort[seq(1, 508, by = 4)]
num = 2
aic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))
bic.mat = rep(10 ^ 100, (num + 1)) %o% rep(10 ^ 100, (num + 1)) %o% rep(10 ^
                                                                          100, (num + 1)) %o% rep(10 ^ 100, (num + 1))

for (ar in 0:num) {
  for (ma in 0:num) {
    for (AR in 0:num) {
      for (MA in 0:num) {
        x = tryCatch(
          sarima(cmort2, ar, 0, ma, AR, 1, MA, 13),
          error = function(e) {
            x = list()
            x$AIC = 10 ^ 100
            x$BIC = 10 ^ 100
            return(x)
          }
        )
        aic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$AIC
        bic.mat[(ar + 1), (ma + 1), (AR + 1), (MA + 1)] = x$BIC
      }
    }
  }
}
y.aic = which(aic.mat == min(aic.mat), arr.ind = TRUE) - 1
y.bic = which(bic.mat == min(bic.mat), arr.ind = TRUE) - 1
sarima(cmort2, y.aic[1], 0, y.aic[2], y.aic[3], 1, y.aic[4], 13)
sarima.for(cmort2, 20, y.aic[1], 0, y.aic[2], y.aic[3], 1, y.aic[4], 13)

y.aic01 = y.aic

y.aic00
y.aic01
y.aic10
y.aic11

sarima(cmort2, y.aic10[1], 1, y.aic10[2], y.aic10[3], 0, y.aic10[4], 13)

sarima(cmort2, y.aic01[1], 0, y.aic01[2], y.aic01[3], 1, y.aic01[4], 13)

sarima(cmort2, y.aic00[1], 0, y.aic00[2], y.aic00[3], 0, y.aic00[4], 13)

sarima(cmort2, y.aic11[1], 1, y.aic11[2], y.aic11[3], 1, y.aic11[4], 13)

sarima.for(cmort2, 20, y.aic10[1], 1, y.aic10[2], y.aic10[3], 0, y.aic10[4], 13)

sarima.for(cmort2, 20, y.aic01[1], 0, y.aic01[2], y.aic01[3], 1, y.aic01[4], 13)

sarima.for(cmort2, 20, y.aic00[1], 0, y.aic00[2], y.aic00[3], 0, y.aic00[4], 13)

sarima.for(cmort2, 20, y.aic11[1], 1, y.aic11[2], y.aic11[3], 1, y.aic11[4], 13)

library("fpp2")
library("forecast")

fitAR = auto.arima(cmort2) ##explained in Section
#8.7 of HH book

fitAR

checkresiduals(fitAR)
fcast <- forecast(fitAR, PI = TRUE, h = 20)
autoplot(fcast)
