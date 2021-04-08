###DM TEST

library(astsa)
library(fpp2)
library(urca)
library(timeSeries)
library(tseries)
cmort2 = ts(cmort[seq(1, 508, by = 4)], frequency = 13)
plot(cmort2)


fets <- function(x, h) {
  forecast(ets(x), h = h)
}
fnn <- function(x, h) {
  forecast(nnetar(x, lambda = 0), h = h)
}
far <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

fnai <- function(x, h) {
  forecast(Arima(x, order = c(0, 1, 0)), h = h)
}

fnai2 <- function(x, h) {
  forecast(Arima(x, order = c(0, 0, 0)), h = h)
}

#cvETS <- tsCV(cmort2, fets, h = 1)
#cvNNAR <- tsCV(cmort2, fnn, h = 1)
#cvAUTOAR <- tsCV(cmort2, far, h = 1)
#cvNAIVE <- tsCV(cmort2, fnai, h = 1)


cvETS = na.remove(cvETS)
cvNNAR = na.remove(cvNNAR)
cvAUTOAR = na.remove(cvAUTOAR)
cvNAIVE = na.remove(cvNAIVE)

B = round(0.25 * length(cmort2))

ETS.test = cvETS[(length(cvETS) - B):length(cvETS)]
NNAR.test = cvNNAR[(length(cvNNAR) - B):length(cvNNAR)]
AR.test = cvAUTOAR[(length(cvAUTOAR) - B):length(cvAUTOAR)]
NAIVE.test = cvNAIVE[(length(cvNAIVE) - B):length(cvNAIVE)]

mean(ETS.test ^ 2)
mean(NNAR.test ^ 2)
mean(AR.test ^ 2)
mean(NAIVE.test ^ 2)

plot(ETS.test ^ 2 - NNAR.test ^ 2, type = 'l')
abline(h = mean(ETS.test ^ 2 - NNAR.test ^ 2),
       col = 2,
       lwd = 2)
acf(ETS.test ^ 2 - NNAR.test ^ 2)
dm.test(ETS.test, NNAR.test)

plot(ETS.test ^ 2 - AR.test ^ 2, type = 'l')
abline(h = mean(ETS.test ^ 2 - AR.test ^ 2),
       col = 2,
       lwd = 2)
acf(ETS.test ^ 2 - AR.test ^ 2)
dm.test(ETS.test, AR.test)

plot(ETS.test ^ 2 - NAIVE.test ^ 2, type = 'l')
abline(h = mean(ETS.test ^ 2 - NAIVE.test ^ 2),
       col = 2,
       lwd = 2)
acf(ETS.test ^ 2 - NAIVE.test ^ 2)
dm.test(ETS.test, NAIVE.test, power = 2)



library(sandwich)

#cvETSh <- tsCV(cmort2, fets, h = 13)
#cvNNARh <- tsCV(cmort2, fnn, h = 13)
#cvAUTOARh <- tsCV(cmort2, far, h = 13)
#cvNAIVEh <- tsCV(cmort2, fnai, h = 13)
#cvNAIVE2h <- tsCV(cmort2, fnai2, h = 13)


cvETSh = na.remove(cvETSh)
cvNNARh = na.remove(cvNNARh)
cvAUTOARh = na.remove(cvAUTOARh)
cvNAIVEh = na.remove(cvNAIVEh)
cvNAIVE2h = na.remove(cvNAIVE2h)


B = round(0.25 * length(cmort2))

ETS.test = cvETSh[(nrow(cvETSh) - B + 1):nrow(cvETSh),]
NNAR.test = cvNNARh[(nrow(cvNNARh) - B + 1):nrow(cvNNARh), ]
AR.test = cvAUTOARh[(nrow(cvAUTOARh) - B + 1):nrow(cvAUTOARh), ]
NAIVE.test = cvNAIVEh[(nrow(cvNAIVEh) - B + 1):nrow(cvNAIVEh), ]
NAIVE2.test = cvNAIVE2h[(nrow(cvNAIVE2h) - B + 1):nrow(cvNAIVE2h), ]

mean(ETS.test ^ 2)
mean(NNAR.test ^ 2)
mean(AR.test ^ 2)
mean(NAIVE.test ^ 2)
mean(NAIVE2.test ^ 2)


plot(rowMeans(ETS.test ^ 2) - rowMeans(NNAR.test ^ 2), type = 'l')
abline(h = mean(ETS.test ^ 2 - NNAR.test ^ 2),
       col = 2,
       lwd = 2)
acf(rowMeans(ETS.test ^ 2) - rowMeans(NNAR.test ^ 2))
slrv = lrvar(rowMeans(ETS.test ^ 2) - rowMeans(NNAR.test ^ 2), type = "neweywest")
mETSNNAR = mean(rowMeans(ETS.test ^ 2) - rowMeans(NNAR.test ^ 2))
#DM P-value
2 * (1 - pnorm(sqrt(B) * abs(mETSNNAR) / sqrt(slrv)))

plot(rowMeans(ETS.test ^ 2) - rowMeans(AR.test ^ 2), type = 'l')
abline(h = mean(ETS.test ^ 2 - AR.test ^ 2),
       col = 2,
       lwd = 2)
acf(rowMeans(ETS.test ^ 2) - rowMeans(AR.test ^ 2))
slrv = lrvar(rowMeans(ETS.test ^ 2) - rowMeans(AR.test ^ 2), type = "neweywest")
mETSAR = mean(rowMeans(ETS.test ^ 2) - rowMeans(AR.test ^ 2))
#DM P-value
2 * (1 - pnorm(sqrt(B) * abs(mETSAR) / sqrt(slrv)))


plot(rowMeans(NAIVE.test ^ 2) - rowMeans(AR.test ^ 2), type = 'l')
abline(h = mean(NAIVE.test ^ 2 - AR.test ^ 2),
       col = 2,
       lwd = 2)
acf(rowMeans(NAIVE.test ^ 2) - rowMeans(AR.test ^ 2))
slrv = lrvar(rowMeans(NAIVE.test ^ 2) - rowMeans(AR.test ^ 2), type = "neweywest")
mNAIVEAR = mean(rowMeans(NAIVE.test ^ 2) - rowMeans(AR.test ^ 2))
#DM P-value
2 * (1 - pnorm(sqrt(B) * abs(mNAIVEAR) / sqrt(slrv)))


#Sanity Test-- Predicting white noise
#note NNAR does not work in this case
#since often the optimization does not converge

set.seed(123)
x = rnorm(100)
#cvETSh2 <- tsCV(x, fets, h = 13)
#cvAUTOARh2 <- tsCV(x, far, h = 13)
#cvNAIVEh2 <- tsCV(x, fnai, h = 13)
#cvNAIVE2h2 <- tsCV(x, fnai2, h = 13)


cvETSh2 = na.remove(cvETSh2)
cvAUTOARh2 = na.remove(cvAUTOARh2)
cvNAIVEh2 = na.remove(cvNAIVEh2)
cvNAIVE2h2 = na.remove(cvNAIVE2h2)


B = round(0.25 * length(x))

ETS.test2 = cvETSh2[(nrow(cvETSh2) - B + 1):nrow(cvETSh2),]
AR.test2 = cvAUTOARh2[(nrow(cvAUTOARh2) - B + 1):nrow(cvAUTOARh2), ]
NAIVE.test2 = cvNAIVEh2[(nrow(cvNAIVEh2) - B + 1):nrow(cvNAIVEh2), ]
NAIVE2.test2 = cvNAIVE2[(nrow(cvNAIVE2h2) - B + 1):nrow(cvNAIVE2h2), ]

mean(ETS.test2 ^ 2)
mean(AR.test2 ^ 2)
mean(NAIVE.test2 ^ 2)
mean(NAIVE2.test2 ^ 2)



plot(rowMeans(ETS.test2 ^ 2) - rowMeans(AR.test2 ^ 2), type = 'l')
abline(h = mean(ETS.test2 ^ 2 - AR.test2 ^ 2),
       col = 2,
       lwd = 2)
acf(rowMeans(ETS.test2 ^ 2) - rowMeans(AR.test2 ^ 2))
slrv = lrvar(rowMeans(ETS.test2 ^ 2) - rowMeans(AR.test2 ^ 2), type = "neweywest")
mETSAR = mean(rowMeans(ETS.test2 ^ 2) - rowMeans(AR.test2 ^ 2))
#DM P-value
2 * (1 - pnorm(sqrt(B) * abs(mETSAR) / sqrt(slrv)))


plot(rowMeans(NAIVE.test2 ^ 2) - rowMeans(AR.test2 ^ 2), type = 'l')
abline(h = mean(NAIVE.test2 ^ 2 - AR.test2 ^ 2),
       col = 2,
       lwd = 2)
acf(rowMeans(NAIVE.test2 ^ 2) - rowMeans(AR.test2 ^ 2))
slrv = lrvar(rowMeans(NAIVE.test2 ^ 2) - rowMeans(AR.test2 ^ 2), type =
               "neweywest")
mNAIVEAR = mean(rowMeans(NAIVE.test2 ^ 2) - rowMeans(AR.test2 ^ 2))
#DM P-value
2 * (1 - pnorm(sqrt(B) * abs(mNAIVEAR) / sqrt(slrv)))


plot(rowMeans(NAIVE2.test2 ^ 2) - rowMeans(AR.test2 ^ 2), type = 'l')
abline(
  h = mean(NAIVE2.test2 ^ 2 - AR.test2 ^ 2),
  col = 2,
  lwd = 2
)
acf(rowMeans(NAIVE2.test2 ^ 2) - rowMeans(AR.test2 ^ 2))
slrv = lrvar(rowMeans(NAIVE2.test2 ^ 2) - rowMeans(AR.test2 ^ 2), type =
               "neweywest")
mNAIVEAR = mean(rowMeans(NAIVE2.test2 ^ 2) - rowMeans(AR.test2 ^ 2))
#DM P-value
2 * (1 - pnorm(sqrt(B) * abs(mNAIVEAR) / sqrt(slrv)))
