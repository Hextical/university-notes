er = arima.sim(list(
  order = c(1, 0, 2),
  ar = .5,
  ma = c(.5, .5)
), n = 100)
dat1 = c(1:100) + er
dat1

plot(dat1, ylab = "", main = "Mystery!")

plot(diff(dat1))

par(mfrow = c(2, 1))
acf(diff(dat1))
pacf(diff(dat1))

dev.off()

sarima(dat1, 1, 1, 1)

sarima(dat1, 1, 1, 2)

sarima(dat1, 2, 1, 1)
sarima(dat1, 2, 1, 2)
sarima(dat1, 2, 1, 3)
sarima(dat1, 3, 1, 2)
sarima(dat1, 3, 1, 3)

sarima.for(dat1, 10, 3, 1, 2)
