# Reproduce Code, all images are 5x7 in. (landscape)
set.seed(123)
library(astsa)

# Figure 1.1
plot(jj, type = "o", ylab = "Quarterly Earnings per Share")
# Figure 1.2
plot(gtemp, type = "o", ylab = "Global Temperature Deviations")
# Figure 1.3
plot.ts(rnorm(500), main = "Gaussian White Noise", ylab = "w")
# Figure 1.4
fit <- lm(gtemp ~ time(gtemp), na.action = NULL)
plot.ts(gtemp, type = "o", ylab = "Global Temperature Deviations")
abline(fit)
# Figure 1.5
plot(resid(fit), type = "o", main = "detrended")
# Figure 1.6
plot(diff(gtemp), type = "o", main = "first difference")
# Figure 1.7
par(mfrow = c(2, 1))
plot(diff(gtemp), main = "first difference Temp data")
plot(rnorm(gtemp),
     type = "l",
     main = "white noise",
     ylab = "w")
par(mfrow = c(1,1))
# Figure 2.1
acf(rnorm(500))
# Figure 2.2
plot(acf(diff(gtemp)))
# Figure 2.3
acf(gtemp)
# Figure 2.4
plot(as.ts(cumsum(rnorm(100))), main = "autoregression, phi=1")
# Figure 2.5
acf(as.ts(cumsum(rnorm(100))))
# Figure 3.1
par(mfrow = c(3, 1))

ma0.sim <- arima.sim(list(order = c(0, 0, 0), ma = c()), n = 134)
plot(ma0.sim, ylab = "x", main = "white noise")

ma1.sim <- arima.sim(list(order = c(0, 0, 1), ma = c(1)), n = 134)
plot(ma1.sim, ylab = "v", main = (expression(MA(1) ~  ~  ~ theta[1] == 1)))

ma2.sim <-
  arima.sim(list(order = c(0, 0, 2), ma = c(1, 1)), n = 134)
plot(ma2.sim, ylab = "y", main = (expression(paste(
  MA(2), ~  ~  ~ theta[1], " = ",  theta[2], " = ", 1
))))

# Figure 3.2
acf(ma0.sim)
acf(ma1.sim)
acf(ma2.sim)

# Figure 3.3
ar0.sim <- arima.sim(list(order = c(1, 0, 0), ar = c(0.5)), n = 134)
plot(ar0.sim, ylab = "x", main = (expression(AR(1) ~  ~  ~ phi[1] == 0.5)))

ar1.sim <- arima.sim(list(order = c(1, 0, 0), ar = c(0.9)), n = 134)
plot(ar1.sim, ylab = "y", main = (expression(AR(1) ~  ~  ~ phi[1] == 0.9)))

ar2.sim <-
  arima.sim(list(order = c(1, 0, 0), ar = c(-0.9)), n = 134)
plot(ar2.sim, ylab = "z", main = (expression(AR(1) ~  ~  ~ phi[1] == -0.9)))

# Figure 3.4
acf(ar0.sim)
acf(ar1.sim)
acf(ar2.sim)
