library(astsa)
####
##Cardiovascular Mortality Forecasting
#####

# SLIDE 1

# SLIDE 2
wk = time(cmort) - mean(time(cmort))  # wk is essentially t/52 centered at zero
wk2 = wk ^ 2
wk3 = wk ^ 3
cs = cos(2 * pi * wk)
sn = sin(2 * pi * wk)
cs1 = cos(4 * pi * wk)
sn1 = sin(4 * pi * wk)

reg2 = lm(cmort ~ wk + wk2 + wk3 + sn + cs + sn1 + cs1, na.action = NULL)
plot(cmort, type = "p", ylab = "mortality")

# SLIDE 3

# SLIDE 4
lines(fitted(reg2))

# SLIDE 5
par(mfrow = c(2, 1))
plot(residuals(reg2)) # \hat{Y}_t = X_t - \hat{S}_t "seems reasonably stationary"
acf(residuals(reg2))  # Mild serial correlation

# SLIDE 7
par(mfrow = c(1, 1))
rec = residuals(reg2)
pacf(rec)

# SLIDE 6 - Normal Q-Q Plot, stuff here
qqnorm(rec)

# SLIDE 8
regr = arima(rec, c(2, 0, 1))
regr$coef

# SLIDE 9
par(mfrow = c(2, 1))
acf(residuals(reg2), 20, ylim = c(-.1, 1))
plot(
  ARMAacf(ar = c(0.0885,  0.3195), ma = c(0.1328), 20),
  type = "h",
  ylim = c(-.1, 1),
  main = "Theoretical ARMA(2,1) ACF",
  ylab = ""
)
abline(h = 0)

# SLIDE 10
par(mfrow = c(1, 1))
fore = predict(regr, n.ahead = 10)
ts.plot(
  rec,
  fore$pred,
  col = 1:2,
  xlim = c(1979.3, 1979.95),
  main = "10-step Prediction of residuals"
)
lines(fore$pred, type = "p", col = 2)
lines(fore$pred + qnorm(0.975) * fore$se,
      lty = "dashed",
      col = 4)
lines(fore$pred - qnorm(0.975) * fore$se,
      lty = "dashed",
      col = 4)

# SLIDE 11
inc = wk[2] - wk[1]
wkp = wk[length(wk)] + inc * (1:10)
wk = c(wk, wkp)
wk2 = wk ^ 2
wk3 = wk ^ 3
cs = cos(2 * pi * wk)
sn = sin(2 * pi * wk)
cs1 = cos(4 * pi * wk)
sn1 = sin(4 * pi * wk)

fit1 = 88.55365 - 2.96791228 * wk + 0.04624381 * wk2 +  0.09833614 * wk3 +  8.21611903 *
  cs +  4.08801987 * sn + 1.20542123 * sn1 + 2.77101452 * cs1
datl = rec + fit1[1:length(rec)]

plot(
  datl[(length(datl) - 19):length(datl)],
  xlim = c(1, 30),
  type = 'l',
  ylim = c(70, 110),
  ylab = ""
)
lines(21:30, fit1[(length(fit1) - 9):length(fit1)], col = 2, lty = 2)

# SLIDE 12
plot(
  datl[(length(datl) - 19):length(datl)],
  xlim = c(1, 30),
  type = 'l',
  ylim = c(70, 110),
  ylab = ""
)
lines(21:30, fit1[(length(fit1) - 9):length(fit1)], col = 2, lty = 2)
lines(21:30, fit1[(length(fit1) - 9):length(fit1)] + fore$pred, type = "p", col = 2)
lines(21:30,
      fit1[(length(fit1) - 9):length(fit1)] + fore$pred + 2 * fore$se,
      lty = "dashed",
      col = 4)
lines(21:30,
      fit1[(length(fit1) - 9):length(fit1)] + fore$pred - 2 * fore$se,
      lty = "dashed",
      col = 4)
