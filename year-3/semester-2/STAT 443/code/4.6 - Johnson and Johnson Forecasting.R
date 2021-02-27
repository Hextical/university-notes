####
##Johnson and Johnson Forecasting
#####

plot(jj, ylab = "Johnson and Johnson Earnings")
jj.ts = as.vector(jj)

plot(log(jj))

time.vec = 1:length(jj)
time.vec2 = time.vec ^ 2
time.vec3 = time.vec ^ 3
time.vec4 = time.vec ^ 4
time.vec5 = time.vec ^ 5

num = length(jj)
reg1 = lm(log(jj) ~ time.vec, na.action = NULL)
reg2 = lm(log(jj) ~ time.vec + time.vec2, na.action = NULL)
reg3 = lm(log(jj) ~ time.vec + time.vec2 + time.vec3, na.action = NULL)
reg4 = lm(log(jj) ~ time.vec + time.vec2 + time.vec3 + time.vec4, na.action =
            NULL)
reg5 = lm(log(jj) ~ time.vec + time.vec2 + time.vec3 + time.vec4 + time.vec5,
          na.action = NULL)

AIC(reg1)
AIC(reg2)
AIC(reg3)
AIC(reg4)
AIC(reg5)

plot(log(jj), type = "p", ylab = "log Earnings")
lines(fitted(reg4), col = 1)

par(mfrow = c(2, 1))
plot(resid(reg4), type = 'l')
acf(resid(reg4))

dev.off()

pacf(resid(reg4))

rec = resid(reg4)

regr = arima(rec, c(4, 0, 0))

par(mfrow = c(2, 1))

regr$coef
plot(
  ARMAacf(ar = regr$coef[1:4], ma = c(0), 20),
  type = "h",
  ylim = c(-1, 1),
  main = "Theoretical AR(4) ACF",
  ylab = "",
  xaxt = "n"
)
abline(h = 0)

acf(rec)

dev.off()

fore = predict(regr, n.ahead = 4)
ts.plot(rec, fore$pred, col = 1:2, main = "4-step Prediction of residuals")
lines(fore$pred, type = "p", col = 2)
lines(fore$pred + 2 * fore$se, lty = "dashed", col = 4)
lines(fore$pred - 2 * fore$se, lty = "dashed", col = 4)

plot(
  log(jj.ts),
  type = 'l',
  xlim = c(60, 88),
  ylim = c(1, 4)
)

fore.trend = reg4$coefficients[1] + reg4$coefficients[2] * (85:88) + reg4$coefficients[3] *
  (85:88) ^ 2 + reg4$coefficients[4] * (85:88) ^ 3 + reg4$coefficients[5] *
  (85:88) ^ 4

lines(85:88, fore.trend, lty = 2, col = 2)

points(85:88, fore.trend + fore$pred, col = 2)
lines(
  85:88,
  fore.trend + fore$pred + 2 * fore$se,
  col = 3,
  lty = 2,
  lwd = 2
)
lines(
  85:88,
  fore.trend + fore$pred - 2 * fore$se,
  col = 3,
  lty = 2,
  lwd = 2
)

dev.off()

plot(jj.ts,
     type = 'l',
     xlim = c(1, 88),
     ylim = c(0, 25))
lines(85:88, exp(fore.trend), lty = 2, col = 2)

points(85:88, exp(fore.trend + fore$pred), col = 2)
lines(
  85:88,
  exp(fore.trend + fore$pred + 2 * fore$se),
  col = 3,
  lty = 2,
  lwd = 2
)
lines(
  85:88,
  exp(fore.trend + fore$pred - 2 * fore$se),
  col = 3,
  lty = 2,
  lwd = 2
)
