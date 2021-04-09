##Time series imputation using Kalman Smoothing


library(astsa)
library(imputeTS)

cmort2 = ts(lap[seq(1, 508, by = 4), 3], frequency = 13)

cmort2missing = cmort2
mis.t = 40:60

cmort2missing[mis.t] = NA

plot(cmort2missing)


x = Arima(
  cmort2missing,
  order = c(0, 0, 2),
  seasonal = c(1, 1, 0),
  include.drift = TRUE
)

usermodel = x$model

imp.cmort.kal = na_kalman(cmort2missing, model = usermodel)
ggplot_na_imputations(cmort2missing, imp.cmort.kal)

imp.cmort.kal = na_kalman(cmort2missing, model = "auto.arima")
ggplot_na_imputations(cmort2missing, imp.cmort.kal)


resid = na_remove(x$residuals)

dev.off()

par(mfrow = c(2, 1))
acf(resid)
qqnorm(resid)
Box.test(resid, lag = 15)



dev.off()

plot(
  as.numeric(imp.cmort.kal),
  type = 'l',
  main = "Imputed Series",
  ylab = ""
)
lines(mis.t,
      imp.cmort.kal[mis.t] + 2 * sd(resid),
      col = 4,
      lty = 2)
lines(mis.t,
      imp.cmort.kal[mis.t] - 2 * sd(resid),
      col = 4,
      lty = 2)
points(mis.t, cmort2[mis.t], col = 2)
