#Regression with ARIMA errors

library(astsa)
library(fpp2)
library(vars)

cmort2 = ts(lap[seq(1, 508, by = 4), 3], frequency = 13)
temp2 = ts(lap[seq(1, 508, by = 4), 4], frequency = 13)
part2 = ts(lap[seq(1, 508, by = 4), 11], frequency = 13)
cmorttrue = window(cmort2, start = c(9, 12), end = c(10, 11))
cmort2 = ts(cmort2[-c((length(cmort2) - 11):length(cmort2))], frequency = 13)
temp2 = ts(temp2[-c((length(cmort2) - 11):length(cmort2))], frequency = 13)
part2 = ts(part2[-c((length(cmort2) - 11):length(cmort2))], frequency = 13)


dat.mat = cbind(as.numeric(temp2), as.numeric(part2))

dat.mat.full = cbind(as.numeric(cmort2), as.numeric(temp2), as.numeric(part2))
colnames(dat.mat.full) = c("Cmort", "Temp", "Part")




par(mfrow = c(3, 1))
plot(cmort2)
plot(temp2)
plot(part2)

temp.mod = auto.arima(temp2)
part.mod = auto.arima(part2)

temp.mod
checkresiduals(temp.mod)

part.mod
checkresiduals(part.mod)

temp.for = forecast(temp.mod, h = 12)
autoplot(temp.for)

part.for = forecast(part.mod, h = 12)
autoplot(part.for)
#part.for.ets=forecast(part.mod.ets,h=12)
#plot(part.for.ets)

#part.for=as.numeric(part.for.for(part2,12,1,0,1,1,1,1,13)$pred)
temp.for = ts(temp.for$mean, frequency = 13)
part.for = ts(part.for$mean, frequency = 13)

dat.mat.for = cbind(temp.for, part.for)

#ar.reg.all=arimax(cmort2, order=c(2,1,2),  seasonal = list(order = c(1, 0, 2), period = 13), xreg = dat.mat, include.mean = TRUE)
#ar.reg.part=arimax(cmort2, order=c(2,1,2),  seasonal = list(order = c(1, 0, 2), period = 13), xreg = temp2, include.mean = TRUE)
#sar.noreg=arimax(cmort2, order=c(2,1,2),  seasonal = list(order = c(1, 0, 2), period = 13), include.mean = TRUE)

ar.regf = auto.arima(cmort2, xreg = dat.mat)

ar.regf

checkresiduals(ar.regf)

x = forecast(ar.regf, xreg = dat.mat.for, h = 12)
autoplot(x, ylim = c(60, 123)) +
  autolayer(cmorttrue)


temp.for = forecast(temp.mod, h = 12)
autoplot(temp.for)

part.for = forecast(part.mod, h = 12)
autoplot(part.for)

dat.mat2 = cbind(
  ts(as.numeric(temp2)[-1], frequency = 13),
  ts(as.numeric(temp2)[-length(temp2)], frequency = 13),
  ts(as.numeric(part2)[-1], frequency = 13),
  ts(as.numeric(part2)[-length(part2)], , frequency = 13)
)
dat.mat.for2 = cbind(
  ts(temp.for$mean, , frequency = 13),
  ts(c(temp.for$mean[-length(temp.for$mean)],
       temp2[length(temp2)]), frequency = 13),
  ts(part.for$mean, frequency = 13),
  ts(c(part.for$mean[-length(part.for$mean)], part2[length(part2)]), frequency = 13)
)

#ar.reg.all=arimax(cmort2, order=c(2,1,2),  seasonal = list(order = c(1, 0, 2), period = 13), xreg = dat.mat, include.mean = TRUE)
#ar.reg.part=arimax(cmort2, order=c(2,1,2),  seasonal = list(order = c(1, 0, 2), period = 13), xreg = temp2, include.mean = TRUE)
#sar.noreg=arimax(cmort2, order=c(2,1,2),  seasonal = list(order = c(1, 0, 2), period = 13), include.mean = TRUE)

ar.regf2 = auto.arima(ts(cmort2[-1], frequency = 13), xreg = dat.mat2)

ar.regf2

checkresiduals(ar.regf2)

x2 = forecast(ar.regf2, xreg = dat.mat.for2, h = 12)
autoplot(x2, ylim = c(60, 123)) +
  autolayer(cmorttrue)


#compare to the standard SARIMA without regressors
ar.noregf = auto.arima(cmort2)
x.noreg = forecast(ar.noregf, h = 12)
autoplot(x.noreg, ylim = c(60, 123)) +
  autolayer(cmorttrue)
