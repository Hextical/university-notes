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

#VAR forecasts


x = VARselect(dat.mat.full, lag.max = 15)$criteria[1,] #computes AICs for VAR models up to lag 15
plot(
  x,
  main = "AIC as function of maximal lag",
  xlab = "LAG",
  ylab = "AIC",
  cex = 2
)


var_mort <- VAR(dat.mat.full, p = 5, type = "const")
var_mort_prd <- predict(var_mort, n.ahead = 13, ci = 0.95)
fanchart(var_mort_prd)


#Multivariate Portmanteau Test
var_mort_1 <- VAR(dat.mat.full, p = 1, type = "const")
serial.test(var_mort_1, lags.pt = 13)

serial.test(var_mort, lags.pt = 13)
