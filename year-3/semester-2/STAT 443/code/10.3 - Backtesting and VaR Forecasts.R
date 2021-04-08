library(fGarch)
library(astsa)

nyse = astsa::nyse
plot(nyse)
abline(v = 1000,
       lty = 2,
       lwd = 3,
       col = 3)

nyse.g.1 = garchFit( ~ arma(0, 0) + garch(1, 1), nyse[1:1000])
predict(nyse.g.1, 10)
predict(nyse.g.1, 10)$standardDeviation


num = length(1001:length(nyse))

histc = 1:num
historic = 0
for (j in (1000:(length(nyse) - 1))) {
  #cut=ecdf(nyse[(j-250):j])(.01)
  cut = quantile(nyse[(j - 250):j], .01)
  histc[j - 1000 + 1] = cut
  if (nyse[j + 1] < cut) {
    historic = historic + 1
  }
}

historic / num

Riskc = 1:num

RiskMetrics = 0
lam = .94
sig1 = mean(nyse[(1000 - 250):999] ^ 2)
for (j in (1000:(length(nyse) - 1))) {
  signew = lam * sig1 + (1 - lam) * nyse[j] ^ 2
  sig1 = signew

  Riskc[j - 1000 + 1] = sqrt(signew) * qnorm(.01)

  if (nyse[j + 1] < sqrt(signew) * qnorm(.01)) {
    RiskMetrics = RiskMetrics + 1
  }
}
RiskMetrics / num

num = length((1001:length(nyse)))
gNorm = 0
gNormQ = 1:length((1001:length(nyse)))


gNonParam = 0
gNonParamQ = 1:length((1001:length(nyse)))


for (j in (1000:(length(nyse) - 1))) {
  nyse.g.1 = garchFit( ~ arma(0, 0) + garch(1, 1), nyse[1:(j)])

  error.dis = nyse[1:j] / nyse.g.1@sigma.t
  sigthat = predict(nyse.g.1, 1)$standardDeviation[1]

  #error.dis=residuals(nyse.g.1)
  #error.dis=(error.dis-mean(error.dis))/sd(error.dis)
  gNormQ[j - 1000 + 1] = sigthat * qnorm(0.01)

  gNonParamQ[j - 1000 + 1] = sigthat * quantile(error.dis, .01)

  if (nyse[j + 1] < sigthat * qnorm(0.01)) {
    gNorm = gNorm + 1
  }



  if (nyse[j + 1] < sigthat * quantile(error.dis, .01)) {
    gNonParam = gNonParam + 1
  }

}
gNorm / num
gNonParam / num








plot(nyse[1000:2000])
lines(1:1000, histc[1:1000])
lines(1:1000, Riskc[1:1000], col = 2)
lines(1:1000, gNormQ[1:1000], col = 3)
lines(1:1000, gNonParamQ[1:1000], col = 4)

#historical

plot(
  nyse[1001:2000],
  ylab = "NYSE Log Returns",
  main = paste(
    "Violation Rate = ",
    round(historic / num, 5) ,
    " Nominal Rate is 0.01, ",
    "Mean VaR = ",
    round(mean(histc), 4)
  )
)
lines(1:1000, histc[1:1000])


#Risk Metrics

plot(
  nyse[1001:2000],
  ylab = "NYSE Log Returns",
  main = paste(
    "Violation Rate = ",
    round(RiskMetrics / num, 5) ,
    " Nominal Rate is 0.01, " ,
    "Mean VaR = ",
    round(mean(Riskc[1:1000]), 4)
  )
)
lines(1:1000, Riskc[1:1000], col = 2)


#GARCH Normal Innovations


plot(
  nyse[1001:2000],
  ylab = "NYSE Log Returns",
  main = paste(
    "Violation Rate = ",
    round(gNorm / num, 5) ,
    " Nominal Rate is 0.01, " ,
    "Mean VaR = ",
    round(mean(gNormQ[1:1000]), 4)
  )
)
lines(1:1000, gNormQ[1:1000], col = 3)

#GARCH Bootstrap Innovations

plot(
  nyse[1001:2000],
  ylab = "NYSE Log Returns",
  main = paste(
    "Violation Rate = ",
    round(gNonParam / num, 5) ,
    " Nominal Rate is 0.01, ",
    "Mean VaR = ",
    round(mean(gNonParamQ[1:1000]), 4)
  )
)
lines(1:1000, gNonParamQ[1:1000], col = 4)
