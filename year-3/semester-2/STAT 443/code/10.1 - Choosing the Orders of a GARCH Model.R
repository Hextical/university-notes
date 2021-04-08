library(fGarch)

nyse = astsa::nyse

plot(nyse)

summary(nyse.g10 <- garchFit(~ arma(0, 0) + garch(1, 0), nyse))

summary(nyse.g11 <- garchFit(~ arma(0, 0) + garch(1, 1), nyse))

summary(nyse.g12 <- garchFit(~ arma(0, 0) + garch(1, 2), nyse))

summary(nyse.g21 <- garchFit(~ arma(0, 0) + garch(2, 1), nyse))

summary(nyse.g22 <- garchFit(~ arma(0, 0) + garch(2, 2), nyse))

u = nyse.g11@sigma.t

for (j in (935:960)) {
  par(mfrow = c(2, 1))
  plot(window(nyse, start = 900, end = j),
       ylim = c(-.22, .2),
       ylab = "NYSE Returns")
  plot(u[(900 + 1):(j + 1)], ylab = "1-step Volatility prediction", ylim =
         c(0, max(u)))
  invisible(readline(prompt = "Press [enter] to continue"))
}
