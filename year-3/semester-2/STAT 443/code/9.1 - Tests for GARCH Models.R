# Load 8.3 first.

library(fGarch)

plot(nyse)

acf(nyse)

Box.test(nyse, 20, "Ljung-Box")

Box.test(nyse^2, 20, "Ljung-Box")

summary(nyse.g <- garchFit(~arma(0,0) + garch(1,0), nyse))

summary(nyse.g <- garchFit(~arma(0,0) + garch(1,1), nyse))
