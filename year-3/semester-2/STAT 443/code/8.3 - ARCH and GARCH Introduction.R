library(astsa)
library(fGarch)
library(rugarch)
library(WeightedPortTest)

library(astsa)

nyse = astsa::nyse

plot(nyse, main = "Returns of the New York Stock Exchange Index, 1984-1991")

plot(nyse - mean(nyse))
abline(h = 2 * sd(nyse),
       lty = 2,
       col = 3)
abline(h = -2 * sd(nyse),
       lty = 2,
       col = 3)

plot(nyse[750:1250] - mean(nyse))
abline(h = 2 * sd(nyse),
       lty = 2,
       col = 3)
abline(h = -2 * sd(nyse),
       lty = 2,
       col = 3)

acf(nyse)


acf(nyse ^ 2)
