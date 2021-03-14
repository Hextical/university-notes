library(fGarch)
mod = garchSpec()

mod

x = garchSim(spec = mod, n = 1000)

plot(x)

acf(x)

acf(x ^ 2)
