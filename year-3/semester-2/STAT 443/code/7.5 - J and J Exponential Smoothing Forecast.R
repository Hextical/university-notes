library(fpp2)
library(astsa)

plot(jj)

x = ets(jj, model = "ANN")
autoplot(x)
y = forecast(x, 20)
autoplot(y)


x = ets(jj, model = "AAN")
autoplot(x)
y = forecast(x, 20)
autoplot(y)

frequency(jj)

x = ets(jj, model = "AAA")
autoplot(x)
y = forecast(x, 20)
autoplot(y)

x = ets(jj, model = "MAA")
y = forecast(x, 20)
autoplot(y)

x = ets(jj, model = "MAM")
y = forecast(x, 20)
autoplot(y)

x = ets(jj, model = "MMM")
y = forecast(x, 20)
autoplot(y)

x = ets(jj)
x
autoplot(x)
checkresiduals(x)
y = forecast(x, 20)
autoplot(y)

x = ets(jj, model = "ANN")
x
autoplot(x)
checkresiduals(x)
y = forecast(x, 20)
autoplot(y)
