##CROSS VALIDATION

cmort2 = ts(cmort2, frequency = 13)
plot(cmort2)


tr = round(length(cmort2) * 0.75)
cv00 = 1:length(tr:(length(cmort2) - 1))
cv11 = 1:length(tr:(length(cmort2) - 1))
cv01 = 1:length(tr:(length(cmort2) - 1))
cv10 = 1:length(tr:(length(cmort2) - 1))
cvAUTOAR = 1:length(tr:(length(cmort2) - 1))

for (j in (tr:(length(cmort2) - 1))) {
  print(j)
  #x1=sarima.for(cmort2[1:j],1,y.aic00[1],0,y.aic00[2],y.aic00[3],0,y.aic00[4],13)
  x00 = tryCatch(
    sarima.for(cmort2[1:j], 1, y.aic00[1], 0, y.aic00[2], y.aic00[3], 0, y.aic00[4], 13),
    error = function(e) {
      x = list()
      x$pred = cmort2[j]
      return(x)
    }
  )
  x10 = tryCatch(
    sarima.for(cmort2[1:j], 1, y.aic10[1], 1, y.aic10[2], y.aic10[3], 0, y.aic10[4], 13),
    error = function(e) {
      x = list()
      x$pred = cmort2[j]
      return(x)
    }
  )
  x01 = tryCatch(
    sarima.for(cmort2[1:j], 1, y.aic01[1], 0, y.aic01[2], y.aic01[3], 1, y.aic01[4], 13),
    error = function(e) {
      x = list()
      x$pred = cmort2[j]
      return(x)
    }
  )
  x11 = tryCatch(
    sarima.for(cmort2[1:j], 1, y.aic11[1], 1, y.aic11[2], y.aic11[3], 1, y.aic11[4], 13),
    error = function(e) {
      x = list()
      x$pred = cmort2[j]
      return(x)
    }
  )
  cmort2j = ts(cmort2[1:j], frequency = 13)
  xAAR = forecast(auto.arima(cmort2j), h = 1)$mean
  cvAUTOAR[j - tr + 1] = xAAR - cmort2[j + 1]
  cv00[j - tr + 1] = x00$pred - cmort2[j + 1]
  cv10[j - tr + 1] = x10$pred - cmort2[j + 1]
  cv01[j - tr + 1] = x01$pred - cmort2[j + 1]
  cv11[j - tr + 1] = x11$pred - cmort2[j + 1]
}

#One year ahead
h = 13
tr = round(length(cmort2) * 0.70)
cv00h = 1:length(tr:(length(cmort2) - 1 - h))
cv11h = 1:length(tr:(length(cmort2) - 1 - h))
cv01h = 1:length(tr:(length(cmort2) - 1 - h))
cv10h = 1:length(tr:(length(cmort2) - 1 - h))

for (j in (tr:(length(cmort2) - 1 - h))) {
  print(j)
  #x1=sarima.for(cmort2[1:j],1,y.aic00[1],0,y.aic00[2],y.aic00[3],0,y.aic00[4],13)
  x00 = tryCatch(
    sarima.for(cmort2[1:j], h, y.aic00[1], 0, y.aic00[2], y.aic00[3], 0, y.aic00[4], 13),
    error = function(e) {
      x = list()
      x$pred = rep(cmort2[j], h)
      return(x)
    }
  )
  x10 = tryCatch(
    sarima.for(cmort2[1:j], h, y.aic10[1], 1, y.aic10[2], y.aic10[3], 0, y.aic10[4], 13),
    error = function(e) {
      x = list()
      x$pred = rep(cmort2[j], h)
      return(x)
    }
  )
  x01 = tryCatch(
    sarima.for(cmort2[1:j], h, y.aic01[1], 0, y.aic01[2], y.aic01[3], 1, y.aic01[4], 13),
    error = function(e) {
      x = list()
      x$pred = rep(cmort2[j], h)
      return(x)
    }
  )
  x11 = tryCatch(
    sarima.for(cmort2[1:j], h, y.aic11[1], 1, y.aic11[2], y.aic11[3], 1, y.aic11[4], 13),
    error = function(e) {
      x = list()
      x$pred = rep(cmort2[j], h)
      return(x)
    }
  )
  cv00h[j - tr + 1] = sum((x00$pred - cmort2[(j + 1):(j + h)]) ^ 2)
  cv10h[j - tr + 1] = sum((x10$pred - cmort2[(j + 1):(j + h)]) ^ 2)
  cv01h[j - tr + 1] = sum((x01$pred - cmort2[(j + 1):(j + h)]) ^ 2)
  cv11h[j - tr + 1] = sum((x11$pred - cmort2[(j + 1):(j + h)]) ^ 2)
}


mean(cv00)
mean(cv10)
mean(cv01)
mean(cv11)
mean(cvAUTOAR)
sum(cv00 ^ 2)
sum(cv10 ^ 2)
sum(cv01 ^ 2)
sum(cv11 ^ 2)
sum(cvAUTOAR ^ 2)

mean(cv00h)
mean(cv10h)
mean(cv01h)
mean(cv11h)
