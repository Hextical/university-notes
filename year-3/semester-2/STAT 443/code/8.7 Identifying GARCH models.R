# autocorrelation function

library(astsa)
nyse = astsa::nyse

plot(nyse, main = "Returns of the New York Stock Exchange Index, 1984-1991")

acf(nyse)



gamma <- function(x, h) {
  n <- length(x)
  h <- abs(h)
  x <- x - mean(x)
  gamma <- sum(x[1:(n - h)] * x[(h + 1):n]) / n
}
rho <- function(x, h)
  rho <- gamma(x, h) / gamma(x, 0)
# acf function with significance bands of a strong white noise
nl.acf <- function(x, main = NULL, method = 'NP') {
  n <- length(x)
  nlag <- as.integer(min(10 * log10(n), n - 1))
  acf.val <- sapply(c(1:nlag), function(h)
    rho(x, h))
  x2 <- x ^ 2
  var <- 1 + (sapply(c(1:nlag), function(h)
    gamma(x2, h))) / gamma(x, 0) ^ 2
  band <- sqrt(var / n)
  minval <- 1.2 * min(acf.val, -1.96 * band, -1.96 / sqrt(n))
  maxval <- 1.2 * max(acf.val, 1.96 * band, 1.96 / sqrt(n))
  acf(
    x,
    xlab = 'Lag',
    ylab = 'SACR',
    ylim = c(minval, maxval),
    main = main
  )
  lines(c(1:nlag), -1.96 * band, lty = 1, col = 'red')
  lines(c(1:nlag), 1.96 * band, lty = 1, col = 'red')
}

nl.acf(nyse)
