# 6.5 x 8.5, landscape
library(latex2exp)
x <- seq(-4,4,by = 0.01)
area_poly <- function(cur, cutoff, side=c(1,-1), col = "grey", border=NA, ...) {
  if (side[1]>0 )# on the right
  {
    pos <- min(which(cur$x > cutoff))
    end <- length(cur$x)
  }
  else # on the left
  {
    pos <- max(which(cur$x < cutoff))
    end <- 1
  }
  polygon(x=c(cur$x[end:pos], cur$x[pos], cur$x[end]),
          y=c(cur$y[end:pos], 0, 0), col=col, border=border, ...)
}

cc <- curve(dnorm(x), from = -4, to = 4, type = "l", xaxt='n', yaxt='n', ann=FALSE, bty = "n")

area_poly(cc, cutoff = 2, side = 1, col = "chartreuse4")
area_poly(cc, cutoff = -2, side = 0, col = "chartreuse4")

area_poly(cc, cutoff = 1.5, side = 1, col = "purple", density = 20)
area_poly(cc, cutoff = -1.5, side = 0, col = "purple", density = 20)

area_poly(cc, cutoff = 2.5, side = 1, col = "blue")
area_poly(cc, cutoff = -2.5, side = 0, col = "blue")
text(0,0.2, TeX(r'($1-\alpha$)'), cex = 1.5, col = "chartreuse4")
text(2.5, 0.04, TeX(r'($\alpha/2$)'), col = "chartreuse4")
text(-2.5, 0.04, TeX(r'($\alpha/2$)'), col = "chartreuse4")

text(-2.6, -0.011, TeX(r'($-|t|$)'), col = "blue")
text(-2.1, -0.011, TeX(r'($-z_{\alpha/2}$)'), col = "chartreuse4")
text(-1.6, -0.011, TeX(r'($-|t|$)'), col = "purple")

text(2.6, -0.011, TeX(r'($|t|$)'), col = "blue")
text(2.1, -0.011, TeX(r'($z_{\alpha/2}$)'), col = "chartreuse4")
text(1.6, -0.011, TeX(r'($|t|$)'), col = "purple")

text(0, -0.01, TeX(r'($0$)'), col = "red")

legend(
  "topright",
  legend = c(
    TeX(r'($p$-value, we do not reject $\mathbf{H}_0$)'),
    TeX(r'($p$-value, we reject $\mathbf{H}_0$)'),
    TeX(r'(Rejection Region)')
  ),
  fill = c("purple", "blue", "chartreuse4"), bty = "n"
)

cc <- curve(dnorm(x), from = -4, to = 4, type = "l", xaxt='n', yaxt='n', ann=FALSE, bty = "n")
area_poly(cc, cutoff = 2, side = 1, col = "chartreuse4")
legend(
  "topright",
  legend = c(
    TeX(r'(Rejection Region)')
  ),
  fill = c("chartreuse4"), bty = "n"
)
text(0,0.2, TeX(r'($1-\alpha$)'), cex = 1.5, col = "chartreuse4")
text(2.6, 0.04, TeX(r'($\alpha$)'), col = "chartreuse4")
text(2.1, -0.01, TeX(r'($z_\alpha$)'), col = "chartreuse4")

text(0, -0.01, TeX(r'($0$)'), col = "red")

cc <- curve(dnorm(x), from = -4, to = 4, type = "l", xaxt='n', yaxt='n', ann=FALSE, bty = "n")
area_poly(cc, cutoff = -2, side = 0, col = "chartreuse4")
legend(
  "topright",
  legend = c(
    TeX(r'(Rejection Region)')
  ),
  fill = c("chartreuse4"), bty = "n"
)
text(0,0.2, TeX(r'($1-\alpha$)'), cex = 1.5, col = "chartreuse4")
text(-2.6, 0.04, TeX(r'($\alpha$)'), col = "chartreuse4")
text(-2.1, -0.01, TeX(r'($-z_\alpha$)'), col = "chartreuse4")

text(0, -0.01, TeX(r'($0$)'), col = "red")

M <- seq(1, 100)
plot(M, 1 - (1 - 0.05) ^ M, type = "l", col = "darkblue", lwd = 2,
     ylab = "Family-wise Error Rate (FWER)", xlab = "Number of Tests (M)")
abline(a = 0.05, b = 0, col = "red", lwd = 2, lty = 2)
text(85, 0.1, TeX(r'($\alpha$)'))
text(93, 0.105, "=0.05")

p <- c(0.015, 0.029, 0.008, 0.026)
plot(rank(p), p, pch = 20,
     xlab = "Rank (k)",
     ylab = "Sorted p-values",
     xlim = c(1, 4), ylim = c(0,0.05),
     xaxt = "n")
axis(1, at = seq(1, 4))
M <- 4
alpha_star <- 0.05
k <- seq(1, 4, by = 0.01)
lines(k, alpha_star / (M - k + 1),
      col = "red", lty = 2, lwd = 1.5)
lines(k, rep(alpha_star / M, times = length(k)),
      col = "blue", lty = 2, lwd = 1.5)
lines(k, rep(1 - (1 - alpha_star) ^ (1 / M),  times = length(k)),
      col = "chartreuse4", lty = 2, lwd = 1.5)
legend("topleft", legend = c("Bonferroni", "Sidak", "Holmes"),
       col = c("blue", "green", "red"),
       lty = 2, lwd = 1.5, bty = "n")

library(latex2exp)
x <- seq(0, 0.5, by = 0.01)
plot(
  x,
  1 - exp(-x),
  ylim = c(0, 0.5),
  xlab = TeX(r'($\alpha^*$)'),
  ylab = TeX(r'($1-e^{-\alpha^*}$)'),
  type = "l",
  col = "darkblue"
)
lines(x,
      x,
      type = "l",
      col = "red",
      lty = 2)
legend(
  "bottomright",
  legend = c("Asym. Error Rate", "Line of Equality"),
  col = c("darkblue", "red"),
  lty = c(1, 2),
  bty = "n"
)

## The Four-Test Example

## These were the p-values
p1 <- 0.015
p2 <- 0.029
p3 <- 0.008
p4 <- 0.026

## The plot of ordered p-values vs. rank + significance thresholds is created below:

# Define some useful variables:
alpha.star <- 0.05
p <- c(p1, p2, p3, p4)
M <- length(p)
rank <- seq(0, M + 1, 0.001)

# Make the plot:
plot(
  x = 1:M,
  y = sort(p),
  pch = 16,
  ylab = "Sorted p-values",
  xlab = "Rank (k)",
  ylim = c(0, max(alpha.star, max(p))),
  xaxt = "n"
)
axis(side = 1,
     at = 1:M,
     labels = 1:M)

# Holm's threshold:
lines(
  x = rank,
  y = alpha.star / (M - rank + 1),
  col = "red",
  lty = 2
)

# Bonferroni's threshold:
abline(h = alpha.star / M, col = "blue", lty = 2)

# Šidák's threshold
abline(h = 1 - (1 - alpha.star) ^ (1 / M),
       col = "green",
       lty = 2)

# BH threshold
abline(a = 0,
       b = alpha.star / M,
       col = "purple",
       lty = 2)

# Legend:
legend(
  "topleft",
  legend = c("Bonferroni", "Sidak", "Holmes"),
  bty = "n",
  col = c("blue", "green", "red", "purple"),
  lty = 2,
  cex = 0.7
)
