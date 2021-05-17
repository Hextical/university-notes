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
    TeX(r'($p$-value, we do not reject $H_0$)'),
    TeX(r'($p$-value, we reject $H_0$)'),
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
