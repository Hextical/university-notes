f1 <- function (x, y) {
  return (0.5 * x + 0.5 * y)
}
f2 <- function (x, y) {
  return (1 + 0.2*x + 0.2*y + 0.5*x*y)
}
f3 <- function (x, y) {
  return (1 + 0.02*x + 0.02*y + 0.05*x*y - 0.07*x^2 - 0.07*y^2)
}

## persp example code
par(bg = "white")
x <- seq(-10, 10, length = 30)
y <- x
z <- outer(x, y, f2)
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette(c("lightblue", "blue"))
# Generate the desired number of colors from this palette
nbcol <- 50
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- (z[-1,-1] + z[-1,-ncz] + z[-nrz,-1] + z[-nrz,-ncz]) / 4
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(
  x,
  y,
  z,
  col = color[facetcol],
  phi = 30,
  theta = -45,
  border = NA,
  axes = F
)
contour(x, y, z)
