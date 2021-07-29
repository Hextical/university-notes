## Response Surface Visualizations

library(plotly) # needed for interactive 3D plots

# Function to create x and y grids for contour and surface plots 
mesh <- function(x, y) { 
  Nx <- length(x)
  Ny <- length(y)
  list(
    x = matrix(nrow = Nx, ncol = Ny, data = x),
    y = matrix(nrow = Nx, ncol = Ny, data = y, byrow = TRUE)
  )
}

# Set up x-space and define colour palette
grd <- mesh(x = seq(-10, 10, length.out = 100), y = seq(-10, 10, length.out = 100))
x1 <- grd$x
x2 <- grd$y
blue_palette <- colorRampPalette(c(rgb(247,251,255,maxColorValue = 255), rgb(8,48,107,maxColorValue = 255)))

## First-Order Surface
z.fo <- 0.5*x1 + 0.5*x2

# 2D contour plot
contour(x = seq(-10, 10, length.out = 100), xlab = "x1",
        y = seq(-10, 10, length.out = 100), ylab = "x2",
        z = z.fo, 
        nlevels = 10, col = blue_palette(10), labcex = 0.9)


# 3D surface plot
fig.fo <- plot_ly(x = seq(-10, 10, length.out = 100), 
                  y = seq(-10, 10, length.out = 100), 
                  z = z.fo, colors = "Blues", showscale = FALSE) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig.fo <- fig.fo %>% layout(
  scene = list(
    camera=list(eye = list(x=-1.25, y=-1.25, z=1.25)),
    xaxis = list(title = "x1"),
    yaxis = list(title = "x2")
  )
)
fig.fo

## First-Order Surface + Interaction
z.foi <- 1 + 0.2*x1 + 0.2*x2 + 0.5*x1*x2

# 2D contour plot
contour(x = seq(-10, 10, length.out = 100), xlab = "x1",
        y = seq(-10, 10, length.out = 100), ylab = "x2",
        z = z.foi, 
        nlevels = 10, col = blue_palette(10), labcex = 0.9)

# 3D surface plot
fig.foi <- plot_ly(x = seq(-10, 10, length.out = 100), 
                  y = seq(-10, 10, length.out = 100), 
                  z = z.foi, colors = "Blues", showscale = FALSE) %>% add_surface(
                    contours = list(
                      z = list(
                        show=TRUE,
                        usecolormap=TRUE,
                        highlightcolor="#ff0000",
                        project=list(z=TRUE)
                      )
                    )
                  )
fig.foi <- fig.foi %>% layout(
  scene = list(
    camera=list(eye = list(x=-1.25, y=-1.25, z=1.25)),
    xaxis = list(title = "x1"),
    yaxis = list(title = "x2")
  )
)
fig.foi

## Second-Order Surface
z.so <- 1 + 0.02*x1 + 0.02*x2 + 0.05*x1*x2 - 0.07*x1^2 - 0.07*x2^2

# 2D contour plot
contour(x = seq(-10, 10, length.out = 100), xlab = "x1",
        y = seq(-10, 10, length.out = 100), ylab = "x2",
        z = z.so, 
        nlevels = 10, col = blue_palette(10), labcex = 0.9)

# 3D surface plot
fig.so <- plot_ly(x = seq(-10, 10, length.out = 100), 
                   y = seq(-10, 10, length.out = 100), 
                   z = z.so, colors = "Blues", showscale = FALSE) %>% add_surface(
                     contours = list(
                       z = list(
                         show=TRUE,
                         usecolormap=TRUE,
                         highlightcolor="#ff0000",
                         project=list(z=TRUE)
                       )
                     )
                   )
fig.so <- fig.so %>% layout(
  scene = list(
    camera=list(eye = list(x=-1.25, y=-1.25, z=1.25)),
    xaxis = list(title = "x1"),
    yaxis = list(title = "x2")
  )
)
fig.so



