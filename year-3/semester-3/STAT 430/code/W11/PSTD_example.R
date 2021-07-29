## Path of Steepest Descent Example

# Function to create blues
blue_palette <- colorRampPalette(c(rgb(247,251,255,maxColorValue = 255), rgb(8,48,107,maxColorValue = 255)))

# Function for converting from natural units to coded units
convert.N.to.C <- function(U,UH,UL){
  x <- (U - (UH+UL)/2) / ((UH-UL)/2)
  return(x)
}

# Function for converting from coded units to natural units
convert.C.to.N <- function(x,UH,UL){
  U <- x*((UH-UL)/2) + (UH+UL)/2
  return(U)
}

# Function to create x and y grids for contour plots 
mesh <- function(x, y) { 
  Nx <- length(x)
  Ny <- length(y)
  list(
    x = matrix(nrow = Nx, ncol = Ny, data = x),
    y = matrix(nrow = Nx, ncol = Ny, data = y, byrow = TRUE)
  )
}

## Load the data associated with the 2^2 + cp experiment
setwd("/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")
netflix.ph1 <- read.csv("2^2+cp.csv", header = TRUE)

## The factors and their low/center/high levels are as follows:
## Preview Length: 90  vs 105  vs 120
## Preview Size:   0.2 vs 0.35 vs 0.5

## The number of units in each of the 5 conditions is:
table(netflix.ph1$Prev.Length, netflix.ph1$Prev.Size)

## Determine whether we're close to the optimum to begin with
## (i.e, check whether the pure quadratic effect is significant)
ph1 <- data.frame(y = netflix.ph1$Browse.Time,
                  x1 = convert.N.to.C(U = netflix.ph1$Prev.Length, UH = 120, UL = 90),
                  x2 = convert.N.to.C(U = netflix.ph1$Prev.Size, UH = 0.5, UL = 0.2))
ph1$xPQ <- (ph1$x1^2 + ph1$x2^2)/2

## Check the average browsing time in each condition:
aggregate(ph1$y, by = list(x1 = ph1$x1, x2 = ph1$x2), FUN = mean)

## The difference in average browsing time in factorial conditions vs. the center 
## point condition
mean(ph1$y[ph1$xPQ != 0]) - mean(ph1$y[ph1$xPQ == 0])


## Check to see if that's significant
m <- lm(y~x1+x2+x1*x2+xPQ, data = ph1)
summary(m)

## It isn't, so we're in a flat area of the response surface. We should
## perform a steepest descent phase.

## Fit the first order model to determine the direction of the path of 
## steepest descent
m.fo <- lm(y~x1+x2, data = ph1)
beta0 <- coef(m.fo)[1]
beta1 <- coef(m.fo)[2]
beta2 <- coef(m.fo)[3]
grd <- mesh(x = seq(convert.N.to.C(U = 30, UH = 120, UL = 90), 
                    convert.N.to.C(U = 120, UH = 120, UL = 90), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 0.2, UH = 0.5, UL = 0.2), 
                    convert.N.to.C(U = 0.8, UH = 0.5, UL = 0.2), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.fo <- beta0 + beta1*x1 + beta2*x2
# 2D contour plot
contour(x = seq(convert.N.to.C(U = 30, UH = 120, UL = 90), 
                convert.N.to.C(U = 120, UH = 120, UL = 90), 
                length.out = 100),
        y = seq(convert.N.to.C(U = 0.2, UH = 0.5, UL = 0.2), 
                convert.N.to.C(U = 0.8, UH = 0.5, UL = 0.2), 
                length.out = 100), 
        z = eta.fo, xlab = "x1 (Preview Length)", ylab = "x2 (Preview Size)",
        nlevels = 15, col = blue_palette(15), labcex = 0.9, asp=1)
abline(a = 0, b = beta2/beta1, lty = 2)
points(x = 0, y = 0, col = "red", pch = 16)

## Calculate the coordinates along this path that we will experiment at

# The gradient vector
g <- matrix(c(beta1, beta2), nrow = 1)

# We will take steps of size 5 seconds in preview length. In coded units this is
PL.step <- convert.N.to.C(U = 105 + 5, UH = 120, UL = 90)
lamda <- PL.step/abs(beta1)

## Step 0: The center point we've already observed
x.old <- matrix(0, nrow=1, ncol=2)
text(x = 0, y = 0+0.25, labels = "0")
step0 <- data.frame(Prev.Length = convert.C.to.N(x = 0, UH = 120, UL = 90), 
                 Prev.Size = convert.C.to.N(x = 0, UH = 0.5, UL = 0.2))

## Step 1: 
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "1")
step1 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 90), 
                    Prev.Size = convert.C.to.N(x = x.new[1,2], UH = 0.5, UL = 0.2))

## Step 2: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "2")
step2 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 90), 
                    Prev.Size = convert.C.to.N(x = x.new[1,2], UH = 0.5, UL = 0.2))

## Step 3: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "3")
step3 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 90), 
                    Prev.Size = convert.C.to.N(x = x.new[1,2], UH = 0.5, UL = 0.2))

## Step 4: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "4")
step4 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 90), 
                    Prev.Size = convert.C.to.N(x = x.new[1,2], UH = 0.5, UL = 0.2))

## Step 5: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "5")
step5 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 90), 
                    Prev.Size = convert.C.to.N(x = x.new[1,2], UH = 0.5, UL = 0.2))

## Step 6: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "6")
step6 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 90), 
                    Prev.Size = convert.C.to.N(x = x.new[1,2], UH = 0.5, UL = 0.2))

## Step 7:
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "7")
step7 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 90), 
                    Prev.Size = convert.C.to.N(x = x.new[1,2], UH = 0.5, UL = 0.2))

## The following is a list of the conditions along the path of steepest descent
pstd.cond <- data.frame(Step = 0:7, rbind(step0, step1, step2, step3, step4, step5, step6, step7))
pstd.cond

## Load the data associated with the steepest descent search
netflix.ph2 <- read.csv("pstd.csv", header = TRUE)

## Calculate the average browsing time in each of these conditions and find the 
## condition that minimizes it
pstd.means <- aggregate(netflix.ph2$Browse.Time, 
                        by = list(Prev.Length = netflix.ph2$Prev.Length, 
                                  Prev.Size = netflix.ph2$Prev.Size), 
                        FUN = mean)

plot(x = 0:7, y = pstd.means$x,
     type = "l", xlab = "Step Number", ylab = "Average Browsing Time")
points(x = 0:7, y = pstd.means$x,
       col = "red", pch = 16)

## Clearly average browsing time was minimized at Step 6
pstd.cond[pstd.cond$Step == 6,]

## We should follow this up with 2^2 factorial conditions to ensure we're close to optimum
## We will re-center our coded scale in this new region as follows:
## Preview Length: 60  vs 75  vs 90
## Preview Size:   0.6 vs 0.7 vs 0.8

## Load this data and check whether the pure quadratic effect is significant
netflix.ph2.5 <- read.csv("2^2+cp_second_time.csv", header = TRUE)
ph2.5 <- data.frame(y = netflix.ph2.5$Browse.Time,
                  x1 = convert.N.to.C(U = netflix.ph2.5$Prev.Length, UH = 90, UL = 60),
                  x2 = convert.N.to.C(U = netflix.ph2.5$Prev.Size, UH = 0.8, UL = 0.6))
ph2.5$xPQ <- (ph2.5$x1^2 + ph2.5$x2^2)/2

## Check the average browsing time in each condition:
aggregate(ph2.5$y, by = list(x1 = ph2.5$x1, x2 = ph2.5$x2), FUN = mean)

## The difference in average browsing time in factorial conditions vs. the center 
## point condition
mean(ph2.5$y[ph2.5$xPQ != 0]) - mean(ph2.5$y[ph2.5$xPQ == 0])

## Check to see if that's significant
m <- lm(y~x1+x2+x1*x2+xPQ, data = ph2.5)
summary(m)

## Yes, it is significant and so there is significant quadratic curvature in
## this region of the response surface. We should now commence phase 3 and 
## perform a respond surface design and fit a full second order model.
