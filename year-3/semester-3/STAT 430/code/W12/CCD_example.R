## Central Composite Design Example

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

## Load the data associated with the CCD experiment
setwd("/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")
lyft <- read.csv("lyft.csv", header = TRUE)


## The data correspond to a CCD on two factors with axial conditions based on a = 1.4
## The loaded data is in coded units. The "low" and "high" values of each factor in
## natural units is as follows:
##   x1: Discount amount (%):      25 vs 75
##   x2: Discount duration (days): 2  vs 7

condition <- data.frame(x1 = convert.C.to.N(x = c(-1,-1,1,1,0,1.4,-1.4,0,0), UH = 75, UL = 25), 
                        x2 = convert.C.to.N(x = c(-1,1,-1,1,0,0,0,1.4,-1.4), UH = 7, UL = 2))

## Calculate the booking rate in each condition
pi_hat <- aggregate(x = lyft$y, by = list(condition.num = kronecker(1:9, rep(1, 500))), FUN = mean)
data.frame(Condition.Num = pi_hat$condition.num, 
           Disc.Amnt = condition$x1, 
           Disc.Dur = condition$x2,
           Booking.Rate = pi_hat$x)

## We then fit the full 2nd-order response surface
model <- glm(y ~ x1 + x2 + x1*x2 + I(x1^2) + I(x2^2), data = lyft, family = binomial(link = "logit"))
summary(model)

## Let's visualize this surface:
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]
beta2 <- coef(model)[3]
beta12 <- coef(model)[6]
beta11 <- coef(model)[4]
beta22 <- coef(model)[5]
grd <- mesh(x = seq(convert.N.to.C(U = 0, UH = 75, UL = 25), 
                    convert.N.to.C(U = 100, UH = 75, UL = 25), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 0, UH = 7, UL = 2), 
                    convert.N.to.C(U = 10, UH = 7, UL = 2), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.so <- beta0 + beta1*x1 + beta2*x2 + beta12*x1*x2 + beta11*x1^2 + beta22*x2^2
pi.so <- exp(eta.so)/(1+exp(eta.so))

# 2D contour plot (coded units)
contour(x = seq(convert.N.to.C(U = 0, UH = 75, UL = 25), 
                convert.N.to.C(U = 100, UH = 75, UL = 25), 
                length.out = 100), 
        y = seq(convert.N.to.C(U = 0, UH = 7, UL = 2), 
                convert.N.to.C(U = 10, UH = 7, UL = 2), 
                length.out = 100), 
        z = pi.so, xlab = "x1", ylab = "x2",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

## Let's find the maximum of this surface and the corresponding factor levels 
## at which this is achieved
b <- matrix(c(beta1,beta2), ncol = 1)
B <- matrix(c(beta11, 0.5*beta12, 0.5*beta12, beta22), nrow = 2, ncol = 2)
x.s <- -0.5*solve(B) %*% b 
points(x = x.s[1], y = x.s[2], col = "red", pch = 16)

# The predicted book rate at this configuration is:
eta.s <- beta0 + 0.5*t(x.s) %*% b
exp(eta.s)/(1+exp(eta.s))

# In natural units this optimum is located at
convert.C.to.N(x = x.s[1,1], UH = 75, UL = 25)
convert.C.to.N(x = x.s[2,1], UH = 7, UL = 2)

# Remake the contour plot but in natural units
contour(x = seq(0, 100, length.out = 100), 
        y = seq(0, 10, length.out = 100), 
        z = pi.so, xlab = "Discount Amount (%)", ylab = "Discount Duration (Days)",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s[1,1], UH = 75, UL = 25),
       y = convert.C.to.N(x = x.s[2,1], UH = 7, UL = 2), 
       col = "red", pch = 16)

points(x = 50, y = 2, pch = 16, col = "green")

## 95% prediction interval at this optimum:
n.data <- data.frame(x1=x.s[1,1], x2=x.s[2,1])
pred <- predict(model, newdata = n.data, type = "response", se.fit = TRUE)
pred 
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))

## 95% prediction interval at convenient near-optimum:
n.data <- data.frame(x1=0, x2=-1)
pred <- predict(model, newdata = n.data, type = "response", se.fit = TRUE)
pred
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))

## Thus, it seems like a promotional offer associated with a 50% discount offered for 2 days results in an
## optimal booking rate (~80%)
