### STAT 331 simple linear regression demo

dat <- read.csv("florange.csv")

head(dat)

# Scatterplot
plot(dat$acres,dat$boxes)

# Summary statistics calculation examples
r <- cor(dat$acres,dat$boxes)
xbar <- mean(dat$acres)
ybar <- mean(dat$boxes)
sd_x <- sd(dat$acres)
sd_y <- sd(dat$boxes)

# Manual calculation examples
Sxx <- sum( (dat$acres - xbar)^2 )
Sxy <- sum( (dat$acres - xbar) * (dat$boxes - ybar) )

# R's "lm" function fits linear models
lm.1 <- lm(dat$boxes~dat$acres)
summary(lm.1)

# Fitted values
lm.1$fitted.values

# Residuals
lm.1$residuals

# Manual calculation of sigma^2 estimate
sum(lm.1$residuals^2) / 23
# or sigma estimate
sqrt(sum(lm.1$residuals^2) / 23)

# t distribution values
qt(0.975,23)
(1-pt(17.263,23))*2

# Discussion
# - is sigma plausibly the same for all values of y? --> appears to be violated, can consider
# taking the log
# - are the error terms plausibly independent? (e.g., does knowing one e_i help predict
# e_j for a different county?)

