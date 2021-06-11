## Candy Crush F-test Example

## Change working directory
setwd(dir = "/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")

## Read in the data
candy <- read.csv(file = "candycrush.csv", header = T)

## Plot the data
boxplot(time ~ booster, data = candy, xaxt = "n", ylab = "Length of Game Play (minutes)", xlab = "Condition")
axis(side = 1, at = c(1,2,3), labels = c("Lollipop Hammer", "Jelly Fish", "Color Bomb"))

## Perform the test by fitting a linear regression model
model <- lm(time ~ factor(booster), data = candy)
summary(model)
anova(model)
anova(model)$`Pr(>F)`

## Perform the test by calculate sums of squares manually
N <- nrow(candy)
m <- 3
cond1 <- candy$time[candy$booster == 1]
n1 <- length(cond1)
cond2 <- candy$time[candy$booster == 2]
n2 <- length(cond2)
cond3 <- candy$time[candy$booster == 3]
n3 <- length(cond3)
SSC <- n1*(mean(cond1) - mean(candy$time))^2 + n2*(mean(cond2) - mean(candy$time))^2 + n3*(mean(cond3) - mean(candy$time))^2 
SSE <- sum((cond1 - mean(cond1))^2) + sum((cond2 - mean(cond2))^2) + sum((cond3 - mean(cond3))^2)
t <- (SSC/(m-1)) / (SSE/(N-m))
pv <- pf(q = t, df1 = m-1, df2 = N-m, lower.tail = FALSE)
SSC
SSE
t
pv

## Check model assumptions 
par(mfrow=c(1,3))
hist(model$residuals, xlab = "Residials", main = "Histogram of Residuals")
qqnorm(model$residuals)
qqline(model$residuals, col = "red")
plot(x = model$fitted.values, y = model$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
