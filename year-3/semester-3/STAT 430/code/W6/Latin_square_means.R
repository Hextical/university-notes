## Analyzing a Latin Square Design to Compare Means

## Read-in the data
netflix <- read.csv("netflix-lsd.csv", header = TRUE)

## Response by condition
par(mfrow=c(1,2))
boxplot(Y ~ Condition, data = netflix, xlab = "Condition", ylab = "Latency (ms)", xaxt = "n", ylim = c(0,100))
axis(side = 1, at = 1:4, labels = c("A", "B", "C", "D"))
mean.condition <- aggregate(x = netflix$Y, by = list(Condition = netflix$Condition), FUN = mean)
mean.condition
plot(x = 1:4, y = mean.condition$x, xlab = "Condition", ylab = "Avg. Latency (ms)", pch = 16, xaxt = "n", ylim = c(0,100))
lines(x = 1:4, y = mean.condition$x)
axis(side = 1, at = 1:4, labels = c("A", "B", "C", "D"))

## Response by browser
boxplot(Y ~ Browser, data = netflix, xlab = "Browser", ylab = "Latency (ms)", ylim = c(0,100))
mean.browser <- aggregate(x = netflix$Y, by = list(Browser = netflix$Browser), FUN = mean)
mean.browser
plot(x = 1:4, y = mean.browser$x, xlab = "Browser", ylab = "Avg. Latency (ms)", pch = 16, xaxt = "n", ylim = c(0,100))
lines(x = 1:4, y = mean.browser$x)
axis(side = 1, at = 1:4, labels = c("Chrome", "Edge", "Firefox", "Safari"))

## Response by time of day
boxplot(Y ~ factor(Time), data = netflix, xlab = "Time of Day", ylab = "Latency (ms)", ylim = c(0,100), xaxt = "n")
axis(side = 1, at = 1:4, labels = c("00:01-06:00", "06:01-12:00", "12:01-18:00", "18:01-00:00"))
mean.time <- aggregate(x = netflix$Y, by = list(Time = netflix$Time), FUN = mean)
mean.time
plot(x = 1:4, y = mean.time$x, xlab = "Time of Day", ylab = "Avg. Latency (ms)", pch = 16, xaxt = "n", ylim = c(0,100))
lines(x = 1:4, y = mean.time$x)
axis(side = 1, at = 1:4, labels = c("00:01-06:00", "06:01-12:00", "12:01-18:00", "18:01-00:00"))

## Fit the appropriate linear regression model and perform the relevant F-tests
full <- lm(Y ~ Condition + Browser + factor(Time), data = netflix)
summary(full)
anova(full)

## Partial F-tests
red1 <- lm(Y ~ Browser + factor(Time), data = netflix)
red2 <- lm(Y ~ Condition + factor(Time), data = netflix)
red3 <- lm(Y ~ Condition + Browser, data = netflix)
red4 <- lm(Y ~ Condition, data = netflix)

# Is there a difference in expected response between conditions?
anova(red1, full)

# Was it necessary to block by browser?
anova(red2, full)

# Was it necessary to block by time of day?
anova(red3, full)

# Was it necessary to block at all?
anova(red4, full)


## Check model assumptions graphically
par(mfrow=c(1,3))
hist(full$residuals, xlab = "Residuals", main = "Histogram of Residuals", probability = TRUE)
lines(x = seq(-30,30,0.01), y = dnorm(x = seq(-30,30,0.01), mean = mean(full$residuals), sd = sd(full$residuals)), col = "red")
qqnorm(full$residuals)
qqline(full$residuals, col = "red")
plot(x = full$fitted.values, y = full$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")

