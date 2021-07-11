## Comparing Multiple Means Within Blocks

## Read-in the data
gap <- read.csv("thegap.csv", header = TRUE)

## Response by condition
par(mfrow=c(1,2))
boxplot(Y ~ Condition, data = gap, xlab = "Condition", ylab = "Purchase Total ($)", xaxt = "n", ylim = c(20,80))
axis(side = 1, at = 1:3, labels = c("Promo 1", "Promo 2", "Promo 3"))
mean.condition <- aggregate(x = gap$Y, by = list(Promo = gap$Condition), FUN = mean)
mean.condition
plot(x = 1:3, y = mean.condition$x, xlab = "Condition", ylab = "Avg. Purchase Total ($)", pch = 16, xaxt = "n", ylim = c(20,80))
lines(x = 1:3, y = mean.condition$x)
axis(side = 1, at = 1:3, labels = c("Promo 1", "Promo 2", "Promo 3"))

## Response by block
boxplot(Y ~ factor(Block, levels = c("M", "Tu", "W", "Th", "F"), labels = c("M", "Tu", "W", "Th", "F"),
                   ordered = TRUE), data = gap, xlab = "Day of Week", ylab = "Purchase Total ($)", ylim = c(20,80))
mean.block <- aggregate(x = gap$Y, by = list(Day = gap$Block), FUN = mean)
mean.block <- mean.block[c(2,4,5,3,1),]
mean.block
plot(x = 1:5, y = mean.block$x, xlab = "Day of Week", ylab = "Avg. Purchase Total ($)", pch = 16, xaxt = "n", ylim = c(20,80))
lines(x = 1:5, y = mean.block$x)
axis(side = 1, at = 1:5, labels = c("M", "Tu", "W", "Th", "F"))


## Fit the appropriate linear regression model and perform the relevant F-tests
full <- lm(Y ~ Condition + Block, data = gap)
summary(full)
anova(full)

## Partial F-tests
red1 <- lm(Y ~ Block, data = gap)
red2 <- lm(Y ~ Condition, data = gap)

# Is there a difference in expected response between conditions?
anova(red1, full)

# Was blocking necessary?
anova(red2, full)


## Check model assumptions graphically
par(mfrow=c(1,3))
hist(full$residuals, xlab = "Residuals", main = "Histogram of Residuals", probability = TRUE)
lines(x = seq(-10,10,0.01), y = dnorm(x = seq(-10,10,0.01), mean = mean(full$residuals), sd = sd(full$residuals)), col = "red")
qqnorm(full$residuals)
qqline(full$residuals, col = "red")
plot(x = full$fitted.values, y = full$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")

## Check model assumptions with hypothesis tests
shapiro.test(x = full$residuals)
group <- kronecker(1:15, rep(1,50))
bartlett.test(x = full$residuals, g = group)


## Now let's try to do this manually...
N <- dim(gap)[1]
m <- length(unique(gap$Condition))
b <- length(unique(gap$Block))
n <- 50

# Condition-specific means:
mean1 <- mean(gap$Y[gap$Condition == "V1"])
mean2 <- mean(gap$Y[gap$Condition == "V2"])
mean3 <- mean(gap$Y[gap$Condition == "V3"])

# Block-specific means:
meanM <- mean(gap$Y[gap$Block == "M"])
meanTu <- mean(gap$Y[gap$Block == "Tu"])
meanW <- mean(gap$Y[gap$Block == "W"])
meanTh <- mean(gap$Y[gap$Block == "Th"])
meanF <- mean(gap$Y[gap$Block == "F"])

# Overall mean:
meanOverall <- mean(gap$Y)

SSC <- b*n*((mean1 - meanOverall)^2 + (mean2 - meanOverall)^2 + (mean3 - meanOverall)^2)
SSB <- m*n*((meanM - meanOverall)^2 + (meanTu - meanOverall)^2 + (meanW - meanOverall)^2 + (meanTh - meanOverall)^2 + (meanF - meanOverall)^2)
SSE <- sum((gap$Y[gap$Condition == "V1" & gap$Block == "M"] - mean1 - meanM + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V1" & gap$Block == "Tu"] - mean1 - meanTu + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V1" & gap$Block == "W"] - mean1 - meanW + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V1" & gap$Block == "Th"] - mean1 - meanTh + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V1" & gap$Block == "F"] - mean1 - meanF + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V2" & gap$Block == "M"] - mean2 - meanM + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V2" & gap$Block == "Tu"] - mean2 - meanTu + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V2" & gap$Block == "W"] - mean2 - meanW + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V2" & gap$Block == "Th"] - mean2 - meanTh + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V2" & gap$Block == "F"] - mean2 - meanF + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V3" & gap$Block == "M"] - mean3 - meanM + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V3" & gap$Block == "Tu"] - mean3 - meanTu + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V3" & gap$Block == "W"] - mean3 - meanW + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V3" & gap$Block == "Th"] -mean3 - meanTh + meanOverall)^2) + 
       sum((gap$Y[gap$Condition == "V3" & gap$Block == "F"] -mean3 - meanF + meanOverall)^2)
sum(c(SSC, SSB, SSE))
SST <- var(gap$Y)*(N-1)
SST
df <- c(m-1, b-1, N-m-b+1)
MS <- c(SSC, SSB, SSE) / df
Tstat <- MS[1:2]/MS[3]
pval <- pf(q = Tstat, df1 = df[1:2], df2 = rep(df[3], 2), lower.tail = FALSE)

# Check that all of this work matched the anova output
anova(full)
c(SSC, SSB, SSE)
MS
Tstat
pval





