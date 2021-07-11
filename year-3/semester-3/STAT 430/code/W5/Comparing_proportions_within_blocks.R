## Comparing Multiple Proportions Within Blocks

## Read-in the data
enterprise <- read.csv("enterprise.csv", header = TRUE)

## Response by condition
mean.condition <- aggregate(x = enterprise$Y, by = list(Ad = enterprise$Condition), FUN = mean)
mean.condition
par(mfrow=c(1,2))
plot(x = 1:3, y = mean.condition$x, xlab = "Condition", ylab = "Click Through Rate", pch = 16, xaxt = "n", ylim = c(0.05, 0.1))
lines(x = 1:3, y = mean.condition$x)
axis(side = 1, at = 1:3, labels = c("Ad1", "Ad2", "Ad3"))

## Response by block
mean.block <- aggregate(x = enterprise$Y, by = list(Region = enterprise$Block), FUN = mean)
mean.block
plot(x = 1:4, y = mean.block$x, xlab = "Region", ylab = "Click Through Rate", pch = 16, xaxt = "n", ylim = c(0.05, 0.1))
lines(x = 1:4, y = mean.block$x)
axis(side = 1, at = 1:4, labels = c("NE", "NW", "SE", "SW"))

## Fit the appropriate logistic regression model and perform the relevant LR-tests
full <- glm(Y ~ factor(Condition) + factor(Block), family = binomial(link = "logit"), data = enterprise)
summary(full)

## Likelihood Ratio Tests
red1 <- glm(Y ~ factor(Block), family = binomial(link = "logit"), data = enterprise) 
red2 <- glm(Y ~ factor(Condition), family = binomial(link = "logit"), data = enterprise) 


# Is there a difference in expected response between conditions?
anova(red1, full, test = "LRT")

# Was blocking necessary?
anova(red2, full, test = "LRT")


## The LRTs manually...
# Reduced 1 vs. Full
t_C <- as.numeric(2*(logLik(full)-logLik(red1)))
pval_C <- pchisq(q = t_C, df = 2, lower.tail = FALSE)
t_C
pval_C

# Reduced 2 vs. Full
t_B <- as.numeric(2*(logLik(full)-logLik(red2)))
pval_B <- pchisq(q = t_B, df = 3, lower.tail = FALSE)
t_B
pval_B




