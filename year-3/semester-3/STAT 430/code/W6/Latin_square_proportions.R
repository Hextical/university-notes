## Analyzing a Latin Square Design to Compare Proportions

## Read-in the data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")
uber <- read.csv("uber-lsd.csv", header = TRUE)

## Response by condition
mean.df <- aggregate(x = uber$Y, by = list(Promo = uber$Promo), FUN = mean)
mean.df
par(mfrow=c(1,3))
mean.df <- aggregate(x = uber$Y, by = list(Promo = uber$Promo), FUN = mean)
plot(x = 1:3, y = mean.df$x, xlab = "Condition", ylab = "Booking Rate", pch = 16, xaxt = "n", ylim = c(0.05, 0.11))
lines(x = 1:3, y = mean.df$x)
axis(side = 1, at = 1:3, labels = c("Promo A", "Promo B", "Promo C"))

## Response by day
mean.nf1 <- aggregate(x = uber$Y, by = list(Day = uber$Day), FUN = mean)
plot(x = 1:3, y = mean.nf1$x, xlab = "Day", ylab = "Booking Rate", pch = 16, xaxt = "n", ylim = c(0.05, 0.11))
lines(x = 1:3, y = mean.nf1$x)
axis(side = 1, at = 1:3, labels = c("Friday", "Saturday", "Sunday"))

## Response by city
mean.nf2 <- aggregate(x = uber$Y, by = list(City = uber$City), FUN = mean)
plot(x = 1:3, y = mean.nf2$x, xlab = "City", ylab = "Booking Rate", pch = 16, xaxt = "n", ylim = c(0.05, 0.11))
lines(x = 1:3, y = mean.nf2$x)
axis(side = 1, at = 1:3, labels = c("Montreal", "Toronto", "Vancouver"))

## Fit the appropriate logistic regression model and perform the relevant LR-tests
full <- glm(Y ~ Promo + Day + City, family = binomial(link = "logit"), data = uber)
summary(full)

## Likelihood Ratio Tests
red1 <- glm(Y ~ Day + City, family = binomial(link = "logit"), data = uber) 
red2 <- glm(Y ~ Promo + City, family = binomial(link = "logit"), data = uber) 
red3 <- glm(Y ~ Promo + Day, family = binomial(link = "logit"), data = uber) 
red4 <- glm(Y ~ Promo, family = binomial(link = "logit"), data = uber) 


# Is there a difference in expected response between conditions?
anova(red1, full, test = "LRT")

# Was it necessary to block by day?
anova(red2, full, test = "LRT")

# Was it necessary to block by city?
anova(red3, full, test = "LRT")

# Was it necessary to block at all?
anova(red4, full, test = "LRT")

