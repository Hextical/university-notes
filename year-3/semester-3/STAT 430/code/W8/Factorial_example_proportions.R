## Tinyco's Bananimal Example

# Read-in the data
tinyco <- read.csv(file = "bananimal.csv", header = TRUE)

# Extract and personalize the variables for easy use
y <- tinyco$y
tinyco$colour <- factor(tinyco$colour, levels = c("y", "g"), 
                        labels = c("Yellow", "Gold"))
colour <- tinyco$colour
tinyco$price <- factor(tinyco$price, levels = c(10,20,30), 
                       labels = c("$10", "$20", "$30"))
price <- tinyco$price

# Numerically summarize the data
summary(tinyco)

# Overall Purchase rate
mean(y)

# Purchase rate by colour
agg.col <- aggregate(y, by = list(Colour = colour), FUN = mean)
agg.col

# Purchase rate by price
agg.prc <- aggregate(y, by = list(Price = price), FUN = mean)
agg.prc

# Purchase rate by colour by price
agg.col.prc <- aggregate(y, by = list(Price = price, Colour = colour), 
                         FUN = mean)
agg.col.prc 


# Main and interaction effect plots for Colour
par(mfrow=c(1,2))
plot(x = 1:2, y = agg.col$x, 
     pch = 16, ylim = c(0, 0.3), xaxt = "n", xlab = "Colour", 
     ylab = "Purchase Rate", main = "Main Effect of Colour")
lines(x = 1:2, y = agg.col$x)
axis(side = 1, at = 1:2, labels = levels(colour))

interaction.plot(colour, price, y, main = "Colour-by-Price Interaction", 
                 xlab = "Colour", ylab = "Purchase Rate", ylim = c(0,0.3),
                 trace.label = "Price")
points(x = c(1,1,1,2,2,2), y = agg.col.prc$x, pch = 16)


# Main and interaction effect plots for Price
plot(x = 1:3, y = agg.prc$x, 
     pch = 16, ylim = c(0, 0.3), xaxt = "n", xlab = "Price", 
     ylab = "Purchase Rate", main = "Main Effect of Price")
lines(x = 1:3, y = agg.prc$x)
axis(side = 1, at = 1:3, labels = levels(price))

interaction.plot(price, colour, y, main = "Price-by-Colour Interaction", 
                 xlab = "Price", ylab = "Purchase Rate", ylim = c(0,0.3),
                 trace.label = "Colour")
points(x = c(1,1,2,2,3,3), y = agg.col.prc$x[c(1,4,2,5,3,6)], pch = 16)


# Fit the main effects model
main <- glm(y ~ colour + price, family = binomial(link = "logit"))
summary(main)

# Is the main effect of colour significant?
red1 <- glm(y ~ price, family = binomial(link = "logit"))
# Manually:
t <- 2*(logLik(main) - logLik(red1))
pval <- pchisq(q = t, df = 1, lower.tail = FALSE)
cat(paste0("Test Statistic: ", t, "\nP-value: ", pval))
# Or automatically:
anova(red1, main, test = "LRT")

# Is the main effect of price significant?
red2 <- glm(y ~ colour, family = binomial(link = "logit"))
# Manually:
t <- 2*(logLik(main) - logLik(red2))
pval <- pchisq(q = t, df = 2, lower.tail = FALSE)
cat(paste0("Test Statistic: ", t, "\nP-value: ", pval))
# Or automatically:
anova(red2, main, test = "LRT")

## BUT the interaction plots suggest interaction, so we should be careful 
## interpreting main effects

# Fit the full model that accounts for both main and interaction effects
full <- glm(y ~ colour * price, family = binomial(link = "logit"))
summary(full)

# Manually:
t <- 2*(logLik(full) - logLik(main))
pval <- pchisq(q = t, df = 2, lower.tail = FALSE)
cat(paste0("Test Statistic: ", t, "\nP-value: ", pval))
# Or automatically:
anova(main, full, test = "LRT")

# Which condition maximizes Purchase rate?
agg.col.prc
cond.counts <- aggregate(y, by = list(Price = price, Colour = colour), FUN = sum)
cond.counts
cond.sizes <- aggregate(y, by = list(Price = price, Colour = colour), FUN = length)
cond.sizes

# Are they all the same?
prop.test(x = cond.counts$x, n = cond.sizes$x)

# No, so which are different?
pairwise.prop.test(x = cond.counts$x, 
                   n = cond.sizes$x, 
                   p.adjust.method = "holm")

# Are 1,4,5,6 not significantly different?
prop.test(x = cond.counts$x[c(1,4:6)], n = cond.sizes$x[c(1,4:6)])







