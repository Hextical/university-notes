## 2^4 Factorial Example with Logistic Regression

## Get the data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")
credit <- read.csv(file = "creditcard.csv", header = T)

## Summarize the data
conv.tab <- table(data.frame(Condition = credit$Cond, Conversion = credit$y))
data.frame(Condition = 1:16, 
           Num.Conversion = conv.tab[,2], 
           Conversion.Rate = round(conv.tab[,2]/7500, 4))

## Fit a full model with all main effects and interaction terms
model <- glm(y ~ x1 * x2 * x3 * x4, 
             family = binomial(link = "logit"), data = credit)
summary(model)
logLik(model)

## Fit a reduced model with just the main effects and interactions that 
## appear to be significant
model_red <- glm(y ~ x1 + x2 + x3 + x4 + x1:x2 + x3:x4, 
                 family = binomial(link = "logit"), data = credit)
summary(model_red)
logLik(model_red)

## Are the full and reduced models significantly different?
a <- anova(model_red, model, test = "LRT")
a
# Manually now...
tstat <- as.numeric(2*(logLik(model) - logLik(model_red)))
tstat
pval <- pchisq(q = tstat, df = 9, lower.tail = F)
pval

## Main Effects plots
library(gplots)
par(mfrow=c(2,2)) 
plotmeans(formula = y~x1, ylab = "Conversion Rate", xlab = "Annual Fee (x1)", 
          data = credit, xaxt = "n", ylim = c(0,0.04), pch = 16)
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
plotmeans(formula = y~x2, ylab = "Conversion Rate", xlab = "Account-opening Fee (x2)", 
          data = credit, xaxt = "n", ylim = c(0,0.04), pch = 16)
axis(side = 1, at = c(1,2), labels = c("No", "Yes"))
plotmeans(formula = y~x3, ylab = "Conversion Rate", xlab = "Initial Interest Rate (x3)", 
          data = credit, xaxt = "n", ylim = c(0,0.04), pch = 16)
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
plotmeans(formula = y~x4, ylab = "Conversion Rate", xlab = "Long-term Interest Rate (x4)", 
          data = credit, xaxt = "n", ylim = c(0,0.04), pch = 16)
axis(side = 1, at = c(1,2), labels = c("Low", "High"))

## Interaction Plots
x1 <- credit$x1
x2 <- credit$x2
x3 <- credit$x3
x4 <- credit$x4
y <- credit$y
par(mfrow=c(1,2))
interaction.plot(x1, x2, y, ylab = "Conversion Rate", xlab = "Annual Fee (x1)", 
                 main = "", legend = F, xaxt = "n", ylim = c(0,0.04))
points(x = c(1,1), y = c(mean(credit[credit$x1==-1 & credit$x2==-1,]$y), 
                         mean(credit[credit$x1==-1 & credit$x2==1,]$y)), pch = 16)
points(x = c(2,2), y = c(mean(credit[credit$x1==1 & credit$x2==-1,]$y), 
                         mean(credit[credit$x1==1 & credit$x2==1,]$y)), pch = 16)
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
legend("topleft", legend = c("Opening Fee (x2)","Yes", "No"), lty = c(1,1,2), 
       col=c("white", "black", "black"), cex = 0.75, bty = "n")

interaction.plot(x3, x4, y, ylab = "Conversion Rate", xlab = "Initial Interest Rate (x3)", 
                 main = "", legend = F, xaxt = "n", ylim = c(0,0.04))
points(x = c(1,1), y = c(mean(credit[credit$x3==-1 & credit$x4==-1,]$y), 
                         mean(credit[credit$x3==-1 & credit$x4==1,]$y)), pch = 16)
points(x = c(2,2), y = c(mean(credit[credit$x3==1 & credit$x4==-1,]$y), 
                         mean(credit[credit$x3==1 & credit$x4==1,]$y)), pch = 16)
axis(side = 1, at = c(1,2), labels = c("Current", "Lower"))
legend("bottomright", legend = c("Long-term Rate (x4)","High", "Low"), lty = c(1,1,2), 
       col=c("white", "black", "black"), cex = 0.75, bty = "n")

## So which condition(s) is (are) best?
X <- data.frame(x1 = kronecker(rep(1,8), c(-1,1)), 
                x2 = rep(kronecker(c(-1,1), rep(1,2)), 4), 
                x3 = rep(kronecker(c(-1,1), rep(1,4)), 2), 
                x4 = kronecker(c(-1,1), rep(1,8)))
pred <- data.frame(X, fit = predict(model_red, newdata = X, type = "response"))
pred
pred[order(-pred$fit),]

## Conditions 6, 2, and 14 all have a low annual fee and no account-opening fee
cond6 <- sum(credit[credit$Cond == 6, ]$y)
cond2 <- sum(credit[credit$Cond == 2, ]$y)
cond14 <- sum(credit[credit$Cond == 14, ]$y)
prop.test(x = c(cond6, cond2, cond14), n = rep(7500, 3))

## None of these is significantly different -- so choose the one that
## maximizes profits for the credit card company

