## 2^(8-4) Fractional Factorial Example

## Get the data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")
data <- read.csv(file = "chehalem.csv", header = T)
A <- factor(data$A, levels = c(-1,1), labels = c("Pommard", "Wadenswil"))
B <- factor(data$B, levels = c(-1,1), labels = c("Allier", "Troncais"))
C <- factor(data$C, levels = c(-1,1), labels = c("Old", "New"))
D <- factor(data$D, levels = c(-1,1), labels = c("Champagne", "Montrachet"))
E <- factor(data$E, levels = c(-1,1), labels = c("None", "All"))
F <- factor(data$F, levels = c(-1,1), labels = c("Light", "Medium"))
G <- factor(data$G, levels = c(-1,1), labels = c("None", "10%"))
H <- factor(data$H, levels = c(-1,1), labels = c("Low", "High"))
y <- data$y

## Try fitting the full model with all 2^8 terms that would be of interest
model.full <- lm(y~(A+B+C+D+E+F+G+H)^8, data = data) 
summary(model.full)

## Figure out what has been aliased
library(FrF2)
aliases(model.full) 


## Fit the largest model we can, explicitly highlighting the main effects and two-factor interactions
model.fullish <- lm(y ~ A+B+C+D+E+F+G+H+A:B+A:C+A:D+A:E+A:F+A:G+A:H, data = data)
summary(model.fullish)

## Identify the most influential factors
effects <- 2*model.fullish$coefficients[2:length(model.fullish$coefficients)]
effects[order(abs(effects), decreasing = TRUE)]
## Note: it appears as though factors D, E, F, G have the largest main effects and the 2-factor 
##       interactions AC = DF, AH = FG, and AD = EG are most important. Let's try fitting a reduced model 
##       with just these terms

model.red <- lm(y ~ A+ B + C + D + E + F + G + D:F + F:G + E:G, data = data)
summary(model.red)
anova(model.red, model.fullish)

## Main Effects plots
library(gplots)
par(mfrow=c(2,2)) 
plotmeans(formula = y~A, ylab = "Tasting Score", xlab = "Pinot Clone (A)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("Pommard", "Wadenswil"))
plotmeans(formula = y~B, ylab = "Tasting Score", xlab = "Oak Type (B)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("Allier", "Troncais"))
plotmeans(formula = y~C, ylab = "Tasting Score", xlab = "Barrel Age (C)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("Old", "New"))
plotmeans(formula = y~D, ylab = "Tasting Score", xlab = "Yeast Contact (D)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("Champagne", "Montrachet"))
plotmeans(formula = y~E, ylab = "Tasting Score", xlab = "Stems (E)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("None", "All"))
plotmeans(formula = y~F, ylab = "Tasting Score", xlab = "Barrel Toast (F)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("Light", "Medium"))
plotmeans(formula = y~G, ylab = "Tasting Score", xlab = "Whole Cluster (G)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("None", "10%"))
plotmeans(formula = y~H, ylab = "Tasting Score", xlab = "Fermentation Temp (H)", ylim = c(1, 16), data = data, xaxt = "n", pch = 16)
axis(side = 1, at = c(1,2), labels = c("Low", "High"))

## Interaction Plots
par(mfrow=c(1,3))
interaction.plot(D, F, y, ylab = "Mean Response Rate", xlab = "Yeast Type (D)", main = "", ylim = c(1, 16), legend = FALSE)
points(x = c(1,1), y = c(mean(data[data$D==-1 & data$F==-1,]$y),mean(data[data$D==-1 & data$F==1,]$y)), pch = 16)
points(x = c(2,2), y = c(mean(data[data$D==1 & data$F==-1,]$y),mean(data[data$D==1 & data$F==1,]$y)), pch = 16)
legend("bottomleft", legend = c("Toast (F)","Medium", "Light"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.8, bty = "n")
interaction.plot(F, G, y, ylab = "Mean Response Rate", xlab = "Barrel Toast (F)", main = "", ylim = c(1, 16), legend = FALSE)
points(x = c(1,1), y = c(mean(data[data$F==-1 & data$G==-1,]$y),mean(data[data$F==-1 & data$G==1,]$y)), pch = 16)
points(x = c(2,2), y = c(mean(data[data$F==1 & data$G==-1,]$y),mean(data[data$F==1 & data$G==1,]$y)), pch = 16)
legend("bottomleft", legend = c("Whole Cluster (G)","10%", "None"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.8, bty = "n")
interaction.plot(E, G, y, ylab = "Mean Response Rate", xlab = "Stems (E)", main = "", ylim = c(1, 16), legend = FALSE)
points(x = c(1,1), y = c(mean(data[data$E==-1 & data$G==-1,]$y),mean(data[data$E==-1 & data$G==1,]$y)), pch = 16)
points(x = c(2,2), y = c(mean(data[data$E==1 & data$G==-1,]$y),mean(data[data$E==1 & data$G==1,]$y)), pch = 16)
legend("bottomleft", legend = c("Whole Cluster (G)","10%", "None"), lty = c(1,1,2), col=c("white", "black", "black"), cex = 0.8, bty = "n")

