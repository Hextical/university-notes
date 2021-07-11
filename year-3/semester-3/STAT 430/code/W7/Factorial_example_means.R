## Instagram Factorial Analysis

## Get the data
setwd("/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")
data <- read.csv(file = "instagram-factorial.csv", header = T)

Time <- data$Time
Frequency <- factor(data$Frequency, levels = c(0,1,2,3), 
                    labels = c("9:1", "7:1", "4:1", "1:1"))
Type <- factor(data$Type, levels = c(1,2), 
               labels = c("Photo", "Video"))

## Numerical summaries of the data
summary(data.frame(Time, Frequency, Type))
sd(Time)

## Condition Means
cond01 <- mean(Time[Frequency=="9:1" & Type == "Photo"])
cond02 <- mean(Time[Frequency=="9:1" & Type == "Video"])
cond11 <- mean(Time[Frequency=="7:1" & Type == "Photo"])
cond12 <- mean(Time[Frequency=="7:1" & Type == "Video"])
cond21 <- mean(Time[Frequency=="4:1" & Type == "Photo"])
cond22 <- mean(Time[Frequency=="4:1" & Type == "Video"])
cond31 <- mean(Time[Frequency=="1:1" & Type == "Photo"])
cond32 <- mean(Time[Frequency=="1:1" & Type == "Video"])

## Graphical Summaries of the data
library(gplots)
par(mfrow = c(1,2))
boxplot(Time ~ Frequency, main = "Boxplot of Session Duration by Ad Frequency", xlab = "Ad Frequency", ylab = "Session Duration (min)", ylim = c(0,11))
plotmeans(Time ~ Frequency, main = "Main Effect Plot for Ad Frequency", xlab = "Ad Frequency", ylab = "Mean Session Duration (min)", pch = 16, ylim = c(0,11))

boxplot(Time ~ Type, main = "Boxplot of Session Duration by Ad Type", xlab = "Ad Type", ylab = "Session Duration (min)", ylim = c(0,11))
plotmeans(Time ~ Type, main = "Main Effect Plot for Ad Type", xlab = "Ad Type", ylab = "Mean Session Duration (min)", ylim = c(0,11), pch = 16)

interaction.plot(Frequency, Type, Time, main = "Interaction Plot for Ad Frequency and Ad Type", ylab = "Mean Session Duration (min)", xlab = "Ad Frequency")
points(x=c(1,1,2,2,3,3,4,4), 
       y = c(cond01, cond02, cond11, cond12, cond21, cond22, cond31, cond32), 
       pch = 16)
interaction.plot(Type, Frequency, Time, main = "Interaction Plot for Ad Frequency and Ad Type", ylab = "Mean Session Duration (min)", xlab = "Ad Frequency")
points(x=c(1,1,1,1,2,2,2,2), 
       y = c(cond01, cond11, cond21, cond31, cond02, cond12, cond22, cond32), 
       pch = 16)

## Marginal Means
print(paste("9:1 frequency -- ", round(mean(data[data$Frequency==0,]$Time), 2), sep = ""))
print(paste("7:1 frequency -- ", round(mean(data[data$Frequency==1,]$Time), 2), sep = ""))
print(paste("4:1 frequency -- ", round(mean(data[data$Frequency==2,]$Time), 2), sep = ""))
print(paste("1:1 frequency -- ", round(mean(data[data$Frequency==3,]$Time), 2), sep = ""))

print(paste("Photo -- ", round(mean(data[data$Type==1,]$Time), 2), sep = ""))
print(paste("Video -- ", round(mean(data[data$Type==2,]$Time), 2), sep = ""))

## Notes:
# Frequency seems to be have a large and significant main effect
# Type seems to have a small and maybe significant main effect
# Interaction between Frequency and Type is existent but minimal
# Thus, the main effects seem to drive variation in the response (engagement time)

## Formal Analysis of Variance
model <- lm(Time ~ Frequency * Type)
summary(model)

## Compare this "full" model to one with the interaction terms
model.red1 <- lm(Time ~ Frequency + Type)
anova(model.red1, model)

## Compare the main effects model with ones that exclude Frequency and Type
model.red2 <- lm(Time ~ Frequency)
anova(model.red2, model.red1)

model.red3 <- lm(Time ~ Type)
anova(model.red3, model.red1)

## Notes:
# Our intuition based on the plots was correct: the interaction is minimal but significant, and 
# both main effects are, with "Frequency" explaining most of the variation in engagement time.

## The "effect" of Frequency when ad Type is photo
prev <- c("9:1", "7:1", "4:1", "1:1")
f.effect.mat.p <- matrix(0, nrow = 4, ncol = 4)
rownames(f.effect.mat.p) <- prev
colnames(f.effect.mat.p) <- prev
f.pvalue.mat.p <- f.effect.mat.p

for (i in 1:4){
  for (j in 1:4){
    ttest <- t.test(Time[Frequency==prev[i] & Type=="Photo"], 
                    Time[Frequency==prev[j] & Type=="Photo"])
    f.effect.mat.p[i,j] <- as.numeric(diff(ttest$estimate))
    f.pvalue.mat.p[i,j] <- as.numeric(ttest$p.value)
  }
}
# The following matrix shows the expected change in engagement time when going from one level of Frequency to another
# The rows and columns correspond to levels 9:1, 7:1, 4:1, 1:1
f.effect.mat.p
# The following matrix shows the two sided p-values associated with the comparisons in the previous matrix
# The rows and columns correspond to levels 0, 1, 2, 3 (9:1, 7:1, 4:1, 1:1)
f.pvalue.mat.p

## The "effect" of Frequency when ad Type is video
prev <- c("9:1", "7:1", "4:1", "1:1")
f.effect.mat.v <- matrix(0, nrow = 4, ncol = 4)
rownames(f.effect.mat.v) <- prev
colnames(f.effect.mat.v) <- prev
f.pvalue.mat.v <- f.effect.mat.v

for (i in 1:4){
  for (j in 1:4){
    ttest <- t.test(Time[Frequency==prev[i] & Type=="Video"], 
                    Time[Frequency==prev[j] & Type=="Video"])
    f.effect.mat.v[i,j] <- as.numeric(diff(ttest$estimate))
    f.pvalue.mat.v[i,j] <- as.numeric(ttest$p.value)
  }
}
# The following matrix shows the expected change in engagement time when going from one level of Frequency to another
# The rows and columns correspond to levels 9:1, 7:1, 4:1, 1:1
f.effect.mat.v
# The following matrix shows the two sided p-values associated with the comparisons in the previous matrix
# The rows and columns correspond to levels 0, 1, 2, 3 (9:1, 7:1, 4:1, 1:1)
f.pvalue.mat.v
