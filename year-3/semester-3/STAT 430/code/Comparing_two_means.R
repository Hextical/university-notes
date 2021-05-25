## Instagram t-test Example

## Change working directory
setwd(dir = "/Users/nstevens/Dropbox/Teaching/STAT_430/Spring_2021/R Stuff/")

## Read in the data
data <- read.csv(file = "instagram.csv", header = T)
cond1 <- data$Condition_1
cond2 <- data$Condition_2

## Plot the data
par(mfrow=c(2,1))
xmin <- min(min(cond1), min(cond2))
xmax <- max(max(cond1), max(cond2))
hist(cond1, xlim = c(xmin, xmax), main = "Minutes Engaged", xlab = "Condition 1")
abline(v = mean(cond1), col = "red", lwd = 2)
hist(cond2, xlim = c(xmin, xmax), main = "", xlab = "Condition 2")
abline(v = mean(cond2), col = "red", lwd = 2)

## First test the equality of variances
var.test(x = cond1, y = cond2, ratio = 1, alternative = "two.sided", conf.level = 0.95)

## Manual p-value calculation:
pf(q = 0.9376, df1 = 499, df2 = 499, lower.tail = TRUE) + 
  pf(q = 1/0.9376, df1 = 499, df2 = 499, lower.tail = FALSE)


## Perform the test
## Ho: mu1 = mu2 vs. Ha: mu1 != mu2
t.test(x = cond1, y = cond2, alternative = "two.sided", 
       mu = 0, paired = F, var.equal = T, conf.level = 0.95)

## Manual p-value calculation:
pt(q = -30.101, df = 998, lower.tail = TRUE) +
  pt(q = 30.101, df = 998, lower.tail = FALSE)

## Ho: mu1 <= mu2 vs. Ha: mu1 > mu2
t.test(x = cond1, y = cond2, alternative = "greater", 
       mu = 0, paired = F, var.equal = T, conf.level = 0.95)

## Manual p-value calculation:
pt(q = 30.101, df = 998, lower.tail = FALSE)

## Ho: mu1 >= mu2 vs. Ha: mu1 < mu2
t.test(x = cond1, y = cond2, alternative = "less", 
       mu = 0, paired = F, var.equal = T, conf.level = 0.95)

## Manual p-value calculation:
pt(q = 30.101, df = 998, lower.tail = TRUE)

