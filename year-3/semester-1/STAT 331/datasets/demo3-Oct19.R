## R demo for Oct 19
## Plotting functions and histograms, F distribution,
## ANOVA tables, F tests, MLR with categorical variables

# Plotting functions (e.g., probability density functions)
x <- seq(-4,4,0.01)  # grid of x values to evaluate
y <- dnorm(x,0,1)    # function that evaluates normal PDF with mean 0 and SD 1
plot(x,y, type="l")

y <- x^2
plot(x,y, type="l")

# F distribution examples
x <- seq(0,5,0.01)
plot(x, y = df(x, df1 = 1, df2 = 1), type = "l", xlab = "x", ylab = "density")
plot(x, y = df(x, df1 = 1, df2 = 1), type = "l", col = "black", xlab = "x",
     ylab = "density", ylim = c(0,2.5), lwd=2)
lines(x, y = df(x, df1 = 1, df2 = 100), type = "l", col = "green", lwd=2)
lines(x, y = df(x, df1 = 5, df2 = 1), type = "l", col = "blue", lwd=2)
lines(x, y = df(x, df1 = 5, df2 = 100), type = "l", col = "purple", lwd=2)
lines(x, y = df(x, df1 = 10, df2 = 1), type = "l", col = "red", lwd=2)
lines(x, y = df(x, df1 = 10, df2 = 100), type = "l", col = "orange", lwd=2)
legend("topright", legend = c("df1=1, df2=1","df1=1, df2=100","df1=5, df2=1",
                          "df1=5, df2=100","df1=10, df2=1","df1=10, df2=100"),
       lty = 1, col = c("black", "green", "blue", "purple", "red", "orange"))

# Random numbers from F distribution
set.seed(12345678)
randF <- rf(1000, 5, 100)
hist(randF)
hist(randF, freq=FALSE)
# add true density
lines(x, y = df(x, df1 = 5, df2 = 100), type = "l", col = "purple", lwd=2)
# set y-axis limits and more detailed histogram bins
hist(randF, freq=FALSE, ylim=c(0,0.8), breaks=25)
lines(x, y = df(x, df1 = 5, df2 = 100), type = "l", col = "purple", lwd=2)

randF <- rf(10000, 5, 100)
hist(randF, freq=FALSE, ylim=c(0,0.8), breaks=25)
lines(x, y = df(x, df1 = 5, df2 = 100), type = "l", col = "purple", lwd=2)

## Revisit rocket example
rocket <- read.csv(file="rocket.csv")
m1 <- lm(thrust ~ nozzle + propratio, data = rocket)
summary(m1)
anova(m1)  # compare with ANOVA table on board Oct 5
anova(m1)$`Sum Sq`
sum(anova(m1)$`Sum Sq`[1:2])
SSRes <- anova(m1)$`Sum Sq`[3]

# Test of overall significance
m_red <- lm(thrust ~ 1, data = rocket)
summary(m_red)
anova(m_red)
SSRes_A <- anova(m_red)$`Sum Sq`[1]

# F-statistic
l <- 2
n <- nrow(rocket)
p <- 2
Fstat <- ((SSRes_A - SSRes)/l) / (SSRes / (n-p-1))
pval <- 1 - pf(Fstat, df1 = l, df2 = n-p-1)


## Coffee example  (Coffee Quality Institute, 2018)
coffee <- read.csv("coffee_arabica.csv")
head(coffee)

mfull <- lm(Flavor~ factor(Processing.Method) + Aroma + Aftertaste + 
    Body + Acidity + Balance + Sweetness + Uniformity + Moisture, dat=coffee)
summary(mfull)
anova(mfull)
SSRes <- anova(mfull)$`Sum Sq`[10]

## Reduced model without Uniformity and Moisture (beta9=beta10=0):
m_red <- lm(Flavor~ factor(Processing.Method) + Aroma + Aftertaste + 
              Body + Acidity + Balance + Sweetness, dat=coffee)
summary(m_red)
anova(m_red)
SSRes_A <- anova(m_red)$`Sum Sq`[8]

## F-statistic
l <- 2
n <- nrow(coffee)
p <- 10
Fstat <- ((SSRes_A - SSRes)/l) / (SSRes / (n-p-1))
pval <- 1 - pf(Fstat, df1 = l, df2 = n-p-1)

## Reduced model without Uniformity and Moisture and
## setting effect of Dry = Semi (beta1=beta9=beta10=0)
coffee$method2 <- ifelse(coffee$Processing.Method %in% 
                c('Natural / Dry', 'Semi-washed / Semi-pulped'), 0, 1) # 1 = wet, 0 otherwise
coffee$wet <- ifelse(coffee$Processing.Method == 'Washed / Wet', 0, 1) # 1 = semi/dry, 0 otherwise

m_red2 <- lm(Flavor~ method2 + Aroma + Aftertaste + 
               Body + Acidity + Balance + Sweetness, dat=coffee)
summary(m_red2)
anova(m_red2)
SSRes_A <- anova(m_red2)$`Sum Sq`[8]

## F-statistic
l <- 3
n <- nrow(coffee)
p <- 10
Fstat <- ((SSRes_A - SSRes)/l) / (SSRes / (n-p-1))
pval <- 1 - pf(Fstat, df1 = l, df2 = n-p-1)
