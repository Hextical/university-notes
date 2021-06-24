# Example 1
dat <- c(65, 70, 80, 85, 75)
y.bar <- mean(dat)
s <- sd(dat)
n <- length(dat)
df <- length(dat) - 1
t <- qt(0.975, df)
left <- y.bar - t * s / sqrt(n)
right <- y.bar + t * s / sqrt(n)

# Example 2
boys <-
  c(39, 42, 47, 50, 52, 52, 54, 55, 55, 56, 56, 56, 58, 60, 62)
girls <-
  c(44, 45, 48, 50, 51, 52, 53, 53, 57, 58, 59, 60, 62, 63, 64)
y_b.bar <- mean(boys)
y_g.bar <- mean(girls)
s_b.sq <- var(boys)
s_g.sq <- var(girls)
n_b <- length(boys)
n_g <- length(girls)
s_p.sq <-
  ((n_g - 1) * s_g.sq + (n_b - 1) * s_b.sq) / (n_g + n_b - 2)
df <- n_g + n_b - 2
t <- qt(0.975, df)
left <- (y_b.bar - y_g.bar) - t * sqrt(s_p.sq * (1 / n_g + 1 / n_b))
right <-
  (y_b.bar - y_g.bar) + t * sqrt(s_p.sq * (1 / n_g + 1 / n_b))

# Example 3
boys <- c(39, 42, 47, 50, 52, 52, 54, 55, 55, 56, 56, 56, 58, 60, 62)
girls <- c(44, 45, 48, 50, 51, 52, 53, 53, 57, 58, 59, 60, 62, 63, 64)
diff <- boys - girls
y_d.bar <- mean(diff)
s_d <- sd(diff)
n_d <- length(diff)
df <- length(diff) - 1
t <- qt(0.975, df)
left <- y_d.bar - t * s_d / sqrt(n_d)
right <- y_d.bar + t * s_d / sqrt(n_d)

# Example 4
boys <-
  c(39, 42, 47, 50, 52, 52, 54, 55, 55, 56, 56, 56, 58, 60, 62)
girls <-
  c(44, 45, 48, 50, 51, 52, 53, 53, 57, 58, 59, 60, 62, 63, 64)
y_b.bar <- mean(boys)
y_g.bar <- mean(girls)
s_b.sq <- var(boys)
s_g.sq <- var(girls)
n_b <- length(boys)
n_g <- length(girls)
df <- n_g + n_b - 2
t <- qt(0.975, df)
left <- (y_b.bar - y_g.bar) - t * sqrt(s_b.sq / n_g + s_g.sq / n_b)
right <-
  (y_b.bar - y_g.bar) + t * sqrt(s_b.sq / n_g + s_g.sq / n_b)

# Example 5
n <- 430
voters <- 267
p.hat <- 267 / 430
z <- qnorm(0.975)
left <- p.hat - z * sqrt((p.hat * (1 - p.hat)) / n)
right <-  p.hat + z * sqrt((p.hat * (1 - p.hat)) / n)

# Example 1
high <- c(11, 19, 32, 12, 7, 14)
low <- c(17, 21 , 14 , 11 , 18 , 9)
diff <- high - low
y_d.bar <- mean(diff)
s_d <- sd(diff)
n_d <- length(diff)
df <- length(diff) - 1
d <- (y_d.bar - 0) / (s_d / sqrt(n_d))
pval <- 2 * (1 - pt(d, df))

# Example 2
coke <- c(8, 7, 18, 42, 21)
water <- c(5, 11, 21, 9, 14)
y_c.bar <- mean(coke)
y_w.bar <- mean(water)
s_c.sq <- var(coke)
s_w.sq <- var(water)
n_c <- length(coke)
n_w <- length(water)
s_p.sq <-
  ((n_w - 1) * s_w.sq + (n_c - 1) * s_c.sq) / (n_w + n_c - 2)
df <- n_c + n_w - 2
t <- qt(0.975, df)
d <- (y_c.bar - y_w.bar) / sqrt(s_p.sq * (1 / n_w + 1 / n_c))
pval <- 1 - pt(d, df)

# Data frames
grp1 <- c(50, 53, 52, 58)
grp2 <- c(62, 55, 58, 60)
# Must run to get same results as textbook
options(contrasts = c('contr.sum', 'contr.poly'))
Y <- c(grp1, grp2)
# Makes a discrete variable
x <- as.factor(c(rep(1, 4), rep(2, 4)))
# Builds the model
model <- lm(Y ~ x)
# Displays the output
summary(model)

left <- -2.75 - 2.75 - qt(0.975, 6) * sqrt(summary(model)$sigma ^ 2 / 2)
right <- -2.75 - 2.75 + qt(0.975, 6) * sqrt(summary(model)$sigma ^ 2 / 2)

d <- (-2.75 - 2.75) / (summary(model)$sigma / sqrt(2))
pval <- 2 * (1 - pt(abs(d), 6))

# Example 2
options(contrasts = c('contr.sum', 'contr.poly'))
Marks1 = c(55, 92, 48, 57, 66, 72)
Marks2 = c(62, 95, 84, 83, 66, 75)
Marks3 = c(89, 92, 94, 99, 87, 67)
Marks4 = c(25, 35, 71, 42, 44, 30)
Y = c(Marks1, Marks2, Marks3, Marks4)

x = as.factor(c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6)))
model = lm(Y ~ x)
summary(model)
tau.1 <- summary(model)$coefficients[2]
tau.2 <- summary(model)$coefficients[3]
tau.3 <- summary(model)$coefficients[4]
tau.4 <- -1 * (tau.1 + tau.2 + tau.3)

tau.1 - tau.2 - qt(0.975, 20) * summary(model)$sigma/sqrt(3)
tau.1 - tau.2 + qt(0.975, 20) * summary(model)$sigma/sqrt(3)

theta <- ((tau.1 + tau.4) / 2) - ((tau.2 + tau.3) / 2)
d <- (theta - 0) / (summary(model)$sigma / sqrt(6))
pval <- 2 * (1 - pt(abs(d), 20))

par(mfrow = c(3, 3))
curve(df(x, df1 = 1, df2 = 1),
      from = 0,
      to = 3,
      main = "df=1,1")
curve(df(x, df1 = 1, df2 = 10),
      from = 0,
      to = 3,
      main = "df=1,10")
curve(df(x, df1 = 1, df2 = 20),
      from = 0,
      to = 3,
      main = "df=1,20")
curve(df(x, df1 = 5, df2 = 1),
      from = 0,
      to = 3,
      main = "df=5,1")
curve(df(x, df1 = 5, df2 = 10),
      from = 0,
      to = 3,
      main = "df=5,10")
curve(df(x, df1 = 5, df2 = 20),
      from = 0,
      to = 3,
      main = "df=5,20")
curve(df(x, df1 = 10, df2 = 1),
      from = 0,
      to = 3,
      main = "df=10,1")
curve(df(x, df1 = 10, df2 = 10),
      from = 0,
      to = 3,
      main = "df=10,10")
curve(df(x, df1 = 10, df2 = 20),
      from = 0,
      to = 3,
      main = "df=10,20")

# Model 6 Ex 1

grp1 = c(50, 53, 52, 58)
grp2 = c(62, 55, 58)
Y = c(grp1, grp2)
x = as.factor(c(rep(1, 4), rep(2, 3)))
# Group Averages
grp_av = tapply(Y, x, mean, na.rm = T)
mu = mean(Y)
# Treatment Effects
tau1 = (grp_av - mean(Y))[1]
tau2 = (grp_av - mean(Y))[2]
# Estimated Sigma
sigma = summary(lm(Y ~ x))$sigma
# Values
sigma
tau1
tau2
mu
#3.447221, -2.178571, 2.904762, 55.42857
anova(lm(Y ~ x))

tau1 - tau2 - qt(0.975, 5) * sqrt((7 * sigma ^ 2) / 12)
tau1 - tau2 + qt(0.975, 5) * sqrt((7 * sigma ^ 2) / 12)


x = seq(0, 3.5, by = 0.1)
y = seq(0, 0.09, by = 0.01)
u = seq(0, 3.59, by = 0.01)
p = pnorm(u)
m = matrix(p, ncol = 10, byrow = TRUE)
colnames(m) = y
row.names(m) = x
library(xtable)
newm = xtable(m, digits = 4)
print.xtable(newm, type = "latex", file = "nor1.tex")

p = c(0.6, 0.7, 0.8, 0.9, 0.95, 0.975, 0.99, 0.995, 0.9995)
n = seq(1, 31, by = 1)
q = matrix(data = NA, nrow = 31, ncol = 9)
for (i in 1:length(p)) {
  for (j in 1:length(n)) {

    if (j == 31) {
      q[j, i] = qt(p[i], .Machine$double.xmax)
    } else {
      q[j, i] = qnorm(p[i])
    }
  }
}
colnames(q) = p
row.names(q) = n
library(xtable)
newt = xtable(q, digits = 3)
print.xtable(newt, type = "latex", file = "t-quantile.tex")

library(xtable)
df1 <- c(seq(from = 1, to = 10), seq(from = 20, to = 40, by = 10))
df2 <- c(seq(from = 1, to = 30), 40, 50, 100)
q = matrix(data = NA, nrow = 33, ncol = 13)
for (i in 1:length(df1)) {
  for (j in 1:length(df2)) {
      q[j, i] = qf(0.05, df1[i], df2[j], lower.tail = FALSE)
  }
}
colnames(q) = df1
row.names(q) = df2
newt = xtable(q, digits = 2)
print.xtable(newt, type = "latex", file = "f-table-0.1.tex")


# Step 1 – Change the directory
#   In R, select FILE, CHANGE DIR
#   select the folder your data
#   is located in.

# Step 2 – use read.table
Data = read.table("blocked.csv", sep = ",", header = T)

# Step 3 – Have a look at the data:
Data
# To build a model we type:
options(contrasts = c('contr.sum', 'contr.poly'))
attach(Data)
Treatment = as.factor(Treatment)
Block = as.factor(Block)
Model = lm(Value ~ Treatment + Block)

# To look at the output, we type:
summary(Model)

# ANOVA
anova(Model)

Model = lm(Value~Treatment)
summary(Model)
anova(Model)

# Redefine NewCode = 2Height+Freq:
# Height Freq New Code
# 0      0    0
# 0      1    1
# 1      0    2
# 1      1    3

# Removes all variables
rm(list = ls())
options(contrasts = c('contr.sum', 'contr.poly'))
data = read.table("stepping2.csv",
                  header = T,
                  sep = ",",
                  as.is = T)
attach(data)
Y = HR - RestHR
Trt = 2 * Height + Frequency
Trt = as.factor(Trt)
Model = lm(Y ~ Trt)

rm(list = ls())
options(contrasts = c('contr.sum', 'contr.poly'))
data = read.table("stepping2.csv",
                  header = T,
                  sep = ",",
                  as.is = T)
attach(data)
Y = HR - RestHR
Height = as.factor(Height)
Freq = as.factor(Frequency)
Model = lm(Y ~ Freq + Height + Freq * Height)
anova(Model)

attach(women)
mean(height)
mean(weight)
set.seed(332)
sample_heights = sample(height, 5)
mean(sample_heights)
sd(sample_heights)

sample_weights = c(123, 129, 135, 146, 120)
mean(sample_weights)

plot(weight, height)
sample_weights = sample_weights - mean(sample_weights) # x_i - bar(x)
summary(lm(sample_heights ~ sample_weights)) # Y_i ~ x_i - bar(x)
