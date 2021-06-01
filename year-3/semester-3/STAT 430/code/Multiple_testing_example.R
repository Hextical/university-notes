## The Four-Test Example

## These were the p-values
p1 <- 0.015
p2 <- 0.029
p3 <- 0.008
p4 <- 0.026

## The plot of ordered p-values vs. rank + significance thresholds is created below:

# Define some useful variables:
alpha.star <-0.05
p <- c(p1, p2, p3, p4)
M <- length(p)
rank <- seq(0, M+1, 0.001)

# Make the plot:
plot(x = 1:M, y = sort(p), pch = 16, ylab = "Sorted p-values", xlab = "Rank", ylim = c(0,max(alpha.star,max(p))), xaxt = "n")
axis(side = 1, at = 1:M, labels = 1:M)

# Holm's threshold:
lines(x = rank, y = alpha.star/(M-rank+1), col = "red", lty = 2) 

# Bonferroni's threshold:
abline(h = alpha.star/M, col = "blue", lty = 2) 

# Šidák's threshold
abline(h = 1-(1-alpha.star)^(1/M), col = "green", lty = 2) 

# BH threshold
abline(a = 0, b = alpha.star/M, col = "purple", lty = 2) 

# Legend:
legend("topleft", legend = c("Bonferroni", "Sidak", "Holmes", "BH"),bty = "n",
       col = c("blue", "green", "red", "purple"), lty = 2, cex = 0.7)

## Adjusted p-values

# Bonferroni:
p.adjust(p, method = "bonferroni")

# Šidák:
1-(1-p)^M

# Holm:
p.adjust(p, method = "holm")

# Benjamini-Hochberg:
p.adjust(p, method = "BH")

# A series of pairwise t-tests or chi-squared tests (given real data), that automatically perform
# the p-value adjustment, can be implemented with:
? pairwise.t.test
? pairwise.prop.test


