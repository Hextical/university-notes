## Optimizely Chi-Squared test Example

## The observed number of conversions in condition 1 was 280 and in condition 2 it was 399. 
## These conditions respectively contained n1 = 8872 and n2 = 8642 experimental units. This 
## is all the information we need to perform the chi-squared test in R. We don't need the 
## raw data itself, just these summaries.

## Perform the test
## Ho: pi1 = pi2 vs. Ha: pi1 != pi2
prop.test(x = c(280, 399), n = c(8872, 8642), alternative = "two.sided", correct = F)

## Manual p-value calculation
pnorm(q = -sqrt(25.075), lower.tail = TRUE) + pnorm(q = sqrt(25.075), lower.tail = FALSE)
pchisq(q = 25.075, df = 1, lower.tail = FALSE)


## Ho: pi1 <= pi2 vs. Ha: pi1 > pi2
prop.test(x = c(280, 399), n = c(8872, 8642), alternative = "greater", correct = F)

## Manual p-value calculation
pnorm(q = -sqrt(25.075), lower.tail = FALSE)
1-pchisq(q = 25.075, df = 1, lower.tail = FALSE)/2


## Ho: pi1 >= pi2 vs. Ha: pi1 < pi2
prop.test(x = c(280, 399), n = c(8872, 8642), alternative = "less", correct = F)

## Manual p-value calculation
pnorm(q = -sqrt(25.075), lower.tail = TRUE)
pchisq(q = 25.075, df = 1, lower.tail = FALSE)/2

