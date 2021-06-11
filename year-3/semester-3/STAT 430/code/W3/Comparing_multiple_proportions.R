###################################
## Nike SB Chi-Squared test Example

## The observed number of views in conditions 1 to 5 were respectively, 160, 95, 141, 293, 
## and 196. These conditions contained n1 = 5014, n2 = 4971, n3 = 5030, n4 = 5007, and 
## n5 = 4980 experimental units. Using this and only this information we can now  perform 
## the chi-squared test in R. 

## Perform the test
## Ho: pi1 = pi2 = pi3 = pi4 = pi5 vs. Ha: pij != pik for some k != j
prop.test(x = c(160, 95, 141, 293, 197), n = c(5014, 4971, 5030, 5007, 4980), correct = F)


## Manual calculation of the p-value:
pchisq(q = 129.1686, df = 4, lower.tail = FALSE)

## Double check that this is correct:
prop.test(x = c(160, 95, 141, 293, 197), n = c(5014, 4971, 5030, 5007, 4980), correct = F)$p.value



