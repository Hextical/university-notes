library(astsa)
library(fpp2)
library(vars)

cmort2 = ts(lap[seq(1, 508, by = 4), 3], frequency = 13)
temp2 = ts(lap[seq(1, 508, by = 4), 4], frequency = 13)
part2 = ts(lap[seq(1, 508, by = 4), 11], frequency = 13)
cmorttrue = window(cmort2, start = c(9, 12), end = c(10, 11))
cmort2 = ts(cmort2[-c((length(cmort2) - 11):length(cmort2))], frequency = 13)
temp2 = ts(temp2[-c((length(cmort2) - 11):length(cmort2))], frequency = 13)
part2 = ts(part2[-c((length(cmort2) - 11):length(cmort2))], frequency = 13)

dat.mat = cbind(as.numeric(temp2), as.numeric(part2))

dat.mat.full = cbind(as.numeric(cmort2), as.numeric(temp2), as.numeric(part2))
colnames(dat.mat.full) = c("Cmort", "Temp", "Part")

par(mfrow = c(3, 1))
plot(cmort2)
plot(temp2)
plot(part2)


dev.off()

ccf(cmort2, temp2)
ccf(cmort2, part2)

ccf(diff(cmort2), diff(temp2))
ccf(diff(cmort2), diff(part2))
