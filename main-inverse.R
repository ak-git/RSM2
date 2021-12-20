# max - 630 sec
sTime <- 190
eTime <- 190 + 45
outFile <- "2021-10-22 18-57-26 6 mm.csv"
interval <- (sTime * 1000 + 1):(eTime * 1000)
aper <- read.csv("2021-10-22 18-57-26 754 aper.csv")[interval,]
plog <- read.csv("2021-10-22 18-57-26 755 PureLogic.csv")[interval,]
df <- merge(aper, plog)

par(mfrow = c(6, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', ylab = expression(bold(R ~ " 6 x 18 mm")), lwd = 2)
plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', ylab = expression(bold(R ~ " 30 x 18 mm")), lwd = 2)
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'INDENTATION', col = 'darkgreen', lwd = 2)

inv <- read.csv("2021-10-22 18-57-26 6 mm inverse.csv")
inv <- inv[sTime < inv$TIME & inv$TIME < eTime,]
plot(inv$TIME, inv$rho1, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[1])), col = 'red', lwd = 2)
plot(inv$TIME, inv$rho2, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[2])), col = 'orange', lwd = 2)
plot(inv$TIME, inv$h, type = 'l', xlab = 'Time, s', ylab = 'h', col = 'blue', lwd = 2)