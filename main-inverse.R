inv <- read.csv("2021-05-12 19-03-11 7 mm inverse.csv")
inv <- inv[60 < inv$TIME & inv$TIME < 1000,]

par(mfrow = c(4, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(inv$TIME, inv$rho1, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[1])))
plot(inv$TIME, inv$rho2, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[2])))
plot(inv$TIME, inv$h, type = 'l', xlab = 'Time, s', ylab = 'h')
plot(inv$TIME, inv$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')