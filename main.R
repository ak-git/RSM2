par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(df$TIME, df$APPARENT_RHO_06_18, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[a] ~ " 6 x 18 mm")), col = 'red')
plot(df$TIME, df$APPARENT_RHO_30_18, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[a] ~ " 30 x 18 mm")), col = 'orange')
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')