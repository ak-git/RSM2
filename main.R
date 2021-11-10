if (length(ls()) == 0) {
  source("main-readFiles.R")
}

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(out$TIME, out$A_RHO_S1, lty = 'blank', type = 'b', xlab = 'Time, s', ylab = expression(bold(rho[a] ~ " 6 x 18 mm")), col = 'red')
plot(out$TIME, out$A_RHO_S2, lty = 'blank', type = 'b', xlab = 'Time, s', ylab = expression(bold(rho[a] ~ " 30 x 18 mm")), col = 'orange')
plot(out$TIME, out$POSITION, lty = 'blank', type = 'b', xlab = 'Time, s', ylab = 'Indentation')


library("smoother")
window <- 10 / (out$TIME[length(out$TIME)] - out$TIME[1]) # 7 sec smoothing
par(mfrow = c(5, 1), mar = c(2, 6, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(out$TIME, out$A_RHO_S1, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 6 x 18 mm")), col = 'red')
lines(out$TIME, smth(out$A_RHO_S1, window = window, method = "gaussian"), col = "black", lty = "solid", lwd = 2)

plot(out$TIME, out$dA_RHO_S1, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(frac(partialdiff ~ rho, partialdiff ~ phi) ~ " 6 x 18 mm")), col = 'orange')
lines(out$TIME, smth(out$dA_RHO_S1, window = window, method = "gaussian"), col = "black", lty = "solid", lwd = 2)

plot(out$TIME, out$A_RHO_S2, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 30 x 18 mm")), col = 'red')
lines(out$TIME, smth(out$A_RHO_S2, window = window, method = "gaussian"), col = "black", lty = "solid", lwd = 2)

plot(out$TIME, out$dA_RHO_S2, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(frac(partialdiff ~ rho, partialdiff ~ phi) ~ " 30 x 18 mm")), col = 'orange')
lines(out$TIME, smth(out$dA_RHO_S2, window = window, method = "gaussian"), col = "black", lty = "solid", lwd = 2)

plot(df$TIME, df$POSITION, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = 'Indentation')

write.csv(out, file = outFile, row.names = FALSE)