if (length(ls()) == 0) {
  source("main-readFiles.R")
}

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(out$TIME, out$RHO_S1, lty = 'blank', type = 'b', xlab = 'Time, s', ylab = expression(bold(rho ~ " 6 x 18 mm")), col = 'red')
plot(out$TIME, out$RHO_S2, lty = 'blank', type = 'b', xlab = 'Time, s', ylab = expression(bold(rho ~ " 30 x 18 mm")), col = 'orange')
plot(out$TIME, out$POSITION, lty = 'blank', type = 'b', xlab = 'Time, s', ylab = 'Indentation')


library("smoother")
window <- 10 / (out$TIME[length(out$TIME)] - out$TIME[1]) # 10 sec smoothing
par(mfrow = c(5, 1), mar = c(2, 6, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

smthRHO_S1 <- smth(out$RHO_S1, window = window, method = "gaussian")
plot(out$TIME, out$RHO_S1, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 6 x 18 mm")), col = 'red',
     ylim = c(min(na.omit(smthRHO_S1)), max(na.omit(smthRHO_S1))))
lines(out$TIME, smthRHO_S1, col = "black", lty = "solid", lwd = 2)

smthRHO_S1_DIFF <- smth(out$RHO_S1_DIFF, window = window, method = "gaussian")
plot(out$TIME, out$RHO_S1_DIFF, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(frac(partialdiff ~ rho, partialdiff ~ phi) ~ " 6 x 18 mm")), col = 'orange',
     ylim = c(min(na.omit(smthRHO_S1_DIFF)), max(na.omit(smthRHO_S1_DIFF))))
lines(out$TIME, smthRHO_S1_DIFF, col = "black", lty = "solid", lwd = 2)

smthRHO_S2 <- smth(out$RHO_S2, window = window, method = "gaussian")
plot(out$TIME, out$RHO_S2, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 30 x 18 mm")), col = 'red',
     ylim = c(min(na.omit(smthRHO_S2)), max(na.omit(smthRHO_S2))))
lines(out$TIME, smthRHO_S2, col = "black", lty = "solid", lwd = 2)

smthRHO_S2_DIFF <- smth(out$RHO_S2_DIFF, window = window, method = "gaussian")
plot(out$TIME, out$RHO_S2_DIFF, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = expression(bold(frac(partialdiff ~ rho, partialdiff ~ phi) ~ " 30 x 18 mm")), col = 'orange',
     ylim = c(min(na.omit(smthRHO_S2_DIFF)), max(na.omit(smthRHO_S2_DIFF))))
lines(out$TIME, smthRHO_S2_DIFF, col = "black", lty = "solid", lwd = 2)

plot(df$TIME, df$POSITION, lty = 'solid', type = 'l', xlab = 'Time, s', ylab = 'Indentation')

out_file <- na.omit(data.frame(out$TIME, out$POSITION, smthRHO_S1, smthRHO_S1_DIFF, smthRHO_S2, smthRHO_S2_DIFF))
colnames(out_file) <- c('TIME', 'POSITION', 'RHO_S1', 'RHO_S1_DIFF', 'RHO_S2', 'RHO_S2_DIFF')
write.csv(out_file, file = outFile, row.names = FALSE)