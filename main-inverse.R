mmBase <- 6
interval <- (215 * 1000 + 1):(375 * 1000)
df <- merge(eval(parse(text = ls(pattern = "aper$"))), eval(parse(text = ls(pattern = "PureLogic$"))))[interval,]
df <- data.frame(df$TIME, df$R1, df$R2, df$POSITION)
colnames(df) <- c('TIME', 'R1', 'R2', 'POSITION')

inv <- eval(parse(text = ls(pattern = "inverse$")))
inv <- inv[inv$rho1 > 10 * inv$rho1AbsError &
             inv$rho2 > 10 * inv$rho2AbsError &
             inv$h > 10 * inv$hAbsError &
             inv$h < 10,]
xlim <- c(min(na.omit(inv$TIME)), max(na.omit(inv$TIME)))
# Графики исходных сигналов
par(mfrow = c(5, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', col = 'red', lwd = 2, xlim = xlim, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', col = 'orange', lwd = 2, xlim = xlim, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', col = 'blue', lwd = 2, xlim = xlim, ylab = 'POSITION, mm')
# Графики параметров модели
ylim <- c(min(na.omit(inv$rho1), na.omit(inv$rho2)), max(na.omit(inv$rho1), na.omit(inv$rho2)))
plot(inv$TIME, inv$rho1, ylim = ylim, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho)), col = 'red', lwd = 2, lty = 1)
lines(inv$TIME, inv$rho2, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[2])), col = 'orange', lwd = 2, lty = 5)
# legend("bottomleft",
#        legend = c(
#          expression(bold(rho[1])),
#          expression(bold(rho[2]))
#        ),
#        lty = c(1, 5), lwd = c(2, 2), col = c('red', 'orange'), horiz = T
# )
plot(inv$TIME, inv$h, type = 'l', xlab = 'Time, s', ylab = 'h', col = 'blue', lwd = 2)