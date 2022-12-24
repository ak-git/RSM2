library('scales')
mmBase <- 6
interval <- (185 * 1000 + 1):(240 * 1000)

source(file = 'read.R')

inv <- read.csv(list.files(pattern = "inverse.csv$"))
tail(inv)
inv <- inv[inv$rho1 > 10 * inv$rho1AbsError &
             inv$rho2 > 10 * inv$rho2AbsError &
             inv$h > 10 * inv$hAbsError &
             inv$h < 10,]
xlim <- c(min(na.omit(inv$TIME)), max(na.omit(inv$TIME)))

# Графики исходных сигналов
par(mfrow = c(5, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
xlab <- 'Time, s'
plot(df$TIME, df$R1, type = 'l', xlab = xlab, col = col[1], lwd = 2, xlim = xlim,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, col = col[2], lwd = 2, xlim = xlim,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, col = col[3], lwd = 2, xlim = xlim, ylab = 'POSITION, mm')

# Графики параметров модели
ylim <- c(min(na.omit(inv$rho1), na.omit(inv$rho2)), max(na.omit(inv$rho1), na.omit(inv$rho2)))
col <- hue_pal()(3)
lty <- c(1, 5)
plot(inv$TIME, inv$rho1, ylim = ylim, type = 'l', xlab = xlab, ylab = expression(bold(rho)), col = col[1], lwd = 2, lty = lty[1])
lines(inv$TIME, inv$rho2, type = 'l', xlab = xlab, ylab = expression(bold(rho[2])), col = col[2], lwd = 2, lty = lty[2])
legend("topright", legend = c(expression(bold(rho[1])), expression(bold(rho[2]))),
       lty = lty, lwd = c(2, 2), col = col, horiz = F, cex = 0.7
)
plot(inv$TIME, inv$h, type = 'l', xlab = xlab, ylab = 'h', col = col[3], lwd = 2)