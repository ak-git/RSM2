library('scales')
mmBase <- 6

inv <- read.csv(list.files(pattern = "inverse.csv$"))
xlim <- c(min(na.omit(inv$TIME)), max(na.omit(inv$TIME)))

interval <- (xlim[1] * 1000 + 1):(xlim[2] * 1000)
source(file = 'read.R')

# Графики исходных сигналов
par(mfrow = c(6, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(6)
xlab <- 'Time, s'
plot(df$TIME, df$R1, type = 'l', col = col[1], lwd = 2, xlim = xlim, xlab = xlab, ylab = expression(Omega),
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, col = col[2], lwd = 2, xlim = xlim, ylab = expression(Omega),
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', col = col[3], lwd = 2, xlim = xlim, xlab = xlab, ylab = 'mm',
     main = 'Position')

# Графики параметров модели
plot(inv$TIME, inv$rho1, type = 'l', col = col[4], lwd = 2, xlab = xlab, ylab = expression(Omega %.% m),
     main = expression(bold(rho[1])))
plot(inv$TIME, inv$rho2, type = 'l', col = col[5], lwd = 2, xlab = xlab, ylab = expression(Omega %.% m),
     main = expression(bold(rho[2])))
plot(inv$TIME, inv$h, type = 'l', col = col[6], lwd = 2, xlab = xlab, ylab = 'mm', main = 'h')