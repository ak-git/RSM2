library('scales')

mmBase <- 7

interval <- (0 * 1000 + 1):(117 * 1000)
rm(list = c("interval"))
source(file = 'read.R')

par(mfrow = c(3, 1), mar = c(3, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
xlab <- 'Время, с'
col <- hue_pal()(3)
lwd <- 3
plot(df$TIME, df$R1, type = 'l', xlab = xlab, ylab = expression(Omega), col = col[1], lwd = lwd,
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, ylab = expression(Omega), col = col[2], lwd = lwd,
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, ylab = 'mm', col = col[3], lwd = lwd,
     main = 'Положение электродной системы по вертикали')

write.csv(df, file = 'out-avg-selector.csv', row.names = FALSE)
