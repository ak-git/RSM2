library('scales')
mmBase <- 7

interval <- (13 * 1000 + 1):(23 * 1000)
interval <- (34 * 1000 + 1):(44 * 1000)
interval <- (45 * 1000 + 1):(55 * 1000)

interval <- (75 * 1000 + 1):(84 * 1000)
interval <- (85 * 1000 + 1):(95 * 1000)


source(file = 'read.R')

# Графики исходных сигналов
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
xlab <- 'Time, s'
plot(df$TIME, df$R1, type = 'l', xlab = xlab, col = col[1], lwd = 2,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, col = col[2], lwd = 2,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, col = col[3], lwd = 2, ylab = 'POSITION, mm')

paste(round(mean(df$R1), digits = 3), round(mean(df$R2), digits = 3), sep = ", ")