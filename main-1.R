library('scales')
mmBase <- 6

interval <- (0 * 1000):(77 * 1000)
interval <- (70 * 1000):(77 * 1000)

source(file = 'read.R')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
xlab <- 'Time, s'
plot(df$TIME, df$R1, type = 'l', xlab = xlab, col = col[1], lwd = 2,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, col = col[2], lwd = 2,
     ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, col = col[3], lwd = 2, ylab = 'POSITION, mm')

R <- c(mean(df$R1), mean(df$R2))
paste("R =", round(R, 3), collapse = "; ")