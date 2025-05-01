inverse2 <- read.csv(list.files(pattern = "E9712_NaN 6 mm inverse - 1.0.csv"), sep = "|")
inverse2 <- data.frame(inverse2$TIME, inverse2$POSITION, inverse2$RHO_1, inverse2$RHO_2, inverse2$DH_MM, inverse2$H_MM)
colnames(inverse2) <- c('TIME', 'POSITION', 'RHO_1', 'RHO_2', 'DH_MM', 'H_MM')
head(inverse2)

inverse2invalid <- read.csv(list.files(pattern = "E9712_90 6 mm inverse - 1.0.csv"), sep = "|")
inverse2invalid <- data.frame(inverse2invalid$TIME, inverse2invalid$POSITION, inverse2invalid$RHO_1, inverse2invalid$RHO_2, inverse2invalid$DH_MM, inverse2invalid$H_MM)
colnames(inverse2invalid) <- c('TIME', 'POSITION', 'RHO_1', 'RHO_2', 'DH_MM', 'H_MM')
head(inverse2invalid)

interval <- (inverse2$TIME[1] * 1000 + 1):(rev(inverse2$TIME)[1] * 1000)
source(file = 'read.R')

library('scales')
par(mfrow = c(4, 1), mar = c(2, 5, 2, 1), cex = 1.2, family = 'mono', las = 1, tck = 1)
lwd <- 2
col <- hue_pal()(8)
xlab <- 'Time, s'

plot(inverse2invalid$TIME, inverse2invalid$RHO_1, type = 'l', lwd = lwd, xlab = xlab, col = col[1],
     ylab = expression(rho[1]~~Ohm-m))
lines(inverse2$TIME, inverse2$RHO_1, type = 'l', lty = 'dashed', lwd = lwd, col = col[2])
legend("bottomright", box.col = 'black',
       title = '2-layer inverse solution',
       legend = c(
         expression(bold(dh == const)),
         expression(bold(dh == auto))
       ),
       lty = c('solid', 'dashed'), lwd = c(2, 2), col = col[1:2], horiz = T
)

plot(inverse2invalid$TIME, inverse2invalid$RHO_2, type = 'l', lwd = lwd, xlab = xlab, col = col[3],
     ylab = expression(rho[2]~~Ohm-m))
lines(inverse2$TIME, inverse2$RHO_2, type = 'l', lty = 'dashed', lwd = lwd, col = col[4])
legend("topright", box.col = 'black',
       title = '2-layer inverse solution',
       legend = c(
         expression(bold(dh == const)),
         expression(bold(dh == auto))
       ),
       lty = c('solid', 'dashed'), lwd = c(2, 2), col = col[3:4], horiz = T
)

plot(inverse2invalid$TIME, inverse2invalid$H_MM, type = 'l', lwd = lwd, xlab = xlab, col = col[5], ylab = 'h, mm')
lines(inverse2$TIME, inverse2$H_MM, type = 'l', lty = 'dashed', lwd = lwd, col = col[6])
legend("topright", box.col = 'black',
       title = '2-layer inverse solution',
       legend = c(
         expression(bold(dh == const)),
         expression(bold(dh == auto))
       ),
       lty = c('solid', 'dashed'), lwd = c(2, 2), col = col[5:6], horiz = T
)

plot(inverse2invalid$TIME, inverse2invalid$DH_MM * 1000, type = 'l', lwd = lwd, xlab = xlab, col = col[7], ylab = 'dh, mkm',
     ylim = c(10, 90))
lines(inverse2$TIME, inverse2$DH_MM * 1000, type = 'l', lty = 'dashed', lwd = lwd, col = col[8])
legend("bottomright", box.col = 'black',
       title = '2-layer inverse solution',
       legend = c(
         expression(bold(dh == const)),
         expression(bold(dh == auto))
       ),
       lty = c('solid', 'dashed'), lwd = c(2, 2), col = col[7:8], horiz = T
)