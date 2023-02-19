library('scales')
mmBase <- 7

filter <- function(inv) {
  return(inv[1 < inv$rho1 &
               inv$rho1 < 100 &
               1 < inv$rho2 &
               inv$rho2 < 100 &
               1 < inv$h &
               inv$h < 8,])
}

inv <- read.csv(list.files(pattern = "inverse.csv$"))
inv <- filter(inv)
xlim <- c(min(na.omit(inv$TIME)), max(na.omit(inv$TIME)))

interval <- (xlim[1] * 1000 + 1):(xlim[2] * 1000)
source(file = 'read.R')

# Графики исходных сигналов
par(mfcol = c(3, 2), mar = c(3, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
xlab <- 'Время, с'
lwd <- 3
plot(df$TIME, df$R1, type = 'l', col = col[1], lwd = lwd, xlim = xlim, xlab = xlab, ylab = expression(Omega),
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, col = col[2], lwd = lwd, xlim = xlim, ylab = expression(Omega),
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', col = col[3], lwd = lwd, xlim = xlim, xlab = xlab, ylab = 'mm',
     main = 'Положение электродной системы по вертикали')

# Графики параметров модели
fileSuffix <- c(0.0, 1.0, 10.0)
lineType <- 'l'
col2 <- hue_pal()(length(fileSuffix))
for (i in seq_along(fileSuffix)) {
  if (i == 1) {
    plot(inv$TIME, inv$rho1, type = lineType, col = col2[i], lwd = lwd, xlab = xlab, ylab = expression(Omega %.% m),
         main = expression(bold(rho[1])))
  }
  else {
    inv2 <- read.csv(list.files(pattern = paste0("inverse - ", format(fileSuffix[i], nsmall = 1), ".csv$")))
    inv2 <- filter(inv2)
    lines(inv2$TIME, inv2$rho1, type = lineType, col = col2[i], lwd = lwd, lty = i)
  }
}
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)

for (i in seq_along(fileSuffix)) {
  if (i == 1) {
    plot(inv$TIME, inv$rho2, type = lineType, col = col2[i], lwd = lwd, xlab = xlab, ylab = expression(Omega %.% m),
         main = expression(bold(rho[2])))
  }
  else {
    inv2 <- read.csv(list.files(pattern = paste0("inverse - ", format(fileSuffix[i], nsmall = 1), ".csv$")))
    inv2 <- filter(inv2)
    lines(inv2$TIME, inv2$rho2, type = lineType, col = col2[i], lwd = lwd, lty = i)
  }
}
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)

for (i in seq_along(fileSuffix)) {
  if (i == 1) {
    plot(inv$TIME, inv$h, type = lineType, col = col2[i], lwd = lwd, xlab = xlab, ylab = 'mm', main = 'h')
  }
  else {
    inv2 <- read.csv(list.files(pattern = paste0("inverse - ", format(fileSuffix[i], nsmall = 1), ".csv$")))
    inv2 <- filter(inv2)
    lines(inv2$TIME, inv2$h, type = lineType, col = col2[i], lwd = lwd, lty = i)
  }
}
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)