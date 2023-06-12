library('scales')
mmBase <- 7

fileSuffix <- rev(c(0.2, 0.5, 1.0))

filter <- function(inv) {
  return(inv[1 < inv$rho1 &
               inv$rho1 < 100 &
               1 < inv$rho2 &
               inv$rho2 < 100 &
               1 < inv$h &
               inv$h < 15,])
}

inv <- read.csv(list.files(pattern = paste0("inverse - ", format(fileSuffix[1], nsmall = 1), ".csv$")))
inv <- filter(inv)
xlim <- c(min(na.omit(inv$TIME)), max(na.omit(inv$TIME)))

interval <- (xlim[1] * 1000 + 1):(xlim[2] * 1000)
source(file = 'read.R')

# Данные параметров модели
lineType <- 'l'
col2 <- hue_pal()(length(fileSuffix))

inv <- lapply(seq_along(fileSuffix), function (i) {
  filter(read.csv(list.files(pattern = paste0("inverse - ", format(fileSuffix[i], nsmall = 1), ".csv$"))))
})

# Графики исходных сигналов
par(mfcol = c(3, 2), mar = c(3, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
xlab <- 'Время, с'
lwd <- 3
plot(df$TIME, df$R1, type = 'l', col = col[1], lwd = lwd, xlim = xlim, xlab = xlab, ylab = expression(Omega),
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase, L = mmBase * 3)),
     ylim = c(min(sapply(inv, function (i) {min(na.omit(i$PREDICTED_R1))}), df$R1), max(sapply(inv, function (i) {max(na.omit(i$PREDICTED_R1))}), df$R1)))
lapply(seq_along(fileSuffix), function (i) {
  inv2 <- inv[[i]]
  lines(inv2$TIME, inv2$PREDICTED_R1, type = lineType, col = col2[i], lwd = lwd, lty = i + 1)
})
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)

plot(df$TIME, df$R2, type = 'l', xlab = xlab, col = col[2], lwd = lwd, xlim = xlim, ylab = expression(Omega),
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase * 5, L = mmBase * 3)),
     ylim = c(min(sapply(inv, function (i) {min(na.omit(i$PREDICTED_R2))}), df$R2), max(sapply(inv, function (i) {max(na.omit(i$PREDICTED_R2))}), df$R2)))
lapply(seq_along(fileSuffix), function (i) {
  inv2 <- inv[[i]]
  lines(inv2$TIME, inv2$PREDICTED_R2, type = lineType, col = col2[i], lwd = lwd, lty = i + 1)
})
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)

plot(df$TIME, df$ANGLE, type = 'l', col = col[3], lwd = lwd, xlim = xlim, xlab = xlab, ylab = 'градусы',
     main = 'Угол разгибания кисти')

# Графики параметров модели
lapply(seq_along(fileSuffix), function (i) {
  inv2 <- inv[[i]]
  if (i == 1) {
    plot(inv2$TIME, inv2$rho1, type = lineType, col = col2[i], lwd = lwd, xlab = xlab, ylab = expression(Omega %.% m),
         main = expression(bold(rho[1])),
         ylim = c(min(sapply(inv, function (i) {min(na.omit(i$rho1))})), max(sapply(inv, function (i) {max(na.omit(i$rho1))}))))
  }
  else {
    lines(inv2$TIME, inv2$rho1, type = lineType, col = col2[i], lwd = lwd, lty = i)
  }
})
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)

lapply(seq_along(fileSuffix), function (i) {
  inv2 <- inv[[i]]
  if (i == 1) {
    plot(inv2$TIME, inv2$rho2, type = lineType, col = col2[i], lwd = lwd, xlab = xlab, ylab = expression(Omega %.% m),
         main = expression(bold(rho[2])),
         ylim = c(min(sapply(inv, function (i) {min(na.omit(i$rho2))})), max(sapply(inv, function (i) {max(na.omit(i$rho2))}))))
  }
  else {
    lines(inv2$TIME, inv2$rho2, type = lineType, col = col2[i], lwd = lwd, lty = i)
  }
})
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)

lapply(seq_along(fileSuffix), function (i) {
  inv2 <- inv[[i]]
  if (i == 1) {
    plot(inv2$TIME, inv2$h, type = lineType, col = col2[i], lwd = lwd, xlab = xlab, ylab = 'mm', main = 'h',
         ylim = c(min(sapply(inv, function (i) {min(na.omit(i$h))})), max(sapply(inv, function (i) {max(na.omit(i$h))}))))
  }
  else {
    lines(inv2$TIME, inv2$h, type = lineType, col = col2[i], lwd = lwd, lty = i)
  }
})
legend("topright", legend = fileSuffix, title = expression(alpha), lty = seq_along(fileSuffix), col = col2, horiz = F, cex = 0.8)