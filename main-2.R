library('scales')
mmBase <- 7

interval <- (24.4 * 1000 + 1):(26 * 1000)
interval <- (29 * 1000 + 1):(30.5 * 1000)

interval <- (52.2 * 1000 + 1):(53.6 * 1000)
interval <- (56.8 * 1000 + 1):(58.4 * 1000)

interval <- (80.0 * 1000 + 1):(82.5 * 1000)
interval <- (84.6 * 1000 + 1):(87.1 * 1000)

interval <- (89.3 * 1000 + 1):(91.8 * 1000)
interval <- (94 * 1000 + 1):(96.5 * 1000)
interval <- (98.7 * 1000 + 1):(101.2 * 1000)
#
# interval <- (108 * 1000 + 1):(109.5 * 1000)
# interval <- (112.6 * 1000 + 1):(114.1 * 1000)

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

mmToSI <- function(mm) {
  return(mm / 1000.0)
}

layer1Inverse <- function(smm, lmm, ohms) {
  mmToSI(smm) -> s
  mmToSI(lmm) -> l

  (ohms * pi) / (2.0 / abs(l - s) - 2.0 / (l + s)) -> rho
  return(rho)
}

df$A1 <- layer1Inverse(mmBase, mmBase * 3.0, df$R1)
df$A2 <- layer1Inverse(mmBase * 5.0, mmBase * 3.0, df$R2)

step <- 1000 / (6 / 2)
outPosition <- sapply(1:(length(df$TIME) / step - 1),
                      function(x) {
                        center <- x * step
                        c(df$TIME[center], df$POSITION[center])
                      }
)
outPosition <- as.data.frame(t(outPosition))
colnames(outPosition) <- c('TIME', 'POSITION')

outRSrt <- sapply(1:3,
                  function(x) {
                    center <- x * step
                    start <- center - step / 1.5
                    end <- center + step / 1.5
                    interval <- (start):(end)
                    R1 <- max(df$R1[interval])
                    R2 <- max(df$R2[interval])
                    A1 <- max(df$A1[interval])
                    A2 <- max(df$A2[interval])
                    c(df$TIME[center], R1, R2, A1, A2)
                  }
)
outRSrt <- as.data.frame(t(outRSrt))
colnames(outRSrt) <- c('TIME', 'R1', 'R2', 'A1', 'A2')

outREnd <- sapply(1:3,
                  function(x) {
                    center <- x * step + step / 2
                    start <- center - step / 1.5
                    end <- center + step / 1.5
                    interval <- (start):(end)
                    R1 <- min(df$R1[interval])
                    R2 <- min(df$R2[interval])
                    A1 <- min(df$A1[interval])
                    A2 <- min(df$A2[interval])
                    c(df$TIME[center], R1, R2, A1, A2)
                  }
)
outREnd <- as.data.frame(t(outREnd))
colnames(outREnd) <- c('TIME', 'R1', 'R2', 'A1', 'A2')

# Графики выбранных точек начала и конца переходов кажущихся удельных сопротивлений
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.2, family = 'mono', las = 1, tck = 1)
lwd <- 2
col <- hue_pal()(3)
plot(df$TIME, df$A1, type = 'l', lwd = lwd, xlab = xlab, ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$A1, type = 'b', lwd = lwd, lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$A1, type = 'b', lwd = lwd, lty = 'blank', col = col[2])

plot(df$TIME, df$A2, type = 'l', lwd = 2, xlab = xlab, ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase * 5, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$A2, type = 'b', lwd = lwd, lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$A2, type = 'b', lwd = lwd, lty = 'blank', col = col[2])

plot(df$TIME, df$POSITION, type = 'l', lwd = lwd, xlab = xlab, ylab = 'POSITION, mm')
lines(outPosition$TIME, outPosition$POSITION, type = 'b', lwd = lwd, lty = 'blank', col = col[3])

# Графики выбранных точек начала и конца переходов
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.2, family = 'mono', las = 1, tck = 1)
lwd <- 2
col <- hue_pal()(3)
plot(df$TIME, df$R1, type = 'l', lwd = lwd, xlab = xlab, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$R1, type = 'b', lwd = lwd, lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$R1, type = 'b', lwd = lwd, lty = 'blank', col = col[2])

plot(df$TIME, df$R2, type = 'l', lwd = 2, xlab = xlab, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$R2, type = 'b', lwd = lwd, lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$R2, type = 'b', lwd = lwd, lty = 'blank', col = col[2])

plot(df$TIME, df$POSITION, type = 'l', lwd = lwd, xlab = xlab, ylab = 'POSITION, mm')
lines(outPosition$TIME, outPosition$POSITION, type = 'b', lwd = lwd, lty = 'blank', col = col[3])

# Данные для расчета
paste("")
paste(min(df$POSITION), "mm;", max(df$POSITION), "mm;", min(interval - 1) / 1000, " - ", max(interval) / 1000, "s")
paste("rho / diff rho")
paste(mean(c(median(outRSrt$A1), median(outREnd$A1))),
      mean(c(median(outRSrt$A2), median(outREnd$A2))),
      (max(median(outRSrt$A1), median(outREnd$A1)) - min(median(outRSrt$A1), median(outREnd$A1))) / ((max(df$POSITION) - min(df$POSITION)) / (mmBase * 3)),
      (max(median(outRSrt$A2), median(outREnd$A2)) - min(median(outRSrt$A2), median(outREnd$A2))) / ((max(df$POSITION) - min(df$POSITION)) / (mmBase * 3)),
      sep = ", ")
paste("Last 1 sec avg + 45 mkm diff")
meanR1 <- median(rev(df$R1)[1:1000])
meanR2 <- median(rev(df$R2)[1:1000])
paste(meanR1, meanR2, meanR1 + median(abs(outRSrt$R1 - outREnd$R1)) / 2, meanR2 + median(abs(outRSrt$R2 - outREnd$R2)) / 2, sep = ", ")