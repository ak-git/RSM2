library('scales')
mmBase <- 6
interval <- (215 * 1000 + 1):(375 * 1000)

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
# Графики кажущихся удельных сопротивлений
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
plot(df$TIME, df$A1, type = 'l', xlab = xlab, col = col[1], lwd = 2,
     ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$A2, type = 'l', xlab = xlab, col = col[2], lwd = 2,
     ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, col = col[3], lwd = 2, ylab = 'POSITION, mm')

step <- 500
outPosition <- sapply(1:(length(df$TIME) / step),
                      function(x) {
                        center <- x * step - step / 2 + 1
                        c(df$TIME[center], df$POSITION[center])
                      }
)
outPosition <- as.data.frame(t(outPosition))
colnames(outPosition) <- c('TIME', 'POSITION')

outRSrt <- sapply(1:(length(df$TIME) / step),
                  function(x) {
                    center <- x * step - step / 2 + 1 - step / 4
                    start <- center
                    end <- center + step / 10
                    interval <- (start):(end)
                    R1 <- mean(df$R1[interval])
                    R2 <- mean(df$R2[interval])
                    c(df$TIME[start], R1, R2)
                  }
)
outRSrt <- as.data.frame(t(outRSrt))
colnames(outRSrt) <- c('TIME', 'R1', 'R2')

outREnd <- sapply(1:(length(df$TIME) / step),
                  function(x) {
                    center <- x * step - step / 2 + 1 + step / 4
                    start <- center - step / 10
                    end <- center
                    interval <- (start):(end)
                    R1 <- mean(df$R1[interval])
                    R2 <- mean(df$R2[interval])
                    c(df$TIME[end], R1, R2)
                  }
)
outREnd <- as.data.frame(t(outREnd))
colnames(outREnd) <- c('TIME', 'R1', 'R2')

# Графики выбранных точек начала и конца переходов
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
plot(df$TIME, df$R1, type = 'l', xlab = xlab, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$R1, type = 'b', lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$R1, type = 'b', lty = 'blank', col = col[2])

plot(df$TIME, df$R2, type = 'l', xlab = xlab, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$R2, type = 'b', lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$R2, type = 'b', lty = 'blank', col = col[2])

plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, ylab = 'POSITION, mm')
lines(outPosition$TIME, outPosition$POSITION, type = 'b', lty = 'blank', col = col[3])

outA <- sapply(2:(length(outPosition$TIME) - 1),
               function(x) {
                 R1 <- max(mean(c(outREnd$R1[x - 1], outRSrt$R1[x + 1])), mean(c(outRSrt$R1[x], outREnd$R1[x])))
                 R2 <- max(mean(c(outREnd$R2[x - 1], outRSrt$R2[x + 1])), mean(c(outRSrt$R2[x], outREnd$R2[x])))
                 A1 <- layer1Inverse(mmBase, mmBase * 3.0, R1)
                 A2 <- layer1Inverse(mmBase * 5.0, mmBase * 3.0, R2)

                 DR1 <- mean(
                   c(
                     (outRSrt$R1[x] - outREnd$R1[x - 1]) / ((outPosition$POSITION[x] - outPosition$POSITION[x - 1]) / (mmBase * 3)),
                     (outRSrt$R1[x + 1] - outREnd$R1[x]) / ((outPosition$POSITION[x + 1] - outPosition$POSITION[x]) / (mmBase * 3))
                   )
                 )
                 DR2 <- mean(
                   c(
                     (outRSrt$R2[x] - outREnd$R2[x - 1]) / ((outPosition$POSITION[x] - outPosition$POSITION[x - 1]) / (mmBase * 3)),
                     (outRSrt$R2[x + 1] - outREnd$R2[x]) / ((outPosition$POSITION[x + 1] - outPosition$POSITION[x]) / (mmBase * 3))
                   )
                 )
                 DA1 <- layer1Inverse(mmBase, mmBase * 3.0, DR1)
                 DA2 <- layer1Inverse(mmBase * 5.0, mmBase * 3.0, DR2)
                 c(outPosition$TIME[x], outPosition$POSITION[x], A1, A2, DA1, DA2)
               }
)
outA <- as.data.frame(t(outA))
colnames(outA) <- c('TIME', 'POSITION', 'A1', 'A2', 'DA1', 'DA2')

# Графики кажущихся удельных сопротивлений
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(3)
plot(df$TIME, df$A1, type = 'l', xlab = xlab, col = col[1], lwd = 2,
     ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase, L = mmBase * 3)))
lines(outA$TIME, outA$A1, type = 'b', lty = 'blank', col = 'black')
plot(df$TIME, df$A2, type = 'l', xlab = xlab, col = col[2], lwd = 2,
     ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase * 5, L = mmBase * 3)))
lines(outA$TIME, outA$A2, type = 'b', lty = 'blank', col = 'black')
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, col = col[3], lwd = 2, ylab = 'POSITION, mm')
lines(outA$TIME, outA$POSITION, type = 'b', lty = 'blank', col = 'black')

library("smoother")
window <- 7 / (outA$TIME[length(outA$TIME)] - outA$TIME[1]) # 7 sec smoothing
smthA1 <- smth(outA$A1, window = window, method = "gaussian")
smthA2 <- smth(outA$A2, window = window, method = "gaussian")
smthDA1 <- smth(outA$DA1, window = window, method = "gaussian")
smthDA2 <- smth(outA$DA2, window = window, method = "gaussian")

# Графики кажущихся удельных сопротивлений и их производных
par(mfrow = c(5, 1), mar = c(2, 6, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
col <- hue_pal()(5)
plot(outA$TIME, outA$A1, type = 'l', xlab = xlab, col = 'black', lwd = 1,
     ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase, L = mmBase * 3)))
lines(outA$TIME, smthA1, col = col[1], lty = "solid", lwd = 2)

plot(outA$TIME, outA$A2, type = 'l', xlab = xlab, col = 'black', lwd = 1,
     ylab = substitute(bold(rho[s ~ x ~ L ~ mm] ~ ~Omega %.% ~~m), list(s = mmBase * 5, L = mmBase * 3)))
lines(outA$TIME, smthA2, col = col[2], lty = "solid", lwd = 2)

plot(outA$TIME, outA$DA1, type = 'l', xlab = xlab, col = 'black', lwd = 1,
     ylab = substitute(bold(frac(partialdiff ~ rho[s ~ x ~ L ~ mm], partialdiff ~ psi) ~ ~Omega %.% ~~m), list(s = mmBase, L = mmBase * 3)))
lines(outA$TIME, smthDA1, col = col[3], lty = "solid", lwd = 2)

plot(outA$TIME, outA$DA2, type = 'l', xlab = xlab, col = 'black', lwd = 1,
     ylab = substitute(bold(frac(partialdiff ~ rho[s ~ x ~ L ~ mm], partialdiff ~ psi) ~ ~Omega %.% ~~m), list(s = mmBase * 5, L = mmBase * 3)))
lines(outA$TIME, smthDA2, col = col[4], lty = "solid", lwd = 2)

plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, col = col[5], lwd = 2,
     ylab = 'POSITION, mm', xlim = c(min(outA$TIME), max(outA$TIME)))

out_file <- na.omit(data.frame(outA$TIME, outA$POSITION, smthA1, smthA2, smthDA1, smthDA2))
colnames(out_file) <- c('TIME', 'POSITION', 'A1', 'A2', 'DA1', 'DA2')
write.csv(out_file, file = paste('out', mmBase, 'mm.csv', sep = ' '), row.names = FALSE)