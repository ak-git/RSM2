library('scales')
mmBase <- 7
interval <- (134 * 1000 + 1):(144 * 1000)

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

step <- 254
outPosition <- sapply(0:(length(df$TIME) / step),
                      function(x) {
                        center <- x * step + step / 2
                        c(df$TIME[center], df$POSITION[center])
                      }
)
outPosition <- as.data.frame(t(outPosition))
colnames(outPosition) <- c('TIME', 'POSITION')

outRSrt <- sapply(0:(length(df$TIME) / step),
                  function(x) {
                    center <- x * step + step / 2
                    start <- center - step / 8
                    end <- center + step / 8
                    interval <- (start):(end)
                    R1 <- max(df$R1[interval])
                    R2 <- max(df$R2[interval])
                    c(df$TIME[center], R1, R2)
                  }
)
outRSrt <- as.data.frame(t(outRSrt))
colnames(outRSrt) <- c('TIME', 'R1', 'R2')

outREnd <- sapply(0:(length(df$TIME) / step),
                  function(x) {
                    center <- x * step + step
                    start <- center - step / 8
                    end <- center + step / 8
                    interval <- (start):(end)
                    R1 <- min(df$R1[interval])
                    R2 <- min(df$R2[interval])
                    c(df$TIME[center], R1, R2)
                  }
)
outREnd <- as.data.frame(t(outREnd))
colnames(outREnd) <- c('TIME', 'R1', 'R2')

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

outA <- sapply(1:(length(outPosition$TIME) - 1),
               function(x) {
                 R1begin <- outRSrt$R1[x]
                 R2begin <- outRSrt$R2[x]

                 R1end <- outREnd$R1[x]
                 R2end <- outREnd$R2[x]

                 c(outPosition$TIME[x], outPosition$POSITION[x], R1begin, R2begin, R1end, R2end)
               }
)
outA <- as.data.frame(t(outA))
colnames(outA) <- c('TIME', 'POSITION', 'R1-begin', 'R2-begin', 'R1-end', 'R2-end')
write.csv(outA, file = paste('out', mmBase, 'mm.csv', sep = ' '), row.names = TRUE)
paste(mean(df$R1), mean(df$R2), mean(outA$`R1-begin` - outA$`R1-end`), mean(outA$`R2-begin` - outA$`R2-end`), sep = ", ")