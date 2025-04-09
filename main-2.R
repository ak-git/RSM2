library('scales')
mmBase <- 7

begin <- 73.76
begin <- begin + 2.42 / 2
begin <- begin + 2.42 / 2

begin <- begin + 2.43 / 2
begin <- begin + 2.43 / 2

begin <- begin + 2.42 / 2
begin <- begin + 2.42 / 2

begin <- begin + 2.42 / 2
begin <- begin + 2.42 / 2

begin <- begin + 2.43 / 2
begin <- begin + 2.43 / 2

begin <- begin + 2.43 / 2
begin <- begin + 2.43 / 2

begin <- begin + 2.42 / 2
begin <- begin + 2.42 / 2

begin <- begin + 2.44 / 2
begin <- begin + 2.44 / 2

begin <- begin + 2.42 / 2
begin <- begin + 2.42 / 2

begin <- begin + 2.42 / 2
begin <- begin + 2.42 / 2

begin <- begin + 2.42 / 2
begin <- begin + 2.42 / 2

begin <- begin + 2.43 / 2
begin <- begin + 2.43 / 2

begin <- begin + 2.43 / 2
begin <- begin + 2.43 / 2

interval <- (begin * 1000 + 1):((begin + 2) * 1000)

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

step <- 1000 / 5
outPosition <- sapply(1:4,
                      function(x) {
                        center <- x * step - step / 2
                        c(df$TIME[center], df$POSITION[center])
                      }
)
outPosition <- as.data.frame(t(outPosition))
colnames(outPosition) <- c('TIME', 'POSITION')

outRSrt <- sapply(1:4,
                  function(x) {
                    center <- x * step
                    start <- center - step / 20
                    end <- center + step / 20
                    interval <- (start):(end)
                    R1 <- min(df$R1[interval])
                    R2 <- min(df$R2[interval])
                    c(df$TIME[center], R1, R2)
                  }
)
outRSrt <- as.data.frame(t(outRSrt))
colnames(outRSrt) <- c('TIME', 'R1', 'R2')

outREnd <- sapply(1:4,
                  function(x) {
                    center <- x * step + step / 2
                    start <- center - step / 20
                    end <- center + step / 20
                    interval <- (start):(end)
                    R1 <- max(df$R1[interval])
                    R2 <- max(df$R2[interval])
                    c(df$TIME[center], R1, R2)
                  }
)
outREnd <- as.data.frame(t(outREnd))
colnames(outREnd) <- c('TIME', 'R1', 'R2')

out <- data.frame(outRSrt$TIME[1], outPosition$POSITION[1],
                  outRSrt$R1[1], outRSrt$R2[1],
                  round(mean(abs(outREnd$R1 - outRSrt$R1)) * sign(outREnd$R1[1] - outRSrt$R1[1]), digits = 4),
                  round(mean(abs(outREnd$R2 - outRSrt$R2)) * sign(outREnd$R2[1] - outRSrt$R2[1]), digits = 4)
)
out[nrow(out) + 1, ] <- c(
  rev(outRSrt$TIME)[1], outPosition$POSITION[1],
  rev(outREnd$R1)[1], rev(outREnd$R2)[1],
  round(mean(abs(outREnd$R1 - outRSrt$R1)) * sign(outREnd$R1[1] - outRSrt$R1[1]), digits = 4),
  round(mean(abs(outREnd$R2 - outRSrt$R2)) * sign(outREnd$R2[1] - outRSrt$R2[1]), digits = 4)
)
colnames(out) <- c('TIME', 'POSITION', 'R1_START', 'R2_START', 'R1_DIFF', 'R2_DIFF')
write.table(out, quote = FALSE, sep = " | ", file = 'out.csv', row.names = FALSE, col.names = FALSE)

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
paste(min(df$POSITION))