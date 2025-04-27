library('scales')
mmBase <- 6

begin <- 103.17
begin <- 107.05
begin <- 110.88
begin <- 114.78
begin <- 118.65
begin <- 122.54
begin <- 126.41
begin <- 130.31
begin <- 134.22
begin <- 138.12
begin <- 142.015
begin <- 145.92
begin <- 149.78
begin <- 153.475
interval <- (begin * 1000 + 1):((begin + 3.7) * 1000)

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

step <- 205
startPlate <- 870
outPosition <- median(df$POSITION[1:1000])
dHmm <- round(max(df$POSITION[1000:2000]) - min(df$POSITION[1000:2000]), digits = 3)
outRSrt <- sapply(2:12,
                  function(x) {
                    center <- x * step + startPlate
                    c(df$TIME[center], df$R1[center], df$R2[center])
                  }
)
outRSrt <- as.data.frame(t(outRSrt))
colnames(outRSrt) <- c('TIME', 'R1', 'R2')

outREnd <- sapply(2:12,
                  function(x) {
                    center <- x * step + startPlate + 70
                    c(df$TIME[center], df$R1[center], df$R2[center])
                  }
)
outREnd <- as.data.frame(t(outREnd))
colnames(outREnd) <- c('TIME', 'R1', 'R2')

out <- data.frame(outRSrt$TIME[1], outPosition,
                  outRSrt$R1[1], outRSrt$R2[1],
                  round(mean(abs(outREnd$R1 - outRSrt$R1)) * sign(median(outREnd$R1 - outRSrt$R1)), digits = 4),
                  round(mean(abs(outREnd$R2 - outRSrt$R2)) * sign(median(outREnd$R2 - outRSrt$R2)), digits = 4),
                  dHmm
)
out[nrow(out) + 1, ] <- c(
  rev(outRSrt$TIME)[1], outPosition,
  rev(outREnd$R1)[1], rev(outREnd$R2)[1],
  round(mean(abs(outREnd$R1 - outRSrt$R1)) * sign(median(outREnd$R1 - outRSrt$R1)), digits = 4),
  round(mean(abs(outREnd$R2 - outRSrt$R2)) * sign(median(outREnd$R2 - outRSrt$R2)), digits = 4),
  dHmm
)
colnames(out) <- c('TIME', 'POSITION', 'R1_START', 'R2_START', 'R1_DIFF', 'R2_DIFF', 'DH_MM')
write.table(out, quote = FALSE, sep = " | ", file = 'out.csv', row.names = FALSE, col.names = TRUE)

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
paste(outRSrt$TIME[1], outPosition,
      outRSrt$R1[1], outRSrt$R2[1],
      round(mean(abs(outREnd$R1 - outRSrt$R1)) * sign(outREnd$R1[1] - outRSrt$R1[1]), digits = 4),
      round(mean(abs(outREnd$R2 - outRSrt$R2)) * sign(outREnd$R2[1] - outRSrt$R2[1]), digits = 4),
      sep = ", ")