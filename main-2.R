library('scales')
mmBase <- 6

begin <- 56.87
begin <- 56.87 + 3.892
begin <- 56.87 + 3.892 * 2
begin <- 56.87 + 3.892 * 3
begin <- 56.87 + 3.892 * 4
begin <- 76.31
begin <- 76.31 + 3.87
begin <- 76.31 + 3.87 * 2
begin <- 76.31 + 3.87 * 3
begin <- 76.31 + 3.87 * 4
begin <- 76.31 + 3.87 * 5
begin <- 99.54

begin <- 126.41 - 3.87 * 6
begin <- 126.41 - 3.87 * 5
begin <- 126.41 - 3.87 * 4
begin <- 114.78
begin <- 118.65
begin <- 122.54
begin <- 126.41
begin <- 130.31
begin <- 134.22
begin <- 138.12
begin <- 142.015
begin <- 145.92
# begin <- 149.78
# begin <- 153.475

baseREndInterval <- 3.72 * 1000
interval <- (begin * 1000 + 1):(begin * 1000 + baseREndInterval + 300)

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
startPlate <- 860
outPosition <- median(df$POSITION[1:1000])
dHmm <- round(max(df$POSITION[1000:2000]) - min(df$POSITION[1000:2000]), digits = 3)

baseRStart <- as.data.frame(t(c(df$TIME[startPlate + step],
                                mean(df$R1[(startPlate + step - 70):(startPlate + step)]),
                                mean(df$R2[(startPlate + step - 70):(startPlate + step)]))))
colnames(baseRStart) <- c('TIME', 'R1', 'R2')
baseREnd <- as.data.frame(t(c(df$TIME[baseREndInterval],
                              mean(df$R1[(startPlate + step * 13.5):baseREndInterval]),
                              mean(df$R2[(startPlate + step * 13.5):baseREndInterval]))))
colnames(baseREnd) <- c('TIME', 'R1', 'R2')
baseREnd2 <- as.data.frame(t(c(df$TIME[baseREndInterval + 140],
                               df$R1[baseREndInterval + 140],
                               df$R2[baseREndInterval + 140])))
colnames(baseREnd2) <- c('TIME', 'R1', 'R2')

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
                    center <- x * step + startPlate + 90
                    c(df$TIME[center], df$R1[center], df$R2[center])
                  }
)
outREnd <- as.data.frame(t(outREnd))
colnames(outREnd) <- c('TIME', 'R1', 'R2')

out <- data.frame(baseRStart$TIME[1], outPosition,
                  round(baseRStart$R1[1], digits = 3), round(baseRStart$R2[1], digits = 3),
                  round(mean(abs(outREnd$R1 - outRSrt$R1)), digits = 4),
                  round(mean(abs(outREnd$R2 - outRSrt$R2)), digits = 4),
                  dHmm
)
out[nrow(out) + 1, ] <- c(
  baseREnd$TIME[1], outPosition,
  round(baseREnd$R1[1], digits = 3), round(baseREnd$R2[1], digits = 3),
  round(baseREnd2$R1 - baseREnd$R1, digits = 4),
  round(baseREnd2$R2 - baseREnd$R2, digits = 4),
  dHmm * 2
)
colnames(out) <- c('TIME', 'POSITION', 'R1_START', 'R2_START', 'R1_DIFF', 'R2_DIFF', 'DH_MM')
write.table(out, quote = FALSE, sep = " | ", file = 'out.csv', row.names = FALSE, col.names = TRUE)

# Графики выбранных точек начала и конца переходов
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.2, family = 'mono', las = 1, tck = 1)
lwd <- 2
col <- hue_pal()(4)
plot(df$TIME, df$R1, type = 'l', lwd = lwd, xlab = xlab, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$R1, type = 'b', lwd = lwd, lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$R1, type = 'b', lwd = lwd, lty = 'blank', col = col[2])
lines(baseRStart$TIME, baseRStart$R1, type = 'b', lwd = lwd, lty = 'blank', col = col[3])
lines(baseREnd$TIME, baseREnd$R1, type = 'b', lwd = lwd, lty = 'blank', col = col[3])
lines(baseREnd2$TIME, baseREnd2$R1, type = 'b', lwd = lwd, lty = 'blank', col = col[4])

plot(df$TIME, df$R2, type = 'l', lwd = 2, xlab = xlab, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), list(s = mmBase * 5, L = mmBase * 3)))
lines(outRSrt$TIME, outRSrt$R2, type = 'b', lwd = lwd, lty = 'blank', col = col[1])
lines(outREnd$TIME, outREnd$R2, type = 'b', lwd = lwd, lty = 'blank', col = col[2])
lines(baseRStart$TIME, baseRStart$R2, type = 'b', lwd = lwd, lty = 'blank', col = col[3])
lines(baseREnd$TIME, baseREnd$R2, type = 'b', lwd = lwd, lty = 'blank', col = col[3])
lines(baseREnd2$TIME, baseREnd2$R2, type = 'b', lwd = lwd, lty = 'blank', col = col[4])

plot(df$TIME, df$POSITION, type = 'l', lwd = lwd, xlab = xlab, ylab = 'POSITION, mm')
lines(baseRStart$TIME, outPosition, type = 'b', lwd = lwd, lty = 'blank', col = col[3])
lines(baseREnd$TIME, outPosition, type = 'b', lwd = lwd, lty = 'blank', col = col[3])
paste(baseRStart$TIME[1], outPosition,
      round(baseRStart$R1[1], digits = 3), round(baseRStart$R2[1], digits = 3),
      round(mean(abs(outREnd$R1 - outRSrt$R1)), digits = 4),
      round(mean(abs(outREnd$R2 - outRSrt$R2)), digits = 4),
      round(dHmm, digits = 3),
      sep = ", ")
paste(baseRStart$TIME[1], outPosition,
      round(baseREnd$R1[1], digits = 3), round(baseREnd$R2[1], digits = 3),
      round(baseREnd2$R1 - baseREnd$R1, digits = 4),
      round(baseREnd2$R2 - baseREnd$R2, digits = 4),
      round(dHmm * 2, digits = 3),
      sep = ", ")