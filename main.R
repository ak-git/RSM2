mmBase <- 8
fixOhms <- as.integer(readline("switch? 0 - no, 1 - yes")) > 0
interval <- (1 * 1000 + 1):(80 * 1000)
df <- read.csv(list.files(pattern = ".csv$"))[interval,]


yLab1 <- list(s = mmBase, L = mmBase * 3)
yLab2 <- list(s = mmBase * 5, L = mmBase * 3)
if (fixOhms) {
  df$R2 <- df$R2 * 2 - df$R1 * 2
  df$R1 <- df$R1 * 2
  yLab1 <- list(s = mmBase * 2, L = mmBase * 4)
  yLab2 <- list(s = mmBase * 6, L = mmBase * 4)
}

# Графики исходных сигналов
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', col = 'red', lwd = 2, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), yLab1))
plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', col = 'orange', lwd = 2, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), yLab2))
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', col = 'blue', lwd = 2, ylab = 'POSITION, mm')

step <- 4000
step2 <- 2000
centerShift <- 0
splinePosition <- sapply(0:(length(df$TIME) / step),
                         function(x) {
                           center <- x * step + step2 / 2 + 1
                           c(df$TIME[center], df$POSITION[center])
                         }
)
splinePosition <- as.data.frame(t(splinePosition))
colnames(splinePosition) <- c('TIME', 'POSITION')

splineR <- sapply(0:(length(df$TIME) / step),
                  function(x) {
                    center <- x * step + step2 / 2 + 1 + centerShift
                    start <- center - step2 / 4
                    end <- center + step2 / 4
                    interval <- (start):(end)
                    R1 <- mean(df$R1[interval])
                    R2 <- mean(df$R2[interval])
                    c(df$TIME[center], R1, R2)
                  }
)
splineR <- as.data.frame(t(splineR))
colnames(splineR) <- c('TIME', 'R1', 'R2')
splineR1Line <- spline(splineR$TIME, splineR$R1, xout = df$TIME)
splineR2Line <- spline(splineR$TIME, splineR$R2, xout = df$TIME)
par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', col = 'red', lwd = 2, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), yLab1))
lines(splineR$TIME, splineR$R1, type = 'b', lty = 'blank', col = 'black')
lines(splineR1Line, col = "black")

plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', col = 'orange', lwd = 2, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), yLab2))
lines(splineR$TIME, splineR$R2, type = 'b', lty = 'blank', col = 'black')
lines(splineR2Line, col = "black")
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', col = 'blue', lwd = 2, ylab = 'POSITION, mm')
lines(splinePosition$TIME, splinePosition$POSITION, type = 'b', lty = 'blank', col = 'black')

paste("h = ", df$POSITION[1], " mm")

pulseR1 <- df$R1 - splineR1Line$y
pulseR2 <- df$R2 - splineR2Line$y
pointsR <- sapply(0:(length(df$TIME) / (step / 2)),
                  function(x) {
                    center <- x * step / 2 + step2 / 2 + 1 + centerShift
                    start <- center - step2 / 4
                    end <- center + step2 / 4
                    interval <- (start):(end)
                    R1 <- mean(pulseR1[interval])
                    R2 <- mean(pulseR2[interval])
                    c(df$TIME[center], df$POSITION[center], R1, R2)
                  }
)
pointsR <- as.data.frame(t(pointsR))
colnames(pointsR) <- c('TIME', 'POSITION', 'R1', 'R2')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, pulseR1, type = 'l', xlab = 'Time, s', col = 'red', lwd = 2, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), yLab1))
lines(pointsR$TIME, pointsR$R1, type = 'b', lty = 'blank', col = 'black')
plot(df$TIME, pulseR2, type = 'l', xlab = 'Time, s', col = 'orange', lwd = 2, ylab = substitute(bold(R[s ~ x ~ L ~ mm] ~ ~Omega), yLab2))
lines(pointsR$TIME, pointsR$R2, type = 'b', lty = 'blank', col = 'black')
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', col = 'blue', lwd = 2, ylab = 'POSITION, mm')
lines(pointsR$TIME, pointsR$POSITION, type = 'b', lty = 'blank', col = 'black')

pointsDiffR <- sapply(1:((length(pointsR$TIME) - 2) / 2),
                      function(x) {
                        POSITION <- pointsR$POSITION[2 * x] - pointsR$POSITION[2 * x - 1]
                        dR1 <- pointsR$R1[2 * x] - pointsR$R1[2 * x - 1]
                        dR2 <- pointsR$R2[2 * x] - pointsR$R2[2 * x - 1]
                        c(pointsR$TIME[2 * x - 1], POSITION, dR1, dR2)
                      }
)
pointsDiffR <- as.data.frame(t(pointsDiffR))
if (fixOhms) {
  colnames(pointsDiffR) <- c('TIME', 'POSITION', 'dR3', 'dR4')
} else {
  colnames(pointsDiffR) <- c('TIME', 'POSITION', 'dR1', 'dR2')
}

median(splineR1Line$y)
median(splineR2Line$y)
aggregate(pointsDiffR, by = list(pointsDiffR$POSITION), FUN = mean)