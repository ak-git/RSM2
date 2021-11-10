outFile <- "2021-10-25 17-23-43.csv"
aper <- read.csv("2021-10-25 17-23-43 246 aper.csv");
plog <- read.csv("2021-10-25 17-23-43 256 PureLogic.csv")

df <- merge(aper, plog)[(220 * 1000 + 1):(292 * 1000),]

step <- 500
out <- sapply(1:(length(df$TIME) / step - 1),
              function(x) {
                center <- x * step - step / 2 + 1
                dhmm <- df$POSITION[center + step] - df$POSITION[center]

                interval <- (x * step - 4 * step / 10 + 1):(x * step - step / 10 + 1)
                intervalNext <- interval + step

                mR1 <- mean(df$APPARENT_06_18_RHO[interval])
                mR1_ <- mean(df$APPARENT_06_18_RHO[intervalNext])
                mR2 <- mean(df$APPARENT_30_18_RHO[interval])
                mR2_ <- mean(df$APPARENT_30_18_RHO[intervalNext])

                c(df$TIME[center],
                  mR1, (mR1_ - mR1) / (dhmm / 18.0),
                  mR2, (mR2_ - mR2) / (dhmm / 18.0),
                  df$POSITION[center]
                )
              }
)

out <- as.data.frame(t(out))
colnames(out) <- c('TIME', 'A_RHO_1', 'dA_RHO_1', 'A_RHO_2', 'dA_RHO_2', 'POSITION')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(df$TIME, df$APPARENT_06_18_RHO, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[a] ~ " 6 x 18 mm")))
lines(out$TIME, out$A_RHO_1, type = 'b', lty = 'blank', col = 'red')

plot(df$TIME, df$APPARENT_30_18_RHO, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho[a] ~ " 30 x 18 mm")))
lines(out$TIME, out$A_RHO_2, type = 'b', lty = 'blank', col = 'orange')

plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')
lines(out$TIME, out$POSITION, type = 'b', lty = 'blank', col = 'blue')