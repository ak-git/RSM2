# max - 514 sec
outFile <- "2021-10-14 15-09-28 7 mm.csv"
interval <- (0 * 1000 + 1):(514 * 1000)
aper <- read.csv("2021-10-14 15-09-28 992 aper.csv")[interval,]
plog <- read.csv("2021-10-14 15-09-29 011 PureLogic.csv")[interval,]
df <- merge(aper, plog)

step <- 500
out <- sapply(1:(length(df$TIME) / step - 1),
              function(x) {
                center <- x * step - step / 2 + 1
                dhmm <- df$POSITION[center + step] - df$POSITION[center]

                interval <- (x * step - 4 * step / 10 + 1):(x * step - step / 10 + 1)
                intervalNext <- interval + step

                rS1 <- mean(df$R1[interval])
                rS1_ <- mean(df$R1[intervalNext])
                rS2 <- mean(df$R2[interval])
                rS2_ <- mean(df$R2[intervalNext])

                aRhoS1 <- mean(df$APPARENT_07_21_RHO[interval])
                aRhoS1_ <- mean(df$APPARENT_07_21_RHO[intervalNext])
                aRhoS2 <- mean(df$APPARENT_35_21_RHO[interval])
                aRhoS2_ <- mean(df$APPARENT_35_21_RHO[intervalNext])

                c(df$TIME[center], df$POSITION[center],
                  rS1, rS1_, rS2, rS2_, dhmm,
                  mean(c(aRhoS1, aRhoS1_)), (aRhoS1_ - aRhoS1) / (dhmm / 21.0),
                  mean(c(aRhoS2, aRhoS2_)), (aRhoS2_ - aRhoS2) / (dhmm / 21.0)
                )
              }
)

out <- as.data.frame(t(out))
colnames(out) <- c('TIME', 'POSITION', 'R1', 'R1`', 'R2', 'R2`', 'dh', 'RHO_S1', 'RHO_S1_DIFF', 'RHO_S2', 'RHO_S2_DIFF')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', ylab = expression(bold(R ~ " 7 x 21 mm")))
lines(out$TIME, out$R1, type = 'b', lty = 'blank', col = 'red')

plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', ylab = expression(bold(R ~ " 35 x 21 mm")))
lines(out$TIME, out$R2, type = 'b', lty = 'blank', col = 'orange')

plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')
lines(out$TIME, out$POSITION, type = 'b', lty = 'blank', col = 'blue')


par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$APPARENT_07_21_RHO, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 7 x 21 mm")))
lines(out$TIME, out$RHO_S1, type = 'b', lty = 'blank', col = 'red')

plot(df$TIME, df$APPARENT_35_21_RHO, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 35 x 21 mm")))
lines(out$TIME, out$RHO_S2, type = 'b', lty = 'blank', col = 'orange')

plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')
lines(out$TIME, out$POSITION, type = 'b', lty = 'blank', col = 'blue')