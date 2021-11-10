# max - 385 sec
outFile <- "2021-10-25 17-23-43.csv"
interval <- (220 * 1000 + 1):(292 * 1000)
aper <- read.csv("2021-10-25 17-23-43 246 aper.csv")[interval,]
plog <- read.csv("2021-10-25 17-23-43 256 PureLogic.csv")[interval,]
df <- merge(aper, plog)

step <- 500
out <- sapply(1:(length(df$TIME) / step - 1),
              function(x) {
                center <- x * step - step / 2 + 1
                dhmm <- df$POSITION[center + step] - df$POSITION[center]

                interval <- (x * step - 4 * step / 10 + 1):(x * step - step / 10 + 1)
                intervalNext <- interval + step

                aRhoS1 <- mean(df$APPARENT_06_18_RHO[interval])
                aRhoS1_ <- mean(df$APPARENT_06_18_RHO[intervalNext])
                aRhoS2 <- mean(df$APPARENT_30_18_RHO[interval])
                aRhoS2_ <- mean(df$APPARENT_30_18_RHO[intervalNext])

                c(df$TIME[center], df$POSITION[center],
                  mean(c(aRhoS1, aRhoS1_)), (aRhoS1_ - aRhoS1) / (dhmm / 18.0),
                  mean(c(aRhoS2, aRhoS2_)), (aRhoS2_ - aRhoS2) / (dhmm / 18.0)
                )
              }
)

out <- as.data.frame(t(out))
out <- out[seq_len(nrow(out)) %% 2 == 1,]
colnames(out) <- c('TIME', 'POSITION', 'A_RHO_S1', 'dA_RHO_S1', 'A_RHO_S2', 'dA_RHO_S2')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(df$TIME, df$APPARENT_06_18_RHO, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 6 x 18 mm")))
lines(out$TIME, out$A_RHO_S1, type = 'b', lty = 'blank', col = 'red')

plot(df$TIME, df$APPARENT_30_18_RHO, type = 'l', xlab = 'Time, s', ylab = expression(bold(rho ~ " 30 x 18 mm")))
lines(out$TIME, out$A_RHO_S2, type = 'b', lty = 'blank', col = 'orange')

plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')
lines(out$TIME, out$POSITION, type = 'b', lty = 'blank', col = 'blue')