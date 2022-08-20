pureLogic <- read.csv(list.files(pattern = "PureLogic.csv$"))
tail(pureLogic)
aper <- read.csv(list.files(pattern = "aper.csv$"))
tail(aper)

pureLogicMillis <- 0
pureLogic2 <- sapply(1:(length(pureLogic$TIME)),
                     function(x) {
                       c(x + pureLogicMillis, pureLogic$POSITION[x])
                     }
)
pureLogic2 <- as.data.frame(t(pureLogic2))
colnames(pureLogic2) <- c('TIME', 'POSITION')

aperMillis <- 0
aper2 <- sapply(1:(length(aper$TIME)),
                function(x) {
                  c(x + aperMillis, aper$R1[x], aper$MYO1[x], aper$R2[x], aper$MYO2[x], aper$CCR[x])
                }
)
aper2 <- as.data.frame(t(aper2))
colnames(aper2) <- c('TIME', 'R1', 'MYO1', 'R2', 'MYO2', 'CCR')

df <- merge(pureLogic2, aper2)
df2 <- sapply(1:(length(df$TIME)),
              function(x) {
                c((df$TIME[x] - df$TIME[1]) / 1000, df$POSITION[x], df$R1[x], df$MYO1[x], df$R2[x], df$MYO2[x], df$CCR[x])
              }
)
df <- as.data.frame(t(df2))
tail(df)
# interval <- (150 * 1000 + 1):(170 * 1000)
# df <- df[interval,]
colnames(df) <- c('TIME', 'POSITION', 'R1', 'MYO1', 'R2', 'MYO2', 'CCR')

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'Position, mm', col = 'red', lwd = 2)
plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', ylab = 'R, Ohms', col = 'orange', lwd = 2,
     ylim = c(min(df$R1, df$R2), min(250, max(df$R1, df$R2))))
lines(df$TIME, df$R2, type = 'l', xlab = 'Time, s', ylab = 'R2, Ohms', col = 'green', lwd = 2)
plot(df$TIME, df$MYO1, type = 'l', xlab = 'Time, s', ylab = 'EMG1', col = 'black', lwd = 0.5, ylim = c(-0.1, 0.1))
# plot(df$TIME, df$MYO2, type = 'l', xlab = 'Time, s', ylab = 'EMG2', col = 'black', lwd = 1, ylim = c(-0.5, 0.5))
# plot(df$TIME, df$CCR, type = 'l', xlab = 'Time, s', ylab = 'CCR, Ohms', col = 'black', lwd = 2)

write.csv(df, file = 'out.csv', row.names = FALSE)
