pureLogic <- read.csv(list.files(pattern = "PureLogic.csv$"))
tail(pureLogic)
aper <- read.csv(list.files(pattern = "aper.csv$"))
tail(aper)
brikoStand <- read.csv(list.files(pattern = "Stand.csv$"))
tail(brikoStand)

pureLogicMillis <- 0
pureLogic2 <- sapply(1:(length(pureLogic$TIME)),
                     function(x) {
                       c(x + pureLogicMillis, pureLogic$POSITION[x])
                     }
)
pureLogic2 <- as.data.frame(t(pureLogic2))
colnames(pureLogic2) <- c('TIME', 'POSITION')

aperMillis <- 50501 - 33450
aper2 <- sapply(1:(length(aper$TIME)),
                function(x) {
                  c(x + aperMillis, aper$R1[x], aper$R2[x], aper$CCR[x])
                }
)
aper2 <- as.data.frame(t(aper2))
colnames(aper2) <- c('TIME', 'R1', 'R2', 'CCR')

brikoStandMillis <- 65508 - 33450
brikoStand2 <- sapply(1:(length(brikoStand$TIME)),
                      function(x) {
                        c(x + brikoStandMillis, brikoStand$C[x])
                      }
)
brikoStand2 <- as.data.frame(t(brikoStand2))
colnames(brikoStand2) <- c('TIME', 'PRESSURE')

df <- merge(merge(pureLogic2, aper2), brikoStand2)
df2 <- sapply(1:(length(df$TIME)),
              function(x) {
                c((df$TIME[x] - df$TIME[1]) / 1000, df$POSITION[x], df$R1[x], df$R2[x], df$CCR[x], df$PRESSURE[x])
              }
)
df <- as.data.frame(t(df2))
colnames(df) <- c('TIME', 'POSITION', 'R1', 'R2', 'CCR', 'PRESSURE')

par(mfrow = c(4, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'Position, mm', col = 'red', lwd = 2)
plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', ylab = 'Ohms', col = 'green', lwd = 2)
lines(df$TIME, df$R2, type = 'l', xlab = 'Time, s', col = 'orange', lwd = 2)
plot(df$TIME, df$CCR, type = 'l', xlab = 'Time, s', ylab = 'CCR, Ohms', col = 'black', lwd = 2)
plot(df$TIME, df$PRESSURE, type = 'l', xlab = 'Time, s', col = 'blue', lwd = 2)

write.csv(df, file = 'out.csv', row.names = FALSE)
