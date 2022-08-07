pureLogic <- read.csv(list.files(pattern = "PureLogic.csv$"))
tail(pureLogic)

brikoStand <- read.csv(list.files(pattern = "Stand.csv$"))
tail(brikoStand)

length(pureLogic$TIME) - length(brikoStand$TIME)

pureLogicMillis <- 0
pureLogic2 <- sapply(1:(length(pureLogic$TIME)),
                     function(x) {
                       c(x + pureLogicMillis, pureLogic$POSITION[x])
                     }
)
pureLogic2 <- as.data.frame(t(pureLogic2))
colnames(pureLogic2) <- c('TIME', 'POSITION')

brikoStandMillis <- 11783 - 11777
brikoStand2 <- sapply(1:(length(brikoStand$TIME)),
                     function(x) {
                       c(x + brikoStandMillis, brikoStand$C[x])
                     }
)
brikoStand2 <- as.data.frame(t(brikoStand2))
colnames(brikoStand2) <- c('TIME', 'PRESSURE')

df <- merge(pureLogic2, brikoStand2)
df2 <- sapply(1:(length(df$TIME)),
                      function(x) {
                        c((df$TIME[x] - df$TIME[1]) / 1000, df$POSITION[x], df$PRESSURE[x])
                      }
)
df <- as.data.frame(t(df2))
colnames(df) <- c('TIME', 'POSITION', 'PRESSURE')

par(mfrow = c(2, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', col = 'red', lwd = 2)
plot(df$TIME, df$PRESSURE, type = 'l', xlab = 'Time, s', col = 'blue', lwd = 2)

write.csv(df, file = 'out.csv', row.names = FALSE)
