library('scales')

mmBase <- 7

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
                  c(x + aperMillis, aper$R1[x], aper$R2[x], aper$CCR[x])
                }
)
aper2 <- as.data.frame(t(aper2))
colnames(aper2) <- c('TIME', 'R1', 'R2', 'CCR')

df <- merge(pureLogic2, aper2)
df2 <- sapply(1:(length(df$TIME)),
              function(x) {
                c((df$TIME[x] - df$TIME[1]) / 1000, df$POSITION[x], df$R1[x], df$R2[x], df$CCR[x])
              }
)
df <- as.data.frame(t(df2))
tail(df)
colnames(df) <- c('TIME', 'POSITION', 'R1', 'R2', 'CCR')

par(mfrow = c(3, 1), mar = c(3, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)
xlab <- 'Время, с'
col <- hue_pal()(3)
lwd <- 3
plot(df$TIME, df$R1, type = 'l', xlab = xlab, ylab = expression(Omega), col = col[1], lwd = lwd,
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase, L = mmBase * 3)))
plot(df$TIME, df$R2, type = 'l', xlab = xlab, ylab = expression(Omega), col = col[2], lwd = lwd,
     main = substitute(bold(R[s ~ x ~ L ~ mm]), list(s = mmBase * 5, L = mmBase * 3)))
plot(df$TIME, df$POSITION, type = 'l', xlab = xlab, ylab = 'mm', col = col[3], lwd = lwd,
     main = 'Положение электродной системы по вертикали')

write.csv(df, file = 'out-avg-selector.csv', row.names = FALSE)
