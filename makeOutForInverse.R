if (!is.list(df)) {
  source("main-readFiles.R")
}

par(mfrow = c(3, 1), mar = c(2, 5, 2, 1), cex = 1.0, family = 'mono', las = 1, tck = 1)

plot(df$TIME, df$R1, type = 'l', xlab = 'Time, s', ylab = 'R1')
lines(out$TIME, out$R1, type = 'b', lty = 'blank', col = 'red')

plot(df$TIME, df$R2, type = 'l', xlab = 'Time, s', ylab = 'R2')
lines(out$TIME, out$R2, type = 'b', lty = 'blank', col = 'orange')

plot(df$TIME, df$POSITION, type = 'l', xlab = 'Time, s', ylab = 'POSITION')
lines(out$TIME, out$POSITION, type = 'b', lty = 'blank', col = 'blue')

write.csv(out, file = outFile, row.names = FALSE)