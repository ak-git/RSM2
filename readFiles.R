outFile <- "2021-10-25 17-23-43.csv"
aper <- read.csv("2021-10-25 17-23-43 246 aper.csv");
plog <- read.csv("2021-10-25 17-23-43 256 PureLogic.csv")

df <- merge(aper, plog)[(220 * 1000 + 1):(292 * 1000), ]

step <- 500
out <- sapply(1:(length(df$TIME) / step - 1), function(x) {
  center <- x * step - step / 2 + 1
  interval <- (x * step - 4 * step / 10 + 1):(x * step - step / 10 + 1)
  intervalNext <- interval + step
  c(df$TIME[center],
    mean(df$R1[interval]), mean(df$R1[intervalNext]),
    mean(df$R2[interval]), mean(df$R2[intervalNext]),
    df$POSITION[center],
    df$POSITION[center + step] - df$POSITION[center])
})

out <- as.data.frame(t(out))
colnames(out) <- c('TIME', 'R1', 'R1`', 'R2', 'R2`', 'POSITION', 'dH')