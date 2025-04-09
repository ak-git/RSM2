aper <- read.csv(list.files(pattern = "aper.csv$"))
tail(aper)
pureLogic <- read.csv(list.files(pattern = "PureLogicF5_0.csv$"))
tail(pureLogic)

df <- merge(aper, pureLogic)
df <- data.frame(df$TIME, df$POSITION, df$R1, df$R2, df$CCR)
colnames(df) <- c('TIME', 'POSITION', 'R1', 'R2', 'CCR')

if (length(ls(pattern = "interval")) != 0) {
  df <- df[interval,]
}
