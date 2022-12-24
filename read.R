aper <- read.csv(list.files(pattern = "aper.csv$"))
tail(aper)
pureLogic <- read.csv(list.files(pattern = "PureLogic.csv$"))
tail(pureLogic)

df <- merge(aper, pureLogic)[interval,]
df <- data.frame(df$TIME, df$R1, df$R2, df$POSITION)
colnames(df) <- c('TIME', 'R1', 'R2', 'POSITION')