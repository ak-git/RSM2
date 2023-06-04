aper <- read.csv(list.files(pattern = "aper.csv$"))
tail(aper)
pureLogic <- read.csv(list.files(pattern = "PureLogic.csv$"))
tail(pureLogic)

if (length(ls(pattern = "interval")) == 0) {
  df <- merge(aper, pureLogic)
} else {
  df <- merge(aper, pureLogic)[interval,]
}

df <- data.frame(df$TIME, df$POSITION, df$R1, df$R2, df$CCR)
colnames(df) <- c('TIME', 'POSITION', 'R1', 'R2', 'CCR')