aper <- read.csv(list.files(pattern = "aper.csv$"))
tail(aper)
pureLogic <- read.csv(list.files(pattern = "PureLogic.csv$"))
tail(pureLogic)

df <- merge(aper, pureLogic)
df <- data.frame(df$TIME, df$POSITION, df$R1, df$R2, df$CCR)
colnames(df) <- c('TIME', 'POSITION', 'R1', 'R2', 'CCR')


angle <- read.csv(list.files(pattern = "angle.csv$"))
angle <- as.data.frame(spline(angle$TIME, angle$ANGLE, xout = df$TIME))
colnames(angle) <- c('TIME', 'ANGLE')
tail(angle)
df <- merge(df, angle)
df <- data.frame(df$TIME, df$POSITION, df$R1, df$R2, df$CCR, df$ANGLE)
colnames(df) <- c('TIME', 'POSITION', 'R1', 'R2', 'CCR', 'ANGLE')

if (length(ls(pattern = "interval")) != 0) {
  df <- df[interval,]
}
