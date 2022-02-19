aper <- read.csv('2022-02-03 13-21-41 869 aper.csv')
plog <- read.csv('2022-02-03 13-21-41 984 PureLogic.csv')
bstand <- read.csv('2022-02-03 13-21-41 924 Briko-Stand.csv')
df <- merge(aper, merge(plog, bstand))

out_file <- na.omit(data.frame(df$TIME, df$R1, df$R2, df$POSITION, df$HX1))
colnames(out_file) <- c('TIME', 'R1', 'R2', 'POSITION', 'FORCE')
write.table(out_file, file = 'out.csv', row.names = FALSE, dec = ',')

# library('ggplot2')
# library('grid')
# pvp <- viewport(layout = grid.layout(4, 1))
# pushViewport(pvp)
#
# print(ggplot(data = df) + geom_line(mapping = aes(x = TIME, y = R1)), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(ggplot(data = df) + geom_line(mapping = aes(x = TIME, y = R2)), vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(ggplot(data = df) + geom_line(mapping = aes(x = TIME, y = POSITION)), vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
# print(ggplot(data = df) + geom_line(mapping = aes(x = TIME, y = HX1)), vp = viewport(layout.pos.row = 4, layout.pos.col = 1))