library(dplyr)
fileName <- "2021-05-12 19-03-11 7 mm inverse.csv"
data <- read.csv(fileName)

for (t in seq(from = min(data$TIME), to = max(data$TIME), by = 0.5)) {
  if (!(t %in% data$TIME)) {
    data <- rbind(data, c(t, NA, NA, NA, NA, NA, NA))
  }
}

data <- arrange(data, data$TIME)
write.csv(data, file = fileName, row.names = FALSE)