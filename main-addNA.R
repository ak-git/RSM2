library(dplyr)
fileName <- "2021-10-25 17-23-43 6 mm inverse.csv"
data <- read.csv(fileName)

for (t in seq(from = min(data$TIME), to = max(data$TIME), by = 0.5)) {
  if (!(t %in% data$TIME)) {
    data <- rbind(data, c(t, NA, NA, NA, NA, NA, NA))
  }
}

data <- arrange(data, data$TIME)
write.csv(data, file = fileName, row.names = FALSE)