if (length(ls()) == 0) {
  source("main-readFiles.R")
}

write.csv(out, file = outFile, row.names = FALSE)