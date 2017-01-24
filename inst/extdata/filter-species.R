# Simple program that reads in a dataset, modifies it and writes it out.
specInfo <- read.csv("sample-data.csv")
specFiltered <- subset(data, nutr >= "5")
outFile <- tempfile(fileext=".csv")
write.csv(specFiltered, file=outFile, row.names=FALSE)

