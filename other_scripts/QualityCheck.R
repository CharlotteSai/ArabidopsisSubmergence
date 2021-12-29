library(ngsReports)

# arrange input data dir
rawDir <- "1_FastQC/Raw"
trimmedDir <- "1_FastQC/Trimmed"
mapLogDir <- "1_FastQC/mappingQuality" 

rawData <- list.files(rawDir, pattern = "fastqc.zip$", full.names = TRUE) %>% 
  FastqcDataList()
trimmedData <- list.files(trimmedDir, pattern = "fastqc.zip$", full.names = TRUE) %>% 
  FastqcDataList()

# row data 
plotSummary(rawData)
plotSummary(trimmedData)

# GC content
plotGcContent(rawData, plotType = "line", theoreticalGC = FALSE) 
plotGcContent(trimmedData, plotType = "line", theoreticalGC = FALSE) 

# Sequence Duplication Levels
plotDupLevels(rawData)
plotDupLevels(trimmedData)

# STAR mapping
logfiles <- list.files(mapLogDir, pattern = ".out", full.names = TRUE)
starLog <- importNgsLogs(logfiles, type = "star") 
