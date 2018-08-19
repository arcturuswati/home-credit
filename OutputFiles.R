wd.path <- "/Users/Swati/Documents/KaggleCompetitions/HomeCreditDefaultRisk/in/"
setwd(wd.path)
output.dir <- "/Users/Swati/Documents/KaggleCompetitions/HomeCreditDefaultRisk/out/"

filenames <- dir(output.dir, pattern=".csv", all.files=FALSE, full.names=FALSE)
filePrefix <- 'file' # change this to whatever is the file name

out.file <- list()
for(i in 1:length(filenames)) {
  readFile <- read.csv(paste0(output.dir, filenames[i]))
  readFile$Rank <- rank(readFile$TARGET)
  names(readFile)[names(readFile) == "TARGET"] <- strsplit(filenames[i], ".csv")[[1]]
  names(readFile)[names(readFile) == "Rank"] <- paste0("Rank", strsplit(strsplit(filenames[i], ".csv")[[1]], filePrefix)[[1]][2])
  out.file[[i]] <- readFile
  if(i == 1) combinedFile <- readFile
  else combinedFile <- combinedFile %>% left_join(readFile)
}

output.vars <- names(combinedFile)[names(combinedFile) %like% filePrefix]
spearmanCorr <- cor(combinedFile[names(combinedFile) %in% output.vars], method = "spearman")
row.names(combinedFile) <- combinedFile$SK_ID_CURR
averageRank <- combinedFile[,!(names(combinedFile) %in% c("SK_ID_CURR", output.vars))] 
averageRank$aveRank <- apply(averageRank, MARGIN = 1, mean)
averageRank$SK_ID_CURR <- as.integer(row.names(averageRank))
averageProb <- combinedFile[,names(combinedFile) %in% output.vars]
averageProb$aveProb <- apply(averageProb, MARGIN = 1, mean)
averageProb$SK_ID_CURR <- as.integer(row.names(averageProb))

combinedFile <- combinedFile %>% 
  left_join(averageRank %>% select(SK_ID_CURR, aveRank)) %>% 
  left_join(averageProb %>% select(SK_ID_CURR, aveProb)) %>% 
  select(SK_ID_CURR, aveRank, aveProb)

