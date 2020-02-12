#! /bin/bash setR
####! /usr/bin/Rscript
#argv <- commandArgs(TRUE)

dev.off()

usage <- function() {
    print("Usage: countHbyZone.r zone <exp directories>")
    print("  directories should contain files named mobileObject_zone_1_2-[0-9]+.csv")
    print("  zone = {0,1,2}")
    print("  Counts the number of Hepatocytes where dPVMin <= dPV <= dPVMax.")
    quit()
}
if (length(argv) < 2) usage()

## for the color space max and min
minmean <-  9e10
maxmean <- -9e10

zone <- as.numeric(argv[1])
tailName <- ifelse (zone < 2, "1_2", "3")

fileRoot <- "mobileObject_zone_"

## for each experiment
for (expDir in argv[2:length(argv)]) {
  files <- list.files(path = expDir, pattern=paste(fileRoot,tailName,sep=""))
  baseNames <- vector()
  cellNum <- data.frame(character(),numeric(), stringsAsFactors=FALSE) # num columns per file so that averages can be calculated in the next round
  cellNum <- cellNum[2,]
  fileNdx <- 1 # needed to build cellNum as a data.frame with stringsAsFactors=FALSE (can't use rbind())
  for (file in files) {
    print(paste("Processing",file))
    datasetname <- sub(".csv","",sub(fileRoot,"",file))
    fileDat <- read.csv(paste(expDir,file,sep="/"), nrows=1) # only read in the first line
    ## extract the dPV data from this file and cbind it
    nZNdx <- 1
    nPVcolumns <- vector() # list of columns where dPV ∈ [dPVMin, dPVMax]
    for (cname in colnames(fileDat[2:ncol(fileDat)])) {
      splitCName <- unlist(strsplit(cname,"[.]"))
      columnZone <- as.numeric(sub("X","",splitCName[1]))
      if (zone == columnZone) {
        nPVcolumns <- append(nPVcolumns, nZNdx+1)  # Add 1 to skip the first column
      }
      ## append the base name for this column
      baseNames <- append(baseNames,splitCName[5])
      nZNdx <- nZNdx + 1
    }
    totalCount <- length(nPVcolumns) # count all the columns ∈[dPVMin,dPVMax]
    ## if there are no columns ∈ [dPVMin,dPVMax] go to next file
    if (length(nPVcolumns) <= 0) next
    uniqueNames <- unique(baseNames)
    uniqueCount <- totalCount/length(uniqueNames)
    cellNum[fileNdx,] <- c(datasetname,uniqueCount)
    fileNdx <- fileNdx + 1

    rm(fileDat)
    gc()
  }
  countDat <- as.numeric(cellNum[,2])
  print(paste("zone ",zone,": #H_μ = ", as.character(mean(countDat)),
              ", #H_min = ", as.character(min(countDat)),
              ", #H_max = ", as.character(max(countDat))), sep="")
  colnames(cellNum) <- c("Trialfile", "#Hepatocytes")
  write.csv(cellNum, paste(expDir,"_#Hepatocytes_zone_",zone,".csv", sep=""), row.names=FALSE)

}
q()
