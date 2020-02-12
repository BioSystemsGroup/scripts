#! /bin/bash setR
####! /usr/bin/Rscript
#argv <- commandArgs(TRUE)

dev.off()

usage <- function() {
    print("Usage: countHbydCV.r dCVMin dCVMax <exp directories>")
    print("  directories should contain files named mobileObject_zone_1_2-[0-9]+.csv")
    print("  Counts the number of Hepatocytes where dCVMin <= dCV <= dCVMax.")
    print("  Note, this requires the nectrig branch code changes, which output dCV to the mobileObject files.")
    quit()
}
if (length(argv) < 3) usage()

## for the color space max and min
minmean <-  9e10
maxmean <- -9e10

dCVMin <- as.numeric(argv[1])
dCVMax <- as.numeric(argv[2])

fileRoot <- "mobileObject_zone_"

## for each experiment
for (expDir in argv[3:length(argv)]) {
  files <- list.files(path = expDir, pattern=fileRoot)
  baseNames <- vector()
  cellNum <- data.frame(character(),numeric(), stringsAsFactors=FALSE) # num columns per file so that averages can be calculated in the next round
  cellNum <- cellNum[2,]
  fileNdx <- 1 # needed to build cellNum as a data.frame with stringsAsFactors=FALSE (can't use rbind())
  for (file in files) {
    print(paste("Processing",file))
    datasetname <- sub(".csv","",sub("mobileObject_zone_","",file))
    fileDat <- read.csv(paste(expDir,file,sep="/"), nrows=1) # only read in the first line
    ## extract the dCV data from this file and cbind it
    nCVNdx <- 1
    nCVcolumns <- vector() # list of columns where dCV ∈ [dCVMin, dCVMax]
    for (cname in colnames(fileDat[2:ncol(fileDat)])) {
      splitCName <- unlist(strsplit(cname,"[.]"))
      dCV <- splitCName[5] # dCV is the 5th thing in the column name
      dCV <- as.numeric(dCV)
      if ((dCVMin <= dCV) && (dCV <= dCVMax)) {
        nCVcolumns <- append(nCVcolumns, nCVNdx+1)  # Add 1 to skip the first column
      }
      ## append the base name for this column
      baseNames <- append(baseNames,splitCName[6])
      nCVNdx <- nCVNdx + 1
    }
    totalCount <- length(nCVcolumns) # count all the columns ∈[dCVMin,dCVMax]
    ## if there are no columns ∈ [dCVMin,dCVMax] go to next file
    if (length(nCVcolumns) <= 0) next
    uniqueNames <- unique(baseNames)
    uniqueCount <- totalCount/length(uniqueNames)
    cellNum[fileNdx,] <- c(datasetname,uniqueCount)
    fileNdx <- fileNdx + 1

    rm(fileDat)
    gc()
  }
  countDat <- as.numeric(cellNum[,2])
  print(paste("dCV∈[",as.character(dCVMin),",",as.character(dCVMax),"]: #H_μ = ", as.character(mean(countDat)),
              ", #H_min = ", as.character(min(countDat)),
              ", #H_max = ", as.character(max(countDat))), sep="")
  colnames(cellNum) <- c("Trialfile", "#Hepatocytes")
  write.csv(cellNum, paste(expDir,"_#Hepatocytes_dCV∈[", as.character(dCVMin), ",", as.character(dCVMax), "].csv", sep=""), row.names=FALSE)

}
q()
