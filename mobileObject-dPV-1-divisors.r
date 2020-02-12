#! /bin/bash setR
####! /usr/bin/Rscript
#argv <- commandArgs(TRUE)

dev.off()

usage <- function() {
    print("Usage: mobileObject-dPV-1.r dPVMin dPVMax <exp directories>")
    print("  directories should contain files named mobileObject_zone_1_2-[0-9]+.csv")
    quit()
}
if (length(argv) < 3) usage()

# for the color space max and min
minmean <-  9e10
maxmean <- -9e10

dPVMin <- as.numeric(argv[1])
dPVMax <- as.numeric(argv[2])

fileRoot <- "mobileObject_zone_"

# for each experiment
for (expDir in argv[3:length(argv)]) {
    files <- list.files(path = expDir, pattern=fileRoot)
    cellNum <- data.frame(character(),numeric(), stringsAsFactors=FALSE) # num columns per file so that averages can be calculated in the next round
    cellNum <- cellNum[2,]
    fileNdx <- 1 # needed to build cellNum as a data.frame with stringsAsFactors=FALSE (can't use rbind())
    for (file in files) {
        print(paste("Processing",file))
        datasetname <- sub(".csv","",sub("mobileObject_zone_","",file))
        fileDat <- read.csv(paste(expDir,file,sep="/"), nrows=1) # only read in the first line
        # extract the relevant data from this file and cbind it
        nPVNdx <- 1
        nPVcolumns <- vector() # list of columns where dPV ∈ [dPVMin, dPVMax)
        for (cname in colnames(fileDat[2:ncol(fileDat)])) {
            dPV <- unlist(strsplit(cname,"[.]"))[4] # dPV is the 4th thing in the column name
            dPV <- as.numeric(dPV)
            if ((dPVMin <= dPV) && (dPV < dPVMax)) {
                nPVcolumns <- append(nPVcolumns, nPVNdx+1)  # Add 1 to skip the first column
            }
            nPVNdx <- nPVNdx + 1
        }

        # if there are no columns ∈ [dPVMin,dPVMax] go to next file
        if (length(nPVcolumns) <= 0) next
        
        cellNum[fileNdx,] <- c(datasetname,length(nPVcolumns)) # divisor for Sums to get averages
        fileNdx <- fileNdx + 1

        rm(fileDat)
        gc()
    }
    colnames(cellNum) <- c("trialfile", "#columns")
    write.csv(cellNum, paste(expDir,"_mobileObject_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-divisors.csv", sep=""), row.names=FALSE)
    
}

#q()
