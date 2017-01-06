#! /bin/bash setR
####! /usr/bin/Rscript
#argv <- commandArgs(TRUE)

dev.off()

usage <- function() {
    print("Usage: rxn-dPV-1.r dPVMin dPVMax <exp directories>")
    print("  directories should contain files named rxnProduct_zone_1_2-[0-9]+.csv")
    quit()
}
if (length(argv) < 3) usage()

# for the color space max and min
minmean <-  9e10
maxmean <- -9e10

dPVMin <- as.numeric(argv[1])
dPVMax <- as.numeric(argv[2])

fileRoot <- "rxnProduct_zone_"

highest.dPV <- -1

# for each experiment
for (expDir in argv[3:length(argv)]) {
    files <- list.files(path = expDir, pattern=fileRoot)
    cellNum <- data.frame(character(),numeric(), stringsAsFactors=FALSE) # num columns per file so that averages can be calculated in the next round
    cellNum <- cellNum[2,]
    fileNdx <- 1 # needed to build cellNum as a data.frame with stringsAsFactors=FALSE (can't use rbind())
    for (file in files) {
        print(paste("Processing",file))
        datasetname <- sub(".csv","",sub("rxnProduct_zone_","",file))
        dat <- data.frame()
        fileDat <- read.csv(paste(expDir,file,sep="/"))
        # extract the relevant data from this file and cbind it
        nPVNdx <- 1
        nPVcolumns <- vector() # list of columns where dPV ∈ [dPVMin, dPVMax)
        for (cname in colnames(fileDat[2:ncol(fileDat)])) {
            dPV <- unlist(strsplit(cname,"[.]"))[4] # dPV is the 4th thing in the column name
            dPV <- as.numeric(dPV)
            highest.dPV <- max(highest.dPV, dPV)
            if ((dPVMin <= dPV) && (dPV < dPVMax)) {
                nPVcolumns <- append(nPVcolumns, nPVNdx+1)  # Add 1 to skip the first column
            }
            nPVNdx <- nPVNdx + 1
        }

        # if there are no columns ∈ [dPVMin,dPVMax] go to next file
        if (length(nPVcolumns) <= 0) next
        
        # use time column as initial data.frame
        if (nrow(dat) <= 0) dat <- fileDat[1]

        # use the nPVcolumns to slice out the right columns
        dat <- cbind(dat, fileDat[nPVcolumns])
#stop()
        rm(fileDat)  # attempt to keep a small memory footprint
        
        # parse out rxn products
        rxnnames <- list()
        for (cname in colnames(dat[2:ncol(dat)])) {
            rxnnames[cname] <- unlist(strsplit(cname,"[.]"))[5] # rxnprod name is the 5th element
        }
        rxnnames <- unique(rxnnames)

        rxnSum <- dat[1] # start with Time column
        for (rxnName in rxnnames) {
            rxnDat <- dat[,grep(paste(".",rxnName,"$",sep=""),names(dat))]
            # finally get the means for each time
            if (is.vector(rxnDat)) rxnDat <- as.data.frame(rxnDat) # defensive
            rxnSum <- cbind(rxnSum, rowSums(rxnDat))
        }
        colnames(rxnSum) <- c("Time",rxnnames)
        write.csv(rxnSum,paste(expDir, "_rxnProduct_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-", datasetname, "-sum.csv", sep=""), row.names=FALSE)
        cellNum[fileNdx,] <- c(datasetname,length(nPVcolumns)) # divisor for Sums to get averages
        fileNdx <- fileNdx + 1

        rm(dat)  # attempt to keep small memory footprint
        gc()
    }
    colnames(cellNum) <- c("trialfile", "#columns")
    write.csv(cellNum, paste(expDir,"_rxnProduct_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-divisors.csv", sep=""), row.names=FALSE)
    
}
print(paste("highest dPV found → ",highest.dPV), sep="")
q()
