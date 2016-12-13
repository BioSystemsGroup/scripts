#! /usr/bin/Rscript
####! /bin/bash setR
argv <- commandArgs(TRUE)

#dev.off()

usage <- function() {
    print("Usage: rxn-dPV-2.r dPVMin dPVMax <exp name>")
    print("  e.g. rxn-dPV-2.r 1 5 exp000x")
    print("  The current directory should contain files named like this:")
    print("    exp000x_rxnProduct_dPV∈[1,5]-1_2-0000-sum.csv and ")
    print("    exp000x_rxnProduct_dPV∈[1,5]-divisors.csv")
    print("  where the \"-divisors\" file contains the column numbers to use to divide into the sums to get the averages.")
    quit()
}
if (length(argv) < 3) usage()

dPVMin <- as.numeric(argv[1])
dPVMax <- as.numeric(argv[2])

# get the file names
fileRoot <- paste(argv[3],"_rxnProduct_dPV∈\\[",dPVMin,",",dPVMax,"]",sep="")
files <- list.files(pattern=fileRoot)
sumfiles <- files[grep("-sum.csv",files)]
divfile <- files[grep("-divisors.csv",files)]

# extract and sum each rxn product from each data set and divide by the sum of the column numbers
# i.e. (∑rxnᵢ)/∑colsⱼ, where i ∈ {NecInhib, S, G, Marker, ...} and j ∈ {1_2-????, 3-????}

for (sumfile in sumfiles) {
    fileDat <- read.csv(sumfile)
    if (!exists("dat")) dat <- fileDat[1]
    dat <- cbind(dat,fileDat[2:ncol(fileDat)])
}

rxnnames <- unique(colnames(dat))
rxnnames <- rxnnames[2:length(rxnnames)]

# sum the sums per unique reaction product
for (rxn in rxnnames) {
    rxnDat <- dat[,grep(paste(rxn,"$",sep=""),names(dat))]
    if (!exists("rxnSum"))
        rxnSum <- rowSums(rxnDat)
    else
        rxnSum <- cbind(rxnSum,rowSums(rxnDat))
}
colnames(rxnSum) <- rxnnames

# now read in the total number of columns
divs <- read.csv(divfile)
totalcells <- sum(divs[2])/length(rxnnames)  # number of columns divided by number of rxn products

# now divide in the total columns (number of cells) to get the avg per cell
rxnAvg <- cbind(dat[1], rxnSum/totalcells)

avgname <- sub("-divisors","-totals",divfile)
write.csv(rxnAvg, avgname, row.names=FALSE)

q()
