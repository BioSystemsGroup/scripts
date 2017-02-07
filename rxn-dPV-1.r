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

source("~/R/misc.r")

# for the color space max and min
minmean <-  9e10
maxmean <- -9e10

dPVMin <- as.numeric(argv[1])
dPVMax <- as.numeric(argv[2])

###
##  Declare a couple of globals
###
inFileRoot <- "rxnProduct_zone_"
outFileRoot <- "dPV.rxn/"
if (!file.exists(outFileRoot)) dir.create(outFileRoot)

exps <- argv[3:length(argv)]
highest.dPV <- maxdPV(exps)

band <- c(dPVMin, dPVMax)
snd(band, exps)

q()

