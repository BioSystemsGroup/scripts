#! /usr/bin/Rscript
#! /bin/bash setR
##dev.off()
argv <- commandArgs(TRUE)

###
## Calculates the averages of the reaction product data.  Differs from
## rxn-dPV-inband.r in that you provide a band and it extracts the max
## dPV from the data files, where rxn-dPV-inband.r takes the min and
## max as parameters.
##
## Time-stamp: <2017-03-21 09:15:37 gepr>
###

usage <- function() {
    print("Usage: rxn-dPV.r <band width> <exp directories>")
    print("  directories should contain files named rxnProduct_zone_?-????.csv")
    print("  Extracts and averages all rxn data from the rxn files for the Hepatocytes")
    print("  within max(dPV)/band bands.")
    quit()
}

if (length(argv) < 2 || is.na(strtoi(argv[1]))) usage()

source("~/R/misc.r")

if (!file.exists(outFileRoot)) dir.create(outFileRoot)

exps <- argv[2:length(argv)]
highest.dPV <- maxdPV(exps)

bandwidth <- as.numeric(argv[1])
bandmins <- seq(0,highest.dPV,bandwidth)
for (bmin in bandmins) {
  band <- c(bmin,bmin+bandwidth)
  snd(band, exps)
  for (expName in exps) tot(band, expName)
}

q()
