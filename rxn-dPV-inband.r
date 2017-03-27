#! /usr/bin/Rscript
#! /bin/bash setR
argv <- commandArgs(TRUE)
##dev.off()

###
## Calculates the averages of the reaction product data.  Differs from rxn-dPV.r
## in that you provide a min and max dPV, wheras rxn-dPV.r takes a band and
## extracts the max dPV from the data files.
##
## Time-stamp: <2017-03-21 09:15:37 gepr>
###

usage <- function() {
    print("Usage: rxn-dPV-inband.r dPVMin dPVMax <exp directories>")
    print("  directories should contain files named rxnProduct_zone_1_2-[0-9]+.csv")
    quit()
}
if (length(argv) < 3) usage()

source("~/R/misc.r")

dPVMin <- as.numeric(argv[1])
dPVMax <- as.numeric(argv[2])

if (!file.exists(outFileRoot)) dir.create(outFileRoot)

exps <- argv[3:length(argv)]
##highest.dPV <- maxdPV(exps)

band <- c(dPVMin, dPVMax)
snd(band, exps) # sum and divisors
for (expName in exps) tot(band, expName) # sums/divisors

q()

