#! /bin/bash setR
#! /usr/bin/Rscript
#argv <- commandArgs(TRUE)
dev.off()

usage <- function() {
    print("Usage: rxn-dPV-2.r dPVMin dPVMax <exp name>")
    print("  e.g. rxn-dPV-2.r 1 5 exp000x")
    print("  The current directory should contain files named like this:")
    print("    dPV.rxn/exp000x_rxnProduct_dPV∈[1,5]-1_2-0000-sum.csv and ")
    print("    dPV.rxn/exp000x_rxnProduct_dPV∈[1,5]-divisors.csv")
    print("  where the \"-divisors\" file contains the column numbers to use to divide into the sums to get the averages.")
    quit()
}
if (length(argv) < 3) usage()

dPVMin <- as.numeric(argv[1])
dPVMax <- as.numeric(argv[2])

source("~/R/misc.r")

###
##  Declare a couple of globals
###
inFileRoot <- "rxnProduct_zone_"
outFileRoot <- "dPV.rxn/"
if (!file.exists(outFileRoot)) {
  print(paste(outFileRoot,"must exist and contain the sums and divisors files."))
  usage()
  q()
}

exps <- argv[3:length(argv)]
highest.dPV <- maxdPV(exps)

band <- c(dPVMin, dPVMax)
for (expName in exps) tot(band, expName)

q()
