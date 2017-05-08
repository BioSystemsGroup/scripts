#! /usr/bin/Rscript
#! /bin/bash setR
argv <- commandArgs(TRUE)
##dev.off()

usage <- function() {
    print("Usage: hsol-dPV-1.r dPVMin dPVMax <exp directories>")
    print("  directories should contain files named hsolute_zone_1_2-[0-9]+.csv")
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
snd(band, exps)

q()

