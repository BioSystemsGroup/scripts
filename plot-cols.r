#! /usr/bin/Rscript

##
# Read multiple *.csv files and plot each column.
#
# Time-stamp: <2017-01-03 10:49:56 gepr>
#
#dev.off()

argv <- commandArgs(TRUE)

if (length(argv) < 1) {
    print("Usage: plot-cols *_rxnProduct_zone_0.csv")
    print("  e.g. cmp-by-col.r x00[1-6]_mean_body.csv")
    quit()
}

#
# test for and create graphics subdirectory
#
if (!file.exists("graphics")) dir.create("graphics")

for (f in argv) {
  seps <- gregexpr('/',f)[[1]] # get all the '/' locations
  aftersep <- substr(f,seps[length(seps)]+1,nchar(f)) # get everything after the last '/'
  expname <- substr(aftersep,0,regexpr('_',aftersep)-1)
  compname <- substr(f,regexpr('_',f)+1,nchar(f))
  fileName.base <- paste(expname,substr(compname, 0, regexpr('(_|.csv)', compname)-1),sep='-')
  dat <- read.csv(f)

  attach(dat)
  for (column in colnames(dat)[2:ncol(dat)]) {
    png(paste("graphics/",fileName.base,"-", column,".png",sep=""), width=1600, height=1600)
    par(cex=2, lwd=3, mar=c(5,6,4,2), cex.main=3, cex.axis=2, cex.lab=2)

    plot(Time,type="l", dat[[column]], ylab=column)
    grid()
    title(expname)
  }
  detach(dat)
}

