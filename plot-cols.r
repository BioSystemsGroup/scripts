#! /usr/bin/Rscript

##
# Read multiple *.csv files and plot each column.
#
# Time-stamp: <2016-07-27 14:26:09 gepr>
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
  compname <- substr(f,regexpr('_',f)+1,nchar(f))
  fileName.base <- substr(compname, 0, regexpr('(_|.csv)', compname)-1)
  dat <- read.csv(f)

  attach(dat)
  for (column in colnames(dat)[2:ncol(dat)]) {
    png(paste("graphics/",fileName.base,"-", column,".png",sep=""), width=1600, height=1600)
    par(cex=2, lwd=3, mar=c(5,6,4,2), cex.main=3, cex.axis=2, cex.lab=2)

    plot(Time,type="l", dat[[column]], ylab=column)
    grid()
  }
  detach(dat)
}

