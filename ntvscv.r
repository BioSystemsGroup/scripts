#! /usr/bin/Rscript
##
# Read the necTrig versus dCV .csv file and make a barplot.
#
# Time-stamp: <2018-03-14 10:02:06 gepr>
#

usage <- function() {
  print("Usage: plot-ntvscv.r <*_nectrig-nvsd-dCV∈[min,max).csv>")
  print("  e.g. plot-ntvscv.r mbase000x_nectrig-nvsd-dCV∈[0,100).csv")
  quit("no")
}

argv <- commandArgs(TRUE)
if (argv < 1) usage()
files <- argv

if (!file.exists("graphics")) dir.create("graphics")

for (f in files) {
  if (!file.exists(f)) {
    print(paste(f,"does not exist!"))
    next
  }

  prefix <- substr(f,0,regexpr("_nectrig-nvsd",f)-1)
  postfix <- substr(f,regexpr("dCV∈",f),regexpr(".csv",f)-1)
  main <- paste(prefix,postfix)
  d <- read.csv(f)

  plotFile <- paste("graphics/",prefix,"-nvsd-",postfix,".png",sep="")
  png(plotFile, width=1600, height=1600)
  ## set margins and title, axis, and label font sizes
  par(mar=c(5,6,4,2), cex.main=2, cex.axis=2, cex.lab=2)

  xlab <- paste("μ(",colnames(d)[1],")",sep="")
  ylab <- "aHPCs Triggered"
  barplot(d[,2],names.arg=d[,1],xlab=xlab,ylab=ylab,main=main)
  box()
  grid(NULL,NULL)
}
