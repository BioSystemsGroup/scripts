#! /usr/bin/Rscript

###
## Read multiple *.csv files and plot each column.
##
## Time-stamp: <2018-05-22 11:19:26 gepr>
##

plot.data <- F
plot.svg <- F

source("~/R/misc.r") # for moving average

argv <- commandArgs(TRUE)

if (length(argv) < 1) {
    print("Usage: plot-cols <CSV file1> <CSV file2>")
    print("  e.g. plot-cols.r exp_hsolute-dCV∈[0,100).csv")
    quit()
}

###
## test for and create graphics subdirectory
###
if (!file.exists("graphics")) dir.create("graphics")

for (f in argv) {
  seps <- gregexpr('/',f)[[1]] # get all the '/' locations
  aftersep <- substr(f,seps[length(seps)]+1,nchar(f)) # get everything after the last '/'
  expname <- substr(aftersep,0,regexpr('_',aftersep)-1)
  compname <- substr(f,regexpr('_',f)+1,nchar(f))
  fileName.base <- paste(expname,substr(compname, 0, regexpr('.csv', compname)-1),sep='-')
print(fileName.base)
  dat <- read.csv(f)

  dat.tmp <- dat
  dat.tmp[is.na(dat.tmp)] <- 0 # replace NAs with zeros
  dat.ma <- apply(dat.tmp[,2:ncol(dat.tmp)], 2, ma.cent, n=181)
  dat.ma <- cbind(dat.tmp[,1], dat.ma)
  dat.ma <- as.data.frame(dat.ma)
  colnames(dat.ma) <- colnames(dat)

  attach(dat)
  for (column in colnames(dat)[2:ncol(dat)]) {
    fileName <- paste("graphics/", fileName.base, "-", column,
                      ifelse(plot.data, "-wd", ""), sep="")
    if (plot.svg) {
      svg(paste(fileName,".svg",sep=""), width=10, height=10)
    } else {
      png(paste(fileName,".png",sep=""), width=1600, height=1600)
    }

    par(cex=2, lwd=3, mar=c(5,6,4,2), cex.main=1, cex.axis=1, cex.lab=1)

    plot(Time, dat.ma[[column]], ylab=column, type="l")
    if (plot.data) points(dat[[column]], pch="·")

    grid()
    minor.tick(nx=5, ny=5, tick.ratio=0.5)

    title(fileName.base)
  }
  detach(dat)
}

