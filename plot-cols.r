#! /usr/bin/Rscript

###
## Read multiple *.csv files and plot each column.
##
## Time-stamp: <2019-11-17 16:58:00 gepr>
##

sample.freq <- 10
ma.window <- 181
plot.data <- F
plot.svg <- F

source("~/R/misc.r") # for moving average

argv <- commandArgs(TRUE)

usage <- function() {
  print("Usage: plot-cols <raw|data|nodata> <CSV file1> <CSV file2>")
  print("  e.g. plot-cols.r exp_hsolute-dCV∈[0,100).csv")
  quit()
}
if (length(argv) < 2) {
  cat( "Not enough arguments.\n" )
  usage()
}
data.status <- argv[1]
if (data.status != "raw" && data.status != "data" && data.status != "nodata") {
  cat( paste("Data status", data.status, "invalid.\n"))
  usage()
}

files <- argv[-1]

###
## test for and create graphics subdirectory
###
if (!file.exists("graphics")) dir.create("graphics")

for (f in files) {
  seps <- gregexpr('/',f)[[1]] # get all the '/' locations
  aftersep <- substr(f,seps[length(seps)]+1,nchar(f)) # get everything after the last '/'
  expname <- substr(aftersep,0,regexpr('_',aftersep)-1)
  compname <- substr(f,regexpr('_',f)+1,nchar(f))
  fileName.base <- paste(expname,substr(compname, 0, regexpr('.csv', compname)-1),sep='-')

if (grepl('/', fileName.base)) {
  library(stringr)
  fileName.base <- str_replace_all(fileName.base, '/', '_')
}
cat(paste(fileName.base,"\n"))

  dat <- read.csv(f)

  dat.tmp <- dat
  dat.tmp[is.na(dat.tmp)] <- 0 # replace NAs with zeros
  if (nrow(dat.tmp) < ma.window) {
    ma.window.new <- nrow(dat.tmp)/4
    cat("WARNING! MA Window of",ma.window,"is longer than series. Using window of",ma.window.new,"\n")
    ma.window <- ma.window.new
  }
  dat.ma <- apply(dat.tmp[,2:ncol(dat.tmp)], 2, ma.cent, n=ma.window)
  dat.ma <- cbind(dat.tmp[,1], dat.ma)
  dat.ma <- as.data.frame(dat.ma)
  colnames(dat.ma) <- colnames(dat)

  attach(dat)
  for (column in colnames(dat)[2:ncol(dat)]) {
    fileName <- paste("graphics/", fileName.base, "-", column,
                      ifelse(plot.data, "-wd", ""), sep="")
    
    ## if there are no values, skip this plot
    if (all(is.na(dat[[column]]))) {
        cat("All values are NA for",column,"Skipping the plot.\n")
        next()
    } 
    
    if (plot.svg) {
      svg(paste(fileName,".svg",sep=""), width=9, height=9)
      cex=1
    } else {
      png(paste(fileName,".png",sep=""), width=1600, height=1600)
      cex=2.5
    }
    par(cex=cex, lwd=3, mar=c(5,6,4,2), cex.main=1, cex.axis=1, cex.lab=1)

    ## raw ≡ only raw data
    ## nodata ≡ only moving average
    ## data ≡ raw data + moving average
    if (data.status == "data" || data.status == "raw") {
      refData <- dat[[column]]
      plot.data <- T
    } else { # data.status == "nodata"
      refData <- dat.ma
      plot.data <- F
    }
    ## handle the case where there's only 1 value in the series
    ylim <- c(min(0, refData, na.rm=T), max(refData,na.rm=T))

    if (plot.data) {
      pointsize <- ifelse(data.status == "raw", cex*2, cex)
      if (data.status == "raw") pointsize <- cex*2
      plot( dat[ (row(dat)%%sample.freq)==0, 1], dat[ (row(dat)%%sample.freq)==0, column],
        ylim=ylim,
        xlab=colnames(dat)[1], ylab=column, type="p", pch="·",cex=pointsize)
      if (data.status == "data")
        lines(dat.ma[ (row(dat.ma)%%sample.freq)==0, 1], dat.ma[ (row(dat.ma)%%sample.freq)==0, column], lwd=5)
    } else {
      plot(Time, dat.ma[[column]],
      ylim=ylim,
      ylab=column, type="l")
    }

    grid()
    minor.tick(nx=5, ny=5, tick.ratio=0.5)

    if (data.status == "raw") title(fileName.base)
    else title(paste(fileName.base,", maw =",ma.window))

  }
  detach(dat)
}

