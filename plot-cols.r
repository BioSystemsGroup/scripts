#! /usr/bin/Rscript

##
# Read multiple *.csv files and plot each column.
#
# Time-stamp: <2017-01-19 11:47:15 gepr>
#
#dev.off()

#########################################
## define the moving average functions
ma.cent <- function(x,n=5) {
  if (n%%2 == 0) {
     print("A centered moving average should use an odd window.")
     q("no")
  }
  filter(x,rep(1/n,n), sides=2)
}
ma.left <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}
##
#########################################

argv <- commandArgs(TRUE)

if (length(argv) < 1) {
    print("Usage: plot-cols *_hsolute_zone_0.csv")
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
  fileName.base <- paste(expname,substr(compname, 0, regexpr('(-|.csv)', compname)-1),sep='-')
  dat <- read.csv(f)

  dat.tmp <- dat
  dat.tmp[is.na(dat.tmp)] <- 0 # replace NAs with zeros
  dat.ma <- apply(dat.tmp[,2:ncol(dat.tmp)], 2, ma.cent, n=301)
  dat.ma <- cbind(dat.tmp[,1], dat.ma)
  dat.ma <- as.data.frame(dat.ma)
  colnames(dat.ma) <- colnames(dat)

  attach(dat)
  for (column in colnames(dat)[2:ncol(dat)]) {
    png(paste("graphics/",fileName.base,"-", column,".png",sep=""), width=1600, height=1600)
    par(cex=2, lwd=3, mar=c(5,6,4,2), cex.main=3, cex.axis=2, cex.lab=2)

    plot(Time,pch="·", dat[[column]], ylab=column)
    lines(dat.ma[[column]])

    grid()
    title(expname)
  }
  detach(dat)
}

