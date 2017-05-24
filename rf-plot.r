#! /usr/bin/Rscript
##! /bin/bash setR

##
# Read multiple reaction field MC trial data files from the experiment directory and plot them as images.
#
# Time-stamp: <2017-05-24 13:41:15 gepr>
#
#dev.off()
INVERT <- T
argv <- commandArgs(TRUE)

usage <- function() {
  if (length(argv) < 1) {
    print("Usage: rf-plot.r <exp dir>")
    quit()
  }
}
if (length(argv) < 1) usage();

suppressMessages(library(imager))
source("~/R/misc.r")

##
## test for and create graphics subdirectory
##
if (!file.exists("graphics")) dir.create("graphics")

exp <- argv[1]

for (dir in c("dPV","dCV")) {
  ## test for the averaged file
  filename <- paste(exp,"_rxnprod-",dir,sep="")
  if (!file.exists(paste(filename,".csv",sep=""))) usage()

  print(paste("Generating avg ",dir," rxn field for ",exp,sep=""))

  pdf(paste("graphics/",filename,".pdf",sep=""),width=8.5,height=11)
  par(mar=c(5,6,4,2), oma=c(0,0,3,0), cex.main=2, cex.axis=2, cex.lab=2)
  rf <- read.csv(paste(filename,".csv",sep=""))
  rfmt <- t(as.matrix(rf))
  if (INVERT) rfmt <- max(rfmt)-rfmt
  rfi <- as.cimg(rfmt)
  plot(rfi,asp="varying",main="Average", xlab=dir, ylab="Cycle")
  grid()
  minor.tick(nx=5,ny=5)

  ###
  ## loop through MC trial files and add them to the PDF
  ###
  pattern <- paste("rxnprod-",dir,"-[0-9]+.csv",sep="")
  files <- list.files(path=exp, pattern=pattern,recursive=T,full.names=T)
  nplots <- length(files)
  ppp <- 4

  par(mfrow=c(ppp,1))
  trials <- 1
  for (f in files) {

    print(paste("    Now trial ",trials,sep=""))

    rf <- read.csv(f,colClasses="numeric")
    rfmt <- t(as.matrix(rf))
    if (INVERT) rfmt <- max(rfmt)-rfmt
    rfi <- as.cimg(rfmt)
    plot(rfi,asp="varying",main=paste("Trial",trials), xlab=dir,ylab="Cycle")
    grid()
    minor.tick(nx=5,ny=5)
    ##if (trials %% ppp == 0) par(mfrow=c(ppp,1))
    trials <- trials + 1
  }
  title(paste(exp,dir,"Reaction Fields"),outer=T)
  dev.off()

} ## end loop over direction

##> rf0 <- read.csv("ecrf000x/rxnprod-0000.csv.gz")
##> rf0i <- as.cimg(t(as.matrix(rf0)))
##> rf1 <- read.csv("ecrf000x/rxnprod-0001.csv.gz")
##> rf1i <- as.cimg(t(as.matrix(rf1)))
##> rf2 <- read.csv("ecrf000x/rxnprod-0002.csv.gz")
##> rf2i <- as.cimg(t(as.matrix(rf2)))
##> par(mfrow=c(3,1))
##> plot(rf0i,asp="varying")
##> plot(rf1i,asp="varying")
##> plot(rf2i,asp="varying")
