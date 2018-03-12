#! /usr/bin/Rscript
###
## Read multiple reaction field MC trial data files from the experiment directory and plot them as images.
##
## Time-stamp: <2018-03-12 10:06:13 gepr>
###
argv <- commandArgs(T)

usage <- function() {
  print("Usage: ntsnaps.r <snap files>")
  quit("no")
}
if (length(argv) < 1) usage();

suppressMessages(library(imager))

for (snap in argv) {
  s <- read.csv(snap)
  i <- as.cimg(as.vector(as.matrix(s[,2:ncol(s)])),x=ncol(s)-1,y=length(s[,1]))
  imFile <-  paste(snap,".png", sep="")
  save.image(i, imFile)
  scaleX <- ncol(s)*10
  scaleY <- nrow(s)*10
  system(paste("convert ",imFile, " -scale ",scaleX,"x",scaleY, " ", imFile, sep=""))
}
