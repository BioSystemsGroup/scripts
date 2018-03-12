#! /usr/bin/Rscript
###
## Read multiple reaction field MC trial data files from the experiment directory and plot them as images.
##
## Time-stamp: <2018-03-12 14:22:23 gepr>
###
argv <- commandArgs(T)

usage <- function() {
  print("Usage: ntsnaps.r <snap CSV files>")
  quit("no")
}
if (length(argv) < 1) usage();

suppressMessages(library(imager))
pngs <- vector()
for (snap in argv) {
  filebase <- substr(snap,0,regexpr('-[0-9]+.csv',snap)-1)
  cycle <- substr(snap,regexpr('-[0-9]+.csv',snap)+1,regexpr('.csv',snap)-1)
  s <- read.csv(snap)
  i <- as.cimg(as.vector(as.matrix(s[,2:ncol(s)])),x=ncol(s)-1,y=length(s[,1]))
  imFile <-  paste(filebase,"-",cycle,".png", sep="")
  save.image(i, imFile)
  scaleX <- ncol(s)*20
  scaleY <- nrow(s)*20
  system(paste("convert ",imFile, " -scale ",scaleX,"x",scaleY, " ", imFile, sep=""))
  pngs <- paste(pngs,imFile)
}
snapStart <- regexpr('/snap',filebase)
snapStop <- regexpr('-[0-9]+-SS',filebase)
animFileName <- paste(substr(filebase,0,snapStart),"anim",substr(filebase, snapStop,nchar(filebase)),".gif", sep="")
system(paste("convert -delay 100 ",pngs," ", animFileName, sep=""))
