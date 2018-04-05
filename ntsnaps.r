#! /usr/bin/Rscript
###
## Read multiple reaction field MC trial data files from the experiment directory and plot them as images.
##
## Time-stamp: <2018-04-05 11:16:02 gepr>
###

ANIMATE <- T
NUM.STATES <- 3

argv <- commandArgs(T)

usage <- function() {
  print("Usage: ntsnaps.r <snap CSV files>")
  quit("no")
}
if (length(argv) < 1) usage();

## define the colourscale -- turns single value of blue into black, white, yellow, and red
cscale <- function(r,g,b) {
  ifelse (b == 0, rgb(0,0,0),
          ifelse (b <= 1/3, rgb(1,1,1),
                  ifelse (b <= 2/3, rgb(1,1,0),
                          rgb(1,0,0))))
}

suppressMessages(library(imager))

pngs <- vector()
for (snap in argv) {
  filebase <- substr(snap,0,regexpr('-[0-9]+.csv',snap)-1)
  cycle <- substr(snap,regexpr('-[0-9]+.csv',snap)+1,regexpr('.csv',snap)-1)
  s <- read.csv(snap)
  ## divide by number of states to get a range of one color (blue) and render as a data.frame
  sxyv <- as.data.frame(as.cimg(as.vector(t(as.matrix(s[,2:ncol(s)])))/NUM.STATES,x=ncol(s)-1,y=length(s[,1])))
  ## append the color column
  sxyvc <- cbind(sxyv,rep(1,nrow(sxyv)))
  colnames(sxyvc) <- c(colnames(sxyv),"cc")
  ## reorder the columns for as.cimg()
  sxycv <- sxyvc[,c("x","y","cc","value")]

  i <- as.cimg(sxycv)

  imFile <-  paste(filebase,"-",cycle,".png", sep="")
  if (F) {
    png(imFile,width=ncol(s)-1,height=nrow(s))
    par(mar=rep(0,4)) ## no margins!
    plot(i,interpolate=F,colorscale=cscale,rescale=F,axes=F)
    dev.off()
  }
  if (T) save.image(i,imFile)
  ## use ImageMagick to scale it up
  scaleX <- ncol(s)*20
  scaleY <- nrow(s)*20
  system(paste("convert ",imFile, " -scale ",scaleX,"x",scaleY, " ", imFile, sep=""))

  pngs <- paste(pngs,imFile)
}

## animate the images
if (ANIMATE) {
  snapStart <- regexpr('/snap',filebase)
  snapStop <- regexpr('-[0-9]+-SS',filebase)
  animFileName <- paste(substr(filebase,0,snapStart),"anim",substr(filebase, snapStop,nchar(filebase)),".gif", sep="")
  system(paste("convert -delay 1 ",pngs," ", animFileName, sep=""))
}
