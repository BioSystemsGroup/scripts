#! /usr/bin/Rscript
###
## Read multiple reaction field MC trial data files from the experiment directory and plot them as images.
##
## Time-stamp: <2018-04-09 14:07:40 gepr>
###

PLOTTING <- T
ANIMATE <- T
NUM.STATES <- 3

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
  snapStart <- regexpr('/snap',filebase)
  snapStop <- regexpr('-[0-9]+-SS',filebase)
  trial <- substr(snap, regexpr('[0-9]+-SS:',snap), regexpr('-SS:',snap)-1)
  ss <- substr(snap,regexpr('SS:',snap),regexpr('-[0-9]+.csv',snap)-1)
  cycle <- substr(snap,regexpr('-[0-9]+.csv',snap)+1,regexpr('.csv',snap)-1)
  s <- read.csv(snap)
  ## divide by number of states to get a range of one color (blue) and render as a data.frame
  sxyv <- as.data.frame(as.cimg(as.vector(t(as.matrix(s[,2:ncol(s)])))/NUM.STATES,x=ncol(s)-1,y=length(s[,1])))
  ## append the color column
  sxyvc <- cbind(sxyv,rep(3,nrow(sxyv)))
  colnames(sxyvc) <- c(colnames(sxyv),"cc")
  ## reorder the columns for as.cimg()
  sxycv <- sxyvc[,c("x","y","cc","value")]

  suppressWarnings(i <- as.cimg(sxycv))

  imFile <-  paste(filebase,"-",cycle,".png", sep="")

  if (PLOTTING) {
    title <- paste("Trial:",trial, ss, "Cycle:", cycle)
    ###
    ## define the colourscale for plotting: turns single value of blue into black, white, yellow, and red
    ###
    cscale <- function(r,g,b) {
      ifelse (b == 0, rgb(0,0,0),
              ifelse (b <= 1/3, rgb(1,1,1),
                      ifelse (b <= 2/3, rgb(1,1,0),
                              rgb(1,0,0))))
    }
    ##png(imFile,width=ncol(s)-1,height=nrow(s))
    ##par(mar=rep(0,4)) ## no margins!
    png(imFile ,width=640,height=480)
    plot(i, xlim=c(1,ncol(s)-1), ylim=c(nrow(s),1),
         xlab="X", ylab="Y", main=title,
         asp="varying", interpolate=F, colorscale=cscale, rescale=F, yaxt='n', xaxt='n')
    dev.off()
  }
  if (!PLOTTING) {
    ###
    ## define colors for save.image: black -- no cell, white -- normal, yellow -- stressed, red -- necrotic
    ###
    parsecolor <- function(shade) {
      red <- matrix(0,nrow=nrow(shade),ncol=ncol(shade))
      green <- matrix(0,nrow=nrow(shade),ncol=ncol(shade))
      blue <- matrix(0,nrow=nrow(shade),ncol=ncol(shade))
      for (x in 1:nrow(shade)) {
        for (y in 1:ncol(shade)) {
          if (1/3 <= shade[x,y] && shade[x,y] < 2/3) {
            red[x,y] <- 1
            green[x,y] <- 1
            blue[x,y] <- 1
          }
          if (2/3 <= shade[x,y] && shade[x,y] < 1) {
            red[x,y] <- 1
            green[x,y] <- 1
          }
          if (shade[x,y] == 1) {
            red[x,y] <- 1
          }
        }
      }
      list(red,green,blue)
    }
    current.blue <- i[,,,3]
    irecolored <- i
    color <- parsecolor(current.blue)
    irecolored[,,,1] <- color[[1]]
    irecolored[,,,2] <- color[[2]]
    irecolored[,,,3] <- color[[3]]
    save.image(irecolored,imFile)

    ## use ImageMagick to scale it up
    scaleX <- ncol(s)*20
    scaleY <- nrow(s)*20
    system(paste("convert ",imFile, " -scale ",scaleX,"x",scaleY, " ", imFile, sep=""))
  }

  pngs <- paste(pngs,imFile)
}

## animate the images
if (ANIMATE) {
  animFileName <- paste(trial,"-",ss,"-","anim",".gif", sep="")
  print(paste("Animating", animFileName))
  system(paste("convert -delay 1 ",pngs," ", animFileName, sep=""))
}

