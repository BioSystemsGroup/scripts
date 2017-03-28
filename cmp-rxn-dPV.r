#! /usr/bin/Rscript
#! /bin/bash setR
argv <- commandArgs(TRUE)

##
# Read multiple *.csv files and plot each column vs the 1st.
#
# Time-stamp: <2017-03-28 08:27:07 gepr>
#

plot.data <- TRUE

source("~/R/misc.r")

if (length(argv) < 2) {
    print("Usage: cmp-rxn-dPV.r <experiment names>")
    print("  e.g. cmp-rxn-dPV.r exp000x exp001x ...")
    print("  Files like dPV.rxn/exp000x_rxnProduct_dPV∈[0,5]-totals.csv should exist.")
    print("Note that columns and band sizes must match across all files.")
    quit()
}


PATH <- "dPV.rxn"
exps <- argv
fnbase <- "_rxnProduct_dPV∈["
fnsuff <- "]-totals.csv"

# determine # of plots
nplots <- length(exps)
plot.cols <- round(sqrt(nplots))
#plot.cols <- 3
# add a new row if we rounded up
plot.rows <- ifelse(plot.cols >= sqrt(nplots), plot.cols, plot.cols+1)
#plot.rows <- 6

#
# test for and create graphics subdirectory
#
if (!file.exists("graphics")) dir.create("graphics")

## get the max dPV
for (exp in exps) {
  pattern=paste(exp,fnbase,"[[:print:]]+",fnsuff,sep="")
  expfiles <- list.files(path = PATH, pattern = pattern, full.names=T)

  #expbounds <- vector()
  for (f in expfiles) {
    min.ndx <- regexpr("[[:digit:]]+,[[:digit:]]+]", f)
    max.ndx <- regexpr("[[:digit:]]+]-totals.csv", f)
    max.end <- max.ndx + regexpr("]",substr(f,max.ndx,nchar(f)))-2
    filebounds <- strsplit(substr(f,min.ndx,max.end),',')
    filebounds <- as.numeric(filebounds[[1]])
    if (exists("expbounds")) {
      if (filebounds[1] < expbounds[1]) expbounds[1] <- filebounds[1]
      if (filebounds[2] > expbounds[2]) expbounds[2] <- filebounds[2]
    } else {
      expbounds <- filebounds
    }
    fileband <- filebounds[2]-filebounds[1]
    if (exists("band")) {
      if (fileband != band) {
        print("Error!  All the bands must be the same.")
        stop()
        q()
      }
    } else {
      band <- fileband
    }
    rm("filebounds")
  } ## end for (f in expfiles)

  if (exists("bounds")) {
    bounds <- rbind(bounds, expbounds)
  } else {
    bounds <- expbounds
  }

} ## end for (exp in exps)
rownames(bounds) <- exps

## sort bounds by largest dPV
bounds <- bounds[sort.list(bounds[,2],decreasing=T),]

for (rndx in seq(0,max(bounds[,2])-band,band)) {
  print(paste("Processing dPV∈[", rndx, ",", rndx+band, "]", sep=""))

  ## build this list of data.frames
  dat <- vector("list")
  dat.ma <- vector("list")
  for (endx in 1:nrow(bounds)) {
    exp <- rownames(bounds)[endx]
    ## if it exists, read it
    f <- paste(PATH,"/",exp,fnbase,rndx,",",rndx+band,fnsuff,sep="")
    if (file.exists(f)) {
      raw <- read.csv(f)
      dat[[endx]] <- raw
      raw[is.na(raw)] <- 0 ## replace NAs with 0
      ma <- apply(raw[,2:ncol(raw)], 2, ma.cent, n=301)
      ma <- as.data.frame(cbind(raw[,1],ma))
      colnames(ma) <- colnames(raw)
      dat.ma[[endx]] <- ma
    } else {
      ## if not, create an NA-filled data.frame with the headers
      ##    assume ∃ a previous data.frame to use
      dat[[endx]] <- data.frame(matrix(0, nrow = nrow(dat[[endx-1]]), ncol = ncol(dat[[endx-1]])))
      colnames(dat[[endx]]) <- colnames(dat[[endx-1]])
      dat.ma[[endx]] <- dat[[endx]]
    }

  } ## end for (endx in 1:nrow(bounds))

  ## plot all experiments 1 page/column
  columns <- colnames(dat[[1]])
  max.x <- max(dat[[1]][1])

  for (column in columns[2:length(columns)]) {
    min.y <- 0
    max.y <- 0
    for (endx in 1:nrow(bounds)) {
      if (!is.element(column,colnames(dat[[endx]]))) next # skip columns that don't exist in all experiments
      max.exp <- max(dat[[endx]][column])
      if (max.exp > max.y) max.y <- max.exp
    }

    ## set up the page
    title.suff <- paste("-",column,"-dPV∈[",rndx,",",rndx+band,"]",sep="")
    outputFile <- paste("graphics/rxn-", paste(exps,collapse="_"),title.suff,".png",sep="")
    png(outputFile, width=1600, height=1600)
    par(mar=c(5,6,4,2), cex.main=2, cex.axis=2, cex.lab=2)
    par(mfrow=c(plot.rows, plot.cols))

    for (endx in 1:nrow(bounds)) {
      exp <- rownames(bounds)[endx]
      ## plot this data.frame on the page
      attach(dat[[endx]])
      plot(Time, get(column), pch="•", ylab=column, ylim=c(min.y,max.y), xlim=c(0,max.x), main=paste(exp,title.suff,sep=""))
      detach(dat[[endx]])

      attach(dat.ma[[endx]])
      lines(Time, get(column),lwd=2)
      detach(dat.ma[[endx]])
      grid()
      minor.tick(nx=5,ny=5)
    }
    dev.off()
  }

} ## end for (rndx in 0:max(bounds[,2]))

#q()
