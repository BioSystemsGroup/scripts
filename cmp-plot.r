#! /usr/bin/Rscript
##
# Read multiple *.csv files and plot each column vs the 1st.
#
# Time-stamp: <2019-06-17 11:29:22 gepr>
#

sample.freq <- 1
plot.svg <- T
ma.window <- 181
use.frames <- F

source("~/R/misc.r")

argv <- commandArgs(TRUE)

if (length(argv) < 3) {
    print("Usage: cmp-plot.r <raw|data|nodata> <analysis .csv file> <analysis .csv file>")
    print("  e.g. cmp-plot.r data x00[1-6]_body.csv y00[1-6]_hsolute-dCV.csv")
    print("Note that columns must match across all files.")
    quit()
}

data.status <- argv[1]
files <- argv[-1]

if (use.frames) {
  ## determine # of plot frames
  nplots <- length(files)
  plot.cols <- round(sqrt(nplots))
  ##plot.cols <- 2
  ## add a new row if we rounded up
  plot.rows <- ifelse(plot.cols >= sqrt(nplots), plot.cols, plot.cols+1)
  ##plot.rows <- 4
}

#
# test for and create graphics subdirectory
#
if (!file.exists("graphics")) dir.create("graphics")

data <- vector("list")
data.ma <- vector("list")
titles <- vector("list")
## get component name from basename of 1st argument
files.basename <- basename(files[1])
compname <- substr(files.basename,regexpr('_',files.basename)+1,nchar(files.basename))
fileName.base <- substr(compname, 0, regexpr('(_|.csv)', compname)-1)
expnames <- ""
cat(paste(fileName.base,'\n'))
filenum <- 1
for (f in files) {
  nxtName <- substr(basename(f),0,regexpr('_',basename(f))-1)
  titles[[filenum]] <- nxtName
  expnames <- paste(expnames,nxtName,sep="-")
  raw <- read.csv(f)
  data[[filenum]] <- raw

  raw[is.na(raw)] <- 0 # replace NAs with zeros?

  if (nrow(raw) < ma.window) {
    ma.window.new <- nrow(raw)/4
    cat("WARNING! MA Window of",ma.window,"is longer than series. Using window of",ma.window.new,"\n")
    ma.window <- ma.window.new
  }
  ma <- apply(raw[,2:ncol(raw)], 2, ma.cent, n=ma.window)
  ma <- cbind(raw[,1], ma)
  colnames(ma)[1] <- colnames(raw)[1]
  data.ma[[filenum]] <- as.data.frame(ma)

  filenum <- filenum+1
}
## assume all Time vectors are the same
columns <- colnames(data[[1]])
column.1 <- columns[1]
max.1 <- max(data[[1]][column.1])

pb <- txtProgressBar(min=0,max=length(columns),style=3)
setTxtProgressBar(pb,1)

for (column in columns[2:length(columns)]) {
  skip <- FALSE

  ###
  ## get min & max of this column over all data sets if it exists
  ##
  min.2 <- Inf
  max.2 <- -1 # init max.2

  ## raw ≡ only raw data
  ## nodata ≡ only moving average
  ## data ≡ raw data + moving average
  if (data.status == "data" || data.status == "raw") {
    refData <- data
    plot.data <- T
  } else { # data.status == "nodata"
    refData <- data.ma
    plot.data <- F
  }

  for (df in refData) {
    if (!is.element(column,colnames(df)) || all(is.na(df[column]))) skip <- TRUE
    else {
      min.2 <- min(min.2, min(df[column], na.rm=TRUE), na.rm=TRUE)
      max.2 <- max(max.2, max(df[column], na.rm=TRUE), na.rm=TRUE)
    }
  }

  ##print(paste("Working on",column,"..."))

  fileName <- paste("graphics/", fileName.base, "-", column,
                    ifelse(plot.data, "-wd", ""), "-", expnames, sep="")
  if (nchar(fileName) > 255) {
    library(digest)
    fileName <- paste("graphics/", fileName.base, "-", column,
                      ifelse(plot.data, "-wd", ""), "-", digest(expnames), sep="")
  }

  if (plot.svg) {
    svg(paste(fileName,".svg",sep=""), width=10, height=10)
  } else {
    png(paste(fileName,".png",sep=""), width=1600, height=1600)
  }

  ## set margins and title, axis, and label font sizes
  ##   format is c(bottom, left, top, right)
  if (use.frames) {
    par(mar=c(5,6,4,2), cex.main=2, cex.axis=2, cex.lab=2)
    par(mfrow=c(plot.rows,plot.cols))
  } else {
    ## place the right margin 1 unit for each of the characters +  units for the line
    right.margin <- max(nchar(titles)) + ifelse(plot.svg, -3, 6)
    par(mar=c(5,6,4,right.margin), cex.main=2, cex.axis=2, cex.lab=2)
  }

  datnames <- c()
  datcolors <- c()
  datltys <- numeric()
  # plot this column from all data sets
  ndx <- 1
  for (df in data.ma) {
    datcolors[ndx] <- ndx+1
    datnames[ndx] <- titles[[ndx]]
    datltys[ndx] <- 1
    attach(df)
    if (exists(column)) {
      zeroed <- F
      ma <- as.data.frame(cbind(get(column.1), get(column)))
    } else {
      zeroed <- T
      index <- get(column.1)
      ma <- as.data.frame(cbind(index, rep(0,length(index))))
    }
    detach(df)
    colnames(ma) <- c(column.1, column)
    if (plot.data && !zeroed) {
      attach(data[[ndx]])
      dat <- as.data.frame(cbind(get(column.1), get(column)))
      detach(data[[ndx]])
      colnames(dat) <- c(column.1, column)
      if (use.frames || ndx == 1) {
        mainTitle <- ifelse(use.frames, titles[[ndx]], fileName.base)
        plot(dat[ (row(dat)%%sample.freq)==0 ,1], dat[ (row(dat)%%sample.freq)==0 ,2],
             main=mainTitle,
             xlab=colnames(dat)[1], ylab=colnames(dat)[2],
             xlim=c(0,max.1), ylim=c(min.2,max.2),
             type="p",
             col=datcolors[ndx],
             pch="·")
      } else {
        points(dat[ (row(dat)%%sample.freq)==0 ,1], dat[ (row(dat)%%sample.freq)==0 ,2],
               pch="·",
               col=datcolors[ndx])
      }
      if (data.status == "data") lines(ma[ (row(ma)%%sample.freq)==0 ,1], ma[ (row(ma)%%sample.freq)==0 ,2],
                                       col=datcolors[ndx])
    } else {
      if (use.frames || ndx == 1) {
        mainTitle <- ifelse(use.frames, titles[[ndx]], fileName.base)
        plot(ma[ (row(ma)%%sample.freq)==0 ,1], ma[ (row(ma)%%sample.freq)==0 ,2],
             main=mainTitle,
             xlab=colnames(ma)[1], ylab=colnames(ma)[2],
             xlim=c(0,max.1), ylim=c(min.2,max.2),
             type="l",
             col=datcolors[ndx]
             )#, pch=NA)
      } else {
        lines(ma[ (row(ma)%%sample.freq)==0 ,1], ma[ (row(ma)%%sample.freq)==0 ,2],
              col=datcolors[ndx])
      }
    }
    minor.tick(nx=5,ny=5)

    grid()
    ndx <- ndx+1
  }
  if (!use.frames) {
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 1), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    fontSize <- ifelse(plot.svg, 0.8, 2)
    legend("right", legend=datnames, col=datcolors, lty=datltys, cex=fontSize, bty="n", inset=c(0,0))
  }

  setTxtProgressBar(pb,getTxtProgressBar(pb)+1)
}

close(pb)

