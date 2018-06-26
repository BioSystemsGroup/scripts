#! /usr/bin/Rscript
##
# Read multiple *.csv files and plot each column vs the 1st.
#
# Time-stamp: <2018-06-20 11:30:27 gepr>
#


plot.data <- T
plot.svg <- F
ma.window <- 181

source("~/R/misc.r")

argv <- commandArgs(TRUE)

if (length(argv) < 2) {
    print("Usage: cmp-movavg.r <analysis .csv file> <analysis .csv file>")
    print("  e.g. cmp-movavg.r x00[1-6]_body.csv y00[1-6]_hsolute-dCV.csv")
    print("Note that columns must match across all files.")
    quit()
}


# determine # of plots
nplots <- length(argv)
plot.cols <- round(sqrt(nplots))
#plot.cols <- 2
# add a new row if we rounded up
plot.rows <- ifelse(plot.cols >= sqrt(nplots), plot.cols, plot.cols+1)
#plot.rows <- 4

#
# test for and create graphics subdirectory
#
if (!file.exists("graphics")) dir.create("graphics")

data <- vector("list")
data.ma <- vector("list")
titles <- vector("list")
## get component name from basename of 1st argument
argv.basename <- basename(argv[1])
compname <- substr(argv.basename,regexpr('_',argv.basename)+1,nchar(argv.basename))
fileName.base <- substr(compname, 0, regexpr('(_|.csv)', compname)-1)
expnames <- ""
print(fileName.base)
filenum <- 1
for (f in argv) {
  nxtName <- substr(basename(f),0,regexpr('_',basename(f))-1)
  titles[[filenum]] <- nxtName
  expnames <- paste(expnames,nxtName,sep="-")
  raw <- read.csv(f)
  data[[filenum]] <- raw

  raw[is.na(raw)] <- 0 # replace NAs with zeros?

  if (nrow(raw) < ma.window) {
    ma.window.new <- nrow(raw)/4
    if((nrow(raw) %% 2) == 0) {
		  ma.window.new <- ma.window.new + 1
	}
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

  ## if we're plotting the original data, use it for the dependent scale
  if (plot.data) refData <- data
  else refData <- data.ma

  for (df in refData) {
    if (!is.element(column,colnames(df)) || all(is.na(df[column]))) skip <- TRUE
    else {
      min.2 <- min(min.2, min(df[column], na.rm=TRUE), na.rm=TRUE)
      max.2 <- max(max.2, max(df[column], na.rm=TRUE), na.rm=TRUE)
    }
  }
  ##if (skip) next  # skip columns that don't exist in all files

  ##print(paste("Working on",column,"..."))

  fileName <- paste("graphics/", fileName.base, "-", column,
                    ifelse(plot.data, "-wd", ""), expnames, sep="")
  if (nchar(fileName) > 255) {
    library(digest)
    fileName <- paste("graphics/", fileName.base, "-", column,
                      ifelse(plot.data, "-wd", ""), digest(expnames), sep="")
  }

  if (plot.svg) {
    svg(paste(fileName,".svg",sep=""), width=10, height=10)
  } else {
    png(paste(fileName,".png",sep=""), width=1600, height=1600)
  }

  # set margins and title, axis, and label font sizes
  par(mar=c(5,6,4,2), cex.main=2, cex.axis=2, cex.lab=2)
  par(mfrow=c(plot.rows,plot.cols))

  # plot this column from all data sets
  ndx <- 1
  for (df in data.ma) {
    attach(df)
    if (exists(column)) {
      zeroed <- F
      ma <- cbind(get(column.1), get(column))
    } else {
      zeroed <- T
      index <- get(column.1)
      ma <- cbind(index, rep(0,length(index)))
    }
    detach(df)
    colnames(ma) <- c(column.1, column)
    plot(ma, main=titles[[ndx]], xlim=c(0,max.1), ylim=c(min.2,max.2), type="l") ##, pch=NA)
    ##lines(ma)
    ## if we're plotting original data, use points()
    if (plot.data && !zeroed) {
      attach(data[[ndx]])
      dat <- cbind(get(column.1), get(column))
      detach(data[[ndx]])
      points(dat[,1],dat[,2],pch="·")
    }
    minor.tick(nx=5,ny=5)

    grid()
    ndx <- ndx+1
  }
  setTxtProgressBar(pb,getTxtProgressBar(pb)+1)
}

close(pb)

