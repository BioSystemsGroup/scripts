#! /usr/bin/Rscript
##! /bin/bash setR

##
# Read multiple *.csv files and plot each column vs the 1st.
#
# Time-stamp: <2016-12-23 10:46:49 gepr>
#
#dev.off()

plot.data <- TRUE

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

if (length(argv) < 2) {
    print("Usage: cmp-by-col.r *_rxnProduct_zone_0.csv")
    print("  e.g. cmp-by-col.r x00[1-6]_mean_body.csv")
    print("Note that columns must match across all files.")
    quit()
}


# determine # of plots
nplots <- length(argv)
plot.cols <- round(sqrt(nplots))
# add a new row if we rounded up
plot.rows <- ifelse(plot.cols >= sqrt(nplots), plot.cols, plot.cols+1)
#plot.cols <- 4
#plot.rows <- 3

#
# test for and create graphics subdirectory
#
if (!file.exists("graphics")) dir.create("graphics")

data <- vector("list")
data.ma <- vector("list")
titles <- vector("list")
compname <- substr(argv[1],regexpr('_',argv[1])+1,nchar(argv[1]))
fileName.base <- substr(compname, 0, regexpr('(_|.csv)', compname)-1)
expnames <- ""
print(fileName.base)
filenum <- 1
for (f in argv) {
  nxtName <- substr(f,0,regexpr('_',f)-1)
  titles[[filenum]] <- nxtName
  fileName.base <- paste(fileName.base,nxtName,sep="-")
  expnames <- paste(expnames,nxtName,sep="-")
  raw <- read.csv(f)
  data[[filenum]] <- raw

  raw[is.na(raw)] <- 0 # replace NAs with zeros?

  ma <- apply(raw[,2:ncol(raw)], 2, ma.cent, n=301)
  ma <- cbind(raw[,1], ma)
  colnames(ma)[1] <- colnames(raw)[1]
  data.ma[[filenum]] <- as.data.frame(ma)

  filenum <- filenum+1
}

## assume all Time vectors are the same
## only plot columns from the 1st file, and that exist in all other files

columns <- colnames(data[[1]])
column.1 <- columns[1]
max.1 <- max(data[[1]][column.1])

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
    if (!is.element(column,colnames(df))) skip <- TRUE
    else {
      min.2 <- min(min.2, min(df[column], na.rm=TRUE), na.rm=TRUE)
      max.2 <- max(max.2, max(df[column], na.rm=TRUE), na.rm=TRUE)
    }
  }
  if (skip) next  # skip columns that don't exist in all files

  print(paste("Working on",column,"..."))
  fileName <- paste("graphics/", fileName.base, "-", column, 
  ifelse(plot.data, "-wd", ""), expnames, ".png", sep="")
   png(fileName, width=1600, height=1600)
   # set margins and title, axis, and label font sizes
   par(mar=c(5,6,4,2), cex.main=2, cex.axis=2, cex.lab=2)
   par(mfrow=c(plot.rows,plot.cols))


   # plot this column from all data sets
   ndx <- 1
   for (df in data.ma) {
       attach(df)
       ma <- cbind(get(column.1), get(column))
       detach(df)
       colnames(ma) <- c(column.1, column)
       plot(ma, main=titles[[ndx]], xlim=c(0,max.1), ylim=c(min.2,max.2))
       ## if we're plotting original data, use points()
       if (plot.data) {
          attach(data[[ndx]])
          dat <- cbind(get(column.1), get(column))
          detach(data[[ndx]])
          points(dat[,1],dat[,2],pch="·")
       }

       grid()
       ndx <- ndx+1
   }
}

#q()
