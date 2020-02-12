#! /usr/bin/Rscript
##
# Read multiple *.csv files and plot each column vs the 1st.
#
# Time-stamp: <2019-07-02 10:54:39 gepr>
#

sample.freq <- 1
plot.svg <- F
ma.window <- 181
use.frames <- F

library(ggplot2)
library(grid)
source("~/R/misc.r")

argv <- commandArgs(TRUE)

if (length(argv) < 3) {
    print("Usage: cmp-plot.r <raw|data|nodata> <analysis .csv file> <analysis .csv file>")
    print("  e.g. cmp-plot.r data x00[1-6]_body.csv y00[1-6]_mobileObject-dCV.csv")
    print("Note that columns must match across all files.")
    quit()
}

data.status <- argv[1]
files <- argv[-1]

## determine # of plot frames or legend panels
nplots <- length(files)
plot.cols <- round(sqrt(nplots))
##plot.cols <- 2
## add a new row if we rounded up
plot.rows <- ifelse(plot.cols >= sqrt(nplots), plot.cols, plot.cols+1)
##plot.rows <- 4

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
    axis.text.size <- 10
    axis.title.size <- 10
    plot.title.size <- 14
    point.size <- 2.5
    legend.point.size <- 5
    legend.text.size <- axis.text.size
  } else {
    png(paste(fileName,".png",sep=""), width=1600, height=1600)
    axis.text.size <- 20
    axis.title.size <- 20
    plot.title.size <- 24
    point.size <- 5
    legend.point.size <- 10
    legend.text.size <- axis.text.size
  }

  ## set margins and title, axis, and label font sizes
  ##   format is c(bottom, left, top, right)
  if (use.frames) {
    ##par(mfrow=c(plot.rows,plot.cols))
    pushViewport(viewport(layout = grid.layout(plot.rows, plot.cols)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    par(mar=c(5,6,4,2))
  } else {
    ## place the bottom margin ???
    bottom.margin <- 10 ##length(titles)/10 + ifelse(plot.svg, -3, 6)
    par(mar=c(bottom.margin, 6,4,2))
  }

  colors = rainbow(filenum, s=0.6, v=0.9)[sample(1:filenum,filenum)] # random colors for that many files

  if (!use.frames) gg <- ggplot() # all series on 1 plot frame
  datnames <- c()
  datltys <- numeric()
  # plot this column from all data sets
  ndx <- 1
  grid.row <- 1
  grid.col <- 1
  for (df in data.ma) {
    if (use.frames) gg <- ggplot()
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
        gg <- gg + geom_point(data=dat, aes_(as.name(column.1), as.name(column), color=titles[[ndx]]), shape='·', size=point.size)
        gg <- gg + ggtitle(mainTitle)
        gg <- gg + xlab(colnames(dat)[1]) + ylab(colnames(dat)[2])
        gg <- gg + xlim(c(0,max.1)) + ylim(c(min.2,max.2))
      } else {
        gg <- gg + geom_point(data=dat, aes_(as.name(column.1), as.name(column), color=titles[[ndx]]), shape='·', size=point.size)
      }
      if (data.status == "data")
        gg <-  gg + geom_line(data=ma, aes_(as.name(column.1), as.name(column), color=titles[[ndx]]))
    } else {
      if (use.frames || ndx == 1) {
        mainTitle <- ifelse(use.frames, titles[[ndx]], fileName.base)
        gg <- gg + geom_line(data=ma, aes_(as.name(column.1), as.name(column), color=titles[[ndx]]))
        gg <- gg + ggtitle(mainTitle)
        gg <- gg + xlab(colnames(ma)[1]) + ylab(colnames(ma)[2])
        gg <- gg + xlim(c(0,max.1)) + ylim(c(min.2,max.2))
      } else {
        gg <- gg + geom_line(data=ma, aes_(as.name(column.1), as.name(column), color=titles[[ndx]]))
      }
    }
    ##minor.tick(nx=5,ny=5)

    ##grid()

    if (use.frames) {
      gg <- gg +
        theme(
            axis.text.x= element_text(color="black",angle=45, size=axis.text.size, vjust=0.5),
            axis.text.y= element_text(color="black", size=axis.text.size, vjust=0.5),
            axis.title.x = element_text(color="black",size=axis.title.size, vjust=0.5),
            axis.title.y = element_text(color="black",size=axis.title.size, vjust=0.5),
            plot.title = element_text(color="black",face="bold",size=plot.title.size, hjust=0.5,vjust=1),
            panel.background = element_blank(),
            panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
            legend.position="none"
            )
      gg <- gg + scale_color_manual(values=rep("black",3))
      suppressWarnings(print(gg, vp = vplayout(grid.row, grid.col)))

      ## increment the grid indices ordered by row
      if (ndx%%plot.rows == 0) {
        grid.row <- 0
        grid.col <- grid.col + 1
      }
      grid.row <- grid.row + 1

    }

    ndx <- ndx+1
  }

  if (!use.frames) {
    gg <- gg +
      theme(
          axis.text.x= element_text(color="black",angle=45, size=axis.text.size, vjust=0.5),
          axis.text.y= element_text(color="black", size=axis.text.size, vjust=0.5),
          axis.title.x = element_text(color="black",size=axis.title.size, vjust=0.5),
          axis.title.y = element_text(color="black",size=axis.title.size, vjust=0.5),
          plot.title = element_text(color="black",face="bold",size=plot.title.size, hjust=0.5,vjust=1),
          panel.background = element_blank(),
          panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
          legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=legend.text.size),
          legend.key = element_rect(fill="white"),
          ##legend.background = element_rect(fill=NA)
          )
    gg <- gg +
      guides(color=guide_legend(ncol=plot.cols+1, byrow=T,
                                override.aes=list(size=legend.point.size)
                                ))
    gg <- gg + scale_color_manual(values=colors)
    suppressWarnings(print(gg))
  }

  setTxtProgressBar(pb,getTxtProgressBar(pb)+1)
}

close(pb)

