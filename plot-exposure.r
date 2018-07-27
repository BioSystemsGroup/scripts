#! /usr/bin/Rscript

###
## Read multiple *.csv files and plot  the calculated exposure foreach column.
##
###

argv <- commandArgs(T)
plot.svg <- F
require(stats) # for statistics
source("~/R/misc.r")

usage <- function() {
  print("Usage: exposure.r dTime Hcount <CSV file1> <CSV file2> ...")
  print("       dTime = time interval to sample data for exposure")
  print("               default = 50")
  print("       Hcount = number of aHPCs to scale data")
  print("               default = 1000")
  print("  e.g. plot-cols.r 50 1000 exp_celladj-dCV-avg-pHPC-pMC∈[0,8).csv")
  quit()
}

if (length(argv) < 3) usage()

dTime <- as.numeric(argv[1])
Hcount <- as.numeric(argv[2])
datafiles <- argv[-(1:2)] ## all remaining args

if (!file.exists("graphics")) dir.create("graphics")

edata <- vector("list")

for (f in datafiles) {
	print(paste("Working on", f))

	if (!file.exists(f)) {
		print(paste(f,"doesn't exist."))
		next
	}
	
	## parse file name
	seps <- gregexpr('/',f)[[1]] # get all the '/' locations
	aftersep <- substr(f,seps[length(seps)]+1,nchar(f)) # get everything after the last '/'
	expname <- substr(aftersep,0,regexpr('_',aftersep)-1)
	compname <- substr(f,regexpr('_',f)+1,nchar(f))
	fileName.base <- paste(expname,substr(compname, 0, regexpr('.csv', compname)-1),sep='-')
	
	## read data and process
	dat <- read.csv(f)
	dat.time <- dat[,1]
	
	dat.mat <- diff(as.matrix(dat[,2:ncol(dat)]))
	dat.mat <- Hcount*dat.mat
	dat.tmp <- as.data.frame(dat.mat)
	dat.tmp <- cbind(dat.time[-length(dat.time)],dat.mat)
	colnames(dat.tmp) <- colnames(dat)
	
	#dat.tmp <- dat
	dat.tmp[is.na(dat.tmp)] <- 0 # replace NAs with zeros
	dat.ma <- apply(dat.tmp[,2:ncol(dat.tmp)], 2, ma.cent, n=dTime)
	dat.ma <- cbind(dat.tmp[,1], dat.ma)
	dat.ma <- as.data.frame(dat.ma)
	colnames(dat.ma) <- colnames(dat)

	attach(dat.ma)
	for (column in colnames(dat)[2:ncol(dat)]) {
		fileName <- paste("graphics/", fileName.base, "-", column, "-exposure",sep="")
		if (plot.svg) {
		svg(paste(fileName,".svg",sep=""), width=10, height=10)
		} else {
		png(paste(fileName,".png",sep=""), width=1600, height=1600)
		}

		par(cex=2, lwd=3, mar=c(5,6,4,2), cex.main=1, cex.axis=1, cex.lab=1)

		plot(Time, dat.ma[[column]], ylab=column, type="l")
		#if (plot.data) points(dat[[column]], pch="·")

		grid()
		minor.tick(nx=5, ny=5, tick.ratio=0.5)

		title(fileName.base)
	}
	detach(dat.ma)
}

quit()
