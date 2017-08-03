#! /usr/bin/Rscript

##
# Read multiple *.csv files and plot each column.
#
# Time-stamp: <2017-01-19 11:47:15 gepr>
#

argv <- commandArgs(TRUE)

if (length(argv) < 1) {
    print("Usage:	plot-vs-time.r <.csv files>")
    print("		if plotting moving average, then")
    print("			plot-vs-time.r n <.csv files>")
    print("			where n = odd number of data point window")
    print("			for moving average")
    print("  .csv files contain data where the first column is time")
    quit()
}

#print(paste("argv[1] = ", argv[1]))
mavg = FALSE
if (!grepl(".csv", argv[1])) {
	n = as.numeric(argv[1])
	mavg = TRUE
	#print(paste("n = ", n))
}

library(ggplot2)
library(svglite)

#########################################
## define the moving average functions
ma.cent <- function(x,n) {
	  if (n%%2 == 0) {
		 print("A centered moving average should use an odd window.")
		 q("no")
	  }
	  filter(x,rep(1/n,n), sides=2)
}
ma.left <- function(x,n){filter(x,rep(1/n,n), sides=1)}
##
#########################################

#
# test for and create graphics subdirectory
#
if (!file.exists("graphics")) dir.create("graphics")

# loop through data files and write plot to file
if (mavg) {
	datafiles <- argv[2:length(argv)]
	#print(paste("argv[2] = ", argv[2]))
} else {
	datafiles <- argv
}
for (f in datafiles) {
	seps <- gregexpr('/',f)[[1]] # get all the '/' locations
	aftersep <- substr(f,seps[length(seps)]+1,nchar(f)) # get everything after the last '/'
	expname <- substr(aftersep,0,regexpr('_',aftersep)-1)
	compname <- substr(f,regexpr('_',f)+1,nchar(f))
	fileName.base <- paste(expname,substr(compname, 0, regexpr('(-|.csv)', compname)-1),sep='-')
	dat <- read.csv(f)
		
	dat.tmp <- dat
	dat.tmp[is.na(dat.tmp)] <- 0 # replace NAs with zeros
	
	if (mavg) {
		dat.ma <- apply(dat.tmp[,2:ncol(dat.tmp)], 2, ma.cent, n)
		dat.ma <- cbind(dat.tmp[,1], dat.ma)
		dat.ma <- as.data.frame(dat.ma)
		colnames(dat.ma) <- colnames(dat)
	}

	attach(dat)
	for (column in colnames(dat)[2:ncol(dat)]) {
		#png(paste("graphics/",fileName.base,"-", column,".png",sep=""), width=1600, height=1600)
		#svg(paste("graphics/",fileName.base,"-", column,"-3.svg",sep=""), width=10, height=10)
		svg(paste("graphics/",fileName.base,"-", column,".svg",sep=""))
		#par(cex=2, lwd=3, mar=c(5,6,4,2), cex.main=3, cex.axis=2, cex.lab=2)
		
		plot(dat[[1]],pch="·", dat[[column]], xlab="Time", ylab=column)
		#plot(Time,pch="·", dat[[column]], ylab=column)
		
		if (mavg) {
			lines(dat.ma[[column]])
		}

		grid()
		title(expname)
	  }
	detach(dat)
	  
	#png("Callus-GBSame-false.png")
	#svg("graphics/all-on-one.svg")
	#x <- data.matrix(x2)
	#y<- dat[2:ncol(dat)]
	#y <- data.matrix(y)
	# points connected by lines
	#matplot(dat[[1]], y, type = "b", lty = 1, col = 1, pch = ".", xlab = "Time", ylab = "Value")
	#title(colnames(dat))
}

dev.off()
quit()
