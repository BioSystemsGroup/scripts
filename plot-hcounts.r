#! /usr/bin/Rscript
##
## Plot the cumulative, total over all trials, Hcounts as function of d[CP]V
##
## Time-stamp: <2019-04-19 09:44:20 gepr>
##
argv <- commandArgs(T)

CUMSUM <- T
BARPLOT <- T
PLOT.SVG <- F

exps <- argv
directions <- c("dCV", "dPV")

#
# test for and create graphics subdirectory
#
if (!file.exists("graphics")) dir.create("graphics")

for (exp in exps) {
  for (direction in directions) {
    contents <- paste(ifelse(CUMSUM,"∫",""), "Σtrial(ΣHcount∕",direction,")", sep="")
    infile <- paste(exp,"_Hcounts-all-", direction, sep="")
    d <- read.csv(paste(exp,"-reduced/",infile, ".csv",sep=""))
    main <- exp
    xlabel <- direction
    ylabel <- contents
    columns <- 2:(ncol(d)-3)
    dt <- t(d[(nrow(d)-2),columns])

    if (CUMSUM) dt <- cumsum(dt)

    outFile <- paste("graphics/", exp, "_", contents, sep="")
    if (PLOT.SVG)
      svg(paste(outFile,".svg",sep=""),10,10)
    else
      png(paste(outFile, ".png", sep=""), 1600, 1600)

    ## set margins and title, axis, and label font sizes
    par(mar=c(5,6,4,2), cex.main=2, cex.axis=2, cex.lab=2)

    if (BARPLOT) {
      ##      dtv <- as.vector(t(dt))
      dtv <- as.vector(dt)
      barplot(dtv, names.arg=columns-2, main=main, xlab=xlabel, ylab=ylabel)
      box()
    } else {
      plot(columns-2, dt, main=main, xlab=xlabel, ylab=ylabel)
    }
    grid()
    dev.off()
  }
}
