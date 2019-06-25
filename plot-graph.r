#! /usr/bin/Rscript
###
## Read in a HepStruct adjacency matrix and plot to a PNG file.
##
## Time-stamp: <2019-06-25 16:41:47 gepr>
##
argv <- commandArgs(TRUE)

usage <- function() {
    print("Usage: plot-graph.r <adjacency matrix CSV file>")
    print("  File format has rows as FROM vertices, columns as TO vertices, and ")
    print("  and values as the number of edges FROM → TO.")
    quit()
}

if (length(argv) < 1) usage()

inFileName <- argv[1]
outFileBase <- substr(basename(inFileName), 0, regexpr('.csv', basename(inFileName))-1)
cat(paste("Plotting", outFileBase,"\n"))
outFileName <- paste("graphics/", outFileBase, sep="")

## test for and create graphics subdirectory
if (!file.exists("graphics")) dir.create("graphics")

library(igraph, quietly=T, warn.conflicts=F)
am <- read.csv(inFileName, row.names=1, check.names=F)
am <- as.matrix(am)
g <- graph_from_adjacency_matrix(am,mode="directed")

png(paste(outFileName,".png",sep=""), width=800, height=800)

plot(g,layout=layout_as_tree)
