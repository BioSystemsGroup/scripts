#! /usr/bin/Rscript

###
## Calculate the Jensen-Shannon Divergences between the (shared) columns
## of 2 csv files.
##
## Time-stamp: <2019-02-26 16:15:05 gepr>
###

argv <- commandArgs(TRUE)

if (length(argv) < 2) {
  print("Usage: jsd.r <file1> <file2>")
  print("  e.g. jsd.r tsa001.rv0x-reduced/tsa001.rv0x_celladj-dCV∈[0,4)-to-tsa001.rv0x_celladj-dCV∈[20,24)-ratio.csv
 tsa002.rv0x-reduced/tsa002.rv0x_celladj-dCV∈[0,4)-to-tsa002.rv0x_celladj-dCV∈[20,24)-ratio.csv")
  quit()
}

file1 <- argv[1]
exp1 <- substr(basename(file1),0,regexpr('_',basename(file1))-1)
file2 <- argv[2]
exp2 <- substr(basename(file2),0,regexpr('_',basename(file2))-1)

###
## Maps a series onto [0,1]
###
normalize <- function(x,r0,r1) {
  old_min <- min(x)
  old_range <- max(x)-old_min
  new_range <- r1-r0
  new <- (x - old_min) / old_range * new_range + r0
  output <- new/sum(new)
}

library(philentropy)

###
## 1) Normalizes the series'.
## 2) Removes the zeroes.
## 3) Runs philentropy.JSD(ts1,ts2).
###
getjsd <- function(ts1, ts2) {
  ts1n <- normalize(ts1,0,1)
  ts1nnnan <- ts1n[!is.nan(ts1n)]
  ts1nnz <- ts1nnnan[ts1nnnan != 0]
  ts2n <- normalize(ts2,0,1)
  ts2nnnan <- ts2n[!is.nan(ts2n)]
  ts2nnz <- ts2nnnan[ts2nnnan != 0]
  if (length(ts1nnz) == 0 || length(ts2nnz) == 0) return(NaN)
  x <- rbind(ts1nnz, ts2nnz)
  JSD(x)
}

d1 <- read.csv(file1, colClasses="numeric")
d2 <- read.csv(file2, colClasses="numeric")
scols <- intersect(colnames(d1),colnames(d2))
scols <- scols[scols != "Time"] ## remove time
## loop over shared columns
for (s in scols) {
  suppressWarnings(
      jsd <- getjsd(d1[,s],d2[,s])
  )
  if (exists("sims")) sims <- rbind(sims,c(s,jsd))
  else sims <- c(s,jsd)
}
colnames(sims)[1] <- "solute"

fn <- paste("jsd(", exp1, ":", exp2, ")", sep="")
colnames(sims)[2] <- fn
write.csv(sims, paste(fn, ".csv", sep=""), row.names=F, quote=F)
rm(sims)

