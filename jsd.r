#! /usr/bin/Rscript

##
# Calculate the Jensen-Shannon Divergences between two experiments.
#
# Time-stamp: <2017-11-10 11:49:36 gepr>
##

TYPE <- c("body","extra","extRatio","hsolute-dCV∈[0,100)")

argv <- commandArgs(TRUE)

if (length(argv) < 2) {
  print("Usage: jsd.r <exp1> <exp2>")
  print("  e.g. jsd.r ntt003x ntt004x")
  quit()
}

exp1 <- argv[1]
exp2 <- argv[2]

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

## loop over TYPE
for (t in TYPE) {
  e1d <- read.csv(paste(exp1,"_",t,".csv",sep=""))
  e2d <- read.csv(paste(exp2,"_",t,".csv",sep=""))
  scols <- intersect(colnames(e1d),colnames(e2d))
  scols <- scols[scols != "Time"] ## remove time
  ## loop over shared columns
  for (s in scols) {
    jsd <- getjsd(e1d[,s],e2d[,s])
    if (exists("sims")) sims <- rbind(sims,c(s,jsd))
    else sims <- c(s,jsd)
  }
  colnames(sims)[1] <- paste(t,"series")
  fn <- paste("jsd-",exp1,"-vs-",exp2,"_",t,".csv",sep="")
  write.csv(sims,fn,row.names=F,quote=F)
  rm(sims)
}

