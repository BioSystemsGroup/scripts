#########################################
## Define the following functions:
## ma.cent, ma.left: moving averages centered and left
## minor.tick: add minor tickmarks to a plot
## maxdPV: get the maximum dPV values from hsolute files
## snd: calculate and write sums and divisors for hsolutes
## tot: use sums and divisors files to calculate totals (averages)
## insertRow: inserts a row of zeros into a data.frame
## pad1stcolumns: pads the 1st DF with new columns from the 2nd
##
## Time-stamp: <2017-02-08 09:15:37 gepr>
#########################################

#########################################
## centered moving average
require(stats)
ma.cent <- function(x, n=5) {
  if (n%%2 == 0) {
     print("A centered moving average should use an odd window.")
     q("no")
  }
  filter(x,rep(1/n,n), sides=2)
}
#########################################
## leftward moving average
ma.left <- function(x, n=5) {
    filter(x,rep(1/n,n), sides=1)
}

#########################################
## place minor tickmarks on a plot
##
## stolen from: https://github.com/harrelfe/Hmisc/blob/master/R/minor.tick.s
minor.tick <- function (nx = 2, ny = 2, tick.ratio = 0.5, x.args = list(), y.args = list()) {
  ax <- function(w, n, tick.ratio, add.args) {
    range <- par("usr")[if (w == "x") 1 : 2  else 3 : 4]
    tick.pos <- if (w == "x") par("xaxp") else par("yaxp")
    distance.between.minor <- (tick.pos[2] - tick.pos[1])/tick.pos[3]/n
    possible.minors <- tick.pos[1] - (0 : 100) * distance.between.minor
    low.candidates <- possible.minors >= range[1]
    low.minor <- if (any(low.candidates))
                   min(possible.minors[low.candidates])
                 else
                   tick.pos[1]
    possible.minors <- tick.pos[2] + (0 : 100) * distance.between.minor
    hi.candidates <- possible.minors <= range[2]
    hi.minor <- if (any(hi.candidates))
                  max(possible.minors[hi.candidates])
                else
                  tick.pos[2]
    axis.args <- c(list(if (w == "x") 1 else 2,
                        seq(low.minor, hi.minor, by = distance.between.minor),
                        labels = FALSE, tcl = par("tcl") * tick.ratio),
                        add.args);
	do.call(axis, axis.args);
    }
  if (nx > 1)
    ax("x", nx, tick.ratio = tick.ratio, x.args)
  if (ny > 1)
    ax("y", ny, tick.ratio = tick.ratio, y.args)
  invisible()
}
##

###
##  Declare a couple of globals
###
inFileRoot <- "hsolute_zone_"
outFileRoot <- "dPV.hsol"

#########################################
## Calculate the maximum dPV in all files in all experiments.
##
maxdPV <- function(exps) {
  total.maxdPV <- -1
  for (expDir in exps) {
    exp.maxdPV <- -1
    files <- list.files(path = expDir, pattern=inFileRoot)
    for (file in files) {
      file.maxdPV <- -1
      cols <- colnames(read.csv(paste(expDir,file,sep="/"), nrows=1))
      cols <- cols[2:length(cols)]
      for (column in cols) {
         dPV <- unlist(strsplit(column,"[.]"))[4] # dPV is the 4th entry in the column name
         dPV <- as.numeric(dPV)
         file.maxdPV <- max(file.maxdPV, dPV)
      }
      print(paste(file,": max(dPV) = ",file.maxdPV,sep=""))
      exp.maxdPV <- max(exp.maxdPV, file.maxdPV)
    }
    print(paste(expDir,": max(dPV) = ",exp.maxdPV,sep=""))
    total.maxdPV <- max(total.maxdPV, exp.maxdPV)
  }
  print(paste("max(dPV) over all experiments =",total.maxdPV))
  total.maxdPV
}
##

#########################################
## calc and write sums and divisors files
###
snd <- function(band, exps) {
  dPVMin <- band[1]
  dPVMax <- band[2]

  print(paste("Sums and divisors for dPV∈[",dPVMin,",",dPVMax,")", sep=""))

for (expDir in exps) {
    files <- list.files(path = expDir, pattern=inFileRoot)
    cellNum <- data.frame(character(),numeric(), stringsAsFactors=FALSE) # num columns per file so that averages can be calculated in the next round
    cellNum <- cellNum[2,]
    fileNdx <- 1 # needed to build cellNum as a data.frame with stringsAsFactors=FALSE (can't use rbind())
    for (file in files) {
        print(paste("Calculating Sums and Divisors for ",file))
        datasetname <- sub(".csv","",sub(inFileRoot,"",file))
        dat <- data.frame()
        fileDat <- read.csv(paste(expDir,file,sep="/"))
        # extract the relevant data from this file and cbind it
        nPVNdx <- 1
        nPVcolumns <- vector() # list of columns where dPV ∈ [dPVMin, dPVMax)
        column.names <- colnames(fileDat[2:ncol(fileDat)])

        for (cname in column.names) {
            dPV <- unlist(strsplit(cname,"[.]"))[4] # dPV is the 4th thing in the column name
            dPV <- as.numeric(dPV)
##            highest.dPV <- max(highest.dPV, dPV)
            if ((dPVMin <= dPV) && (dPV < dPVMax)) {
                nPVcolumns <- append(nPVcolumns, nPVNdx+1)  # Add 1 to skip the first column
            }
            nPVNdx <- nPVNdx + 1
        }

        # if there are no columns ∈ [dPVMin,dPVMax] go to next file
        if (length(nPVcolumns) <= 0) next

        # use time column as initial data.frame
        if (nrow(dat) <= 0) dat <- fileDat[1]

        # use the nPVcolumns to slice out the right columns
        dat <- cbind(dat, fileDat[nPVcolumns])
#stop()
        rm(fileDat)  # attempt to keep a small memory footprint

        # parse out hsol products
        hsolnames <- list()
        for (cname in colnames(dat[2:ncol(dat)])) {
            hsolName <- unlist(strsplit(cname,"[.]"))[6] # hsolprod name is the 6th element
            if (is.na(hsolName)) {
              print("Could not parse reaction product name from column header.")
              q()
            }
            hsolnames[cname] <- hsolName
        }
        hsolnames <- unique(hsolnames)

        hsolSum <- dat[1] # start with Time column
        for (hsolName in hsolnames) {
            hsolDat <- dat[,grep(paste("[.]",hsolName,"$",sep=""),names(dat))]
            # finally get the means for each time
            if (is.vector(hsolDat)) hsolDat <- as.data.frame(hsolDat) # defensive
            hsolSum <- cbind(hsolSum, rowSums(hsolDat))
        }
        colnames(hsolSum) <- c("Time",hsolnames)
        sumname <- paste(outFileRoot, "/", expDir, "_hsolute_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-", datasetname, "-sum.csv", sep="")
        write.csv(hsolSum, sumname, row.names=FALSE)
        cellNum[fileNdx,] <- c(datasetname,length(nPVcolumns)) # divisor for Sums to get averages
        fileNdx <- fileNdx + 1

        rm(dat)  # attempt to keep small memory footprint
        gc()
    } ## end for (file in files) {
    colnames(cellNum) <- c("trialfile", "#columns")

    divname <- paste(outFileRoot, "/", expDir,"_hsolute_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-divisors.csv", sep="")
    write.csv(cellNum, divname, row.names=FALSE)

  } ## end for (expDir in exps) {

} ## end snd()
##

#########################################
## Read the sums and divisors and divide
###
tot <- function(band, expName) {
  dPVMin <- band[1]
  dPVMax <- band[2]

  print(paste("Calculating totals for",expName))

  ## get the file names
  sndFileRoot <- paste(expName,"_hsolute_dPV∈\\[",dPVMin,",",dPVMax,"]",sep="")
  files <- list.files(path=outFileRoot, pattern=sndFileRoot, full.names=TRUE)
  ## ensure that the files start with the expName
  files <- files[grep(paste(outFileRoot,expName,sep="/"), files)]

  sumfiles <- files[grep("-sum.csv",files)]
  divfile <- files[grep("-divisors.csv",files)]

  ## extract and sum each hsol product from each data set and divide by the sum of the column numbers
  ## i.e. (∑hsolᵢ)/∑colsⱼ, where i ∈ {NecInhib, S, G, Marker, ...} and j ∈ {1_2-????, 3-????}

  for (sumfile in sumfiles) {
    fileDat <- read.csv(sumfile)
    if (!exists("dat")) dat <- fileDat[1]
    dat <- cbind(dat,fileDat[2:ncol(fileDat)])
  }

  hsolnames <- unique(colnames(dat))
  hsolnames <- hsolnames[2:length(hsolnames)]

  ## sum the sums per unique reaction product
  for (hsol in hsolnames) {
    hsolDat <- dat[,grep(paste("^",hsol,"$",sep=""),names(dat))]
    hsolDat <- as.matrix(hsolDat)
    if (!exists("hsolSum"))
      hsolSum <- rowSums(hsolDat)
    else
      hsolSum <- cbind(hsolSum, rowSums(hsolDat))
  }
  colnames(hsolSum) <- hsolnames

  ## now read in the total number of columns
  divs <- read.csv(divfile)
  totalcells <- sum(divs[2])/length(hsolnames)  # number of columns divided by number of hsol products

  ## now divide in the total columns (number of cells) to get the avg per cell
  hsolAvg <- cbind(dat[1], hsolSum/totalcells)

  avgname <- sub("-divisors","-totals",divfile)
  write.csv(hsolAvg, avgname, row.names=FALSE)

} ## end tot()
##

###
## inserts newrow into a DF sorted by the 1st element of newrow.
## stolen and modified from:
## http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
###
insertRow <- function(existingDF, newrow) {
  edf1 <- existingDF[,1]
  ## index <- which(abs(edf1-newrow[[1]])==min(abs(edf1-newrow[[1]]))) ## find index of the next row > the value
  index <- which((edf1-newrow[[1]]) > 0) ## find index of the next row > the value

  ##print(index)

  ## if it never goes negative, then append to the end
  if (length(index) == 0 ) existingDF <- rbind(existingDF,newrow)

  if (length(index) >= 1) {
    index <- index[1]
    existingDF[seq(index+1,nrow(existingDF)+1),] <- existingDF[seq(index,nrow(existingDF)),]
    existingDF[index,] <- newrow
  }

  existingDF
}

###
## add columns and rows that exist in operand 2 to operand 1, setting the
## column name as it is in operand 2
###
pad1stColumns <- function(first, second) {
  ## insert columns of NAs that don't exist in first
  matched.columns <- match(colnames(second), colnames(first))
  for (col.index in which(is.na(matched.columns))) {
    newcol <- vector(mode="numeric",length=length(first[,1]))
    ##newcol[] <- NA
    total.names <- colnames(first)
    first <- cbind(first, newcol)
    colnames(first) <- c(total.names, colnames(second)[col.index])
  }
  first
}

###
## Written for the EnzymeGroup data reduction workflow.  Takes the output
## of eg-dPV.r, selects the columns within the band and sums them by EG.
###
sumBandByLastTag <- function(dat, band) {
  dMin <- band[1]
  dMax <- band[2]

  groups <- vector()
  colmatches <- vector()
  for (cn in colnames(dat)[2:ncol(dat)]) {
    splitted <- unlist(strsplit(cn,':'))
    d <- as.numeric(splitted[1])
    g <- splitted[2]
    if (dMin <= d && d < dMax) colmatches <- c(colmatches,cn)
    groups <- c(groups,g)
  }
  groups <- unique(groups)
  inband <- dat[,colmatches]

  for (g in groups) {
    gsum <- inband[,grep(paste(":",g,sep=""),colnames(inband))]
    if (ncol(inband) > length(groups))
      gsum <- rowSums(gsum)
    if (exists("gsums")) gsums <- cbind(gsums,gsum)
    else gsums <- gsum
  }
  gsums <- cbind(dat[,1],gsums)
  colnames(gsums) <- c("Time",groups)

  gsums
}


###
## Written for the EnzymeGroup data reduction workflow.  Called by eg-dPV.r;
## sums all the columns in files in allfiles and divides by |allfiles|.
###
avgByColumn <- function(allfiles) {
  dat <- vector("list")
  groups <- vector()

  pb <- txtProgressBar(min=0,max=length(allfiles),style=3)  ## progress bar
  setTxtProgressBar(pb,0); ## progress bar

  tNdx <- 1
  for (t in allfiles) {
    tdat <- read.csv(t, check.names=F)

    ## get unique groups from 1st file
    if (tNdx == 1) {
      for (cn in colnames(tdat)[2:length(colnames(tdat))]) {
        group <- unlist(strsplit(cn,":"))[3]
        groups <- c(groups,group)
      }
      groups <- unique(groups)
    }

    ## add this trial data to the list of DFs
    ##dat[[tNdx]] <- tdat

    ## merge with total sums
    if (tNdx == 1) {
      time <- tdat[,1]
      tdat <- tdat[2:ncol(tdat)]
      totals <- tdat
    } else {
      ## slice off Time
      totals <- totals[,2:ncol(totals)]
      tdat <- tdat[,2:ncol(tdat)]
      ## pad with any missing columns
      totals <- pad1stColumns(totals, tdat)
      tpad <- pad1stColumns(tdat, totals)
      ## sort both before adding
      totals <- totals[, order(names(totals))]
      tpad <- tpad[, order(names(tpad))]
      totals <- totals[,2:ncol(totals)] + tpad[,2:ncol(tpad)]
    }

    setTxtProgressBar(pb,tNdx); ## progress bar

    tNdx <- tNdx + 1
  } ## for (f in allfiles)

  setTxtProgressBar(pb,tNdx); ## progress bar

  totals <- totals[,2:length(totals)]/length(allfiles)
  totals <- cbind(time,totals)
  colnames(totals)[1] <- "Time"

  close(pb) ## progress bar

  totals
}
