#########################################
## Time-stamp: <2019-11-11 19:42:44 gepr>
##
## Define the following functions:
## %!in%: infix operator returns T ∀x∈X such that x∉Y.
## ma.cent, ma.left: moving averages centered and left
## ma.check: if the window is too long, return a shorter odd window
## minor.tick: add minor tickmarks to a plot
## maxdPV: get the maximum dPV values from mobileObjectute files
## snd: calculate and write sums and divisors for mobileObjectutes
## tot: use sums and divisors files to calculate totals (averages)
## insertRow: inserts a row of zeros into a data.frame
## pad1stcolumns: pads the 1st DF with new columns from the 2nd
##
#########################################

#########################################
## infix operator to determine which members of the 1st operand are NOT members of the 2nd operand
## i.e. return T ∀x∈X such that x∉Y.
'%!in%' <- function(x,y)!('%in%'(x,y))

#########################################
## a function to apply is.nan() and is.infinite to data.frames
## e.g.: df[is.nan(df)] <- 0.0
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

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
## Check MA window and, if the given window is too long, return a dynamic odd MA window
ma.check <- function(x, w=5) {
  if (is.null(dim(x))) {
    cat("Error! Argument must have non-zero number of rows.\n")
    return(NaN)
  }
  w.new <- w
  if (nrow(x) < w.new) {
    w.new <- nrow(x)/4
    w.new <- ifelse(w.new%%2 == 0, w.new-1, w.new)
    cat("WARNING! MA Window of",w,"is longer than series. Using window of",w.new,"\n")
  }
  return(w.new)
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
inFileRoot <- "mobileObjectute_zone_"
outFileRoot <- "dPV.mobileObject"

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

        # parse out mobileObject products
        mobileObjectnames <- list()
        for (cname in colnames(dat[2:ncol(dat)])) {
            mobileObjectName <- unlist(strsplit(cname,"[.]"))[6] # mobileObjectprod name is the 6th element
            if (is.na(mobileObjectName)) {
              print("Could not parse reaction product name from column header.")
              q()
            }
            mobileObjectnames[cname] <- mobileObjectName
        }
        mobileObjectnames <- unique(mobileObjectnames)

        mobileObjectSum <- dat[1] # start with Time column
        for (mobileObjectName in mobileObjectnames) {
            mobileObjectDat <- dat[,grep(paste("[.]",mobileObjectName,"$",sep=""),names(dat))]
            # finally get the means for each time
            if (is.vector(mobileObjectDat)) mobileObjectDat <- as.data.frame(mobileObjectDat) # defensive
            mobileObjectSum <- cbind(mobileObjectSum, rowSums(mobileObjectDat))
        }
        colnames(mobileObjectSum) <- c("Time",mobileObjectnames)
        sumname <- paste(outFileRoot, "/", expDir, "_mobileObjectute_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-", datasetname, "-sum.csv", sep="")
        write.csv(mobileObjectSum, sumname, row.names=FALSE)
        cellNum[fileNdx,] <- c(datasetname,length(nPVcolumns)) # divisor for Sums to get averages
        fileNdx <- fileNdx + 1

        rm(dat)  # attempt to keep small memory footprint
        gc()
    } ## end for (file in files) {
    colnames(cellNum) <- c("trialfile", "#columns")

    divname <- paste(outFileRoot, "/", expDir,"_mobileObjectute_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-divisors.csv", sep="")
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
  sndFileRoot <- paste(expName,"_mobileObjectute_dPV∈\\[",dPVMin,",",dPVMax,"]",sep="")
  files <- list.files(path=outFileRoot, pattern=sndFileRoot, full.names=TRUE)
  ## ensure that the files start with the expName
  files <- files[grep(paste(outFileRoot,expName,sep="/"), files)]

  sumfiles <- files[grep("-sum.csv",files)]
  divfile <- files[grep("-divisors.csv",files)]

  ## extract and sum each mobileObject product from each data set and divide by the sum of the column numbers
  ## i.e. (∑mobileObjectᵢ)/∑colsⱼ, where i ∈ {NecInhib, S, G, Marker, ...} and j ∈ {1_2-????, 3-????}

  for (sumfile in sumfiles) {
    fileDat <- read.csv(sumfile)
    if (!exists("dat")) dat <- fileDat[1]
    dat <- cbind(dat,fileDat[2:ncol(fileDat)])
  }

  mobileObjectnames <- unique(colnames(dat))
  mobileObjectnames <- mobileObjectnames[2:length(mobileObjectnames)]

  ## sum the sums per unique reaction product
  for (mobileObject in mobileObjectnames) {
    mobileObjectDat <- dat[,grep(paste("^",mobileObject,"$",sep=""),names(dat))]
    mobileObjectDat <- as.matrix(mobileObjectDat)
    if (!exists("mobileObjectSum"))
      mobileObjectSum <- rowSums(mobileObjectDat)
    else
      mobileObjectSum <- cbind(mobileObjectSum, rowSums(mobileObjectDat))
  }
  colnames(mobileObjectSum) <- mobileObjectnames

  ## now read in the total number of columns
  divs <- read.csv(divfile)
  totalcells <- sum(divs[2])/length(mobileObjectnames)  # number of columns divided by number of mobileObject products

  ## now divide in the total columns (number of cells) to get the avg per cell
  mobileObjectAvg <- cbind(dat[1], mobileObjectSum/totalcells)

  avgname <- sub("-divisors","-totals",divfile)
  write.csv(mobileObjectAvg, avgname, row.names=FALSE)

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
    ## note the "$" in the grep() call delimits the search string (without it, a grep for "G" finds both "G" and "GSH_..."
    gsum <- inband[,grep(paste(":",g,"$",sep=""),colnames(inband))]
    if (ncol(inband) > length(groups))
      gsum <- rowSums(gsum)
    if (exists("gsums")) gsums <- cbind(gsums,gsum)
    else gsums <- gsum
  }

  if (length(gsums) >= 1) {
    gsums <- cbind(dat[,1],gsums)
    colnames(gsums) <- c("Time",groups)
  }
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
      tdat <- tdat[,2:ncol(tdat)]
      ## pad with any missing columns
      totals <- pad1stColumns(totals, tdat)
      tpad <- pad1stColumns(tdat, totals)
      ## sort both before adding
      totals <- totals[, order(names(totals))]
      tpad <- tpad[, order(names(tpad))]
      totals <- totals + tpad
    }

    setTxtProgressBar(pb,tNdx); ## progress bar

    tNdx <- tNdx + 1
  } ## for (f in allfiles)

  setTxtProgressBar(pb,tNdx); ## progress bar

  totals <- totals/length(allfiles)
  totals <- cbind(time,totals)
  colnames(totals)[1] <- "Time"

  close(pb) ## progress bar

  totals
}
