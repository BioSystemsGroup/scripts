#########################################
## Define the following functions:
## ma.cent, ma.left: moving averages centered and left
## minor.tick: add minor tickmarks to a plot
## maxdPV: get the maximum dPV values from rxnProduct files
## snd: calculate and write sums and divisors for rxnProducts
## tot: use sums and divisors files to calculate totals (averages)
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
inFileRoot <- "rxnProduct_zone_"
outFileRoot <- "dPV.rxn/"

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
        print(paste("Processing",file))
        datasetname <- sub(".csv","",sub(inFileRoot,"",file))
        dat <- data.frame()
        fileDat <- read.csv(paste(expDir,file,sep="/"))
        # extract the relevant data from this file and cbind it
        nPVNdx <- 1
        nPVcolumns <- vector() # list of columns where dPV ∈ [dPVMin, dPVMax)
        for (cname in colnames(fileDat[2:ncol(fileDat)])) {
            dPV <- unlist(strsplit(cname,"[.]"))[4] # dPV is the 4th thing in the column name
            dPV <- as.numeric(dPV)
            highest.dPV <- max(highest.dPV, dPV)
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
        
        # parse out rxn products
        rxnnames <- list()
        for (cname in colnames(dat[2:ncol(dat)])) {
            rxnName <- unlist(strsplit(cname,"[.]"))[6] # rxnprod name is the 6th element
            if (is.na(rxnName)) {
              print("Could not parse reaction product name from column header.")
              q()
            }
            rxnnames[cname] <- rxnName
        }
        rxnnames <- unique(rxnnames)

        rxnSum <- dat[1] # start with Time column
        for (rxnName in rxnnames) {
            rxnDat <- dat[,grep(paste(".",rxnName,"$",sep=""),names(dat))]
            # finally get the means for each time
            if (is.vector(rxnDat)) rxnDat <- as.data.frame(rxnDat) # defensive
            rxnSum <- cbind(rxnSum, rowSums(rxnDat))
        }
        colnames(rxnSum) <- c("Time",rxnnames)
        sumname <- paste(outFileRoot, expDir, "_rxnProduct_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-", datasetname, "-sum.csv", sep="")
        write.csv(rxnSum, sumname, row.names=FALSE)
        cellNum[fileNdx,] <- c(datasetname,length(nPVcolumns)) # divisor for Sums to get averages
        fileNdx <- fileNdx + 1

        rm(dat)  # attempt to keep small memory footprint
        gc()
    } ## end for (file in files) {
    colnames(cellNum) <- c("trialfile", "#columns")

    divname <- paste(outFileRoot, expDir,"_rxnProduct_dPV∈[", as.character(dPVMin), ",", as.character(dPVMax), "]-divisors.csv", sep="")
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

  ## get the file names
  sndFileRoot <- paste(expName,"_rxnProduct_dPV∈\\[",dPVMin,",",dPVMax,"]",sep="")
  files <- list.files(path=outFileRoot, pattern=sndFileRoot, full.names=TRUE)

  sumfiles <- files[grep("-sum.csv",files)]
  divfile <- files[grep("-divisors.csv",files)]

  ## extract and sum each rxn product from each data set and divide by the sum of the column numbers
  ## i.e. (∑rxnᵢ)/∑colsⱼ, where i ∈ {NecInhib, S, G, Marker, ...} and j ∈ {1_2-????, 3-????}

  for (sumfile in sumfiles) {
    fileDat <- read.csv(sumfile)
    if (!exists("dat")) dat <- fileDat[1]
    dat <- cbind(dat,fileDat[2:ncol(fileDat)])
  }

  rxnnames <- unique(colnames(dat))
  rxnnames <- rxnnames[2:length(rxnnames)]

  ## sum the sums per unique reaction product
  for (rxn in rxnnames) {
    rxnDat <- dat[,grep(paste(rxn,"$",sep=""),names(dat))]
    rxnDat <- as.matrix(rxnDat)
    if (!exists("rxnSum"))
      rxnSum <- rowSums(rxnDat)
    else
      rxnSum <- cbind(rxnSum, rowSums(rxnDat))
  }
  colnames(rxnSum) <- rxnnames

  ## now read in the total number of columns
  divs <- read.csv(divfile)
  totalcells <- sum(divs[2])/length(rxnnames)  # number of columns divided by number of rxn products

  ## now divide in the total columns (number of cells) to get the avg per cell
  rxnAvg <- cbind(dat[1], rxnSum/totalcells)

  avgname <- sub("-divisors","-totals",divfile)
  write.csv(rxnAvg, avgname, row.names=FALSE)

} ## end tot()
##
