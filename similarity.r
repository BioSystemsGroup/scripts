#! /usr/bin/Rscript
###
## Provides a suite of various distance functions for calculating similarity.
## Derived from Yuanyuan Xiao's implementation of Dev Nag's paper, "Decomposition
## of Similarity Measures for Time Series Analysis", 2004.
##
## Time-stamp: <2019-09-09 15:10:38 gepr>
###
argv <- commandArgs(TRUE)

exp1 <- argv[1]
exp2 <- argv[2]

options(width=135)

###--------------------------------------------------------------------------
##                               peakID
## A function that disects an outflow profile into different segments:
## start, peak, tail1, tail2
## 1) input should be a vector of an outflow profile (evenly spaced in time)
###----------------------------------------------------------------------------
peakID <- function(x, log=TRUE) {
  locateID <- function(s) {
    N <- length(s)
    ID <- vector(mode="character", length=N)
    smax <- max(s)
    i.smax <- which.max(s)
    hmax <- ifelse(log, smax-0.3, smax/2)
    i2.smax <- which.min(abs(s[i.smax:N]-hmax))+i.smax-1
    i1.smax <- which.min(abs(s[1:i.smax]-hmax))
    i3.smax <- i2.smax+ceiling((N-i2.smax)/2)


    ID[1:(i1.smax-1)] <- "start"
    ID[i1.smax:i2.smax] <- "peak"
    ID[(i2.smax+1):i3.smax] <- "tail1"
    ID[(i3.smax+1):N] <- "tail2"
    factor(ID, levels=c("start","peak","tail1","tail2"))
  }
  if(is.vector(x)) RET <- locateID(x)
  else if(is.matrix(x)) RET <- apply(x, 2, locateID)
  else if(is.data.frame(x)) RET <- sapply(x, locateID)
  else RET <- locateID(as.vector(x))

  return(RET)

}

###--------------------------------------------------------------------------------
##                   calcDist.func
## This function implements Dev Nag's semi-linear distance approach
## 1) The user inputs J, alpha and epsilon, see Nag's paper
## 2) The function returns a function with bindings for the above arguments
## 3) The returned function accepts two vectors and computes their distance
## 4) usages: for euclidean distance, suppose "x" and "y" are the two vectors to
##    compute distance
##    J <- 20; alpha <- c(c(0,1),rep(0,18)); epsilon <- rep(1,20)
##    euclid <- calcDist.func(J=J, alpha=alpha, epsilon=epsilon)
##    dist <- euclid(x,y)
###----------------------------------------------------------------------------------

calcDist.func <- function(z.fun="peakID", weights=NULL, J=20,
                          alpha=rep(1,J), epsilon=rep(1,J), ...) {
  function(x,y) {
                                        # sanity check
    if ((data.class(x)!="numeric") || (data.class(y)!="numeric"))
      stop("both x and y need to be numeric vectors...")
    if (length(x)!=length(y))
      stop("size of x need to equal size of y...")
    if (length(alpha)==1) alpha <- rep(alpha, J)
    if (length(epsilon)==1) epsilon <- rep(epsilon, J)
    if ((length(alpha)!=J) || (length(epsilon)!=J))
      stop("both size of alpha and epsilon need to be equal to J...")
    if (is.null(weights)) weights <- rep(1, length(x))
    else weights <- weights*length(x)/sum(weights)

    if (is.character(z.fun)) z<- do.call(z.fun, list(y))
    else z <- as.factor(TRUE)
    good <- !(is.na(y)|is.na(x)|is.infinite(y)|is.infinite(x))

    D <- numeric(length(levels(z)))
    for (i in 1:length(levels(z))) {
      xx <- x[good & (z==levels(z)[i])]
      yy <- y[good & (z==levels(z)[i])]
      wts <- weights[good & (z==levels(z)[i])]
      for (j in 1:J)
        D[i] <- D[i] + alpha[j]*1/length(xx)*sum((wts*abs(xx-yy))^j, na.rm=TRUE)^(epsilon[j]/j)
    }
    if(length(levels(z))>1){
      xx <- x[good]
      yy <- y[good]
      wts <- weights[good]
      d <- 0
      for (j in 1:J)
        d <- d + alpha[j]*1/length(xx)*sum((wts*abs(xx-yy))^j, na.rm=TRUE)^(epsilon[j]/j)
      D <- c(D,d)
      names(D) <- c(levels(z),"total")
    }
    return(D)
  }
}

euclidean = calcDist.func(weights=NULL, J=20, alpha=c(0,1,rep(0,18)), epsilon=rep(1,20))

mse <- function(x, na.rm = FALSE) {
  if (is.matrix(x))
    apply(x, 2, z, function(z) sum(x^2, na.rm=TRUE))
  else if (is.vector(x))
    sum(x^2, na.rm = na.rm)
  else if (is.data.frame(x))
    sapply(x,function(z) sum(x^2, na.rm=TRUE))
  else sum(as.vector(x)^2, na.rm = na.rm)
}

###--------------------------------------------------------------------------------
##                   calcVar.func
## This function calculates variance of the deviance (x-y)
## 1) The argument "func" could be "sd"(default) or "mad"(robust version of sd) or
##    other functions
## 2) The function returns a function with argument bindings
## 3) The returned function accepts two vectors and computes their distance
## 4) usages: f <- calcVar.func(func=mad)
##      dist <- f(x,y)
###----------------------------------------------------------------------------------

calcVar.func <- function(z.fun="peakID", weights=NULL, func=sd) {
  function(x,y) {
    if ((data.class(x)!="numeric") || (data.class(y)!="numeric"))
      stop("both x and y need to be numeric vectors...")
    if (length(x)!=length(y))
      stop("size of x need to equal size of y...")
    if (is.null(weights)) weights <- rep(1, length(x))
    else weights <- weights*length(x)/sum(weights)
    if (is.character(z.fun)) z<- do.call(z.fun, list(y))
    else z <- as.factor(TRUE)

    good <- !(is.na(y)|is.na(x)|is.infinite(y)|is.infinite(x))
    D <- numeric(length(levels(z)))
    for (i in 1:length(levels(z))) {
      xx <- x[good & (z==levels(z)[i])]
      yy <- y[good & (z==levels(z)[i])]
      wts <- weights[good & (z==levels(z)[i])]
      if((length(xx)==0)||(length(yy)==0))
        D[i] <- NA
      else
        D[i] <- func(wts*(xx-yy), na.rm=TRUE)
    }
    if(length(levels(z))>1){
      xx <- x[good]
      yy <- y[good]
      wts <- weights[good]
      D <- c(D, func(wts*(xx-yy),na.rm=TRUE))
    }
    names(D) <- c(levels(z), "total")
    return(D)

  }
}

###--------------------------------------------------------------------------------
##                               Dev Nag's distance measure list
## This list is for input into the function calcSimilarity, specify similarity.funcs=DevsList
## for more information see Table 2 in Dev Nag's paper and the function calcDist.func
###-------------------------------------------------------------------------------------
DevsList <- list(
    constant = calcDist.func(z=NULL, weights=NULL, J=20, alpha=rep(1,20), epsilon=rep(1,20)),
    euclidean = calcDist.func(z=NULL, weights=NULL, J=20, alpha=c(0,1,rep(0,18)), epsilon=rep(1,20)),
    ascend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=((1:20)-1)/20, epsilon=rep(1,20)),
    descend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=1-((1:20)-1)/20, epsilon=rep(1,20)),
    eascend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=(rep(1,20)), epsilon=c(1:20)/20),
    edescend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=(rep(1,20)), epsilon=1-(c(1:20)-1)/20))

###----------------------------------------------------------------------------------------
##                               Another list of similarity functions
## This list is for input into the function calcSimilarity, specify similarity.funcs=smList
## it includes euclidean distance, city block distnace, semi-linear distance, global_sd, global_mad
###-----------------------------------------------------------------------------------------------
smList <- list(
    city = calcDist.func(z="peakID", weights=NULL, J=20, alpha=c(1,rep(0,19)), epsilon=rep(1,20)),
    euclidean = calcDist.func(z="peakID", weights=NULL, J=20, alpha=c(0,1,rep(0,18)), epsilon=rep(1,20)),
    constant = calcDist.func(z="peakID", weights=NULL, J=20, alpha=rep(1,20), epsilon=rep(1,20)),
    sd = calcVar.func(z="peakID", func=sd),
    mad = calcVar.func(z="peakID", func=mad))


###------------------------------------------------------------------------------------------------
##                                 calcSimilarity
## A functions that computes specified similarity measures for an object of class "LOP"
## 1) it accepts an object of class "LOP" and a list of similarity measures
## 2) it returns an object of class "LOPsimilarity"; see the function plot.LOPsimilarity
## 3) if smooth=TRUE, similarity is measured betweent the SMOOTHED model data and the reference;
##    if smooth=FALSE, similarity is meausured between individual runs (specified by index) and the
##    reference
## 4) usage: suppose "X" is an object of class "LOP"
##      smRes <- calcSimilarity(X, smooth=FALSE)
##      plot(smRes)
###-----------------------------------------------------------------------------------------------
calcSimilarity <- function(model, ref, logit=TRUE,
                           similarity.funcs=smList, ...) {
  NSim <- length(similarity.funcs)

  ## match lengths
  if (nrow(ref) > nrow(model)) ref <- ref[1:nrow(model),]
  if (nrow(model) > nrow(ref)) model <- model[1:nrow(ref),]

  if (logit) {
    model[,2:ncol(model)] <- log10(model[,2:ncol(model)])
    ref[,2:ncol(ref)] <- log10(ref[,2:ncol(ref)])
  }

  D <- list()
  for (i in 1:NSim) {
    sim.func <- similarity.funcs[[i]]
    d <- NULL
    ##    for (j in ncol(model)){ # calculates the similarity between specified runs and the reference
    for (colname in colnames(model)) {
      m <- model[,colname]
      r <- ref[,colname]
      d <- cbind(d,sim.func(m, r))
    }
    colnames(d) <- colnames(model)
    D <- c(D,list(d))

  }

  names(D) <- names(similarity.funcs)
  return(D)
}

d1 <- read.csv(paste(exp1,"-reduced/",exp1,"_body-avg.csv",sep=""))
d2 <- read.csv(paste(exp2,"-reduced/",exp2,"_body-avg.csv",sep=""))

calcSimilarity(d1,d2,logit=F,similarity.funcs=smList)
calcSimilarity(d1,d2,logit=F,similarity.funcs=DevsList)
