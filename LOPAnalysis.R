###------------------------------------------------------------------------------
# LOP: Liver Outflow Profile class
# methods for this class include:
# plot, smoothing, points, scattermatrix, profile.test, tail.test, calcSimilarity
###------------------------------------------------------------------------------
setClass("LOP", representation("list"))

###------------------------------------------------------------------------------
# LOPsimilarity: list of similarity results, used for plotting
# methods for this class include:
# plot
###------------------------------------------------------------------------------
setClass("LOPsimilarity", representation("list"))

###------------------------------------------------------------------------------
#                         read.LOP
# function to read liver outflow profile files into R
# 1)the files should be in the .csv format, although this function could be easily
#   modified to accommodate other types;
# 2)model data (output from the agent models) are expected to be in separate files
#   for each run
# 3)reference data are expected to be held in one file
# 4)this function returns an object of class "LOP"
##--------------------------------------------------------------------------------
read.LOP <-
  function(run.fnames, run.pattern="run",
           ref.fname="reference.csv", ref.pattern="reference",
           path=".",
           ...)
  {
    if(missing(run.fnames))    fnames <- dir(path, pattern=paste("*",run.pattern,"*",sep=""))
    nfiles <- length(fnames)
    if(nfiles == 0) stop("Cannot find any model data files...")

    if(is.null(path))
      fullfnames <- fnames
    else
      fullfnames <- file.path(path,fnames)

    # reading in model data
    for(i in 1:nfiles) {
      cat(paste("Reading", fullfnames[i]), "\t")
      if(i==1) Model <- as.matrix(read.csv(fullfnames[i],...))
      else Model <- cbind(Model, read.csv(fullfnames[i],...)[,2])
      if (i %% 2 == 0) cat("\n")
    }
    cat("\n")

    Model <- as.matrix(Model)
    N <- dim(Model)[[2]]-1
    colnames(Model) <- c("Seconds", paste("Run", sep="", c(1:N)))

    # reading in reference data
    if(missing(ref.fname))    ref.fname <- dir(path, pattern=paste("*",ref.pattern,"*",sep=""))
    nfiles <- length(ref.fname)
    if(nfiles == 0) stop("Cannot find any reference data files...")
    if(nfiles > 1) stop("Please consolidate reference data into one file...")
    if(is.null(path))
      ref.fname <- ref.fname
    else
      ref.fname <- file.path(path,ref.fname)
    print(paste("Reading", ref.fname))
    Reference <- as.matrix(read.csv(ref.fname,...))
    NRef <- dim(Reference)[[2]]-1

    Dat <- list(Model=Model, Reference=Reference, NRun=N, NRef=NRef)
    Dat <- new("LOP", unclass(Dat))

    return(Dat)
  }

###--------------------------------------------------------------------------
#                               approxLOP
# A function that interpolates reference data points so that they are same in length
# with the model data
# 1) input should be an object of class "LOP"
###----------------------------------------------------------------------------
approxLOP <- function(x, method="linear",...) {
  if (data.class(x) != "LOP") stop("Please input an object of class LOP...")
  R <- x$Reference
  Time <- x$Model[,1]
  Reference <- apply(R, 2, function(z) { approx(R[,1], z, method=method, xout=Time)$y })
  x$Reference <- Reference

  return(x)
}

###--------------------------------------------------------------------------
#                               peakID
# A function that disects an outflow profile into different segments:
# start, peak, tail1, tail2
# 1) input should be a vector of an outflow profile (evenly spaced in time)
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

###--------------------------------------------------------------------------
#                               plot.LOP
# A plot function for the LOP class
# 1) input should be an object of the "LOP" class
# 2) usage, suppose "X" is of class "LOP", do: plot(X)
# 3) the function plots a scatter plot of fraction vs. time
# 4) if smooth=TRUE, it also draws a smoothed (spline) line summarizing all model runs.
# 5) if draw.ref=TRUE, it also draws all reference lines for comparisons
###----------------------------------------------------------------------------
plot.LOP <- function(obj, xlab="Second", ylab="Fraction",
                      logit=TRUE, pch=20, col="gray",
                      smooth=TRUE, draw.ref=TRUE,
                      ...) {
  NRun <- obj$NRun
  NRef <- obj$NRef
  Model <- obj$Model
  Reference <- obj$Reference
  if (logit) {
    Model[,2:(NRun+1)] <- log10(Model[,2:(NRun+1)])
    Reference[,2:(NRef+1)] <- log10(Reference[,2:(NRef+1)])
  }
  # plot model data
  x <- rep(Model[,1],NRun)
  y <- Model[,2:(NRun+1)]
  good <- !(is.na(x)|is.na(y)|is.infinite(x)|is.infinite(y))
  if (logit & !length(grep("log",ylab,ignore=T))) ylab=paste("log(",ylab,")",sep="")
  plot(x[good], y[good], xlab=xlab, ylab=ylab, pch=pch, col=col, ...)
  if (smooth){
    s <- smooth.spline(x=x[good],y=y[good])
    points(s$x, s$y, col=2, pch=19)
  }

  if (draw.ref) {
     x1 <- Reference[,1]
     y1 <- Reference[,2:(NRef+1)]
     for (i in 1:NRef)
       lines(x1, y1[,i], col=2+i,lwd=2)
     lines(x1,rowMeans(y1,na.rm=T), col=2, lwd=2,lty=2)
  }

  pos <- c(range(x[good])[1]+2*diff(range(x[good]))/3, range(y[good])[2]-1*diff(range(y[good]))/5)
  if (smooth) legend(pos[1], pos[2], "smoothed model", col=2, pch=19, bty="n")
  pos <- c(range(x[good])[1]+2*diff(range(x[good]))/3, range(y[good])[2]-1*diff(range(y[good]))/4)
  if (draw.ref) legend(pos[1], pos[2], paste("Ref ", c(1:NRef, "Mean")), col=c(3:(2+NRef),2), lty=c(rep(1,NRef),2), bty="n")

}

###-------------------------------------------------------------------------------
#                               smoothing.LOP
# A smooth function for the class "LOP"
# 1) usage: suppose "X" is of class "LOP", do smoothing(X)
# 2) smooth.func -- the type of smoothing to be chosen from
# 3) the function returns the fitted smooth model
###---------------------------------------------------------------------------------
if(!isGeneric("smoothing")) setGeneric("smoothing", function(object, ...)
                                    standardGeneric("smoothing"))
setMethod("smoothing", "LOP", function(object, logit=TRUE,
                                        smooth.func=c("smooth.spline", "ksmooth", "supsmu", "lowess"),
                                        ...){
  smooth.fun <- match.arg(smooth.func)
  dots <- list(...)
  NRun <- object$NRun
  Model <- object$Model
  if (logit)
    Model[,2:(NRun+1)] <- log10(Model[,2:(NRun+1)])

  x <- rep(Model[,1],NRun)
  y <- Model[,2:(NRun+1)]
  good <- !(is.na(x)|is.na(y)|is.infinite(x)|is.infinite(y))
  smooth.args <- dots[intersect(names(formals(smooth.fun)), names(dots))]
  s <- do.call(smooth.fun, c(list(x=x[good],y=y[good]), smooth.args))
  return(s)

})

###--------------------------------------------------------------------------
#                               points.LOP
# A function to add points on a existing plot for the LOP class
# 1) input should be an object of the "LOP" class
# 2) usage, suppose "X" is of class "LOP", do: plot(X, smooth=FLASE)
#    points(X, col="red", smooth.func="smooth.spline")
#    points(X, col="green", smooth.func="ksmooth")
# 3) the function adds points of a smoothed line to the current plot
# 4) smooth.func -- the type of smoothers to be chosen from
###----------------------------------------------------------------------------
points.LOP <- function(obj, logit=F, pch=19, col=2,
                      smooth.func=c("smooth.spline", "ksmooth", "supsmu", "lowess"),
                      ...) {
  smooth.fun <- match.arg(smooth.func)
  dots <- list(...)
  NRun <- obj$NRun
  Model <- obj$Model
  if (logit) Model[,2:(NRun+1)] <- log10(Model[,2:(NRun+1)])
  x <- rep(Model[,1],NRun)
  y <- Model[,2:(NRun+1)]
  good <- !(is.na(x)|is.na(y)|is.infinite(x)|is.infinite(y))
  smooth.args <- dots[intersect(names(formals(smooth.fun)), names(dots))]
  s <- do.call(smooth.fun, c(list(x=x[good],y=y[good]), smooth.args))
  points(s$x, s$y, col=col, pch=pch)

}
###--------------------------------------------------------------------------
#                               scattermatrix.LOP
# A function that produces scatterplot matrix for the LOP class
# 1) input should be an object of the "LOP" class
# 2) usage, suppose "X" is of class "LOP", do: scattermatrix(X)
# 3) the function produces a scatter plot (fraction vs. time) matrix of specified
#    numbers of rows and columns
# 4) if for example, index=1:9, ncolumn=3, it creates scatter plots for the first
#    nine runs and arranges them in a 3X3 matrix
# 5) sm.func -- similarity measure to be specified, results will be printed on the plots
###----------------------------------------------------------------------------
if(!isGeneric("scattermatrix"))
  setGeneric("scattermatrix", function(obj, ...)
             standardGeneric("scattermatrix"))

setMethod("scattermatrix", "LOP",
          function(obj, index=NULL, xlab="Second", ylab="Fraction",
                   main="Liver Outflow Profile From Individual Model Run",
                   logit=TRUE, interpolate=TRUE, ncolumn=3, pch=20,
                   sm.func=euclidean, ...) {
  NRun <- obj$NRun
  NRef <- obj$NRef
  if (is.null(index)) index <- c(1:NRun)
  if(interpolate) obj <- approxLOP(obj)
  Ref <- obj$Reference
  Model <- obj$Model
  if (logit) {
    Model[,2:(NRun+1)] <- log10(Model[,2:(NRun+1)])
    Ref[,2:(NRef+1)] <- log10(Ref[,2:(NRef+1)])
  }
  R <- rowMeans(Ref[, 2:(NRef+1)], na.rm=TRUE)
  sd <- apply(Ref[,2:(NRef+1)],1,sd, na.rm=T)

  if(length(index) > ncolumn)
    par(mfrow=c(ceiling(length(index)/ncolumn), ncolumn), mar=rep(2,4), oma=c(4,4,4,2))
  else
    par(mfrow=c(1, length(index)), mar=rep(2,4), oma=c(3,3,4,2))
  for (i in index) {
    m <- Model[, i+1]
    good <- !(is.na(m)|is.na(R)|is.infinite(m)|is.infinite(R))
    Rs <- R[good]
    sds <- sd[good]
    ms <- m[good]
    ylim <- c(min(c(Rs-sds, ms), na.rm=TRUE), max(c(Rs+sds, ms), na.rm=TRUE))
    #cat(ylim)
    plot(Model[,1], m, ylim=ylim, xlab="", ylab="", type="l",col=2)
    lines(Ref[,1], R, col=4)
    lines(Ref[,1], R+sd, lty=3)
    lines(Ref[,1], R-sd, lty=3)
    if(is.function(sm.func)) {
      d <- sm.func(m,R)
      legend(range(Ref[,1])[1]+diff(range(Ref[,1]))*1/3,ylim[2],paste(names(d),round(d,3),sep=":"),bty="n")
    }
  }
  mtext(xlab,1,line=2,outer=TRUE)
  mtext(if (logit & !length(grep("log",ylab,ignore=T))) paste("log(",ylab,")",sep="")
        else ylab, 2, line=2, outer=TRUE)
  mtext(main,3,line=1,outer=TRUE, cex=1.2)



})

###---------------------------------------------------------------------------
#                                   tail.test
# A function that tests if the slop of the model tail is different from the reference tail
# 1) it accepts an object of class "LOP")
# 2) usage: suppose "X" is of class "LOP", do tail.test(X)
# 3) it regresses the model tail on the reference tail and tests if the slope of the regression
#    line is equal to zero; if zero, model tail = reference tail
###----------------------------------------------------------------------------

if(!isGeneric("tail.test")) setGeneric("tail.test", function(object, ...)
                                    standardGeneric("tail.test"))
setMethod("tail.test", "LOP", function(object, logit=TRUE, interpolate=TRUE,
                                        smooth.func=c("smooth.spline", "ksmooth", "supsmu", "lowess"),
                                        ...){
  smooth.fun <- match.arg(smooth.func)
  dots <- list(...)
  NRun <- object$NRun
  NRef <- object$NRef
  if(interpolate) object <- approxLOP(object)
  Ref <- object$Reference
  if (logit) Ref[,2:(NRef+1)] <- log10(Ref[,2:(NRef+1)])

  s <- smoothing(object, logit=logit, smooth.func=smooth.fun, ...)
  pred.s <- predict(s, Ref[,1])
  if (data.class(pred.s)=="list") ym <- pred.s$y
  else ym <- pred.s
  yr <- rowMeans(Ref[,2:(NRef+1)], na.rm=T)
  good <- !(is.na(ym)|is.na(yr)|is.infinite(ym)|is.infinite(yr))
  ym <- ym[good]
  yr <- yr[good]
  o <- min(which(ym==max(ym,na.rm=T), which(yr==max(yr,na.rm=T))))
  plot(yr[o:length(ym)], ym[o:length(ym)], pch=19, xlab="Reference (mean)", ylab="Model", main="Model vs. Reference Tail Comparison")

  fit <- lm(ym[o:length(ym)]~yr[o:length(ym)])# regress model tail on reference tail
  coef <- summary(fit)$coef
  coef[2,3] <- (coef[2,1]-1)/coef[2,2] # t for testing b=1
  coef[2,4] <- (1-pt(abs(coef[2,3]),df=fit$df))*2 # p for testing b=1
  rownames(coef) <- c("Intercept", "Slope (H0: b=1)")
  abline(fit, col="blue",lwd=2)
  abline(a=0,b=1, lwd=2.5, col="gray", lty=2)
  pos <- c(range(yr)[1], range(ym)[2]-1*diff(range(ym))/10)
  legend(pos[1], pos[2], c("Model Tail ~ Reference Tail", "Identity line, b=1, a=0"), lty=c(1,2),
         col=c("blue","gray"), lwd=2, bty="n")

  return(coef)

})

###-----------------------------------------------------------------------------------
#                                profile.test
# A function that tests if the general profiles of model and reference are the same
# 1) it accepts an object of class "LOP"
# 2) usage: suppose "X" is an object of "LOP", do profile.test(X)
#)3) the function produces a plot that draws the mean reference outflow profile with error bars
# 4) model data are smoothed according to the smoother the user specified for "smooth.func"
# 5) similarity measure ("sm.func") specified by the user is used to calculate the distance between
#    smoothed model and the mean reference lines, the result in printed on the plot
# 6) the function returns a table that shows the number and percentage of points from the model
#    data stratified by segments that fall into the reference error envelop
###------------------------------------------------------------------------------------
if(!isGeneric("profile.test")) setGeneric("profile.test", function(object, ...)
                                    standardGeneric("profile.test"))
setMethod("profile.test", "LOP", function(object, logit=TRUE, interpolate=TRUE,
                                        smooth.func=c("smooth.spline", "ksmooth", "supsmu", "lowess"),
                                          sm.func=euclidean, ...){
  smooth.fun <- match.arg(smooth.func)
  dots <- list(...)
  NRun <- object$NRun
  NRef <- object$NRef
  if (interpolate) object <- approxLOP(object) # interpolate the reference
  Ref <- object$Reference
  if (logit)
    Ref[,2:(NRef+1)] <- log10(Ref[,2:(NRef+1)])

  s <- smoothing(object, logit=logit, smooth.func=smooth.fun, ...) # smooth the model
  pred.s <- predict(s, Ref[,1])
  if (data.class(pred.s)=="list") ym <- pred.s$y
  else ym <- pred.s
  yr <- rowMeans(Ref[,2:(NRef+1)], na.rm=T)
  yrsd <- apply(Ref[,2:(NRef+1)],1,sd, na.rm=T)
  good <- !(is.na(ym)|is.na(yr)|is.na(yrsd)|is.infinite(ym)|is.infinite(yr))
  yms <- ym[good]
  yrs <- yr[good]
  yrsds <- yrsd[good]
  ylab <- ifelse(logit, "Fraction", "log(Fraction)")
  if(!interpolate) {
    # plots reference data with error bars
    error.bar(Ref[good,1], yrs, yrsds, gap=FALSE, pch=19, cex=1.3, col=1, xlab="Second", ylab=ylab)
    points(Ref[good,1], yms, pch=19, col=2, cex=1.3)
  }
  else {
    plot(Ref[good,1], yrs, pch=19, xlab="Second", ylab=ylab)
    points(Ref[good,1], yms, pch=19, col=2)
    segments(Ref[good,1],yrs-yrsds, Ref[good,1],yrs+yrsds)

  }
  if(is.function(sm.func))
    dis <- sm.func(ym, yr)

  pos <- c(range(Ref[good,1])[1]+1*diff(range(Ref[good,1]))/2, range(yrs)[2]-1*diff(range(yrs))/8)
  legend(pos[1], pos[2], c("Reference", "Smoothed Model"), pch=19, col=c(1,2), bty="n")
  legend(pos[1], range(yrs)[2]-1*diff(range(yrs))/4,paste(names(dis),round(dis,3),sep=":"),bty="n")
  d <- peakID(yr)
  RET <- NULL

  for (i in levels(d)) {
     yrs <- yr[good & (d==i)]
     yms <- ym[good & (d==i)]
     yrsds <- yrsd[good & (d==i)]
     RET <- cbind(RET, c(sum((yms>=yrs-yrsds) & (yms<=yrs+yrsds)), mean((yms>=yrs-yrsds) & (yms<=yrs+yrsds))))
     abline(v=Ref[which(d==i)[1],1], col="blue", lwd=1,lty=2)
     text(median(Ref[d==i&good,1]), min(yr[good])+0.2, i, cex=0.8)
  }
  RET <- cbind(RET, c(sum(RET[1,]), sum(RET[1,])/sum(good)))
  colnames(RET) <- c(levels(d), "total")
  rownames(RET) <- c("no.IN", "%IN")
  return(RET)

})

###--------------------------------------------------------------------------
#                                       plot.LOPsimilarity
# A function that visually displays the similarity results
# 1) it accepts a function of class "LOPsimilarity"
# 2) usage: suppose X is of class "LOPsimilarity", do plot(X)
# 3) see also the function calcSimilarity in the distance.R file
# 4) if plot.by=="SM", the figure is produced based on similarity measures
# 5) if plot.by=="LOP", the figure is produced based on outflow profile segments
###-----------------------------------------------------------------------------
plot.LOPsimilarity <- function(object, index=2, ncolumn=2, plot.by=c("SM","LOP")) {
  plot.by <- match.arg(plot.by)
  if (plot.by=="SM") {
    x <- object[[index]]
    if(is.vector(x)) x <- matrix(x, nc=length(x))
    if(is.character(index)) index <- match(index, names(object))
    main <- names(object)[index]
    ylabs <- rownames(x)
  }
  if (plot.by=="LOP"){
    x <- NULL
    if(is.character(index)) index <- match(index, rownames(object[[1]]))
    for (i in 1:length(object))
      x <- rbind(x, object[[i]][index,])
    main <- rownames(object[[1]])[index]
    ylabs <- names(object)
  }

  bad <- which(apply(x,1,function(z) sum(is.na(z))>=length(z)/2))
  if(length(bad)>0) {
    x <- x[-bad,]
    ylabs <- ylabs[-bad]
  }
  if(nrow(x)<=1) stop("too many NA numbers...")

  nplot <- nrow(x)
  if(nplot>ncolumn)
    par(mfrow=c(ceiling(nplot/ncolumn), ncolumn), mar=c(2,4,1,1), oma=c(2,2,4,2))
  else
    par(mfrow=c(1, nplot), mar=c(2,4,1,1), oma=c(4,2,4,2))
  for (i in 1:nplot) {
    xx <- as.vector(x[i,])
    d <- diff(range(xx,na.rm=TRUE))
    nRun <- ncol(x)
    ylab <- ylabs[i]
    xlab <- ""
    ylim <- c(min(xx, na.rm=TRUE), max(xx,na.rm=TRUE)+d/5)
    # mean of similarity results
    m <- mean(xx, na.rm=TRUE)
    # sd of similarity results
    sd <- sd(xx, na.rm=TRUE)
    # which runs are 1sd bigger than the mean
    which.sd <- which(xx>=(m+sd) & xx<(m+2*sd))
    # which runs are 2sd bigger than the mean
    which.2sd <- which(xx>=m+2*sd)
    plot((1:nRun)[-c(which.sd,which.2sd)], xx[-c(which.sd,which.2sd)], ylim=ylim, ylab=ylab, xlab=xlab, type="h")
    # runs that are 1sd bigger than the mean are colored blue
    lines(which.sd, xx[which.sd],col=4, type="h")
    # runs that are 2sd bigger than the mean are color red
    text(which.sd, xx[which.sd]+d/10,which.sd,col=4)
    abline(h=m, lty=2, col="gray")
    abline(h=m+sd, lty=2, col=4)
    if (length(which.2sd) >0) {
      lines(which.2sd, xx[which.2sd], col=2, type="h")
      text(which.2sd, xx[which.2sd]+d/10,which.2sd,col=2)
       abline(h=m+2*sd, lty=2, col=2)
    }
   }
  mtext(paste("Similarity Measure of LOP From Individual Model Run -- ",main, sep=""),3,line=1,outer=TRUE)
}



#--------------------------------------------------------------------------
# error.bar --- draws error bar on a scatter plot
# modified from a Splus function
#---------------------------------------------------------------------------
error.bar <- function(x, y = NULL, lower, upper, incr = T, bar.ends
	 = T, gap = T, add = F, horizontal = F, ...,
	xlab = deparse(substitute(x)), xlim, ylim)
{
	draw.null.warn <- function(draw, gap)
	{
		if(!any(draw)) {
			warning("Not enough room for a gap.")
			draw <- !draw
			gap <- 0
		}
		invisible(list(draw = draw, gap = gap)
			)
	}
	if(missing(x))
		stop("no data for x or y")
	if(missing(y)) {
		if(missing(xlab))
			xlab <- "Index"
		y <- x
		x <- time(x)
	}
	n <- length(x)
	if(length(y) != n)
		stop("length of y must equal the length of x")
	center <- if(horizontal) x else y
	if(missing(lower))
		stop("you must provide lower")
	if(length(lower) > 1 && length(lower) != n)
		stop("length of lower must be 1 or equa l to the length of x")
	#if incr=T lower is assumed >=0
	if(incr) lower <- center - abs(lower)
        else lower <- rep(lower, length = n)
	if(any(lower >= center))
		warning(paste("There are values of 'lower' which are greater or equal to ",
			if(horizontal) "x" else "y"))
	if(missing(upper))
		upper <- 2 * center - lower
	else {
		if(length(upper) > 1 && length(upper) !=n)
			stop("length of upper must be 1 or equal to the length of x")
		if(incr)
			upper <- center + upper
		else upper <- rep(upper, length = n)
	}
	if(any(upper <= center))
		warning(paste(
			"There are values of 'upper' which are smaller or\nequal to ",
			if(horizontal) "x" else "y"))
	if(!add)
		if(horizontal) {
			if(missing(ylim))
				plot(x, y, xlim = if(missing(xlim)) range(c(lower,upper),na.rm = T)
					 else xlim,
					xlab = xlab,
					...)
			else plot(x, y, xlim = if(
					missing(xlim)
					) range(c(
						lower,
						upper),
						na.rm
						 = T)
					 else xlim,
					ylim = ylim,
					xlab = xlab,
					...)
		}
		else {
			if(missing(xlim))
				plot(x, y, ylim = if(missing(ylim)) range(c(lower,upper),na.rm = T)
                                                  else ylim, xlab = xlab, ...)
			else plot(x, y, ylim = if(missing(ylim)) range(c(lower, upper),na.rm = T)
					 else ylim, xlim = xlim, xlab = xlab, ...)
		}
	if(horizontal) {
		if(gap)
			gap <- 0.75 * par("cxy")[
				1]
		draw <- x - lower > gap
		z <- draw.null.warn(draw, gap)
		draw <- z$draw
		gap <- z$gap
		segments(lower[draw], y[draw], x[
			draw] - gap, y[draw])
		draw <- upper - x > gap
		z <- draw.null.warn(draw, gap)
		draw <- z$draw
		gap <- z$gap
		segments(x[draw] + gap, y[draw], upper[
			draw], y[draw])
		if(bar.ends) {
			size.bar <- par("cxy")[2]
			segments(lower, y - size.bar,
				lower, y + size.bar)
			segments(upper, y - size.bar,
				upper, y + size.bar)
		}
	}
	else {
		if(gap)
			gap <- 0.75 * par("cxy")[
				2]
		draw <- upper - y > gap
		z <- draw.null.warn(draw, gap)
		draw <- z$draw
		gap <- z$gap
		segments(x[draw], y[draw] + gap, x[
			draw], upper[draw])
		draw <- y - lower > gap
		z <- draw.null.warn(draw, gap)
		draw <- z$draw
		gap <- z$gap
		segments(x[draw], y[draw] - gap, x[
			draw], lower[draw])
		if(bar.ends) {
			size.bar <- par("cxy")[1]/2
			segments(x - size.bar, upper,
				x + size.bar, upper)
			segments(x - size.bar, lower,
				x + size.bar, lower)
		}
	}
}

