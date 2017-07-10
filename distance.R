###--------------------------------------------------------------------------------
#                   calcDist.func
# This function implements Dev Nag's semi-linear distance approach
# 1) The user inputs J, alpha and epsilon, see Nag's paper
# 2) The function returns a function with bindings for the above arguments
# 3) The returned function accepts two vectors and computes their distance
# 4) usages: for euclidean distance, suppose "x" and "y" are the two vectors to compute distance
#            J <- 20; alpha <- c(c(0,1),rep(0,18)); epsilon <- rep(1,20)
#            euclid <- calcDist.func(J=J, alpha=alpha, epsilon=epsilon)
#            dist <- euclid(x,y)
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
#
mse <- function(x, na.rm = FALSE)
{
    if (is.matrix(x))
        apply(x, 2, z, function(z) sum(x^2, na.rm=TRUE))
    else if (is.vector(x))
        sum(x^2, na.rm = na.rm)
    else if (is.data.frame(x))
        sapply(x,function(z) sum(x^2, na.rm=TRUE))
    else sum(as.vector(x)^2, na.rm = na.rm)
}

###--------------------------------------------------------------------------------
#                   calcVar.func
# This function calculates variance of the deviance (x-y)
# 1) The argument "func" could be "sd"(default) or "mad"(robust version of sd) or other functions
# 2) The function returns a function with argument bindings
# 3) The returned function accepts two vectors and computes their distance
# 4) usages: f <- calcVar.func(func=mad)
#            dist <- f(x,y)
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
#                               Dev Nag's distance measure list
# This list is for input into the function calcSimilarity, specify similarity.funcs=DevsList
# for more information see Table 2 in Dev Nag's paper and the function calcDist.func
###-------------------------------------------------------------------------------------
DevsList <- list(
                 constant = calcDist.func(z=NULL, weights=NULL, J=20, alpha=rep(1,20), epsilon=rep(1,20)),
                 euclidean = calcDist.func(z=NULL, weights=NULL, J=20, alpha=c(0,1,rep(0,18)), epsilon=rep(1,20)),
                 ascend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=((1:20)-1)/20, epsilon=rep(1,20)),
                 descend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=1-((1:20)-1)/20, epsilon=rep(1,20)),
                 eascend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=(rep(1,20)), epsilon=c(1:20)/20),

                 edescend = calcDist.func(z=NULL, weights=NULL, J=20, alpha=(rep(1,20)), epsilon=1-(c(1:20)-1)/20))

###----------------------------------------------------------------------------------------
#                               Another list of similarity functions
# This list is for input into the function calcSimilarity, specify similarity.funcs=smList
# it includes euclidean distance, city block distnace, semi-linear distance, global_sd, global_mad
###-----------------------------------------------------------------------------------------------
smList <- list(
               city = calcDist.func(z="peakID", weights=NULL, J=20, alpha=c(1,rep(0,19)), epsilon=rep(1,20)),
               euclidean = calcDist.func(z="peakID", weights=NULL, J=20, alpha=c(0,1,rep(0,18)), epsilon=rep(1,20)),
               constant = calcDist.func(z="peakID", weights=NULL, J=20, alpha=rep(1,20), epsilon=rep(1,20)),
               sd = calcVar.func(z="peakID", func=sd),
               mad = calcVar.func(z="peakID", func=mad))


###------------------------------------------------------------------------------------------------
#                                 calcSimilarity
# A functions that computes specified similarity measures for an object of class "LOP"
# 1) it accepts an object of class "LOP" and a list of similarity measures
# 2) it returns an object of class "LOPsimilarity"; see the function plot.LOPsimilarity
# 3) if smooth=TRUE, similarity is measured betweent the SMOOTHED model data and the reference;
#    if smooth=FALSE, similarity is meausured between individual runs (specified by index) and the reference
# 4) usage: suppose "X" is an object of class "LOP"
#            smRes <- calcSimilarity(X, smooth=FALSE)
#            plot(smRes)
###-----------------------------------------------------------------------------------------------
if(!isGeneric("calcSimilarity")) setGeneric("calcSimilarity", function(object, ...)
                                    standardGeneric("calcSimilarity"))
setMethod("calcSimilarity", "LOP", function(object, logit=TRUE, index=NULL, interpolate=TRUE,
                                            smooth=TRUE,
                                            smooth.func=c("smooth.spline", "ksmooth",
                                              "supsmu", "lowess"),
                                            similarity.funcs=smList, ...) {
  NRun <- object$NRun
  NRef <- object$NRef
  NSim <- length(similarity.funcs)

  if(interpolate) {
    object <- approxLOP(object)
  } else {
    if (nrow(object$Reference) > nrow(object$Model))
      object$Reference <- object$Reference[1:nrow(object$Model),]
  }

  Ref <- object$Reference
  Ref[is.na(Ref)] <- 0.0

  if (logit) Ref[,2:(NRef+1)] <- log10(Ref[,2:(NRef+1)])

  if (NRef > 1)
    r <- rowMeans(Ref[,2:(NRef+1)], na.rm=TRUE)
  else
    r <- Ref[,2]
  r[is.nan(r)] <- 0.0

  if (smooth) {# smooth the model data
    smooth.func <- match.arg(smooth.func)
    s <- smoothing(object, logit=logit, smooth.func=smooth.func, ...)
    pred.s <- predict(s, Ref[,1])
    if (data.class(pred.s)=="list") m <- pred.s$y
    else m <- pred.s

    D <- list()
    for (sim.func in similarity.funcs)
       D <- c(D, list(sim.func(m, r))) # calculates the similarity between the smoothed model and the reference

  } else { # don't smooth
    if (is.null(index)) index <- c(1:NRun)
    Model <- object$Model
    if (logit) Model <- log10(object$Model)
    D <- list()
    for (i in 1:NSim) {
      sim.func <- similarity.funcs[[i]]
      d <- NULL
      for (j in index){ # calculates the similarity between specified runs and the reference
        m <- Model[,j+1]
        d <- cbind(d,sim.func(m, r))
      }
      colnames(d) <- index
      D <- c(D,list(d))

    }

  }

  names(D) <- names(similarity.funcs)
  class(D) <- "LOPsimilarity"
  return(D)
  })
