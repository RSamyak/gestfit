#' @import splines
#' @import Matrix

gest_fit_pcurve <- function(x,
                     y,
                     tim,
                     id,
                     ...) {
  
  o <- order(id, tim)
  x <- x[o,]
  y <- y[o]
  id <- id[o]
  tim <- tim[o]
  
  ilist <- split(seq(along = id), id)
  ylist <- split(y, id)
  
  xlist <- lapply(split(as.data.frame(x), id), data.matrix)
  
  gaplist <- lapply(ylist, function(u)
    u - u[1])
  
  indexlist <- sapply(ylist, length)
  indexlist <- rep(seq(length(indexlist)), indexlist)
  indexlist <- split(indexlist, id)
  
  fitgrid = gestationPC.list(xlist,
                             ylist,
                             ...)
  
  return(fitgrid)
}

predict.gest_fit_pcurve <- function(fitgrid,
                             xnew,
                             timnew,
                             idnew, ...) {
  require(Matrix)
  
  o <- order(idnew, timnew)
  invo <- Matrix::invPerm(o)
  xnew <- xnew[o,]
  idnew <- idnew[o]
  timnew <- timnew[o]
  
  ilist <- split(seq(along = idnew), idnew)
  timlist <- split(timnew, idnew)
  
  gaplist <- lapply(timlist, function(u)
    u - u[1])
  
  xlist <- lapply(split(as.data.frame(xnew), idnew), data.matrix)
  
  predgrid.list <- predlist.gPC(xlist, fitgrid, vplus = 0.1, ...)
  predage <- predlist(predgrid.list, gaplist)
  
  predictions <- unlist(predage)[invo]
  
  return(predictions)
}