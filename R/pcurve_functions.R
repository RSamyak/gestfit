### Write a function to fit a principal curve model, and select features to be used.
gestationPC.list = function(xlist, gagelist, ...) {
  x = do.call("rbind", xlist)
  gage = unlist(gagelist)
  gestationPC(x, gage, ...)
}


gestationPC = function(x,
                       gage,
                       alpha = 1e-14,
                       num = min(100, ncol(x)),
                       knots = seq(from = 10, to = 40, by = 10),
                       Boundary.knots = c(4, 42),
                       gage.grid = seq(from = 4, to = 42, by = .1),
                       ...) {
  require(splines)
  
  if(is.null(alpha) & is.null(num)){
    stop("Need to provide at least one of alpha, num.")
  }
  
  Hns = ns(
    gage,
    knots = knots,
    Boundary.knots = Boundary.knots,
    intercept = TRUE
  )
  
  Hnsg = ns(
    gage.grid,
    knots = knots,
    Boundary.knots = Boundary.knots,
    intercept = TRUE
  )
  
  nsdf = ncol(Hns)
  nsample = nrow(Hns)
  
  ### Get the fits for spline, linear, and constant
  coefns = solve(t(Hns) %*% Hns, t(Hns) %*% x)
  
  residns = x - Hns %*% coefns
  Sigma2 = apply(residns ^ 2, 2, mean)
  
  Varx = apply(scale(x, TRUE, FALSE) ^ 2, 2, mean)
  
  Fns = ((Varx - Sigma2) / (nsdf - 1)) / (Sigma2 / (nsample - nsdf))
  
  pns = pf(Fns, nsdf - 1, nsample - nsdf, lower = FALSE)
  
  if (!is.null(num))
    alpha = sort(pns)[num]
  
  inc = (pns <= alpha)
  ### Get the fit grid for the chosen variables, and the variance measures for them
  fitgrid = Hnsg %*% coefns[, inc]
  varinc = Sigma2[inc]
  return(list(
    pcgrid = fitgrid,
    v = varinc,
    included = inc,
    gage.grid
  ))
}
#### Write a function to compute the weighted distance of each measurement from each
####position along the curve

pred.gPC = function(x, pcobj, vplus = 0) {
  pcgrid = pcobj$pcgrid
  x = x[, pcobj$included, drop = FALSE]
  nx = nrow(x)
  ng = nrow(pcgrid)
  nvar = ncol(x)
  repx = rep(seq(nx), rep(ng, nx))
  repg = rep(seq(ng), nx)
  matrix((scale(x[repx, ] - pcgrid[repg, ], FALSE, sqrt(pcobj$v + vplus)) ^
            2) %*% rep(1 / nvar, nvar), nx, ng, byrow = TRUE)
}

predlist.gPC = function(xlist, ...)
  lapply(xlist, pred.gPC, ...)
#    pred.gPC(do.call("rbind",xlist),...)

lagmean = function(x, lag = NULL) {
  if (is.null(lag))
    mean(x)
  else{
    n = length(x)
    i = max(n - lag, 1)
    mean(x[i:n])
  }
}

predsubject = function(predgrid, gage.gaps, gage.grid, lag = NULL) {
  predgage = gage.gaps
  gage.gaps = gage.gaps - min(gage.gaps) # in case the first is not 0
  for (j in seq(length(gage.gaps))) {
    predgage[j] = predtuple(predgrid[1:j, , drop = FALSE], gage.gaps[1:j], gage.grid)
  }
  predgage
}
predlist = function(predgrid.list, gap.list, gage.grid = seq(from = 4, to = 42, by = .1), ...) {
  predage.list = gap.list
  for (i in seq(along = gap.list))
    predage.list[[i]] = predsubject(predgrid.list[[i]], gap.list[[i]], gage.grid, ...)
  predage.list
}

predtuple = function(predgrid, gage.gaps, gage.grid, lag = NULL) {
  ### predgrid is dimension #gaps x length(grid)
  ###  We are using a tuple of measurements for a subject, and the idea is to
  ### produce the derived distance from sliding this tuple along
  ### gaps should be a vector of weeks, starting with 0 (we assume the rows are in order)
  ### Returns the predicted age for the last of the measurements
  gaps = gap.age(gage.gaps)
  maxgap = max(gaps)
  nage = ncol(predgrid)
  ntuple = length(gaps)
  dist0 = predgrid[1, 1:(nage - maxgap), drop = FALSE]
  if (ntuple > 1)
  {
    for (i in 2:ntuple) {
      dist0 = rbind(dist0, predgrid[i, (1 + gaps[i]):(nage - maxgap + gaps[i])])
    }
  }
  dist0 = apply(dist0, 2, lagmean, lag)
  o = order(dist0)[1]
  gage.grid[o] + max(gage.gaps)
}

gap.age = function(gages, round = 1) {
  gages = round(gages, round)
  (gages - gages[1]) / (.1 ^ round)
}


predone = function(predgrid, gage.grid) {
  require(mda)
  id = softmax(-predgrid)
  gage.grid[id]
}
predone.list = function(predgrid.list, gage.grid) {
  predonelist = predgrid.list
  for (i in seq(along = predonelist))
    predonelist[[i]] = predone(predgrid.list[[i]], gage.grid)
  predonelist
}

### Functions for creating subsets of observations for testing

sublist.index = function(ylist, target = c(14, 20, 24)) {
  getindex = function(y, target) {
    indices = seq(along = y)
    slist = integer(length(target))
    for (j in seq(along = target)) {
      x = abs(y - target[j])
      slist[j] = order(x)[1]
      y[slist[j]] = -100
    }
    slist
  }
  lapply(ylist, getindex, target = target)
}
sublist.y = function(ylist, index) {
  iseq = seq(along = ylist)
  lapply(iseq, function(i)
    ylist[[i]][index[[i]]])
}
sublist.x = function(xlist, index) {
  iseq = seq(along = index)
  lapply(iseq, function(i)
    xlist[[i]][index[[i]], , drop = FALSE])
}
