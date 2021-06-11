gest_fit_gbm_intervals <- function(x,
                         y,
                         tim,
                         id,
                         limits = c(.05, .95),
                         # method = c("pcurve", "gbm", "lasso"),
                         ...) {
  # method <- match.arg(method)
  
  
  screen <- TRUE
  
  set.seed(4334)
  
  if (screen) {
    cc = abs(cor(y, x))
    oo = rank(cc) > ncol(x) - 1000
  } else{
    oo = rep(TRUE, ncol(x))
  }
  
  x_screen = x[, oo, drop = FALSE]
  
  xnames <- colnames(x_screen)
  
  rank_gap <- data.frame(id = id,
                         tim = tim) %>%
    group_by(id) %>%
    mutate(ranks = rank(tim)) %>%
    ungroup %>%
    pull(ranks)
  
  a = gbm.fit(
    cbind(x_screen, rank_gap),
    y,
    interaction.depth = 2,
    distribution = "gaussian",
    shrinkage = .01,
    n.trees = 500,
    verbose = FALSE
  )
  
  imp <- summary(a, plotit = FALSE)
  ooo <- rownames(imp)[which(imp[, 2] > 0)]
  
  keep <- match(ooo, xnames)
  keep <- keep[!is.na(keep)]
  
  b1 <- gbm.fit(
    cbind(x_screen[, keep, drop = FALSE], rank_gap),
    y,
    interaction.depth = 2,
    distribution = list(name = "quantile",
                        alpha = c(limits[1])),
    shrinkage = .01,
    n.trees = 500,
    verbose = FALSE
  )
  
  b2 <- gbm.fit(
    cbind(x_screen[, keep, drop = FALSE], rank_gap),
    y,
    interaction.depth = 2,
    distribution = list(name = "quantile",
                        alpha = c(limits[2])),
    shrinkage = .01,
    n.trees = 500,
    verbose = FALSE
  )
  
  ret <- list(b1 = b1, b2 = b2)
  
  ret$oo <- oo
  ret$keep <- keep
  ret$limits <- limits
  
  return(ret)
}

predict.gest_fit_gbm_intervals <- function(fit,
                                 xnew,
                                 timnew,
                                 idnew) {
  require(Matrix)
  
  oo <- fit$oo
  keep <- fit$keep
  
  xnew_screen = xnew[, oo, drop = FALSE]
  
  rank_gap <- data.frame(id = idnew,
                         tim = timnew) %>%
    group_by(id) %>%
    mutate(ranks = rank(tim)) %>%
    ungroup %>%
    pull(ranks)
  
  
  yval.hat.left = predict(
    fit$b1, 
    cbind(xnew_screen[, keep, drop = FALSE], rank_gap), 
    n.trees = seq(10, 500, by = 50)
  )
  yval.hat.left = yval.hat.left[, ncol(yval.hat.left)]
  
  yval.hat.right = predict(
    fit$b2, 
    cbind(xnew_screen[, keep, drop = FALSE], rank_gap), 
    n.trees = seq(10, 500, by = 50)
  )
  yval.hat.right = yval.hat.right[, ncol(yval.hat.right)]
  
  yval.hat = cbind(yval.hat.left, yval.hat.right)
  
  return(yval.hat)
}

alignment_response_intervals <- function(y, ...){
  apply(y, 2, alignment_response, ...)
}




devtools::install_github("ryantibs/quantgen", subdir="R-package/quantgen")
library(quantgen)

gest_fit_lasso_intervals <- function(x,
                                   y,
                                   tim,
                                   id,
                                   limits = c(.05, .95),
                                   # method = c("pcurve", "gbm", "lasso"),
                                   ...) {
  # method <- match.arg(method)
  
  
  screen <- TRUE
  
  set.seed(4334)
  
  if (screen) {
    cc = abs(cor(y, x))
    oo = rank(cc) > ncol(x) - 1000
  } else{
    oo = rep(TRUE, ncol(x))
  }
  
  x_screen = x[, oo, drop = FALSE]
  
  xnames <- colnames(x_screen)
  
  rank_gap <- data.frame(id = id,
                         tim = tim) %>%
    group_by(id) %>%
    mutate(ranks = rank(tim)) %>%
    ungroup %>%
    pull(ranks)
  
  a = cv_quantile_lasso(
    cbind(x_screen, rank_gap),
    y,
    tau = limits,
    verbose = TRUE,
    lp_solver = "gurobi"
  )
  
  ret <- list(a = a)
  
  ret$oo <- oo
  ret$limits <- limits
  
  return(ret)
}

predict.gest_fit_lasso_intervals <- function(fit,
                                           xnew,
                                           timnew,
                                           idnew) {
  require(Matrix)
  
  oo <- fit$oo
  
  xnew_screen = xnew[, oo, drop = FALSE]
  
  rank_gap <- data.frame(id = idnew,
                         tim = timnew) %>%
    group_by(id) %>%
    mutate(ranks = rank(tim)) %>%
    ungroup %>%
    pull(ranks)
  
  
  yval.hat = predict(
    fit$a, 
    cbind(xnew_screen, rank_gap)
  )
  
  colnames(yval.hat) <- c("yval.hat.left", "yval.hat.right")
  
  return(yval.hat)
}
