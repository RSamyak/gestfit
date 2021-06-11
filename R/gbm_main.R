#' @import gbm

gest_fit_gbm <- function(x,
                         y,
                         tim,
                         id,
                         ...) {
  
  
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
  
  a2 <- gbm.fit(
    cbind(x_screen[, keep, drop = FALSE], rank_gap),
    y,
    interaction.depth = 2,
    distribution = "gaussian",
    shrinkage = .01,
    n.trees = 500,
    verbose = FALSE
  )
  
  
  a2$oo <- oo
  a2$keep <- keep
  
  return(a2)
}

predict.gest_fit_gbm <- function(fit,
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

  
  yval.hat = predict(
    fit, 
    cbind(xnew_screen[, keep, drop = FALSE], rank_gap), 
    n.trees = seq(10, 500, by = 50)
  )
  yval.hat = yval.hat[, ncol(yval.hat)]
  
  return(yval.hat)
}