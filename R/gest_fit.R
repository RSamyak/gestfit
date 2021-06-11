#' Fit a model to gestational data
#' 
#' Fit a model to gestational data as in PAPER
#' 
#' @param x                 training feature matrix
#' @param y                 training response vector
#' @param tim               training timestamps
#' @param id                training subject IDs, grouping variable
#' @param method            one of "gbm", "pcurve", "intervals"
#' @param interval_limits   if method = "intervals", a numerical vector
#'                             indicating nominal quantiles for endpoints
#' 
#' @export

gest_fit <- function(x,
                     y,
                     tim,
                     id,
                     method = c("gbm", "pcurve", "intervals"),
                     interval_limits = NULL,
                     ...
){
  
  method <- match.arg(method)
  
  if(method == "gbm"){
    ret <- gest_fit_gbm(x, y, tim, id, ...)
  } else if(method == "pcurve"){
    ret <- gest_fit_pcurve(x, y, tim, id, ...)
  } else if(method == "intervals"){
    stopifnot(!is.null(intervals))
    ret <- gest_fit_gbm_intervals(x, y, tim, id, limits = interval_limits)
  }
  
  ret$method <- method
  class(ret) <- c(class(ret), "gest_fit")
  
  return(ret)
}

