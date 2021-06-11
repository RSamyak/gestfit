#' Make predictions for gest_fit object
#' 
#' Make predictions for gestational data
#' 
#' @param fit        a gest_fit object
#' @param xnew       feature matrix for prediction
#' @param timnew     vector of timestamps
#' @param idew       vector of subject IDs, grouping variable
#' 
#' @export


predict.gest_fit <- function(fit,
                             xnew,
                             timnew,
                             idnew){
  method <- fit$method
  
  if(method == "gbm"){
    ret <- predict.gest_fit_gbm(fit, xnew, timnew, idnew)
  } else if(method == "pcurve"){
    ret <- predict.gest_fit_pcurve(fit, xnew, timnew, idnew)
  } else if(method == "intervals"){
    stopifnot(!is.null(intervals))
    ret <- predict.gest_fit_gbm_intervals(fit, xnew, timnew, idnew)
  }
  
  return(ret)
}