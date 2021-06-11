
#' Aligned mean of a given vector
#' 
#' This function does the alignment processing for a single subject
#' 
#' @param y     response
#' @param gap   timestamps
#' 
aligned_mean <- function(y, gap){

  
  stopifnot(length(y) == length(gap))

  centred_y <- y - gap
  mean_y <- mean(centred_y, na.rm = TRUE)
  return(mean_y + gap)
}

#' Aligned mean of a given vector
#' 
#' This function does the alignment processing for a single subject,
#'  aligning only on observations in the past
#' 
#' @param y     response
#' @param gap   timestamps
#' 
aligned_mean_only_past <- function(y, gap){
  
  ret <- rep(NA, length(y))
  for(this_gap in unique(gap)){
    o <- which(gap <= this_gap)
    oo <- which(gap[o] == this_gap)
    
    ret[o[oo]] <- aligned_mean(y[o], gap[o])[oo]
  }
  
  return(ret)
}

#' Alignment of predictions using timestamps
#'
#' This performs alignment of predictions using timestamps
#'
#' @param y      response
#' @param gap    offsets
#' @param id     subject id
#' 
#' @export alignment_response
alignment_response <- function(y, gap, id, only.past = TRUE){
  
  ylist <- split(y, id)
  gaplist <- split(gap, id)
  
  if(only.past){
    retlist <- mapply(aligned_mean_only_past, ylist, gaplist, SIMPLIFY = FALSE)
  }
  
  else{
    retlist <- mapply(aligned_mean, ylist, gaplist, SIMPLIFY = FALSE)
  }
  
  ret <- unsplit(retlist, id)
  
  return(ret)
}


# myplot=function(subjectid,yhat,y,mfrow){
#   par(mfrow=mfrow)
#   for( k in unique(subjectid)){
#     oo=which(subjectid==k)
#     plot(yhat[oo],y[oo])
#     abline(0,1)
#   }
# }

# keepf <- function(y, id, weeks = c(10, 14, 20)) {
#   #keep just obs with ages closest to 10,14,20
#   keep <- rep(F, length(y))
#   for (u in unique(id)) {
#     uu <- which(id == u)
#     for (week in weeks) {
#       o <- which.min(abs(y[uu] - week))
#       keep[uu[o]] <- TRUE
#     }
#   }
#   return(keep)
# }
# 
# 
# 
# falign <- function(yhat, gap, id) {
#   ret <- yhat
#   for (k in id) {
#     o <- which(id == k)
#     
#     # ### TODO: This assumes ordered response??
#     # ### Check: Results somehow seem to be much worse if you order
#     # oo <- order(gap[o])
#     # o <- o[oo]
#     
#     if (length(o) > 1) {
#       #adj only possible with >1  time point
#       y1hat <- yhat[o][1]
#       for (i in 2:length(gap[o])) {
#         y1hat <- c(y1hat, yhat[o][i] - (gap[o][i] - gap[o][1]))
#       }
#       
#       m <- cumsum(y1hat) / (1:length(o))
#       
#       ret[o] <- m + c(0, gap[o][-1] - gap[o][1])
#     }
#     
#   }
#   return(ret)
# }
