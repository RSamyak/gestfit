nfolds <- length(unique(idtrain))

ytrain_predcv <- rep(NA, length(ytrain))

for(i in 1:nfolds){
  
  cat(i)
  
  this_id <- unique(idtrain)[i]
  
  otest <- idtrain == this_id
  otrain <- !otest
  
  otest_filter <- rep(FALSE, length(otest))
  
  keepweeks <- c(12, 16, 26)
    
    
    for(week in keepweeks){
      oo <- which.min(abs(ytrain[otest] - week))
      otest_filter[otest][oo] <- TRUE
      # count <- count + 1
      # if(length(o[oo]) > 1) break
      # if(count > 3) break
    }
    
  
    ytrain_predcv[otest_filter] <- T
}

cat(sum(!is.na(ytrain_pred_cv)))

