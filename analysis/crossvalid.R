

nfolds <- length(unique(idtrain))

ytrain_predcv_gbm <- ytrain_predcv <- ytrain_predcv_gbm2 <- ytrain_predcv2 <- rep(NA, length(ytrain))

for(i in 1:nfolds){
  
  cat(i)
  
  this_id <- unique(idtrain)[i]
  
  otest <- idtrain == this_id
  otrain <- !otest
  
  ofitgbm <- gest_fit_gbm(xtrain[otrain, ],
                          ytrain[otrain],
                          gaptrain[otrain],
                          idtrain[otrain])
  
  opredy_gbm <- predict.gest_fit_gbm(ofitgbm,
                                     xtrain[otest, ],
                                     gaptrain[otest],
                                     idtrain[otest])
  opredy_gbm2 <- alignment_response(opredy_gbm, gaptrain[otest], idtrain[otest])
  
  cat(".")
  
  fitpc <- gest_fit(xtrain[otrain, ], ytrain[otrain], gaptrain[otrain], idtrain[otrain])
  
  opredy <- predict.gest_fit(fitpc, xtrain[otest, ], gaptrain[otest], idtrain[otest])
  
  opredy2 <- alignment_response(opredy, gaptrain[otest], idtrain[otest])
  
  cat(".")
  
  ytrain_predcv[otest] <- opredy
  ytrain_predcv2[otest] <- opredy2
  ytrain_predcv_gbm[otest] <- opredy_gbm
  ytrain_predcv_gbm2[otest] <- opredy_gbm2
  
  cat(" ", fill = TRUE)
}