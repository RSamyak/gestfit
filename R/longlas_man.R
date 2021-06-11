gest_fit_longlas <- function(x,
                         y,
                         tim,
                         id,
                         lags = 3,
                         ...) {
  
  
  lagged <- function(i){paste0("lagged", i)}
  
  lagged0 <- x %>% 
    {cbind(y, tim, id, .)} %>%
    as.data.frame %>%
    group_by(id) %>%
    {.}
  
  lagged0 <- lagged0 %>%
    arrange(id, tim)
  
  for(i in 1:lags){
    assign(lagged(i), mutate_all(get(lagged(i-1)), lag))
  }
  
  for(i in 1:lags){
    assign(lagged(i), 
           get(lagged(i)) %>%
             ungroup %>%
             select(-c(id, y, tim)))
    
    `colnames<-`(get(lagged(i)), paste0(colnames(get(lagged(i))), "_lag1"))
  }
  
  x <- lagged0 %>%
    select(-c(id, y, tim))
  
  for(i in 1:lags){
    x <- bind_cols(x, get(lagged(i)))
  }
  
  y <- lagged0 %>% pull(y)
  id <- lagged0 %>% pull(id)
  tim <- lagged0 %>% pull(tim)
  
  
  x <- as.matrix(x)
  nonzeromeans <- mean(x[!is.na(x)])
  x[is.na(x)] <- nonzeromeans
  
  # nonzeromeans <- apply(x, 2, function(u){mean(u[is.na(u)])})
  # 
  # for(i in 1:ncol(x)){
  #   x[is.na(x[,i])] <- nonzeromeans[i]
  # }
  
  fit <- cv.glmnet(x, y)
  fit$lags <- lags
  fit$nonzeromeans <- nonzeromeans
  
  
  fit
}

predict.gest_fit_longlas <- function(fit,
                                 xnew,
                                 timnew,
                                 idnew) {
  
  lags <- fit$lags
  
  o <- order(idnew, timnew)
  invo <- Matrix::invPerm(o)
  
  x <- xnew[o, ]
  tim <- timnew[o]
  id <- idnew[o]
  
  lagged <- function(i){paste0("lagged", i)}
  
  lagged0 <- x %>% 
    {cbind(tim, id, .)} %>%
    as.data.frame %>%
    group_by(id) %>%
    {.}
  
  for(i in 1:lags){
    assign(lagged(i), mutate_all(get(lagged(i-1)), lag))
  }
  
  for(i in 1:lags){
    assign(lagged(i), 
           get(lagged(i)) %>%
             ungroup %>%
             select(-c(id, tim)))
    
    `colnames<-`(get(lagged(i)), paste0(colnames(get(lagged(i))), "_lag1"))
    
  }
  
  x <- lagged0 %>%
    select(-c(id, tim))
  
  for(i in 1:lags){
    x <- bind_cols(x, get(lagged(i)))
  }
  
  x <- as.matrix(x)
  
  x[is.na(x)] <- fit$nonzeromeans[1]
  
  # for(i in 1:ncol(x)){
  #   x[is.na(x[,i])] <- fit$nonzeromeans[i]
  # }
  
  yhat <- predict(fit, x)
  
  return(yhat[invo])
}
