
keepval <- rep(FALSE, length(idval))

keepweeks <- c(12, 16, 26)

for (i in 1:length(unique(idval))){
  count <- 0
  o <- which(idval == unique(idval)[i])
  
  for(week in keepweeks){
    oo <- which.min(abs(yval[o] - week))
    keepval[o[oo]] <- TRUE
    count <- count + 1
    if(length(o[oo]) > 1) break
    if(count > 3) break
  }
}

xval <- xval[keepval, ]
yval <- yval[keepval]
idval <- idval[keepval]
gapval <- gapval[keepval]

rm(keepval)