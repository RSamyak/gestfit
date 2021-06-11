
variables_to_keep <- c("Estriol.16.Glucuronide", "Pregnenolone", "Progesterone")


if(all(variables_to_keep %in% colnames(xtrain))){
  xtrain <- xtrain[, variables_to_keep]
  xval <- xval[, variables_to_keep]
} else stop("Have you loaded compounds data?")

