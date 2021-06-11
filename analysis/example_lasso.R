library(glmnet)
library(tidyverse)
library(gridExtra)

path <- "../R/"

for(file in list.files(path)) source(file.path(path, file))



source("load_data_gstage.R")
# source("load_data_deliv_age.R")
# source("load_data_gstage_compounds.R")
# source("load_data_deliv_age_compounds.R")

source("filter_keepval.R")
source("select_keep3.R")

lassofit <- cv.glmnet(xtrain, ytrain, foldid = as.numeric(as.factor(idtrain)))

ypred_lasso <- predict(lassofit, xval)

mean(abs(ypred_lasso - yval))
sqrt(mean((ypred_lasso - yval)**2))

sum(coef(lassofit) != 0)
