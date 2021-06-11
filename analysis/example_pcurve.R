library(tidyverse)

path <- "../R/"

for(file in list.files(path)) source(file.path(path, file))


# source("load_data_gstage.R")
# source("load_data_deliv_age.R")
source("load_data_gstage_compounds.R")
# source("load_data_deliv_age_compounds.R")

# source("select_keep3.R")
# source("filter_keepval.R")

fitpc <- gest_fit(xtrain, ytrain, gaptrain, idtrain)

predy <- predict.gest_fit(fitpc, xval, gapval, idval)

predy2 <- alignment_response(predy, gapval, idval)

sqrt(mean((predy-yval)**2))
sqrt(mean((predy2-yval)**2))

mean(abs(predy - yval))
mean(abs(predy2 - yval))

p1 <- ggplot(data = NULL) + 
  geom_point(aes(x = yval, y = predy, colour = as.factor(idval))) +
  geom_abline() +
  labs(x = "True Response",
       y = "Predicted Value",
       colour = "Subject ID") + 
  theme_bw() + theme(legend.position = "bottom")

p2 <- ggplot(data = NULL) + 
  geom_point(aes(x = yval, y = predy2, colour = as.factor(idval))) +
  geom_abline() +
  labs(x = "True Response",
       y = "Predictions w/ Alignment",
       colour = "Subject ID") + 
  theme_bw() + theme(legend.position = "bottom")

plot(p2)

ggsave("alignment_pcurve_deliv_age_compounds.png", plot = p1)
