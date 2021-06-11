library(gbm)
library(tidyverse)
library(gridExtra)

path <- "../R/"

for(file in list.files(path)) source(file.path(path, file))



# source("load_data_gstage.R")
# source("load_data_deliv_age.R")
source("load_data_gstage_compounds.R")
# source("load_data_deliv_age_compounds.R")

source("select_keep3.R")
# source("filter_keepval.R")



fitgbm <- gest_fit_gbm(xtrain, ytrain, gaptrain, idtrain)

predy <- predict.gest_fit_gbm(fitgbm, xval, gapval, idval)

predy2 <- alignment_response(predy, gapval, idval)

sqrt(mean((predy - yval)**2))
sqrt(mean((predy2 - yval)**2))

mean(abs(predy - yval))
mean(abs(predy2 - yval))

summary(fitgbm) %>% head

colour_breaks <- sort(unique(idval))
colour_values <- scales::hue_pal()(length(unique(idval)))

p1 <- ggplot(data = NULL) + 
  geom_point(aes(x = yval, y = predy, colour = as.factor(idval))) +
  geom_abline() +
  labs(x = "True Response",
       y = "Predicted Value",
       colour = "Subject ID") + 
  ggtitle("Before Alignment") +
  scale_colour_manual(breaks = colour_breaks, values = colour_values) +
  theme_bw() + theme(legend.position = "bottom")

p2 <- ggplot(data = NULL) + 
  geom_point(aes(x = yval, y = predy2, colour = as.factor(idval))) +
  geom_abline() +
  labs(x = "True Response",
       y = "Predicted Value",
       colour = "Subject ID") + 
  ggtitle("After Alignment") +
  scale_colour_manual(breaks = colour_breaks, values = colour_values) +
  theme_bw() + theme(legend.position = "bottom")



p <- arrangeGrob(p1, p2, ncol = 2)
# ggsave("alignment_gbm_gstage_compounds.png", plot = p)
plot(p)
