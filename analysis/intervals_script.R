library(gbm)
library(tidyverse)
library(gridExtra)

path <- "../R/"

for(file in list.files(path)) source(file.path(path, file))

#################################333


new_ids <- readxl::read_xlsx("../../../liang/Subjectid_rename_for publication.xlsx")

new_ids <- new_ids %>%
  rename(subject_id := `Original subject id`,
         new_subject_id_raw := `New subject id`) %>%
  mutate(new_subject_id = str_remove(new_subject_id_raw, "DP"))

source("load_data_gstage.R")

new_ids_train <- new_ids %>% 
  filter(subject_id %in% idtrain)

colour_breaks_train <- new_ids_train$subject_id
colour_values_train <- scales::hue_pal()(length(new_ids_train$subject_id))
colour_names_train <- new_ids_train$new_subject_id

new_ids <- new_ids %>% 
  filter(subject_id %in% idval)

colour_breaks <- new_ids$subject_id
colour_values <- scales::hue_pal()(length(new_ids$subject_id))
colour_names <- new_ids$new_subject_id

#################



source("load_data_gstage.R")
# do_everything("gstage")
# do_everything_cv("gstage")
# source("filter_keepval.R")
# do_everything("gstage_only3")
# do_everything_cv("gstage_only3")


# source("load_data_deliv_age.R")
# do_everything("deliv_age")
# do_everything_cv("deliv_age")
# source("filter_keepval.R")
# do_everything("deliv_age_only3")
# do_everything_cv("deliv_age_only3")


# source("load_data_gstage_compounds.R")
# do_everything("gstage_compounds")
# do_everything_cv("gstage_compounds")
# source("filter_keepval.R")
# do_everything("gstage_compounds_only3")
# do_everything_cv("gstage_compounds_only3")
# source("select_keep3.R")
# do_everything("gstage_compounds_only3_3feat")
# do_everything_cv("gstage_compounds_only3_3feat")

# source("load_data_deliv_age_compounds.R")
# do_everything("deliv_age_compounds")
# do_everything_cv("deliv_age_compounds")
# source("filter_keepval.R")
# do_everything("deliv_age_compounds_only3")
# do_everything_cv("deliv_age_compounds_only3")
# source("select_keep3.R")
# do_everything("deliv_age_compounds_only3_3feat")
# do_everything_cv("deliv_age_compounds_only3_3feat")


myplot <- function(plotdf){
  ggplot(plotdf, aes(x = yval, colour = as.factor(idval))) + 
    geom_linerange(aes(ymin = yval.hat.left, ymax = yval.hat.right)) + 
    geom_abline() + theme_bw() + 
    labs(x = "True Response",
         y = "Predicted Value",
         colour = "Subject ID") + 
    scale_colour_manual(breaks = colour_breaks, values = colour_values, labels = colour_names, guide = NULL) +
    theme_bw() + theme(legend.position = "bottom")
}


fitgbm_int <- gest_fit_gbm_intervals(xtrain, ytrain, gaptrain, idtrain,
                                     limits = c(.4, .6))

predy <- predict.gest_fit_gbm_intervals(fitgbm_int, xval, gapval, idval)

predy2 <- alignment_response_intervals(predy, gapval, idval)


# fitlasso_int <- gest_fit_lasso_intervals(xtrain, ytrain, gaptrain, idtrain,
#                                      limits = c(.4, .6))
fitlasso_int <- readRDS("temp_fitlassoint.RDS")


predy <- predict.gest_fit_lasso_intervals(fitlasso_int, xval, gapval, idval)

predy2 <- alignment_response_intervals(predy, gapval, idval)



plotdf_unaligned <- data.frame(predy) %>% cbind(yval, idval)
myplot(plotdf_unaligned)



plotdf_aligned <- data.frame(predy2) %>% cbind(yval, idval)
myplot(plotdf_aligned)

plotdf_unaligned %>% 
  mutate(covered = yval <= yval.hat.right & yval >= yval.hat.left,
         width = yval.hat.right - yval.hat.left) %>%
  group_by(idval) %>%
  summarise(coverage = mean(covered),
            width = mean(width))




