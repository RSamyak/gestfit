library(gbm)
library(tidyverse)
library(gridExtra)

path <- "../R/"

for(file in list.files(path)) source(file.path(path, file))

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


do_everything_intervals <- function(str){
  
  fitgbm_int <- gest_fit_gbm_intervals(xtrain, ytrain, gaptrain, idtrain,
                                       limits = c(.25, .75))

  predy <- predict.gest_fit_gbm_intervals(fitgbm_int, xval, gapval, idval)
  
  ## fitlasso_int <- gest_fit_lasso_intervals(xtrain, ytrain, gaptrain, idtrain,
  ##                                          limits = c(.4, .6))
  # fitlasso_int <- readRDS("./temp_fitlassoint.RDS")
  
  # predy <- predict.gest_fit_lasso_intervals(fitlasso_int, xval, gapval, idval)
  
  
  predy2 <- alignment_response_intervals(predy, gapval, idval)
  
  
  
  plotdf_unaligned <- data.frame(predy) %>% cbind(yval, idval)
  
  
  
  plotdf_aligned <- data.frame(predy2) %>% cbind(yval, idval)
  
  plotdf_unaligned %>% 
    mutate(covered = yval <= yval.hat.right & yval >= yval.hat.left,
           width = yval.hat.right - yval.hat.left) %>%
    group_by(idval) %>%
    summarise(coverage = mean(covered),
              width = mean(width)) %>%
    print
  
  plotdf_aligned %>% 
    mutate(covered = yval <= yval.hat.right & yval >= yval.hat.left,
           width = yval.hat.right - yval.hat.left) %>%
    group_by(idval) %>%
    summarise(coverage = mean(covered),
              width = mean(width)) %>%
    print
  
  plotdf <- bind_rows(plotdf_unaligned, plotdf_aligned, .id = "name")  %>%
    mutate(name = as.factor(name))
  
  levels(plotdf$name) <- c("No Alignment", "Aligned")
  
  
  p <- ggplot(plotdf, aes(x = yval, colour = as.factor(idval))) + 
    geom_linerange(aes(ymin = yval.hat.left, ymax = yval.hat.right)) + 
    geom_abline() + theme_bw() + 
    labs(x = "True Response",
         y = "Predicted Value",
         colour = "Subject ID") + 
    facet_wrap(~ name) +
    scale_colour_manual(breaks = colour_breaks, values = colour_values, labels = colour_names) +
    theme_bw() + theme(legend.position = "bottom")
  
  
  
  ggsave(sprintf("alignment_intervals_all_%s.png", str), plot = p, width = 4.67, height = 3.5)
}


source("load_data_gstage.R")
do_everything_intervals("gstage_gbm_25_75")


do_everything_intervals("gstage_lasso")
