library(gbm)
library(tidyverse)
library(gridExtra)
library(glmnet)

path <- "R/"

for(file in list.files(path)) source(file.path(path, file))

path_to_data <- "../data"

new_ids <- readxl::read_xlsx(file.path(path_to_data, "Subjectid_rename_for publication.xlsx"))

new_ids <- new_ids %>%
  rename(subject_id := `Original subject id`,
         new_subject_id_raw := `New subject id`) %>%
  mutate(new_subject_id = str_remove(new_subject_id_raw, "DP"))

source("analysis/load_data_gstage.R")

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


do_everything <- function(str){
  fitgbm <- gest_fit_gbm(xtrain, ytrain, gaptrain, idtrain)
  
  print(summary(fitgbm, plotit = FALSE)[1:50, 1])
  
  predy_gbm <- predict.gest_fit_gbm(fitgbm, xval, gapval, idval)
  
  predy_gbm2 <- alignment_response(predy_gbm, gapval, idval)
  
  print(paste("RMS_gbm", sqrt(mean((predy_gbm-yval)**2))))
  print(sqrt(mean((predy_gbm2-yval)**2)))
  
  print(paste("MAE_gbm", mean(abs(predy_gbm - yval))))
  print(mean(abs(predy_gbm2 - yval)))
  
  suppressMessages({
  fitlonglas <- gest_fit_longlas(xtrain, ytrain, gaptrain, idtrain)
  predy_longlas <- predict.gest_fit_longlas(fitlonglas, xval, gapval, idval)

  predy_longlas2 <- alignment_response(predy_longlas, gapval, idval)

  })
  
  print(paste("RMS_longlas", sqrt(mean((predy_longlas-yval)**2))))
  print(sqrt(mean((predy_longlas2-yval)**2)))

  print(paste("MAE_longlas", mean(abs(predy_longlas - yval))))
  print(mean(abs(predy_longlas2 - yval)))
  

  fitpc <- gest_fit_pcurve(xtrain, ytrain, gaptrain, idtrain)
  
  predy <- predict.gest_fit_pcurve(fitpc, xval, gapval, idval)
  
  predy2 <- alignment_response(predy, gapval, idval)
  
  print(paste("RMS_pcurve", sqrt(mean((predy-yval)**2))))
  print(sqrt(mean((predy2-yval)**2)))
  
  print(paste("MAE_pcurve", mean(abs(predy - yval))))
  print(mean(abs(predy2 - yval)))
  
  xlims <- ylims <- c(4, 46)
  
  if(grepl("deliv_age", str)) {
    yval <-  yval - 40
    predy_gbm <- predy_gbm - 40
    predy_gbm2 <- predy_gbm2 - 40
    predy <- predy - 40
    predy2 <- predy2 - 40
    
    xlims <- ylims <- c(-36, 1)
  }
  
  
  plot_dataframe <- tibble(true_value = yval, 
                               `Boosted trees, no alignment` = predy_gbm,
                               `Boosted trees, aligned` = predy_gbm2,
                               `Principal curves` = predy,
                               # pcurve_align = predy2,
                               colour = as.factor(idval)
                               )
  plot_dataframe <- plot_dataframe %>% 
    pivot_longer(cols = c(`Boosted trees, no alignment`, 
                          `Boosted trees, aligned`, 
                          `Principal curves`)) %>%
    mutate(name = factor(name, levels = c("Boosted trees, no alignment", 
                                          "Boosted trees, aligned", 
                                          "Principal curves")))
  
  p <- ggplot(plot_dataframe, aes(x = true_value, y = value, colour = colour)) +
    geom_point(size = 1, alpha = .7) +
    # geom_line(size = .8, alpha = .7) +
    geom_abline() + 
    labs(x = "True Response",
         y = "Predicted Value",
         colour = "Subject ID") + 
    facet_wrap(~name, ncol = 3) +
    coord_equal() +
    ylim(ylims) + xlim(xlims) +
    scale_colour_manual(breaks = colour_breaks, values = colour_values, labels = colour_names) +
    theme_bw() + theme(legend.position = "bottom")
  p
  ggsave(sprintf("alignment_all_%s.png", str), plot = p, width = 7, height = 3.5)
    
  # ggsave(sprintf("alignment_pcurve_%s.png", str), plot = p1,
  #        width = 3.34, height = 3.53)
}


do_everything_cv <- function(str){
  
  nfolds <- length(unique(idtrain))
  
  ytrain_predcv_gbm <- ytrain_predcv <- ytrain_predcv_gbm2 <- ytrain_predcv2 <- rep(NA, length(ytrain))
  
  for(i in 1:nfolds){
    
    cat(i)
    
    this_id <- unique(idtrain)[i]
    
    otest <- idtrain == this_id
    otrain <- !otest
    
    otest_filter <- rep(FALSE, length(otest))
    
    ofitgbm <- gest_fit_gbm(xtrain[otrain, , drop = FALSE],
                            ytrain[otrain],
                            gaptrain[otrain],
                            idtrain[otrain])
    
    if(grepl("only3", str)){
      keepweeks <- c(12, 16, 26)
      
        
        for(week in keepweeks){
          oo <- which.min(abs(ytrain[otest] - week))
          otest_filter[otest][oo] <- TRUE
          # count <- count + 1
          # if(length(o[oo]) > 1) break
          # if(count > 3) break
        }
      
    } else{
      otest_filter <- otest
    }
    
    opredy_gbm <- predict.gest_fit_gbm(ofitgbm,
                                       xtrain[otest_filter, , drop = FALSE],
                                       gaptrain[otest_filter],
                                       idtrain[otest_filter])
    opredy_gbm2 <- alignment_response(opredy_gbm, gaptrain[otest_filter], idtrain[otest_filter])
    
    cat(".")
    
    fitpc <- gest_fit_pcurve(xtrain[otrain, , drop = FALSE], ytrain[otrain], gaptrain[otrain], idtrain[otrain])
    
    if(sum(otest_filter) != 1) {  ## Hack, fix this
      opredy <- predict.gest_fit_pcurve(fitpc, xtrain[otest_filter, , drop = FALSE], gaptrain[otest_filter], idtrain[otest_filter])
      
      opredy2 <- alignment_response(opredy, gaptrain[otest_filter], idtrain[otest_filter])
      
    
    
    
    cat(".")
    
    ytrain_predcv[otest_filter] <- opredy
    ytrain_predcv2[otest_filter] <- opredy2
    ytrain_predcv_gbm[otest_filter] <- opredy_gbm
    ytrain_predcv_gbm2[otest_filter] <- opredy_gbm2
    
    cat(" ", fill = TRUE)
    }
  }
  
  
  print(paste("RMS_gbm_cv", sqrt(mean((ytrain_predcv_gbm-ytrain)**2, na.rm = TRUE))))
  print(sqrt(mean((ytrain_predcv_gbm2-ytrain)**2, na.rm = TRUE)))
  
  print(paste("MAE_gbm_cv", mean(abs(ytrain_predcv_gbm - ytrain), na.rm = TRUE)))
  print(mean(abs(ytrain_predcv_gbm2 - ytrain), na.rm = TRUE))
  
  print(paste("RMS_pcurve_cv", sqrt(mean((ytrain_predcv-ytrain)**2, na.rm = TRUE))))
  print(sqrt(mean((ytrain_predcv2-ytrain)**2, na.rm = TRUE)))
  
  print(paste("MAE_pcurve_cv", mean(abs(ytrain_predcv - ytrain), na.rm = TRUE)))
  print(mean(abs(ytrain_predcv2 - ytrain), na.rm = TRUE))
  
  xlims <- ylims <- c(4, 46)
  
  if(grepl("deliv_age", str)) {
    ytrain <-  ytrain - 40
    ytrain_predcv_gbm <- ytrain_predcv_gbm - 40
    ytrain_predcv_gbm2 <- ytrain_predcv_gbm2 - 40
    ytrain_predcv <- ytrain_predcv - 40
    ytrain_predcv2 <- ytrain_predcv2 - 40
    
    xlims <- ylims <- c(-36, 1)
  }
  

  
  plot_dataframe <- tibble(true_value = ytrain, 
                           `Boosted trees, no alignment` = ytrain_predcv_gbm,
                           `Boosted trees, aligned` = ytrain_predcv_gbm2,
                           `Principal curves` = ytrain_predcv,
                           # pcurve_align = predy2,
                           colour = as.factor(idtrain)
  )
  plot_dataframe <- plot_dataframe %>% 
    pivot_longer(cols = c(`Boosted trees, no alignment`, 
                          `Boosted trees, aligned`, 
                          `Principal curves`)) %>%
    mutate(name = factor(name, levels = c("Boosted trees, no alignment", 
                                          "Boosted trees, aligned", 
                                          "Principal curves")))
  
  p <- ggplot(plot_dataframe) +
    geom_point(aes(x = true_value, y = value, colour = colour)) +
    geom_abline() + 
    labs(x = "Response",
         y = "Predicted Value (CV)",
         colour = "Subject ID") + 
    facet_wrap(~name, ncol = 3) +
    coord_equal() + 
    xlim(xlims) + ylim(ylims) +
    scale_colour_manual(breaks = colour_breaks_train, values = colour_values_train, labels = colour_names_train, guide = NULL) +
    theme_bw() + theme(legend.position = "bottom")
  
  ggsave(sprintf("alignment_all_cv_%s.png", str), plot = p, width = 7, height = 3.5)
  
}




source("analysis/load_data_gstage.R")
    do_everything("gstage")
    do_everything_cv("gstage")
  source("analysis/filter_keepval.R")
    do_everything("gstage_only3")
    do_everything_cv("gstage_only3")


source("analysis/load_data_deliv_age.R")
    do_everything("deliv_age")
    do_everything_cv("deliv_age")
  source("analysis/filter_keepval.R")
    do_everything("deliv_age_only3")
    do_everything_cv("deliv_age_only3")


source("analysis/load_data_gstage_compounds.R")
    do_everything("gstage_compounds")
    do_everything_cv("gstage_compounds")
  source("analysis/filter_keepval.R")
    do_everything("gstage_compounds_only3")
    do_everything_cv("gstage_compounds_only3")
  source("analysis/select_keep3.R")
    do_everything("gstage_compounds_only3_3feat")
    do_everything_cv("gstage_compounds_only3_3feat")

source("analysis/load_data_deliv_age_compounds.R")
    do_everything("deliv_age_compounds")
    do_everything_cv("deliv_age_compounds")
  source("analysis/filter_keepval.R")
    do_everything("deliv_age_compounds_only3")
    do_everything_cv("deliv_age_compounds_only3")
  source("analysis/select_keep3.R")
    do_everything("deliv_age_compounds_only3_3feat")
    do_everything_cv("deliv_age_compounds_only3_3feat")
    
    
source("load_data_gstage_compounds_new.R")
 
