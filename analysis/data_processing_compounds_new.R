library(tidyverse)

data_location <- "../data"


data <- readxl::read_xlsx(file.path(data_location, "Table_S3.xlsx"))

colnames(data)[1:5] <- c("subject_id", "g_stage", "birth_g_stage", "cohort", "labor_onset")

data <- mutate(data, cohort = (cohort == "Discovery")) %>%
  rename(trainind := cohort)

data_full <- filter(data,
                    g_stage <= birth_g_stage)

## Note this step is only meaningful for those that had natural births.
data_full <- mutate(data_full,
                    deliv_age = g_stage + (40 - birth_g_stage))

data_full <- data_full %>%
  group_by(subject_id) %>%
  mutate(gap = g_stage - min(g_stage, na.rm = TRUE)) %>%
  ungroup


saveRDS(data_full, file="../data/data_full_compounds_new.RDS")
