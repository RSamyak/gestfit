library(tidyverse)

data_location <- "../data"

# data <- read.csv(file.path(data_location, "allfeatures.csv"))

data <- read.csv(file.path(data_location, "shortlist.csv"))


meta <- read.csv(file.path(data_location, "metadata.csv")) %>%
  select(-X)

birthtype <- read.csv(file.path(data_location, "labor_onset.csv"))


data_full <- inner_join(meta, data, by = "sample_id")
data_full <- left_join(data_full, birthtype, by = "subject_id")

data_full <- filter(data_full,
                    g_stage <= birth_g_stage)

data_full <- mutate(data_full,
                    trainind = (year_lcms == 2016))

## Note this step is only meaningful for those that had natural births.
data_full <- mutate(data_full,
                    deliv_age = g_stage + (40 - birth_g_stage))

data_full <- data_full %>%
  group_by(subject_id) %>%
  mutate(gap = g_stage - min(g_stage, na.rm = TRUE)) %>%
  ungroup


saveRDS(data_full, file="data/data_full.RDS")
