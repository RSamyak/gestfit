data_full <- readRDS(file.path("data", "data_full_compounds.RDS"))


data_train <- data_full %>%
  filter(trainind,
         labor_onset == "natural")

data_val <- data_full %>%
  filter(!trainind,
         labor_onset == "natural")

data_new <- data_full %>%
  filter(!trainind) %>%
  select(subject_id, sample_id, g_stage) %>%
  group_by(subject_id) %>%
  mutate(keepsmall = ifelse(abs(g_stage - 12) == min(abs(g_stage - 12))|
                              abs(g_stage - 16) == min(abs(g_stage - 16))|
                              abs(g_stage - 26) == min(abs(g_stage - 26)), TRUE, FALSE)) %>%
  mutate(keepsmall = ifelse(subject_id == 5 & g_stage == 11, FALSE, keepsmall))


xall <- data_full %>%
  select(-c(X, sample_id, subject_id,
            birth_g_stage, g_stage, year_lcms,
            trainind, labor_onset, deliv_age, gap
  )) %>%
  as.matrix

#### Gather Training Set
xtrain <- data_train %>%
  select(-c(X, sample_id, subject_id,
            birth_g_stage, g_stage, year_lcms,
            trainind, labor_onset, deliv_age, gap
  )) %>%
  as.matrix


ytrain <- data_train %>%
  pull(deliv_age)

idtrain <- data_train %>%
  pull(subject_id)

gaptrain <- data_train %>% 
  pull(gap)

data_val <- data_val %>%
  left_join(data_new) %>%
  filter(keepsmall) %>%
  select(-keepsmall)

#### Gather Validation Set
xval <- data_val %>%
  select(-c(X, sample_id, subject_id,
            birth_g_stage, g_stage, year_lcms,
            trainind, labor_onset, deliv_age, gap
  )) %>%
  as.matrix

yval <- data_val %>%
  pull(deliv_age)


idval <- data_val %>%
  pull(subject_id)

gapval <- data_val %>% 
  pull(gap)
