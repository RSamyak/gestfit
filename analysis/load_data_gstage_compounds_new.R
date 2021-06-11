data_full <- readRDS(file.path("data", "data_full_compounds_new.RDS"))


data_train <- data_full %>%
  filter(trainind)

data_val <- data_full %>%
  filter(!trainind)

xall <- data_full %>%
  select(-c(subject_id,
            birth_g_stage, g_stage, 
            trainind, labor_onset, deliv_age, gap
  )) %>%
  as.matrix

#### Gather Training Set
xtrain <- data_train %>%
  select(-c(subject_id,
            birth_g_stage, g_stage, 
            trainind, labor_onset, deliv_age, gap
  )) %>%
  as.matrix

ytrain <- data_train %>%
  pull(g_stage)


idtrain <- data_train %>%
  pull(subject_id)

gaptrain <- data_train %>% 
  pull(gap)

#### Gather Validation Set
xval <- data_val %>%
  select(-c(subject_id,
            birth_g_stage, g_stage, 
            trainind, labor_onset, deliv_age, gap
  )) %>%
  as.matrix

yval <- data_val %>%
  pull(g_stage)


idval <- data_val %>%
  pull(subject_id)

gapval <- data_val %>% 
  pull(gap)
