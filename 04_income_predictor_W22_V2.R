pacman::p_load(tidyverse, haven, jtools, randomForest, caret)

rm(list = ls())

dat <- read_dta("data/panel_data/BES2019_W22_v24.0.dta")

# scaling functions --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# ind vars ------------------------------------------

dat <- dat %>% 
  mutate(
    white_british = fct_lump_n(as.factor(p_ethnicity), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    no_religion = fct_lump_n(as.factor(p_religion), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double()
  ) %>% 
  rename(la_code = oslaua_code)

dat$male <- ifelse(dat$gender == 1, 1, 0)
dat$income <- ifelse(dat$p_gross_household %in% c(16, 17), 
                     NA, dat$p_gross_household)
dat$own_outright <- ifelse(dat$p_housing == 1, 1, 0)
dat$private_renting <- ifelse(dat$p_housing == 4, 1, 0)
dat$social_housing <- ifelse(dat$p_housing == 5|dat$p_housing == 6, 1, 0)
dat$own_mortgage <- ifelse(dat$p_housing == 2, 1, 0)
dat$homeowner <- ifelse(dat$own_outright == 1 | dat$own_mortgage == 1, 1, 0)
dat$age_raw <- dat$age
dat$age <- scale_this(dat$age)
dat$edu_20plus <- ifelse(dat$p_education_age == 5, 1, 0)
dat$edu_20plus[is.na(dat$p_education_age)] <- NA
dat$broadsheet <- ifelse(dat$p_paper_read %in% c(6,7,8,9,10), 1, 0)
dat$broadsheet[is.na(dat$p_paper_read)] <- NA
dat$full_time <- ifelse(dat$p_work_stat == 1, 1, 0)
dat <- dat %>% 
  mutate(low_income = ifelse(p_gross_household < 5, 1, 0) %>% as.factor)
dat$low_income[is.na(dat$income)] <- NA
dat$disabled <- ifelse(dat$p_disability %in% c(1, 2), 1, 0)
dat$disabled[is.na(dat$p_disability)] <- NA
dat$p_hh_size <- ifelse(dat$p_hh_size %in% c(9, 10), 
                        NA, dat$p_hh_size)
dat$single_household <- ifelse(dat$p_hh_size == 1, 1, 0)
dat$cohabiting <- ifelse(dat$p_marital %in% c(1, 2, 4), 1, 0)
dat$cohabiting[is.na(dat$p_marital)] <- NA
dat$edu_15 <- ifelse(dat$p_education_age == 1, 1, 0)
dat$edu_15[is.na(dat$p_education_age)] <- NA
dat$edu_16 <- ifelse(dat$p_education_age == 2, 1, 0)
dat$edu_16[is.na(dat$p_education_age)] <- NA
dat$e <- ifelse(dat$p_socgrade == 6, 1, 0)
dat$d <- ifelse(dat$p_socgrade == 5, 1, 0)
dat$c2 <- ifelse(dat$p_socgrade == 4, 1, 0)
dat$c1 <- ifelse(dat$p_socgrade == 3, 1, 0)
dat$b <- ifelse(dat$p_socgrade == 2, 1, 0)
dat$pakistan_bangladesh <- ifelse(dat$p_ethnicity %in% c(8, 9), 1, 0)
dat$black <- ifelse(dat$p_ethnicity %in% c(11, 12, 13), 1, 0)
dat$pri_job <- ifelse(dat$p_job_sector == 1, 1, 0)
dat$pri_job[is.na(dat$p_job_sector)] <- NA
dat$pub_job <- ifelse(dat$p_job_sector == 2, 1, 0)
dat$pub_job[is.na(dat$p_job_sector)] <- NA
dat$unemployed <- ifelse(dat$p_work_stat == 6, 1, 0)
dat$unemployed[is.na(dat$p_work_stat)] <- NA
dat$part_time <- ifelse(dat$p_work_stat %in% c(2, 3), 1, 0)
dat$part_time[is.na(dat$p_work_stat)] <- NA
dat$retired <- ifelse(dat$p_work_stat == 5, 1, 0)
dat$retired[is.na(dat$p_work_stat)] <- NA

dat %>% count(gor)
dat$london <- ifelse(dat$gor == 7, 1, 0)
dat$southeast <- ifelse(dat$gor == 8, 1, 0)
dat$region_fct <- as.factor(dat$gor)

dat %>% 
  count(income, p_gross_household)

dat %>% count(income, p_gross_household, low_income)

dat %>% 
  count(low_income) %>% 
  na.omit() %>% 
  mutate(perc = n / sum(n))

dat %>% 
  count(social_housing, homeowner) %>% 
  mutate(perc = n / sum(n))

dat %>%
  filter(is.na(low_income)) %>% 
  count(social_housing, homeowner) %>% 
  mutate(perc = n/sum(n))

# model datasets ----------------------------------------------------

mod_dat <- dat %>% 
  select(id, income, p_gross_household, full_time, p_education_age, male, p_hh_size,
         single_household, disabled, edu_15, edu_16, edu_20plus,
         london, southeast, pri_job, pub_job,
         unemployed, retired, full_time, part_time, white_british,
         black, pakistan_bangladesh, age_raw, non_uk_born, social_housing,
         private_renting, own_mortgage, own_outright, homeowner,
         region_fct, e, d, c2, c1, b, cohabiting) %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  )

mod_dat %>% count(income, p_gross_household)

mod_dat <- mod_dat %>% 
  select(id, income, full_time, education_age, male, log_hh, disabled, unemployed,
         full_time, part_time, white_british, pakistan_bangladesh,
         log_age, non_uk_born, social_housing, private_renting,
         own_mortgage, own_outright, region_fct, e, d, c1, c2, b, cohabiting) %>% 
  na.omit()

# test and train: 0.9/0.1 ratio
set.seed(123)
train_set <- sample_frac(mod_dat, 0.9) %>% as.data.frame()
test_set <- mod_dat %>% filter(!id %in% train_set$id) %>% as.data.frame()

# Defining the training controls for multiple models -----------------------------

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final'
)

predictors <- c("full_time","education_age","male","log_hh","disabled","unemployed",
                "part_time","white_british","pakistan_bangladesh","log_age",
                "non_uk_born","social_housing","private_renting","own_mortgage",
                "own_outright","region_fct","e","d","c1","c2","b","cohabiting")

outcome_name <- "income"

# training lower layer models -------------------------------------------------

#Training the random forest model
model_rf <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='rf',
                  trControl=fitControl,
                  tuneLength=3)

saveRDS(model_rf, file = "model_rf_W22.RDS")

#Training the lm model
model_lm <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='lm',
                  trControl=fitControl,
                  tuneLength=3)

#Training the nn model
model_nn <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='nnet',
                  trControl=fitControl,
                  linout = TRUE,
                  tuneLength=3)

test_set$pred_rf <- predict(object = model_rf, test_set[,predictors])
test_set$pred_lm <- predict(object = model_lm, test_set[,predictors])
test_set$pred_nn <- predict(object = model_nn, test_set[,predictors])

#Predicting the out of fold prediction income for training data
train_set$OOF_pred_rf <- model_rf$pred$pred[order(model_rf$pred$rowIndex)]
train_set$OOF_pred_lm <- model_lm$pred$pred[order(model_lm$pred$rowIndex)]
train_set$OOF_pred_nn <- model_nn$pred$pred[order(model_nn$pred$rowIndex)]

#Predicting income for the test data
test_set$OOF_pred_rf <- predict(model_rf, test_set[predictors])
test_set$OOF_pred_lm <- predict(model_lm, test_set[predictors])
test_set$OOF_pred_nn <- predict(model_nn, test_set[predictors])

# ensemble model -----------------------------------------------------------

#Predictors for top layer models 
predictors_top <- c('OOF_pred_rf', 'OOF_pred_lm', 'OOF_pred_nn') 

#lm as top layer model 
model_el <- train(train_set[,predictors_top],
                  train_set[,outcome_name],
                  method = 'lm',
                  trControl = fitControl,
                  tuneLength = 3)

#lm as top layer model 
model_el2 <- train(train_set[,predictors_top],
                   train_set[,outcome_name],
                   method = 'nnet',
                   trControl = fitControl,
                   linout = TRUE,
                   tuneLength = 3)

#predict using lm top layer model
test_set$pred_el <- predict(model_el, test_set[,predictors_top])
test_set$pred_el2 <- predict(model_el2, test_set[,predictors_top])

test_set <- test_set %>% 
  mutate(
    pred_avg = (OOF_pred_rf + OOF_pred_lm + OOF_pred_nn)/3
  )

# RMSE on test data ----------------------------------------------------------

sqrt(mean((test_set$income - test_set$pred_rf)^2))
sqrt(mean((test_set$income - test_set$pred_lm)^2))
sqrt(mean((test_set$income - test_set$pred_nn)^2))
sqrt(mean((test_set$income - test_set$pred_el)^2))
sqrt(mean((test_set$income - test_set$pred_el2)^2))
sqrt(mean((test_set$income - test_set$pred_avg)^2))

#Predicting income for the whole dataset --------------------------------------

pred_df <- dat %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  ) %>% 
  select(id, all_of(predictors)) %>% 
  na.omit()

pred_df$OOF_pred_rf <- predict(model_rf, pred_df[predictors])
pred_df$OOF_pred_lm <- predict(model_lm, pred_df[predictors])
pred_df$OOF_pred_nn <- predict(model_nn, pred_df[predictors])
pred_df$pred_el <- predict(model_el2, pred_df[,predictors_top])

# distribution of predictions and real values ---------------------------------

pred_df %>% 
  ggplot() +
  geom_density(aes(x = pred_el), fill = "lightgrey", alpha = 0.5) +
  geom_density(data = dat, aes(x = income), fill = "lightblue", alpha = 0.5) +
  theme_bw()

# visualising distribution of residuals
pred_df %>% 
  left_join(mod_dat %>% select(id, income), by = "id") %>% 
  mutate(res = income - pred_el) %>% 
  ggplot(aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, colour = "black", fill = "lightgrey") +
  geom_density() +
  theme_bw()

# saving ensemble model ------------------------------------------------

income_preds_W22 <- pred_df %>% select(id, pred_el)

save(income_preds_W22, file = "income_preds_W22.RData")
