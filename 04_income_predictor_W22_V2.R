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

# test and train set ---------------------------------------------

outcome <- mod_dat$income
predictors <- c("full_time","education_age","male","log_hh","disabled","unemployed",
                "part_time","white_british","pakistan_bangladesh","log_age",
                "non_uk_born","social_housing","private_renting","own_mortgage",
                "own_outright","region_fct","e","d","c1","c2","b","cohabiting")
mod_predictors <- mod_dat[,predictors]
set.seed(123)
in_train <- createDataPartition(outcome, p = 0.9, list = F)
train_predictors <- mod_predictors[in_train,] |> as.data.frame()
test_predictors <- mod_predictors[-in_train,] |> as.data.frame()
train_outcome <- outcome[in_train]
test_outcome <- outcome[-in_train]

# scaling and centering
x_trans <- preProcess(train_predictors)
train_predictors <- predict(x_trans, train_predictors)
test_predictors <- predict(x_trans, test_predictors)
mod_predictors <- predict(x_trans, mod_predictors)

# Defining the training controls for multiple models -----------------------------

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final'
)

# training lower layer models -------------------------------------------------

#Training the random forest model
model_rf <- train(train_predictors,
                  train_outcome,
                  method='rf',
                  trControl=fitControl,
                  tuneLength=3)

saveRDS(model_rf, file = "model_rf_W22.RDS")

#Training the lm model
model_lm <- train(train_predictors,
                  train_outcome,
                  method='lm',
                  trControl=fitControl)

#Training the lasso model
region_df <- model.matrix(train_outcome ~ train_predictors[,"region_fct"])[,-1]
colnames(region_df) <- str_c(seq(2,11,1))
edu_df <- model.matrix(train_outcome ~ train_predictors[,"education_age"])[,-1]
colnames(edu_df) <- str_c(str_c("edu",seq(2,7,1)))

ls_train_predictors <- cbind(train_predictors[,-c(2,16,23,24,25)],region_df,edu_df)
ls_train_predictors |> map_chr(class)

lasso_grid <- data.frame(fraction = seq(.5, 1, length = 10))
set.seed(123)
model_ls <- train(ls_train_predictors,
                  train_outcome,
                  method='lasso',
                  tuneGrid = lasso_grid,
                  trControl=fitControl)

model_ls # lasso fraction = 1, therefore equivalent to OLS

#Training the nn model
my_grid <- expand.grid(.decay = c(0.9, 0.5, 0.1), .size = c(5:10))

model_nn <- train(train_predictors,
                  train_outcome,
                  method='nnet',
                  tuneGrid = my_grid,
                  trControl=fitControl,
                  linout = TRUE,
                  maxit = 1000)
model_nn

saveRDS(model_nn, file = "model_nn_W22.RDS")

test_predictors$pred_rf <- predict(object = model_rf, test_predictors)
test_predictors$pred_lm <- predict(object = model_lm, test_predictors)
test_predictors$pred_nn <- predict(object = model_nn, test_predictors)

#Predicting the out of fold prediction non-decent % for training data
train_predictors$OOF_pred_rf <- model_rf$pred$pred[order(model_rf$pred$rowIndex)]
train_predictors$OOF_pred_lm <- model_lm$pred$pred[order(model_lm$pred$rowIndex)]
train_predictors$OOF_pred_nn <- model_nn$pred$pred[order(model_nn$pred$rowIndex)]

#Predicting non-decent % for the test data
test_predictors$OOF_pred_rf <- predict(model_rf, test_predictors)
test_predictors$OOF_pred_lm <- predict(model_lm, test_predictors)
test_predictors$OOF_pred_nn <- predict(model_nn, test_predictors)

# ensemble model -----------------------------------------------------------

#Predictors for top layer models 
predictors_top <- c('OOF_pred_rf', 'OOF_pred_lm', 'OOF_pred_nn') 

#lm as top layer model 
model_elm <- train(train_predictors[,predictors_top],
                   train_outcome,
                   method = 'lm',
                   trControl = fitControl)

#predict using lm top layer model
test_predictors$pred_elm <- predict(model_elm, test_predictors)

# RMSE on test data ----------------------------------------------------------

sqrt(mean((test_outcome - test_predictors$pred_rf)^2))
sqrt(mean((test_outcome - test_predictors$pred_lm)^2))
sqrt(mean((test_outcome - test_predictors$pred_nn)^2))
sqrt(mean((test_outcome - test_predictors$pred_elm)^2))

#Predicting income for the whole dataset --------------------------------------

pred_df <- dat %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  ) %>% 
  select(id, all_of(predictors)) %>% 
  na.omit()

pred_df <- predict(x_trans, pred_df)
pred_df$OOF_pred_rf <- predict(model_rf, pred_df[predictors])
pred_df$OOF_pred_lm <- predict(model_lm, pred_df[predictors])
pred_df$OOF_pred_nn <- predict(model_nn, pred_df[predictors])
pred_df$pred_elm <- predict(model_elm, pred_df[,predictors_top])

# distribution of predictions and real values ---------------------------------

pred_df %>% 
  ggplot() +
  geom_density(aes(x = pred_elm), fill = "lightgrey", alpha = 0.5) +
  geom_density(data = dat, aes(x = income), fill = "lightblue", alpha = 0.5) +
  theme_bw()

# visualising distribution of residuals
pred_df %>% 
  left_join(mod_dat %>% select(id, income), by = "id") %>% 
  mutate(res = income - pred_elm) %>% 
  ggplot(aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, colour = "black", fill = "lightgrey") +
  geom_density() +
  theme_bw()

# saving ensemble model ------------------------------------------------

income_preds_W22 <- pred_df |> 
  rename(pred_el = pred_elm) |> 
  select(id, pred_el)

save(income_preds_W22, file = "income_preds_W22.RData")
