pacman::p_load(tidyverse, haven, jtools, randomForest, caret)

rm(list = ls())

df <- read_dta("data/panel_data/BES2019_W20_v24.0.dta")

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# ind vars ------------------------------------------

# p_edlevel, p_ethnicity, p_religion, p_housing, age, 
# p_socgrade, p_country_birth, p_past_vote_2019
# ind vars
df <- df %>% 
  mutate(
    uni = fct_collapse(
      as.factor(p_edlevel),
      "1" = c("4","5"),
      "0" = c("0","1","2","3")
    ) %>% 
      as.character() %>% 
      parse_double(),
    white_british = fct_lump_n(as.factor(p_ethnicity), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    no_religion = fct_lump_n(as.factor(p_religion), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    soc_class = fct_collapse(
      as.factor(p_socgrade),
      "A-B" = c("1","2"),
      "C1-C2" = c("3","4"),
      "D-E" = c("5","6"),
      "Other" = c("8")
    ),
    non_uk_born = fct_lump_n(as.factor(p_country_birth), n = 1) %>% 
      fct_recode("0" = "Other") %>% 
      as.character() %>% 
      parse_double(),
    tory_2019 = fct_lump_n(as.factor(p_past_vote_2019), n = 1)
  ) %>% 
  rename(la_code = oslaua_code)

df$male <- ifelse(df$gender == 1, 1, 0)
df$soc_class[df$soc_class == "Other"] <- NA
df$c1_c2 <- ifelse(df$soc_class == "C1-C2", 1, 0)
df$d_e <- ifelse(df$soc_class == "D-E", 1, 0)
df$income <- ifelse(df$p_gross_household %in% c(16, 17), 
                    NA, df$p_gross_household)
df$own_outright <- ifelse(df$p_housing == 1, 1, 0)
df$private_renting <- ifelse(df$p_housing == 4, 1, 0)
df$social_housing <- ifelse(df$p_housing == 5|df$p_housing == 6, 1, 0)
df$own_mortgage <- ifelse(df$p_housing == 2, 1, 0)
df$homeowner <- ifelse(df$own_outright == 1 | df$own_mortgage == 1, 1, 0)
df$tory_2019 <- ifelse(df$p_turnout_2019 == 0|df$tory_2019 == "Other", 0, 1)
df$tory_2019[df$p_past_vote_2019 == 9999] <- NA
df$non_voter <- df$p_turnout_2019 == 0
df$non_voter[df$p_turnout_2019 == 9999] <- NA
df$immigSelf[df$immigSelf == 9999] <- NA
df$age_raw <- df$age
df$age <- scale_this(df$age)
df$edu_20plus <- ifelse(df$p_education_age == 5, 1, 0)
df$edu_20plus[is.na(df$p_education_age)] <- NA

# vars for income predictions
df$full_time <- ifelse(df$p_work_stat == 1, 1, 0)
df$disabled <- ifelse(df$p_disability %in% c(1, 2), 1, 0)
df$disabled[is.na(df$p_disability)] <- NA
df$p_hh_size <- ifelse(df$p_hh_size %in% c(9, 10), 
                       NA, df$p_hh_size)
df$cohabiting <- ifelse(df$p_marital %in% c(1, 2, 4), 1, 0)
df$cohabiting[is.na(df$p_marital)] <- NA
df$e <- ifelse(df$p_socgrade == 6, 1, 0)
df$d <- ifelse(df$p_socgrade == 5, 1, 0)
df$c2 <- ifelse(df$p_socgrade == 4, 1, 0)
df$c1 <- ifelse(df$p_socgrade == 3, 1, 0)
df$b <- ifelse(df$p_socgrade == 2, 1, 0)
df$pakistan_bangladesh <- ifelse(df$p_ethnicity %in% c(8, 9), 1, 0)
df$unemployed <- ifelse(df$p_work_stat == 6, 1, 0)
df$unemployed[is.na(df$p_work_stat)] <- NA
df$part_time <- ifelse(df$p_work_stat %in% c(2, 3), 1, 0)
df$part_time[is.na(df$p_work_stat)] <- NA
df$region_fct <- as.factor(df$gor)

df %>% count(male, gender)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(soc_class, c1_c2, d_e, p_socgrade)
df %>% count(income, p_gross_household)
df %>% count(p_housing, own_outright, own_mortgage, homeowner, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(non_voter, p_turnout_2019)
df %>% count(edu_20plus, p_education_age)
df %>% count(immigSelf)
df %>% count(full_time, p_work_stat)
df %>% count(disabled, p_disability)
df %>% count(cohabiting, p_marital)

df <- df %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  )

df %>% count(education_age, p_education_age)
df %>% count(log_hh, p_hh_size)

# model ---------------------------------------------

mod_dat <- df %>% 
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

saveRDS(model_rf, file = "model_rf_W20.RDS")

#Training the lm model
model_lm <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='lm',
                  trControl=fitControl)

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

#predict using lm and nnet top layer models
test_set$pred_el <- predict(model_el, test_set[,predictors_top])

# RMSE on test data ----------------------------------------------------------

sqrt(mean((test_set$income - test_set$pred_rf)^2))
sqrt(mean((test_set$income - test_set$pred_lm)^2))
sqrt(mean((test_set$income - test_set$pred_nn)^2))
sqrt(mean((test_set$income - test_set$pred_el)^2))

#Predicting income for the whole dataset --------------------------------------

pred_df <- df %>% 
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
pred_df$pred_el <- predict(model_el, pred_df[,predictors_top])

# distribution of predictions and real values ---------------------------------

pred_df %>% 
  ggplot() +
  geom_density(aes(x = pred_el), fill = "lightgrey", alpha = 0.5) +
  geom_density(data = df, aes(x = income), fill = "lightblue", alpha = 0.5) +
  theme_bw()

# visualising distribution of residuals
pred_df %>% 
  left_join(df %>% select(id, income), by = "id") %>% 
  mutate(res = income - pred_el) %>% 
  ggplot(aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, colour = "black", fill = "lightgrey") +
  geom_density() +
  theme_bw()

# saving ensemble model ------------------------------------------------

income_preds_W20 <- pred_df %>% select(id, pred_el)

save(income_preds_W20, file = "income_preds_W20.RData")
