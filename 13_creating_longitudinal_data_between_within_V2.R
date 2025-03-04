################################################################
# creating longitudinal dataset -------------------------------
################################################################

pacman::p_load(tidyverse, haven, caret)

rm(list = ls())

# scaling function ------------------------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

# datasets -------------------------------------------------------------------
# w10 nov-dec 2016
# w11 april-may 2017
# w14 may 2018
# w17 nov 2019
# w20 jun 2020
# w22 nov-dec 2021

wave_year <- seq(2016, 2021, 1)
data_files <- c("BES2015_W10_v24.0.dta",
                "BES2015_W11_v24.0.dta",
                "BES2017_W14_v24.0.dta",
                "BES2019_W17_v24.0.dta",
                "BES2019_W20_v24.0.dta",
                "BES2019_W22_v24.0.dta")

file_path <- "data/panel_data/"
vars <- c("id", "immigSelf", "p_edlevel", "p_education_age", 
          "p_ethnicity", "p_religion", "p_socgrade", 
          "p_country_birth", "p_gross_household",
          "p_housing", "age", "gender", "oslaua_code","gor","year",
          "p_gross_household", "p_work_stat", "p_disability",
          "p_hh_size", "p_marital")

w10 <- read_dta(file = str_c(file_path, data_files[1]))
w10$year <- wave_year[1]
w10 <- w10 %>% 
  select(all_of(vars))

w11 <- read_dta(file = str_c(file_path, data_files[2]))
w11$year <- wave_year[2]
w11 <- w11 %>% 
  select(all_of(vars))

w14 <- read_dta(file = str_c(file_path, data_files[3]))
w14$year <- wave_year[3]
w14 <- w14 %>% 
  select(all_of(vars))

w17 <- read_dta(file = str_c(file_path, data_files[4]))
w17$year <- wave_year[4]
w17 <- w17 %>% 
  select(all_of(vars))

w20 <- read_dta(file = str_c(file_path, data_files[5]))
w20$year <- wave_year[5]
w20 <- w20 %>% 
  select(all_of(vars))

w22 <- read_dta(file = str_c(file_path, data_files[6]))
w22$year <- wave_year[6]
w22 <- w22 %>% 
  select(all_of(vars))

df <- bind_rows(w10, w11, w14, w17, w20, w22)

rm(w10, w11, w14, w17, w20, w22)

# filtering out those without la -------------------------------------------------------

df <- df %>% filter(oslaua_code!= "")

# people who moved -------------------------------------------------------------------

# filters for presence in each wave
ids_2016 <- df$id[df$year == 2016]
ids_2017 <- df$id[df$year == 2017]
ids_2018 <- df$id[df$year == 2018]
ids_2019 <- df$id[df$year == 2019]
ids_2020 <- df$id[df$year == 2020]
ids_2021 <- df$id[df$year == 2021]

# people in all at least three waves
df <- df %>% 
  mutate(
    in_2016 = ifelse(id %in% ids_2016, 1, 0),
    in_2017 = ifelse(id %in% ids_2017, 1, 0),
    in_2018 = ifelse(id %in% ids_2018, 1, 0),
    in_2019 = ifelse(id %in% ids_2019, 1, 0),
    in_2020 = ifelse(id %in% ids_2020, 1, 0),
    in_2021 = ifelse(id %in% ids_2021, 1, 0)
  ) %>% 
  mutate(wave_n = select(., in_2016:in_2021) %>%  rowSums(na.rm = T),
         gor = as_factor(gor)) %>% 
  arrange(id, year) %>% 
  filter(wave_n >= 3)

nrow(df) # 125,777

movers <- df |> 
  group_by(id, wave_n, oslaua_code) |> 
  summarise(
    la_count = n(),
    .groups = "drop"
  ) |> 
  mutate(moved = ifelse(la_count == wave_n, FALSE, TRUE))

df <- df |> 
  left_join(movers |> select(id, moved) |> unique(), 
            by = "id") |> 
  filter(moved == FALSE)

nrow(df) # 113,419

# affordability ---------------------------------------------------------

afford <- read_csv("data/affordability_ratio_las.csv",
                   na = c(":", "NA"))

year_range <- as.character(seq(2016,2021,1))
afford <- afford %>% 
  rename(oslaua_code = `Local authority code`) %>% 
  select(oslaua_code, all_of(year_range)) %>% 
  pivot_longer(cols = all_of(year_range),
               names_to = "year",
               values_to = "affordability") %>% 
  group_by(oslaua_code) %>% 
  mutate(year = as.double(year),
         affordability_log = log(affordability),
         affordability_mean = mean(affordability, na.rm = T),
         affordability_within = affordability - affordability_mean,
         affordability_log_mean = mean(affordability_log, na.rm = T),
         affordability_log_within = affordability_log - affordability_log_mean) %>% 
  ungroup()

prices <- read_csv("data/median_house_prices.csv")
names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(oslaua_code = `Local authority code`) %>% 
  select(oslaua_code, all_of(year_range)) %>% 
  pivot_longer(cols = all_of(year_range),
               names_to = "year",
               values_to = "prices") %>% 
  group_by(oslaua_code) %>%
  mutate(year = as.double(year),
         prices = log(prices),
         prices_mean = mean(prices, na.rm = T),
         prices_within = prices - prices_mean) %>% 
  ungroup(oslaua_code) 

df <- df %>% 
  left_join(afford, by = c("oslaua_code","year")) %>% 
  left_join(prices, by = c("oslaua_code","year"))

# population data -----------------------------------------------------------

load("working/data/pop.RData")

pop <- pop %>%
  filter(year != 2014 & year != 2015) %>% 
  select(-la_name) %>% 
  group_by(oslaua_code) %>% 
  mutate(pop_density_mean = mean(pop_density, na.rm = T),
         pop_density_within = pop_density - pop_density_mean) %>% 
  ungroup()
  
df <- df %>% 
  left_join(pop, by = c("oslaua_code", "year"))

# birth country data -----------------------------------------------------------

load("working/data/bc.RData")

bc |> count(year)

bc <- bc %>% 
  select(oslaua_code, year, foreign_per_1000) %>% 
  group_by(oslaua_code) %>% 
  mutate(foreign_per_1000_mean = mean(foreign_per_1000, na.rm = T),
         foreign_per_1000_within = foreign_per_1000 - foreign_per_1000_mean) %>% 
  ungroup()

bc |> count(year)

df <- df %>% left_join(bc, by = c("oslaua_code","year"))

# age data -------------------------------------------------------------

load("working/data/las_by_age.RData")

df <- df %>% 
  left_join(las_by_age, by = c("oslaua_code","year"))  %>% 
  mutate(over_65_pct = ifelse(is.na(over_65_pct_post19),
                              over_65_pct_pre19, 
                              over_65_pct_post19),
         under_15_pct = ifelse(is.na(under_15_pct_post19),
                               under_15_pct_pre19, 
                               under_15_pct_post19)) %>% 
  select(-over_65_pct_post19, -over_65_pct_pre19,
         -under_15_pct_post19, -under_15_pct_pre19) %>% 
  group_by(oslaua_code) %>% 
  mutate(over_65_pct_mean = mean(over_65_pct, na.rm = T),
         over_65_pct_within = over_65_pct - over_65_pct_mean,
         under_15_pct_mean = mean(under_15_pct, na.rm = T),
         under_15_pct_within = under_15_pct - under_15_pct_mean) %>% 
  ungroup()

## education data -------------------------------------------------------

edu <- read_csv("data/census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(oslaua_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) %>% 
  select(oslaua_code, degree_pct)

census_recon <- read_csv("data/census_reconciliation.csv")

edu_2011 <- read_csv("data/degrees_2011.csv")

edu_bind <- edu_2011 %>% 
  rename(code_2011 = 3,
         total = 5,
         degree = 11) %>% 
  left_join(census_recon, by = "code_2011") %>% 
  filter(!is.na(code_2021)) %>% 
  arrange(code_2021) %>% 
  group_by(code_2021) %>% 
  summarise(total = sum(total),
            degrees = sum(degree),
            .groups = "drop") %>% 
  mutate(degree_pct = degrees / total) %>% 
  select(code_2021, degree_pct) %>% 
  rename(oslaua_code = code_2021)

edu_2011 <- edu_2011 %>% 
  rename(oslaua_code = 3,
         total = 5,
         degree = 11) %>% 
  mutate(degree_pct = (degree / total)*100) %>% 
  select(oslaua_code, degree_pct) %>% 
  bind_rows(edu_bind) %>% 
  rename(degree_pct_2011 = degree_pct)

edu_merge <- edu |> 
  left_join(edu_2011, by = "oslaua_code") |> 
  rename(degree_pct_2021 = degree_pct) |> 
  mutate(
    k = ifelse(degree_pct_2021 >= degree_pct_2011, 1, 0),
    a = degree_pct_2011 - k,
    b = (degree_pct_2021 - k)/(degree_pct_2011 - k),
    xi_2016 = (2016-2011)/(2021-2011),
    xi_2017 = (2017-2011)/(2021-2011),
    xi_2018 = (2018-2011)/(2021-2011),
    xi_2019 = (2019-2011)/(2021-2011),
    xi_2020 = (2020-2011)/(2021-2011),
    degree_pct_2016 = k + (a * (b ^ xi_2016)),
    degree_pct_2017 = k + (a * (b ^ xi_2017)),
    degree_pct_2018 = k + (a * (b ^ xi_2018)),
    degree_pct_2019 = k + (a * (b ^ xi_2019)),
    degree_pct_2020 = k + (a * (b ^ xi_2020))
  ) |> 
  select(oslaua_code, contains("degree_pct")) |> 
  pivot_longer(
    cols = degree_pct_2021:degree_pct_2020,
    names_to = c("var","year"),
    values_to = "degree_pct",
    names_sep = "t_"
  ) |> 
  mutate(year = as.integer(year)) |> 
  filter(year != 2011) |> 
  select(-var) |> 
  arrange(oslaua_code, year) |> 
  group_by(oslaua_code) %>% 
  mutate(degree_pct_mean = mean(degree_pct, na.rm = T),
         degree_pct_within = degree_pct - degree_pct_mean) %>% 
  ungroup()

edu_merge

df <- df %>% 
  left_join(edu_merge, by = c("oslaua_code","year"))

# percentage manufacturing employment --------------------------------------

indus_2016 <- read_csv("data/2016_industry_employment.csv")

indus_clean <- function(df, year_dbl){
  out <- df %>% 
    filter(str_detect(Area,"ladu")) %>% 
    rename(oslaua_code = mnemonic,
           manufacturing = 5) %>% 
    select(oslaua_code:23) %>% 
    pivot_longer(
      cols = 2:22,
      names_to = "industry",
      values_to = "employment"
    ) %>% 
    group_by(oslaua_code) %>% 
    mutate(total_employment = sum(employment),
           manuf_pct = employment / total_employment,
           year = year_dbl) %>% 
    ungroup() %>% 
    filter(industry == "manufacturing")
  return(out)
}

indus_2016 <- indus_clean(indus_2016, 2016)

indus_2017 <- read_csv("data/2017_industry_employment.csv")
indus_2017 <- indus_clean(indus_2017, 2017)

indus_2018 <- read_csv("data/2018_industry_employment.csv")
indus_2018 <- indus_clean(indus_2018, 2018)

indus_2019 <- read_csv("data/2019_industry_employment.csv")
indus_2019 <- indus_clean(indus_2019, 2019)

indus_2020 <- read_csv("data/2020_industry_employment.csv")
indus_2020 <- indus_clean(indus_2020, 2020)

indus_2021 <- read_csv("data/2021_industry_employment.csv")
indus_2021 <- indus_clean(indus_2021, 2021)

manuf_pct <- bind_rows(
  indus_2016, indus_2017, indus_2018,
  indus_2019, indus_2020, indus_2021
  ) %>% 
  select(oslaua_code, manuf_pct, year) %>% 
  group_by(oslaua_code) %>% 
  mutate(manuf_pct_mean = mean(manuf_pct, na.rm = T),
         manuf_pct_within = manuf_pct - manuf_pct_mean) %>% 
  ungroup()

df <- df %>% 
  left_join(manuf_pct, by = c("oslaua_code","year"))

rm(indus_2016, indus_2017, indus_2018, indus_2019, indus_2020, indus_2021)

## centering, scaling and between-within calculations LA vars -------------------

# centering year
df <- df %>% 
  mutate(year_c = year - 2016)

scaling_vars <- df %>% 
  select(affordability:manuf_pct_within) %>% 
  names()

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

df[scaling_vars] <- df[scaling_vars] %>%
  map_df(scale_this)

## creating person variables ------------------------------------------------

df$uni <- ifelse(
  df$p_edlevel == 4|df$p_edlevel == 5, 1, 0
)

df$male <- ifelse(df$gender == 1, 1, 0)

df$white_british <- ifelse(
  df$p_ethnicity == 1, 1, 0
)

df$white_british[df$p_ethnicity == 16] <- NA

df$no_religion <- ifelse(
  df$p_religion == 1, 1, 0
)

df$c1_c2 <- ifelse(
  df$p_socgrade == 3 | df$p_socgrade == 4, 1, 0
)
df$c1_c2[df$p_socgrade == 7|df$p_socgrade == 8] <- NA

df$d_e <- ifelse(
  df$p_socgrade == 5 | df$p_socgrade == 6, 1, 0
)
df$d_e[df$p_socgrade == 7|df$p_socgrade == 8] <- NA

df$non_uk_born <- ifelse(
  df$p_country_birth != 1, 1, 0
)
df$non_uk_born[df$p_country_birth == 9999] <- NA

df$own_outright <- ifelse(
  df$p_housing == 1, 1, 0
)

df$own_mortgage <- ifelse(
  df$p_housing == 2, 1, 0
)

df$homeowner <- ifelse(
  df$p_housing == 1|df$p_housing == 2, 1, 0
)

df$private_renting <- ifelse(
  df$p_housing == 4, 1, 0
)

df$social_housing <- ifelse(
  df$p_housing == 5|df$p_housing == 6, 1, 0
)

df$edu_20plus <- ifelse(
  df$p_education_age == 5, 1, 0
  )
df$edu_20plus[is.na(df$p_education_age)] <- NA

df %>% count(male, gender)
df %>% count(uni, p_edlevel)
df %>% count(white_british, p_ethnicity)
df %>% count(no_religion, p_religion)
df %>% count(c1_c2, d_e, p_socgrade)
df %>% count(p_housing, homeowner, social_housing, private_renting)
df %>% count(non_uk_born, p_country_birth)
df %>% count(edu_20plus, p_education_age)

# scaling age
df$age_raw <- df$age
df$age <- scale_this(df$age)

df %>% 
  summarise(mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T))

# income predictions datasets ----------------------------------

# additional predictor vars
df$income <- ifelse(df$p_gross_household %in% c(16, 17), 
                     NA, df$p_gross_household)
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

df %>% count(gor)
df$region_fct <- as.factor(df$gor)
df$year_fct <- as.factor(df$year_c)

df %>% 
  count(income, p_gross_household)

# modelling dataframe
mod_dat <- df %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
    ) %>%
  select(id, income, full_time, education_age, male, log_hh, disabled, unemployed,
         full_time, part_time, white_british, pakistan_bangladesh,
         log_age, non_uk_born, social_housing, private_renting,
         own_mortgage, own_outright, region_fct, e, d, c1, c2, b, cohabiting,
         year_fct) %>%
  na.omit()

# test and train: 0.9/0.1 ratio
set.seed(123)
train_set <- sample_frac(mod_dat, 0.9) %>% as.data.frame()
test_set <- mod_dat %>% filter(!id %in% train_set$id) %>% as.data.frame()

# ensemble model for imputation -------------------------------------------------

# Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final'
)

predictors <- c("full_time","education_age","male","log_hh","disabled","unemployed",
                "part_time","white_british","pakistan_bangladesh","log_age",
                "non_uk_born","social_housing","private_renting","own_mortgage",
                "own_outright","region_fct","e","d","c1","c2","b","cohabiting",
                "year_fct")

outcome_name <- "income"

# training lower layer models
#Training the lm model
model_lm <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='lm',
                  trControl=fitControl,
                  tuneLength=3)

#Training the nn model
set.seed(123)
model_nn <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='nnet',
                  trControl=fitControl,
                  linout = TRUE,
                  tuneLength=3)

# test predictions
test_set$pred_lm <- predict(object = model_lm, test_set[,predictors])
test_set$pred_nn <- predict(object = model_nn, test_set[,predictors])

#Predicting the out of fold prediction income for training data
train_set$OOF_pred_lm <- model_lm$pred$pred[order(model_lm$pred$rowIndex)]
train_set$OOF_pred_nn <- model_nn$pred$pred[order(model_nn$pred$rowIndex)]

#Predicting income for the test data
test_set$OOF_pred_lm <- predict(model_lm, test_set[predictors])
test_set$OOF_pred_nn <- predict(model_nn, test_set[predictors])

# ensemble model
#Predictors for top layer models 
predictors_top <- c('OOF_pred_lm', 'OOF_pred_nn') 

#lm as top layer model 
model_el <- train(train_set[,predictors_top],
                  train_set[,outcome_name],
                  method = 'lm',
                  trControl = fitControl,
                  tuneLength = 3)

#lm as top layer model 
set.seed(123)
model_el2 <- train(train_set[,predictors_top],
                   train_set[,outcome_name],
                   method = 'nnet',
                   trControl = fitControl,
                   linout = TRUE,
                   tuneLength = 3)

#predict using lm top layer model
test_set$pred_el <- predict(model_el, test_set[,predictors_top])
test_set$pred_el2 <- predict(model_el2, test_set[,predictors_top])

# RMSE on test data 
sqrt(mean((test_set$income - test_set$pred_lm)^2))
sqrt(mean((test_set$income - test_set$pred_nn)^2))
sqrt(mean((test_set$income - test_set$pred_el)^2))
sqrt(mean((test_set$income - test_set$pred_el2)^2))

#Predicting income for the whole dataset --------------------------------------

pred_df <- df %>% 
  mutate(
    education_age = as.factor(p_education_age),
    log_age = log(age_raw),
    log_hh = log(p_hh_size)
  ) %>% 
  select(id, all_of(predictors)) %>% 
  na.omit()

pred_df$OOF_pred_lm <- predict(model_lm, pred_df[predictors])
pred_df$OOF_pred_nn <- predict(model_nn, pred_df[predictors])
pred_df$pred_el <- predict(model_el2, pred_df[,predictors_top])

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

income_preds_long <- pred_df %>% select(id, year_fct, pred_el)
save(income_preds_long, file = "income_preds_long.RData")

# commented code to load pre-saved income predictions
# load(file = "income_preds_long.RData")

# income full
df <- df %>% 
  left_join(income_preds_long, by = c("id","year_fct")) %>% 
  rename(income_preds = pred_el) %>% 
  mutate(income_full = ifelse(is.na(income), income_preds, income))

sum(is.na(df$income_preds))
sum(is.na(df$income_full))
sum(is.na(df$income))

# removing don't knows -------------------------------------------------------

df %>% 
  count(immigSelf)

df$immigSelf[df$immigSelf == 9999] <- NA

df %>% 
  count(immigSelf)

# reordering immigSelf -----------------------------------

df <- df %>%
  mutate(
    immigpro = immigSelf,
    immigSelf = 10 - immigpro
  )

df %>% count(immigSelf, immigpro)

df$id <- as.factor(df$id)

# selecting vars and saving ---------------------------------------------

# producing dataset
immig_df <- df %>% 
  select(immigSelf, all_of(scaling_vars), male, uni, edu_20plus, 
         white_british, no_religion, c1_c2, d_e, non_uk_born, income_full,
         homeowner, private_renting, social_housing, year_c, region_fct,
         oslaua_code, id) 

names(immig_df)

save(immig_df, file = "working/data/longitudinal_df.RData")
