pacman::p_load(tidyverse, haven, jtools)

rm(list = ls())

df <- read_dta("data/panel_data/BES2019_W23_v25.0.dta")

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

mod_df <- df %>% 
  select(income, full_time, education_age, male, log_hh, disabled, unemployed,
         full_time, part_time, white_british, pakistan_bangladesh,
         log_age, non_uk_born, social_housing, private_renting,
         own_mortgage, own_outright, region_fct, e, d, c1, c2, b, cohabiting) %>% 
  na.omit()

mincer_mod <- lm(income ~ .,
                 data = mod_df)

summary(mincer_mod)

# saving lm model ------------------------------------------------

saveRDS(mincer_mod, file = "working/models/lm_income_w23.RDS")
