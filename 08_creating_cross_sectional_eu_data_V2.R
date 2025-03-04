##########################################################################
# creating cross-sectional dataset ---------------------------------------
##########################################################################

# packages ---------------------------------------------------------------

pacman::p_load(tidyverse, haven)

rm(list = ls())

# BES wave 22 ------------------------------------------------------------

df <- read_dta("data/panel_data/BES2019_W20_v24.0.dta")

# scaling functions --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

##############################################################
# level 1 vars -----------------------------------------------
##############################################################

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

# making DV with NAs
df %>% count(euPriorityBalance)
df$euPriorityBalance[df$euPriorityBalance == 9999] <- NA
df <- df %>% 
  rename(control_immi = euPriorityBalance)

# income predictions ----------------------------------------

load("income_preds_W20.RData")

##############################################################
# level 2 vars -----------------------------------------------
##############################################################

# affordability data ---------------------------------------------------------

# affordability ratio
afford <- read_csv("data/affordability_ratio_las.csv",
                   na = c(":", "NA"))

afford <- afford %>% 
  rename(la_code = `Local authority code`,
         affordability = `2020`) %>% 
  select(la_code, affordability) %>% 
  mutate(affordability_log = log(affordability))

# prices
prices <- read_csv("data/median_house_prices.csv")

names(prices) <- names(prices) %>% 
  str_remove_all("Year ending Sep") %>% 
  str_squish()

prices <- prices %>% 
  rename(la_code = `Local authority code`) %>% 
  mutate(prices = log(`2020`)) %>% 
  select(la_code, prices)

# merging
df <- df %>% 
  left_join(afford, by = "la_code") %>% 
  left_join(prices, by = "la_code")

df %>% 
  filter(is.na(affordability)) %>% 
  select(la_code) %>% 
  unique() %>% 
  as_vector() # missing = Scotland

# population data -----------------------------------------------------------

load("working/data/pop.RData")

pop <- pop |> 
  filter(year == 2020) |> 
  rename(la_code = oslaua_code,
         pop_sqm_2020 = pop_density) |> 
  select(la_code, pop_sqm_2020)

df <- df %>% 
  left_join(pop, by = "la_code")

# birth country ---------------------------------------------------

load("working/data/bc.RData")

bc <- bc %>%
  filter(year == 2020) %>%
  select(oslaua_code, foreign_per_1000)

df <- df %>% 
  left_join(bc, by = c("la_code" = "oslaua_code"))

## age of LAs --------------------------------------------------------

load("working/data/las_by_age.RData")

las_by_age <- las_by_age %>% 
  filter(year == 2020) %>% 
  select(-year)

df <- df %>% 
  left_join(las_by_age, by = c("la_code" = "oslaua_code"))  %>% 
  mutate(over_65_pct = ifelse(is.na(over_65_pct_post19),
                              over_65_pct_pre19, 
                              over_65_pct_post19),
         under_15_pct = ifelse(is.na(under_15_pct_post19),
                               under_15_pct_pre19, 
                               under_15_pct_post19)) %>% 
  select(-over_65_pct_post19, -over_65_pct_pre19,
         -under_15_pct_post19, -under_15_pct_pre19)

# education ---------------------------------------

edu <- read_csv("data/census_education.csv",
                na = c("x","NA"))

edu <- edu %>% 
  rename(la_code = `Area code`,
         degree_pct = `Level 4 qualifications and above (percent)`) |>  
  select(la_code, degree_pct) |> 
  mutate(degree_pct = degree_pct/100)

df <- df %>% 
  left_join(edu, by = "la_code")

# region --------------------------------------------------------------------

region <- read_csv("data/lasregionew2021lookup.csv")

region <- region %>% 
  rename(la_code = `LA code`,
         region_code = `Region code`) %>% 
  select(la_code, region_code)

df <- df %>% 
  left_join(region, by = "la_code")

# scaling variables --------------------------------------------------------

# renaming originals
level_twos <- df %>% select(affordability:degree_pct) %>% names()
rename_raw <- function(df, vars){
  df <- df %>% 
    mutate(across({{vars}}, \(x) x = x, .names = "{.col}_raw"))
  return(df)
}

df <- df %>% rename_raw(all_of(level_twos))

# scaling
df[level_twos] <- df[level_twos] %>%
  map_df(scale_this)

# selecting variables --------------------------------------------------

df <- df %>% 
  left_join(income_preds_W20, by = "id") %>% 
  rename(income_preds = pred_el) %>% 
  select(id, control_immi, la_code, male, uni, white_british,  
         no_religion, c1_c2, d_e, 
         own_outright, own_mortgage, social_housing, private_renting,
         age, age_raw, non_uk_born, homeowner, edu_20plus, income, 
         full_time, all_of(level_twos), contains("raw"), 
         region_code, income_preds) %>% 
  mutate(income_full = ifelse(is.na(income), income_preds, income))

# saving data -------------------------------------------------------

save(df, file = "working/data/cross_sectional_df_eu.RData")

# summary statistics --------------------------------------------------

summ_vars <- c("male","uni","social_housing","homeowner","private_renting",
               "white_british","no_religion","edu_20plus","age_raw","c1_c2","d_e",
               "non_uk_born","uni","income","foreign_per_1000_raw",
               "over_65_pct_raw","under_15_pct_raw","over_65_pct_raw","degree_pct_raw",
               "pop_sqm_2020_raw","affordability_raw","prices_raw", "income_full",
               "control_immi")

summ_df <- df |> 
  filter(str_detect(la_code, "^S")==FALSE) |>
  select(all_of(summ_vars))

summ_table <- summ_df |>  
  summarise(across(all_of(summ_vars), 
                   list(mean = ~mean(.,na.rm = T),
                        sd = ~sd(., na.rm = T)),
                   .names = "{.col}.{.fn}")) |> 
  pivot_longer(everything(),
               names_to = c("var","stat"),
               values_to = "value",
               names_sep = "\\.") |>
  pivot_wider(names_from = "stat", values_from = "value") |> 
  mutate(na = summ_df |> map_int(~sum(is.na(.))))

write.csv(summ_table, "summary_statistics_W20.csv")  
