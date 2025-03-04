pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, lfe)

rm(list = ls())

########################################################################
# immigration ----------------------------------------------------------
########################################################################

# loading data
load("working/data/longitudinal_df.RData")

# saving a df for later application of filters prior to robustness checks
df <- immig_df %>% 
  rename(LAD = oslaua_code) %>% 
  mutate(social_housing.affordability_mean = social_housing * affordability_mean,
         homeowner.affordability_mean = homeowner * affordability_mean)

# missing observations -------------------------------

immig_df %>% map_int(~sum(is.na(.)))

missing_las <- function(df, x){
  step1 <- df %>% 
    mutate(nas = is.na({{x}})) %>% 
    filter(nas == T) %>% 
    count(oslaua_code) %>% 
    arrange(desc(n))
  out <- step1$n
  names(out) <- step1$oslaua_code
  return(out)
}

# missing affordability is Scotland and City of London
missing_las(immig_df, affordability)

# missing degree pct is Scotland
missing_las(immig_df, degree_pct)

# missing foreign_per_1000 is Scotland and City of London
missing_las(immig_df, foreign_per_1000)

# dataset for rewb models including degree_pct_within
rewb_df <- immig_df %>% 
  select(-prices, -prices_mean, -prices_within,
         -uni, -income_full) %>% 
  rename(LAD = oslaua_code) %>% 
  mutate(social_housing.affordability_mean = social_housing * affordability_mean,
         homeowner.affordability_mean = homeowner * affordability_mean) %>% 
  na.omit()

# dataset for main model
immig_df <- immig_df %>% 
  select(-prices, -prices_mean, -prices_within,
         -uni, -income_full, -degree_pct_within, -degree_pct) %>% 
  rename(LAD = oslaua_code) %>% 
  mutate(social_housing.affordability_mean = social_housing * affordability_mean,
         homeowner.affordability_mean = homeowner * affordability_mean) %>% 
  na.omit()

nrow(df) - nrow(immig_df)

# dataset for robustness with prices
immig_df_price <- df %>% 
  select(-affordability, 
         -affordability_log, -affordability_log_mean,
         -affordability_mean, -affordability_within,
         -affordability_log_within, -uni, -income_full, -degree_pct_within, -degree_pct) %>% 
  na.omit()

# dataset for uni model
immig_df_uni <- df %>% 
  select(-prices, -prices_mean, -prices_within,
         -edu_20plus, -income_full, -degree_pct_within, -degree_pct) %>% 
  na.omit()

# dataset for income model
immig_df_inc <- df %>% 
  select(-prices, -prices_mean, -prices_within,
         -uni, -degree_pct_within, -degree_pct) %>% 
  na.omit()

immig_df |> count(year_c)

# REWB model ------------------------------------------------------------------

immi_rewb <- lmer(immigSelf ~ affordability_within + affordability_mean +
                    pop_density_within + pop_density_mean +
                    foreign_per_1000_within + foreign_per_1000_mean +
                    over_65_pct_within + over_65_pct_mean +
                    under_15_pct_within + under_15_pct_mean +
                    manuf_pct_within + manuf_pct_mean +
                    degree_pct_mean + degree_pct_within +
                    as.factor(year_c) + (1|LAD) + (1|LAD:id),
                  data = rewb_df, REML = FALSE)
summary(immi_rewb)
summ(immi_rewb)

# removing manufacturing percent
immi_rewb2 <- lmer(immigSelf ~ affordability_within + affordability_mean +
                    pop_density_within + pop_density_mean +
                    foreign_per_1000_within + foreign_per_1000_mean +
                    over_65_pct_within + over_65_pct_mean +
                    under_15_pct_within + under_15_pct_mean +
                    #manuf_pct_within + manuf_pct_mean +
                    degree_pct_mean + degree_pct_within +
                    as.factor(year_c) + (1|LAD) + (1|LAD:id),
                  data = rewb_df, REML = FALSE)
summary(immi_rewb2)
summ(immi_rewb2)

# removing population density
immi_rewb3 <- lmer(immigSelf ~ affordability_within + affordability_mean +
                     #pop_density_within + pop_density_mean +
                     foreign_per_1000_within + foreign_per_1000_mean +
                     over_65_pct_within + over_65_pct_mean +
                     under_15_pct_within + under_15_pct_mean +
                     #manuf_pct_within + manuf_pct_mean +
                     degree_pct_mean + degree_pct_within +
                     as.factor(year_c) + (1|LAD) + (1|LAD:id),
                   data = rewb_df, REML = FALSE)
summary(immi_rewb3)
summ(immi_rewb3)

# adding regional dummy
immi_rewb4 <- lmer(immigSelf ~ affordability_within + affordability_mean +
                     #pop_density_within + pop_density_mean +
                     foreign_per_1000_within + foreign_per_1000_mean +
                     over_65_pct_within + over_65_pct_mean +
                     under_15_pct_within + under_15_pct_mean +
                     #manuf_pct_within + manuf_pct_mean +
                     degree_pct_mean + degree_pct_within +
                     as.factor(year_c) + 
                     region_fct +
                     (1|LAD) + (1|LAD:id),
                   data = rewb_df, REML = FALSE)
summary(immi_rewb4)
summ(immi_rewb4)

# interaction effects ------------------------------------------------

immi_mod <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability_mean +
                   affordability_within + # affordability +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   degree_pct_mean + #degree_pct_within + 
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born +
                   as.factor(year_c) + 
                   (1|LAD) + (1|LAD:id),
                 data = immig_df, REML = FALSE)
summary(immi_mod)

immi_int <- lmer(immigSelf ~ social_housing + homeowner + private_renting +
                   affordability_mean +
                   social_housing.affordability_mean + # SH interaction
                   homeowner.affordability_mean + 
                   affordability_within + # affordability +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   degree_pct_mean + #degree_pct_within + 
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born +
                   as.factor(year_c) + 
                   (1|LAD) + (1|LAD:id),
                 data = immig_df, REML = FALSE)
summary(immi_int)

anova(immi_mod, immi_int)

saveRDS(immi_int, file = "working/markdown_data/immi_int_long.RDS")

# robustness check - log affordability ---------------------------------

immi_log <- lmer(immigSelf ~ (social_housing * affordability_log_mean) +
                   (homeowner * affordability_log_mean) +
                   affordability_log_within + # affordability +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   degree_pct_mean + #degree_pct_within + 
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born +
                   as.factor(year_c) +
                   (1|LAD) + (1|LAD:id),
                 data = immig_df, REML = FALSE)
summary(immi_log)

# robustness check - prices -------------------------------------------

immi_price <- lmer(immigSelf ~ (social_housing * prices_mean) +
                     (homeowner * prices_mean) +
                     prices_within + 
                     #pop_density_within + pop_density_mean +
                     foreign_per_1000_within + foreign_per_1000_mean +
                     over_65_pct_within + over_65_pct_mean +
                     under_15_pct_within + under_15_pct_mean +
                     degree_pct_mean + #degree_pct_within +
                     #manuf_pct_within + manuf_pct_mean +
                     edu_20plus + male + white_british + no_religion + 
                     c1_c2 + d_e + non_uk_born +
                     as.factor(year_c) +
                     (1|LAD) + (1|LAD:id),
                   data = immig_df_price, REML = FALSE)
summary(immi_price)

# robustness check - uni -------------------------------------------

immi_uni <- lmer(immigSelf ~ (social_housing * affordability_mean) +
                   (homeowner * affordability_mean) +
                   affordability_within +
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   degree_pct_mean + #degree_pct_within + 
                   #manuf_pct_within + manuf_pct_mean +
                   male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born +
                   as.factor(year_c) + 
                   uni + # adding uni
                   (1|LAD) + (1|LAD:id),
                 data = immig_df_uni, REML = FALSE)
summary(immi_uni)

# robustness check - income -------------------------------------------

immi_inc <- lmer(immigSelf ~ (social_housing * affordability_mean) +
                   (homeowner * affordability_mean) +
                   affordability_within + 
                   #pop_density_within + pop_density_mean +
                   foreign_per_1000_within + foreign_per_1000_mean +
                   over_65_pct_within + over_65_pct_mean +
                   under_15_pct_within + under_15_pct_mean +
                   degree_pct_mean + #degree_pct_within + 
                   #manuf_pct_within + manuf_pct_mean +
                   edu_20plus + male + white_british + no_religion + 
                   c1_c2 + d_e + non_uk_born +
                   as.factor(year_c) +
                   income_full + # adding income
                   (1|LAD) + (1|LAD:id),
                 data = immig_df_inc, REML = FALSE)
summary(immi_inc)

######################################################################
# variation in affordability models ---------------------------------
######################################################################

rm(list = ls())

afford <- read_csv("data/affordability_ratio_las.csv",
                   na = c(":", "NA"))

year_range <- as.character(seq(2002,2022,1))

afford <- afford %>% 
  rename(oslaua_code = `Local authority code`) %>% 
  select(oslaua_code, all_of(year_range)) %>% 
  pivot_longer(cols = all_of(year_range),
               names_to = "year",
               values_to = "affordability") |> 
  mutate(year = as.integer(year),
         year_c = year - 2002)

region <- read_csv("data/lasregionew2021lookup.csv")

region <- region |> 
  rename(oslaua_code = `LA code`,
         region_code = `Region code`,
         region_name = `Region name`) |>  
  select(oslaua_code, region_code, region_name)

afford <- afford |> 
  left_join(region, by = "oslaua_code") |> 
  na.omit() |> 
  filter(oslaua_code != "E06000053")

rm(region)

afford  |> 
  ggplot(aes(x = year_c, y = affordability)) +
  geom_line(aes(group = oslaua_code), alpha = 0.3) +
  geom_smooth() +
  facet_wrap(~region_name) +
  theme_minimal()

mlm_mod <- lmer(affordability ~ (1|region_name:oslaua_code) + (1|region_name),
                data = afford, REML = FALSE)

summ(mlm_mod)

afford_2010 <- afford |> 
  filter(year >= 2010)

mlm_mod2 <- lmer(affordability ~ (1|region_name:oslaua_code) + (1|region_name),
                 data = afford_2010, REML = FALSE)

summ(mlm_mod2)

