pacman::p_load(haven, tidyverse, jtools, lme4, lmerTest, margins)

rm(list = ls())

# loading data ----------------------------------------------------------------

load("working/data/cross_sectional_df_equality.RData")

# missing values ---------------------------------------

level_twos <- df %>% select(affordability:degree_pct) %>% names()

df_equal <- df %>% 
  select(la_code, uni, male, white_british, no_religion, edu_20plus,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         equality_too_far, all_of(level_twos), contains("raw")) %>% 
  rename(LAD = la_code)

df_equal %>% map_int(~sum(is.na(.)))

df_equal <- df_equal %>% select(-uni) %>% na.omit()

df_equal_uni <- df %>% 
  select(la_code, uni, male, white_british, no_religion, 
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         equality_too_far, all_of(level_twos), contains("raw")) %>%
  rename(LAD = la_code) %>% 
  na.omit()

nrow(df) - nrow(df_equal)

(nrow(df) - nrow(df_equal)) / nrow(df)

df_inc <- df %>% 
  select(la_code, male, white_british, no_religion, edu_20plus, 
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         equality_too_far, all_of(level_twos), contains("raw"),
         income_full) %>%
  rename(LAD = la_code)

df_inc %>% map_int(~sum(is.na(.)))

df_inc <- df_inc %>% na.omit()

###############################################################################
# modelling ------------------------------------------------------------------
###############################################################################

# cross level interaction ------------------------------------------------------

df_equal <- df_equal %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

equal_int <- glmer(equality_too_far ~ 
                     social_housing + homeowner +  private_renting + 
                     affordability +
                     male + white_british + no_religion + edu_20plus +
                     age +
                     c1_c2 + d_e + non_uk_born + foreign_per_1000 + #pop_sqm_2022 + 
                     over_65_pct + under_15_pct + degree_pct +
                     social_housing.affordability +
                     homeowner.affordability + (1|LAD),
                   data = df_equal, family = binomial("logit"))
summary(equal_int)

saveRDS(equal_int, file = "working/markdown_data/equal_int.RDS")

# marginal effects
marginals_main <- margins(equal_int, type = "response")

summary(marginals_main)

saveRDS(marginals_main, 
        file = "working/markdown_data/marginals_equality.RDS")

# with uni var ---------------------------------------------------

df_equal_uni <- df_equal_uni %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

equal_uni <- glmer(equality_too_far ~ social_housing.affordability +
                     homeowner.affordability +
                     social_housing + homeowner + affordability +
                     male + white_british + no_religion + uni +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born +
                     foreign_per_1000 + # pop_sqm_2021 +
                     over_65_pct + under_15_pct + degree_pct +
                     #manuf_pct + 
                     (1|LAD),
                   data = df_equal_uni, family = binomial("logit"))
summary(equal_uni)

# marginal effects
marginals_uni <- margins(equal_uni, type = "response")

summary(marginals_uni)

# with income --------------------------------------

df_inc <- df_inc %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

equal_inc <- glmer(equality_too_far ~ social_housing.affordability +
                     homeowner.affordability +
                     social_housing + homeowner + affordability +
                     male + white_british + no_religion + edu_20plus +
                     private_renting + age +
                     c1_c2 + d_e + non_uk_born + income_full +
                     foreign_per_1000 + # pop_sqm_2021 +
                     over_65_pct + under_15_pct + degree_pct +
                     #manuf_pct + 
                     (1|LAD),
                   data = df_inc, family = binomial("logit"))
summary(equal_inc)

# marginal effects
marginals_inc <- margins(equal_inc, type = "response")

summary(marginals_inc)

# coefficient plot -------------------------------------------

marginals_main_tib <- summary(marginals_main) %>% 
  as_tibble() %>% 
  mutate(Model = "Main")

marginals_uni_tib <- summary(marginals_uni) %>% 
  as_tibble() %>% 
  mutate(Model = "Uni")

marginals_inc_tib <- summary(marginals_inc) %>% 
  as_tibble() %>% 
  mutate(Model = "Income")

plot_names <- tibble(
  term = c("social_housing",
           "affordability",
           "homeowner", 
           "private_renting",
           "social_housing.affordability",
           "homeowner.affordability"),
  var_names = c("Social housing",
                "Affordability",
                "Homeowner",
                "Private renting",
                "Affordability:Social housing",
                "Affordability:Homeowner")
)

coef_plot_equality <- bind_rows(marginals_main_tib,
                                marginals_uni_tib,
                                marginals_inc_tib) %>%
  filter(factor %in% plot_names$term) %>% 
  left_join(plot_names, by = c("factor" = "term")) %>% 
  ggplot(aes(x = AME, y = fct_rev(var_names), colour = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
             linewidth = 1.5, alpha = 0.7) +
  geom_linerangeh(aes(xmin = lower, xmax = upper), 
                  position = position_dodge(width = 0.4),
                  size = 1) +
  geom_point(position = position_dodge(width = 0.4),
             shape = 21, fill = "white", size = 3.5) +
  theme_minimal() +
  drop_y_gridlines() +
  labs(x = "Average Marginal Effect", y = NULL) +
  theme(legend.position = "top") +
  scale_colour_viridis_d()

coef_plot_equality

saveRDS(coef_plot_equality,
        file = "working/markdown_viz/coef_plot_equality.RDS")
