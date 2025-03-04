pacman::p_load(haven, tidyverse, jtools, lme4, lmerTest, ggstance)

rm(list = ls())

# loading data ----------------------------------------------------------------

load("working/data/cross_sectional_df_eu.RData")

# rescale function --------------------------------------------

rescale01 <- function(x, ...){
  out <- ((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
  return(out)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# missing values ---------------------------------------

level_twos <- df %>% select(affordability:degree_pct) %>% names()

df_immi <- df %>% 
  select(la_code, uni, male, white_british, no_religion, edu_20plus,
         c1_c2, d_e, own_outright, own_mortgage, social_housing,
         private_renting, age, age_raw, non_uk_born, homeowner,
         control_immi, all_of(level_twos), contains("raw"), 
         region_code, income, income_preds, income_full) %>% 
  rename(LAD = la_code)

these_vars <- df_immi %>% 
  select(-uni, -income, -income_preds, -income_full, -LAD) %>% 
  names()

df_immi %>% map_int(~sum(is.na(.)))

df_immi <- df_immi %>% 
  select(LAD, all_of(these_vars)) %>% 
  na.omit()

df_immi_uni <- df %>% 
  select(all_of(these_vars), uni, la_code) %>%
  rename(LAD = la_code) %>% 
  na.omit()

nrow(df) - nrow(df_immi)

(nrow(df) - nrow(df_immi)) / nrow(df)

df_inc <- df %>% 
  select(all_of(these_vars), income_full, la_code) %>% 
  rename(LAD = la_code) %>% 
  na.omit()

# distribution of DV --------------------------------------------

df_immi %>% 
  ggplot(aes(x = control_immi)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 1, 
                 colour = "black",
                 fill = "lightgrey") +
  scale_x_continuous(breaks = seq(0,10,1)) +
  theme_minimal() +
  labs(x = "Access single market ... Control immigration")

###############################################################################
# modelling ------------------------------------------------------------------
###############################################################################

# null model test ----------------------------------------------

# null model
immig_fit <- lm(control_immi ~ 1, data = df_immi)

# lmer null model
immig_lmer <- lmer(control_immi ~ (1|LAD), data = df_immi,
                   REML = FALSE)

summ(immig_lmer)

logLik(immig_fit)
logLik(immig_lmer)
2 * (logLik(immig_lmer) - logLik(immig_fit))

# hypothesis vars only, testing improved fit from SH interaction --------------

# making interaction terms with scaled values
df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_hypot <- lmer(control_immi ~ homeowner.affordability +
                     homeowner + affordability +
                     (1|LAD),
                   data = df_immi, REML = FALSE)

summary(immi_hypot)

immi_hypot_sh <- lmer(control_immi ~ homeowner.affordability +
                        social_housing.affordability +
                        homeowner + affordability +
                        social_housing +
                        (1|LAD),
                      data = df_immi, REML = FALSE)

summary(immi_hypot_sh)

# improvement in model fit is significant
anova(immi_hypot, immi_hypot_sh)

# including controls, testing for improvement from SH interaction -------------

immig_hom <- lmer(control_immi ~ social_housing + homeowner + private_renting +
                    affordability +
                    male + white_british + 
                    no_religion + edu_20plus +
                    age + 
                    c1_c2 + d_e + non_uk_born + 
                    foreign_per_1000 +
                    over_65_pct + under_15_pct + 
                    degree_pct + 
                    homeowner.affordability +
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_hom)

immig_int <- lmer(control_immi ~ social_housing + homeowner + private_renting +
                    affordability +
                    male + white_british + 
                    no_religion + edu_20plus +
                    age + 
                    c1_c2 + d_e + non_uk_born + 
                    foreign_per_1000 +
                    over_65_pct + under_15_pct + 
                    degree_pct +
                    social_housing.affordability + # adding SH interaction
                    homeowner.affordability +
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_int)

# model fit is improved significantly according to chi square
anova(immig_hom, immig_int)

saveRDS(immig_int, file = "working/markdown_data/immig_eu_int.RDS")

# with uni var ---------------------------------------------------

df_immi_uni <- df_immi_uni %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immig_uni <- lmer(control_immi ~ social_housing.affordability +
                    homeowner.affordability +
                    social_housing + homeowner + affordability +
                    male + white_british + no_religion +
                    private_renting + age +
                    c1_c2 + d_e + non_uk_born +
                    foreign_per_1000 + 
                    over_65_pct + under_15_pct + degree_pct + 
                    uni + # adding uni
                    (1|LAD),
                  data = df_immi_uni, REML = FALSE)
summary(immig_uni)

# log affordability ---------------------------------------

immig_log <- lmer(control_immi ~ (social_housing * affordability_log) +
                    (homeowner * affordability_log) + 
                    private_renting +  male + white_british + 
                    no_religion + edu_20plus +
                    age + c1_c2 + d_e + non_uk_born +
                    foreign_per_1000 + 
                    over_65_pct + under_15_pct + 
                    degree_pct + 
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_log)

# prices --------------------------------------------------

immig_pri <- lmer(control_immi ~ (social_housing * prices) + 
                     (homeowner * prices) + 
                     private_renting +  male + white_british + 
                     no_religion + edu_20plus +
                     age + c1_c2 + d_e + non_uk_born +
                     foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + 
                     (1|LAD),
                   data = df_immi, REML = FALSE)
summary(immig_pri)

# region dummies ------------------------------------------

immig_reg <- lmer(control_immi ~ social_housing + homeowner + private_renting +
                    affordability +
                    male + white_british +
                    no_religion + edu_20plus +
                    age +
                    c1_c2 + d_e + non_uk_born +
                    foreign_per_1000 + 
                    over_65_pct + under_15_pct +
                    degree_pct + 
                    social_housing.affordability +
                    homeowner.affordability + 
                    region_code + # adding region
                    (1|LAD),
                  data = df_immi, REML = FALSE)
summary(immig_reg)

# including income_full ------------------------------------------------

df_inc <- df_inc %>% 
  mutate(social_housing.affordability = social_housing * affordability,
         homeowner.affordability = homeowner * affordability)

immi_inc <- lmer(control_immi ~ social_housing.affordability +
                   homeowner.affordability +
                   social_housing + homeowner + affordability +
                   male + white_british + no_religion + edu_20plus +
                   private_renting + age +
                   c1_c2 + d_e + non_uk_born +
                   foreign_per_1000 + 
                   over_65_pct + under_15_pct + degree_pct +  
                   income_full + # adding income
                   (1|LAD),
                 data = df_inc, REML = FALSE)
summary(immi_inc)

# not including homeowner interaction -----------------------------------------

immi_shint <- lmer(control_immi ~ (social_housing * affordability) +
                     homeowner +
                     male + white_british + 
                     no_religion + edu_20plus +
                     private_renting + age + 
                     c1_c2 + d_e + non_uk_born + 
                     foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + 
                     (1|LAD),
                   data = df_immi, REML = FALSE)
summary(immi_shint)

# robustness check - log scale
immi_shlog <- lmer(control_immi ~ (social_housing * affordability_log) +
                     homeowner +
                     male + white_british + 
                     no_religion + edu_20plus +
                     private_renting + age + 
                     c1_c2 + d_e + non_uk_born + 
                     foreign_per_1000 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + 
                     (1|LAD),
                   data = df_immi, REML = FALSE)
summary(immi_shlog)

# robustness check - with prices 
immi_int_shprice <- lmer(control_immi ~ (social_housing * prices) +
                           homeowner +
                           male + white_british +
                           no_religion + edu_20plus +
                           private_renting + age +
                           c1_c2 + d_e + non_uk_born +
                           foreign_per_1000 + 
                           over_65_pct + under_15_pct +
                           degree_pct + 
                           (1|LAD),
                         data = df_immi, REML = FALSE)
summary(immi_int_shprice)

# robustness check - dummy for region
immi_shreg <- lmer(control_immi ~ social_housing.affordability + 
                     social_housing + homeowner + affordability +
                     male + white_british + 
                     no_religion + edu_20plus +
                     private_renting + age + 
                     c1_c2 + d_e + non_uk_born + 
                     foreign_per_1000 +  
                     over_65_pct + under_15_pct + 
                     degree_pct +  
                     region_code + # adding region
                     (1|LAD),
                   data = df_immi, REML = FALSE)
summary(immi_shreg)

# robustness check - uni > education age
immi_shuni <- lmer(control_immi ~ social_housing + homeowner + private_renting +  
                     affordability +
                     male + white_british + 
                     no_religion + 
                     age + 
                     c1_c2 + d_e + non_uk_born + 
                     foreign_per_1000 +  
                     over_65_pct + under_15_pct + 
                     degree_pct + 
                     social_housing.affordability + 
                     uni + # adding uni
                     (1|LAD),
                   data = df_immi_uni, REML = FALSE)

summary(immi_shuni)

# robustness check - including low income predictions
immi_shinc <- lmer(control_immi ~ social_housing + homeowner + private_renting +  
                     affordability +
                     male + white_british + 
                     no_religion + edu_20plus +
                     age +
                     c1_c2 + d_e + non_uk_born + 
                     foreign_per_1000 + # pop_sqm_2021 + 
                     over_65_pct + under_15_pct + 
                     degree_pct + # manuf_pct +
                     social_housing.affordability + 
                     income_full + # adding income
                     (1|LAD),
                   data = df_inc, REML = FALSE)
summary(immi_shinc)

# comparing fixed effects -----------------------------------------------------

model_obj <- list(immig_int,immig_log,immig_pri,immig_uni,immi_inc,immi_shint,immi_shlog,immi_int_shprice,immi_shuni,immi_shinc)
var_names <- c("affordability","affordability_log","prices","affordability","affordability","affordability","affordability_log","prices","affordability","affordability")
sh_interaction <- c("social_housing.affordability","social_housing:affordability_log","social_housing:prices","social_housing.affordability","social_housing.affordability","social_housing:affordability","social_housing:affordability_log","social_housing:prices","social_housing.affordability","social_housing.affordability")
h_interaction <- rep(c("homeowner.affordability","affordability_log:homeowner","prices:homeowner","homeowner.affordability","homeowner.affordability"),2)
home_int <- c("Y","Y","Y","Y","Y","N","N","N","N","N")
model <- rep(c("Main","Log","Prices","Uni","Income"),2)
aff_vars <- c("^affordability$|^affordability_log$|^prices$")

immi_dummy_list <- list()

for(i in seq_along(var_names)){
  dat <- model.matrix(model_obj[[i]])
  aff <- dat[,str_detect(colnames(dat), aff_vars)]
  
  x_scale <- seq(min(aff), 
                 max(aff), 
                 (max(aff) - min(aff))/5)
  sh_i <- sh_interaction[i]
  h_i <- h_interaction[i]
  
  immi_dummy2 <- expand.grid(
    male = c(mean(df_immi$male)),
    white_british = c(mean(df_immi$white_british)),
    no_religion = c(mean(df_immi$no_religion)),
    edu_20plus = c(mean(df_immi$edu_20plus)),
    homeowner = 0,
    private_renting = c(mean(df_immi$private_renting)),
    age = mean(df_immi$age),
    c1_c2 = c(mean(df_immi$c1_c2)),
    d_e = c(mean(df_immi$d_e)),
    non_uk_born = c(mean(df_immi$non_uk_born)),
    #pop_sqm_2021 = c(mean(df_immi$pop_sqm_2021)),
    foreign_per_1000 = c(mean(df_immi$foreign_per_1000)),
    over_65_pct = c(mean(df_immi$over_65_pct)),
    under_15_pct = c(mean(df_immi$under_15_pct)),
    degree_pct = c(mean(df_immi$degree_pct)),
    #manuf_pct = c(mean(df_immi$manuf_pct)),
    social_housing = c(0,1),
    affordability = x_scale,
    #LAD = unique(df_immi$LAD),
    income_full = c(mean(df_inc$income_full)),
    uni = c(mean(df_immi_uni$uni))
  ) %>% 
    mutate({{sh_i}} := social_housing * affordability,
           {{h_i}} := homeowner * affordability,
           affordability_01 = rescale01(affordability),
           affordability_log = affordability,
           prices = affordability)
  
  acov <- vcov(model_obj[[i]])
  fixed <- summary(model_obj[[i]])$coefficients[,"Estimate"]
  vars_order <- names(fixed)[-1]
  xmat <- immi_dummy2 %>%
    mutate(int = 1, .before = 1) %>%
    select(int, all_of(vars_order)) %>%
    as.matrix()
  
  immi_dummy2$fit <- xmat %*% fixed
  immi_dummy2$SE <- xmat %*% acov %*% t(xmat) %>%
    diag() %>%
    sqrt()
  
  immi_dummy2 <- immi_dummy2 %>%
    mutate(LL = fit - qnorm(0.975)*SE,
           UL = fit + qnorm(0.975)*SE)
  
  immi_dummy2$model <- model[i]
  immi_dummy2$home_int_yn <- home_int[i]
  immi_dummy2 <- immi_dummy2 |> 
    mutate(`Social housing` = as.factor(ifelse(social_housing == 1, "Y","N")))
  
  immi_dummy_list[[i]] <- immi_dummy2
  
  #immi_dummy2$preds <- predict(model_obj[[i]], newdata = immi_dummy2)
  
  #immi_dummy_grouped[[i]] <- immi_dummy2 |>
  #  mutate(home_int_yn = home_int[i],
  #         model = model[i]) |>
  #  group_by(social_housing, affordability_01, model, home_int_yn) |>
  #  summarise(control_immi = mean(preds), .groups = "drop") |> 
  #  mutate(`Social housing` = as.factor(ifelse(social_housing == 1, "Y","N")))
  
}

my_line <- function(dat){
  geom_line(data = dat,
            aes(colour = fct_rev(home_int_yn),
                linetype = fct_rev(`Social housing`)),
            linewidth = 1.1)
}
my_ribbon <- function(dat){
  geom_ribbon(data = dat,
              aes(x = affordability_01, y = fit,
                  colour = fct_rev(home_int_yn),
                  fill = fct_rev(home_int_yn),
                  linetype = fct_rev(`Social housing`),
                  ymin = LL, ymax = UL), alpha = 0)
}

immi_dummy_list[[1]] |> 
  ggplot(aes(x = affordability_01, 
             y = fit, 
             linetype = fct_rev(`Social housing`),
             colour = fct_rev(home_int_yn),
             fill = fct_rev(home_int_yn))) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0) +
  my_line(immi_dummy_list[[2]]) +
  my_ribbon(immi_dummy_list[[2]]) +
  my_line(immi_dummy_list[[3]]) +
  my_ribbon(immi_dummy_list[[3]]) +
  my_line(immi_dummy_list[[4]]) +
  my_ribbon(immi_dummy_list[[4]]) +
  my_line(immi_dummy_list[[5]]) +
  my_ribbon(immi_dummy_list[[5]]) +
  my_line(immi_dummy_list[[6]]) +
  my_ribbon(immi_dummy_list[[6]]) +
  my_line(immi_dummy_list[[7]]) +
  my_ribbon(immi_dummy_list[[7]]) +
  my_line(immi_dummy_list[[8]]) +
  my_ribbon(immi_dummy_list[[8]]) +
  my_line(immi_dummy_list[[9]]) +
  my_ribbon(immi_dummy_list[[9]]) +
  my_line(immi_dummy_list[[10]]) +
  my_ribbon(immi_dummy_list[[10]]) +
  facet_wrap(~model) +
  theme_bw() +
  drop_y_gridlines() +
  labs(x = "Affordability", y = "Control immigration",
       colour = "Homeowner interaction", fill = "Homeowner interaction",
       linetype = "Social housing") +
  theme(legend.position = "top") +
  scale_colour_viridis_d()

# coef plot ---------------------------------------------------------

confint_clean <- function(confint_obj, lmer_obj){
  confint_obj %>% 
    as_tibble() %>% 
    bind_cols(
      tibble(term = c(".sig01",".sigma",names(fixef(lmer_obj))))
    ) %>% 
    bind_cols(tibble(estimate = c(NA, NA, fixef(lmer_obj)))) %>% 
    na.omit() %>% 
    filter(term != "(Intercept)") %>% 
    rename(lower = 1, upper = 2)
}

set.seed(123)
int_confint <- confint(immig_int, method = "profile")
uni_confint <- confint(immig_uni, method = "profile")
inc_confint <- confint(immi_inc, method = "profile")

housing_vars <- c("affordability","homeowner","social_housing",
                  "homeowner.affordability","social_housing.affordability")

plot_names <- tibble(
  term = c(housing_vars, "private_renting"),
  var_names = c("Affordability",
                "Homeowner",
                "Social housing",
                "Affordability:Homeowner",
                "Affordability:Social housing",
                "Private renting")
)

coef_plot_immi <- int_confint %>% 
  confint_clean(immig_int) %>% 
  full_join(
    uni_confint %>% 
      confint_clean(immig_uni),
    by = c("term"),
    suffix = c(".int",".uni")
  ) %>% 
  full_join(
    inc_confint %>% 
      confint_clean(immi_inc),
    by = "term"
  ) %>%
  rename(
    lower.inc = lower,
    upper.inc = upper,
    estimate.inc = estimate
  ) %>%
  select(term, everything()) %>% 
  filter(term %in% plot_names$term) %>% 
  pivot_longer(cols = lower.int:estimate.inc,
               names_to = c("parameter", "model"),
               values_to = "value",
               names_sep = "\\.") %>%
  pivot_wider(names_from = "parameter",
              values_from = "value") %>%
  mutate(Model = ifelse(model == "inc", "Income",
                        ifelse(model == "int", "Main", "Uni"))) %>% 
  left_join(plot_names, by = "term") %>% 
  ggplot(aes(x = estimate, y = fct_rev(var_names), colour = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "lightgrey", 
             linewidth = 1.5, alpha = 0.7) +
  geom_linerangeh(aes(xmin = lower, xmax = upper), 
                  position = position_dodge(width = 0.4),
                  size = 1) +
  geom_point(position = position_dodge(width = 0.4),
             shape = 21, fill = "white", size = 3.5) +
  theme_bw() +
  drop_y_gridlines() +
  labs(x = "Estimate", y = NULL) +
  theme(legend.position = "top") +
  scale_colour_viridis_d()

coef_plot_immi

saveRDS(coef_plot_immi, file = "working/markdown_viz/coef_plot_eu.RDS")

# visualising interaction term ------------------------------

# making interaction terms with raw values
df_immi <- df_immi %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

immi_viz <- lmer(control_immi ~ social_housing.affordability +
                   homeowner.affordability +
                   social_housing + homeowner + affordability_raw +
                   male + white_british + 
                   no_religion + edu_20plus +
                   private_renting + age + 
                   c1_c2 + d_e + non_uk_born + 
                   foreign_per_1000 + #pop_sqm_2021 +
                   over_65_pct + under_15_pct + 
                   degree_pct + # manuf_pct + 
                   (1|LAD),
                 data = df_immi, REML = FALSE)

# anti immigration among social housing tenants 
x_scale <- seq(min(df_immi$affordability_raw), 
               max(df_immi$affordability_raw), 
               (max(df_immi$affordability_raw) - min(df_immi$affordability_raw))/5)

immi_dummy <- expand.grid(
  male = c(mean(df_immi$male)),
  white_british = c(mean(df_immi$white_british)),
  no_religion = c(mean(df_immi$no_religion)),
  edu_20plus = c(mean(df_immi$edu_20plus)),
  homeowner = c(0,1),
  private_renting = c(mean(df_immi$private_renting)),
  age = mean(df_immi$age),
  c1_c2 = c(mean(df_immi$c1_c2)),
  d_e = c(mean(df_immi$d_e)),
  non_uk_born = c(mean(df_immi$non_uk_born)),
  #pop_sqm_2021 = c(mean(df_immi$pop_sqm_2021)),
  foreign_per_1000 = c(mean(df_immi$foreign_per_1000)),
  over_65_pct = c(mean(df_immi$over_65_pct)),
  under_15_pct = c(mean(df_immi$under_15_pct)),
  degree_pct = c(mean(df_immi$degree_pct)),
  #manuf_pct = c(mean(df_immi$manuf_pct)),
  social_housing = c(0,1),
  affordability_raw = x_scale
) %>% 
  mutate(social_housing.affordability = social_housing * affordability_raw,
         homeowner.affordability = homeowner * affordability_raw)

acov <- vcov(immi_viz)
fixed <- summary(immi_viz)$coefficients[,"Estimate"]
vars_order <- names(fixed)[-1]
xmat <- immi_dummy %>%
  mutate(int = 1, .before = 1) %>%
  select(int, all_of(vars_order)) %>%
  as.matrix()

immi_dummy$fit <- xmat %*% fixed
immi_dummy$SE <- xmat %*% acov %*% t(xmat) %>%
  diag() %>%
  sqrt()

immi_dummy <- immi_dummy %>%
  mutate(LL = fit - qnorm(0.975)*SE,
         UL = fit + qnorm(0.975)*SE)

pacman::p_load(patchwork)

p1 <- immi_dummy %>%
  mutate(
    tenure = ifelse(social_housing == 1 & homeowner == 0, "Social housing",
                    ifelse(homeowner == 1 & social_housing == 0, "Homeowner",
                           ifelse(social_housing == 0 & homeowner == 0, "Other",
                                  "remove")))
  ) %>% 
  filter(tenure != "remove") %>% 
  mutate(tenure = fct_drop(tenure)) %>% 
  ggplot(aes(x = affordability_raw, y = fit,
             colour = tenure)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, group = tenure, fill = tenure,
                  colour = NULL),
              alpha = 0.2) +
  geom_line(linewidth = 1.5) +
  theme_bw() +
  drop_y_gridlines() +
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  labs(x = NULL,
       y = "Control immigration",
       colour = "Tenure",
       fill = "Tenure") +
  #coord_cartesian(ylim = c(5.5,8.5)) +
  theme(legend.position = "top")

p2 <- df_immi %>% 
  ggplot(aes(x = affordability_raw)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100, 
                 colour = "black", fill = "lightgrey") +
  theme_bw() +
  drop_y_gridlines() +
  labs(x = "Affordability ratio", y = "Density")

int_plot <- p1 / plot_spacer() / p2  + plot_layout(heights = c(5, -0.2 , 3))

int_plot

saveRDS(int_plot, file = "working/markdown_viz/int_plot_eu.RDS")
