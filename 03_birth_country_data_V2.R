## NOTE: this dataset discontinued by ONS from 2021
## So using the 2021 as a proxy for the 2022 data

pacman::p_load(tidyverse)

rm(list = ls())

bc_2020 <- read_csv("data/Birth country data/birth_country_2020.csv",
                    na = c(":",""))

map_chr(bc_2020, class)

bc_2020 <- bc_2020 |> 
  rename(
    oslaua_code = 1,
    total_pop = `All estimate`,
    uk_pop = `United Kingdom estimate`
    ) |>
  mutate(
    total_pop = total_pop * 1000,
    uk_pop = uk_pop * 1000,
    foreign_pop = total_pop - uk_pop,
    pop_1000 = total_pop / 1000,
    foreign_per_1000 = foreign_pop / pop_1000,
    year = 2020
    ) %>%
  select(oslaua_code, year, total_pop, uk_pop, pop_1000, foreign_pop, foreign_per_1000)

bc_2021 <- read_csv("data/Birth country data/birth_country_2021.csv",
                    na = c("Not Available",""))
bc_2021 %>% map_chr(class)
bc_2021 <- bc_2021 %>% 
  rename(oslaua_code = `Area Code`,
         total_pop = `All\nEstimate`,
         uk_pop = `United Kingdom\nEstimate`) %>% 
  mutate(
    foreign_pop = total_pop - uk_pop,
    pop_1000 = total_pop / 1000,
    foreign_per_1000 = foreign_pop / pop_1000,
    year = 2021) %>% 
  select(oslaua_code, year, total_pop, uk_pop, pop_1000, foreign_pop, foreign_per_1000)

bc <- bind_rows(bc_2020, bc_2021) %>% 
  arrange(oslaua_code, year)

old_codes <- read_csv("data/Birth country data/old_la_codes.csv") %>% 
  select(oslaua_code, old_code) %>% 
  rename(new_code = oslaua_code)

old_codes <- old_codes %>% 
  left_join(bc, by = c("old_code" = "oslaua_code")) %>% 
  group_by(new_code, year) %>% 
  summarise(total_pop = sum(total_pop),
            uk_pop = sum(uk_pop),
            .groups = "drop") %>% 
  mutate(pop_1000 = total_pop / 1000,
         foreign_pop = total_pop - uk_pop,
         foreign_per_1000 = foreign_pop / pop_1000) %>% 
  rename(oslaua_code = new_code)

bc <- bc %>%
  bind_rows(old_codes)

save(bc, file = "working/data/bc.RData")
