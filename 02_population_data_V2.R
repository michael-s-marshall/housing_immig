pacman::p_load(tidyverse)

rm(list = ls())

pop <- read_csv("data/population_data.csv")

pop <- pop %>% 
  mutate(Code = str_trim(Code)) |>  
  select(Code, Name, contains("people per sq"))

names(pop)

names(pop) <- c("oslaua_code", "la_name",
                seq(2022,2011,-1))

pop <- pop |> 
  select(oslaua_code:`2020`) |> 
  pivot_longer(`2022`:`2020`,
               names_to = "year",
               values_to = "pop_density") |>  
  mutate(year = as.double(year))

save(pop, file = "working/data/pop.RData")
