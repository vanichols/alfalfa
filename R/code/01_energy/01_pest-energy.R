# calculate energy use
#created 2/16
#--2/28 update to production_id and assumption_id (not scenario_id)

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) %>% 
  fill(assumption_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value)

a_p <- 
  a |> 
  filter(cat_ass == "pesticide manufacture") |>
  mutate(value_ass = as.numeric(value_ass)) 




# pesticide manu energy ---------------------------------------------------

p <- read_csv("R/data_tidy/prod_pesticides.csv")

p1 <-
  p %>% 
  select(-cat) %>% 
  left_join(a_p, by = "desc") 

p2 <- 
  p1 %>%
  mutate(value = value * value_ass,
         unit = "mj/stand",
         cat = cat_ass) %>% 
  select(assumption_id, cat, desc, unit, value)


p2 %>% 
  write_csv("R/data_tidy/energy_pest.csv")
