#--processing irrigation component of scenario sheet
#--created 2/15, updated 2/16


rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# data --------------------------------------------------------------------

d_raw <- read_csv("R/data_raw/lca-sheets/raw_production.csv",
                  skip = 5) %>% 
  janitor::remove_empty()


d <- fun_preproc(d_raw)


# seed --------------------------------------------------------------------

s1 <- 
  d %>% 
  filter(cat == "seed")


s2 <- 
  s1 %>%
  select(-unit) %>% 
  #--change to be on a hectare basis
  mutate(
    value_lbs_per_ac = value,
    value_kg_per_ha = value_lbs_per_ac * kg_per_lb * ac_per_ha,
    unit = "kg / stand"
  ) %>% 
  select(production_id, cat, desc, unit, value_kg_per_ha) %>% 
  rename(value = value_kg_per_ha)

s2

s2 %>% 
  write_csv("R/data_tidy/prod_seeds.csv")

