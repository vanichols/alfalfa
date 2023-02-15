#--processing seed component of scenario sheet
#--created 2/15

rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")


# clean up function -------------------------------------------------------

d_raw <- read_excel("R/data_raw/lca-sheets/enterprise-flows-scenario-format.xlsx",
                    sheet = "scenarios",
                    skip = 5)


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
  select(scenario_id, cat, desc, unit, value_kg_per_ha) %>% 
  rename(value = value_kg_per_ha)

s2

s2 %>% 
  write_csv("R/data_tidy/lca_seeds.csv")

