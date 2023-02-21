#--processing irrigation component of scenario sheet
#--created 2/15, updated 2/16


rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# data --------------------------------------------------------------------

d_raw <- read_csv("R/data_raw/lca-sheets/raw_cv_001.csv",
                  skip = 5) %>% 
  janitor::remove_empty()


d <- fun_preproc(d_raw)




# stand life ---------------------------------------------------------------

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(scenario_id, stand_life_yrs)

sl %>% 
  write_csv("R/data_tidy/prod_standlife.csv")


#--get tons dry matter per acre per year
d1 <- 
  d %>%
  filter(cat == "yield") %>% 
  filter(grepl("production", desc)) %>% 
  #-convert units
  mutate(value_dry_ton_per_ac_per_year = case_when(
    unit == "ton/ac/yr at 10% moisture" ~ (value * (1 - 0.1)),
    unit == "ton/ac/yr at 30% moisture" ~ (value * (1 - 0.3)),
    TRUE ~ 0)
  ) %>% 
#--get kg dry matter per ha per year
  mutate(value_kg_per_ha_per_year = 
           value_dry_ton_per_ac_per_year * 
           lbs_per_ton * 
           kg_per_lb * 
           ac_per_ha)

#--include stand life length
d2 <- 
  d1 %>% 
  select(-value, -value_dry_ton_per_ac_per_year) %>% 
#--get stand life 
  left_join(sl) %>% 
  mutate(value_kg_per_ha_per_standlife = 
           value_kg_per_ha_per_year * stand_life_yrs)
  

d3 <- 
  d2 %>%
  select(-value_kg_per_ha_per_year, -stand_life_yrs) %>% 
  mutate(unit = "kg / stand") %>% 
  rename(value = value_kg_per_ha_per_standlife) 

d3

d3 %>%
  write_csv("R/data_tidy/prod_yields.csv")    


