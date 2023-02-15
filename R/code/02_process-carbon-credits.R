#--using california healthy soils, assign carbon credits

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")



d_raw <-
  read_excel(
    "R/data_raw/lca-sheets/enterprise-flows-scenario-format.xlsx",
    sheet = "production",
    skip = 5
  )


d <- fun_preproc(d_raw)


#--stand life
sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(scenario_id, stand_life_yrs)


#--carbon credits
cc1 <-
  read_excel(
    "R/data_raw/lca-sheets/enterprise-flows-scenario-format.xlsx",
    sheet = "carbon_credits",
    skip = 5
  ) %>% 
  fill(unit) %>% 
  mutate_if(is.character, str_to_lower)



#--change units
cc2 <- 
  cc1 %>% 
  pivot_longer(co2:n2o) %>% 
  mutate(co2e_kg_ha_yr = value * ac_per_ha * 1000) %>% 
  select(-value, -unit)



# create data template ----------------------------------------------------


d1 <- 
  d %>% 
  filter(cat == "carbon credit program") %>% 
  select(scenario_id, cat, desc) %>% 
  distinct() %>% 
  left_join(sl)


# get carbon credits for each scenario-------------------------------------------

d2 <- 
  d1  %>% 
  # assign each scenario a county (or avg of counties?)
  mutate(county = case_when(
    scenario_id == "cv_01" ~ "tulare",
    TRUE ~ "XXX")
  ) %>% 
  #--add stand life
  left_join(sl) %>%
  #--add carbon credits
  left_join(cc2 %>% 
              rename("desc" = practice)) %>% 
  mutate(co2e_kg_ha = co2e_kg_ha_yr * stand_life_yrs) 

d3 <- 
  d2 %>% 
  group_by(scenario_id, cat, desc) %>% 
  summarise(value = sum(co2e_kg_ha)) %>% 
  mutate(unit = "kg co2e / stand life")

d3

d3 %>% 
  write_csv("R/data_tidy/lca_carboncredits.csv")

    
    
