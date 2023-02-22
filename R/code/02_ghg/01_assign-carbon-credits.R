#--using california healthy soils reference and assumptions, assign carbon credits

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# data for stand life --------------------------------------------------------------------

d_raw <- read_csv("R/data_raw/lca-sheets/raw_cv_001.csv",
                  skip = 5) %>% 
  janitor::remove_empty()

d <- fun_preproc(d_raw)

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(scenario_id, stand_life_yrs)



# reference table ---------------------------------------------------------

ca <- read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx",
                 skip = 5) %>% 
  fill(unit)

# assumed county and program ----------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  filter(cat_ass == "carbon credit")

#--I need the stand lenth, which comes from yields

stnd <- 
  read_csv("R/data_tidy/lca_yields.csv") %>% 
  select(system, stand_life_yrs) %>% 
  distinct()

cc2 <- 
  cc1 %>% 
  left_join(stnd)

# ca healthy soils values -------------------------------------------------

d <- read_csv("R/data_raw/cahealthysoils_carbon-credits-by-county.csv",
              skip = 5)

#--change units
d1 <- 
  d %>% 
  fill(units) %>% 
  pivot_longer(co2:n2o) %>% 
  mutate(co2e_kg_ha_yr = value * ac_per_ha * 1000)

#--getting mid, best, worst
#--units are kg co2e per ha per year
d2 <- 
  d1 %>% 
  select(county, practice, name, co2e_kg_ha_yr) %>%
    group_by(county, name) %>% 
    summarise(mid = min(co2e_kg_ha_yr),
              worst = mid,
              best = max(co2e_kg_ha_yr)) %>% 
  rename("flow_desc" = name) %>% 
  pivot_longer(mid:best)

  


# assign system to a county ----------------------------------------------

cc3 <- 
  cc2 %>%
  select(-units, -value) %>%
  mutate(
    county = case_when(
      system == "tulare_county" ~ "tulare",
      system == "central_valley_organic" ~ "yolo", #--no idea what to pick
      TRUE ~ "XXXX"
    )
  ) %>% 
  left_join(d2) %>% 
  #--account for stand life
  mutate(value = value * stand_life_yrs) 



cc3 %>% 
  select(-stand_life_yrs, - county) %>% 
  mutate(units = "kg") %>% 
  write_csv("R/data_tidy/lca_carboncredits.csv")

    
    
