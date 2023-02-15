#--using california healthy soils, assign carbon credits

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")


# clean up function -------------------------------------------------------


fun_preproc <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(system, .direction = c("down")) %>% 
    fill(flow_type, .direction = c("down")) %>% 
    fill(flow_cat, .direction = c("down")) %>% 
    select(-notes) %>% 
    pivot_longer(mid:worst) 
    
  return(tmp)
  }


cc <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "carbon_credits", 
                   skip = 5)

cc1 <- 
  fun_preproc(data = cc) 

cc1

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

    
    
