#--processing irrigation component of scenario sheet
#--created 2/15, updated 2/16
#--ftm energy eqn uses stupid units, keep those


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



# irrigation --------------------------------------------------------------

#--irrigation used for establishment, only once during stand life
i_est <- 
  d %>% 
  filter(cat == "irrigation") %>%
  filter(grepl("est", desc))

i_est1 <- 
  i_est %>% 
  #-change from ac-in to ha-m...or liters?
  mutate(value_ha_m = value * ha_per_ac * m_per_in,
         value_l = value_ha_m * m2_per_ha * l_water_per_m3) %>% 
  mutate(unit = "l / stand", 
         value = value_l) %>%
  select(scenario_id, cat, desc, unit, value)
  


#--irrigation used for production
i_prod <- 
  d %>% 
  filter(cat == "irrigation") %>%
  filter(grepl("prod", desc)) %>% 
  left_join(sl) %>% 
  mutate(value = value * stand_life_yrs,
         unit = "ac-in/stand") %>% 
  select(-stand_life_yrs)

i_prod1 <- 
  i_prod %>% 
  #-change from ac-in to ha-m...or liters?
  mutate(value_ha_m = value * ha_per_ac * m_per_in,
         value_l = value_ha_m * m2_per_ha * l_water_per_m3) %>% 
  mutate(unit = "l / stand", 
         value = value_l) %>%
  select(scenario_id, cat, desc, unit, value)


i2 <- 
  i_est1 %>% 
  bind_rows(i_prod1) %>%
  bind_rows(i_est) %>% 
  bind_rows(i_prod)

i2

i2 %>% 
  write_csv("R/data_tidy/prod_irrigation.csv")

