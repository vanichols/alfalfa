#--processing irrigation component of scenario sheet
#--created 2/15

rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")


# clean up function -------------------------------------------------------

d_raw <- read_excel("R/data_raw/lca-sheets/enterprise-flows-scenario-format.xlsx",
                    sheet = "production",
                    skip = 5)


d <- fun_preproc(d_raw)

# stand life ---------------------------------------------------------------

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(scenario_id, stand_life_yrs)



# irrigation --------------------------------------------------------------

#--irrigation used for establishment
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
  


#--irrigation used annually
i_ann <- 
  d %>% 
  filter(cat == "irrigation") %>%
  filter(!grepl("est", desc))

i_ann1 <- 
  i_ann %>% 
  #-change from ac-in to ha-m...or liters?
  mutate(value_ha_m = value * ha_per_ac * m_per_in,
         value_l = value_ha_m * m2_per_ha * l_water_per_m3) %>% 
  mutate(unit = "l / stand", 
         value = value_l) %>%
  select(scenario_id, cat, desc, unit, value)


i2 <- 
  i_est1 %>% 
  bind_rows(i_ann1)

i2

i2 %>% 
  write_csv("R/data_tidy/lca_irrigation.csv")

