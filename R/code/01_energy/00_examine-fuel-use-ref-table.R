# look at how much energy a given fuel amoutn has
#created 2/21/23

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")



# table from hoffman book -------------------------------------------------


d <- read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5)


#--how do they compare with FTM? Fine. 
d %>% 
  mutate(energy_btu_per_gal = case_when(
   energy_content_unit == "MJ/L" ~ energy_content * btu_per_mj * l_per_gal,
   energy_content_unit == "MJ/m3" ~ energy_content * btu_per_mj * m3_per_cuft
  )
)


d %>% 
  mutate(co2_lb_per_fuel_unit = case_when(
    co2e_unit == "kg/gal" ~ co2e * lb_per_kg,
    co2e_unit == "kg/scf" ~ co2e * lb_per_kg
  )
  )
