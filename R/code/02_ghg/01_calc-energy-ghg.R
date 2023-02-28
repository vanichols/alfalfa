#--change energy to ghg emissions
#--requires assumptions about source of energy
#--those are listed under the 'energy sources' category of the assumptions file
#--2/28


library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")

# assumptions -------------------------------------------------------------

a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(assumption_id, cat) %>% 
  select(-notes) 

a_source <- 
  a %>% 
  filter(grepl("energy source", cat))


# energy ------------------------------------------------------------------

tot <- read_csv("R/data_tidy/energy_tot.csv")


# ghg emissions for each fuel type ----------------------------------------

ghg <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5) |> 
  mutate_if(is.character, str_to_lower)

ghg_use <- 
  ghg %>% 
  filter(desc == "co2e released from use") |> 
  full_join(
    ghg %>% 
      filter(desc == "energy content"),
    by = "fuel_type") |> 
  mutate(unit = "kg co2e/mj",
         value = case_when(
           grepl("kg/gal", unit.x) ~ value.x * gal_per_l * 1/value.y,
           grepl("lb/mwh", unit.x) ~ value.x * kg_per_lb * mwh_per_kwh * 1/value.y,
           grepl("kg/scf", unit.x) ~ value.x * cuft_per_m3 * 1/value.y,
           TRUE ~ 999
         ),
         cat = "fuel use") |> 
  select(fuel_type, cat, unit, value) 
ghg_use

ghg_man <- 
  ghg %>% 
  filter(desc == "co2e released from manufacturing") |> 
  mutate(unit = "kg co2e/mj",
         value = value * mmbtu_per_btu * btu_per_mj * kg_per_g,
         cat = "fuel manufacture") |> 
  select(-desc)

ghg_use


ghg_tot <- 
  ghg_man |> 
  bind_rows(ghg_use) |> 
  group_by(fuel_type, unit) |> 
  summarise(value = sum(value),
            cat = "fuel use + manufacture")

# combine -----------------------------------------------------------------

#---how does FTM do this???

#--irrigation; fuel, field ops; fuel, harvest all have assumptions
a_source

#--assign a fuel type to things without an assumption
tot |> 
  mutate(a_merge = case_when(
    cat == "irrigation" ~ "irrigation",
    (cat == "fuel use") & (desc == "field ops")
    
