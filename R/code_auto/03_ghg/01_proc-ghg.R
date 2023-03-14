#--calculate ghg emissions, using energy calcs as base

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code_auto/00_conversions.R")
source("R/code_auto/00_funs.R")

my_scenario <- "tulare_001"

# calculate energy used
# created 3/14, combining code from 'code' folder

rm(list = ls())
library(tidyverse)

source("R/code_auto/00_conversions.R")
source("R/code_auto/00_funs.R")


my_scenario <- "tulare_001"


# references --------------------------------------------------------------

r_carbon <- 
  read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx",
                 skip = 5) %>% 
  fill(unit) |> 
  mutate_if(is.character, str_to_lower)

r_fuelghg <- 
  read_csv("R/data_refs/ref_fuel_ghg.csv") |> 
  mutate_if(is.character, str_to_lower) 

#--use energy contents
r_fuele <- 
  read_csv("R/data_refs/ref_fuel-energy.csv") 

# assumption data -------------------------------------------------------------

d_a0 <- read_csv(paste0("R/data_inputs/", my_scenario, "/datin_assumptions.csv"), skip = 5) 
d_a <- fun_preproc_assum(d_a0)


# production and energy data --------------------------------------------------------------------

d_p <- read_csv(paste0("R/code_auto/01_proc-prod/", my_scenario, "-production.csv")) 
d_e <- read_csv(paste0("R/code_auto/02_energy/", my_scenario, "-energy.csv")) 


# 1. carbon credits (c) ---------------------------------------------------

#--using california healthy soils reference and assumptions, assign carbon credits
#--note they already converted to co2e using their own conversions...

#--get assumed county and practice scenario
c_scen <- 
  d_a %>% 
  filter(assump_cat == "carbon credit") |> 
  select(assump_id, assump_desc, assump_value) %>% 
  pivot_wider(names_from = assump_desc, values_from = assump_value)

#--stand life
c_sl <- 
  d_p |> 
  filter(cat == "yield",
         desc == "stand life") |> 
  rename("stand_life_yrs" = value) |> 
  select(production_id, stand_life_yrs)


#--assign carbon credit based on assumed county/practice
c1 <- 
  c_scen %>% 
  left_join(r_carbon, by = c("county", "practice")) %>% 
  pivot_longer(co2:n2o)

#--change units, do total years
c2 <- 
  c1 %>% 
  bind_cols(c_sl) %>% 
  mutate(co2e_kghayr = 
          value * ac_per_ha * 1000,
         co2e_kghastand = co2e_kghayr * stand_life_yrs) 


#--make values negative because it is a sequestering
c3 <- 
  c2 %>% 
  #--make consistent with other formats
  mutate(
    cat = "carbon credit",
    desc = name,
    value = -co2e_kghastand,
    unit = "kg co2e/stand") %>% 
  select(production_id, assump_id, cat, desc, unit, value)


g1 <- c3


# 2. energy ghg (e)---------------------------------------------------------------------
#--change energy to ghg emissions
#--requires assumptions about source of energy
#--those are listed under the 'energy sources' category of the assumptions file

#--what timespan for gwp
e_gwp <- 
  d_a |> 
  filter(assump_cat == "gwp") |> 
  pull(assump_value)


#--what source to use for ghg emissions? 
e_dsghg <- 
  d_a |> 
  filter(assump_cat == "data source",
         assump_desc == "ghg from fuel") |> 
  pull(assump_value)

#--what source to use for fuel energy contents?
e_dse <- 
  d_a |> 
  filter(assump_cat == "data source",
         assump_desc == "fuel energy content") |> 
  pull(assump_value)


# get ghg per mj ----------------------------------------------------------

e_ghg <- 
  r_fuelghg |> 
  filter(grepl(e_dsghg, source)) |> #--assumed energy content info
  filter(time_horizon == e_gwp) #--assumed timeframe


#--energy content

e_fuele <- 
  r_fuele |> 
  filter(source == e_dse) |> 
  mutate(energy_mj_per_l = value) |> 
  select(fuel_type, energy_mj_per_l)

############################ astopped
c3 <- 
  c |> 
  left_join(ec) |> 
  mutate(value2 = case_when(
    unit == "kg/l" ~ value * 1/energy_mj_per_l,
    unit == "kg/kwh" ~ value * kwh_per_btu * btu_per_mj),
    unit2 = "kg co2e/mj") |> 
  select(fuel_type, unit2, value2)

c3

# energy used------------------------------------------------------------------

tot <- read_csv("R/data_tidy/energy_tot.csv")


# deal with the ones where we know the fuel type --------------------------

tot2 <- 
  tot |> 
  filter(!is.na(fuel_type)) |> 
  left_join(c3) |> 
  mutate(value4 = value * value2,
         unit4 = "kg co2e/stand") |> 
  select(production_id, 
         assump_id, 
         cat, 
         desc, 
         fuel_type, 
         unit4,
         value4)
tot2  


# how about where we don't ------------------------------------------------

#--the values really aren't that different, just assume diesel
c3


tot3 <- 
  tot |> 
  filter(is.na(fuel_type)) |> 
  mutate(fuel_type = "diesel") |> 
  left_join(c3) |> 
  mutate(value4 = value * value2,
         unit4 = "kg co2e/stand") |> 
  select(production_id, 
         assump_id, 
         cat, 
         desc, 
         fuel_type, 
         unit4,
         value4)

tot4 <-
  tot2 |> 
  bind_rows(tot3) |> 
  rename(unit = unit4,
         value = value4)

#--simplify pesticides into one cat
tot5 <- 
  tot4 |> 
  mutate(desc = case_when(
    cat == "pesticide manufacture" ~ "pesticide",
    TRUE ~ desc)
  ) |> 
  group_by(production_id, assump_id, cat, desc, fuel_type, unit) |> 
  summarise(value = sum(value))



tot5 |> 
  write_csv("R/data_tidy/ghg_energy-co2e.csv")


