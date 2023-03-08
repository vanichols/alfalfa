#--change energy to ghg emissions
#--requires assumptions about source of energy
#--those are listed under the 'energy sources' category of the assumptions file
#--2/28
#--3/8 clean up, assume a time frame


library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_inputs/datin_assumptions.csv", skip = 5) 
a1 <- fun_preproc_assum(a)

#--what timespan for gwp
a_gwp <- 
  a1 |> 
  filter(assump_cat == "gwp") |> 
  pull(assump_value)


#--what source to use for ghg emissions? 
a_src <- 
  a1 |> 
  filter(assump_cat == "data source",
         assump_desc == "ghg from fuel") |> 
  pull(assump_value)

#--what source to use for fuel energy contents?
a_src2 <- 
  a1 |> 
  filter(assump_cat == "data source",
         assump_desc == "fuel energy content") |> 
  pull(assump_value)


# get ghg per mj ----------------------------------------------------------

c <- 
  read_csv("R/data_refs/ref_fuel_ghg.csv") |> 
  mutate_if(is.character, str_to_lower) |> 
  filter(grepl(a_src, source)) |> 
  filter(time_horizon == a_gwp)


#--use energy contents
ec <- read_csv("R/data_refs/ref_fuel-energy.csv") |> 
  filter(source == a_src2) |> 
  mutate(energy_mj_per_l = value) |> 
  select(fuel_type, energy_mj_per_l)

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
  bind_rows(tot3)

tot4

tot4 |> 
  write_csv("R/data_tidy/ghg_energy-co2e.csv")
