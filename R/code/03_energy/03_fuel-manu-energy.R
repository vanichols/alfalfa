# calculate energy used to produce the fuel used
#--in progress, 2/24
#--this is confusing. Electricity has a higher manufacturing energy than fossil fuels by 10x
#--I'm not sure if I should include this or not
#--it applies to the tractor fuel use, and the irrigation fuel use

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")

#--relies on 02_fuel-use-energy calcs

# ref data ----------------------------------------------------------------

#--fuel energies and efficiencies
fe <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5) 


# energy required from fuel -----------------------------------------------

tr <- read_csv("R/data_tidy/energy_tractor.csv")
ir <- read_csv("R/data_tidy/energy_irrig.csv")


d <- 
  tr |> 
  bind_rows(ir) |> 
  group_by(production_id, assumption_id, desc, fuel_type, unit) |> 
  summarise(value = sum(value)) |> 
  filter(value != 0)


# energy reqd to manu fuel ------------------------------------------------

me <- 
  fe |> 
  filter(grepl("manufacture", desc)) |> 
  mutate_if(is.character, str_to_lower) |> 
  mutate(manuenergy_per_prodenergy = value * mmbtu_per_btu) |> 
  select(fuel_type, manuenergy_per_prodenergy)


m1 <- 
  d |> 
  rename(energy_needed_mj_stand = value) |> 
  mutate_if(is.character, str_trim) |> 
  left_join(me) |> 
  mutate(
    value = energy_needed_mj_stand * manuenergy_per_prodenergy,
         unit = "mj/stand",
         cat = "fuel manufacture") |> 
  select(production_id, assumption_id, fuel_type, cat, desc, unit, value)


m1 |> 
  write_csv("R/data_tidy/energy_fuel-manu.csv")


