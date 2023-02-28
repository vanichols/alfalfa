# calculate energy used to produce the fuel used
#--in progress, 2/24
#--this is confusing. Electricity has a higher manufacturing energy than fossil fuels by 10x

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")

#--relies on 02_fuel-use-energy calcs

# ref data ----------------------------------------------------------------

#--fuel energies and efficiencies
fe <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5) 


# energy required from fuel -----------------------------------------------

fu <- read_csv("R/data_tidy/energy_fuel-use.csv")



# energy reqd to manu fuel ------------------------------------------------

m <- 
  fe |> 
  filter(grepl("manufacture", desc)) |> 
  mutate(manu_energy_btu_mmbtu = value) |> 
  select(fuel_type, manu_energy_btu_mmbtu)


m1 <- 
  fu |> 
  rename(energy_needed = value) |> 
  separate(desc, into = c("desc", "fuel_type"), sep = ",") |> 
  mutate_if(is.character, str_trim) |> 
  left_join(m) |> 
  mutate(value = energy_needed * manu_energy_btu_mmbtu / 1000000,
         unit = "mj/stand",
         cat = "fuel manufacture") |> 
  unite(desc, fuel_type, col = "desc", sep = ", ") |> 
  select(production_id, assumption_id, cat, desc, unit, value)


m1 |> 
  write_csv("R/data_tidy/energy_fuel-manu.csv")


