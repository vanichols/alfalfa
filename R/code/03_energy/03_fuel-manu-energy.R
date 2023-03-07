# calculate energy used to produce the fuel used
#--in progress, 2/24
#--this is confusing. Electricity has a higher manufacturing energy than fossil fuels by 10x
#--I'm not sure if I should include this or not
#--it applies to the tractor fuel use, and the irrigation fuel use
# 3/2--going through and simplifying/cleaning to fit new file format

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# ref data ----------------------------------------------------------------

#--fuel manufacture
m <- 
  read_csv("R/data_refs/ref_fuel-manu.csv") 

m1 <- 
  m |> 
  rename("fuel_manu" = value,
         "unit_fuelmanu" = unit) |> 
  select(-source)

# energy required from fuel -----------------------------------------------

 #--fuel used for tractor
tr <- read_csv("R/data_tidy/energy_tractor.csv")

#--fuel used for irrigation
ir <- read_csv("R/data_tidy/energy_irrig.csv")


d <- 
  tr |> 
  bind_rows(ir) |> 
  group_by(production_id, assump_id, desc, fuel_type, unit) |> 
  summarise(value = sum(value)) |> 
  filter(value != 0)

#--change to btu/stand
d1 <- 
  d |> 
  mutate(value = value * btu_per_mj,
         unit = "btu/stand")

d1

# energy reqd to manu fuel ------------------------------------------------

d2 <- 
  d1 |> 
  left_join(m1) |> 
  mutate(manu_btu = value * fuel_manu,
         manu_mj = manu_btu * mj_per_btu)


# clean it up -------------------------------------------------------------

d3 <- 
  d2 |> 
  mutate(unit = "mj/stand",
         value = manu_mj, 
         cat = "fuel manufacture") |> 
  select(production_id, assump_id, fuel_type, cat, desc, unit, value)


d3 |> 
  write_csv("R/data_tidy/energy_fuel-manu.csv")


