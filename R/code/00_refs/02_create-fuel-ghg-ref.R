# purpose: compare fuel co2 emissions from diff sources, units are different which is hard
#--3/8 create
#--3/17 new file structure

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_funs/fxn_conversions.R")


# read in manual reference sheet ------------------------------------------

ghg <- read_excel("R/data_refs/refbyhand_fuel.xlsx", skip = 5, sheet = "combustion-co2") |> 
  select(-notes)



# fix diesel units --------------------------------------------------------

f1 <- 
  ghg |> 
  filter(fuel_type == "diesel") |> 
  mutate(
    value = case_when(
      #--epa
    unit == "kg/gal" ~ value * gal_per_l,
    unit == "g/gal" ~ value * kg_per_g * gal_per_l,
    #--grassini
    unit == "kg/l" ~ value,
    #--ipcc
    unit == "g/kg" ~ value * kg_per_g * dies_dens_g_per_ml * kg_per_g * ml_per_l,
    #--ftm
    unit == "lb/gal" ~ value * kg_per_lb * gal_per_l,
    TRUE ~ 999),
    unit = "kg/l") |> 
  arrange(molecule)

f1 

# fix gas units --------------------------------------------------------

#--need energy content of gas
#--assume the epa value
gas_ene_mj_per_l <- 
  read_csv("R/data_refs/ref_fuel-energy.csv") |> 
  filter(fuel_type == "gasoline",
         source == "epa") |> 
  pull(value)

f2 <- 
  ghg |> 
  filter(fuel_type == "gasoline") |> 
  mutate(
    value = case_when(
      #--epa
      unit == "kg/gal" ~ value * gal_per_l,
      unit == "g/gal" ~ value * kg_per_g * gal_per_l,
      #--grassini
      unit == "kg/l" ~ value,
      #--ipcc
      unit == "kg/tj" ~ value * tj_per_mj * gas_ene_mj_per_l,
      #--ftm
      unit == "lb/gal" ~ value * kg_per_lb * gal_per_l,
      TRUE ~ 999
    ),
    unit = "kg/l") |> 
    arrange(molecule)

f2

# fix electricity units ---------------------------------------------------


f3 <- 
  ghg |> 
  filter(fuel_type == "electricity") |> 
  mutate(
    value = case_when(
      unit == "lb/MWh" ~ value * kg_per_lb * mwh_per_kwh,
      unit == "kg/kwh" ~ value,
      unit == "lb/kwh" ~ value * kg_per_lb,
      TRUE ~ 999
    ),
    unit = "kg/kwh"
  )

f3

# combine -----------------------------------------------------------------


f_all <- 
  f1 |> 
  bind_rows(f2) |> 
  bind_rows(f3)


# add in conversions -----------------------

convs <- 
  read_excel("R/data_refs/refbyhand_gwp.xlsx", skip = 5) 

f_all2 <- 
  f_all |> 
  left_join(convs) |> 
  mutate(value2 = value * global_warming_potential,
         molecule = "co2e") |> 
  group_by(fuel_type, unit, source, time_horizon) |> 
  summarise(value3 = sum(value2))

f_all3 <- 
  f_all2 |> 
  rename("value" = value3)

#--grassini is so bad?!
f_all3 |> 
  ggplot(aes(time_horizon, value)) + 
  geom_point(aes(color = source), size = 4) + 
  facet_grid(.~ fuel_type)

f_all3 |> 
 write_csv("R/data_refs/ref_fuel_ghg.csv") 
