# calculate energy use
#created 2/16
#--2/28 - updated to production_id/assumption_id
#--3/1 update and check/clean

#--note: using ucanr equations, they match ftm roughly

#rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# assumptions -------------------------------------------------------------

a <- read_csv("R/data_inputs/datin_assumptions.csv", skip = 5) 
a1 <- fun_preproc_assum(a)

#--which energy content source to use
a_ds <- 
  a1 |> 
  filter(assump_cat == "data source") |> 
  pull(assump_value)

#--which fuel used for irrigation
a_fu <- 
  a1 |> 
  filter(assump_cat == "energy source",
         assump_desc == "irrigation") |> 
  rename(fuel_type = assump_value) |> 
  select(assump_id, fuel_type)

#--the efficienvies of irrigation
a_effs <- 
  a1 |> 
  filter(assump_cat == "irrigation") |> 
  mutate(assump_value = as.numeric(assump_value)) |> 
  filter(grepl("eff", assump_desc)) |> 
  #--get just the type (sprinkler, surface, drip)
  separate(assump_desc, into = c("type", "x", "xx")) |> 
  mutate(eff_frac = assump_value) |> 
  select(assump_id, type, eff_frac)

#--assumed percentage of water needs satisfied with surface water
a_pct_surf <- 
  a1 |>
  filter(assump_cat == "irrigation") |> 
  mutate(assump_value = as.numeric(assump_value)) |> 
  filter(assump_desc == "fraction from surface source") |> 
  pull(assump_value)

a_welldepth_ft <-
  a1 |>
  filter(assump_cat == "irrigation") |> 
  mutate(assump_value = as.numeric(assump_value)) |> 
  filter(assump_desc == "depth of well") |>
  pull(assump_value)


a_pump_pres_psi <-
  a1 |> 
  filter(assump_cat == "irrigation") |> 
  mutate(assump_value = as.numeric(assump_value)) |> 
  filter(assump_desc == "pump pressure") |>
  pull(assump_value)


# refs --------------------------------------------------------------------

#--get thermal efficiency of assumed fuel
f_eff <- 
  a_fu |> 
  left_join(
  read_csv("R/data_refs/ref_fuel-conv-eff.csv") 
  ) |> 
  mutate(therm_eff = value/100) |> 
  select(-value, -unit)


#--energy content of fuels using assumed data source
f_en <- 
  a_fu |> 
  left_join(
  read_csv("R/data_refs/ref_fuel-energy.csv")) |> 
  filter(source == a_ds) |> 
  mutate(energy_cont = value) |> 
  select(assump_id, fuel_type, energy_cont)
  
# i. irrigation --------------------------------------------------------------

i <- read_csv("R/data_tidy/prod_irrigation.csv")

#--need to know the type, the source, the amount
#--the source is based on the assumption 

i1 <- 
  i |> 
  separate(desc, into = c("type", "x"), sep = ",") |> 
  filter(grepl("ac-in", unit)) |> 
  mutate(water_applied_ac_in = value) |> 
  select(production_id, cat, type, water_applied_ac_in) 

i2 <- 
  i1 |> 
  mutate(surface = water_applied_ac_in * a_pct_surf,
         ground = water_applied_ac_in - surface) |>
  select(-water_applied_ac_in) |>
  pivot_longer(surface:ground, values_to = "water_acin") |>
  mutate(pump_press_psi = a_pump_pres_psi,
         welldepth_ft = ifelse(name == "ground",
                              a_welldepth_ft,
                              0))

#--do some goofy conversions
i3 <- 
  i2 |> 
  left_join(a_effs, by = "type") |> 
  mutate(
    pump_press_ft = pump_press_psi * fthead_per_psi,
    #--change ac-in to gallons, then pounds of water
    water_galac = water_acin * ft_per_in * gal_per_acft,
    water_lbsac = water_galac * lb_per_gal_water,
    #--btus used per foot pound
    ftlb_per_ac = water_lbsac * (pump_press_ft + welldepth_ft),
    btu_per_ac = ftlb_per_ac * btu_per_ftlb,
    #--take into account eff
    mj_per_ha = btu_per_ac * ha_per_ac * mj_per_btu / eff_frac
  ) 
  

i4 <- 
  i3 |> 
  unite(type, name, col = "desc", sep = ", ") |> 
  select(production_id, assump_id, cat, desc, mj_per_ha) |> 
  rename(value =  mj_per_ha) |> 
  mutate(unit = "mj/stand")



i4


# take into account type of fuel used -------------------------------------

i5 <- 
  i4 |> 
  left_join(a_fu) |> 
  left_join(f_eff)

i6 <- 
  i5 |> 
  mutate(energy_reqd = value/(therm_eff)) |> 
  select(production_id,
         assump_id,
         cat,
         desc,
         fuel_type,
         unit, energy_reqd) |> 
  rename(value = energy_reqd)

# save it -----------------------------------------------------------------

i6 |> 
  write_csv("R/data_tidy/energy_irrig.csv")
