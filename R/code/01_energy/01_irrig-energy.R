# calculate energy use
#created 2/16
#--2/28 - updated to production_id/assumption_id

#--note: using ucanr equations, they match ftm roughly

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) |> 
  fill(assumption_id, cat) |> 
  select(-notes) |> 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) |> 
  filter(cat_ass == "irrigation") |> 
  filter(value_ass != "diesel") |> 
  mutate(value_ass = as.numeric(value_ass))

a

a_effs <- 
  a |> 
  filter(grepl("eff", desc)) |> 
  separate(desc, into = c("type", "x", "xx")) |> 
  mutate(eff_frac = value_ass) |> 
  select(assumption_id, type, eff_frac)


a_pct_surf <- 
  a |> 
  filter(desc == "fraction from surface source") |> 
  pull(value_ass)

a_welldepth_ft <-
  a |>
  filter(desc == "depth of well") |>
  pull(value_ass)


a_pump_pres_psi <-
  a |>
  filter(desc == "pump pressure") |>
  pull(value_ass)



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
  select(production_id, assumption_id, cat, desc, mj_per_ha) |> 
  rename(value =  mj_per_ha) |> 
  mutate(unit = "mj/stand")


i4 |> 
  write_csv("R/data_tidy/energy_irrig.csv")
