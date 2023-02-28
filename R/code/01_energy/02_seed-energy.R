# calculate energy use
#created 2/16
#--2/23 eliminated harvest ops energy use from seed calcs
#--2/28 - updated to production_id/assumption_id

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


#--this needs fixed to not include all of the harvest passes

# assumptions -------------------------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) |> 
  fill(assumption_id, cat) |> 
  select(-notes) |> 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value)

a


# seeds -------------------------------------------------------------------

f <- read_csv("R/data_tidy/energy_fert.csv")
p <- read_csv("R/data_tidy/energy_pest.csv")
fu <- read_csv("R/data_tidy/energy_fuel-use.csv") |> filter(grepl("field ops", desc))
#--not sure whether to include this or not
fm <- read_csv("R/data_tidy/energy_fuel-manu.csv") |> filter(grepl("field ops", desc))
i <- read_csv("R/data_tidy/energy_irrig.csv")


tot <- 
  f |> 
  bind_rows(p) |> 
  bind_rows(fu) |> 
  bind_rows(fm) |> #--should I include this?! Mmmm not sure. 
  bind_rows(i) |> 
  fill(production_id, assumption_id, .direction = "downup") |> 
  group_by(production_id, assumption_id, unit) |> 
  summarise(value = sum(value))


# calc energy per unit seed produced --------------------------------------


a_seed_yld <- 
  a |>
  filter(cat_ass == "seed manufacture",
         desc == "seed yield") |>
  mutate(
    seed_yld_kg_ha = as.numeric(value_ass) * kg_per_lb * ac_per_ha
  ) |> 
  select(assumption_id, seed_yld_kg_ha)


a_seed_energy <- 
  tot |> 
  #--assume this is energy used to create the given seed yield
  left_join(a_seed_yld) |> 
  #--get mj/kg seed produced
  mutate(mj_kgseed = value/seed_yld_kg_ha) |>
  select(production_id, assumption_id, mj_kgseed) 

#--ftm has a much lower value. But whatever. 
a_seed_energy |> 
  mutate(btu_lbseed = mj_kgseed * kg_per_lb * btu_per_mj) |> 
  mutate(ftm_value = 1973)

#--get seeding rate
s <- read_csv("R/data_tidy/prod_seeds.csv")


s1 <- 
  s |> 
  left_join(a_seed_energy) |> 
  #--multiply by amount of seed used to plant
  mutate(value = value * mj_kgseed,
         unit = "mj/stand") |> 
  group_by(production_id, assumption_id, cat, desc, unit) |> 
  summarise(value = sum(value))


s1 |> 
  write_csv("R/data_tidy/energy_seed.csv")

