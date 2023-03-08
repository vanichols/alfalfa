# calculate energy use
#created 2/16
#--2/23 eliminated harvest ops energy use from seed calcs
#--2/28 - updated to production_id/assumption_id
#--3/8 updating to new file formats

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# assumptions -------------------------------------------------------------

a <- read_csv("R/data_inputs/datin_assumptions.csv", skip = 5) 
a1 <- fun_preproc_assum(a)




# seeds -------------------------------------------------------------------

#--fuel
tr <- read_csv("R/data_tidy/energy_tractor.csv") |> filter(desc != "harvest")
ir <- read_csv("R/data_tidy/energy_irrig.csv")
ma <- read_csv("R/data_tidy/energy_fuel-manu.csv") |>  filter(desc != "harvest")

#--fert
f <- read_csv("R/data_tidy/energy_fert.csv")

#--pest
p <- read_csv("R/data_tidy/energy_pest.csv")


tot <- 
  tr |> 
  bind_rows(ir) |> 
  bind_rows(ma) |> 
  bind_rows(f) |> 
  bind_rows(p) |> 
  fill(production_id, assump_id, .direction = "downup") |> 
  group_by(production_id, assump_id, unit) |> 
  summarise(value = sum(value))

tot

# calc energy per unit seed produced --------------------------------------

a_seed_yld <- 
  a1 |>
  filter(assump_cat == "seed manufacture",
         assump_desc == "seed yield") |>
  mutate(
    seed_yld_kg_ha = as.numeric(assump_value) * kg_per_lb * ac_per_ha
  ) |> 
  select(assump_id, seed_yld_kg_ha)


a_seed_energy <- 
  tot |> 
  #--assume this is energy used to create the given seed yield
  left_join(a_seed_yld) |> 
  #--get mj/kg seed produced
  mutate(mj_kgseed = value/seed_yld_kg_ha) |>
  select(production_id, assump_id, mj_kgseed) 

#--ftm has a much lower value. But whatever. 
#--they don't include fuel manufacturing, irrigatigation fuel ineffic.
a_seed_energy |> 
  mutate(btu_lbseed = mj_kgseed * kg_per_lb * btu_per_mj) |> 
  mutate(ftm_value = 1973)


# seeding rate ------------------------------------------------------------

s <- read_csv("R/data_tidy/prod_seeds.csv")


s1 <- 
  s |> 
  left_join(a_seed_energy) |> 
  #--multiply by amount of seed used to plant
  mutate(value = value * mj_kgseed,
         unit = "mj/stand") |> 
  group_by(production_id, assump_id, cat, desc, unit) |> 
  summarise(value = sum(value))


s1 |> 
  write_csv("R/data_tidy/energy_seed.csv")

