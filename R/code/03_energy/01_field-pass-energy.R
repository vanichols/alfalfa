#--NRCS gives us diesel use
#--we want to convert that to a raw amount of energy required
#--later, we use conversion efficiencies to get the actaul energy consumed to get this task done
#--updated 2/24
#--3/1 file name/code cleanup

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# references --------------------------------------------------------------

#--energy content of diesel - multiple sources
e_diesel <- 
  read_csv("R/data_refs/ref_fuel-energy.csv") |> 
  mutate_if(is.character, str_to_lower) |> 
  filter(fuel_type == "diesel")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_inputs/datin_assumptions.csv", skip = 5) 
a1 <- fun_preproc_assum(a)

#--which energy content source to use
a_ds <- 
  a1 |> 
  filter(assump_cat == "data source") |> 
  pull(assump_value)

#--assumed mj/l in diesel
a_e_mjl <- 
  e_diesel |> 
  filter(source == a_ds) |> 
  pull(value)

#--assumed diesel fuel consumption for each type of operation
a_fu <- 
  a1 |> 
  filter(grepl("fuel", assump_cat)) %>% 
  mutate(assump_value = as.numeric(assump_value)) |> 
  mutate(ldiesel_ha = assump_value) |> 
  rename(desc = assump_desc) |> 
  select(assump_id, desc, ldiesel_ha)


# calculate fuel usage -------------------------------------------------------------

#--operations
fo <- read_csv("R/data_tidy/prod_fieldops.csv")
ho <- read_csv("R/data_tidy/prod_harvestops.csv")

o <- bind_rows(fo, ho)

#--get total l used for each operation per stand life
o1 <- 
  o %>% 
  left_join(a_fu, by = c("desc")) %>% 
  #--#of passes times L used per pass
  mutate(value = value * ldiesel_ha,
         unit = "l diesel/stand") %>% 
  select(production_id, assump_id, desc, value, unit)

#--change to mj used per stand, based on energy content of diesel
o2 <- 
  o1 %>% 
  #--L used times energy per L is MJ per stand
  mutate(value = value * a_e_mjl,
         unit = "mj/stand") 

#--separate operations into harvest and field ops
o3 <- 
  o2 %>% 
  mutate(desc = ifelse(grepl("hay", desc), "harvest", "field ops")) %>% 
  group_by(production_id, assump_id, desc, unit) %>% 
  summarise(diesel_energy = sum(value, na.rm = T)) %>% 
  mutate(cat = "field passes")


# back calculation --------------------------------------------------------

#--the actual energy used will depend on the actual fuel used
#--since we assume diesel, we will use that conversion factor to back-calculate the energy req'd

e_dies_eff <- 
  read_csv("R/data_refs/ref_fuel-conv-eff.csv") |> 
  filter(fuel_type == "diesel") |> 
  mutate(value = value/100) |> 
  pull(value)


# (thermal efficiency diesel) x (energy req'd) = (diesel energy req'd)
o4 <- 
  o3 %>% 
  mutate(value = diesel_energy * e_dies_eff,
         unit = "mj req/stand") %>% #--just to make it clear this isn't the final value
  select(-diesel_energy)
  
  
o4 %>% 
  write_csv("R/data_tidy/energy_field-passes.csv")
