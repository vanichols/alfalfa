#--NRCS gives us diesel use
#--we want to convert that to a raw amount of energy required
#--later, we use conversion efficiencies to get the actaul energy consumed to get this task done
#--updated 2/24

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

#--energy content of diesel
e_diesel_mj_l <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5) %>% 
  filter(
    desc == "energy content",
    fuel_type == "diesel") %>%  
  pull(value)
  

#--assumed diesel fuel consumption for each type of operation
a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(assumption_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  filter(grepl("fuel", cat_ass)) %>% 
  mutate(value_ass = as.numeric(value_ass))




# fuel usage -------------------------------------------------------------

fo <- read_csv("R/data_tidy/prod_fieldops.csv")
ho <- read_csv("R/data_tidy/prod_harvestops.csv")

o <- bind_rows(fo, ho)

#--get total l used for each operation per stand life
o1 <- 
  o %>% 
  left_join(a, by = c("desc")) %>% 
  #--#of passes times L used per pass
  mutate(value = value * value_ass,
         unit = "l diesel/stand") %>% 
  select(production_id, assumption_id, desc, value, unit)

o2 <- 
  o1 %>% 
  #--L used times energy per L is MJ per stand
  mutate(value = value * e_diesel_mj_l,
         unit = "mj/stand") 

#--separate operations into harvest and field ops
o3 <- 
  o2 %>% 
  mutate(desc = ifelse(grepl("hay", desc), "harvest", "field ops")) %>% 
  group_by(production_id, assumption_id, desc, unit) %>% 
  summarise(diesel_energy = sum(value, na.rm = T)) %>% 
  mutate(cat = "field passes")

# the actual energy used will depend on the actual fuel used --------------
#--since we assume diesel, we will use that conversion factor to back-calculate the energy req'd
e_diesel_eff <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5) %>% 
  filter(
    desc == "thermal efficiency",
    fuel_type == "diesel") %>%  
  pull(value)


# (thermal efficiency diesel) x (energy req'd) = (diesel energy req'd)
o4 <- 
  o3 %>% 
  mutate(eff = e_diesel_eff/100,
         value = diesel_energy * eff) %>% 
  select(-diesel_energy, -eff)
  
  

o4 %>% 
  write_csv("R/data_tidy/energy_field-passes.csv")
