# calculate energy required in 01_field-pass-energy
#--we have to assume a fuel used, and it's thermal efficiency (or conversion efficiency)
#--in progress, 2/24
#--change to 'tractor' rather than 'fuel use'

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")

#--relies on 01_field-pass-energy calcs

ops <- read_csv("R/data_tidy/energy_field-passes.csv")


# ref data ----------------------------------------------------------------

#--fuel energies and efficiencies
fu <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5) %>% 
  filter(desc %in% c("energy content", "thermal efficiency")) 

te <- 
  fu %>% 
  filter(desc == "thermal efficiency") %>% 
  mutate(therm_eff = value) %>% 
  select(fuel_type, therm_eff)

# assumptions -------------------------------------------------------------

#--type of fuel used for each operation
a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) %>% 
  fill(assumption_id, cat) %>% 
  select(-notes) %>% 
  rename(
    fuel_type = value) %>% 
  filter(grepl("energy source", cat)) %>% 
  separate(desc, into = c("x", "desc"), sep = ",") %>% 
  filter(!is.na(desc)) %>% 
  select(-x, -cat, -unit) %>% 
  mutate_if(is.character, str_trim)

a  

# field pass energy req'mts-------------------------------------------------------------

e <- read_csv("R/data_tidy/energy_field-passes.csv")

e1 <- 
  e %>% 
  left_join(a) %>% 
  left_join(te)


e2 <- 
  e1 %>% 
  #--calculate amoutn of energy we need to generate based on fuel effiency
  mutate(energy_needed = value / (therm_eff/100)) %>%
  select(production_id, 
         assumption_id,
         fuel_type,
         desc, 
         unit, 
         energy_needed) |> 
  rename(value = energy_needed) |> 
  mutate(cat = "tractor")

e2


e2 %>% 
  write_csv("R/data_tidy/energy_tractor.csv")
