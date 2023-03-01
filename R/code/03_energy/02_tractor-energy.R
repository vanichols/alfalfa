# calculate energy required in 01_field-pass-energy
#--we have to assume a fuel used, and it's thermal efficiency (or conversion efficiency)
#--in progress, 2/24
#--change to 'tractor' rather than 'fuel use'
#--3/1 start sorting through and cleaning


rm(list = ls())
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

#--which fuel used 
a_fu <- 
  a1 |> 
  filter(assump_cat == "energy source",
         assump_desc != "irrigation") |> 
  separate(assump_desc, into = c("x", "desc"), sep = ",") |> 
  mutate_if(is.character, str_trim) |> 
  rename(fuel_type = assump_value) |> 
  select(assump_id, desc, fuel_type)

# ref data ----------------------------------------------------------------


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



# energy required ---------------------------------------------------------

e <- read_csv("R/data_tidy/energy_field-passes.csv")



e1 <- 
  e %>% 
  left_join(a_fu) %>% 
  left_join(f_eff, by = c("assump_id", "desc", "fuel_type"))


e2 <- 
  e1 %>% 
  #--calculate amoutn of energy we need to generate based on fuel effiency
  mutate(energy_needed = value / therm_eff) %>%
  select(production_id, 
         assump_id,
         fuel_type,
         desc, 
         unit, 
         energy_needed) |> 
  rename(value = energy_needed) |> 
  mutate(cat = "tractor",
         unit = "mj/stand")

e2

e2 %>% 
  write_csv("R/data_tidy/energy_tractor.csv")
