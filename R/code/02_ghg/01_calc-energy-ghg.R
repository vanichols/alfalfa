#--change energy to ghg emissions
#--requires assumptions about source of energy
#--those are listed under the 'energy sources' category of the assumptions file



library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")

# assumptions -------------------------------------------------------------

a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) 

a_source <- 
  a %>% 
  filter(grepl("energy source", cat))


# energy ------------------------------------------------------------------


f <- read_csv("R/data_tidy/energy_fert.csv")
f_avoid <- read_csv("R/data_tidy/energy_fert-avoided.csv")
p <- read_csv("R/data_tidy/energy_pest.csv")
fu <- read_csv("R/data_tidy/energy_fuel.csv")
i <- read_csv("R/data_tidy/energy_irrig.csv")
s <- read_csv("R/data_tidy/energy_seed.csv") #--doesn't include harvest ops

tot <- 
  f %>% 
  bind_rows(f_avoid) %>% 
  bind_rows(p) %>% 
  bind_rows(fu) %>% 
  bind_rows(i) %>% 
  bind_rows(s) %>% 
  mutate(cat_short = case_when(
    cat == "fuel use" ~ "fuel",
    cat == "fertilizer manufacture" ~ "fert",
    cat == "fertilizer avoidance" ~ "avoided fert",
    cat == "pesticide manufacture" ~ "pest",
    cat == "irrigation" ~ "irrig",
    cat == "seed" ~ "seed",
    TRUE ~ "XXX"
  )) %>% 
  filter(value != 0) 

tot


# ghg emissions for each fuel type ----------------------------------------

ghg <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5)

ghg %>% 
  mutate(unit = "kg co2e/mj",
         value = case_when(
           grepl("diesel|gasoline", fuel_type) ~ co2e * gal_per_l * 1/energy_content,
           grepl("electricity", fuel_type) ~ co2e * kg_per_lb * mwh_per_kwh * 1/energy_content,
           grepl("natural gas", fuel_type) ~ co2e * cuft_per_m3 * 1/energy_content,
           TRUE ~ 999
         ))

# combine -----------------------------------------------------------------


