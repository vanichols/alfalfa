#--using california healthy soils reference and assumptions, assign carbon credits
#--2/28
#--3/8 updating w/new file structures
#--note they already converted to co2e using their own conversions...

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_inputs/datin_assumptions.csv", skip = 5) 
a1 <- fun_preproc_assum(a)


#--get assumed county and practice scenario
a_scen <- 
  a1 %>% 
  filter(assump_cat == "carbon credit") |> 
  select(assump_id, assump_desc, assump_value) %>% 
  pivot_wider(names_from = assump_desc, values_from = assump_value)


# data for stand life --------------------------------------------------------------------

d_raw <- read_csv("R/data_inputs/datin_production.csv",
                  skip = 5) %>% 
  janitor::remove_empty()

d <- fun_preproc_prod(d_raw)

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(production_id, stand_life_yrs)


# reference table ---------------------------------------------------------

ca <- read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx",
                 skip = 5) %>% 
  fill(unit) |> 
  mutate_if(is.character, str_to_lower)



# get carbon credit -------------------------------------------------------

c1 <- 
  a_scen %>% 
  left_join(ca, by = c("county", "practice")) %>% 
  pivot_longer(co2:n2o)

#--change units, do total years
c2 <- 
  c1 %>% 
  bind_cols(sl) %>% 
  mutate(co2e_kghayr = 
          value * ac_per_ha * 1000,
         co2e_kghastand = co2e_kghayr * stand_life_yrs) 


#--make values negative because it is a sequestering
c3 <- 
  c2 %>% 
  #--make consistent with other formats
  mutate(
    cat = "carbon credit",
    desc = name,
    value = -co2e_kghastand,
    unit = "kg co2e/stand") %>% 
  select(production_id, assump_id, cat, desc, unit, value)


c3

c3 %>% 
  write_csv("R/data_tidy/ghg_carboncredit.csv")

