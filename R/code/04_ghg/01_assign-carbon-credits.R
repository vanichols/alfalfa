#--using california healthy soils reference and assumptions, assign carbon credits
#--2/28


library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# data for stand life --------------------------------------------------------------------

d_raw <- read_csv("R/data_raw/lca-sheets/raw_production.csv",
                  skip = 5) %>% 
  janitor::remove_empty()

d <- fun_preproc(d_raw)

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(production_id, stand_life_yrs)



# reference table ---------------------------------------------------------

ca <- read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx",
                 skip = 5) %>% 
  fill(unit)

# assumed county and program ----------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) %>% 
  fill(assumption_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  filter(cat_ass == "carbon credit")

a_scen <- 
  a %>% 
  select(assumption_id, desc, value_ass) %>% 
  pivot_wider(names_from = desc, values_from = value_ass)


# get carbon credit -------------------------------------------------------

c1 <- 
  a_scen %>% 
  left_join(ca) %>% 
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
  select(production_id, assumption_id, cat, desc, unit, value)


c3 %>% 
  write_csv("R/data_tidy/ghg_carboncredit.csv")

