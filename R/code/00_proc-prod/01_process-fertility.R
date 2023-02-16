#--processing fertility component of scenario sheet
#--created 2/15
#--updatd 2/16, new file configs

rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")
source("R/code/00_funs.R")


# data --------------------------------------------------------------------

d_raw <- read_csv("R/data_raw/lca-sheets/raw_cv_001.csv",
                    skip = 5) %>% 
  janitor::remove_empty()


d <- fun_preproc(d_raw)


# fertility ---------------------------------------------------------------

#--need to know how many assumed applications there are per stand life

f_passes <- 
  d %>% 
  filter(cat == "field ops") %>% 
  filter(grepl("fertilize", desc)) %>% 
  group_by(scenario_id, cat, desc) %>% 
  summarise(value = sum(value))


#--separate passes by fertilizer type
f_pass_map <- 
  f_passes %>% 
  filter(grepl("map", desc)) %>% 
  pull(value)

#--now get amount per pass
f1 <- 
  d %>% 
  filter(cat == "fertility")


# map 11-52-0---------------------------------------------------------------------

f1_map <- 
  f1 %>% 
  filter(grepl("map", desc)) 

#--get total kg applied per stand, per ha
f2_map <- 
  f1_map %>% 
  mutate(
    value = value * f_pass_map * kg_per_lb * ac_per_ha,
    unit = "kg / stand"
    )

f2_map


# combine all ferts -------------------------------------------------------


f2_map %>% 
  write_csv("R/data_tidy/prod_fertility.csv")


#--old from poultry
# fert1_pou <- 
#   fert1 %>% 
#   filter(grepl("poultry", flow_desc)) %>% 
#   fill(value)
# 
# fert2_pou <- 
#   fert1_pou  %>% 
#   mutate(
#     value = value * 0.9 * lbs_per_ton * kg_per_lb * ac_per_ha,
#     units = "kg",
#     flow_desc = "composted poultry litter"
#   )
# 
