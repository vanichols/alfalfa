#--processing fertility component of scenario sheet
#--created 2/15

rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")


# clean up function -------------------------------------------------------

d_raw <- read_excel("R/data_raw/lca-sheets/enterprise-flows-scenario-format.xlsx",
                    sheet = "scenarios",
                    skip = 5)


d <- fun_preproc(d_raw)


# fertility ---------------------------------------------------------------

#--need to know how many assumed applications there are per stand life

f_passes <- 
  d %>% 
  filter(cat == "field ops") %>% 
  filter(grepl("fertilize", desc)) %>% 
  group_by(scenario_id, cat, desc) %>% 
  summarise(value = sum(value))

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

f2_map %>% 
  write_csv("R/data_tidy/lca_fertility.csv")

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
