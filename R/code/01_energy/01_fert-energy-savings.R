# calculate energy use saved from less n requirement for next crop
#created 2/16

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

#--assumed fertilizer manufacturing energy, from greet tables
fe <- 
  read_csv("R/data_refs/ref_fert-energy.csv")

# get amount of fertilizer avoided ----------------------------------------

f <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  #--get assumed fert savings
  filter(grepl("tomatoes", desc)) %>% 
  mutate(value_ass = as.numeric(value_ass),
         kgn_ha = value_ass * kg_per_lb * ac_per_ha) %>% 
  select(scenario_id, kgn_ha)

f_type <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  #--get assumed type of fert
  filter(desc == "type of fertilizer avoided") 


# energy to manu the fertilizer we avoided --------------------------------------------------

f2 <- 
  f %>% 
  left_join(f_type %>% 
              mutate(cat = value_ass) %>% 
              select(scenario_id, cat)) %>% 
  left_join(fe, by = c("cat")) 


f3 <- 
  f2 %>% 
  mutate(value2 = -(value * kgn_ha), #--negative bc we avoided it
         unit = "mj") %>%
  mutate(cat = "fertilizer avoidance",
         desc = "avoided uan-32") %>% 
  select(scenario_id, cat, desc, unit, value2) %>% 
  rename(value = value2)

f3 %>% 
  write_csv("R/data_tidy/energy_fert-avoided.csv")
