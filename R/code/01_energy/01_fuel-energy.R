# calculate energy use
#created 2/16
#--2/23 added option to change fuel source for field and harvest ops
#--- in assumptions file, but did not incorporate into code yet

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  filter(grepl("fuel energy", cat_ass)) %>%
  

# o. fuel usage energy -------------------------------------------------------------


fo <- read_csv("R/data_tidy/prod_fieldops.csv")
ho <- read_csv("R/data_tidy/prod_harvestops.csv")

o <- bind_rows(fo, ho)

o1 <-
  o %>% 
  select(-cat) %>% 
  left_join(
    a %>% 
      filter(cat_ass == "fuel") %>% 
      mutate(value_ass = as.numeric(value_ass)), 
    by = c("scenario_id", "desc")
  ) 


e_diesel <- 
  a %>% 
  mutate(
    value_ass = as.numeric(value_ass),
    value_MJ_per_Ldiesel = value_ass * mj_per_btu * 1/l_per_gal) %>% 
  select(scenario_id, value_MJ_per_Ldiesel)


o2 <- 
  o1  %>% 
  #--#of passes times L used per pass
  mutate(value = value * value_ass) %>% 
  select(scenario_id, desc, value, unit_ass) %>% 
  left_join(e_diesel) %>% 
  #--L used times energy per L is MJ per stand
  mutate(value = value * value_MJ_per_Ldiesel,
         unit = "mj/stand") 


o3 <- 
  o2 %>% 
  mutate(desc = ifelse(grepl("hay", desc), "harvest", "field ops")) %>% 
  group_by(scenario_id, desc, unit) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  mutate(cat = "fuel use")

o3 %>% 
  write_csv("R/data_tidy/energy_fuel.csv")
