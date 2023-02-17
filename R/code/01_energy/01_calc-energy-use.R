# calculate energy use
#created 2/16

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value)

a



# fertilizer manu energy -------------------------------------------------------

f <- read_csv("R/data_tidy/prod_fertility.csv")

f1 <- 
  f %>% 
  left_join(a %>%
              filter(grepl("fert", cat_ass)) %>% 
              mutate(value_ass = as.numeric(value_ass)), 
            by = c("scenario_id", "desc")) 

f2 <- 
  f1 %>% 
  mutate(value = value * value_ass,
         unit = "MJ / stand",
         cat = cat_ass) %>% 
  select(scenario_id, cat, desc, unit, value)


# pesticide manu energy ---------------------------------------------------

p <- read_csv("R/data_tidy/prod_pesticides.csv")

p1 <-
  p %>% 
  select(-cat) %>% 
  left_join(
    a %>% 
      filter(cat_ass == "pesticide manufacture") %>% 
      mutate(value_ass = as.numeric(value_ass)), 
    by = c("scenario_id", "desc")
    ) 

p2 <- 
  p1 %>%
  mutate(value = value * value_ass,
         unit = "MJ / stand",
         cat = cat_ass) %>% 
  select(scenario_id, cat, desc, unit, value)


# fuel manu energy -------------------------------------------------------------


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
  filter(grepl("fuel man", cat_ass)) %>%
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
         unit = "mj/stand") %>% 
  mutate(cat = ifelse(grepl("hay", desc), "harvest fuel", "fieldops fuel")) %>% 
  group_by(scenario_id, cat, unit) %>% 
  summarise(value = sum(value, na.rm = T))


# seeds -------------------------------------------------------------------


# irrigation --------------------------------------------------------------
#--I don't know if sprinkler vs flood should be treated differently

i <- read_csv("R/data_tidy/prod_irrigation.csv")

i_sprink <- 
  i %>% 
  filter(grepl("sprink", desc))


i_flood <- 
  i %>% 
  filter(grepl("flood", desc))

#--assuptions
a_pct <- 
  a %>% 
  filter(cat_ass == "irrigation",
         desc == "fraction from surface source") %>% 
  mutate(value_ass = as.numeric(value_ass))

a_welldepth <- 
  a %>% 
  filter(cat_ass == "irrigation",
         desc == "depth of well") %>%  
  mutate(value_ass = as.numeric(value_ass)) %>% 
  mutate(value_ass = value_ass * m_per_ft,
         unit_ass = "m")


a_pumppres <- 
  a %>% 
  filter(cat_ass == "irrigation",
         desc == "pump pressure") %>%  
  mutate(value_ass = as.numeric(value_ass)) %>% 
  mutate(value_ass = value_ass * mhead_per_psi,
         unit_ass = "m")


i_flood %>%
  mutate(surface = value * a_pct %>% pull(value_ass),
         ground = value - surface) %>% 
  select(-value) %>% 
  pivot_longer(surface:ground)
  
  