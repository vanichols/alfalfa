#--processing lca sheet
#--pesticide flows are done separately bc they are complicated
#--I don't like Joel's wide format, use long format
#--2/14 - added field ops

rm(list = ls())

library(tidyverse)
library(readxl)
library(measurements)


source("R/code/00_conversions.R")


# clean up function -------------------------------------------------------


fun_preproc <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(system, .direction = c("down")) %>% 
    fill(flow_type, .direction = c("down")) %>% 
    fill(flow_cat, .direction = c("down")) %>% 
    select(-notes) %>% 
    pivot_longer(mid:worst) 
    
  return(tmp)
  }



# stand life ---------------------------------------------------------------


sl <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                 sheet = "stand_life",
                 skip = 5)

# field ops -----------------------------------------------------

fops <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "field_ops", 
                   skip = 5)

fops1 <- 
  fun_preproc(data = fops)

fops2 <- 
  fops1 %>% 
  group_by(system, flow_type, flow_cat, flow_desc, units, name) %>% 
  summarise(value = sum(value, na.rm = T))

fops2 %>% 
  write_csv("R/data_tidy/lca_fieldops.csv")



# harvest ops -----------------------------------------------------

hops <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "harvest_ops", 
                   skip = 5)

hops1 <- 
  fun_preproc(data = hops) %>% 
  fill(units)

hops2 <- 
  hops1 %>%
  left_join(sl) %>% 
  mutate(value = value * stand_life_yrs) %>% 
  group_by(system, flow_type, flow_cat, flow_desc, units, name) %>% 
  summarise(value = sum(value, na.rm = T))

hops2 %>% 
  write_csv("R/data_tidy/lca_harvestops.csv")


# yields ------------------------------------------------------------------


ylds <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "yields", 
                   skip = 5)

ylds1 <- 
  fun_preproc(data = ylds)


ylds2 <- 
  ylds1 %>% 
  left_join(sl) %>% 
  #-convert units
  mutate(value2 = case_when(
    units == "ton/ac/yr at 10% moisture" ~ (value * (1 - 0.1)),
    units == "ton/ac/yr at 30% moisture" ~ (value * (1 - 0.3)),
    TRUE ~ 0)
  ) %>% 
  group_by(system, flow_type, flow_cat, stand_life_yrs, name) %>% 
  summarise(value_ton_per_ac_per_yr = sum(value2)) %>% 
  #--change to kg per ha
  mutate(value_kg_per_ha_per_year = 
           value_ton_per_ac_per_yr * 
           lbs_per_ton * 
           kg_per_lb * 
           ac_per_ha,
          units = "kg",
         value = value_kg_per_ha_per_year * stand_life_yrs) %>% 
  select(system, flow_type, flow_cat, name, stand_life_yrs, units, value)

ylds2 %>% 
  write_csv("R/data_tidy/lca_yields.csv")    


# seed --------------------------------------------------------------------

seeds <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "seed", 
                   skip = 5)

seeds1 <- 
  fun_preproc(data = seeds)

seeds2 <- 
  seeds1 %>% 
#--change to be on a hectare basis
  mutate(
    value_lbs_per_ac = value,
    value_kg_per_ha = value_lbs_per_ac * kg_per_lb * ac_per_ha,
    units = "kg"
    ) %>% 
  select(system, flow_type, flow_cat, flow_desc, name, units, value_kg_per_ha) 

seeds2 %>% 
  write_csv("R/data_tidy/lca_seeds.csv")


# fertility ---------------------------------------------------------------


fert <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                    sheet = "fertility", 
                    skip = 5)

fert1 <- 
  fun_preproc(data = fert)

fert1_pou <- 
  fert1 %>% 
  filter(grepl("poultry", flow_desc)) %>% 
  fill(value)

fert2_pou <- 
  fert1_pou  %>% 
  mutate(
    value = value * 0.9 * lbs_per_ton * kg_per_lb * ac_per_ha,
    units = "kg",
    flow_desc = "composted poultry litter"
  )
  

fert1_map <- 
  fert1 %>% 
  filter(grepl("MAP", flow_desc)) %>% 
  fill(value)

fert2_map <- 
  fert1_map %>% 
  mutate(
    value = value * kg_per_lb * ac_per_ha,
    units = "kg",
    flow_desc = "11-52-0-MAP"
  )

fert2 <- 
  fert2_map %>% 
  bind_rows(fert2_pou)

fert2 %>% 
  write_csv("R/data_tidy/lca_fertility.csv")



# irrigation --------------------------------------------------------------


irrig <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "irrigation", 
                   skip = 5)

irrig1 <- 
  fun_preproc(data = irrig)

irrig2 <- 
  irrig1 %>% 
  #-change from ac-in to ha-m? or liters?
  mutate(value_ha_m = value * ha_per_ac * m_per_in,
         value_l = value_ha_m * m2_per_ha * l_water_per_m3) %>% 
  group_by(system, flow_type, flow_cat, flow_desc, name) %>% 
  summarise(value = sum(value_l)) %>% 
  mutate(units = "l")

irrig2 %>% 
  write_csv("R/data_tidy/lca_irrigation.csv")



