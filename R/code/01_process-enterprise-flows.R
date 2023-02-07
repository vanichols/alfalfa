#--think about processing lca sheet

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



# yields ------------------------------------------------------------------


ylds <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "yields", 
                   skip = 5)

ylds1 <- 
  fun_preproc(data = ylds)


ylds2 <- 
  ylds1 %>% 
  #-convert units
  mutate(value2 = case_when(
    units == "ton/ac/yr at 10% moisture" ~ (value * (1 - 0.1)),
    units == "ton/ac/yr at 30% moisture" ~ (value * (1 - 0.3)),
    TRUE ~ 0)
  ) %>% 
  group_by(system, flow_type, flow_cat, name) %>% 
  summarise(value_ton_per_ac_per_yr = sum(value2)) %>% 
  #--change to kg per ha
  mutate(value_kg_per_ha_per_year = 
           value_ton_per_ac_per_yr * 
           lbs_per_ton * 
           kg_per_lb * 
           ac_per_ha,
          units = "kg",
         value = value_kg_per_ha_per_year * 3) %>% 
  select(system, flow_type, flow_cat, name, units, value) %>% 
  pivot_wider(names_from = system, values_from = value) %>% 
  janitor::clean_names()

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
  select(system, flow_type, flow_cat, flow_desc, name, units, value_kg_per_ha) %>% 
  pivot_wider(names_from = system, 
              values_from = value_kg_per_ha) %>% 
  janitor::clean_names()

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
  bind_rows(fert2_pou) %>% 
  pivot_wider(names_from = system, values_from = value) %>% 
  janitor::clean_names()

fert2 %>% 
  write_csv("R/data_tidy/lca_fertility.csv")

