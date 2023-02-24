#--NRCS gives us diesel use
#--we want to convert that to a raw amount of energy required
#--later, we use conversion efficiencies to get the actaul energy consumed to get this task done
#--updated 2/24

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

#--energy content of diesel
e_diesel <- 
  read_csv("R/data_refs/refbyhand_fuel-energy.csv", skip = 5) %>% 
  filter(fuel_type == "diesel") %>% 
  mutate(value_MJ_perLdiesel = energy_content) %>% 
  select(fuel_type, value_MJ_perLdiesel)
  

#--assumed diesel fuel consumption for each type of operation
a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  filter(grepl("fuel", cat_ass)) %>% 
  mutate(value_ass = as.numeric(value_ass))


# fuel usage -------------------------------------------------------------

fo <- read_csv("R/data_tidy/prod_fieldops.csv")
ho <- read_csv("R/data_tidy/prod_harvestops.csv")

o <- bind_rows(fo, ho)

o %>% 
  left_join(a, by = c("desc", "scenario_id")) %>% 
  #--#of passes times L used per pass
  mutate(value = value * value_ass) %>% 
  select(scenario_id, desc, value, unit_ass)



%>% 
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


# fuel manufacturing energy -----------------------------------------------

#----ohhhhhhh how to do this


o3 %>% 
  write_csv("R/data_tidy/energy_fuel.csv")
