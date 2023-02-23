#--calculate n2o emissions avoided from avoided fertilizer
#--use fertilizer n only (assume plant n is not impacted)
#--direct + indirect
#--have to assume a type of fertilizer that is avoided
#--have to assume a crop - let's do tomatoes



library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")

# assumptions -------------------------------------------------------------

a <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value)

#-ipcc assumptions
a_ipcc <- 
  a %>% 
  filter(cat_ass %in% c("n2o direct", "n2o indirect")) %>% 
  mutate(value_ass = as.numeric(value_ass)) 


# get N from fert ---------------------------------------------------------
# Use assumptions from IPCC 2019 refinement, Table 11.1A (direct), and Table 11.3 (indirect)
a_dir <- 
  a_ipcc %>% 
  filter(cat_ass == "n2o direct") %>%
  pivot_wider(names_from = desc, values_from = value_ass) %>% 
  janitor::clean_names() %>% 
  select(-cat_ass, -unit_ass)


a_avoid <- 
  read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
           skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  filter(cat_ass %in% c("fertilizer avoidance")) 


#--assume tomatoes, add assumed fert type

fert_cat_ref <- read_csv("R/data_refs/refbyhand_fert-category.csv", skip = 5) %>% select(-notes)

f_n <- 
  a_avoid %>% 
  filter(grepl("tomatoes", desc)) %>% 
  mutate(value_ass = as.numeric(value_ass)) %>% 
  left_join(
    a_avoid %>% 
      filter(grepl("type of fertilizer avoided", desc)) %>%
      rename("fert_type" = value_ass) %>% 
      select(scenario_id, fert_type)
  ) %>% 
  left_join(fert_cat_ref) %>% 
  mutate(value = value_ass * kg_per_lb * ac_per_ha,
         unit = "kg n",
         desc = "fert n") %>% 
  select(scenario_id, desc, unit, value, fert_cat)


# direct n2o emissions----------------------------------------------------------

ghg_dir <- 
  f_n %>%  
  left_join(a_dir) %>% 
  mutate(n2oN_kg = kg_n_n2o_emitted_per_kg_applied_residue_n * value,
         value2 = n2oN_kg * n_to_n2o * n2o_to_co2e,
         unit = "kg co2e") %>% 
  mutate(desc = "direct",
         cat = "n2o avoidance") %>% 
  select(scenario_id, cat, desc, unit, value2)


# indirect n2o emissions --------------------------------------------------

a_indir <- 
  a_ipcc %>% 
  filter(cat_ass == "n2o indirect") %>% 
  #--get rid of fertilizer types assum, deal with separately
  filter(!grepl("synthetic n,|organic n,", desc)) %>% 
  pivot_wider(names_from = desc, values_from = value_ass) %>% 
  janitor::clean_names() %>% 
  select(-cat_ass, -unit_ass)

#--the different volat constants for each fertilizer type
a_indir_f <- 
  a %>% 
  filter(cat_ass == "n2o indirect") %>% 
  filter(grepl("synthetic n,|organic n,", desc)) %>% 
  separate(desc, into = c("x", "fert_cat"), sep = ",") %>% 
  select(-x) %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate(value_ass = as.numeric(value_ass))


f_n2 <- 
  f_n %>% 
  left_join(a_indir_f) %>% 
  rename("kg_n_volatized_per_kg_applied_n" = value_ass)

#--get all the constants lined up
ghg_ind <- 
  f_n2 %>% 
  left_join(a_indir) 

#--do the calcs for volatization
ghg_vol <- 
  ghg_ind %>% 
  mutate(value2 = 
           value * kg_n_volatized_per_kg_applied_n * kg_n_n2o_per_kg_n_volatalized,
         unit = "kg n2o-n vol", 
         desc = "indirect, volatilize") %>% 
  select(scenario_id, unit, desc, value2)

#--do the calcs for leaching
ghg_leach <- 
  ghg_ind %>% 
  mutate(value2 = 
           value * kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached,
         unit = "kg n2o-n leach",
         desc = "indirect, leach") %>% 
  select(scenario_id, unit, desc, value2)


#--comnbine volat and leach values, change to co2e
ghg_ind1 <- 
  ghg_vol %>% 
  bind_rows(ghg_leach) %>% 
  mutate(value2 = value2 * n_to_n2o * n2o_to_co2e,
         unit = "kg co2e") 

# add together ------------------------------------------------------------

ghg_n2o <- 
  ghg_dir %>% 
  bind_rows(ghg_ind1) %>% 
  fill(cat) %>% 
  mutate(value = -value2) %>% #--negative bc it is avoided
  select(-value2)


ghg_n2o %>% 
  write_csv("R/data_tidy/ghg_avoided-n2o.csv")
