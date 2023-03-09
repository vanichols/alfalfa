#--calculate n2o emissions avoided from avoided fertilizer
#--use fertilizer n only (assume plant n is not impacted)
#--direct + indirect
#--have to assume a type of fertilizer that is avoided
#--have to assume a crop - let's do tomatoes
#--3/9 clean up



library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")

# assumptions -------------------------------------------------------------

a0 <- 
  read_csv("R/data_inputs/datin_assumptions.csv",
           skip = 5) 

a <- fun_preproc_assum(a0)

#--what timespan for gwp
a_gwp <- 
  a |> 
  filter(assump_cat == "gwp") |> 
  pull(assump_value)

#--assumed crop
a_crop <- "tomatoes"

#--assumed type and amount of fertilizer avoided
a_amount <- 
  a |> 
  filter(assump_cat %in% c("fertilizer avoidance")) |> 
  filter(grepl(a_crop, assump_desc)) |> 
  mutate(avoided_lbnac = as.numeric(assump_value)) |> 
  select(assump_id, avoided_lbnac)

a_type <- 
  a |> 
  filter(assump_cat %in% c("fertilizer avoidance")) |> 
  filter(assump_desc == "type of fertilizer avoided") |> 
  mutate(fert_type = assump_value) |> 
  select(assump_id, fert_type)

a_avoid <- 
  a_amount |> 
  left_join(a_type)

#-ipcc assumptions
a_ipcc <- 
  a %>% 
  filter(assump_cat %in% c("n2o direct", "n2o indirect")) %>% 
  mutate(assump_value = as.numeric(assump_value)) 

#--direct emissions assumps
a_dir <- 
  a_ipcc %>% 
  filter(assump_cat == "n2o direct") %>%
  pivot_wider(names_from = assump_desc, values_from = assump_value) %>% 
  janitor::clean_names() %>% 
  select(-assump_cat, -assump_unit)

#--the different volat constants for each fertilizer type
a_indir <- 
  a_ipcc %>% 
  filter(assump_cat == "n2o indirect") %>% 
  filter(grepl("synthetic n,|organic n,", assump_desc)) %>% 
  separate(assump_desc, into = c("x", "fert_cat"), sep = ",") %>% 
  select(-x) %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate(assump_value = as.numeric(assump_value))

# gwp ---------------------------------------------------------------------

gwp_n2o <- 
  read_excel("R/data_refs/refbyhand_gwp.xlsx", skip = 5) |> 
  filter(time_horizon == a_gwp) |> 
  filter(molecule == "n2o") |> 
  pull(global_warming_potential)

# fertilizer --------------------------------------------------------------

fert_cat_ref <- read_csv("R/data_refs/refbyhand_fert-category.csv", skip = 5) %>% select(-notes)

f1 <- 
  a_avoid |> 
  left_join(fert_cat_ref) |> 
  mutate(value = avoided_lbnac * kg_per_lb * ac_per_ha,
         unit = "kg n",
         desc = "fert n") %>% 
  select(assump_id, desc, unit, value, fert_cat)


# direct n2o emissions----------------------------------------------------------

ghg_dir <- 
  f1 %>%  
  left_join(a_dir) %>% 
  mutate(n2oN_kg = kg_n_n2o_emitted_per_kg_applied_residue_n * value,
         value2 = n2oN_kg * n_to_n2o * gwp_n2o,
         unit = "kg co2e") %>% 
  mutate(desc = "direct",
         cat = "n2o avoidance") %>% 
  select(assump_id, cat, desc, unit, value2)

########################----not sure what I did......
#--indirect
ghg_ind <- 
  f1 %>% 
  left_join(a_indir) %>% 
  rename("kg_n_volatized_per_kg_applied_n" = assump_value)

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
         assump_desc = "indirect, volatilize") %>% 
  select(scenario_id, unit, assump_desc, value2)

#--do the calcs for leaching
ghg_leach <- 
  ghg_ind %>% 
  mutate(value2 = 
           value * kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached,
         unit = "kg n2o-n leach",
         assump_desc = "indirect, leach") %>% 
  select(scenario_id, unit, assump_desc, value2)


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
