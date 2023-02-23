#--calculate n2o emissions
#--use fertilizer n and plant n inputs
#--direct + indirect
#--2/15 - need to do indirect
#--2/22 cleaned up, still lacks indirect

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")

# data for stand life --------------------------------------------------------------------

d_raw <- read_csv("R/data_raw/lca-sheets/raw_cv_001.csv",
                  skip = 5) %>% 
  janitor::remove_empty()

d <- fun_preproc(d_raw)

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(scenario_id, stand_life_yrs)

# ipcc --------------------------------------------------------------------



# use paper to check calcs ------------------------------------------------

# https://link.springer.com/article/10.1007/s10705-016-9808-8
# The current IPCC Tier 1 emission factor estimates N2O emissions as one percent 
# of N inputs, which are accounted for as above- and belowground residues 
# during forage crop renewal (plowing under of an existing forage crop). 
# To calculate annual N inputs in perennial legume systems, 
# the residue N during crop renewal is divided by the number of years 
# of continuous cultivation (IPCC 2006). 

# A1 = DM produced annually (kg DM/ha)
# A2 = N content of aboveground biomass (kg N/ kgDM)
# A3 = fraction of aboveground biomass not harvested (defaults to 0.1)
A3 <- 0.1
# B1 = default factor estimating root dry matter as a function of annual 
#  aboveground biomass; 0.4
B1 <- 0.4
# B2 = default N fraction of alfalfa roots (kg/kg DM); 0.019
B2 <- 0.019

# C1 = renewal rate per year (1/stand life), I don't quite get this. It's assuming the n2o is releaesd upon termination?
C1 <- 1/4

# inorganic N mineralized (kg N/ha) = [A1*A2*A3 + A1*B1*B2]*C1
# Nfert must also be added
# numbers from paper as example:
Nfert <- 18


# 2nd year stand = 58 kg N/ha
A1_2yr <- 14100
A2_2yr <- 525/A1_2yr # they measured this - 525 kg of N per ha
Ninputs_2yr <- (A1_2yr * A2_2yr * A3 + A1_2yr * B1 * B2) * C1 + Nfert
ipcc_2yr <- 0.01 * Ninputs_2yr #kg N2O-N / ha / yr

# 5th year stand = 53 kg N/ha
A1_5yr <- 12100
A2_5yr <- 473/A1_5yr
Ninputs_5yr <- (A1_5yr * A2_5yr * A3 + A1_5yr * B1 * B2) * C1 + Nfert
ipcc_5yr <- 0.01 * Ninputs_5yr #kg N2O-N / ha / yr




# assumptions -------------------------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
                skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value) %>% 
  filter(cat_ass == "n2o")

# Use assumptions from IPCC 2019 refinement, Table 11.1A
a1 <- 
  a %>% 
  mutate(value_ass = as.numeric(value_ass)) %>% 
  pivot_wider(names_from = desc, values_from = value_ass) %>% 
  janitor::clean_names() %>% 
  select(-cat_ass, -unit_ass)


# note it is assumed all the n2o is released when the stand is terminated
# therefore the total is amortized over the standlife. Longer standlife = less n2o
# from IPCC 2019 refinement: the nitrogen residue from forages is only accounted for during pasture renewal


# get N from fert ---------------------------------------------------------

#--calculate kg of N applied per ha via fertilizers

#--get n per unit fertilizer, clean up for merg
f_nref <- 
  read_csv("R/data_refs/ref_fert-n.csv") %>% 
  rename(desc = fert_type) %>% 
  mutate(kgn_kgfert = value) %>% 
  select(desc, kgn_kgfert)

f <- read_csv("R/data_tidy/prod_fertility.csv")


f_n <- 
  f %>% 
  left_join(f_nref) %>% 
  mutate(f_ntot = value * kgn_kgfert) %>% 
  group_by(scenario_id) %>% 
  summarise(value = sum(f_ntot, na.rm = T),
            unit = "kg n/stand",
            desc = "fert n")


# get N from plants --------------------------------------------------------

h_dm <- 
  read_csv("R/data_tidy/prod_yields.csv") %>%
  group_by(scenario_id) %>% 
  summarise(value = sum(value)) %>% 
  left_join(sl) %>% 
  mutate(kgdm_per_year = value/stand_life_yrs) %>%
  select(scenario_id, stand_life_yrs, kgdm_per_year) 

  
p_n <- 
  h_dm %>% 
  left_join(a1) %>% 
  mutate(
    dm_n = kgdm_per_year * fraction_of_dm_not_harvested * kg_of_n_per_kg_dm,
    root_n = kgdm_per_year * kg_roots_per_kg_dm_harvested * kg_n_per_kg_root_dm,
    plant_n_kg = (dm_n + root_n)/stand_life_yrs 
  )
    
p_n1 <- 
  p_n %>% 
  mutate(value = plant_n_kg,
         unit = "kg n/stand",
         desc = "plant n") %>% 
  select(scenario_id, desc, value, unit)
    
# convert to n2o ----------------------------------------------------------

# assume 0.5% of n inputs to account for direct n2o emissions (all inputs in dry climates, Table 11.1) 

ghg <- 
  p_n1 %>% 
  bind_rows(f_n) %>%  
  mutate(n2oN_kg = 0.005 * value,
         n2o_kg = n2oN_kg * 44/28) %>% 
  select(-value, -n2oN_kg) 

#---just look at it
ghg %>% 
  mutate(co2_eq_kg = n2o_kg * 298) %>% 
  ggplot(aes(desc, co2_eq_kg)) + 
  geom_col(aes(fill = desc), color = "black") + 
  labs(y = "kg co2-eq per ha",
       x = NULL,
       title = "N2O emissions, IPCC method") 


ghg1 <- 
  ghg %>% 
  mutate(value = n2o_kg * 298,
         unit = "kg co2e/stand") %>% 
  select(-n2o_kg)

ghg1 %>% 
  write_csv("R/data_tidy/ghg_n2o.csv")
