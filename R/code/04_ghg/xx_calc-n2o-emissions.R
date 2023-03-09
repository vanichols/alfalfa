#--calculate n2o emissions
#--use fertilizer n and plant n inputs
#--direct + indirect
#--2/15 - need to do indirect
#--2/22 cleaned up, still lacks indirect
#--2/23 added indirect
#--3/9 checking, moved half to prep-fert and half to calc-n2o
#--keep for reference only!!!!!!!!!!!!!!!

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")

# data for stand life --------------------------------------------------------------------

d_raw <- read_csv("R/data_inputs/datin_production.csv",
                  skip = 5) %>% 
  janitor::remove_empty()

d <- fun_preproc_prod(d_raw)

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(production_id, stand_life_yrs)

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

a0 <- 
  read_csv("R/data_inputs/datin_assumptions.csv",
                skip = 5) 

a <- 
  fun_preproc_assum(a0) 

#--what timespan for gwp
a_gwp <- 
  a |> 
  filter(assump_cat == "gwp") |> 
  pull(assump_value)


a1 <- 
  a |> 
  filter(assump_cat %in% c("n2o direct", "n2o indirect")) %>% 
  mutate(assump_value = as.numeric(assump_value)) 

# Use assumptions from IPCC 2019 refinement, Table 11.1A (direct), and Table 11.3 (indirect)
a_dir <- 
  a1 %>% 
  filter(assump_cat == "n2o direct") %>% 
  pivot_wider(names_from = assump_desc, values_from = assump_value) %>% 
  janitor::clean_names() %>% 
  select(-assump_cat, -assump_unit)

#--indir asssumps, amount volatilized/leached etc.
a_indir <- 
  a1 %>% 
  filter(assump_cat == "n2o indirect") %>% 
  #--get rid of fertilizer types assum, deal with separately
  filter(!grepl("synthetic n,|organic n,", assump_desc)) %>% 
  pivot_wider(names_from = assump_desc, values_from = assump_value) %>% 
  janitor::clean_names() %>% 
  select(-assump_cat, -assump_unit)

#--frac emitted for each type of fertilizer
a_indir_f <- 
  a1 %>% 
  filter(assump_cat == "n2o indirect") %>% 
  filter(grepl("synthetic n,|organic n,", assump_desc)) %>% 
  separate(assump_desc, into = c("x", "fert_cat"), sep = ",") %>% 
  select(-x) %>% 
  mutate_if(is.character, str_trim)


#--this is a hack
a_id <- 
  a1 |> 
  pull(assump_id) |> 
  unique()


# gwp ---------------------------------------------------------------------

gwp_n2o <- 
  read_excel("R/data_refs/refbyhand_gwp.xlsx", skip = 5) |> 
  filter(time_horizon == a_gwp) |> 
  filter(molecule == "n2o") |> 
  pull(global_warming_potential)

# note it is assumed all the n2o is released when the stand is terminated
# therefore the total is amortized over the standlife. Longer standlife = less n2o
# from IPCC 2019 refinement: the nitrogen residue from forages is only accounted for during pasture renewal


# get N from fert ---------------------------------------------------------

#--calculate kg of N applied per ha via fertilizers

#--get n per unit fertilizer, clean up for merg
f_nref <- 
  read_csv("R/data_refs/ref_fert-n-contents.csv") %>% 
  rename(desc = fert_type) %>% 
  mutate(kgn_kgfert = value) %>% 
  select(desc, kgn_kgfert)

#--read in how much of the fert was applied
f <- read_csv("R/data_tidy/prod_fertility.csv")

#--calc n applied via fert
f_n <- 
  f %>% 
  left_join(f_nref) %>% 
  mutate(f_ntot = value * kgn_kgfert) %>% 
  group_by(production_id) %>% 
  summarise(value = sum(f_ntot, na.rm = T),
            unit = "kg n/stand",
            desc = "fert n")


# get N from plants --------------------------------------------------------

#--amount of harvested dry matter each year
h_dm <- 
  read_csv("R/data_tidy/prod_yields.csv") %>%
  group_by(production_id) %>% 
  summarise(value = sum(value)) %>% 
  left_join(sl) %>% 
  mutate(kgdm_per_year = value/stand_life_yrs) %>%
  select(production_id, stand_life_yrs, kgdm_per_year) 

  
#---use IPCC assumptions to get root N and residue N
#--note this is a hack - need to think about combining them more cleanly
p_n <- 
  h_dm |> 
  mutate(assump_id = a_id) |>#--they don't have a common column 
  left_join(a_dir)  |>  
  mutate(
    dm_n = kgdm_per_year * fraction_of_dm_not_harvested * kg_of_n_per_kg_dm,
    root_n = kgdm_per_year * kg_roots_per_kg_dm_harvested * kg_n_per_kg_root_dm,
    plant_n_kg = (dm_n + root_n)/stand_life_yrs 
  )
    
#--clean up to merge with fert n
p_n1 <- 
  p_n %>% 
  mutate(value = plant_n_kg,
         unit = "kg n/stand",
         desc = "plant n") %>% 
  select(production_id, assump_id, desc, value, unit)

#--fert n and plant n    
all_n <- 
  p_n1 %>% 
  bind_rows(f_n) |> 
  fill(assump_id)


# direct n2o emissions----------------------------------------------------------

ghg_dir <- 
 all_n %>%  
  left_join(a_dir) %>% 
  mutate(n2oN_kg = kg_n_n2o_emitted_per_kg_applied_residue_n * value,
         n2o_kg = n2oN_kg * n_to_n2o) %>% 
  select(-value, -n2oN_kg) 

#---just look at it
ghg_dir %>% 
  mutate(co2_eq_kg = n2o_kg * gwp_n2o) %>% 
  ggplot(aes(desc, co2_eq_kg)) + 
  geom_col(aes(fill = desc), color = "black") + 
  labs(y = "kg co2-eq per ha",
       x = NULL,
       title = "N2O emissions, IPCC method") 


ghg_dir1 <- 
  ghg_dir %>% 
  mutate(value = n2o_kg * gwp_n2o,
         unit = "kg co2e/stand") %>% 
  mutate(desc = paste0("direct, ", desc)) %>% 
  select(production_id, assump_id, desc, unit, value)


# indirect n2o emissions --------------------------------------------------

#--assign the fertilizer to the correct category
# (urea, ammonium, nitrate, ammonium-nitrate)

f_cat_ref <- 
  read_csv("R/data_refs/refbyhand_fert-category.csv", skip = 5) %>% 
  select(-notes)

f_cat <- 
  f_cat_ref %>% 
  rename("desc2" = fert_type) %>% 
  left_join(a_indir_f %>% 
              select(fert_cat, assump_value)) %>% 
  rename("kg_n_volatized_per_kg_applied_n" = assump_value)


#--get all the constants lined up
#--note if you apply many types of fertilizer, each should be in a row here

ghg_ind <- 
  all_n %>% 
  #--get the type of fertilizer it is
  left_join(f %>% 
              select(production_id, desc) %>% 
              rename("desc2" = desc) %>% 
              mutate(desc = "fert n")) %>% 
    #--add the category and assumed %N volatilization
  left_join(f_cat) %>% 
  left_join(a_indir) 

#--do the calcs for volatization - plant n is not included here
ghg_vol <- 
  ghg_ind %>% 
  filter(desc != "plant n") %>% 
  mutate(value2 = 
           value * kg_n_volatized_per_kg_applied_n * kg_n_n2o_per_kg_n_volatalized,
         unit = "kg n2o-n vol/stand", 
         desc = "indirect, volatilize") %>% 
  group_by(production_id, unit, desc) %>% 
  #--sum together in case there are multiple fertilizers
  summarise(value = sum(value2))
  
#--do the calcs for leaching
ghg_leach <- 
  ghg_ind %>% 
  mutate(value2 = 
           value * kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached,
         unit = "kg n2o-n leach/stand",
         #--change desc from plant n/fert n to just indirect, leach
         desc = "indirect, leach") %>% 
  group_by(production_id, unit, desc) %>% 
  #--sum them together (plant + all fertilizers)
  summarise(value = sum(value2))
  
#--comnbine volat and leach values, change to co2e
ghg_ind1 <- 
  ghg_vol %>% 
  bind_rows(ghg_leach) %>% 
  mutate(value = value * n_to_n2o * gwp_n2o,
         unit = "kg co2e/stand") 

# add together ------------------------------------------------------------

ghg_n2o <- 
  ghg_dir1 %>% 
  bind_rows(ghg_ind1) %>% 
  mutate(cat = "n2o") |> 
  fill(assump_id)

ghg_n2o %>% 
  write_csv("R/data_tidy/ghg_n2o.csv")
