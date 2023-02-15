#--calculate n2o emissions
#--use fertilizer n and plant n inputs
#--direct + indirect
#--2/15 - need to do indirect

library(tidyverse)
library(readxl)
library(measurements)


rm(list = ls())

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





# direct n2o ---------------------------------------------------------------------



n2o <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "soil_n2o", 
                   skip = 5)

n2o1 <- 
  fun_preproc(data = n2o) 

#--need to utilize fertilizer info for this calc




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



# get N from fert ---------------------------------------------------------

#--calculate kg of N applied per ha via fertilizers

fertn_ref <- read_csv("R/data_tidy/ref_fert-n.csv")

fert <- read_csv("R/data_tidy/lca_fertility.csv")

fert_n <- 
  fert %>% 
  pivot_longer(tulare_county:ncol(.), 
               names_to = "region",
               names_repair = "minimal")  %>%
  left_join(
  fertn_ref %>% 
    rename(flow_desc = fert_type)
  ) %>% 
  mutate(
    fertn_tot = value * fert_n
  ) %>% 
  group_by(name, region) %>% 
  summarise(fert_n_kg = sum(fertn_tot, na.rm = T))


# get N from plants --------------------------------------------------------

dm_harv <- 
  read_csv("R/data_tidy/lca_yields.csv") %>% 
  pivot_longer(6:ncol(.), names_to = "region") %>% 
  mutate(
    dm_per_year_kg = value/stand_life_yrs) %>%
  select(name, region, stand_life, dm_per_year_kg) 

# Use assumptions from IPCC 2019 refinement, Table 11.1A

# assume fraction of aboveground biomass not harvested 
ass_frac_not_harv <- 0.1

# assume frac N in harvested biomass (kg/kg DM)
ass_dm_frac_n <- 0.027

# assume amount of roots per unit dm harvested
ass_root_frac <- 0.4

# assume N fraction of alfalfa roots (kg/kg DM)
ass_root_frac_n <- 0.019

# note it is assumed all the n2o is released when the stand is terminated
# therefore the total is amortized over the standlife. Longer standlife = less n2o
# from IPCC 2019 refinement: the nitrogen residue from forages is only accounted for during pasture renewal


plant_n <- 
  dm_harv %>% 
  mutate(
    frac_dm_not_harv = ass_frac_not_harv,
    dm_frac_n = ass_dm_frac_n,
    root_per_dm = ass_root_frac,
    root_frac_n = ass_root_frac_n
  ) %>% 
  mutate(
    dm_n = dm_per_year_kg * frac_dm_not_harv * dm_frac_n,
    root_n = dm_per_year_kg * root_per_dm * root_frac_n,
    plant_n_kg = (dm_n + root_n)/stand_life 
  )
    
plant_n1 <- 
  plant_n %>% 
  select(name, region, plant_n_kg) %>% 
  arrange(region, name)
    
# convert to n2o ----------------------------------------------------------

# assume 0.5% of n inputs to account for direct n2o emissions (all inputs in dry climates, Table 11.1) 

n2o2 <- 
  plant_n1 %>% 
  left_join(fert_n) %>%
  pivot_longer(plant_n_kg:fert_n_kg,
               names_to = "n_source") %>% 
  mutate(n2oN_kg = 0.005 * value,
         n2o_kg = n2oN_kg * 44/28) %>% 
  select(-value, -n2oN_kg) 

n2o2 %>% 
  pivot_wider(names_from = n_source, 
              values_from = n2o_kg) %>%
  mutate(name = ifelse(name == "best", "high yields",
                       ifelse(name == "worst", "low yields", "typical yields")),
         name = fct_inorder(name)) %>% 
  #mutate(n2o_kg = plant_n_kg + fert_n_kg) %>%
  pivot_longer(plant_n_kg:fert_n_kg, names_to = "n2o_n_source") %>% #--note this is kg of n2o
  mutate(co2_eq_kg = value * 298) %>% 
  ggplot(aes(name, co2_eq_kg)) + 
  geom_col(aes(fill = n2o_n_source), color = "black") + 
  labs(y = "kg co2-eq per ha",
       x = NULL,
       title = "N2O emissions, IPCC method") +
  facet_grid(.~region)

ggsave("R/figs/lca_n2o.png", width = 8, height = 5)



#--format it to fit in template
n2o3 <- 
  n2o2 %>% 
  mutate(system = str_replace_all(region, "_", " "),
         flow_desc = ifelse(n_source == "plant_n_kg", "n2o plant n", "n2o fertilizer"),
         value = n2o_kg,
         units = "kg n2o") %>% 
  select(system, flow_desc, units, name, value)

  


n2o1 %>% 
  select(-units, -value) %>% 
  left_join(n2o3)
