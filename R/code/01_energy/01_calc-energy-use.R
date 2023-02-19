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



# f. fertilizer manu energy -------------------------------------------------------

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


# p. pesticide manu energy ---------------------------------------------------

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
  filter(grepl("fuel energy", cat_ass)) %>%
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
  mutate(cat = ifelse(grepl("hay", desc), "harvest fuel use", "fieldops fuel use")) %>% 
  group_by(scenario_id, cat, unit) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  mutate(desc = "diesel use")


# seeds -------------------------------------------------------------------


# i. irrigation --------------------------------------------------------------
#--I don't know if sprinkler vs flood should be treated differently
#--for now assume sprinkler is surface 

i <- read_csv("R/data_tidy/prod_irrigation.csv")

#--keep the stupid units for ftm eqn
i_sprink <- 
  i %>% 
  filter(grepl("sprink", desc),
         grepl("ac-in", unit))


i_flood <- 
  i %>% 
  filter(grepl("flood", desc),
         grepl("ac-in", unit))

#--assumptions
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

a_other <- 
  a %>% 
  filter(cat_ass == "irrigation",
         is.na(unit_ass),
         value_ass != "diesel") %>%  
  mutate(value_ass = as.numeric(value_ass)) %>%
  select(-cat_ass, -unit_ass) %>% 
  pivot_wider(names_from = desc, 
              values_from = value_ass) %>% 
  janitor::clean_names()


#--calculate things based on FTM eqns 
# note that ftm eqns suck wrt documentation and don't match nrcs estimates (are much lower)

i <- 
  i_flood %>%
  mutate(surface = value * a_pct %>% pull(value_ass),
         ground = value - surface) %>% 
  select(-value) %>% 
  pivot_longer(surface:ground) %>%
  #--assume sprinkler water is surface
  bind_rows(i_sprink %>% 
              mutate(name = "surface")) %>% 
  mutate(pump_press_m = a_pumppres %>% pull(value_ass),
         welldepth_m = ifelse(name == "ground", 
                              a_welldepth %>% pull(value_ass),
                              0),
         head_m = pump_press_m + welldepth_m) %>% 
  left_join(a_other)


#--see the 'compare-ftm-nrcs' qmd, ftm estimates are soooo much lower (?)
i1 <- 
  i %>% 
  mutate( numer = 
            head_m *
            pump_conversion *
            value *
            mm_per_in * 
            ha_per_ac,
          denom = 
           pump_efficiency * 
           irrigation_efficiency * 
           gearhead_efficiency * 
           power_unit_efficiency
          ) %>% 
  mutate(value = numer/denom,
         unit = "MJ/stand") %>% 
  group_by(scenario_id, cat, unit, name) %>% 
  summarise(value = sum(value)) %>% 
  rename(desc = name)




# use all energy to calculate seed ----------------------------------------------

s <- read_csv("R/data_tidy/prod_seeds.csv")

a_seed <- 
  a %>%
  filter(cat_ass == "seed manufacture",
         desc == "seed yield") %>%
  mutate(
    seed_yld_kg_ha = as.numeric(value_ass) * kg_per_lb * ac_per_ha
  ) %>% 
  select(scenario_id, seed_yld_kg_ha)
  

s2 <-
  #--sum all of the energy calculated above
  f2 %>% 
  bind_rows(p2) %>% 
  bind_rows(o2) %>% 
  bind_rows(i1) %>% 
  mutate(unit = str_to_lower(unit), 
         unit = str_remove_all(unit, " ")) %>% 
  group_by(scenario_id, unit) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  #--assume this is energy used to create the given seed yield
  left_join(a_seed) %>% 
  #--get mj/kg seed produced
  mutate(mj_kgseed = value/seed_yld_kg_ha) %>%
  select(scenario_id, mj_kgseed) %>% 
  #--multiply by amount of seed used to plant
  left_join(s) %>% 
  mutate(value = value * mj_kgseed,
         unit = "mj/stand") %>% 
  group_by(scenario_id, cat, desc, unit) %>% 
  summarise(value = sum(value))

s2

# add all together --------------------------------------------------------

all <- 
  f2 %>% 
  bind_rows(p2) %>% 
  bind_rows(o2) %>% 
  bind_rows(i1) %>% 
  bind_rows(s2) %>% 
  mutate(unit = str_to_lower(unit), 
         unit = str_remove_all(unit, " "))

all %>% 
  group_by(scenario_id, cat, unit) %>% 
  summarise(value = sum(value)) %>% 
  arrange(-value)
