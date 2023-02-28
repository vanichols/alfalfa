# calculate energy use
#created 2/16

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

#--assumed fertilizer manufacturing energy, from greet tables
a_fe <- 
  read_csv("R/data_refs/ref_fert-energy.csv") %>% 
  rename(
    unit_ass = unit,
    value_ass = value)

a_fe

# how much fertilizer did we apply?

f <- read_csv("R/data_tidy/prod_fertility.csv")

f1 <- 
  f %>% 
  #--get names ready to merge
  select(-cat) %>% 
  rename(cat = desc) %>% 
  left_join(a_fe, 
            by = c("cat")) 

f2 <- 
  f1 %>% 
  mutate(value2 = value * value_ass,
         unit = "mj/stand") %>%
  unite(cat, desc, col = "desc", sep = ", ") %>% 
  mutate(cat = "fertilizer manufacture") %>% 
  select(production_id, cat, desc, unit, value2) %>% 
  rename(value = value2)

f2 %>% 
  write_csv("R/data_tidy/energy_fert.csv")

