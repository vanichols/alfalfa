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
         unit = "mj/stand",
         cat = cat_ass) %>% 
  select(scenario_id, cat, desc, unit, value)

f2 %>% 
  write_csv("R/data_tidy/energy_fert.csv")
