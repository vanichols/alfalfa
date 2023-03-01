#--processing irrigation component of scenario sheet
#--created 2/15, updated 2/16

rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# data --------------------------------------------------------------------

d_raw <- read_csv("R/data_raw/lca-sheets/raw_production.csv",
                  skip = 5) %>% 
  janitor::remove_empty()


d <- fun_preproc(d_raw)


# stand life ---------------------------------------------------------------

sl <- 
  d %>% 
  filter(desc == "stand life") %>% 
  rename("stand_life_yrs" = value) %>% 
  select(production_id, stand_life_yrs)


# get harvests per standlife ----------------------------------------------

hn <- 
  d %>% 
  filter(cat == "yield") %>%
  filter(grepl("harvests", desc)) %>% 
  separate(desc, into = c("product", "xx")) %>% 
  select(-xx, -unit) %>% 
  left_join(sl) %>% 
  mutate(harv_per_standlife = value * stand_life_yrs) %>% 
  select(-value, -stand_life_yrs, -cat)

# harvest ops -----------------------------------------------------

h1 <- 
  d %>% 
  filter(cat == "harvest ops") %>% 
  separate(desc, into = c("product", "op"), remove = F) %>% 
  left_join(hn)


h2 <- 
  h1 %>% 
  mutate(unit = "pass / stand life",
         value = value * harv_per_standlife) %>% 
  select(-product, -op, -harv_per_standlife)


h2

h2 %>% 
  write_csv("R/data_tidy/prod_harvestops.csv")



