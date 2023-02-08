# try to create reference tables for energy use of things
#created 2/7/2023

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# fertilizer --------------------------------------------------------------

fert_raw <- read_csv("R/data_tidy/lca_fertility.csv")


fert_types <- 
  fert_raw %>% 
  select(flow_desc) %>% 
  unique()

# the greet table

greet_raw <- read_excel("R/data_raw/greet_ag-chemicals.xlsx", skip = 6)


greet_nums <- 
  greet_raw %>% 
  slice(12) %>% 
  select(2:5) 

names(greet_nums) <- c("nitrogen",
                       "p2o5",
                       "k2o",
                       "caco3")

greet1 <- 
  greet_nums %>% 
  pivot_longer(1:ncol(.)) %>% 
  rename("energy_used_btu_per_gram" = value,
         "nutrient" = name) %>% 
  mutate(energy_used_btu_per_kg = parse_number(energy_used_btu_per_gram)*1000) %>% 
  select(-2)

greet1

# MAP contains 11% nitrogen, 52% phosphorous. How to convert?!?
# FTM comes up with 6521 btu/lb of product

ftm_btus_per_kg_product <- 6521 * lb_per_kg

# 1 kg of 

# map is 11-52-0, meaning it is 11% nitrogen and 52% phosphate
# so 1 kg of map has .11 kg N, .52 kg p2o5
# https://fertilizerbrokerage.com/map-11-52-0.html


greet1 %>% 
  mutate(MAP_1kg = c(
    0.11, # nitrogen
    0.52,
    0,
    0)) %>% 
  mutate(energy_used_btu = energy_used_btu_per_kg * MAP_1kg) %>% 
  summarise(energy_used_btu_per_kg_product = sum(energy_used_btu)) %>% 
  mutate(ftm = ftm_btus_per_kg_product)

