# try to create reference tables (still unsure of good format)
#created 2/7/2023
#--modified 2/16

rm(list = ls())
library(tidyverse)
library(readxl)
source("R/code/00_conversions.R")

# amount of N in fertilizer (for N2O emissions) ---------------------------

#--get unique types of fertilizers

fert_types <- 
  read_csv("R/data_tidy/prod_fertility.csv") %>% 
  select(desc) %>% 
  pull() %>% 
  unique()

# MAP is 11% by weight N
# https://www.etoolsage.com/calculator/NPK_Fertilizer_Calculator.asp

# assumed poultry litter is 3% N (for now)
# https://frankiafertilizers.com/product/composted-chicken-manure/

fert_n <- 
  tibble(fert_type = fert_types) %>% 
  mutate(value = case_when(
           fert_type == "11-52-0 map" ~ 0.11,
           #fert_type == "composted poultry litter" ~ 0.03,
           TRUE ~ 999
         ),
         unit = "kg n / kg fertilizer"
         )

fert_n %>% 
  write_csv("R/data_refs/ref_fert-n.csv")



# fertilizer energy --------------------------------------------------------------

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

greet1 %>% 
  write_csv("greet_raw/tidy_greet-fertilizer.csv")

# MAP contains 11% nitrogen, 52% phosphorous. How to convert?!?
# FTM comes up with 6521 btu/lb of product...

ftm_btus_per_kg_product <- 6521 * lb_per_kg


# 1 kg of 

# map is 11-52-0, meaning it is 11% nitrogen and 52% phosphate
# so 1 kg of map has .11 kg N, .52 kg p2o5
# https://fertilizerbrokerage.com/map-11-52-0.html

#--I have no idea what FTM is doing. 
greet1 %>% 
  mutate(MAP_1kg = c(
    0.11, # nitrogen
    0.52,
    0,
    0)) %>% 
  mutate(energy_used_btu = energy_used_btu_per_kg * MAP_1kg) %>% 
  summarise(energy_used_btu_per_kg_product = sum(energy_used_btu)) %>% 
  mutate(ftm = ftm_btus_per_kg_product)


#######-- this needs to be fixed to handle other fertilizer types --###########

#--pull out the unique nutrients
f_nuts <- greet1 %>% pull(nutrient) %>% unique()

#--create data frame for each fertilizer type
f_map <-
  tibble(nutrient = f_nuts) %>%
  mutate(fert_type = "map",
         kg_per_kgprod = c(
           0.11, #--nitrogen
           0.52, #--p2o5
           0, #--k2o
           0 #--caco3
           )
         )
                           

greet2 <- 
  greet1 %>% 
  left_join(f_map) %>% 
    #--energy to manuf each component of 1 kg of product
  mutate(e_btu_per_kg_product = energy_used_btu_per_kg * kg_per_kgprod, 
    #--change to mj per kg
         e_mj_per_kg_product = e_btu_per_kg_product * mj_per_btu,
    #--make cats consistent with others
    cat = "11-52-0 map", 
    desc = nutrient, 
    unit = "MJ/kg prod", 
    value = e_mj_per_kg_product) %>% 
  select(cat, desc, unit, value) %>% 
  filter(value > 0)

greet2

greet2 %>% 
  write_csv("R/data_refs/ref_fert-energy.csv")
