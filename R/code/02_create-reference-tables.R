# try to create reference tables (still unsure of good format)
#created 2/7/2023

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")


# amount of N in fertilizer (for N2O emissions) ---------------------------

#--get unique types of fertilizers

fert_types <- 
  read_csv("R/data_tidy/lca_fertility.csv") %>% 
  select(flow_desc) %>% 
  pull() %>% 
  unique()

# MAP is 11% by weight N
# https://www.etoolsage.com/calculator/NPK_Fertilizer_Calculator.asp

# assumed poultry litter is 3% N (for now)
# https://frankiafertilizers.com/product/composted-chicken-manure/

fert_n <- 
  tibble(fert_type = fert_types) %>% 
  mutate(fert_units = c("kg", "kg"),
         fert_n = case_when(
           fert_type == "11-52-0-MAP" ~ 0.11,
           fert_type == "composted poultry litter" ~ 0.03,
           TRUE ~ 999
         ),
         fert_n_units = "kg"
         )

fert_n %>% 
  write_csv("R/data_tidy/ref_fert-n.csv")


# fuel use for a field op -------------------------------------------------

fuel <- 
  read_excel("ftm_raw/operation-3.9-weps.xlsx") %>% 
  janitor::clean_names() %>% 
  #--note oenergyarea is L diesel/ha
  select(id, op_group1, name, oenergyarea) %>% 
  janitor::remove_empty() %>% 
  mutate(diesel_Lha = as.numeric(oenergyarea),
         diesel_galac = diesel_Lha / 3.7 / 2.47) %>% 
  separate(op_group1, into = c("cat", "subcat", "subsubcat"), sep = ",") %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate_if(is.character, str_trim) %>% 
  filter(diesel_Lha > 0.01) %>% 
  select(-oenergyarea)

#--what are general cats of operations?

read_csv("R/data_tidy/lca_fieldops.csv") %>% 
  select(flow_desc) %>% 
  distinct()

#--find a good value for each

# chisel ------------------------------------------------------------------



#-lots of spread
fuel %>% 
  filter(grepl("chisel", subsubcat)) %>% 
  ggplot(aes(subsubcat, diesel_Lha)) + 
  geom_jitter()

#--move to only include ones w/chisel in the name
fuel %>% 
  filter(grepl("chisel", subsubcat)) %>% 
  filter(grepl("chisel", name)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()



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

