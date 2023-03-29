# write how much n is in each fertilizer type (for n2o emsisions) and energy to manufacture each fertilizer
#created 2/7/2023
#--modified 2/16
#--2/23 added uan-32
#--3/8 added raw nutrient energies from greet
#--3/17 fixing file structure
#--adding things from siskiyou, changed reference data, need to check w/tulare

rm(list = ls())
library(tidyverse)
library(readxl)
source("R/code/00_funs/fxn_conversions.R")



# amount of N in fertilizer (for N2O emissions) ---------------------------

#--get unique types of fertilizers in enterprise budgets

fert_types <- 
  read_excel("R/data_refs/refbyhand_fert-types.xlsx", skip = 5) %>% 
  pull(fert_type) 


# MAP is 11% by weight N
# https://www.etoolsage.com/calculator/NPK_Fertilizer_Calculator.asp

# assumed poultry litter is 3% N (for now)
# https://frankiafertilizers.com/product/composted-chicken-manure/

# uan-32 is 32% nitrogen (25% nitrate, 25% ammonium nitrogen, 50% urea)

#--good resource for these numbers:
# https://www.sciencedirect.com/science/article/pii/S0921344905000819?casa_token=TJjU5CHfvlgAAAAA:ht-E3CMC1ZvNxMsi_KdMph0QRvqnfnTTEfLppbulRdbCUfLYqprh5mm8yQiQtdo_Vx43ZtMiNWY


fert_n <- 
  tibble(fert_type = fert_types) %>% 
  mutate(value = case_when(
           fert_type == "11-52-0 map" ~ 0.11,
           fert_type == "uan-32" ~ 0.32,
           #fert_type == "composted poultry litter" ~ 0.03,
           TRUE ~ 0 #--applies to sulfur, potassium, sodium molybdate
         ),
         unit = "kg n/kg fertilizer"
         )

fert_n

fert_n %>% 
  write_csv("R/data_refs/ref_fert-n-contents.csv")



# fertilizer energy --------------------------------------------------------------


# n, p, k -----------------------------------------------------------------

greet_raw1 <- read_excel("R/data_raw/greet_ag-chemicals.xlsx", sheet = "fert-4chems", skip = 6)

#--values for ag chemicals
greet_nums1 <- 
  greet_raw1 %>% 
  slice(12) %>% 
  select(2:5) 

names(greet_nums1) <- c("nitrogen",
                       "p2o5",
                       "k2o",
                       "caco3")

greet1 <- 
  greet_nums1 %>% 
  pivot_longer(1:ncol(.)) %>% 
  rename("energy_used_btu_gram" = value) %>% 
  mutate(energy_used_btu_g = parse_number(energy_used_btu_gram),
         energy_used_mj_kg = energy_used_btu_g * g_per_kg * mj_per_btu) %>% 
  select(-2)

greet1

#--note this is close to the value assumed by Matt Ryan's study of 39 MJ/kg for ammonia N?

# products ----------------------------------------------------------------

# maybe I should use these instead?

greet_raw2 <- read_excel("R/data_raw/greet_ag-chemicals.xlsx", sheet = "fert-prods", skip = 5)

#--I THINK this is per ton of product. But could be per ton of nutrient in product. Ugh. 
#--values for ag products
greet_nums2 <- 
  greet_raw2 %>% 
  janitor::clean_names() |> 
  mutate_if(is.character, str_to_lower) |> 
  filter(energy_use_mm_btu_ton == "total energy") %>%
  pivot_longer(2:ncol(.)) |> 
  mutate(energy_used_btu_g = (value * 1000000 * lb_per_kg)/ (2000 * 1000),
         energy_used_mj_kg = energy_used_btu_g * mj_per_btu * g_per_kg) |> 
  select(-energy_use_mm_btu_ton, -value)
  
greet_nums2

greet3 <- 
  greet1 |> 
  bind_rows(greet_nums2)



#--write it so I can look at it easily (the actual greet workbooks are a beast)
greet3 %>% 
  write_csv("greet_raw/tidy_greet-fertilizer.csv")


# should I just use these values? put them into the right format?

#--change some names to match the more common names

greet4 <- 
  greet3 |>
  rename("desc" = name,
         "value" = energy_used_mj_kg) |> 
  mutate(unit = "mj/kg prod",
         cat = "fertility",
         desc = str_replace_all(desc, "_", " "),
         desc = case_when(
           desc == "monoammonium phosphate nh4h2po4" ~ "11-52-0 map",
           desc == "urea ammonium nitrate solution" ~ "uan-32",
           TRUE ~ desc
         )) |> 
  select(-energy_used_btu_g)


#--for the 11-52-0 map, separate out the components (Dan wants to see that)

greet_map <- 
  greet4 |> 
  filter(desc == "11-52-0 map") |> 
  mutate(n_comp = 0.11 * 58, #--value for nitrogen
         p_comp = 0.52 * 30.1 #--value for p2o5
         ) |> 
  select(-value) |> 
  pivot_longer(n_comp:p_comp) |> 
  rename("component" = name)
  

greet5 <- 
  greet4 |> 
  filter(desc != "11-52-0 map") |> 
  bind_rows(greet_map)


greet5 %>% 
  write_csv("R/data_refs/ref_fert-energy.csv")



# THIS MIGHT BE WRONG, NOT CURRENTLY USING -----------------------------------------------------


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


#--pull out the unique nutrients
f_nuts <- greet1 %>% pull(name) %>% unique()

#--create data frame for each fertilizer type

#--11-52-0 map
f_map <-
  tibble(name = f_nuts) %>%
  mutate(fert_type = "11-52-0 map",
         kg_per_kgprod = c(
           0.11, #--nitrogen
           0.52, #--p2o5
           0, #--k2o
           0 #--caco3
           )
         )
              
#--uan-32             
f_uan32 <-
  tibble(name = f_nuts) %>%
  mutate(fert_type = "uan-32",
         kg_per_kgprod = c(
           0.32, #--nitrogen
           0, #--p2o5
           0, #--k2o
           0 #--caco3
         )
  )

#--calculate individually, see how it compares
#greet_e1 <-
  f_map %>% 
  bind_rows(f_uan32) %>% 
  left_join(greet1) %>% 
    filter(kg_per_kgprod > 0) |> 
    #--energy to manuf each component of 1 kg of product
    #--is is 1 kg of product?
  mutate(e_btu_kg_name = energy_used_btu_g * g_per_kg * (1 / kg_per_kgprod), 
    #--change to mj per kg
         e_mj_kg_name = e_btu_kg_name * mj_per_btu,
    #--make cats consistent with others
    cat = fert_type,
   desc = name, 
    unit = "mj/kg prod", 
    value = e_mj_kg_name) %>% 
  select(cat, desc, unit, value)




# combine with greet raw nutrient values ----------------------------------

greet3 <- 
  greet1 |> 
  mutate(cat = "raw nutrient",
         desc = nutrient,
         value = energy_used_mj_per_kg,
         unit = "mj/kg nutrient") |> 
  select(cat, desc, value, unit) |> 
  bind_rows(greet2)

#--k2o in greet. Hmm. It is 'potassium oxide'
#--potash = potassium chloride
#--0-0-60 potash is 60% potassium

#--rename k2o as potassium?
# greet4 <- 
#   greet3 |> 
#   mutate(desc = ifelse(desc == "k2o", "potassium", desc))
  
#--i don't know what to do for sulfur or sodium molybdate
#--use greet value for sulfuric acid (Ag_inputs tab, F101); 0.518 mmbtu/ton

sulfur_value <- 
  0.518 * # mmbtu/ton
  (1/lbs_per_ton) *
  (1/1000000) *
  mj_per_btu
  

greet4 |>
  add_row(cat = "raw nutrient", 
          desc = "sulfur",
          value)

