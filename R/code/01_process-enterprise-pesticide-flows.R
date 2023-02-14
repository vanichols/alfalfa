#--process pesticide inputs into weight of active ingredients applied
#--feb 14 2023


rm(list = ls())
library(tidyverse)
library(readxl)
library(measurements)


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




# pesticides --------------------------------------------------------------


pests <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "pesticide", 
                   skip = 5)

pests1 <- 
  fun_preproc(data = pests) %>% 
  fill(value) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  group_by(system, flow_type, flow_cat, flow_desc, units, name) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()


# products to ai ----------------------------------------------------------


prods <- 
  pests1 %>% 
  select(flow_desc) %>% 
  unique() %>% 
  mutate_if(is.character, str_to_lower)

prods

#--active ingred

ais <- 
  prods %>% 
  mutate(ai = case_when(
    flow_desc == "roundup powermaxx" ~ "glyphosate",
    flow_desc == "warrior ii" ~ "lambda-cyhalothrin",
    flow_desc == "coragen" ~ "chlorantraniliprole",
    flow_desc == "chateau" ~ "flumioxazin",
    flow_desc == "prowl h2o" ~ "pendimethalin",
    flow_desc == "gramoxone" ~ "paraquat",
    TRUE ~ "XXXXX"
  ))



#chlorantraniliprole = 1.67 lb AI per gallon

#flumioxazin 51% active ing by wt (it is a granular product)

#glyphosate = 4.8 lbs AE per gallon
#           = 5.88 lbs AI per gallon

#lamba = 2.08 lbs AI per gallon

#paraquat (dichloride?) 30.1% AI, or 25.4% from syngenta? 
# 2 lbs paraquat cation per gallon, or 2.762 lbs paraquat dichloride per gallon

#pendimethalin 38.7% AI (by wt?)
# 3.8 pounds per gallon


ais_vec <- 
  ais %>% 
  pull(ai) %>% 
  unique() %>% 
  sort()

ais_amts <- 
  tribble(
  ~ai,                     ~value_ai,       ~units_ai,
  "chlorantraniliprole",     1.67,         "lb AI/gal",
  "flumioxazin",             0.414,         "g/g", #--for chateau ez, the liquid formulation
  "glyphosate",              5.88,         "lb AI/gal",
  "lambda-cyhalothrin",      2.08,         "lb AI/gal",
  "paraquat",                2.762,        "lb AI/gal",
  "pendimethalin",           3.8,          "lb AI/gal"
)

ai_res <- 
  ais %>% 
  left_join(ais_amts) 
  

# combine with pesticides -------------------------------------------------

pests2 <- 
  pests1 %>% 
  left_join(ai_res) %>% 
  #--rearrange so it's easier to see
  select(system, flow_type, flow_cat, flow_desc, name, value, units, ai, value_ai, units_ai, everything()) 



# deal with units all together --------------------------------------------


pests3 <- 
  pests2 %>% 
  #--deal with applied units
  mutate(
    value_gal_ha = case_when(
      units == "gal/ac" ~ value * ac_per_ha,
      units == "pints/ac" ~ value * gal_per_pint * ac_per_ha,
      units == "oz/ac" ~ value * gal_per_oz * ac_per_ha,
      units == "fl oz/ac" ~ value * gal_per_oz * ac_per_ha,
      TRUE ~ 999)
  ) %>% 
  #--deal with ai units
  mutate(
    value_kgai_ha = case_when(
      units_ai == "lb AI/gal" ~ value_gal_ha * value_ai * kg_per_lb,
      units_ai == "g/g" ~ value_gal_ha * g_per_gal_water * value_ai * (1/1000),
      TRUE ~ 999)
  ) %>% 
  select(system, flow_type, flow_cat, ai, name, value_kgai_ha) %>% 
  mutate(units = "kg") %>% 
  rename("value" = value_kgai_ha)


# write? ------------------------------------------------------------------


pests3 %>% 
  write_csv("R/data_tidy/lca_pesticides.csv")

