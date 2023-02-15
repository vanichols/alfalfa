#--processing pesticides components of scenario sheet
#--created 2/15

rm(list = ls())

library(tidyverse)
library(readxl)


source("R/code/00_conversions.R")


# use clean up function -------------------------------------------------------

d_raw <- read_excel("R/data_raw/lca-sheets/enterprise-flows-scenario-format.xlsx",
                    sheet = "production",
                    skip = 5)


d <- fun_preproc(d_raw)



# pesticides --------------------------------------------------------------


p1 <- 
  d %>% 
  filter(cat == "pesticide") %>% 
  group_by(scenario_id, cat, desc, unit) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()


# products to ai ----------------------------------------------------------


p1_prods <- 
  p1 %>% 
  select(desc) %>% 
  unique() %>% 
  mutate_if(is.character, str_to_lower)

p1_prods

#--assign active ingredients to each product manually

ais <- 
  p1_prods %>% 
  mutate(ai = case_when(
    desc == "roundup powermaxx" ~ "glyphosate",
    desc == "warrior ii" ~ "lambda-cyhalothrin",
    desc == "coragen" ~ "chlorantraniliprole",
    desc == "chateau" ~ "flumioxazin",
    desc == "prowl h2o" ~ "pendimethalin",
    desc == "gramoxone" ~ "paraquat",
    TRUE ~ "XXXXX"
  ))

ais



# concentration of ais ----------------------------------------------------

#chlorantraniliprole = 1.67 lb AI per gallon

#flumioxazin 51% active ing by wt (it is a granular product)

#glyphosate = 4.8 lbs AE per gallon
#           = 5.88 lbs AI per gallon

#lamba = 2.08 lbs AI per gallon

#paraquat (dichloride?) 30.1% AI, or 25.4% from syngenta? 
# 2 lbs paraquat cation per gallon, or 2.762 lbs paraquat dichloride per gallon

#pendimethalin 38.7% AI (by wt?)
# 3.8 pounds per gallon

#--just for looking at
ais_vec <- 
  ais %>% 
  pull(ai) %>% 
  unique() %>% 
  sort()

ais_amts <- 
  tribble(
  ~ai,                     ~value_ai,       ~unit_ai,
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

p2 <- 
  p1 %>% 
  left_join(ai_res) %>% 
  #--rearrange so it's easier to see
  select(scenario_id, cat, desc, value, unit, ai, value_ai, unit_ai, everything()) 


# deal with units all at once --------------------------------------------


p3 <- 
  p2 %>% 
  #--deal with applied units
  mutate(
    value_gal_ha = case_when(
      unit == "gal/ac" ~ value * ac_per_ha,
      unit == "pints/ac" ~ value * gal_per_pint * ac_per_ha,
      unit == "oz/ac" ~ value * gal_per_oz * ac_per_ha,
      unit == "fl oz/ac" ~ value * gal_per_oz * ac_per_ha,
      TRUE ~ 999)
  ) 

#--make sure every unit got converted
p3 %>% 
  select(value_gal_ha)

#--deal with ai unit

p4 <- 
  p3 %>% 
  mutate(
    value_kgai_ha = case_when(
      unit_ai == "lb AI/gal" ~ value_gal_ha * value_ai * kg_per_lb,
      unit_ai == "g/g" ~ value_gal_ha * g_per_gal_water * value_ai * (1/1000),
      TRUE ~ 999)
  ) 

#--everything got converted?
p4 %>% 
  select(value_kgai_ha)

#--clean up
p5 <- 
  p4 %>% 
  select(scenario_id, cat, ai, value_kgai_ha) %>% 
  mutate(unit = "kg / stand") %>% 
  rename(
    "desc" = ai,
    "value" = value_kgai_ha
    )

p5

p5 %>% 
  write_csv("R/data_tidy/lca_pesticides.csv")

