#--prep pesticide section
#--each row must be unique in the final scenario file
#--this code condenses the entered info into that form

library(tidyverse)

# use base scenario -------------------------------------------------------

d1 <- read_csv("R/data_in/tulare/base_pests.csv", skip = 5)
d2 <- read_csv("R/data_in/siskiyou/base_pests.csv", skip = 5) |> 
  group_by(scenario_id, cat, desc, unit) |> 
  summarise(value = sum(value))

d <- 
  d1 %>% 
  bind_rows(d2)

# add ais -----------------------------------------------------------------


#--assign active ingredient to each product name
ais <- 
 d |> 
  mutate(desc = str_to_lower(desc)) |> 
  ungroup() |> 
  select(desc) |> 
  mutate(ai = case_when(
    desc == "roundup powermaxx" ~ "glyphosate",
    desc == "warrior ii" ~ "lambda-cyhalothrin",
    desc == "coragen" ~ "chlorantraniliprole",
    desc == "chateau" ~ "flumioxazin",
    desc == "prowl h2o" ~ "pendimethalin",
    desc == "gramoxone" ~ "paraquat",
    desc == "herbimax" ~ "surfactant",
    desc == "raptor" ~ "imazamox",
    desc == "tricor 75df" ~ "metribuzin",
    desc == "activator 90" ~ "surfactant",
    desc == "steward" ~ "indoxacarb",
    desc == "zinc phosphide" ~ "zinc phosphide",
    TRUE ~ "XXXXX"
  )) |> 
  distinct()



#--conecntration of ais (manual)

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
    "flumioxazin",             0.414,         "g AI/g", #--for chateau ez, the liquid formulation
    "glyphosate",              5.88,         "lb AI/gal",
    "lambda-cyhalothrin",      2.08,         "lb AI/gal",
    "paraquat",                2.762,        "lb AI/gal",
    "pendimethalin",           3.8,          "lb AI/gal",
    "imazamox",                700,            "g AI/kg",
    "metribuzin",              .75,            "g AI/g",
    "surfactant",              .9,             "g AI/g",
    "indoxacarb",              1.25,           "lb AI/gal",
    "zinc phosphide",          1,              "g AI/g"
    
  )

ai_res <- 
  ais %>% 
  left_join(ais_amts) 

ai_res

ai_res |> 
  write_csv("R/data_refs/ref_pest-ais.csv")

