#--prep pesticide section
#--each row must be unique in the final scenario file
#--this code condenses the entered info into that form

library(tidyverse)


# read in manually created field ops info for each scenario ---------------


d <- read_csv("R/data_in/scenbyhand-pests.csv", skip = 5)



# summarise ---------------------------------------------------------------

d1 <- 
  d |> 
  fill(scenario_id, cat) |> 
  group_by(scenario_id, cat, desc, unit) |> 
  summarise(value = sum(value)) |> 
  mutate(desc = str_to_lower(desc))


d1


d1 |> 
  write_csv("R/data_in/scen_pests.csv")


# add ais -----------------------------------------------------------------


#--assign active ingredient to each product name
ais <- 
  d1 %>% 
  ungroup() |> 
  select(desc) |> 
  mutate(ai = case_when(
    desc == "roundup powermaxx" ~ "glyphosate",
    desc == "warrior ii" ~ "lambda-cyhalothrin",
    desc == "coragen" ~ "chlorantraniliprole",
    desc == "chateau" ~ "flumioxazin",
    desc == "prowl h2o" ~ "pendimethalin",
    desc == "gramoxone" ~ "paraquat",
    TRUE ~ "XXXXX"
  ))



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
    "flumioxazin",             0.414,         "g/g", #--for chateau ez, the liquid formulation
    "glyphosate",              5.88,         "lb AI/gal",
    "lambda-cyhalothrin",      2.08,         "lb AI/gal",
    "paraquat",                2.762,        "lb AI/gal",
    "pendimethalin",           3.8,          "lb AI/gal"
  )

ai_res <- 
  ais %>% 
  left_join(ais_amts) 


ai_res |> 
  write_csv("R/data_refs/ref_pest-ais.csv")

