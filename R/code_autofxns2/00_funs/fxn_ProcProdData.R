#--turns the processing of production data into a function
#--process all production info to be standardized format units
#--created 3/14, combining single code files from 'code' folder
#--updated 3/16 to handle new single input file

library(tidyverse)
library(readxl)


ProcProdData <- function(f_scenario_id = "0001"){
  
  
  source("R/code_autofxns2/00_funs/fxn_conversions.R")
  source("R/code_autofxns2/00_funs/fxn_ProcDataIn.R")  
  
# pull in the correct scenario file ------------------------------------------

     # data --------------------------------------------------------------------
  
  d <- 
    read_csv(paste0("R/code_autofxns2/datain/scen_", f_scenario_id, ".csv"), skip = 5) %>% 
    janitor::remove_empty() |> 
    ProcDataIn()
  
    #--stand life
  
  sl <- 
    d %>% 
    filter(desc == "stand life") %>% 
    mutate(value = as.numeric(value)) |> 
    rename("stand_life_yrs" = value) %>% 
    select(scenario_id, stand_life_yrs)
  
  
  # 1. fertility ---------------------------------------------------------------
  
  #--need to know how many assumed applications there are per stand life
  f_passes <- 
    d %>% 
    filter(cat == "field ops") %>% 
    filter(grepl("fertilize", desc)) %>% 
    mutate(value = as.numeric(value)) |> 
    group_by(scenario_id, cat, desc) %>% 
    summarise(value = sum(value))
  
  
  #--separate passes by fertilizer type
  f_pass_map <- 
    f_passes %>% 
    filter(grepl("map", desc)) %>% 
    pull(value)
  
  #--now get amount per pass
  f1 <- 
    d %>% 
    filter(cat == "fertility") |> 
    mutate(value = as.numeric(value))
    
  
  #--do map 11-52-0 (need to amend when adding different fertilizers)
  
  f1_map <- 
    f1 %>% 
    filter(grepl("map", desc)) 
  
  #--get total kg applied per stand, per ha
  f2_map <- 
    f1_map %>% 
    mutate(
      value = value * f_pass_map * kg_per_lb * ac_per_ha,
      unit = "kg/stand"
    )
  
  #f2_map
  
  
  #--combine all ferts
  
  p1 <- f2_map 
  
  
  # 2. field ops (op) -----------------------------------------------------
  
  op1 <- 
    d |> 
    filter(cat == 'field ops') |> 
    mutate(value = as.numeric(value))
  
  op2 <- 
    op1 |> 
    group_by(scenario_id, cat, desc, unit) |> 
    summarise(value = sum(value, na.rm = T)) |> 
    mutate(unit = "pass/stand")
  
  p2 <- op2
  
  
  # 3. harvest ops (h) -----------------------------------------------------
  
  #--number of harvests for hay and silage
  hn <- 
    d %>% 
    filter(cat == "yield") %>%
    filter(grepl("harvests", desc)) %>% 
    mutate(value = as.numeric(value)) |> 
    separate(desc, into = c("product", "xx")) %>% 
    select(-xx, -unit) %>% 
    left_join(sl) %>% 
    mutate(harv_per_standlife = value * stand_life_yrs) %>% 
    select(-value, -stand_life_yrs, -cat)
  
  
  #--number of passes per harvest
  h1 <- 
    d %>% 
    filter(cat == "harvest ops") %>% 
    mutate(value = as.numeric(value)) |> 
    separate(desc, into = c("product", "op"), remove = F) %>% 
    left_join(hn)
  
  
  h2 <- 
    h1 %>% 
    mutate(unit = "pass/stand",
           value = value * harv_per_standlife) %>% 
    select(-product, -op, -harv_per_standlife)
  
  
  p3 <- h2
  
  
  # 4. irrigation (i) --------------------------------------------------------------
  
  #--irrigation used for establishment, only once during stand life
  i_est <- 
    d %>% 
    filter(cat == "irrigation") %>%
    filter(grepl("est", desc)) |> 
    mutate(value = as.numeric(value))
  
  i_est1 <- 
    i_est %>% 
    #-change from ac-in to ha-m...or liters?
    mutate(value_ha_m = value * ha_per_ac * m_per_in,
           value_l = value_ha_m * m2_per_ha * l_water_per_m3) %>% 
    mutate(unit = "l/stand", 
           value = value_l) %>%
    select(scenario_id, cat, desc, unit, value)
  
  
  
  #--irrigation used for production
  i_prod <- 
    d %>% 
    filter(cat == "irrigation") %>%
    filter(grepl("prod", desc)) %>% 
    mutate(value = as.numeric(value)) |> 
    left_join(sl) %>% 
    mutate(value = value * stand_life_yrs,
           unit = "ac-in/stand") %>% 
    select(-stand_life_yrs)
  
  i_prod1 <- 
    i_prod %>% 
    #-change from ac-in to ha-m...or liters?
    mutate(value_ha_m = value * ha_per_ac * m_per_in,
           value_l = value_ha_m * m2_per_ha * l_water_per_m3) %>% 
    mutate(unit = "l/stand", 
           value = value_l) %>%
    select(scenario_id, cat, desc, unit, value)
  
  
  i2 <- 
    i_est1 %>% 
    bind_rows(i_prod1) %>%
    bind_rows(i_est) %>% 
    bind_rows(i_prod)
  
  p4 <- i2
  
  
  # 5. pesticides (c) --------------------------------------------------------------
  
  
  c1 <- 
    d %>% 
    filter(cat == "pesticide") %>% 
    mutate(value = as.numeric(value)) |> 
    group_by(scenario_id, cat, desc, unit) %>% 
    summarise(value = sum(value)) %>% 
    ungroup()
  
  
  #--change products to active ingredients
  
  c1_prods <- 
    c1 %>% 
    select(desc) %>% 
    unique() %>% 
    mutate_if(is.character, str_to_lower)
  
  c1_prods
  
  #--assign active ingredients to each product manually
  
  ais <- 
    c1_prods %>% 
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
  
  
  #--combine with pesticide use
  
  c2 <- 
    c1 %>% 
    left_join(ai_res) %>% 
    #--rearrange so it's easier to see
    select(scenario_id, cat, desc, value, unit, ai, value_ai, unit_ai, everything()) 
  
  
  #--deal with units
  
  c3 <- 
    c2 %>% 
    #--deal with applied units
    mutate(
      value_gal_ha = case_when(
        unit == "gal/ac" ~ value * ac_per_ha,
        unit == "pint/ac" ~ value * gal_per_pint * ac_per_ha,
        unit == "oz/ac" ~ value * gal_per_oz * ac_per_ha,
        unit == "fl oz/ac" ~ value * gal_per_oz * ac_per_ha,
        TRUE ~ 999)
    ) 
  
  #--make sure every unit got converted
  if (nrow(c3 %>% 
    select(value_gal_ha) |> 
    filter(is.na(value_gal_ha))) > 0 ) {print("check pesticide conversions")}
  
  #--deal with ai unit
  
  c4 <- 
    c3 %>% 
    mutate(
      value_kgai_ha = case_when(
        unit_ai == "lb AI/gal" ~ value_gal_ha * value_ai * kg_per_lb,
        unit_ai == "g/g" ~ value_gal_ha * g_per_gal_water * value_ai * (1/1000),
        TRUE ~ 999)
    ) 
  
  #--everything got converted?
  if (nrow(c4 %>% 
           select(value_kgai_ha) |> 
           filter(is.na(value_kgai_ha))) > 0 ) {print("check pesticide conversions")}
  
  
  
  #--clean up
  c5 <- 
    c4 %>% 
    select(scenario_id, cat, ai, value_kgai_ha) %>% 
    mutate(unit = "kg/stand") %>% 
    rename(
      "desc" = ai,
      "value" = value_kgai_ha
    )
  
  p5 <- c5
  
  
  
  # 6. seed (s) --------------------------------------------------------------------
  
  s1 <- 
    d %>% 
    filter(cat == "seed") |> 
    mutate(value = as.numeric(value))
  
  
  s2 <- 
    s1 %>%
    select(-unit) %>% 
    #--change to be on a hectare basis
    mutate(
      value_lbs_per_ac = value,
      value_kg_per_ha = value_lbs_per_ac * kg_per_lb * ac_per_ha,
      unit = "kg/stand"
    ) %>% 
    select(scenario_id, cat, desc, unit, value_kg_per_ha) %>% 
    rename(value = value_kg_per_ha)
  
  p6 <- s2
  
  
  # 7. yields (y) -----------------------------------------------------------
  
  
  #--get tons dry matter per acre per year
  y1 <- 
    d %>%
    filter(cat == "yield") %>% 
    filter(grepl("production", desc)) %>% 
    mutate(value = as.numeric(value)) |> 
    #-convert units
    mutate(value_dry_ton_per_ac_per_year = case_when(
      unit == "ton/ac/yr at 10% moisture" ~ (value * (1 - 0.1)),
      unit == "ton/ac/yr at 30% moisture" ~ (value * (1 - 0.3)),
      TRUE ~ 0)
    ) %>% 
    #--get kg dry matter per ha per year
    mutate(value_kg_per_ha_per_year = 
             value_dry_ton_per_ac_per_year * 
             lbs_per_ton * 
             kg_per_lb * 
             ac_per_ha)
  
  #--include stand life length
  y2 <- 
    y1 %>% 
    select(-value, -value_dry_ton_per_ac_per_year) %>% 
    #--get stand life 
    left_join(sl) %>% 
    mutate(value_kg_per_ha_per_standlife = 
             value_kg_per_ha_per_year * stand_life_yrs)
  
  
  y3 <- 
    y2 %>%
    select(-value_kg_per_ha_per_year, -stand_life_yrs) %>% 
    mutate(unit = "kg/stand") %>% 
    rename(value = value_kg_per_ha_per_standlife) 
  
  #--add standlife info
  sl1 <- 
    sl |> 
    mutate(cat = "yield", 
           desc = "stand life",
           unit = "years",
           value = stand_life_yrs) |> 
    select(-stand_life_yrs)
  
  y4 <- 
    y3 |> 
    bind_rows(sl1)
  
  
  p7 <- y4
  
  
  # combine -----------------------------------------------------------------
  
  p_all <- 
    p1 |> 
    bind_rows(p2) |>
    bind_rows(p3) |> 
    bind_rows(p4) |> 
    bind_rows(p5) |> 
    bind_rows(p6) |> 
    bind_rows(p7) 
  
  
  return(p_all)
  
  
}

