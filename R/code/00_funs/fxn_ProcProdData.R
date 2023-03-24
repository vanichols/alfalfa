#--turns the processing of production data into a function
#--process all production info to be standardized format units
#--created 3/14, combining single code files from 'code' folder
#--updated 3/16 to handle new single input file

library(tidyverse)
library(readxl)


ProcProdData <- function(f_scenario_id = "0001"){
  
  source("R/code/00_funs/fxn_conversions.R")
  source("R/code/00_funs/fxn_ProcDataIn.R")  
  
# pull in the correct scenario file ------------------------------------------

     # data --------------------------------------------------------------------
  
  d <- 
    read_csv(paste0("R/data_scens-notouch/scen_", f_scenario_id, ".csv")) %>% 
    select(-change_ind) 
  
    #--stand life
  
  sl <- 
    d %>% 
    filter(desc == "stand life") %>% 
    mutate(value = as.numeric(value)) |> 
    rename("stand_life_yrs" = value) %>% 
    select(scenario_id, stand_life_yrs)
  

# references --------------------------------------------------------------

r_ais <- read_csv("R/data_refs/ref_pest-ais.csv")  
  
  
  # 1. fertility ---------------------------------------------------------------
  
  #--need to know how many assumed applications there are per stand life of each type of fertilizer
  f_passes <- 
    d %>% 
    filter(cat == "field ops") %>% 
    filter(grepl("fertilize", desc)) %>% 
    mutate(value = as.numeric(value)) |> 
    #--get type of fert pass it is
    separate(desc, into = c("x", "fert_desc"), sep = ",") |> 
    mutate(fert_desc = str_trim(fert_desc),
           pass_per_sl = value) |> 
    select(-x, -notes, -cat, -value, -unit)  
    
  #--if it is every not lb/ac, this needs to be updated
  f_amts <- 
    d |> 
    filter(cat == "fertility") |> 
    separate(desc, into = c("fert_desc", "fert_type"), sep = ",") |> 
    mutate_if(is.character, str_trim) |> 
    left_join(f_passes) |> 
    mutate(value = as.numeric(value) * pass_per_sl,
           unit = case_when(
             (unit == "lb/ac/pass") ~ "lb/ac/stand",
             TRUE ~ "XXXX"
           ))
  
  #--change to kg per ha per stand
  
  f1 <- 
    f_amts %>% 
    mutate(value = case_when(
      unit == "lb/ac/stand" ~ value * kg_per_lb * ac_per_ha,
      TRUE ~ 999),
      unit = "kg/stand") |> 
    group_by(scenario_id, cat, fert_type, unit) |> 
    summarise(value = sum(value)) |> 
    rename("desc" = fert_type)
  
  p1 <- f1
  
  
  # 2. field ops (op) -----------------------------------------------------
  
  op1 <- 
    d |> 
    filter(cat == 'field ops') |> 
    mutate(value = as.numeric(value))
  
  #--simplify fertilizer passes to just be 'fertilize'
  op2 <- 
    op1 |> 
    mutate(desc = ifelse(grepl("fertilize", desc), "fertilize", desc)) |> 
    group_by(scenario_id, cat, desc, unit) |> 
    summarise(value = sum(value, na.rm = T)) 
  
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
    select(-product, -op, -harv_per_standlife) |> 
    filter(value != 0)
  
  
  p3 <- h2
  
  
  # 4. irrigation (i) --------------------------------------------------------------
  
  #--irrigation used for establishment, only once during stand life
  i_est <- 
    d %>% 
    filter(cat == "irrigation") %>%
    filter(grepl("est", desc)) |> 
    mutate(value = as.numeric(value)) |> 
    mutate(unit = "ac-in/stand") |> 
    filter(value != 0)
  
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
    select(-stand_life_yrs) |> 
    filter(value != 0)
  
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
  
  #--make values numeric
  c1 <- 
    d %>% 
    filter(cat == "pesticide") %>% 
    mutate(value = as.numeric(value)) |> 
    ungroup()
  
  
  #--combine application w/ais
  
  c2 <- 
    c1 %>% 
    left_join(r_ais) %>% 
    #--rearrange so it's easier to see
    select(scenario_id, cat, desc, value, unit, ai, value_ai, unit_ai, everything()) 
  
  
  #--deal with units
  c3_liq <- 
    c2 %>% 
    #--just deal with liquids for now
    filter(!grepl("lb", unit)) |> 
    mutate(
      value = case_when(
        unit == "gal/ac" ~ value * ac_per_ha,
        unit == "pint/ac" ~ value * gal_per_pint * ac_per_ha,
        unit == "pt/ac" ~ value * gal_per_pint * ac_per_ha,
        unit == "oz/ac" ~ value * gal_per_oz * ac_per_ha,
        unit == "fl oz/ac" ~ value * gal_per_oz * ac_per_ha,
        TRUE ~ 999),
      unit = "gal/ha"
    ) 
  
  c3_sol <- 
    c2 %>% 
    #--just deal with liquids for now
    filter(grepl("lb", unit)) |> 
    mutate(
      value_applie = case_when(
        unit == "lbs/ac" | unit == "lb/ac" ~ value * kg_per_lb * ac_per_ha,
        TRUE ~ 999),
      unit = "kg/ha"
    ) 
  
  c3 <- 
    c3_liq |> 
    bind_rows(c3_sol)
  
  
  #--make sure every unit got converted
  if (nrow(c3 %>%
           filter(value == 999)) > 0) {
    print("check pesticide conversions")
  } else {
    "pesticide units ok"
  }
  
  #--multiply appliation by ais contained in that application
  c4 <- 
    c3 %>% 
    mutate(
      value_kgai_ha = case_when(
        (unit == "gal/ha" & (unit_ai == "lb AI/gal" | unit_ai == "lb/gal")) ~ 
          value * value_ai * kg_per_lb,
        (unit == "gal/ha" & (unit_ai == "g AI/g" | unit_ai == "g/g")) ~ 
          value * lb_per_gal_water * kg_per_lb * value_ai,
        (unit == "gal/ha" & (unit_ai == "g AI/kg" | unit_ai == "g/kg")) ~ 
          value * lb_per_gal_water * kg_per_lb * value_ai * kg_per_g,
        (unit == "kg/ha" & (unit_ai == "g AI/g" | unit_ai == "g/g")) ~ 
          value * value_ai,
        TRUE ~ 999)
    ) 
  
  #--everything got converted?
  if (nrow(c4 %>% 
           filter(value_kgai_ha == 999)) > 0 ) {print("check pesticide conversions")} else {"pest conv ok"}
  
  
  
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
    bind_rows(sl1) |> 
    filter(value != 0)
  
  
  p7 <- y4
  
  
  # combine -----------------------------------------------------------------
  
  p_all <- 
    p1 |> 
    bind_rows(p2) |>
    bind_rows(p3) |> 
    bind_rows(p4) |> 
    bind_rows(p5) |> 
    bind_rows(p6) |> 
    bind_rows(p7) |> 
    select(-notes) |> 
    mutate(value = as.character(value))
  
  
  return(p_all)
  
  
}

