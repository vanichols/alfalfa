#--Example of running the base scenario for Tulare county
#--combining three data sheets into one for further processing
#--created for Phoo by Gina on 11/26


rm(list = ls())
library(tidyverse)

#--this just has some conversions to reference easily
source("R/code/00_funs/fxn_conversions.R")


# 1. piece together 'base' file -------------------------------------------

#--first thing you do is piece together the entire 'base' file
#--read in field ops, pesticide applications, and other
base_fops <- 
  read_csv(paste0("summary_for_phoo/tulare_fieldops.csv"), skip = 5)  %>%  
  fill(scenario_id, cat) |> 
  group_by(scenario_id, cat, desc) |> 
  summarise(value = sum(value, na.rm = T)) |> 
  mutate(unit = "pass/stand life")

base_pest <- 
  read_csv(paste0("summary_for_phoo/tulare_pests.csv"), skip = 5) |> 
  fill(scenario_id, cat) |> 
  group_by(scenario_id, cat, desc, unit) |> 
  summarise(value = sum(value, na.rm = T)) |> 
  mutate(desc = str_to_lower(desc))

base_other <- 
  read_csv(paste0("summary_for_phoo/tulare_other.csv"), skip = 5)|> 
  fill(scenario_id, .direction = c("down")) %>% 
  fill(cat, .direction = c("down")) %>% 
  fill(desc, .direction = c("down")) %>% 
  select(-notes) %>% 
  mutate_if(is.character, str_to_lower) 


# 2. combine fops, pest, other -----------------------------------------------

  s <- 
    base_fops |> 
    mutate(value = as.character(value)) |> 
    bind_rows(base_pest |> mutate(value = as.character(value))) |> 
    bind_rows(base_other |> mutate(value = as.character(value)))
  
  
  #--read in the short desc of the scenario
  s_desc <- 
    read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5) |> 
    select(scenario_id, scen_desc) |> 
    filter(!is.na(scenario_id)) |> 
    rename(notes = scen_desc)
  
  #--add it to the file, write to use later
  s |>  
    left_join(s_desc) %>% 
    write_csv("summary_for_phoo/tulare-scenario-notouch.csv")
  

