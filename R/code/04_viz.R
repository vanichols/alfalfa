#--look at results
#--created 3/16

rm(list = ls())
library(tidyverse)
library(pals)
library(tidytext)
library(patchwork)


# monster file maker ------------------------------------------------------


d_raw <- 
  list.files(path = "R/data_out/", pattern = ".csv", full.names = T) |> 
  map_df(read_csv)

s_desc <- 
  read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5) |> 
  fill(scen_desc) |> 
  select(scenario_id, scen_desc) |> 
  distinct()

d <- 
  d_raw |> 
  left_join(s_desc)


d_tot <- 
  d |> 
  group_by(scenario_id, scen_desc, unit) |> 
  summarise(value = sum(value))


d_tot |> 
  filter(!grepl("stand", unit)) |> 
  ggplot(aes(reorder_within(scen_desc, value, unit), value)) + 
  geom_col() + 
  coord_flip() +
  scale_y_reordered() +
  facet_wrap(~unit, scales = "free") 


# energy ------------------------------------------------------------------

source("R/code_autofxns2/00_funs/fxn_VizEnergy.R")


i_want_this_scenario_id <- "0001"
e1 <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_e1 <- e1[[1]]
f_e1 <- e1[[2]]


i_want_this_scenario_id <- "0002"
e2 <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_e2 <- e2[[1]]
f_e2 <- e2[[2]]

i_want_this_scenario_id <- "0003"
e3 <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_e3 <- e3[[1]]
f_e3 <- e3[[2]]


f_e1 + f_e2 + f_e3
