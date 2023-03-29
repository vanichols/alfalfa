#--look at results
#--created 3/16
#--reran everything 3/29 (fixed fert energy calcs)

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
  left_join(s_desc) |> 
  filter(scenario_id != "scen_0000")


# get total values --------------------------------------------------------

d_tot <- 
  d |> 
  group_by(scenario_id, scen_desc, unit) |> 
  summarise(value = sum(value))


d_tot |> 
  filter(!grepl("stand", unit)) |> 
  ggplot(aes(reorder_within(scen_desc, value, unit), value)) + 
  geom_col(aes(fill = scen_desc == "base")) + 
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~unit, scales = "free") 



# compare everything to base ----------------------------------------------

d_base <- 
  d_tot |> 
  ungroup() |> 
  filter(scenario_id == "scen_0001") |> 
  rename(base_value = value) |> 
  select(base_value, unit)

d_tot |> 
#  filter(scenario_id != "scen_0001") |> #--this is the base
  left_join(d_base) |> 
  mutate(diff_base = value - base_value) |> 
  filter(grepl("hayr", unit)) |> 
  ggplot(aes(reorder_within(scen_desc, diff_base, unit), diff_base)) + 
  geom_col(aes(fill = diff_base < 0), show.legend = F) + 
  scale_fill_manual(values = c("red", "blue")) +
  scale_x_reordered() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~unit, scales = "free")




# energy ------------------------------------------------------------------

source("R/code/00_funs/fxn_VizEnergy.R")


i_want_this_scenario_id <- "0001"
e1 <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_e1 <- e1[[1]]
f_e1 <- e1[[2]]
f2_e1 <- e1[[3]]


i_want_this_scenario_id <- "0008"
e2 <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_e2 <- e2[[1]]
f_e2 <- e2[[2]]
f2_e2 <- e2[[3]]

i_want_this_scenario_id <- "0009"
e3 <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_e3 <- e3[[1]]
f_e3 <- e3[[2]]
f2_e3 <- e3[[3]]


i_want_this_scenario_id <- "0010"
e4 <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_e4 <- e4[[1]]
f_e4 <- e4[[2]]
f2_e4 <- e4[[3]]



