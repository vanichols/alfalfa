#--make different panels for each healthy soils scenario
#--created 4/5

rm(list = ls())
library(tidyverse)
library(tidytext)
library(patchwork)
library(readxl)
library(scales)
library(ggarchery)
library(ggbreak)



# read in monster data file -----------------------------------------------

d_raw <- read_csv("R/data_tidy/scen_all.csv")

scen_key <- read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5)


#--GJ per stand is not useful
d_ener <- 
  read_csv("R/data_tidy/scen_all-tot.csv") |> 
  filter(name == "energy") %>% 
  filter(unit != "GJ_stand")



# ranges ------------------------------------------------------------------

scen_key %>% 
  select(scenario_id, scen_desc) %>% 
  distinct()

#--group scenarios by a theme 
d_cats <- 
  d_ener %>% 
  mutate(cat = case_when(
    (scenario_id %in% c("scen_0002", "scen_0003",
                        "scen_1002", "scen_1003")) ~ "water source (ground vs surface)",
    (scenario_id %in% c("scen_0008", "scen_1008")) ~ "irr energy source (diesel vs electric)",
    (scenario_id %in% c("scen_0006", "scen_0014",
                        "scen_1006", "scen_1014")) ~ "water delivery (gravity, pump @25-50 psi, )",
    TRUE ~ "other"
  )) 

d_basevals <- 
  d_cats %>%
  filter(scen_desc == "base") %>% 
  select(location, unit, value)

d_base_for_all_cats <- 
  d_cats %>% 
  select(cat) %>% 
  expand_grid(., d_basevals) %>%
  distinct()

d_ranges <- 
  d_cats %>%
  bind_rows(d_base_for_all_cats) %>% 
  group_by(location, cat, unit) %>% 
  summarise(max_val = max(value, na.rm = T),
         min_val = min(value, na.rm = T))

d_ranges %>%
filter(cat != "other") %>% 
  ggplot() + 
  geom_linerange(aes(x = cat, ymin = min_val, ymax = max_val, color = location, group = location), 
                 linewidth = 2, 
                 position = position_dodge2(width = 0.2)) + 
  coord_flip() +
  facet_wrap(~unit, scales = "free", ncol = 1)

