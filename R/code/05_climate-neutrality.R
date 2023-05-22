#--see if any scenarios have a prayer of being climate neutral
#--created 5/22

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

#--scenario key
sk <-
  read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5) %>% 
  select(scenario_id, location, scen_desc) %>% 
  filter(!is.na(scenario_id))

d <- 
  d_raw %>% 
  left_join(sk) %>% 
  filter(grepl("co2", unit)) %>% 
  filter(unit == "kgco2e_hayr") %>% 
  group_by(scenario_id, scen_desc, cat, unit, location) %>% 
  summarise(value = sum(value, na.rm = T))

#--process for figure
d2 <- 
  d %>% 
  mutate(cat = case_when(
    cat == "avoided n2o" ~ "avoided fertilizer n2o",
    cat == "fertilizer avoidance" ~ "avoided fertilizer energy ghg",
    TRUE ~ cat)) %>% 
  arrange(location, scenario_id, value) %>% 
  group_by(scenario_id, location) %>% 
  mutate(cumval = cumsum(value))


# fig ---------------------------------------------------------------------

d2 %>% 
  filter(scen_desc == "base") %>%
  filter(location == "imperial") %>% 
  mutate(cat = fct_inorder(cat),
         cat = fct_rev(cat)) %>% 
  ggplot(aes(cat, cumval)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~location)

d2 %>% 
  filter(scen_desc == "base") %>%
  filter(location == "imperial") %>% 
  mutate(cat = fct_inorder(cat),
         cat = fct_rev(cat)) %>% 
  mutate(prevcumval = lag(cumval),
         prevcumval = ifelse(is.na(prevcumval), 0, prevcumval)) %>% 
  ggplot(aes(cat, cumval)) + 
  geom_segment(aes(x = cat, xend = cat, y = prevcumval, yend = cumval),
               arrow = arrow()) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~location)

