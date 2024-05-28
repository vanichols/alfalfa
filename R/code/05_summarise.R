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



# unique categories -------------------------------------------------------

#--what are the categories?
unique(d_raw$cat)

cats <- 
  d_raw %>% 
  select(cat, desc) %>% 
  distinct() %>% 
  arrange(cat, desc)

cats %>% 
  write_csv("R/data_tidy/categories.csv")


# range in category values ------------------------------------------------

d_raw %>% 
  left_join(scen_key) %>% 
  filter(unit == "kgco2e_hayr") %>% 
    #--not 16 and 17, the carbon credit changes
  filter(!grepl("16", scenario_id),
         !grepl("17", scenario_id)) %>% 
  group_by(scenario_id, cat, unit) %>% 
  summarise(value = sum(value)) %>% 
  group_by(cat, unit) %>% 
  summarise(val_min = min(value), 
            val_max = max(value)) %>% 
  ggplot() + 
  geom_linerange(aes(reorder(x = cat, val_max),ymin = val_min, ymax = val_max)) + 
  coord_flip()

#--separated by location
#--no range in siskiyou pesticides?
d_raw %>% 
  left_join(scen_key) %>% 
  filter(unit == "kgco2e_hayr") %>% 
  #--not 16 and 17, the carbon credit changes
  filter(!grepl("16", scenario_id),
         !grepl("17", scenario_id)) %>%
  group_by(scenario_id, cat, unit, location) %>% 
  summarise(value = sum(value)) %>% 
  group_by(cat, unit, location) %>% 
  summarise(val_min = min(value), 
            val_max = max(value)) %>% 
  ggplot() + 
  geom_linerange(aes(reorder(x = cat, val_max),ymin = val_min, ymax = val_max, 
                     color = location), size = 3, position = position_dodge2(width = 0.5)) + 
  coord_flip() +
  labs(title = "Ranges in category values across scenarios",
       subtitle = "Carbon credit ranges not included",
       y = "kgCO2e per ha per year",
       x = NULL)

d_raw %>% 
  left_join(scen_key) %>% 
  filter(unit == "kgco2e_hayr") %>% 
  #--not 16 and 17, the carbon credit changes
  # filter(!grepl("16", scenario_id),
  #        !grepl("17", scenario_id)) %>%
  group_by(scenario_id, cat, unit, location) %>% 
  summarise(value = sum(value)) %>% 
  group_by(cat, unit, location) %>% 
  summarise(val_min = min(value), 
            val_max = max(value)) %>% 
  ggplot() + 
  geom_linerange(aes(reorder(x = cat, val_max),ymin = val_min, ymax = val_max, 
                     color = location), size = 3, position = position_dodge2(width = 0.5)) + 
  coord_flip() +
  labs(title = "Ranges in category values across scenarios",
       y = "kgCO2e per ha per year",
       x = NULL)



# tulare ------------------------------------------------------------------

#--energy
d_raw %>% 
  filter(unit == "GJ_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare", scen_desc == "base") %>%
  group_by(cat) %>% 
  mutate(sum = sum(value)) %>% 
  arrange(-sum) %>% 
  ungroup() %>% 
  mutate(
    cat = fct_inorder(cat),
    desc = fct_inorder(desc)) %>% 
  ggplot(aes(cat, value)) + 
  geom_col(aes(fill = desc), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = NULL,
       y = "GJ energy used per ha per year",
       title = "Tulare, base scenario")

d_raw %>% 
  select(unit) %>% 
  distinct()

#--GHG
d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare", scen_desc == "base") %>%
  group_by(cat) %>% 
  mutate(sum = sum(value)) %>% 
  arrange(-sum) %>% 
  ungroup() %>% 
  mutate(
    cat = fct_inorder(cat),
    desc = fct_inorder(desc)) %>% 
  ggplot(aes(cat, value)) + 
  geom_col(aes(fill = desc), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = NULL,
       y = "kgCO2e released per ha per year",
       title = "Tulare, base scenario")


d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare") %>%
  mutate(scen_desc = fct_inorder(scen_desc)) %>% 
  ggplot(aes(scen_desc, value)) + 
  geom_col(aes(fill = cat), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x= NULL,
       y = "kgCO2e per ha per year",
       title = "Tulare")


# all ------------------------------------------------------------------

#--GHG
d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(scen_desc == "base") %>%
  group_by(cat) %>% 
  mutate(sum = sum(value)) %>% 
  arrange(-sum) %>% 
  ungroup() %>% 
  mutate(
    cat = fct_inorder(cat),
    desc = fct_inorder(desc)) %>% 
  ggplot(aes(location, value)) + 
  geom_col(aes(fill = cat), color = "gray") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = NULL,
       y = "kgCO2e released per ha per year",
       title = "All locations, base scenarios")


d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare") %>%
  mutate(scen_desc = fct_inorder(scen_desc)) %>% 
  ggplot(aes(scen_desc, value)) + 
  geom_col(aes(fill = cat), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x= NULL,
       y = "kgCO2e per ha per year",
       title = "Tulare")