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
