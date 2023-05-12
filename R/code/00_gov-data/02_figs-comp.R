#--compare nass and cdl acreages

library(tidyverse)
library(maps)
library(ggthemes)
library(scales)

options(ggplot2.discrete.colour = c("#ecb602", "#290849"))

#lighter purple: #55185d

# data --------------------------------------------------------------------

nass_raw <- 
  read_csv("R/data_tidy/nass_ac-harv.csv") %>% 
  filter(name == "acres_harvested")

nass_tot <- 
  nass_raw %>% 
  group_by(program, year, state, name) %>% 
  summarise(value = sum(value)) %>% 
  mutate(county = "all counties")


nass <- 
  nass_raw %>% 
  filter(county %in% c("tulare", "siskiyou", "imperial", "yolo", "all counties")) %>% 
  bind_rows(nass_tot) %>% 
  select(year, county, nass_ac = value)

cdl <- 
  read_csv("R/data_tidy/cdl_alf-acreage.csv") %>% 
  filter(category == "alfalfa") %>% 
  select(year, county, cdl_ac = acreage)

d <- 
  cdl %>% 
  left_join(nass)


# compare -----------------------------------------------------------------

d %>% 
  pivot_longer(nass_ac:cdl_ac) %>%
  mutate(name = ifelse(name == "nass_ac", "NASS - acres harvested",
                       "Crop Data Layer - acres")) %>% 
  ggplot(aes((year), value, color = name)) + 
  geom_point(size = 2, aes(shape = name)) + 
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~county, scales = "free_y") + 
  labs(y = "acres")


#--is acreage just droping in certain regions (nass?)
nass_raw %>% 
  group_by(year, ag_district) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(year, value, color = ag_district)) + 
  geom_line(linewidth = 2) + 
  facet_wrap(~ag_district, scales = "free")

#--make the change relative?
nass_raw %>% 
  group_by(year, ag_district) %>% 
  summarise(value = sum(value)) %>%
  group_by(ag_district) %>% 
  mutate(max_ac = max(value),
         rel_ac = value/max_ac) %>% 
  ggplot(aes(year, rel_ac, groups = ag_district)) + 
  geom_line(linewidth = 2, aes(color = ag_district))

#--central valleys are the ones w/dramatic dec?
nass_raw %>% 
  group_by(year, ag_district) %>% 
  summarise(value = sum(value)) %>%
  group_by(ag_district) %>% 
  mutate(max_ac = max(value),
         rel_ac = value/max_ac) %>% 
  ggplot(aes(year, rel_ac, groups = ag_district)) + 
  geom_line(linewidth = 2, aes(color = ag_district %in% c("san joaquin valley", "sacramento valley")),
            show.legend = F)
