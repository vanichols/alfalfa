# purpose: see how alfalfa acreage has changed in CA over years
# using CDL instead of NASS bc it is easier (https://nassgeodata.gmu.edu/CropScape/)


library(tidyverse)
library(readxl)
library(patchwork)
library(scales)

theme_set(theme_bw())

d1 <- read_csv("data/cdl_2011_06.csv") %>% 
  janitor::clean_names() %>% 
  mutate(year = 2011)

d2 <- read_csv("data/cdl_2016_06.csv") %>% 
  janitor::clean_names() %>% 
  mutate(year = 2016)

d3 <- read_csv("data/cdl_2021_06.csv") %>% 
  janitor::clean_names() %>% 
  mutate(year = 2021)


d <- 
  d1 %>% 
  bind_rows(d2) %>% 
  bind_rows(d3) %>% 
  mutate_if(is.character, str_to_lower) 
  

# fig ---------------------------------------------------------------------

d %>% 
  pull(category) %>% 
  unique()

d %>% 
  group_by(year) %>% 
  summarise(acreage = sum(acreage)) %>% 
  mutate(category = "total")

#--change, absolute
d %>% 
  select(-count, -value) %>% 
  pivot_wider(names_from = year, values_from = acreage) %>% 
  janitor::clean_names() %>% 
  mutate(diff = x2021 - x2011,
         pct_diff = diff/x2011) %>% 
  filter(!is.na(diff),
         abs(diff) > 100000) %>% 
  ggplot(aes(reorder(category, diff), diff)) + 
  geom_col(aes(fill = category == "alfalfa")) +
  scale_y_continuous(labels = label_comma()) +
  coord_flip() + 
  labs(title = "Cropscape change in acreage 2011-2021",
       subtitle = "Filtered to categories with >100,000 acre changes",
       x = NULL,
       y = "Change in Acres")


#--change, relative
d %>% 
  select(-count, -value) %>% 
  pivot_wider(names_from = year, values_from = acreage) %>% 
  janitor::clean_names() %>% 
  mutate(diff = x2021 - x2011,
         pct_diff = diff/x2011) %>% 
  filter(!is.na(diff),
         abs(diff) > 100000) %>% 
  ggplot(aes(reorder(category, pct_diff), pct_diff)) + 
  geom_col() +
  scale_y_continuous(labels = label_percent()) +
  coord_flip() + 
  labs(title = "Cropscape change in acreage 2011-2021",
       subtitle = "Filtered to categories with >100,000 acre changes",
       x = NULL,
       y = "Percent Change in Acres")

#--alfalfa
d %>% 
  mutate_if(is.character, str_to_lower) %>% 
  filter(category == "alfalfa") %>% 
  ggplot(aes(year, acreage)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(labels = label_comma())
