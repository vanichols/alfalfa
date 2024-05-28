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

d_raw <- read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx",
                    skip = 5)

d_raw %>%
  pivot_longer(co2:n2o) %>% 
  ggplot(aes(practice, value)) + 
  geom_point(aes(color = county), size = 4) + 
  coord_flip() + 
  facet_grid(name~.)



