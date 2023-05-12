#--look at alfalfa production in california

library(tidyverse)
library(maps)
library(ggthemes)
library(scales)


# cdl data ----------------------------------------------------------------

d <- read_csv("R/data_tidy/cdl_landuse-acreage.csv")

d %>% pull(year) %>% unique()

d %>% 
  filter(county == "yolo",
         year %in% c(2007, 2012, 2017)) %>% 
  arrange(-year, -acreage) %>% 
  filter(category %in% c("almonds", "alfalfa", 
                         "winter wheat", "tomatoes")) %>% 
  ggplot(aes(year, acreage)) + 
  geom_col(aes(fill = category), color = "black")
