#--look at alfalfa production in california
#--nass data downloaded by hand

library(tidyverse)
library(maps)
library(ggthemes)
library(scales)

# california map data -----------------------------------------------------

ca_counties <- 
  map_data("county", "california") %>% 
  select(lon = long, lat, group, county = subregion) %>% 
  as_tibble() %>% 
  distinct() 

ggplot(ca_counties, aes(lon, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey50") + 
  coord_quickmap()


# data --------------------------------------------------------------------

d <- 
  read_csv("R/data_raw/nass/NASS-milk-production.csv") %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty() %>% 
  mutate_if(is.character, str_to_lower)

d
