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

us.cities %>% 
  as_tibble()
map.cities(country = "USA", capitals = 2)

# raw data ----------------------------------------------------------------

rd <- 
  read_csv("R/data_raw/nass/NASS-alfalfa-hay.csv") %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty() %>% 
  select(-watershed_code, -domain_category, -period,
         -state_ansi, -ag_district_code, -county_ansi,
         -commodity, -geo_level, -domain, -cv_percent) %>% 
  filter(value != "(D)") %>% 
  mutate(value = parse_number(value)) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  arrange(county)


# separate into data items ------------------------------------------------

d <- 
  rd %>% 
  group_by(data_item) %>% 
  group_split()

d1 <- d[[1]]
d2 <- d[[2]]
d3 <- d[[3]]
d4 <- d[[4]]


# ac harv, irr & nonirr ---------------------------------------------------

ah <- 
  d1 %>% 
  bind_rows(d3) %>% 
  mutate(data_item = str_remove_all(pattern = "hay, alfalfa - ", data_item),
         data_item = str_remove_all(pattern = "hay, alfalfa, ", data_item)) %>% 
  pivot_wider(names_from = data_item, values_from = value) %>% 
  janitor::clean_names() %>% 
  replace_na(list(acres_harvested = 0,
                  irrigated_acres_harvested = 0)) %>% 
  mutate(nonirr_acres_harvested = acres_harvested - irrigated_acres_harvested) %>% 
  pivot_longer(6:ncol(.))


ah %>% 
  write_csv("R/data_tidy/nass_ac-harv.csv")


# number of ops -----------------------------------------------------------

ao <- 
  d2 %>% 
  bind_rows(d4) %>% 
  mutate(data_item = str_remove_all(pattern = "hay, alfalfa - ", data_item),
         data_item = str_remove_all(pattern = "hay, alfalfa, ", data_item)) %>% 
  pivot_wider(names_from = data_item, values_from = value) %>% 
  janitor::clean_names() %>% 
  replace_na(list(operations_with_area_harvested = 0,
                  irrigated_operations_with_area_harvested = 0)) %>% 
  mutate(nonirr_operations_with_area_harvested = operations_with_area_harvested - irrigated_operations_with_area_harvested) %>% 
  pivot_longer(6:ncol(.)) %>% 
  mutate(name = str_remove_all(pattern = "_with_area_harvested", name))



ao %>% 
  write_csv("R/data_tidy/nass_alf-ops.csv")

