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

#--calc irrigated vs non-irrigated
read_csv("R/data_tidy/nass_ac-harv.csv")

ac_harv <- 
  read_csv("R/data_tidy/nass_ac-harv.csv")

alf_ops <- 
  read_csv("R/data_tidy/nass_alf-ops.csv")


# look at maps-----------------------------------------------------------------

theme_set(theme_map() + theme(legend.position = "left"))

f1 <- 
  ca_counties %>% 
  left_join(d1) %>% 
  fill(data_item, .direction = "updown") %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = value), colour = "grey50") + 
  coord_quickmap() + 
  scale_fill_viridis_c() + 
  facet_grid(data_item~year, labeller = label_wrap_gen())

f2 <- 
  ca_counties %>% 
  left_join(d2) %>% 
  fill(data_item, .direction = "updown") %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = value), colour = "grey50") + 
  coord_quickmap() + 
  scale_fill_viridis_c() + 
  facet_grid(data_item~year, labeller = label_wrap_gen())

f3 <- 
  ca_counties %>% 
  left_join(d3) %>% 
  fill(data_item, .direction = "updown") %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = value), colour = "grey50") + 
  coord_quickmap() + 
  scale_fill_viridis_c() + 
  facet_grid(data_item~year, labeller = label_wrap_gen())


f4 <- 
  ca_counties %>% 
  left_join(d4) %>% 
  fill(data_item, .direction = "updown") %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = value), colour = "grey50") + 
  coord_quickmap() + 
  scale_fill_viridis_c() + 
  facet_grid(data_item~year, labeller = label_wrap_gen())

library(patchwork)

f1 / f2 / f3 / f4


#--our counties highlighted
ca_hili <- 
  ca_counties %>% 
  filter(county %in% c("siskiyou", "tulare", "imperial"))

ca_counties %>% 
  left_join(d1) %>% 
  fill(data_item, .direction = "updown") %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = value), colour = "grey50") + 
  geom_polygon(data = ca_hili, aes(lon, lat, group = group), fill = NA, color = "red", linewidth = 2) +
  coord_quickmap() + 
  scale_fill_viridis_c() + 
  facet_grid(data_item~year, labeller = label_wrap_gen())

ca_counties %>% 
  left_join(d2) %>% 
  fill(data_item, .direction = "updown") %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = value), colour = "grey50") + 
  geom_polygon(data = ca_hili, aes(lon, lat, group = group), fill = NA, color = "red", linewidth = 2) +
  coord_quickmap() + 
  scale_fill_viridis_c() + 
  facet_grid(data_item~year, labeller = label_wrap_gen())


#--make a nice figure

d1 %>% 
  ggplot(aes(value)) + 
  geom_histogram()

#>150k, 50k-150k, <50k

d1 %>% filter(year == 2017) %>% arrange(value)

ca_counties %>% 
  left_join(d1 %>% filter(year == 2017)) %>% 
  replace_na(list(value = 0)) %>%
  fill(c(data_item, program, year, state, ag_district), .direction = "updown")  %>% 
  mutate(valueF = log(value)) %>% 
  ggplot(aes(lon, lat, group = group)) +
  #geom_polygon(data = ca_counties, fill = "white", colour = "grey50") + 
  geom_polygon(aes(fill = value), colour = "grey50") + 
  #geom_polygon(data = ca_hili, aes(lon, lat, group = group), fill = NA, color = "red", linewidth = 2) +
  coord_quickmap() + 
  scale_fill_viridis_c() + 
  facet_grid(data_item~year, labeller = label_wrap_gen())


# over time ---------------------------------------------------------------

theme_set(theme_bw())

ac_harv %>% 
  ggplot(aes(year, value, group = county)) + 
  geom_line(aes(color = name)) +
  facet_grid(.~name) + 
  labs(title = "Most alfalfa acres are irrigated")


#--irr and non-irr
ac_harv %>% 
  filter(name != "acres_harvested") %>% 
  filter(year == 2017) %>% 
  group_by(year, name) %>% 
  summarise(value = sum(value)) %>% 
  group_by(year) %>% 
  mutate(tot = sum(value),
         pct = value/tot)

f_acres <- 
  ac_harv %>% 
  filter(year == 2017) %>% 
  filter(name != "acres_harvested") %>% 
  ggplot(aes(reorder(county, value, max), value, fill = name)) + 
  geom_col() + 
  coord_flip() + 
  labs(title = "California harvested 770K acres of alfalfa in 2017",
       subtitle = "86% of acres are irrigated") + 
  scale_y_continuous(labels = label_comma())

#f_time <- 
  ac_harv %>% 
  filter(name == "acres_harvested") %>%
  filter(county != "imperial") %>% 
  group_by(year, county) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(year, value)) + 
  geom_line(aes(group = county))  + 
  labs(title = "California harvested 770K acres of alfalfa in 2017",
       subtitle = "86% of acres are irrigated") + 
  scale_y_continuous(labels = label_comma())



#--where do ours fall?
ac_harv %>% 
  filter(name == "acres_harvested") %>% 
  ggplot(aes(year, value, group = county)) + 
  geom_line(aes(color = county %in% c("imperial", "siskiyou", "tulare"))) +
  facet_grid(.~name)

ac_harv %>% 
  filter(name == "acres_harvested",
         county %in% c("imperial", "siskiyou", "tulare")) %>% 
  ggplot(aes(as.factor(year), value, group = county)) + 
  geom_col(aes(fill = county), position = position_dodge()) +
  facet_grid(.~name)


ac_harv %>% 
  filter(name == "acres_harvested") %>% 
  group_by(county) %>% 
  summarise(value = sum(value)) %>% 
  arrange(-value)


alf_ops %>% 
  filter(name == "operations") %>% 
  ggplot(aes(year, value, group = county)) + 
  geom_line(aes(color = county %in% c("imperial", "siskiyou", "tulare"))) +
  facet_grid(.~name)
