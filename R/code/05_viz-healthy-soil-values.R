#--map of carbon credits

library(maps)
library(tidyverse)
library(readxl)
library(patchwork)

c <- 
  read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx", skip = 5) %>% 
  mutate(Mgco2e_ac_yr = co2 + n2o) %>% 
  select(county, practice, Mgco2e_ac_yr) %>% 
  rename(subregion = county)


ca_counties <- 
  map_data("county", "california") %>% 
  as_tibble()

d <- 
  c %>% 
  left_join(ca_counties)

f1 <- 
  ggplot() +
  geom_polygon(
    data = ca_counties,
    aes(long, lat, group = group),
    fill = "white",
    color = "gray50"
  ) +
  geom_polygon(data = d %>% filter(grepl("basic rotation", practice)),
               aes(long, lat, 
                   group = group,
                   fill = Mgco2e_ac_yr),
               color = "gray50") +
  coord_quickmap() +
  scale_fill_viridis_c() +
  facet_grid(. ~ practice, labeller = label_wrap_gen())


f2 <- 
  ggplot() +
  geom_polygon(
    data = ca_counties,
    aes(long, lat, group = group),
    fill = "white",
    color = "gray50"
  ) +
  geom_polygon(data = d %>% filter(grepl("Pasture", practice)),
               aes(long, lat, 
                   group = group,
                   fill = Mgco2e_ac_yr),
               color = "gray50") +
  coord_quickmap() +
  scale_fill_viridis_c() +
  facet_grid(. ~ practice, labeller = label_wrap_gen())

f1 + f2
