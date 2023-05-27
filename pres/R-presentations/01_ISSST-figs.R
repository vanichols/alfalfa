library(tidyverse)
library(maps)
library(ggthemes)
library(scales)

rm(list = ls())

source("Rcode_color-palettes.R")




# 1. water use ---------------------------------------------------------------

pres_theme1 <- 
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(fill = "white", colour = 'black', linewidth = 3))


#--figure 2 from https://pacinst.org/wp-content/uploads/2015/04/CA-Ag-Water-Use.pdf
d1 <- 
  tibble(crop = c("alfalfa", "almond", "pasture", "rice", "corn"),
            waterapplied_million = c(5, 3.8, 3.1, 2.7, 2.2)) %>% 
  mutate_if(is.character, str_to_title)


d1 %>% 
  ggplot(aes(reorder(crop, -waterapplied_million), waterapplied_million)) + 
  geom_col(aes(fill = crop == "Alfalfa"), color = "black", linewidth = 2, show.legend = F) + 
  scale_fill_manual(values = c(p1_tan, p1_dkbl)) + 
  labs(x = NULL,
       y = "Millions of\nacre-ft applied\nin 2010",
       caption = "Data from Department of Water Resources, 2010",
       title = str_wrap("Alfalfa has the highest water application of all California crops", width = 40),
       #subtitle = "This is a function of both acreage and high water requirements"
       ) + 
  pres_theme1

ggsave("pres/fig_water-for-alf.png",
       width = 10, height = 8)

# 2. map of alfalfa acres ----------------------------------------------------

#--nass data

ca_counties <- 
  map_data("county", "california") %>% 
  select(lon = long, lat, group, county = subregion) %>% 
  as_tibble() %>% 
  distinct() 

ggplot(ca_counties, aes(lon, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey50") + 
  coord_quickmap()

#--raw nass data
rd2 <- 
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

d2 <- 
  rd2 %>% 
  group_by(data_item) %>% 
  group_split()

d2_acres <- d2[[1]] #--acres harvested
d2_irracres <- d2[[3]] #--irrigated acres harvested


d2_acres %>% 
  ggplot(aes(value)) + 
  geom_histogram() + 
  theme_bw()



map_theme <- 
  theme_map() + 
  theme(legend.position = c(0.5, 0.7),
        #legend.justification = c(0.9, 0.9),
        legend.text = element_text(size = rel(3.5)),
        legend.title = element_text(size = rel(4)),
        plot.caption = element_text(color = "white"))


#>150k, 50k-150k, <50k
ca_counties %>% 
  left_join(d2_acres %>% filter(year == 2017)) %>% 
  replace_na(list(value = 0)) %>%
  fill(c(data_item, program, year, state, ag_district), .direction = "updown")  %>% 
  mutate(value_cat = case_when(
    value <= 25000 ~ "<25k",
    value > 25000 ~ ">25k"
  )) %>% 
  ggplot(aes(lon, lat, group = group)) +
  #geom_polygon(data = ca_counties, fill = "white", colour = "grey50") + 
  geom_polygon(aes(fill = value_cat), colour = "grey50") + 
  #geom_polygon(data = ca_hili, aes(lon, lat, group = group), fill = NA, color = "red", linewidth = 2) +
  map_theme +
  scale_fill_manual(values = c(p1_tan, p1_dkbl)) +
  labs(fill = "Irrigated acres\nof alfalfa",
       caption = "Data from NASS 2017")

ggsave("pres/fig_map-alf-acres.png",
       width = 8, height = 10)

#--same fig, no legend
#>150k, 50k-150k, <50k
ca_counties %>% 
  left_join(d2_acres %>% filter(year == 2017)) %>% 
  replace_na(list(value = 0)) %>%
  fill(c(data_item, program, year, state, ag_district), .direction = "updown")  %>% 
  mutate(value_cat = case_when(
    value <= 25000 ~ "<25k",
    value > 25000 ~ ">25k"
  )) %>% 
  ggplot(aes(lon, lat, group = group)) +
  #geom_polygon(data = ca_counties, fill = "white", colour = "grey50") + 
  geom_polygon(aes(fill = value_cat), colour = "grey50", show.legend = F) + 
  #geom_polygon(data = ca_hili, aes(lon, lat, group = group), fill = NA, color = "red", linewidth = 2) +
  map_theme +
  scale_fill_manual(values = c(p1_tan, p1_dkbl)) +
  labs(fill = "Irrigated acres\nof alfalfa",
       caption = "Data from NASS 2017")

ggsave("pres/fig_map-alf-acres-no-leg.png",
       width = 8, height = 10)

# 3. water needs ---------------------------------------------------------------

pres_theme3 <- 
  theme_bw() +
  theme(axis.text = element_text(size = rel(2)),
        plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(2)),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(fill = "white", colour = 'black', linewidth = 3))


#--figure 3 from https://pacinst.org/wp-content/uploads/2015/04/CA-Ag-Water-Use.pdf
d3 <- 
  tibble(crop = c("rice", "alfalfa", "pasture", "almond", "cotton"),
         water_req = c(5.1, 4.9, 4.2, 4, 3.2)) %>% 
  mutate_if(is.character, str_to_title)


d3 %>% 
  arrange(water_req) %>% 
  mutate(crop = fct_inorder(crop)) %>% 
  ggplot(aes(crop, water_req)) + 
  geom_segment(aes(x = crop, xend = crop, y = 0, yend = water_req), size = 3) +
  geom_text(aes(x = crop, y = water_req + 1, label = water_req), size = 20) +
  geom_point(aes(fill = crop == "Alfalfa"), stroke = 3, size = 30, show.legend = F, pch = 21) + 
  scale_fill_manual(values = c(p1_tan, p1_dkbl)) + 
  scale_y_continuous(limits = c(0, 7)) +
  labs(x = NULL,
       y = "Acre-ft needed",
       caption = "Data from Department of Water Resources, 2010",
       title = str_wrap("Alfalfa has the 2nd highest water requirements per acre", width = 40),
       ) + 
  coord_flip() +
  pres_theme3

ggsave("pres/fig_alf-water-req.png",
       width = 10, height = 8)

