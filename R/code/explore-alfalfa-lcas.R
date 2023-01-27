# purpose: look at values from literature on alfalfa lcas

library(tidyverse)
library(readxl)
library(patchwork)

source("code/austin-theme.R")


d <- read_excel("data/alfalfa-LCAs.xlsx") %>% janitor::clean_names()

# convert units to Mg CO2eq per ha
d1 <- 
  d %>% 
  filter(is.na(mgco2eq_ha)) %>% 
  mutate(mgco2eq_mgdm = gco2eq_kgdm/1000,
         mgco2eq_ha = mgco2eq_mgdm * yield_mgha,
         mgco2eq_ha = round(mgco2eq_ha, 2))

d2 <- 
  d %>% 
  filter(!is.na(mgco2eq_ha))


res <- 
  d1 %>% 
  bind_rows(d2) %>% 
  select(study, location, desc, mgco2eq_ha)


res %>% 
  mutate(n = 1:n(),
         id = paste(location, n, sep = "_")) %>% 
  ggplot(aes(reorder(id, mgco2eq_ha), mgco2eq_ha)) + 
  geom_hline(yintercept = 0, color = "red") +
  geom_point(size = 5) + 
  geom_segment(aes(x = id, xend = id,
                   y = 0, yend = mgco2eq_ha)) + 
  labs(x = NULL,
       y = "Mg CO2eq\nreleased\nper hectare",
       title = "Alfalfa LCA studies") + 
  theme_ap2_big + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
