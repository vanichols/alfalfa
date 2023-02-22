# calculate energy use
#created 2/16

rm(list = ls())
library(tidyverse)
library(pals)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) %>% 
  fill(scenario_id, cat) %>% 
  select(-notes) %>% 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value)

a


# seeds -------------------------------------------------------------------

f <- read_csv("R/data_tidy/energy_fert.csv")
p <- read_csv("R/data_tidy/energy_pest.csv")
fu <- read_csv("R/data_tidy/energy_fuel.csv")
i <- read_csv("R/data_tidy/energy_irrig.csv")
s <- read_csv("R/data_tidy/energy_seed.csv")

tot <- 
  f %>% 
  bind_rows(p) %>% 
  bind_rows(fu) %>% 
  bind_rows(i) %>% 
  bind_rows(s) %>% 
  mutate(cat_short = case_when(
    cat == "fuel use" ~ "fuel",
    cat == "fertilizer manufacture" ~ "fert",
    cat == "pesticide manufacture" ~ "pest",
    cat == "irrigation" ~ "irrig",
    cat == "seed" ~ "seed",
    TRUE ~ "XXX"
  )) %>% 
  filter(value > 0) 




n_clrs <-
  length(tot %>%
           pull(desc) %>%
           unique())

tot %>% 
  group_by(scenario_id) %>% 
  summarise(value = sum(value)/3)


tot %>% 
  group_by(scenario_id) %>% 
  summarise(value = sum(value))


tot %>% 
  mutate(desc = paste(cat_short, desc, sep = "_")) %>% 
  group_by(scenario_id, cat) %>% 
  mutate(cat_tot = sum(value)) %>% 
  arrange(value) %>% 
  mutate(desc = fct_inorder(desc)) %>% 
  ggplot(aes(reorder(cat_short, value, sum), value)) + 
  geom_col(aes(fill = desc), color = "black") +
  scale_fill_manual(values = kelly(n = n_clrs)) +
  coord_flip() + 
  labs(x = NULL,
       y = "MJ/ha of a 3-year alfalfa stand",
       title = "Tulare County",
       fill = "Component")

ggsave("R/figs/tulare-energy.png")


