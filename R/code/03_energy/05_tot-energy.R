# calculate energy use
#created 2/16
#--3/8 updated with new fuel manu values

rm(list = ls())
library(tidyverse)
library(pals)
source("R/code/00_conversions.R")

# all energy --------------------------------------------------------------

#--fuel
tr <- read_csv("R/data_tidy/energy_tractor.csv")
ir <- read_csv("R/data_tidy/energy_irrig.csv")
ma <- read_csv("R/data_tidy/energy_fuel-manu.csv")

#--fert
f <- read_csv("R/data_tidy/energy_fert.csv")
f_avoid <- read_csv("R/data_tidy/energy_fert-avoided.csv")

#--pest
p <- read_csv("R/data_tidy/energy_pest.csv")

#--seed
#--doesn't include harvest ops, but probably shouldn't include so much irrig
s <- read_csv("R/data_tidy/energy_seed.csv") 


tot <- 
  tr |> 
  bind_rows(ir) |> 
  bind_rows(ma) |> 
  bind_rows(f) |> 
  bind_rows(f_avoid) |> 
  bind_rows(p) |> 
  bind_rows(s) |> 
  filter(value != 0) |> 
  fill(production_id, assump_id, .direction = "downup") |> 
  #--make short category labels for figs
  mutate(cat_short = case_when(
    cat == "irrigation" ~ "irrig",
    cat == "fuel manufacture" ~ "fuel manu",
    cat == "fertilizer manufacture" ~ "fert manu",
    cat == "pesticide manufacture" ~ "pest manu",
    TRUE ~ cat
  ))
  

tot

#--write it
tot |> 
  select(production_id, assump_id, cat,
         desc, fuel_type, unit, value) |> 
  write_csv("R/data_tidy/energy_tot.csv")


# figure ------------------------------------------------------------------

tot |> 
  group_by(production_id) |> 
  summarise(value = sum(value)/3)


tot |> 
  group_by(production_id) |> 
  summarise(value = sum(value))

tot1 <- 
  tot |> 
  select(-fuel_type) |> 
  group_by(production_id, assump_id) |> 
  mutate(unit = "mj/stand",
         value = sum(value),
         cat = "total",
         cat_short = "total",
         desc = "total") |> 
  distinct() |> 
  bind_rows(tot) |> 
  mutate(desc = paste(cat_short, desc, sep = "_")) |> 
  group_by(production_id, assump_id, cat) |> 
  mutate(cat_tot = sum(value)) |> 
  ungroup() 

n_clrs <-
  length(tot1 |>
           unite(desc, cat, col = "cat_desc") |> 
           pull(cat_desc) |>
           unique())

tot1 |> 
  mutate(value = value/3 / 1000) |> 
  arrange(-cat_tot, -value) |> 
  mutate(desc = fct_inorder(desc),
         cat_short = fct_inorder(cat_short)) |> 
  ggplot(aes(fct_rev(cat_short), value)) + 
  geom_col(aes(fill = desc), color = "black") +
  scale_fill_manual(values = stepped(n = n_clrs)) +
  coord_flip() + 
  labs(x = NULL,
       y = "GJ/ha per year",
       title = "Tulare County",
       fill = NULL) + 
  theme(legend.position = "bottom")

ggsave("R/figs/tulare-energy.png")


