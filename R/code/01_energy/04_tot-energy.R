# calculate energy use
#created 2/16

rm(list = ls())
library(tidyverse)
library(pals)
source("R/code/00_conversions.R")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_raw/lca-sheets/raw_assumptions.csv",
              skip = 5) |> 
  fill(assumption_id, cat) |> 
  select(-notes) |> 
  rename(
    cat_ass = cat,
    unit_ass = unit,
    value_ass = value)

a



# all energy --------------------------------------------------------------

#--fuel
fu <- read_csv("R/data_tidy/energy_fuel-use.csv")
fm <- read_csv("R/data_tidy/energy_fuel-manu.csv") #--I'm not sure this should be included

#--fert
f <- read_csv("R/data_tidy/energy_fert.csv")
f_avoid <- read_csv("R/data_tidy/energy_fert-avoided.csv")

#--pest
p <- read_csv("R/data_tidy/energy_pest.csv")

#--irr
i <- read_csv("R/data_tidy/energy_irrig.csv")

#--seed
s <- read_csv("R/data_tidy/energy_seed.csv") #--doesn't include harvest ops


tot <- 
  #--fuel use has to go first bc it has both production_id and assumption_id
  fu |> 
  bind_rows(fm) |> #--not sure whether to include
  bind_rows(f_avoid) |> 
  bind_rows(p) |> 
  bind_rows(f) |> 
  bind_rows(i) |> 
  bind_rows(s) |> 
  mutate(cat_short = case_when(
    cat == "fuel use" ~ "fuel use",
    cat == "fuel manufacture" ~ "fuel manu",
    cat == "fertilizer manufacture" ~ "fert",
    cat == "fertilizer avoidance" ~ "avoided fert",
    cat == "pesticide manufacture" ~ "pest",
    cat == "irrigation" ~ "irrig",
    cat == "seed" ~ "seed",
    TRUE ~ "XXX"
  )) |> 
  filter(value != 0) |> 
  fill(production_id, assumption_id, .direction = "downup")


tot |> 
  select(production_id, assumption_id, cat, cat_short, 
         desc, unit, value) |> 
  write_csv("R/data_tidy/energy_tot.csv")

tot |> 
  fill(production_id, assumption_id)


tot |> 
  group_by(production_id) |> 
  summarise(value = sum(value)/3)


tot |> 
  group_by(production_id) |> 
  summarise(value = sum(value))

tot1 <- 
  tot |> 
  group_by(production_id, assumption_id) |> 
  mutate(unit = "mj/stand",
         value = sum(value),
         cat = "total",
         cat_short = "total", 
         desc = "total") |> 
  distinct() |> 
  bind_rows(tot) |> 
  mutate(desc = paste(cat_short, desc, sep = "_")) |> 
  group_by(production_id, assumption_id, cat) |> 
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
       fill = "Component")

ggsave("R/figs/tulare-energy.png")


