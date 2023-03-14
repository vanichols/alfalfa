#--add together all ghg/credits

library(tidyverse)
library(readxl)
library(pals)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")

# data for stand life --------------------------------------------------------------------

d_raw <- read_csv("R/data_inputs/datin_production.csv",
                  skip = 5) |> 
  janitor::remove_empty()

d <- fun_preproc_prod(d_raw)

sl <- 
  d |> 
  filter(desc == "stand life") |> 
  rename("stand_life_yrs" = value) |> 
  select(production_id, stand_life_yrs)



# data --------------------------------------------------------------------

#--carbon credits
c <- read_csv("R/data_tidy/ghg_carboncredit.csv")

#--ghg from energy consump
e <- read_csv("R/data_tidy/ghg_energy-co2e.csv")

#--n2o
n <- read_csv("R/data_tidy/ghg_n2o.csv")


tot <- 
  c |> 
  bind_rows(e) |> 
  bind_rows(n) |> 
  filter(value != 0) |> 
  #--make short category labels for figs
  mutate(cat_short = case_when(
    cat == "irrigation" ~ "irrig",
    cat == "fuel manufacture" ~ "fuel manu",
    cat == "fertilizer manufacture" ~ "fert manu",
    cat == "pesticide manufacture" ~ "pest manu",
    TRUE ~ cat
  ))



# look at it --------------------------------------------------------------


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
  mutate(unit = "kg co2e/stand",
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

tot1


n_clrs <-
  length(tot1 |>
           unite(desc, cat, col = "cat_desc") |> 
           pull(cat_desc) |>
           unique())


#--need to check if the carbon credit is per year (would multiply by 3?)
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
       y = "Mg co2e/ha per year",
       title = "Tulare County",
       fill = NULL) + 
  theme(legend.position = "bottom")
  
ggsave("R/figs/tulare-ghg.png")
