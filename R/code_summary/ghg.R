#--visualize/summarise results
#--created 3/15, work in progress

rm(list = ls())
library(tidyverse)
library(pals)


# data --------------------------------------------------------------------

e <- read_csv("R/code_auto/02_energy/tulare_001-energy.csv")
g <- read_csv("R/code_auto/03_ghg/tulare_001-ghg.csv")

g1 <- 
  g |> 
  #--make short category labels for figs
  mutate(cat_short = case_when(
    cat == "irrigation" ~ "irrig",
    cat == "fuel manufacture" ~ "fuel manu",
    cat == "fertilizer manufacture" ~ "fert manu",
    cat == "pesticide manufacture" ~ "pest manu",
    TRUE ~ cat
  ))



###################### stopped
g2 <- 
  g1 |> 
  select(-fuel_type) |> 
  mutate(value = kgco2e_hayr) |> 
  group_by(production_id, assump_id) |> 
  mutate(unit = "kg co2e/stand",
         value = sum(value),
         cat = "total",
         cat_short = "total",
         desc = "total") |> 
  distinct() |> 
  bind_rows(g1) |> 
  mutate(desc = paste(cat_short, desc, sep = "_")) |> 
  group_by(production_id, assump_id, cat) |> 
  mutate(cat_tot = sum(value)) |> 
  ungroup() 

t1


n_clrs <-
  length(t1 |>
           unite(desc, cat, col = "cat_desc") |> 
           pull(cat_desc) |>
           unique())


#--need to check if the carbon credit is per year (would multiply by 3?)
t1 |> 
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

