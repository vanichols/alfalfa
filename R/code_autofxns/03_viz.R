#--look at results

rm(list = ls())
library(tidyverse)
library(pals)



# energy ------------------------------------------------------------------

e <- read_csv("R/code_autofxns/dataout/0002-energy.csv")


e1 <- 
  e |> 
  pivot_longer(GJ_stand:MJ_kg) |> 
  rename(unit = name) |> 
  #--make short category labels for figs
  mutate(cat_short = case_when(
    cat == "irrigation" ~ "irrig",
    cat == "fuel manufacture" ~ "fuel manu",
    cat == "fertilizer manufacture" ~ "fert manu",
    cat == "pesticide manufacture" ~ "pest manu",
    TRUE ~ cat
  )) |> 
  select(production_id, assump_id, cat,
         cat_short,
         desc, fuel_type, unit, value)
  
#--get grand total
etot <- 
  e1 |> 
  select(-fuel_type) |> 
  group_by(production_id, assump_id, unit) |> 
  mutate(value = sum(value),
         cat = "total",
         cat_short = "total",
         desc = "total") |> 
  distinct() 

#--combine to graph, get sums of each cat
e2 <- 
  etot |> 
  bind_rows(e1) |> 
  mutate(desc = paste(cat_short, desc, sep = "_")) |> 
  group_by(production_id, assump_id, cat) |> 
  mutate(cat_tot = sum(value)) |> 
  ungroup() 

n_clrs <-
  length(e2 |>
           unite(desc, cat, col = "cat_desc") |> 
           pull(cat_desc) |>
           unique())

e2 |> 
 filter(unit != "GJ_stand") |> 
  arrange(-cat_tot, -value) |> 
  mutate(desc = fct_inorder(desc),
         cat_short = fct_inorder(cat_short)) |> 
  ggplot(aes(fct_rev(cat_short), value)) + 
  geom_col(aes(fill = desc), color = "black") +
  scale_fill_manual(values = stepped(n = n_clrs)) +
  coord_flip() + 
  facet_wrap(~unit, scales = "free") +
  labs(x = NULL,
       title = "Tulare County, baseline",
       fill = NULL) + 
  theme(legend.position = "bottom")




