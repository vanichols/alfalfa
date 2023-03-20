#--make figure in datafigs

library(tidyverse)
library(pals)


VizEnergy <- function(f_scenario_id = "0001"){
  
  
  # results -----------------------------------------------------------------
  
  r <- read_csv(paste0("R/data_out/scen_", f_scenario_id, "-res.csv"))
  
  
  #--get scenario description
  s_log <- read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5)
  
  
  s_desc <- 
    r |> 
    select(scenario_id) |> 
    distinct() |> 
    left_join(s_log) |> 
    pull(scen_desc)
  
  e <- 
    r |> 
    filter(!grepl("co2e", unit)) |> 
    mutate(scenario_desc = s_desc)
  
  
  e1 <- 
    e |> 
    #--simplify irrigation to just ground/surface
    mutate(desc = case_when(
      grepl(", surface", desc) ~ "surface",
      grepl(", ground", desc) ~ "ground",
      TRUE ~ desc
    )) |> 
    group_by(scenario_id, scenario_desc, cat, desc, fuel_type, unit) |> 
    summarise(value = sum(value)) |> 
    #--make short category labels for figs
    mutate(cat_short = case_when(
      cat == "irrigation" ~ "irrig",
      cat == "fuel manufacture" ~ "fuel manu",
      cat == "fertilizer manufacture" ~ "fert manu",
      cat == "pesticide manufacture" ~ "pest manu",
      TRUE ~ cat
    )) |> 
    select(scenario_id,
           scenario_desc,
           cat,
           cat_short,
           desc, fuel_type, unit, value) |> 
    ungroup()
  
  #--get grand total
  etot <- 
    e1 |> 
    select(-fuel_type) |> 
    group_by(scenario_id, unit) |> 
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
    group_by(scenario_id, cat) |> 
    mutate(cat_tot = sum(value)) |> 
    ungroup() 
  
  n_clrs <-
    length(e2 |>
             unite(desc, cat, col = "cat_desc") |> 
             pull(cat_desc) |>
             unique())
  
  fig <- 
    e2 |> 
    filter(unit != "GJ_stand", 
           unit != "MJ_kg") |> 
    arrange(-cat_tot, -value) |> 
    mutate(desc = fct_inorder(desc),
           cat_short = fct_inorder(cat_short)) |> 
    ggplot(aes(fct_rev(cat_short), value)) + 
    geom_col(aes(fill = desc), color = "black") +
    scale_fill_manual(values = stepped(n = n_clrs)) +
    coord_flip() + 
    facet_wrap(~unit, scales = "free") +
    labs(x = NULL,
         title = "Tulare County",
         subtitle = paste(s_desc),
         fill = NULL) + 
    theme(legend.position = "bottom")
  

# fig w/o color -----------------------------------------------------------

  fig2 <- 
    e2 |> 
    filter(unit != "GJ_stand", 
           unit != "MJ_kg") |> 
    arrange(-cat_tot, -value) |> 
    mutate(desc = fct_inorder(desc),
           cat_short = fct_inorder(cat_short)) |> 
    ggplot(aes(fct_rev(cat_short), value, group = desc)) + 
    geom_col(fill = "white", color = "black") +
    coord_flip() + 
    facet_wrap(~unit, scales = "free") +
    labs(x = NULL,
         title = "Tulare County",
         subtitle = paste(s_desc),
         fill = NULL) + 
    theme(legend.position = "bottom")
  
  
  
  res <- list(e2, fig, fig2)
  
  return(res)
  
  
}
