# calculate energy use saved from less n requirement for next crop
#created 2/16
#--updated 2/24, assumptions have assumption id, production has prod_id


rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# references --------------------------------------------------------------

#--fertilizer manufacturing energy, from greet tables
fe <- read_csv("R/data_refs/ref_fert-energy.csv") 



# assumptions -------------------------------------------------------------

a <- read_csv("R/data_inputs/datin_assumptions.csv", skip = 5) 
a1 <- fun_preproc_assum(a)

#--assume tomato crop is following
#--get assumed amoutn of n application avoided
f <- 
  a1 %>% 
  filter(grepl("tomatoes", assump_desc)) %>% 
  mutate(
    assump_value = as.numeric(assump_value),
    #--units are currently lb n/ac
         kgn_ha_avoided = assump_value * kg_per_lb * ac_per_ha) %>% 
  select(assump_id, kgn_ha_avoided)

#--type of fert avoided, matters for N volatilization calcs
f_type <- 
  a1 |> 
  filter(assump_desc == "type of fertilizer avoided") |> 
  mutate(fert_avoided = assump_value) |> 
  select(assump_id, fert_avoided)


# energy to manu the fertilizer we avoided --------------------------------------------------


#--note if this were a different fertilizer, we would need to account for all the product, not just the n manufact. avoided
f2 <- 
  f %>% 
  left_join(f_type) |> 
  left_join(fe, by = c("fert_avoided" = "cat")) |> 
  mutate(
    mj_avoided = -(value * kgn_ha_avoided) #--negative bc we avoided it
  )

#--clean up
f3 <- 
  f2 |> 
  select(assump_id, mj_avoided) |> 
  rename(value = mj_avoided) |> 
  mutate(
    unit = "mj",
    cat = "fertilizer avoidance",
    desc = "avoided uan-32 manu energy")

f3 %>% 
  write_csv("R/data_tidy/energy_fert-avoided.csv")
