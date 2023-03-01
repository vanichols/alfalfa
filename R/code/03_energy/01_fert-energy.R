# calculate energy use
#created 2/16
#--3/1 clean up and check

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# references --------------------------------------------------------------

#--fertilizer manufacturing energy, from greet tables
fe <- read_csv("R/data_refs/ref_fert-energy.csv") |> 
  mutate(mj_kgprod = value,
         component = desc,
         desc = cat) |> 
  select(desc, component, mj_kgprod)



# production info ---------------------------------------------------------

# how much fertilizer did we apply?

f <- read_csv("R/data_tidy/prod_fertility.csv")

#--combine with fert energy

f1 <- 
  f |> 
  left_join(fe)


#--calculate energy used to manufacture each component applied
f2 <- 
  f1 %>% 
  mutate(mj_stand = value * mj_kgprod) |> 
  #--clean up col names
  unite(desc, component, col = "desc", sep = ", ") |> 
  select(production_id, cat, desc, mj_stand) |> 
  mutate(unit = "mj/stand") |> 
    rename(value = mj_stand)
    
    
f2

f2 %>% 
  write_csv("R/data_tidy/energy_fert.csv")

