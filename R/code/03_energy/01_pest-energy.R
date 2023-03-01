# calculate energy use
#created 2/16
#--2/28 update to production_id and assumption_id (not scenario_id)
#--3/1 clean up

rm(list = ls())
library(tidyverse)
source("R/code/00_conversions.R")
source("R/code/00_funs.R")


# assumptions -------------------------------------------------------------

a <- read_csv("R/data_inputs/datin_assumptions.csv", skip = 5) 
a1 <- fun_preproc_assum(a)

a_p <- 
  a1 |> 
  filter(assump_cat == "pesticide manufacture") |>
  mutate(assump_value = as.numeric(assump_value)) |> 
  rename(desc = assump_desc)




# pesticide manu energy ---------------------------------------------------

p <- read_csv("R/data_tidy/prod_pesticides.csv")

p1 <-
  p %>% 
  select(-cat) %>% 
  left_join(a_p, by = "desc") 

p2 <- 
  p1 %>%
  mutate(value = value * assump_value,
         unit = "mj/stand",
         cat = assump_cat) %>% 
  select(assump_id, cat, desc, unit, value)


p2 %>% 
  write_csv("R/data_tidy/energy_pest.csv")
