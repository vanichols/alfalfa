#--processing irrigation component of scenario sheet
#--created 2/15, updated 2/16
#--3/1 file format update

rm(list = ls())

library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")
source("R/code/00_funs.R")



# data --------------------------------------------------------------------

d_raw <- read_csv("R/data_inputs/datin_production.csv",
                  skip = 5) |> 
  janitor::remove_empty()


d <- fun_preproc_prod(d_raw)

# field ops -----------------------------------------------------


f1 <- 
  d |> 
  filter(cat == 'field ops')
  
f2 <- 
  f1 |> 
  group_by(production_id, cat, desc, unit) |> 
  summarise(value = sum(value, na.rm = T)) |> 
  mutate(unit = "pass/stand")

f2

f2 |> 
  write_csv("R/data_tidy/prod_fieldops.csv")



