# purpose: compare fuel energy contents and co2 emissions from diff sources
#--sources use different units which makes it hard
#--3/1 - still in progress for co2

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")


# read in manual reference sheet ------------------------------------------

e <- read_excel("R/data_refs/refbyhand_fuel.xlsx", skip = 5, sheet = "energy")
ghg <- read_excel("R/data_refs/refbyhand_fuel.xlsx", skip = 5, sheet = "combustion-co2")



# energy comparison -------------------------------------------------------

#--I have no idea what is going on with Grassini and Cassman's electricity value

e1 <- 
  e |>
  filter(fuel_type != "electricity") |>
  #--change all liquid values to mj/l
  mutate(
    value2 = case_when(
      unit == "mmbtu/gal" ~ value * btu_per_mmbtu * mj_per_btu * gal_per_l,
      unit == "mj/l" ~ value,
      TRUE ~ 999
    ),
    unit2 = "mj/l"
  ) |> 
  select(fuel_type, source, unit2, value2) |> 
  rename(value = value2,
         unit = unit2)


e1 |> 
  write_csv("R/data_refs/ref_fuel-energy.csv")
