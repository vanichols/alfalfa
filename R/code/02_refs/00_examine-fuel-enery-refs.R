# look at how energy used to create electricity varies by state
#created 3/8/23

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_conversions.R")



e <- read_excel("R/data_refs/refbyhand_fuel.xlsx", sheet = "electric-by-state", skip = 5)


e |> 
  janitor::clean_names() |> 
  mutate(energy_ratio = total_energy / btu_per_mmbtu) |> 
  ggplot(aes(reorder(state, energy_ratio), energy_ratio)) + 
  geom_col(aes(fill = state == "CA")) + 
  coord_flip()
