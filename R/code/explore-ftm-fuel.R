# explore ftm disel values

library(tidyverse)
library(readxl)
library(patchwork)

rm(list = ls())


# data --------------------------------------------------------------------

d <- read_csv("data_raw/ftm_fuel/FTM-diesel-usage/data_tidy/operations_non-tillage.csv")

d_till <- read_csv("data_raw/ftm_fuel/FTM-diesel-usage/data_tidy/operations_tillage.csv")


d_till
