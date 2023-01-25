
library(tidyverse)
library(readxl)
library(ggridges)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

theme_set(theme_bw())

# data --------------------------------------------------------------------

d_till <- 
  read_csv("data_tidy/operations_tillage.csv")


d_other <- 
  read_csv("data_tidy/operations_non-tillage.csv")


# figs --------------------------------------------------------------------


d_till %>% 
  unite(subcat, subsubcat, col = "subcat") %>% 
  ggplot(aes(reorder(subcat, diesel_galac), diesel_galac)) + 
  geom_jitter(width = 0.05) + 
  labs(x = NULL,
       y = "Diesel use (gal/ac)")

ggsave("figs/ftm_tillage.png")

