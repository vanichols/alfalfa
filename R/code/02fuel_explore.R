#--explore


library(tidyverse)
library(readxl)
library(ggridges)

rm(list = ls())

theme_set(theme_bw())

# data --------------------------------------------------------------------

d_till <- 
  read_csv("R/data_tidy/operations_tillage.csv")


d_other <- 
  read_csv("R/data_tidy/operations_non-tillage.csv")


# figs --------------------------------------------------------------------


d_till %>% 
  unite(subcat, subsubcat, col = "subcat") %>% 
  ggplot(aes(reorder(subcat, diesel_galac), diesel_galac)) + 
  geom_jitter(width = 0.05) + 
  labs(x = NULL,
       y = "Diesel use (gal/ac)")

ggsave("R/figs/ftm_tillage.png")

