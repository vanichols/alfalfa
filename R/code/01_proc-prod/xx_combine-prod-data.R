
library(tidyverse)

rm(list = ls())

# production data ---------------------------------------------------------

#--read in production info
f_list <- list.files("R/data_tidy/")
f_prod <- f_list[grepl("prod", f_list)]


d <- NULL

for (i in 1:length(f_prod)) {
  
  d.tmp <- read_csv(paste0("R/data_tidy/", f_prod[i]))
  
  d <- bind_rows(d, d.tmp)
  
}

d

d %>% 
  write_csv("R/data_tidy/prod_all.csv")
