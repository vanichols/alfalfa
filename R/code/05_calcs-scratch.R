#--answer Qs

rm(list = ls())
library(tidyverse)



# 1. number of field passes -----------------------------------------------

# number of field passes in a production year


GetFieldOps <- function(my_scen = "0001"){
  
  d.tmp <- 
    read_csv(paste0("R/data_scens-notouch/scen_", my_scen, ".csv")) %>% 
    filter(cat %in% c("field ops", "harvest ops")) %>% 
    mutate(value = as.numeric(value)) %>% 
    summarise(value = sum(value)) %>% 
    pull(value)
  
  return(d.tmp)
}

GetStandLife <- function(my_loc = "tulare") {

  d.tmp <- 
  read_csv(paste0("R/data_in/", my_loc, "/base_other.csv"), skip = 5) %>% 
  filter(desc == "stand life") %>% 
  mutate(value = as.numeric(value)) %>% 
  pull(value)
  
  return(d.tmp)

    
}

t_fops <- GetFieldOps(my_scen = "0001")
t_sl <- GetStandLife(my_loc = "tulare")

t_fops/t_sl

s_fops <- GetFieldOps(my_scen = "1001")
s_sl <- GetStandLife(my_loc = "siskiyou")
s_fops/s_sl

i_fops <- GetFieldOps(my_scen = "2001")
i_sl <- GetStandLife(my_loc = "imperial")
i_fops/i_sl

