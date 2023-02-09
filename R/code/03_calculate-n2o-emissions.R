#--think about processing lca sheet

library(tidyverse)
library(readxl)
library(measurements)


source("R/code/00_conversions.R")


# clean up function -------------------------------------------------------


fun_preproc <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(system, .direction = c("down")) %>% 
    fill(flow_type, .direction = c("down")) %>% 
    fill(flow_cat, .direction = c("down")) %>% 
    select(-notes) %>% 
    pivot_longer(mid:worst) 
    
  return(tmp)
  }





# n2o ---------------------------------------------------------------------



n2o <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "soil_emissions", 
                   skip = 5)

n2o1 <- 
  fun_preproc(data = n2o) 

#--need to utilize fertilizer info for this calc
