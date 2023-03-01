library(packagefinder)

#findPackage("unit conversion")
library(tidyverse)


fun_preproc_prod <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(production_id, .direction = c("down")) %>% 
    fill(cat, .direction = c("down")) %>% 
    fill(desc, .direction = c("down")) %>% 
    select(-notes) %>% 
    mutate_if(is.character, str_to_lower)
  return(tmp)
}


fun_preproc_assum <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(assump_id, .direction = c("down")) %>% 
    fill(assump_cat, .direction = c("down")) %>% 
    fill(assump_desc, .direction = c("down")) %>% 
    select(-notes) %>% 
    mutate_if(is.character, str_to_lower)
  return(tmp)
}
