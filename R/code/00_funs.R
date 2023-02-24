library(packagefinder)

#findPackage("unit conversion")
library(tidyverse)


fun_preproc <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(production_id, .direction = c("down")) %>% 
    fill(cat, .direction = c("down")) %>% 
    fill(desc, .direction = c("down")) %>% 
    select(-notes) %>% 
    mutate_if(is.character, str_to_lower)
  return(tmp)
}

