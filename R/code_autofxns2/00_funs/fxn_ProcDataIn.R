#findPackage("unit conversion")
library(tidyverse)


ProcDataIn <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(scenario_id, .direction = c("down")) %>% 
    fill(cat, .direction = c("down")) %>% 
    fill(desc, .direction = c("down")) %>% 
    select(-notes) %>% 
    mutate_if(is.character, str_to_lower) 
  return(tmp)
}

