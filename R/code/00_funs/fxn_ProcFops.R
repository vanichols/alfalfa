#--prep field ops section
#--each row must be unique in the final scenario file
#--this code condenses the entered info into that form

library(tidyverse)



ProcFops <- function(f_scenario_id = "0007"){
  # read in manually created field ops info for each scenario ---------------
  
  
  d <- 
    read_csv(paste0("R/data_in/byhand-fieldops/fops_scen-", f_scenario_id, ".csv"), skip = 5) 
  
  
  # summarise ---------------------------------------------------------------
  
  d1 <- 
    d |> 
    fill(scenario_id, cat) |> 
    group_by(scenario_id, cat, desc) |> 
    summarise(value = sum(value, na.rm = T)) |> 
    mutate(unit = "pass/stand life")
  
  
  return(d1)
    
}

