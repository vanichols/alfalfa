#--prep field ops section
#--each row must be unique in the final scenario file
#--this code condenses the entered info into that form
#--updated 3/24 to just take in whatever data you feed it


library(tidyverse)



ProcFops <- function(data = d){
  # read in manually created field ops info for each scenario ---------------
  
  
  # summarise ---------------------------------------------------------------
  
  d1 <- 
    data |> 
    fill(scenario_id, cat) |> 
    group_by(scenario_id, cat, desc) |> 
    summarise(value = sum(value, na.rm = T)) |> 
    mutate(unit = "pass/stand life")
  
  
  return(d1)
    
}

