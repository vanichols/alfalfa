#--prep pesticide section
#--each row must be unique in the final scenario file
#--this code condenses the entered info into that form
#--simplify to just read in the data you feed it 3/24/2023


library(tidyverse)


ProcPest <- function(data = d){
  
  # summarise ---------------------------------------------------------------
  
  d1 <- 
    data |> 
    fill(scenario_id, cat) |> 
    group_by(scenario_id, cat, desc, unit) |> 
    summarise(value = sum(value, na.rm = T)) |> 
    mutate(desc = str_to_lower(desc))
  
  
  d1
  
  return(d1)
  
}

