#--prep field ops section
#--each row must be unique in the final scenario file
#--this code condenses the entered info into that form

library(tidyverse)



# read in manually created field ops info for each scenario ---------------


d <- read_csv("R/data_in/scenbyhand-fieldops.csv", skip = 5)



# summarise ---------------------------------------------------------------

d1 <- 
  d |> 
  fill(scenario_id, cat) |> 
  group_by(scenario_id, cat, desc) |> 
  summarise(value = sum(value)) |> 
  mutate(unit = "pass/stand life")


d1


d1 |> 
  write_csv("R/data_in/scen_fieldops.csv")
