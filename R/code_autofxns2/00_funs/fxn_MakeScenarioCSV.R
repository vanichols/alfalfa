#--create scen_000x files with different variations of a given production scenario
#--using scen_0001 as base file to change
#--3/16 combined assump file and prod file, need to change code

rm(list = ls())
library(tidyverse)

#--first, manually enter what you what into the scenario log in datain folder
#--this fxn translates that change into the file formats the other functions need


MakeScenarioCSV <- function(f_scenario_id = "0003"){
  
  
  source("R/code_autofxns2/00_funs/fxn_ProcDataIn.R")
  source("R/code_autofxns2/00_funs/fxn_conversions.R")
  
  snew <- 
    read_csv("R/code_autofxns2/datain/man_scenario_log.csv", skip = 5) |> 
    filter(scenario_id == paste0("scen_", f_scenario_id)) |> 
    fill(scen_desc)

        
  s <- 
    read_csv("R/code_autofxns2/datain/scen_base_5empty.csv", skip = 5) |> 
    ProcDataIn()
  
  #--separate variables that will and will not change
  
  snew_sub <- 
    snew |> 
    select(scenario_id, cat, desc, unit, value) |> 
    mutate(value = as.character(value))
  
  #--remove the rows that will be subbed out
  ####### this isn't working?!
  #--I think it is working now
  
  s_stable <- 
    anti_join(s |> select(-value, -scenario_id), snew_sub |> select(-value, -scenario_id)) |> 
    left_join(s) |> 
    mutate(change_ind = "n", 
           scenario_id = paste0("scen_", f_scenario_id))
  
  #--tack on the new values
  s1 <- 
    snew_sub |> 
    mutate(change_ind = "y") |> 
    bind_rows(s_stable) 
  
  s1 |>  
    mutate(notes = NA) |> 
    write_csv(paste0("R/code_autofxns2/datain/scen_", f_scenario_id, ".csv"))
  
  print(paste0("scen_", f_scenario_id, " was written"))
  
}


#--testing

test1 <- tibble(x = c("blue", "red"),
                y = c("blue", "blue"))

test2 <- tibble(x = "blue",
                y = "blue")

anti_join(test1, test2)
