#--create scen_000x files with different variations of a given production scenario
#--using scen_0001 as base file to change
#--3/16 combined assump file and prod file, need to change code
#--3/17 pulled out field ops, pesticide app amts, and field ops fuel usage, need to fix code

rm(list = ls())
library(tidyverse)

#--first, manually enter what you what into the scenario log in datain folder
#--this fxn translates that change into the file formats the other functions need


MakeScenarioCSV <- function(f_scenario_id = "0001"){
  
  
  source("R/code/00_funs/fxn_ProcDataIn.R")
  source("R/code/00_funs/fxn_conversions.R")
  
  #--first thing you do is piece together the entire 'base' file
  #--combine field ops, pesticide applications, and other
  #--if the values are NOT overwritten, it uses these base values
  base_fops <- read_csv("R/data_in/scen_fieldops-base.csv")
  base_pest <- read_csv("R/data_in/scen_pests-base.csv")
  base_other <- read_csv("R/data_in/scen_other-base.csv", skip = 5) |> 
    ProcDataIn()
  
  
  #--read in changes in other, if they exist
  n_other <- 
    read_csv("R/data_in/scenbyhand-other.csv", skip = 5) |> 
    ProcDataIn() |> 
    filter(scenario_id == paste0("scen_", f_scenario_id))
  
  #--if there were no changes, just use base
  #--if there were, overwrite the base w/those changes
  if (nrow(n_other) < 1) {n_other_new <- base_other |> mutate(scenario_id = paste0("scen_", f_scenario_id))} else {
    
    #--separate variables that will and will not change
    n_other_sub <- 
      n_other |> 
      select(scenario_id, cat, desc, unit, value) |> 
      mutate(value = as.character(value))
    
    #--remove the rows that will be subbed out
    #--I think it is working now
    
    n_other_stable <- 
      anti_join(base_other |> select(-value, -scenario_id), 
                n_other_sub |> select(-value, -scenario_id)) |> 
      left_join(base_other) |> 
      mutate(change_ind = "n", 
             scenario_id = paste0("scen_", f_scenario_id))
    
    #--tack on the new values
    n_other_new <- 
      n_other_sub |> 
      mutate(change_ind = "y") |> 
      bind_rows(n_other_stable) 
    
    
    
  }
  
  
  #--run the code that transforms the scenbyhand into clean files for this fxn
  source("R/code/01_data-prep/01_field-ops.R")
  n_fops <- read_csv("R/data_in/scen_fieldops.csv") |>
    filter(scenario_id == paste0("scen_", f_scenario_id))
  
  if (nrow(n_fops)  < 1) {
    n_fops_new <-
      base_fops |> mutate(scenario_id = paste0("scen_", f_scenario_id),
                          change_ind = "n")
  }
  else {
    n_fops_new <- n_fops |> mutate(change_ind = "y")
  }
  
  source("R/code/01_data-prep/02_pest.R")
  n_pest <- read_csv("R/data_in/scen_pests.csv") |>
    filter(scenario_id == paste0("scen_", f_scenario_id))
  
  if (nrow(n_pest) < 1) {
    n_pest_new <-
      base_pest |> mutate(scenario_id = paste0("scen_", f_scenario_id),
                          change_ind = "n")
  }
  else {
    n_pest_new <- n_pest |> mutate(change_ind = "y")
  }
  
  s <- 
    n_fops_new |> 
    mutate(value = as.character(value)) |> 
    bind_rows(n_pest_new |> mutate(value = as.character(value))) |> 
    bind_rows(n_other_new |> mutate(value = as.character(value)))
  
  
  #--add on short description
  s_desc <- 
    read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5) |> 
    select(scenario_id, scen_desc) |> 
    filter(!is.na(scenario_id)) |> 
    rename(notes = scen_desc)
  
  s |>  
    left_join(s_desc) |> 
    write_csv(paste0("R/data_scens/scen_", f_scenario_id, ".csv"))
  
  print(paste0("scen_", f_scenario_id, " was written"))
  
}


#--testing

test1 <- tibble(x = c("blue", "red"),
                y = c("blue", "blue"))

test2 <- tibble(x = "blue",
                y = "blue")

anti_join(test1, test2)
