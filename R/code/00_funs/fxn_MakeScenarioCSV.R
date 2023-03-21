#--create scen_000x files with different variations of a given production scenario
#--using scen_0001 as base file to change
#--3/16 combined assump file and prod file, need to change code
#--3/17 pulled out field ops, pesticide app amts, and field ops fuel usage, need to fix code

rm(list = ls())
library(tidyverse)

#--first, manually enter what you what into the scenario log in datain folder
#--this fxn translates that change into the file formats the other functions need


MakeScenarioCSV <- function(f_scenario_id = "0012"){
  
  
  source("R/code/00_funs/fxn_ProcDataIn.R")
  source("R/code/00_funs/fxn_conversions.R")
  source("R/code/00_funs/fxn_ProcFops.R")
  source("R/code/00_funs/fxn_ProcPest.R")
  
  #--first thing you do is piece together the entire 'base' file
  #--combine field ops, pesticide applications, and other
  #--if the values are NOT overwritten, it uses these base values
  base_fops <- read_csv("R/data_in/base_fieldops.csv")
  base_pest <- read_csv("R/data_in/base_pests.csv")
  base_other <- read_csv("R/data_in/base_other.csv", skip = 5) |> 
    ProcDataIn()
  

# other -------------------------------------------------------------------

  
  #--read in changes in other, if they exist
  otherTF <- file.exists(paste0("R/data_in/byhand_other/other_scen-", f_scenario_id, ".csv"))
  
  if (otherTF == 1) {
    #--if there is a new file, use it
    n_other <-
      read_csv(paste0(
        "R/data_in/byhand_other/other_scen-",
        f_scenario_id,
        ".csv"),
        skip = 5
      ) |> 
      ProcDataIn()
    
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
    
  } else {
    #--if there were no changes, just use base
    n_other_new <-
      base_other |> mutate(scenario_id = paste0("scen_", f_scenario_id),
                           change_ind = "n")
  }
  


# fieldops ----------------------------------------------------------------

  #--read in changes in other, if they exist
  fopsTF <- file.exists((paste0("R/data_in/byhand-fieldops/fops_scen-", f_scenario_id, ".csv")))
  
  if (fopsTF == TRUE) {
  
    #--run fxn that summarises data
    n_fops_raw <- read_csv(paste0("R/data_in/byhand-fieldops/fops_scen-", f_scenario_id, ".csv"), skip = 5)
    
    n_fops <- ProcFops(f_scenario_id)
    
    n_fops_new <- n_fops |> mutate(change_ind = "y")
    
  } else {
    
    n_fops_new <-
      base_fops |> mutate(scenario_id = paste0("scen_", f_scenario_id),
                          change_ind = "n")
    
  }
  

# pests -------------------------------------------------------------------

  #--read in changes in other, if they exist
  pestTF <- file.exists((paste0("R/data_in/byhand-pests/pest_scen-", f_scenario_id, ".csv")))
  
  if (pestTF == TRUE) {
    
    #--run fxn that summarises data
    n_pest_raw <- read_csv(paste0("R/data_in/byhand-pests/pest_scen-", f_scenario_id, ".csv"), skip = 5)
    
    n_pest <- ProcPest(f_scenario_id)
    
    n_pest_new <- n_pest |> mutate(change_ind = "y")
    
  } else {
    
    n_pest_new <-
      base_pest |> mutate(scenario_id = paste0("scen_", f_scenario_id),
                          change_ind = "n")
    
  }
  
  

# combine fops, pest, other -----------------------------------------------


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
    write_csv(paste0("R/data_scens-notouch/scen_", f_scenario_id, ".csv"))
  
  print(paste0("scen_", f_scenario_id, " was written"))
  
}


#--testing

test1 <- tibble(x = c("blue", "red"),
                y = c("blue", "blue"))

test2 <- tibble(x = "blue",
                y = "blue")

anti_join(test1, test2)
