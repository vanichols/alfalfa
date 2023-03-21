#--use monster functions to run different scenarios

library(tidyverse)
library(readxl)
library(pals)

#--the scenario you want to run

RunScenario <- function(my_scenario_id = "0012") {
  
  # 1. process production data ----------------------------------------------
  
  source("R/code/00_funs/fxn_ProcProdData.R")
  
  my_prod_data <- ProcProdData(f_scenario_id = my_scenario_id)
  
  
  
  # 2. calculate energy -----------------------------------------------------
  
  
  source("R/code/00_funs/fxn_CalcEnergyUse.R")
  
  my_energy_data <-
    CalcEnergyUse(f_scenario_id = my_scenario_id, f_prod_data = my_prod_data)
  
  
  # 3. calculate ghg -----------------------------------------------------
  
  
  source("R/code/00_funs/fxn_CalcGHG.R")
  
  my_ghg_data <-
    CalcGHG(
      f_scenario_id = my_scenario_id,
      f_prod_data = my_prod_data,
      f_energy_data = my_energy_data
    )
  
  
  # combine -------------------------------------------------------
  
  a <-
    my_energy_data |>
    bind_rows(my_ghg_data)
  

# write results -----------------------------------------------------------

  a |>
    write_csv(paste0(
      "R/data_out/scen_",
      my_scenario_id,
      "-res.csv"
    ))
  
  print(paste(my_scenario_id, "results were written!"))
  
}
