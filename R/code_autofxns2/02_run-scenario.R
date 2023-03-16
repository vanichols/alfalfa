#--use monster functions to run different scenarios

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code_autofxns2/00_funs/fxn_conversions.R")
source("R/code_autofxns2/00_funs/fxn_ProcDataIn.R")


#--the scenario you want to run
my_scenario_id <- "0001"


# 1. process production data ----------------------------------------------

source("R/code_autofxns2/00_funs/fxn_ProcProdData.R")

my_prod_data <- ProcProdData(f_scenario_id = my_scenario_id)



# 2. calculate energy -----------------------------------------------------


source("R/code_autofxns2/00_funs/fxn_CalcEnergyUse.R")

my_energy_data <- CalcEnergyUse(f_scenario_id = my_scenario_id, f_prod_data = my_prod_data)


# 3. calculate ghg -----------------------------------------------------


source("R/code_autofxns2/00_funs/fxn_CalcGHG.R")

my_ghg_data <- CalcGHG(f_scenario_id = my_scenario_id, f_prod_data = my_prod_data, f_energy_data = my_energy_data)


# combine and write -------------------------------------------------------

a <- 
  my_energy_data |> 
  bind_rows(my_ghg_data) 

a |> 
  write_csv(paste0("R/code_autofxns2/dataout/scen_", my_scenario_id, "-res.csv"))
