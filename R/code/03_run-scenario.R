#--use monster functions to run different scenarios

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_funs/fxn_conversions.R")
source("R/code/00_funs/fxn_ProcDataIn.R")


#--the scenario you want to run

source("R/code/00_funs/fxn_RunScenario.R")
 
# RunScenario(my_scenario_id = "0001")
# RunScenario(my_scenario_id = "0002")
# RunScenario(my_scenario_id = "0003")
# RunScenario(my_scenario_id = "0004")
# RunScenario(my_scenario_id = "0005")
# RunScenario(my_scenario_id = "0006")
# RunScenario(my_scenario_id = "0007")
# RunScenario(my_scenario_id = "0008")
# RunScenario(my_scenario_id = "0009")
# RunScenario(my_scenario_id = "0010")
# RunScenario(my_scenario_id = "0011")
RunScenario(my_scenario_id = "0012")
RunScenario(my_scenario_id = "0013")
