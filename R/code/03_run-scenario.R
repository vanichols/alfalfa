#--use monster functions to run different scenarios
#--3/29 reran all scenarios bc I fixed the fertilizer energy calculation
#--3/30 reran all scenarios bc I fixed the seed energy calculation

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_funs/fxn_conversions.R")
source("R/code/00_funs/fxn_ProcDataIn.R")


#--the scenario you want to run

source("R/code/00_funs/fxn_RunScenario.R")
 

# tulare
suppressMessages(RunScenario(my_scenario_id = "0001"))
suppressMessages(RunScenario(my_scenario_id = "0002"))
suppressMessages(RunScenario(my_scenario_id = "0003"))
suppressMessages(RunScenario(my_scenario_id = "0004"))

suppressMessages(RunScenario(my_scenario_id = "0005"))
suppressMessages(RunScenario(my_scenario_id = "0006"))
suppressMessages(RunScenario(my_scenario_id = "0007"))
suppressMessages(RunScenario(my_scenario_id = "0008"))
suppressMessages(RunScenario(my_scenario_id = "0009"))

suppressMessages(RunScenario(my_scenario_id = "0010"))
suppressMessages(RunScenario(my_scenario_id = "0011"))
suppressMessages(RunScenario(my_scenario_id = "0012"))
suppressMessages(RunScenario(my_scenario_id = "0013"))
suppressMessages(RunScenario(my_scenario_id = "0014"))
suppressMessages(RunScenario(my_scenario_id = "0015"))
suppressMessages(RunScenario(my_scenario_id = "0016"))
suppressMessages(RunScenario(my_scenario_id = "0017"))
suppressMessages(RunScenario(my_scenario_id = "0018"))


#--siskiyou
suppressMessages(RunScenario(my_scenario_id = "1001"))
suppressMessages(RunScenario(my_scenario_id = "1002"))
suppressMessages(RunScenario(my_scenario_id = "1003"))
#suppressMessages(RunScenario(my_scenario_id = "1004"))

suppressMessages(RunScenario(my_scenario_id = "1005"))
suppressMessages(RunScenario(my_scenario_id = "1006"))
suppressMessages(RunScenario(my_scenario_id = "1007"))
suppressMessages(RunScenario(my_scenario_id = "1008"))
suppressMessages(RunScenario(my_scenario_id = "1009"))

suppressMessages(RunScenario(my_scenario_id = "1010"))
#suppressMessages(RunScenario(my_scenario_id = "1011"))
#suppressMessages(RunScenario(my_scenario_id = "1012"))
#suppressMessages(RunScenario(my_scenario_id = "1013"))
#suppressMessages(RunScenario(my_scenario_id = "1014"))
suppressMessages(RunScenario(my_scenario_id = "1015"))
suppressMessages(RunScenario(my_scenario_id = "1016"))
suppressMessages(RunScenario(my_scenario_id = "1017"))
suppressMessages(RunScenario(my_scenario_id = "1018"))
