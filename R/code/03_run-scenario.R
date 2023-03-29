#--use monster functions to run different scenarios
#--3/29 reran all scenarios bc I fixed the fertilizer energy calculation

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


#--siskiyou
#suppressMessages(RunScenario(my_scenario_id = "1001"))
