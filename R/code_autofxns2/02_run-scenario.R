#--use monster functions to run different scenarios

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code_autofxns2/00_funs/fxn_conversions.R")
source("R/code_autofxns2/00_funs/fxn_ProcDataIn.R")


#--the scenario you want to run

source("R/code_autofxns2/00_funs/fxn_RunScenario.R")

RunScenario(my_scenario_id = "0001")
RunScenario(my_scenario_id = "0002")
RunScenario(my_scenario_id = "0003")
RunScenario(my_scenario_id = "0004")
