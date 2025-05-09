#--use monster functions to run different scenarios
#--3/29 reran all scenarios bc I fixed the fertilizer energy calculation
#--3/30 reran all scenarios bc I fixed the seed energy calculation
#--4/27 reran all sceanrios bc I fixed the fertilizer application fuel use
#--4/27 ran imperial, pesticides are not correct
#--5/28/24 ran tulare, issues with matching field fertility desc and fert amounts, (map1, fixed?)

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_funs/fxn_conversions.R")
source("R/code/00_funs/fxn_ProcDataIn.R")


#--the scenario you want to run

source("R/code/00_funs/fxn_RunScenario.R")
 

# must get seed energy written --------------------------------------------
#--imperial valley produces a lot of the non dormant seed
suppressMessages(RunScenario(my_scenario_id = "2001"))

# tulare ------------------------------------------------------------------

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
suppressMessages(RunScenario(my_scenario_id = "0019"))



# siskiyou ----------------------------------------------------------------

suppressMessages(RunScenario(my_scenario_id = "1001"))
suppressMessages(RunScenario(my_scenario_id = "1002"))
suppressMessages(RunScenario(my_scenario_id = "1003"))
#suppressMessages(RunScenario(my_scenario_id = "1004")) #--doesn't exist

suppressMessages(RunScenario(my_scenario_id = "1005"))
suppressMessages(RunScenario(my_scenario_id = "1006"))
suppressMessages(RunScenario(my_scenario_id = "1007"))
suppressMessages(RunScenario(my_scenario_id = "1008"))
suppressMessages(RunScenario(my_scenario_id = "1009"))

suppressMessages(RunScenario(my_scenario_id = "1010"))
#suppressMessages(RunScenario(my_scenario_id = "1011")) #--doesn't exist, 11-14
#suppressMessages(RunScenario(my_scenario_id = "1012"))
#suppressMessages(RunScenario(my_scenario_id = "1013"))
#suppressMessages(RunScenario(my_scenario_id = "1014"))
suppressMessages(RunScenario(my_scenario_id = "1015"))
suppressMessages(RunScenario(my_scenario_id = "1016"))
suppressMessages(RunScenario(my_scenario_id = "1017"))
suppressMessages(RunScenario(my_scenario_id = "1018"))
suppressMessages(RunScenario(my_scenario_id = "1019"))



# imperial ----------------------------------------------------------------

#suppressMessages(RunScenario(my_scenario_id = "2001")) #--run first to get seed energy
suppressMessages(RunScenario(my_scenario_id = "2002"))
suppressMessages(RunScenario(my_scenario_id = "2005"))
suppressMessages(RunScenario(my_scenario_id = "2008"))
suppressMessages(RunScenario(my_scenario_id = "2009"))

suppressMessages(RunScenario(my_scenario_id = "2010"))
suppressMessages(RunScenario(my_scenario_id = "2015"))
suppressMessages(RunScenario(my_scenario_id = "2016"))
suppressMessages(RunScenario(my_scenario_id = "2017"))
suppressMessages(RunScenario(my_scenario_id = "2018"))
