#--create scen_000x files with different variations of a given production scenario
#--5/12/23 - updated base files w/new pump/irrigation/well info
#--tulare done
#--note things will need to be rerun based on new irrigation pump table info (5/22)

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_funs/fxn_MakeScenarioCSV.R")

#--first make change manually in scenario log in data_in folder
#--the function translates that change into the file formats the other functions need
#--it writes the data to 'data_scen'

#--note scen_0000 is NOT meant to be run - it only serves as a base of values

# tulare ------------------------------------------------------------------

MakeScenarioCSV(f_scenario_id = "0001", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0002", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0003", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0004", f_county = "tulare")

MakeScenarioCSV(f_scenario_id = "0005", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0006", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0007", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0008", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0009", f_county = "tulare")

MakeScenarioCSV(f_scenario_id = "0010", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0011", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0012", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0013", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0014", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0015", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0016", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0017", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0018", f_county = "tulare")
MakeScenarioCSV(f_scenario_id = "0019", f_county = "tulare")


# siskiyou ----------------------------------------------------------------

MakeScenarioCSV(f_scenario_id = "1001", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1002", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1003", f_county = "siskiyou")
#MakeScenarioCSV(f_scenario_id = "1004", f_county = "siskiyou") #--doesn't exist, to keep alignment w/tulare

MakeScenarioCSV(f_scenario_id = "1005", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1006", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1007", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1008", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1009", f_county = "siskiyou")

MakeScenarioCSV(f_scenario_id = "1010", f_county = "siskiyou")
#MakeScenarioCSV(f_scenario_id = "1011", f_county = "siskiyou")
#MakeScenarioCSV(f_scenario_id = "1012", f_county = "siskiyou")
#MakeScenarioCSV(f_scenario_id = "1013", f_county = "siskiyou")
#MakeScenarioCSV(f_scenario_id = "1014", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1015", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1016", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1017", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1018", f_county = "siskiyou")
MakeScenarioCSV(f_scenario_id = "1019", f_county = "siskiyou")


# imperial ----------------------------------------------------------------

MakeScenarioCSV(f_scenario_id = "2001", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2002", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2005", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2008", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2009", f_county = "imperial")

MakeScenarioCSV(f_scenario_id = "2010", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2015", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2016", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2017", f_county = "imperial")
MakeScenarioCSV(f_scenario_id = "2018", f_county = "imperial")
