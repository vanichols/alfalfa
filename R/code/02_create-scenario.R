#--create scen_000x files with different variations of a given production scenario


rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_funs/fxn_MakeScenarioCSV.R")

#--first make change manually in scenario log in data_in folder
#--the function translates that change into the file formats the other functions need
#--it writes the data to 'data_scen'

#--note scen_0000 is NOT meant to be run - it only serves as a base of values

MakeScenarioCSV(f_scenario_id = "0010")
