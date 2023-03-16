#--create scen_000x files with different variations of a given production scenario


rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code_autofxns2/00_funs/fxn_MakeScenarioCSV.R")

#--first make change manually in scenario log in datain folder
#--the function translates that change into the file formats the other functions need
#--it writes the data to 'datain'

create_this_scenario_id <- "0004"

MakeScenarioCSV(f_scenario_id = create_this_scenario_id)
