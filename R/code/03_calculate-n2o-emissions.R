#--think about processing lca sheet

library(tidyverse)
library(readxl)
library(measurements)


source("R/code/00_conversions.R")


# clean up function -------------------------------------------------------


fun_preproc <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(system, .direction = c("down")) %>% 
    fill(flow_type, .direction = c("down")) %>% 
    fill(flow_cat, .direction = c("down")) %>% 
    select(-notes) %>% 
    pivot_longer(mid:worst) 
    
  return(tmp)
  }





# n2o ---------------------------------------------------------------------



n2o <- read_excel("R/data_raw/lca-sheets/enterprise-flows-all-areas.xlsx",
                   sheet = "soil_emissions", 
                   skip = 5)

n2o1 <- 
  fun_preproc(data = n2o) 

#--need to utilize fertilizer info for this calc




# ipcc --------------------------------------------------------------------



# use paper to check calcs ------------------------------------------------

# https://link.springer.com/article/10.1007/s10705-016-9808-8
# The current IPCC Tier 1 emission factor estimates N2O emissions as one percent 
# of N inputs, which are accounted for as above- and belowground residues 
# during forage crop renewal (plowing under of an existing forage crop). 
# To calculate annual N inputs in perennial legume systems, 
# the residue N during crop renewal is divided by the number of years 
# of continuous cultivation (IPCC 2006). 

# A1 = DM produced annually (kg DM/ha)
# A2 = N content of aboveground biomass (kg N/ kgDM)
# A3 = fraction of aboveground biomass not harvested (defaults to 0.1)
A3 <- 0.1
# B1 = default factor estimating root dry matter as a function of annual 
#  aboveground biomass; 0.4
B1 <- 0.4
# B2 = default N fraction of alfalfa roots (kg/kg DM); 0.019
B2 <- 0.019

# C1 = renewal rate per year (1/stand life)
C1 <- 1/4

# inorganic N mineralized (kg N/ha) = [A1*A2*A3 + A1*B1*B2]*C1
# Nfert must also be added
# numbers from paper as example:
Nfert <- 18


# 2nd year stand = 58 kg N/ha
A1_2yr <- 14100
A2_2yr <- 525/A1_2yr
Ninputs_2yr <- (A1_2yr * A2_2yr * A3 + A1_2yr * B1 * B2) * C1 + Nfert
ipcc_2yr <- 0.01 * Ninputs_2yr #kg N2O-N / ha / yr

# 5th year stand = 53 kg N/ha
A1_5yr <- 12100
A2_5yr <- 473/A1_5yr
Ninputs_5yr <- (A1_5yr * A2_5yr * A3 + A1_5yr * B1 * B2) * C1 + Nfert
ipcc_5yr <- 0.01 * Ninputs_5yr #kg N2O-N / ha / yr

