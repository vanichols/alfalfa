#--create scen_000x files with different variations of a given production scenario


rm(list = ls())
library(tidyverse)
library(readxl)

###############----needs updating

source("R/code_autofxns/fxn_MakeVariant.R")

#--first make change in the variant log
#--the function translates that change into the file formats the other functions need
#--it writes the data to 'datain'

uni_variant_id <- "0002"

# 1. make variant data ----------------------------------------------------

MakeVariant(new_id = uni_variant_id)


# 2. run proc prod data ---------------------------------------------------

source("R/code_autofxns/fxn_ProcProdData.R")

d_prod <- ProcProdData(variant_id = uni_variant_id)


# 3.calc energy use  -----------------------------------------------------

source("R/code_autofxns/fxn_CalcEnergyUse.R")

d_energy <- CalcEnergyUse(variant_id = uni_variant_id, 
                          my_prod_data = d_prod)

d_energy |> 
  write_csv(paste0("R/code_autofxns/dataout/", uni_variant_id, "-energy.csv"))


# 4. run proc ghg ---------------------------------------------------------


