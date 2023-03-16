#--create scen_000x files with different variations of a given production scenario
#--using scen_0001 as base file to change
#--3/16 combined assump file and prod file, need to change code

rm(list = ls())
library(tidyverse)
library(readxl)

########## haven't updated

#--first make change in the variant log
#--this translates that change into the file formats the other functions need


MakeVariant <- function(new_id = "0002"){
  
  
  v <- read_csv("R/code_autofxns/man_variant_log.csv", skip = 5)
  source("R/code_auto/00_funs.R")
  
  vnew <- 
    v |> 
    filter(scenario == paste0("v_", new_id)) 
  
  #--is an assumption or production thing that is changing? (a or p)
  v_changename <- vnew |> pull(data_change)
  v_changetype <- v_changename |> str_sub(1, 1)
  
  
  #--read in base file of whatever is changing
  
  if(v_changetype == "a") {
  
    b <- 
      read_csv(paste0("R/code_autofxns/datain/", v_changetype, "0001.csv"), skip = 5) |> 
      fun_preproc_assum()
    
      
    b1 <- 
      b |> 
      mutate(assump_id = paste0("a", new_id)) |> 
      #--make change
      mutate(assump_value = ifelse(
        (assump_cat == vnew$cat &
           assump_desc == vnew$desc &
           assump_unit == vnew$unit), vnew$value, assump_value))
    
  
  } else {
    
    b <- 
      read_csv(paste0("R/code_autofxns/datain/", v_changetype, "0001.csv"), skip = 5) |> 
      fun_preproc_prod()
    
    b1 <- 
      b |> 
     mutate(production_id = paste0("p", new_id)) |> 
      #--make change
      mutate(value = ifelse(
        (cat == vnew$cat &
          desc == vnew$desc &
          unit == vnew$unit), vnew$value, value))
    
  }
    
  b1 |>  
    mutate(notes = NA) |> 
    write_csv(paste0("R/code_autofxns/datain/", v_changename, ".csv"))
  
  print(paste(v_changename, "was written"))
  
}

