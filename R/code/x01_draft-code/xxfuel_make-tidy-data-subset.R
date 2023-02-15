# look at FTM's fuel use by operation, from NRCS

library(tidyverse)
library(readxl)


rm(list = ls())

# 3.7 L in a gallon
# 2.47 acres in a ha

dat <- 
  read_excel("ftm_raw/operation-3.9-weps.xlsx") %>% 
  janitor::clean_names() %>% 
  #--note oenergyarea is L diesel/ha
  select(id, op_group1, name, oenergyarea) %>% 
  janitor::remove_empty() %>% 
  mutate(diesel_Lha = as.numeric(oenergyarea),
         diesel_galac = diesel_Lha / 3.7 / 2.47) %>% 
  separate(op_group1, into = c("cat", "subcat", "subsubcat"), sep = ",") %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate_if(is.character, str_trim) %>% 
  filter(diesel_Lha > 0.01) %>% 
  select(-oenergyarea)



# keep only possibly relevant ones ----------------------------------------

d1 <- 
  dat %>% 
  filter(id %in% c(seq(23043, 23091, 1),
                   seq(23098, 23132, 1),
                   seq(22714, 22727, 1),
                   seq(22782, 22786, 1),
                   seq(22799, 22800, 1),
                   seq(22869, 22871, 1),
                   seq(22953, 23000, 1), 
                   22917)) 


# simplify sprayer (all same fuel amounts) --------------------------------

d2 <- 
  d1 %>% 
  filter(grepl("spray", name)) %>% 
  filter(id == 22717) %>% 
  mutate(name = "sprayer") %>% 
  bind_rows(
    d1 %>% 
      filter(!grepl("spray", name))
  )


# simplify harvest --------------------------------------------------------

d3 <- 
  d2 %>% 
  filter(grepl("biomass", subcat)) %>% 
  filter(id %in% c(22784, 22799)) %>% 
  mutate(name = case_when(
    id == 22784 ~ "harvest, hay/silage grass or legume",
    id == 22799 ~ "remove residue, forage chopper",
    TRUE ~ name)
  ) %>% 
  bind_rows(
    d2 %>% 
      filter(!grepl("biomass", subcat))
    )


# shredder simp ----------------------------------------------------------------

d4 <- 
  d3 %>% 
  filter(grepl("shredder", name)) %>% 
  filter(id %in% c(22870)) %>% 
  bind_rows(
    d3 %>% 
      filter(!grepl("shredder", name))
  )


# everything but tillage --------------------------------------------------

d5 <- 
  d4 %>% 
  filter(cat != "tillage") %>% 
  filter(id != 23131)

d5 %>% 
  write_csv("R/data_tidy/operations_non-tillage.csv")


# tillage -----------------------------------------------------------------

d6 <- 
  d4 %>% 
  filter(cat == "tillage")

d6 %>% 
  write_csv("R/data_tidy/operations_tillage.csv")
