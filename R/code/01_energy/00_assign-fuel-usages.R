# assign fuel usage for types of passes
#--these are 'first stab' values that are put into the assumptions sheet
# created 2/14/2023

rm(list = ls())
library(tidyverse)

source("R/code/00_conversions.R")


# fuel use for a field op -------------------------------------------------

fuel <- 
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

#--what are general cats of operations?

fops <- 
  read_csv("R/data_tidy/prod_fieldops.csv") %>% 
  select(desc) %>% 
  distinct()


hops <- 
  read_csv("R/data_tidy/prod_harvestops.csv") %>% 
  select(desc) %>% 
  distinct()


ops <- 
  fops %>% 
  bind_rows(hops)

ops

#--what does the nrcs have?
fuel %>% 
  select(subcat) %>% 
  distinct()

# chisel ------------------------------------------------------------------

#--move to only include ones w/chisel in the name
fuel %>% 
  filter(grepl("chisel", subsubcat)) %>% 
  filter(grepl("chisel", name)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()

chisel <- 
  fuel %>% 
  filter(grepl("chisel", subsubcat)) %>% 
  filter(grepl("chisel", name)) %>%
  group_by(subsubcat) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) %>% 
  rename(desc = subsubcat)
  

# disc --------------------------------------------------------------------

fuel %>% 
  filter(grepl("disk", subsubcat)) %>% 
  filter(grepl("disk", name)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()


disk <- 
  fuel %>% 
  filter(grepl("disk", subsubcat)) %>% 
  filter(grepl("disk", name)) %>% 
  group_by(subsubcat) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) %>% 
  rename(desc = subsubcat)


# disc border ridges ------------------------------------------------------

disk_border <- 
  disk %>% 
  mutate(desc = "disk border ridges")

# laser level ---------------------------------------------------------------
ops


fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  mutate(clr = ifelse(name == "laser land leveler", "Y", "N")) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point(aes(color = clr), show.legend = F, size = 2) +
  scale_color_manual(values = c("black", "red")) +
  coord_flip()


laser <- 
  fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  filter(name == "laser land leveler") %>% 
  mutate(desc = "laser level") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) 

  
# plant -------------------------------------------------------------------

fuel %>% 
  filter(grepl("drill", subcat)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()

  
fuel %>% 
  filter(grepl("planter|drill", subcat))  %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()

plant <- 
  fuel %>% 
  filter(grepl("planter|drill", subcat))  %>% 
  mutate(desc = "plant") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) 


# roll --------------------------------------------------------------------
ops

fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  filter(grepl("roll", name)) %>% 
  mutate(clr = ifelse(name == "roller, smooth", "Y", "N")) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point(aes(color = clr), show.legend = F, size = 2) +
  scale_color_manual(values = c("black", "red")) +
  coord_flip()

roll <- 
  fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  filter(grepl("roll", name)) %>% 
  mutate(desc = "roll") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = mean(diesel_Lha, na.rm = T)) 



# stand termination -------------------------------------------------------

# weed control -------------------------------------------------------


# haylage, cut -------------------------------------------------------

# haylage, chop -------------------------------------------------------

# hay, swath -------------------------------------------------------
# hay, rake -------------------------------------------------------
# hay, bale -------------------------------------------------------
# hay, stack -------------------------------------------------------
