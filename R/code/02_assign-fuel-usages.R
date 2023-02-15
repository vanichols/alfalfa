# assign fuel usage for types of passes
# created 2/14/2023

rm(list = ls())
library(tidyverse)
library(readxl)

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

read_csv("R/data_tidy/lca_fieldops.csv") %>% 
  select(flow_desc) %>% 
  distinct()

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



# disc --------------------------------------------------------------------

fuel %>% 
  filter(grepl("disk", subsubcat)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()



# disc border ridges ------------------------------------------------------


# laser level ---------------------------------------------------------------

fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  mutate(clr = ifelse(name == "laser land leveler", "Y", "N")) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point(aes(color = clr), show.legend = F, size = 2) +
  scale_color_manual(values = c("black", "red")) +
  coord_flip()


# plant -------------------------------------------------------------------
fuel %>% 
  filter(grepl("drill", subcat)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()

  
fuel %>% 
  filter(grepl("planter", subcat))  %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()

# roll --------------------------------------------------------------------

fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  mutate(clr = ifelse(name == "roller, smooth", "Y", "N")) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point(aes(color = clr), show.legend = F, size = 2) +
  scale_color_manual(values = c("black", "red")) +
  coord_flip()



# stand termination -------------------------------------------------------




#--find a good value for each

fuel %>% 
  select(subcat) %>% 
  distinct()


# disk --------------------------------------------------------------------


library(plotly)

fuel %>% 
  filter(grepl("disk", subsubcat)) %>% 
  ggplot(aes(subsubcat, diesel_Lha)) + 
  geom_jitter(aes(color = name), show.legend = F)

ggplotly()
  
tmp <- 
  fuel %>% 
  filter(grepl("cultivator", subcat))
