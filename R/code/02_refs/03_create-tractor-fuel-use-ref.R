# assign fuel usage for types of passes
#--these are 'first stab' values that are put into the assumptions sheet
# created 2/14/2023
#--3/1 updated names to be more intuitive. These values get copy-pasted into the baseline assumptions file


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

# 1. chisel ------------------------------------------------------------------

#--move to only include ones w/chisel in the name
fuel %>% 
  filter(grepl("chisel", subsubcat)) %>% 
  filter(grepl("chisel", name)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()

f1 <- 
  fuel %>% 
  filter(grepl("chisel", subsubcat)) %>% 
  filter(grepl("chisel", name)) %>%
  group_by(subsubcat) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) %>% 
  rename(desc = subsubcat)
  

# 2. disc --------------------------------------------------------------------

fuel %>% 
  filter(grepl("disk", subsubcat)) %>% 
  filter(grepl("disk", name)) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point() +
  coord_flip()


f2 <- 
  fuel %>% 
  filter(grepl("disk", subsubcat)) %>% 
  filter(grepl("disk", name)) %>% 
  group_by(subsubcat) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) %>% 
  rename(desc = subsubcat)


# 3. disc border ridges ------------------------------------------------------

f3 <- 
  f2 %>% 
  mutate(desc = "disk border ridges")

# 4. laser level ---------------------------------------------------------------

fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  mutate(clr = ifelse(name == "laser land leveler", "Y", "N")) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point(aes(color = clr), show.legend = F, size = 2) +
  scale_color_manual(values = c("black", "red")) +
  coord_flip()


f4 <- 
  fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  filter(name == "laser land leveler") %>% 
  mutate(desc = "laser level") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) 

  
# 5. plant -------------------------------------------------------------------

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

f5 <- 
  fuel %>% 
  filter(grepl("planter|drill", subcat))  %>% 
  mutate(desc = "plant") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = median(diesel_Lha, na.rm = T)) 


# 6. roll --------------------------------------------------------------------

fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  filter(grepl("roll", name)) %>% 
  mutate(clr = ifelse(name == "roller, smooth", "Y", "N")) %>% 
  ggplot(aes(reorder(name, diesel_Lha), diesel_Lha)) + 
  geom_point(aes(color = clr), show.legend = F, size = 2) +
  scale_color_manual(values = c("black", "red")) +
  coord_flip()

f6 <- 
  fuel %>% 
  filter(grepl("surface", subcat)) %>% 
  filter(grepl("roll", name)) %>% 
  mutate(desc = "roll") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = mean(diesel_Lha, na.rm = T)) 



# 7. stand termination -------------------------------------------------------

#--assume a disking?
f7 <- 
  f2 %>% 
  mutate(desc = "stand termination")

# 8. weed control -------------------------------------------------------

f8 <- 
  fuel %>% 
  filter(grepl("sprayer", name)) %>% 
  mutate(desc = "weed control") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = mean(diesel_Lha, na.rm = T)) 


# 9. insect control ----------------------------------------------------------

f9 <- 
  f8 %>% 
  mutate(desc = "insect control")

# 10. fertilize map -----------------------------------------------------------

#--assume map fertilizer is sprayed on
f10 <- 
  f8 %>% 
  mutate(desc = "fertilize, map")


#--surface broadcast might be good for manure
fuel %>% 
  filter(grepl("fert applic.", name))

# 11. haylage, cut -------------------------------------------------------

f11 <- 
  fuel %>% 
  filter(grepl("mow", name)) %>% 
  filter(subcat == "manage") %>% 
  mutate(desc = "haylage, cut") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = mean(diesel_Lha, na.rm = T)) 


# 12. haylage, chop -------------------------------------------------------

f12 <- 
  fuel %>% 
  filter(grepl("forage chopper", name)) %>% 
  mutate(desc = "haylage, chop") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = mean(diesel_Lha, na.rm = T)) 

# 13. hay, swath -------------------------------------------------------

f13 <- 
  f12 %>% 
  mutate(desc = "hay, swath")

# 14. hay, rake -------------------------------------------------------

f14 <- 
  f11 %>% 
  mutate(desc = "hay, rake")

# 15. hay, bale -------------------------------------------------------

f15 <- 
  fuel %>% 
  filter(grepl("bale", name)) %>% 
  mutate(desc = "hay, bale") %>% 
  group_by(desc) %>% 
  summarise(diesel_Lha = mean(diesel_Lha, na.rm = T)) 

# 16. hay, stack -------------------------------------------------------

#--the 'total' for harvesting is 14.7 in the fuel list
#--so far we have 10.09
#--assume a remove residue pass

f16 <- 
  f12 %>% 
  mutate(desc = "hay, stack")

# put together ------------------------------------------------------------

fuel_list <- NULL


#--change this number if you add more operations
for (i in 1:16){
  fuel_list <- c(fuel_list, paste0("f", i))
  }

fuel_list


dat <- NULL

for (i in 1:length(fuel_list)){

    tmp <- get(fuel_list[i])
    dat <- bind_rows(dat, tmp)  
}

dat %>% 
  write_csv("R/data_refs/ref_ops-fuel-usage.csv")
