# assign fuel usage for types of passes
#--these are 'first stab' values that are put into the assumptions sheet
# created 2/14/2023
#--3/1 updated names to be more intuitive. These values get copy-pasted into the baseline assumptions file
#--3/17 update file structure

rm(list = ls())
library(tidyverse)
library(readxl)

source("R/code/00_funs/fxn_conversions.R")


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

f9a <- 
  f8 %>% 
  mutate(desc = "insect control")

f9b <- 
  fuel %>% 
  filter(grepl("aerial", name),
         subcat == "agchem") %>% 
  select(diesel_Lha) %>% 
  mutate(desc = "insect control, aerial")

f9 <- 
  f9a %>% 
  bind_rows(f9b)

# 10. fertilizer -----------------------------------------------------------

#--in the fops data, surface fertilization has different names to keep thigns straight

#--assume map fertilizer is sprayed on
f10a <- 
  f8 %>% 
  mutate(desc = "fertilize, surface")

f10b <- 
  f8 %>% 
  mutate(desc = "fertilize, map1")


f10c <- 
  f8 %>% 
  mutate(desc = "fertilize, est1")


f10d <- 
  f8 %>% 
  mutate(desc = "fertilize, prod1")

#--injected, assume a shank w/15 inch spacing
f10e <- 
  fuel %>% 
  filter(grepl("fert applic.", name),
         grepl("shank low disturb, 15", name)) %>% 
  select(diesel_Lha) %>% 
  mutate(desc = "fertilize, inject")

#--general fertilize
f10f <- 
  f8 %>% 
  mutate(desc = "fertilize")

f10 <- 
  f10a %>% 
  bind_rows(f10b) %>%
  bind_rows(f10c) %>% 
  bind_rows(f10d) %>% 
  bind_rows(f10e) %>% 
  bind_rows(f10f)
  


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


# 17. corrugate ---------------------------------------------------------------

f17 <- 
  fuel %>% 
  filter(grepl("bed shaper, 8 inch beds", name)) %>% 
  select(diesel_Lha) %>% 
  mutate(desc = "corrugate")


# 18. spike --------------------------------------------------------------

#--assume it is harrowing

f18 <- 
  fuel %>% 
  filter(grepl("harrow, coiled tine weeder", name)) %>% 
  select(diesel_Lha) %>% 
  mutate(desc = "spike")



# put together ------------------------------------------------------------

fuel_list <- NULL


#--change this number if you add more operations
for (i in 1:18){
  fuel_list <- c(fuel_list, paste0("f", i))
  }

fuel_list


dat <- NULL

for (i in 1:length(fuel_list)){

    tmp <- get(fuel_list[i])
    dat <- bind_rows(dat, tmp)  
}


# list of generalized field ops and fuel use from NRCS --------------------

dat |> 
  rename(ldiesel_ha = diesel_Lha) |> 
  write_csv("R/data_refs/ref_ops-fuel-usage.csv")
