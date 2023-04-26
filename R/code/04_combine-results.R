#--combine all results into a monster file
#--convert things into different carbon sequestration scenarios
#--created 4/5

rm(list = ls())
library(tidyverse)

# monster file maker ------------------------------------------------------

d_raw0 <- 
  list.files(path = "R/data_out/", pattern = ".csv", full.names = T) |> 
  map_df(read_csv) 

d_raw <- 
  d_raw0 |> 
  mutate(unit = ifelse(unit == "MJ_kgyield", "GJ_Mgyield", unit))

d_raw |> 
  filter(scenario_id != "scen_0000") |> 
  write_csv("R/data_tidy/scen_all.csv")


# process it --------------------------------------------------------------


s_desc <- 
  read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5) |> 
  fill(scenario_id, scen_desc) |> 
  select(scenario_id, scen_desc, location) |> 
  distinct()

d <- 
  d_raw |> 
  filter(scenario_id != "scen_0000") |> 
  left_join(s_desc)


# wrangle c credit scenarios ----------------------------------------------

cc <- read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx", skip = 5)

# get total values --------------------------------------------------------

d_tot <- 
  d |> 
  group_by(scenario_id, scen_desc, location, unit) |> 
  summarise(value = sum(value))

#--energy, unaffected by c credit scenario
d_tot_en <- 
  d_tot |> 
  filter(grepl("GJ|MJ", unit))

d_tot_ghg <- 
  d_tot |> 
  filter(grepl("kgco2e", unit))


# change cc ---------------------------------------------------------------

#--separate the ones that change the carbon credit
d2_nocc <- 
  d_tot_ghg |> 
  filter(!grepl("credit", scen_desc))

d2_cc <- 
  d_tot_ghg |> 
  filter(grepl("credit", scen_desc))

d2_cc_wide <- 
  d2_cc |> 
  ungroup() |> 
  select(-scenario_id) |> 
  pivot_wider(names_from = scen_desc, values_from = value) |> 
  janitor::clean_names()

dcc <- 
  d2_nocc |> 
  filter(scen_desc == "base") |> 
  rename("conservation_crop_rotation_c_credit" = value) |> 
  select(-scenario_id) |> 
  left_join(d2_cc_wide) 

cc_conv <- 
  dcc |> 
  ungroup() |> 
  mutate(cons_crop_to_pasture = 
           -(conservation_crop_rotation_c_credit - pasture_establishment_c_credit),
         cons_crop_to_nocredit = 
           -(conservation_crop_rotation_c_credit - no_carbon_credit)) |> 
  select(location, unit, cons_crop_to_pasture, cons_crop_to_nocredit)


# scenarios w/diff cc -----------------------------------------------------

cc_scen <- 
  d2_nocc |> 
  rename("conservation_crop_rotation_c_credit" = value) |> 
  left_join(cc_conv) |> 
  mutate(pasture_establishment_c_credit = 
           conservation_crop_rotation_c_credit + cons_crop_to_pasture,
         no_c_credit = 
           conservation_crop_rotation_c_credit + cons_crop_to_nocredit) |> 
  select(-cons_crop_to_pasture, - cons_crop_to_nocredit) %>% 
  pivot_longer(conservation_crop_rotation_c_credit:ncol(.))




cc_scen |> 
  bind_rows(d_tot_en |> mutate(name = "energy")) |> 
  write_csv("R/data_tidy/scen_all-tot.csv")
