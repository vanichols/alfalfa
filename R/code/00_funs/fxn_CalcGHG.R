# calculate ghg
# created 3/16 to handle new single file 
# 3/17 update file names

CalcGHG <- function(f_scenario_id = "1003", 
                    f_prod_data = my_prod_data, 
                    f_energy_data = my_energy_data){
  
  source("R/code/00_funs/fxn_conversions.R")
  source("R/code/00_funs/fxn_ProcDataIn.R")
  
# references --------------------------------------------------------------

r_carbon <- 
  read_excel("R/data_refs/refbyhand_california-healthy-soils.xlsx",
                 skip = 5) %>% 
  fill(unit) |> 
  mutate_if(is.character, str_to_lower)

#--co2 released per unit fuel used
r_fuelghg <- 
  read_csv("R/data_refs/ref_fuel_ghg.csv") |> 
  mutate_if(is.character, str_to_lower) 

#--energy contents
r_fuele <- 
  read_csv("R/data_refs/ref_fuel-energy.csv") 

#--ghg gwp for each time horizon
r_ghg <- 
  read_excel("R/data_refs/refbyhand_gwp.xlsx", skip = 5) 

#--fert categories (urea, ammonium, etc)
r_fertcat <- 
  read_csv("R/data_refs/refbyhand_fert-category.csv", skip = 5) |> 
  select(-notes)

#--amount of n in each type of fertilizer
r_fertn <- 
  read_csv("R/data_refs/ref_fert-n-contents.csv") |> 
  rename(desc = fert_type) |> 
  mutate(kgn_kgfert = value) |> 
  select(desc, kgn_kgfert)



# non production data (other) ---------------------------------------------

d_o <- 
  read_csv(paste0("R/data_scens-notouch/scen_", f_scenario_id, ".csv"))  |> 
  ProcDataIn()
  
#--what timespan for gwp
o_gwp <- 
  d_o |> 
  filter(cat == "gwp") |> 
  pull(value)

o_gwpn2o <- 
  r_ghg |> 
  filter(time_horizon == o_gwp) |> 
  filter(molecule == "n2o") |> 
  pull(global_warming_potential)



# production and energy data --------------------------------------------------------------------

d_p <- f_prod_data

#--keep only MJ/stand
d_e <- 
  f_energy_data |> 
  filter(unit == "GJ_stand") |> 
  mutate(value = value * 1000,
         unit = "MJ_stand")

#--stand life
p_sl <- 
  d_p |> 
  filter(cat == "yield",
         desc == "stand life") |> 
  mutate(value = as.numeric(value)) |> 
  rename("stand_life_yrs" = value) |> 
  select(scenario_id, stand_life_yrs)

# 1. carbon credits (c) ---------------------------------------------------

#--using california healthy soils reference and assumptions, assign carbon credits
#--note they already converted to co2e using their own conversions...

#--get assumed county and practice scenario
c_scen <- 
  d_o %>% 
  filter(cat == "carbon credit") |> 
  select(scenario_id, desc, value) %>% 
  pivot_wider(names_from = desc, values_from = value)


#--assign carbon credit based on assumed county/practice
c1 <- 
  c_scen %>% 
  left_join(r_carbon, by = c("county", "practice")) %>% 
  pivot_longer(co2:n2o)

#--change units, do total years
c2 <- 
  c1 %>% 
  left_join(p_sl) %>% 
  mutate(co2e_kghayr = 
          value * ac_per_ha * 1000,
         co2e_kghastand = co2e_kghayr * stand_life_yrs) 


#--make values negative because it is a sequestering
c3 <- 
  c2 %>% 
  #--make consistent with other formats
  mutate(
    cat = "carbon credit",
    desc = name,
    value = -co2e_kghastand,
    unit = "kg co2e/stand") %>% 
  select(scenario_id, cat, desc, unit, value)


g1 <- c3


# 2. energy ghg (e)---------------------------------------------------------------------
#--change energy to ghg emissions
#--requires assumptions about source of energy
#--those are listed under the 'energy sources' category of the assumptions file


#--what source to use for ghg emissions? EPA
e_dsghg <- 
  d_o |> 
  filter(cat == "data source",
         desc == "ghg from fuel") |> 
  pull(value)

#--what source to use for fuel energy contents?
e_dse <- 
  d_o |> 
  filter(cat == "data source",
         desc == "fuel energy content") |> 
  pull(value)


#--kg co2 per unit fuel 
e_ghg <- 
  r_fuelghg |> 
  filter(grepl(e_dsghg, source)) |> #--assumed energy content info
  filter(time_horizon == o_gwp) #--assumed timeframe


#--energy content
e_fuele <- 
  r_fuele |> 
  filter(source == e_dse) |> 
  mutate(energy_mj_per_l = value) |> 
  select(fuel_type, energy_mj_per_l)

#--get kg co2e released per mj of a fuel used
e_fuelghg <- 
  e_ghg |> 
  left_join(e_fuele) |> 
  mutate(value2 = case_when(
    unit == "kg/l" ~ value * (1/energy_mj_per_l), #--diesel and gasoline
    unit == "kg/kwh" ~ value * kwh_per_btu * btu_per_mj), #--electicity
    unit2 = "kg co2e/mj") |> 
  select(fuel_type, unit2, value2)


#--get the energy used per stand
e1 <- d_e 

#--deal with ones where we know the fuel type

e2 <- 
  e1 |> 
  filter(!is.na(fuel_type)) |> 
  left_join(e_fuelghg) |> 
  mutate(value4 = value * value2,
         unit4 = "kg co2e/stand") |> 
  select(scenario_id, 
         cat, 
         desc, 
         fuel_type, 
         unit4,
         value4)


#--what if we don't know the fuel used?
#--the difference btwn electric and diesel is only 32 kg co2e/stand
#--just assume diesel

e3 <- 
  e1 |> 
  filter(is.na(fuel_type)) |> 
  mutate(fuel_type = "diesel") |> 
  left_join(e_fuelghg) |> 
  mutate(value4 = value * value2,
         unit4 = "kg co2e/stand") |> 
  select(scenario_id, 
         cat, 
         desc, 
         fuel_type, 
         unit4,
         value4)


e4 <-
  e2 |> 
  bind_rows(e3) |> 
  rename(unit = unit4,
         value = value4)

#--simplify pesticides into one cat
e5 <- 
  e4 |> 
  mutate(desc = case_when(
    cat == "pesticide manufacture" ~ "pesticide",
    TRUE ~ desc)
  ) |> 
  group_by(scenario_id, cat, desc, fuel_type, unit) |> 
  summarise(value = sum(value))


g2 <- e5



# 3.  fert n2o (f)---------------------------------------------------------------------

#--calculate n2o emissions
#--use fertilizer n and plant n inputs
#--direct + indirect


#--direct emission assumptions
f_dir <- 
  d_o |> 
  filter(cat == "n2o direct") |> 
  mutate(value = as.numeric(value)) |> 
  pivot_wider(names_from = desc, values_from = value) |> 
  janitor::clean_names() |> 
  select(-cat, -unit)

#--indir asssumps, amount volatilized/leached etc.
f_indir <- 
  d_o |> 
  filter(cat == "n2o indirect") |> 
  #--get rid of fertilizer types assum, deal with separately
  filter(!grepl("synthetic n,|organic n,", desc)) |>
  mutate(value = as.numeric(value)) |> 
  pivot_wider(names_from = desc, values_from = value) |> 
  janitor::clean_names() |> 
  select(-cat, -unit)

#--frac emitted for each type of fertilizer
f_indir_f <- 
  d_o |> 
  filter(cat == "n2o indirect") |> 
  filter(grepl("synthetic n,|organic n,", desc)) |> 
  separate(desc, into = c("x", "fert_cat"), sep = ",") |> 
  select(-x) |> 
  mutate_if(is.character, str_trim) |> 
  mutate(value = as.numeric(value))

#--this is a hack
f_id <- 
  d_o |> 
  pull(scenario_id) |> 
  unique()

#--assumed crop following alfalfa

f_crop <- 
  d_o |> 
  filter(cat == "fertilizer avoidance",
         desc == "subsequent crop") |> 
  pull(value)

#--assumed amount of fertilizer avoided
f_amount <- 
  d_o |> 
  filter(cat %in% c("fertilizer avoidance")) |> 
  filter(grepl(f_crop, desc)) |> 
  mutate(avoided_lbnac = as.numeric(value)) |> 
  select(scenario_id, avoided_lbnac)

#--assumed type of fert avoided (they have different volatilities)
f_type <- 
  d_o |> 
  filter(cat %in% c("fertilizer avoidance")) |> 
  filter(desc == "type of fertilizer avoided") |> 
  mutate(fert_type = value) |> 
  select(scenario_id, fert_type)

f_avoid <- 
  f_amount |> 
  left_join(f_type)

#--actual n2o emissions

# note it is assumed all the n2o is released when the stand is terminated
# therefore the total is amortized over the standlife. Longer standlife = less n2o
# from IPCC 2019 refinement: the nitrogen residue from forages is only accounted for during pasture renewal


#--calculate kg of N applied per ha via fertilizers

#--get n per unit fertilizer
f1 <- 
  d_p |> 
  filter(cat == "fertility") |> 
  mutate(value = as.numeric(value))

#--calc applied n via fertilization
f2 <- 
  f1 |> 
  rename("fert_type" = desc) |> 
  left_join(r_fertcat) |> 
  left_join(r_fertn |> rename("fert_type" = desc)) |> 
  mutate(f_ntot = value * kgn_kgfert) |> 
  group_by(scenario_id, fert_cat) |> 
  summarise(value = sum(f_ntot, na.rm = T),
            unit = "kg n/stand",
            desc = "fert n")

#--get n from plants

#--amount of harvested dry matter each year
f3 <- 
  d_p |> 
  filter(cat == "yield") |> 
  filter(unit == "kg/stand") |> 
  mutate(value = as.numeric(value)) |> 
  group_by(scenario_id) |> 
  summarise(value = sum(value)) |> 
  left_join(p_sl) |> 
  mutate(kgdm_per_year = value/stand_life_yrs) |>
  select(scenario_id, stand_life_yrs, kgdm_per_year) 


#---use IPCC assumptions to get root N and residue N
#--note this is a hack - need to think about combining them more cleanly
f4 <- 
  f3 |> 
  mutate(scenario_id = f_id) |>#--they don't have a common column 
  left_join(f_dir)  |>  
  mutate(
    dm_n = kgdm_per_year * fraction_of_dm_not_harvested * kg_of_n_per_kg_dm,
    root_n = kgdm_per_year * kg_roots_per_kg_dm_harvested * kg_n_per_kg_root_dm,
    plant_n_kg = (dm_n + root_n)/stand_life_yrs 
  )

#--clean up to merge with fert n
f5 <- 
  f4 |> 
  mutate(value = plant_n_kg,
         unit = "kg n/stand",
         desc = "plant n") |> 
  select(scenario_id, scenario_id, desc, value, unit)

#--fert n and plant n 'applied'    
f6 <- 
  f2 |> 
  bind_rows(f5) |> 
  fill(scenario_id, .direction = "updown") |> 
  mutate(cat = "emissions")

#--now do avoided fert amount

f_avoid1 <-
  f_avoid |> 
  left_join(r_fertcat) |> 
  mutate(value = avoided_lbnac * kg_per_lb * ac_per_ha,
         unit = "kg n/stand",
         desc = "fert n",
         cat = "avoided emissions") |> 
  select(scenario_id, cat, desc, unit, value, fert_cat)


#--combine the n sources
f7_all <- 
  f6 |> 
  bind_rows(f_avoid1) |> 
  ungroup() |> 
  fill(scenario_id, .direction = "downup")


#--direct n2o emissions

f8 <- 
  f7_all |>  
  left_join(f_dir) |> 
  mutate(n2oN_kg = kg_n_n2o_emitted_per_kg_applied_residue_n * value,
         n2o_kg = n2oN_kg * n_to_n2o) |> 
  select(-value, -n2oN_kg) 

#---just look at it
# f8 |> 
#   mutate(co2_eq_kg = n2o_kg * o_gwpn2o) |> 
#   ggplot(aes(desc, co2_eq_kg)) + 
#   geom_col(aes(fill = desc), color = "black") + 
#   facet_grid(.~cat) +
#   labs(y = "kg co2-eq per ha",
#        x = NULL,
#        title = "N2O emissions, IPCC method") 


f9_dir <- 
  f8 |> 
  mutate(value = n2o_kg * o_gwpn2o,
         unit = "kg co2e/stand") |> 
  mutate(desc = paste0("direct, ", desc)) |> 
  select(scenario_id, scenario_id, cat, desc, unit, value)

#--indirect n2o emissions

#--assign the fertilizer to the correct category for volatilization
# (urea, ammonium, nitrate, ammonium-nitrate)
#--add in volatilization value
f10 <- 
  r_fertcat |> 
  rename("desc2" = fert_type) |> 
  left_join(f_indir_f |> 
              select(fert_cat, value)) |> 
  rename("kg_n_volatized_per_kg_applied_n" = value)


#--get all the constants lined up
#--note if you apply many types of fertilizer, each should be in a row here

f11 <- 
  f7_all |> 
  # #--get the type of fertilizer it is
  # left_join(f1 |> 
  #             select(scenario_id, desc) |> 
  #             rename("desc2" = desc) |> 
  #             mutate(desc = "fert n")) |> 
  #--add the category and assumed %N volatilization
  left_join(f10) |> 
  left_join(f_indir) 

#--do the calcs for volatization - plant n is not included here
f12_vol <- 
  f11 |> 
  filter(desc != "plant n") |> 
  mutate(value2 = 
           value * kg_n_volatized_per_kg_applied_n * kg_n_n2o_per_kg_n_volatalized,
         unit = "kg n2o-n vol/stand", 
         desc = "indirect, volatilize") |> 
  group_by(scenario_id, cat, unit, desc) |> 
  #--sum together in case there are multiple fertilizers
  summarise(value = sum(value2))

#--do the calcs for leaching
f13_leach <- 
  f11 |> 
  mutate(value2 = 
           value * kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached,
         unit = "kg n2o-n leach/stand",
         #--change desc from plant n/fert n to just indirect, leach
         desc = "indirect, leach") |> 
  group_by(scenario_id, cat, unit, desc) |> 
  #--sum them together (plant + all fertilizers)
  summarise(value = sum(value2))

#--comnbine volat and leach values, change to co2e
f14_ind <- 
  f12_vol |> 
  bind_rows(f13_leach) |> 
  mutate(value = value * n_to_n2o * o_gwpn2o,
         unit = "kg co2e/stand") 

#--combine direct and indirect emissions

f15 <- 
 f9_dir |> 
  bind_rows(f14_ind) |> 
  mutate(cat = str_replace(cat, "emissions", "n2o")) 

#--if it is avoided, it is negative
f16 <- 
  f15 |> 
  mutate(value = ifelse(cat == "n2o", value, -value))

g3 <- f16


# 4. total ghg (t)----------------------------------------------------------------


t <- 
  g1 |> 
  bind_rows(g2) |> 
  bind_rows(g3) |> 
  filter(value != 0) 

#--add scenario and stand life info
#--add in info about stand life and yields

t1 <- 
  d_p |> 
  filter(cat == "yield") |> 
  mutate(value = as.numeric(value)) |> 
  group_by(scenario_id, unit) |> 
  summarise(value = sum(value)) |> 
  pivot_wider(names_from = unit, values_from = value) |> 
  rename("yield_kg_stand" = 2,
         "standlife_years" = 3)

t2 <- 
  t |> 
  left_join(t1) |> 
  select(scenario_id, cat, desc, fuel_type, yield_kg_stand, standlife_years, unit, value)


#--calculate on per ha / yr, and per mg basis
t3 <- 
  t2 |> 
  mutate(
    kgco2e_stand = value,
    kgco2e_hayr = value / standlife_years,
    kgco2e_Mgyield = value / (yield_kg_stand/1000)) |> 
  select(-yield_kg_stand, -standlife_years, -unit, -value)


t4 <- 
  t3  |> 
  pivot_longer(5:7) |> 
  rename(unit = name)

return(t4)

}