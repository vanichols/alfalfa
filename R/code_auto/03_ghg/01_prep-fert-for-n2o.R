#--calculate n2o emissions
#--use fertilizer n and plant n inputs
#--direct + indirect
#--2/15 - need to do indirect
#--2/22 cleaned up, still lacks indirect
#--2/23 added indirect
#--3/9 checking, separated data prep from n2o calcs
#--do both actual and avoided fert prep here

library(tidyverse)
library(readxl)


rm(list = ls())

source("R/code/00_conversions.R")
source("R/code/00_funs.R")


# data for stand life --------------------------------------------------------------------

d_raw <- read_csv("R/data_inputs/datin_production.csv",
                  skip = 5) |> 
  janitor::remove_empty()

d <- fun_preproc_prod(d_raw)

sl <- 
  d |> 
  filter(desc == "stand life") |> 
  rename("stand_life_yrs" = value) |> 
  select(production_id, stand_life_yrs)

# assumptions -------------------------------------------------------------

a0 <- 
  read_csv("R/data_inputs/datin_assumptions.csv",
                skip = 5) 
a <- 
  fun_preproc_assum(a0) 

#--timespan for gwp
a_gwp <- 
  a |> 
  filter(assump_cat == "gwp") |> 
  pull(assump_value)

#--direct emission assumptions
a_dir <- 
  a |> 
  filter(assump_cat == "n2o direct") |> 
  mutate(assump_value = as.numeric(assump_value)) |> 
  pivot_wider(names_from = assump_desc, values_from = assump_value) |> 
  janitor::clean_names() |> 
  select(-assump_cat, -assump_unit)

#--indir asssumps, amount volatilized/leached etc.
a_indir <- 
  a |> 
  filter(assump_cat == "n2o indirect") |> 
  #--get rid of fertilizer types assum, deal with separately
  filter(!grepl("synthetic n,|organic n,", assump_desc)) |>
  mutate(assump_value = as.numeric(assump_value)) |> 
  pivot_wider(names_from = assump_desc, values_from = assump_value) |> 
  janitor::clean_names() |> 
  select(-assump_cat, -assump_unit)

#--frac emitted for each type of fertilizer
a_indir_f <- 
  a |> 
  filter(assump_cat == "n2o indirect") |> 
  filter(grepl("synthetic n,|organic n,", assump_desc)) |> 
  separate(assump_desc, into = c("x", "fert_cat"), sep = ",") |> 
  select(-x) |> 
  mutate_if(is.character, str_trim) |> 
  mutate(assump_value = as.numeric(assump_value))

#--this is a hack
a_id <- 
  a |> 
  pull(assump_id) |> 
  unique()

#--assumed crop following alfalfa
a_crop <- "tomatoes"

#--assumed type and amount of fertilizer avoided
a_amount <- 
  a |> 
  filter(assump_cat %in% c("fertilizer avoidance")) |> 
  filter(grepl(a_crop, assump_desc)) |> 
  mutate(avoided_lbnac = as.numeric(assump_value)) |> 
  select(assump_id, avoided_lbnac)

a_type <- 
  a |> 
  filter(assump_cat %in% c("fertilizer avoidance")) |> 
  filter(assump_desc == "type of fertilizer avoided") |> 
  mutate(fert_type = assump_value) |> 
  select(assump_id, fert_type)

a_avoid <- 
  a_amount |> 
  left_join(a_type)

# gwp ---------------------------------------------------------------------

gwp_n2o <- 
  read_excel("R/data_refs/refbyhand_gwp.xlsx", skip = 5) |> 
  filter(time_horizon == a_gwp) |> 
  filter(molecule == "n2o") |> 
  pull(global_warming_potential)

# note it is assumed all the n2o is released when the stand is terminated
# therefore the total is amortized over the standlife. Longer standlife = less n2o
# from IPCC 2019 refinement: the nitrogen residue from forages is only accounted for during pasture renewal


# fertilizer categories ---------------------------------------------------

f_catref <- read_csv("R/data_refs/refbyhand_fert-category.csv", skip = 5) |> select(-notes)


# N from applied fert ---------------------------------------------------------

#--calculate kg of N applied per ha via fertilizers

#--get n per unit fertilizer, clean up for merg
f_nref <- 
  read_csv("R/data_refs/ref_fert-n-contents.csv") |> 
  rename(desc = fert_type) |> 
  mutate(kgn_kgfert = value) |> 
  select(desc, kgn_kgfert)

#--read in how much of the fert was applied
f <- read_csv("R/data_tidy/prod_fertility.csv")

#--calc applied n via fertilization
n_apf <- 
  f |> 
  rename("fert_type" = desc) |> 
  left_join(f_catref) |> 
  left_join(f_nref |> rename("fert_type" = desc)) |> 
  mutate(f_ntot = value * kgn_kgfert) |> 
  group_by(production_id, fert_cat) |> 
  summarise(value = sum(f_ntot, na.rm = T),
            unit = "kg n/stand",
            desc = "fert n")


# get N from plants --------------------------------------------------------

#--amount of harvested dry matter each year
h_dm <- 
  read_csv("R/data_tidy/prod_yields.csv") |>
  group_by(production_id) |> 
  summarise(value = sum(value)) |> 
  left_join(sl) |> 
  mutate(kgdm_per_year = value/stand_life_yrs) |>
  select(production_id, stand_life_yrs, kgdm_per_year) 


#---use IPCC assumptions to get root N and residue N
#--note this is a hack - need to think about combining them more cleanly
n_p <- 
  h_dm |> 
  mutate(assump_id = a_id) |>#--they don't have a common column 
  left_join(a_dir)  |>  
  mutate(
    dm_n = kgdm_per_year * fraction_of_dm_not_harvested * kg_of_n_per_kg_dm,
    root_n = kgdm_per_year * kg_roots_per_kg_dm_harvested * kg_n_per_kg_root_dm,
    plant_n_kg = (dm_n + root_n)/stand_life_yrs 
  )

#--clean up to merge with fert n
n_p1 <- 
  n_p |> 
  mutate(value = plant_n_kg,
         unit = "kg n/stand",
         desc = "plant n") |> 
  select(production_id, assump_id, desc, value, unit)

#--fert n and plant n 'applied'    
n_ap <- 
  n_p1 |> 
  bind_rows(n_apf) |> 
  fill(assump_id) |> 
  mutate(cat = "emissions")



# avoided fert ------------------------------------------------------------

n_avf <- 
  a_avoid |> 
  left_join(f_catref) |> 
  mutate(value = avoided_lbnac * kg_per_lb * ac_per_ha,
         unit = "kg n/stand",
         desc = "fert n",
         cat = "avoided emissions") |> 
  select(assump_id, cat, desc, unit, value, fert_cat)



# combine -----------------------------------------------------------------

n_all <- 
  n_ap |> 
  bind_rows(n_avf) |> 
  fill(production_id)


n_all |> 
  write_csv("R/data_tidy/ghg_n.csv")

# direct n2o emissions----------------------------------------------------------

ghg_dir <- 
 all_n |>  
  left_join(a_dir) |> 
  mutate(n2oN_kg = kg_n_n2o_emitted_per_kg_applied_residue_n * value,
         n2o_kg = n2oN_kg * n_to_n2o) |> 
  select(-value, -n2oN_kg) 

#---just look at it
ghg_dir |> 
  mutate(co2_eq_kg = n2o_kg * gwp_n2o) |> 
  ggplot(aes(desc, co2_eq_kg)) + 
  geom_col(aes(fill = desc), color = "black") + 
  labs(y = "kg co2-eq per ha",
       x = NULL,
       title = "N2O emissions, IPCC method") 


ghg_dir1 <- 
  ghg_dir |> 
  mutate(value = n2o_kg * gwp_n2o,
         unit = "kg co2e/stand") |> 
  mutate(desc = paste0("direct, ", desc)) |> 
  select(production_id, assump_id, desc, unit, value)


# indirect n2o emissions --------------------------------------------------

#--assign the fertilizer to the correct category
# (urea, ammonium, nitrate, ammonium-nitrate)

f_cat_ref <- 
  read_csv("R/data_refs/refbyhand_fert-category.csv", skip = 5) |> 
  select(-notes)

f_cat <- 
  f_cat_ref |> 
  rename("desc2" = fert_type) |> 
  left_join(a_indir_f |> 
              select(fert_cat, assump_value)) |> 
  rename("kg_n_volatized_per_kg_applied_n" = assump_value)


#--get all the constants lined up
#--note if you apply many types of fertilizer, each should be in a row here

ghg_ind <- 
  all_n |> 
  #--get the type of fertilizer it is
  left_join(f |> 
              select(production_id, desc) |> 
              rename("desc2" = desc) |> 
              mutate(desc = "fert n")) |> 
    #--add the category and assumed %N volatilization
  left_join(f_cat) |> 
  left_join(a_indir) 

#--do the calcs for volatization - plant n is not included here
ghg_vol <- 
  ghg_ind |> 
  filter(desc != "plant n") |> 
  mutate(value2 = 
           value * kg_n_volatized_per_kg_applied_n * kg_n_n2o_per_kg_n_volatalized,
         unit = "kg n2o-n vol/stand", 
         desc = "indirect, volatilize") |> 
  group_by(production_id, unit, desc) |> 
  #--sum together in case there are multiple fertilizers
  summarise(value = sum(value2))
  
#--do the calcs for leaching
ghg_leach <- 
  ghg_ind |> 
  mutate(value2 = 
           value * kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached,
         unit = "kg n2o-n leach/stand",
         #--change desc from plant n/fert n to just indirect, leach
         desc = "indirect, leach") |> 
  group_by(production_id, unit, desc) |> 
  #--sum them together (plant + all fertilizers)
  summarise(value = sum(value2))
  
#--comnbine volat and leach values, change to co2e
ghg_ind1 <- 
  ghg_vol |> 
  bind_rows(ghg_leach) |> 
  mutate(value = value * n_to_n2o * gwp_n2o,
         unit = "kg co2e/stand") 

# add together ------------------------------------------------------------

ghg_n2o <- 
  ghg_dir1 |> 
  bind_rows(ghg_ind1) |> 
  mutate(cat = "n2o") |> 
  fill(assump_id)

ghg_n2o |> 
  write_csv("R/data_tidy/ghg_n2o.csv")
