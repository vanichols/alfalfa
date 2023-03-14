#--calculate n2o emissions avoided from avoided fertilizer
#--use fertilizer n only (assume plant n is not impacted)
#--direct + indirect
#--have to assume a type of fertilizer that is avoided
#--have to assume a crop - let's do tomatoes
#--3/9 clean up



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


# gwp ---------------------------------------------------------------------

gwp_n2o <- 
  read_excel("R/data_refs/refbyhand_gwp.xlsx", skip = 5) |> 
  filter(time_horizon == a_gwp) |> 
  filter(molecule == "n2o") |> 
  pull(global_warming_potential)


# get n -------------------------------------------------------------------

n <- read_csv("R/data_tidy/ghg_n.csv")



# calculate n2o -----------------------------------------------------------


# note it is assumed all the n2o is released when the stand is terminated
# therefore the total is amortized over the standlife. Longer standlife = less n2o
# from IPCC 2019 refinement: the nitrogen residue from forages is only accounted for during pasture renewal

# direct n2o emissions----------------------------------------------------------

#--just a percentage of the N applied or in the plant residue
ghg_dir <- 
  n |>  
  left_join(a_dir) |> 
  mutate(n2oN_kg = kg_n_n2o_emitted_per_kg_applied_residue_n * value,
         n2o_kg = n2oN_kg * n_to_n2o) |> 
  select(production_id, 
         assump_id, 
         cat, 
         desc, n2o_kg) 

#---just look at it
ghg_dir |> 
  mutate(co2_eq_kg = n2o_kg * gwp_n2o) |> 
  ggplot(aes(desc, co2_eq_kg)) + 
  geom_col(aes(fill = desc), color = "black") + 
  facet_grid(.~cat) +
  labs(y = "kg co2-eq per ha",
       x = NULL,
       title = "N2O emissions, IPCC method") 

ghg_dir1 <- 
  ghg_dir |> 
  mutate(value = n2o_kg * gwp_n2o,
         unit = "kg co2e/stand") |> 
  mutate(desc = paste0("direct, ", desc)) |> 
  select(production_id, assump_id, cat, desc, unit, value)


# indirect n2o emissions --------------------------------------------------

#--only applies to fert n?

ghg_ind <- 
  n |> 
  left_join(a_indir_f |> 
              select(fert_cat, assump_value)) |> 
  rename("kg_n_volatized_per_kg_applied_n" = assump_value) |> 
  left_join(a_indir) 

#--do the calcs for volatization - plant n is not included here
ghg_vol <- 
  ghg_ind |> 
  filter(desc != "plant n") |> 
  mutate(value2 = 
           value * kg_n_volatized_per_kg_applied_n * 
           kg_n_n2o_per_kg_n_volatalized,
         unit = "kg n2o-n vol/stand", 
         desc = "indirect, volatilize") |> 
  group_by(production_id, assump_id, desc, cat, unit) |> 
  #--sum together in case there are multiple fertilizers
  summarise(value = sum(value2))

#--do the calcs for leaching, plant n is included
ghg_leach <- 
  ghg_ind |> 
  mutate(value2 = 
           value * kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached,
         unit = "kg n2o-n leach/stand") |> 
  group_by(production_id, assump_id, cat, unit) |> 
  #--sum them together (plant + all fertilizers)
  summarise(value = sum(value2)) |> 
  mutate(desc = "indirect, leach")

#--comnbine volat and leach values, change to co2e
ghg_ind1 <- 
  ghg_vol |> 
  bind_rows(ghg_leach) |> 
  mutate(value = value * n_to_n2o * gwp_n2o,
         unit = "kg co2e/stand") 

ghg_ind1


# combine direct and indir ------------------------------------------------------------

ghg_n2o <- 
  ghg_dir1 |> 
  bind_rows(ghg_ind1) |>
  mutate(value = ifelse(cat == "avoided emissions", -value, value),
         cat = ifelse(cat == "avoided emissions", "avoided n2o", "n2o"))

ghg_n2o |> 
  write_csv("R/data_tidy/ghg_n2o.csv")


