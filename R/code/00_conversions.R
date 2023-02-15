library(packagefinder)

#findPackage("unit conversion")
library(tidyverse)


fun_preproc <- function(data = d.dum) {
  tmp <- 
    data %>% 
    fill(scenario_id, .direction = c("down")) %>% 
    fill(cat, .direction = c("down")) %>% 
    fill(desc, .direction = c("down")) %>% 
    select(-notes) %>% 
    mutate_if(is.character, str_to_lower)
  return(tmp)
}



#packageDetails("measurements")

# I can't get tehe NIST one to work. It has some weird units. 

# conversions -------------------------------------------------------------

library(measurements)

kg_per_lb <- conv_unit(1, "lbs", "kg")
lb_per_kg <- 1 / kg_per_lb

lbs_per_ton <- 2000

in_per_m <- conv_unit(1, "m", "inch")
m_per_in <- 1 / in_per_m

ac_per_ha <- conv_unit(1, "hectare", "acre")
ha_per_ac <- 1/ac_per_ha
m2_per_ha <- 10000

btu_per_mj <- conv_unit(1, "J", "BTU") * 1000 * 1000
mj_per_btu <- 1 / btu_per_mj


cm3_per_m3 <- conv_unit(1, "m3", "cm3")

pints_per_gal <- conv_unit(1, "us_gal", "us_pint")
l_per_gal <- conv_unit(1, "us_gal", "l")
gal_per_pint <- 1/pints_per_gal

oz_per_gal <- conv_unit(1, "us_gal", "us_oz")
gal_per_oz <- 1/oz_per_gal

g_per_gal_water <- 3785.41
l_water_per_m3 <- 1000
