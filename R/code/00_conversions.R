library(packagefinder)

#findPackage("unit conversion")
#packageDetails("measurements")

# I can't get tehe NIST one to work. It has some weird units. 

# conversions -------------------------------------------------------------

library(measurements)

kg_per_lb <- conv_unit(1, "lbs", "kg")
lb_per_kg <- 1 / kg_per_lb

lbs_per_ton <- 2000

ac_per_ha <- conv_unit(1, "hectare", "acre")
ha_per_ac <- 1/ac_per_ha

btu_per_mj <- conv_unit(1, "J", "BTU") * 1000 * 1000
mj_per_btu <- 1 / btu_per_mj


pints_per_gal <- conv_unit(1, "us_gal", "us_pint")
gal_per_pint <- 1/pints_per_gal

oz_per_gal <- conv_unit(1, "us_gal", "us_oz")
gal_per_oz <- 1/oz_per_gal

g_per_gal_water <- 3785.41
