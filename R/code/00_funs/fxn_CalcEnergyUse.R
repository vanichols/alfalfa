# calculate energy used
# created 3/14, combining code from 'code' folder
# updtaed 3/16 to handle new single file 
# 3/17 separated out fuel ops assumptions into its own reference file
# 3/20 - need to figure out how different energy sources are used
#    especially in irrigation
# 3/24 - trying to run w/siskiyou data
# 3/30 - didn't include seed in final dataset, oops!
# 4/27 - fixing field ops fuel records to include diff types of fertilize (inject, surface, prod1, est1, etc)
# 5/12 - include % of water passing through pump as variable



CalcEnergyUse <- function(f_scenario_id = "2001", 
                          f_prod_data = my_prod_data){
  
  source("R/code/00_funs/fxn_conversions.R")
  source("R/code/00_funs/fxn_ProcDataIn.R")
  

# get county --------------------------------------------------------------

  r_county <- 
    read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5) %>% 
    filter(scenario_id == paste0("scen_", f_scenario_id)) %>% 
    pull(location)
  
  # references --------------------------------------------------------------
  
  #--fertilizer manufacturing energy, from greet tables
  r_ferte <- read_csv("R/data_refs/ref_fert-energy.csv") 
  
  #--energy content of diesel - multiple sources
  r_efuel <- 
    read_csv("R/data_refs/ref_fuel-energy.csv") |> 
    mutate_if(is.character, str_to_lower) 
  
  #--conversion efficiencies of different fuels
  r_eff <- 
    read_csv("R/data_refs/ref_fuel-conv-eff.csv") 
  
  #--conversion efficiency of diesel to convert NRCS values to energy
  r_dieseff <- 
    r_eff |> 
    filter(fuel_type == "diesel") |> 
    mutate(value = value/100) |> 
    pull(value)
  
  
  #--fuel manufacture
  r_fuelm <-  read_csv("R/data_refs/ref_fuel-manu.csv") 
  
  #--fuel use per type of pass
  r_fuelops <-  read_csv("R/data_refs/ref_ops-fuel-usage.csv")
  
  #--pump pressure for each source/delivery situation
  #--filter so it is specific to the county in the scenario
  r_ipump <- 
    read_excel("R/data_refs/refbyhand_irr-pump-pressures.xlsx", skip = 5) %>% 
    fill(county, water_source) %>% 
    filter(county == r_county) %>% 
    select(-notes)

# non production data (other) -----------------------------------------------------

  d_o <- 
    read_csv(paste0("R/data_scens-notouch/scen_", f_scenario_id, ".csv"))  |> 
    ProcDataIn()
  
  #--which data source to use for energy content (used in u, i, and ?)
  d_o_fue <- 
    d_o |> 
    filter(cat == "data source") |> 
    filter(desc == "fuel energy content") |> 
    pull(value)
  
  
  # production data --------------------------------------------------------------------
  
  d_p <- f_prod_data
  
  
  # 1. fert avoidance (a) --------------------------------------------
  
  #--what is the following crop
  
  a_crop <- 
    d_o |> 
    filter(cat == "fertilizer avoidance",
           desc == "subsequent crop") |> 
    pull(value)
    
  
  #--get assumed amoutn of n application avoided
  a_avoid <- 
    d_o %>% 
    filter(grepl(a_crop, desc)) %>% 
    mutate(
      value = as.numeric(value),
      #--units are currently lb n/ac
      kgn_ha_avoided = value * kg_per_lb * ac_per_ha) %>% 
    select(scenario_id, kgn_ha_avoided) |> 
    mutate(desc = "nitrogen")
  
  
  
  # energy to manu the fertilizer we avoided
  #--note if this were a different fertilizer, we would need to account for all the product, not just the n manufact. avoided
  
  a2 <- 
    a_avoid %>% 
    left_join(r_ferte,
              by = c("desc")) |> 
    mutate(
      mj_avoided = -(value * kgn_ha_avoided) #--negative bc we avoided it
    )
  
  #--clean up
  a3 <- 
    a2 |> 
    select(scenario_id, mj_avoided) |> 
    rename(value = mj_avoided) |> 
    mutate(
      unit = "mj/stand",
      cat = "fertilizer avoidance",
      desc = "avoided uan-32 manu energy")
  
  e1 <- a3
  
  # 2. fert manu (f) -------------------------------------------------
  
  # how much fertilizer did we apply?
  
  f <- 
    d_p |> 
    filter(cat == "fertility") |> 
    #--remove sodium molybdate (siskiyou, only 1 pound/ac at est)
    filter(desc != "sodium molybdate") |> 
    mutate(value = as.numeric(value))
  
  #--combine with fert energy
  
  f1 <-
    f |>
    left_join(r_ferte |>
                mutate(mj_kgprod = value) |>
                select(desc, cat, component, mj_kgprod))
  
  #--calculate energy used to manufacture each component applied
  f2 <- 
    f1 %>% 
    mutate(mj_stand = value * mj_kgprod) |> 
    mutate(desc = ifelse(is.na(component), desc, paste0(desc, ", ", component))) |> 
    select(scenario_id, cat, desc, mj_stand) |> 
    mutate(unit = "mj/stand") |> 
    rename(value = mj_stand)
  
  
  e2 <- f2
  
  
  # 3. fuel usage (u) -------------------------------------------------------------
  
  #--NRCS gives us diesel use
  #--we want to convert that to a raw amount of energy required
  #--later, we use conversion efficiencies to get the actaul energy consumed to get this task done
  #--operations
  
  #--assumptions
  
  #--assumed mj/l in diesel from that data source
  u_e <- 
    r_efuel |> 
    filter(fuel_type == "diesel") |> 
    filter(source == d_o_fue) |> #--assumed data source for info
    pull(value)
  
  #--assumed diesel fuel consumption for each type of operation (from ref data)
  u_fu <- 
    r_fuelops |> 
    mutate(scenario_id = paste0("scen_", f_scenario_id))
    
  #--production data
  u <- 
    d_p |> 
    filter(cat %in% c("field ops", "harvest ops"))
  
  #--get total l used for each operation per stand life
  u1 <- 
    u %>% 
    left_join(u_fu) %>% 
    mutate(value = as.numeric(value)) |> 
    #--#of passes times L used per pass
    mutate(value = value * ldiesel_ha,
           unit = "l diesel/stand") %>% 
    select(scenario_id, desc, value, unit)
  
  #--change to mj used per stand, based on energy content of diesel
  u2 <- 
    u1 %>% 
    #--L used times energy per L is MJ per stand
    mutate(value = value * u_e,
           unit = "mj/stand") 
  
  #--separate operations into harvest and field ops
  u3 <- 
    u2 %>% 
    mutate(desc = ifelse(grepl("hay", desc), "harvest", "field ops")) %>% 
    group_by(scenario_id, desc, unit) %>% 
    summarise(diesel_energy = sum(value, na.rm = T)) %>% 
    mutate(cat = "field passes")
  
  
  #--back calculate energy required
  #--the actual energy used will depend on the actual fuel used
  #--since we assume diesel, we will use that conversion factor to back-calculate the energy req'd
  
  
  #--the actual energy req'd is less than the diesel energy req'd
  # (thermal efficiency diesel) x (diesel energy req'd) = (actual energy req'd)
  u4 <- 
    u3 %>% 
    mutate(value = diesel_energy * r_dieseff,
           unit = "mj req/stand") %>% #--just to make it clear this isn't the final value
    select(-diesel_energy)
  
  
  # calculate energy required 
  #--we have to assume a fuel used, and it's thermal efficiency (or conversion efficiency)
  
  #--which fuel used 
  u_fu <- 
    d_o |> 
    filter(cat == "energy source",
           desc != "irrigation") |> 
    separate(desc, into = c("x", "desc"), sep = ",") |> 
    mutate_if(is.character, str_trim) |> 
    rename(fuel_type = value) |> 
    select(scenario_id, desc, fuel_type)
  
  
  #--get thermal efficiency of assumed fuel
  u_eff <- 
    u_fu |> 
    left_join(r_eff) |> 
    mutate(therm_eff = value/100) |> 
    select(-value, -unit)
  
  #--energy expended by using selected fuel
  u5 <- 
    u4 |> 
    left_join(u_eff) |> 
    distinct() |> 
    mutate(value2 = value/therm_eff,
           unit = "mj/stand") |> 
    select(-value, -therm_eff) |> 
    rename(value = value2)
  
  e3 <- u5
  
  
  # 4.  irrigation (i)---------------------------------------------------------------------
  
  #--assumptions
  
  #--which fuel used for irrigation
  i_fu <- 
    d_o |> 
    filter(cat == "energy source",
           desc == "irrigation") |> 
    rename(fuel_type = value) |> 
    select(scenario_id, fuel_type)
  
  #--get thermal efficiency of assumed fuel
  i_fueff <- 
    i_fu |> 
    left_join(r_eff) |> 
    mutate(therm_eff = value/100) |> 
    select(-value, -unit)
  
  #--energy content of fuels using assumed data source
  i_fuen <- 
    i_fu |> 
    left_join(r_efuel) |> 
    filter(source == d_o_fue) |> 
    mutate(energy_cont = value) |> 
    select(scenario_id, fuel_type, energy_cont, unit)
  
  #--the efficienvies of irrigation
  i_effs <- 
    d_o |> 
    filter(cat == "irrigation") |> 
    mutate(value = as.numeric(value)) |> 
    filter(grepl("eff", desc)) |> 
    #--get just the type (sprinkler, surface, drip)
    separate(desc, into = c("type", "x", "xx")) |> 
    mutate(eff_frac = value) |> 
    select(scenario_id, type, eff_frac)
  
  #--assumed percentage of water needs satisfied with surface water
  i_psurf <- 
    d_o |>
    filter(cat == "irrigation") |> 
    mutate(value = as.numeric(value)) |> 
    filter(desc == "fraction from surface source") |> 
    pull(value)
  
  i_welldepth_ft <-
    d_o |>
    filter(cat == "irrigation") |> 
    mutate(value = as.numeric(value)) |> 
    filter(desc == "depth of well") |>
    pull(value)
  
#--this will now be a lookup table thing  
  # i_pump_pres_psi <-
  #   d_o |> 
  #   filter(cat == "irrigation") |> 
  #   mutate(value = as.numeric(value)) |> 
  #   filter(desc == "pump pressure") |>
  #   pull(value)
  
  #--data
  i <- 
    d_p |> 
    filter(cat == "irrigation") |> 
    mutate(value = as.numeric(value))
  
  #--need to know the type, the source, the amount
  #--the source is based on the assumption 
  
  #--get the type
  i1 <- 
    i |> 
    separate(desc, into = c("type", "x"), sep = ",") |> 
    filter(grepl("ac-in", unit)) |> 
    mutate(water_applied_ac_in = value) |> 
    select(scenario_id, cat, type, water_applied_ac_in) 
  
  #--the amt coming from each source for each type
  i2 <- 
    i1 |> 
    mutate(surface = water_applied_ac_in * i_psurf, #--assumed amt from surface
           ground = water_applied_ac_in - surface) |>
    select(-water_applied_ac_in) |>
    pivot_longer(surface:ground, values_to = "water_acin") 
  
  #--assign pump pressure depending on type/source combo
  i3 <- 
    i2 %>%
    left_join(r_ipump %>%
                select(
                  name = water_source,
                  type = irr_type,
                  pump_press_psi = pump_psi
                )) %>% 
    mutate(welldepth_ft = ifelse(name == "ground",
                                 i_welldepth_ft, #--assumed pumping depth
                                 0)) %>% 
    filter(!is.na(pump_press_psi))
  
  #--do some goofy conversions
  i4 <- 
    i3 |> 
    left_join(i_effs) |> 
    mutate(
      pump_press_ft = pump_press_psi * fthead_per_psi,
      #--change ac-in to gallons, then pounds of water
      water_galac = water_acin * ft_per_in * gal_per_acft,
      water_lbsac = water_galac * lb_per_gal_water,
      #--btus used per foot pound
      ftlb_per_ac = water_lbsac * (pump_press_ft + welldepth_ft),
      btu_per_ac = ftlb_per_ac * btu_per_ftlb,
      #--take into account eff
      mj_per_ha = btu_per_ac * ha_per_ac * mj_per_btu / eff_frac
    ) 
  
  
  #--energy req'd
  i5 <- 
    i4 |> 
    unite(type, name, col = "desc", sep = ", ") |> 
    select(scenario_id, cat, desc, mj_per_ha) |> 
    rename(value =  mj_per_ha) |> 
    mutate(unit = "mj/stand") |> 
    filter(value != 0)
  
  
  #--take into account type of fuel used, add on thermal efficiency of that fuel
  i6 <- 
    i5 |>
    rename(mj_stand = value) |> 
    select(-unit) |> 
    left_join(i_fuen |> select(-energy_cont, -unit)) |> #attaches scenario to fuel type
    left_join(r_eff |> 
                mutate(therm_eff = value/100) |> 
                select(-unit, -value))
  
  #--calculate actual energy expended, given fuel's thermal efficiency
  i7 <- 
    i6 |> 
    mutate(value = mj_stand/(therm_eff),
           unit = "mj/stand") |> 
    select(scenario_id,
           cat,
           desc,
           fuel_type,
           unit, value) |> 
    filter(value != 0)
  
  
  e4 <- i7
  
  
  # 5. fuel manu (m) -------------------------------------------------------------
  
  # calculate energy used to produce the fuel used
  #--this is confusing. Electricity has a higher manufacturing energy than fossil fuels by 10x
  #--I'm not sure if I should include this or not
  #--it applies to the tractor fuel use, and the irrigation fuel use
  
  m_fuele <- 
    r_fuelm |> 
    rename("fuel_manu" = value,
           "unit_fuelmanu" = unit) |> 
    select(-source)
  
  
  #--energy required from fuel
  
  m <- 
    e3 |> 
    bind_rows(e4)
  
  #--change to btu/stand
  m1 <- 
    m |> 
    mutate(value = value * btu_per_mj,
           unit = "btu/stand")
  
  #--energy req'd to manufacture the fuel
  
  m2 <- 
    m1 |> 
    left_join(m_fuele) |> #--btus used per btu created
    mutate(manu_btu = value * fuel_manu,
           manu_mj = manu_btu * mj_per_btu)
  
  #--clean it up
  m3 <- 
    m2 |> 
    mutate(unit = "mj/stand",
           value = manu_mj, 
           desc = "fuel manufacture") |> 
    select(scenario_id, fuel_type, cat, desc, unit, value)
  
  e5 <- m3
  
  
  # 6. pesticide manu (p) ---------------------------------------------------
  
  #--assumed energy used to manu a unit of ai
  
  p_eais <- 
    d_o |> 
    filter(cat == "pesticide manufacture") |>
    mutate(value = as.numeric(value)) |> 
    rename(mj_kgai = value) |> 
    select(-cat, -unit, -change_ind, -scenario_id)

    
  #--production data
  
  p <- 
    d_p |> 
    filter(cat == "pesticide") |> 
    mutate(value = as.numeric(value)) |> 
    group_by(scenario_id, cat, desc, unit) |> 
    summarise(value = sum(value))
  
  #--note surfactants are not included right now
  #--the zinc phosphide assumed value is definitely a problem
  p1 <-
    p %>% 
    left_join(p_eais) #--assumed energy to manu ais
  
  p2 <- 
    p1 %>%
    mutate(value2 = value * mj_kgai,
           unit = "mj/stand") %>% 
    select(scenario_id, cat, desc, unit, value2) |> 
    rename(value = value2)
  
  e6 <- p2
  
  
  # 7. seed energy (s) ------------------------------------------------------
  
  #---uses values from imperial, scenario 2001
  
  
  if (f_scenario_id == "2001") {
  
    # no harvest ops energy in seed calcs
    # not sure what to do for irrigation, it's left the same for now
    
    #--assumptions
    s_seedyld <- 
      d_o |>
      filter(cat == "seed manufacture",
             desc == "seed yield") |>
      mutate(seed_yld_kg_ha = as.numeric(value) * kg_per_lb * ac_per_ha) |> 
      select(scenario_id, seed_yld_kg_ha)
    
    
    s <- 
      e2 |> 
      bind_rows(e3 |> filter(desc != "harvest")) |> 
      bind_rows(e4) |> 
      bind_rows(e5 |> filter(desc != "harvest")) |> 
      bind_rows(e6) |> 
      fill(scenario_id, .direction = "downup") |> 
      group_by(scenario_id, unit) |> 
      summarise(value = sum(value))
    
    #--energy per unit seed produced
    
    s1 <- 
      s |> 
      #--assume this is energy used to create the given seed yield
      left_join(s_seedyld) |> 
      #--get mj/kg seed produced
      mutate(mj_kgseed = value/seed_yld_kg_ha) |>
      select(scenario_id, mj_kgseed) 
   
    s1 |> write_csv("R/data_refs/ref_seed-energy-imperial.csv")
     
  } else {
    
    s1 <- read_csv("R/data_refs/ref_seed-energy-imperial.csv") |> 
      mutate(scenario_id = paste0("scen_", f_scenario_id))
  }
    
  
    
    #--ftm has a much lower value. But whatever. 
    #--they don't include fuel manufacturing, irrigatigation fuel ineffic.
    # s1 |> 
    #   mutate(btu_lbseed = mj_kgseed * kg_per_lb * btu_per_mj) |> 
    #   mutate(ftm_value = 1973)
    
    
    s2 <- 
      s1 |> 
      left_join(d_p |> filter(cat == "seed")) |> 
      #--multiply by amount of seed used to plant
      mutate(value2 = as.numeric(value) * mj_kgseed,
             unit = "mj/stand")  |> 
      select(-value) |> 
      rename(value = value2)
    
    
    e7 <- s2
    
  
  
  
  # 8. all (e)---------------------------------------------------------------------
  
  e8 <- 
    e1 |> 
    bind_rows(e2) |> 
    bind_rows(e3) |> 
    bind_rows(e4) |> 
    bind_rows(e5) |> 
    bind_rows(e6) |> 
    bind_rows(e7) |> 
    fill(scenario_id, .direction = "downup") 
  
  
  #--add in info about stand life and yields
  
  e9 <- 
    d_p |> 
    filter(cat == "yield") |> 
    mutate(value= as.numeric(value)) |> 
    group_by(scenario_id, unit) |> 
    summarise(value = sum(value)) |> 
    pivot_wider(names_from = unit, values_from = value) |> 
    rename("yield_kg_stand" = 2,
           "standlife_years" = 3)
  
  e10 <- 
    e8 |> 
    left_join(e9) |> 
    select(scenario_id, cat, desc, fuel_type, yield_kg_stand, standlife_years, unit, value)
  
  #--calculate on per ha / yr, and per mg basis
  e11 <- 
    e10 |> 
    mutate(
      GJ_stand = value / 1000,
      GJ_hayr = (value / standlife_years) / 1000,
      MJ_kgyield = value / yield_kg_stand) |> 
    select(-yield_kg_stand, -standlife_years, -unit, -value)
  
  #--pivot to long
  e12 <- 
    e11 |> 
    pivot_longer(GJ_stand:MJ_kgyield) |> 
    rename(unit = name)
  
  
  return(e12)
  
}


