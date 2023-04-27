#--look at results
#--created 3/16
#--reran everything 3/29 (fixed fert energy calcs)

rm(list = ls())
library(tidyverse)
library(pals)
library(tidytext)
library(patchwork)
library(ggplotlyExtra)
library(plotly)
library(ggrepel)


# energy ------------------------------------------------------------------

source("R/code/00_funs/fxn_VizEnergy.R")

#--tulare
i_want_this_scenario_id <- "0001"
t <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_t <- t[[1]]

#--siskiyou
i_want_this_scenario_id <- "1001"
s <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_s <- s[[1]]

#--imperial
i_want_this_scenario_id <- "2001"
i <- VizEnergy(f_scenario_id = i_want_this_scenario_id)
data_i <- i[[1]]

dat_all <- 
  data_t %>% 
  bind_rows(data_s) %>% 
  bind_rows(data_i)

#--why is imperial higher?
dat_all %>% 
  filter(cat == "total") %>% 
  filter(unit == "MJ_kgyield") %>% 
  ggplot(aes(location, value)) + 
  geom_col()


#--look at components

dat_all %>% 
  filter(cat != "total") %>% 
  ggplot(aes(location, value, fill = cat_short)) + 
  geom_col() +
  geom_hline(yintercept = 0) +
  facet_wrap(~unit, scales = "free")


dat_all_sums <- 
  dat_all %>% 
  filter(cat != "total") %>% 
  group_by(location, cat_short, unit) %>% 
  summarise(value = sum(value)) 

#--add 0 values for missing cats (imperial, irrigation)
dat_all_sums2 <- 
  dat_all_sums %>% 
  full_join(dat_all_sums %>% 
              ungroup() %>% 
              expand(location, cat_short, unit) %>% 
              filter(cat_short == "irrig")) %>% 
  mutate(value = ifelse(is.na(value), 0, value))

dat_all_sums2 %>% 
  filter(unit == "MJ_kgyield") %>% 
  ggplot(aes(reorder(cat_short, -value, sum), value, fill = location)) + 
  geom_col(position = position_dodge2(), color = "black") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(imperial = "darkred", siskiyou = "lightblue", tulare = "gold")) +
  facet_wrap(~unit, scales = "free", ncol = 1)

ggsave("R/figs/energy-components.png")


#--why does imperial have more fert avoidance than siskiyou when they both have wheat?
#--because their stand lives are different (and yields)
dat_all %>% 
  filter(cat == "fertilizer avoidance") %>% 
  filter(unit == "GJ_stand")
