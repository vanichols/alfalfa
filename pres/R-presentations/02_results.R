#--see if any scenarios have a prayer of being climate neutral for presentation
#--created 6/2

rm(list = ls())
library(tidyverse)
library(tidytext)
library(patchwork)
library(readxl)
library(scales)
library(ggarchery)
library(ggbreak)


# fig things --------------------------------------------------------------

source("Rcode_color-palettes.R")



# read in monster data file -----------------------------------------------

d_raw <- read_csv("R/data_tidy/scen_all.csv")

#--scenario key
sk <-
  read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5) %>% 
  select(scenario_id, location, scen_desc) %>% 
  filter(!is.na(scenario_id))


d <- 
  d_raw %>% 
  left_join(sk) %>% 
  filter(grepl("co2", unit)) %>% 
  filter(unit == "kgco2e_hayr") %>% 
  group_by(scenario_id, scen_desc, cat, unit, location) %>% 
  summarise(value = sum(value, na.rm = T))

#--process for figure
d2 <- 
  d %>% 
  mutate(cat = case_when(
    cat == "avoided n2o" ~ "avoided fertilizer, n2o",
    cat == "fertilizer avoidance" ~ "avoided fertilizer, manuf.",
    cat == "pesticide" ~ "Pesticide manuf.",
    cat == "fertility" ~ "Fertilizer manuf.",
    cat == "seed" ~ "Seed manuf.",
    cat == "field passes" ~ "Field passes, fuel manuf. + use",
    cat == "irrigation" ~ "Irrigation, fuel manuf. + use",
    TRUE ~ cat)) %>% 
  arrange(location, scenario_id, value) %>% 
  group_by(scenario_id, location) %>% 
  mutate(cumval = cumsum(value)) %>% 
  #--make nice labels
  mutate(
    scen_desc = str_to_title(scen_desc),
    location = str_to_title(location),
    cat = str_to_sentence(cat),
    cat = str_replace(cat, "n2o", "N2O"),
    cat = str_replace(cat, "N2o", "N2O"),
    cat = str_replace(cat, "ghg", "GHG"))


# figs ---------------------------------------------------------------------

pres_theme1 <- 
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(3)),
        plot.subtitle = element_text(size = rel(1.5)),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(2)),
        strip.background = element_rect(fill = p1_tan),
        strip.text = element_text(size = rel(1.5)),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 1),
        plot.background = element_rect(fill = "white", colour = p1_dkbl, linewidth = 3))

co2_lab <- bquote("kg" ~ CO[2] ~ "e" ~ ha^-1 ~ yr^-1)
co2_lab2 <- bquote("Mg" ~ CO[2] ~ "e" ~ ha^-1 ~ yr^-1)


#--base
d2 %>% 
  filter(scen_desc == "Base") %>%
  filter(location == "Imperial") %>% 
  mutate(clr = ifelse(cumval < 0, "neg", "pos")) %>% 
  mutate(
    value2 = -value,
    cat = reorder_within(x = cat, by = value2, within = scen_desc),
    prevcumval = lag(cumval),
    prevcumval = ifelse(is.na(prevcumval), 0, prevcumval)) %>% 
  ggplot(aes(cat, cumval)) + 
  geom_segment(aes(x = cat, xend = cat, y = prevcumval, yend = cumval, color = clr),
               linewidth = 3, arrow = arrow(), show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(p1_ltbl, p1_red)) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL,
       y = co2_lab) +
  pres_theme1 +
  facet_wrap(~location)

ggsave("pres/fig_imp-base.png", width = 10, height = 6)

#--scenarios (get rid of ordering)

d2 %>% 
  filter(scen_desc != "Base") %>%
  filter(location == "Imperial") %>% 
  mutate(clr = ifelse(cumval < 0, "neg", "pos")) %>% 
  mutate(
    value2 = -value,
    cat = reorder_within(x = cat, by = value2, within = scen_desc),
    prevcumval = lag(cumval),
    prevcumval = ifelse(is.na(prevcumval), 0, prevcumval)) %>% 
  ggplot(aes(cat, cumval/1000)) + 
  geom_segment(aes(x = cat, xend = cat, y = prevcumval/1000, yend = cumval/1000, color = clr),
               linewidth = 3, arrow = arrow(), show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(p1_ltbl, p1_red)) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL,
       y = co2_lab2) +
  pres_theme1 +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~scen_desc, scales = "free_y", labeller = label_wrap_gen(15))

ggsave("pres/fig_imp-scens.png", width = 8, height = 7)


# tulare ------------------------------------------------------------------


d2 %>% 
  filter(scen_desc == "Base") %>%
  filter(location == "Tulare") %>% 
  mutate(clr = ifelse(cumval < 0, "neg", "pos")) %>% 
  mutate(
    value2 = -value,
    cat = reorder_within(x = cat, by = value2, within = scen_desc)) %>% 
  mutate(prevcumval = lag(cumval),
         prevcumval = ifelse(is.na(prevcumval), 0, prevcumval)) %>% 
  ggplot(aes(cat, cumval/1000)) + 
  geom_segment(aes(x = cat, xend = cat, y = prevcumval/1000, yend = cumval/1000, color = clr),
               linewidth = 3, arrow = arrow(), show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(p1_ltbl, p1_red)) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL,
       y = co2_lab2) +
  pres_theme1 +
  facet_wrap(~location)


#--scenarios (get rid of ordering)

d2 %>% 
  filter(!(scen_desc %in% c("Base", "Double Pump Pressure",
                            "Double Well Depth", "No Leaching N2o", "No Fertilizer Offset",
                            "All Surface Irr"))) %>%
  filter(location == "Tulare")  %>% 
  mutate(clr = ifelse(cumval < 0, "neg", "pos")) %>% 
  mutate(
    value2 = -value,
    cat = reorder_within(x = cat, by = value2, within = scen_desc),
    prevcumval = lag(cumval),
    prevcumval = ifelse(is.na(prevcumval), 0, prevcumval)) %>% 
  ggplot(aes(cat, cumval/1000)) + 
  geom_segment(aes(x = cat, xend = cat, y = prevcumval/1000, yend = cumval/1000, color = clr),
               linewidth = 3, arrow = arrow(), show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(p1_ltbl, p1_red)) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL,
       y = co2_lab2) +
  pres_theme1 +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~scen_desc, scales = "free_y", labeller = label_wrap_gen(width = 15))

ggsave("pres/fig_tul-scens.png", width = 10, height = 8)

# siskiyou ------------------------------------------------------------------


d2 %>% 
  filter(scen_desc == "Base") %>%
  filter(location == "Siskiyou") %>% 
  mutate(clr = ifelse(cumval < 0, "neg", "pos")) %>% 
  mutate(
    cat = reorder_within(x = cat, by = value, within = scen_desc)) %>% 
  mutate(prevcumval = lag(cumval),
         prevcumval = ifelse(is.na(prevcumval), 0, prevcumval)) %>% 
  ggplot(aes(cat, cumval/1000)) + 
  geom_segment(aes(x = cat, xend = cat, y = prevcumval/1000, yend = cumval/1000, color = clr),
               linewidth = 3, arrow = arrow(), show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(p1_ltbl, p1_red)) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL,
       y = co2_lab2) +
  pres_theme1 +
  facet_wrap(~location)


#--scenarios (get rid of ordering)

d2 %>% 
  filter(!(scen_desc %in% c("Base", "Double Pump Pressure",
                            "Double Well Depth", "No Leaching N2o", "No Fertilizer Offset"))) %>%
  filter(location == "Siskiyou")  %>% 
  mutate(clr = ifelse(cumval < 0, "neg", "pos")) %>% 
  mutate(
    value2 = -value,
    cat = reorder_within(x = cat, by = value2, within = scen_desc),
    prevcumval = lag(cumval),
    prevcumval = ifelse(is.na(prevcumval), 0, prevcumval)) %>% 
  ggplot(aes(cat, cumval/1000)) + 
  geom_segment(aes(x = cat, xend = cat, y = prevcumval/1000, yend = cumval/1000, color = clr),
               linewidth = 3, arrow = arrow(), show.legend = F) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c(p1_ltbl, p1_red)) +
  scale_x_reordered() +
  coord_flip() +
  labs(x = NULL,
       y = co2_lab2) +
  pres_theme1 +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(1), angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~scen_desc, scales = "free_y", labeller = label_wrap_gen(width = 15))

ggsave("pres/fig_sis-scens.png", width = 8, height = 7)


