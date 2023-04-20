#--make different panels for each healthy soils scenario
#--created 4/5

rm(list = ls())
library(tidyverse)
library(tidytext)
library(patchwork)
library(readxl)
library(scales)
library(ggarchery)
library(ggbreak)



# read in monster data file -----------------------------------------------

d_raw <- read_csv("R/data_tidy/scen_all.csv")

cc_scen <- 
  read_csv("R/data_tidy/scen_all-tot.csv") |> 
  filter(name != "energy")


d_ener <- 
  read_csv("R/data_tidy/scen_all-tot.csv") |> 
  filter(name == "energy")


# fig ---------------------------------------------------------------------

cc_scenMg <- 
  cc_scen |> 
  arrange(-value) |> 
  mutate(name = fct_inorder(name),
         unit = case_when(
           (unit == "kgco2e_stand") ~ "Mgco2e_stand",
           (unit == "kgco2e_hayr") ~ "Mgco2e_hayr",
           TRUE ~ unit),
         value = case_when(
           (unit == "Mgco2e_stand"|unit == "Mgco2e_hayr") ~ value / 1000,
           TRUE ~ value)) 

cc_scenMg |> 
  ggplot(aes(location, value)) + 
  geom_jitter(aes(color = name, pch = name), width = 0.1, size = 2) +
  geom_hline(yintercept = 0) +
  facet_wrap(~unit, scales = "free") + 
  theme_bw() +
  theme(strip.text = element_text(size = rel(1.2))) + 
  labs(x = NULL,
       y = NULL,
       color = "CA healthy soils carbon credit",
       shape = "CA healthy soils carbon credit")

ggsave("R/figs/ghg.png")


cc_scenMg <- 
  cc_scen |> 
  arrange(-value) |> 
  mutate(name = fct_inorder(name),
         unit = case_when(
           (unit == "kgco2e_stand") ~ "Mgco2e_stand",
           (unit == "kgco2e_hayr") ~ "Mgco2e_hayr",
           TRUE ~ unit),
         value = case_when(
           (unit == "Mgco2e_stand"|unit == "Mgco2e_hayr") ~ value / 1000,
           TRUE ~ value)) 


# energy ------------------------------------------------------------------

d_ener |> 
  filter(location == "tulare") |> 
  filter(unit == "GJ_hayr",
         value < 12)

d_ener |> 
  ggplot(aes(location, value)) + 
  geom_jitter(aes(fill = location, pch = location), width = 0.1, size = 4, show.legend = F) +
  geom_hline(yintercept = 0) +
  facet_wrap(~unit, scales = "free") + 
  theme_bw() +
  scale_fill_manual(values = c("darkblue", "gold")) +
  scale_shape_manual(values = c(21, 22)) +
  theme(strip.text = element_text(size = rel(1.2))) + 
  labs(x = NULL,
       y = NULL,
       title = "Siskiyou uses lowest amount of energy per hectare per year",
       subtitle = str_wrap("Tulare uses less energy per unit yield, and with gravity-fed irrigation is comparable to Siskiyou",
                           width = 100))


ggsave("R/figs/energy.png", width = 8, height = 6)

# compare impacts w/in a carbon scenario ----------------------------------

d_tot_new <- 
  cc_scen |> 
  ungroup() |> 
  filter(name == "no_c_credit")

#--for crop rot
crot <- 
  cc_scen |> 
  filter(scen_desc == "base",
         name != "pasture_establishment_c_credit") |> 
  pivot_wider(names_from = name,
              values_from = value) |> 
  mutate(
    diff_base = conservation_crop_rotation_c_credit - no_c_credit,
    diff_base_pct = diff_base / no_c_credit,
    scen_desc = "conservation crop rotation carbon credit",
    scenario_id = "xxx") |> 
  select(scen_desc, location, unit, diff_base, diff_base_pct)

#--for pasture
cpast <- 
  cc_scen |> 
  filter(scen_desc == "base",
         name != "conservation_crop_rotation_c_credit") |> 
  pivot_wider(names_from = name,
              values_from = value) |> 
  mutate(
    diff_base = pasture_establishment_c_credit - no_c_credit,
    diff_base_pct = diff_base / no_c_credit,
    scen_desc = "pasture establishment carbon credit",
    scenario_id = "xxx") |> 
  select(scen_desc, location, unit, diff_base, diff_base_pct)


d_base <- 
  d_tot_new |> 
  filter(scen_desc == "base") |> 
  rename(base_value = value) |> 
  select(base_value, location, unit)

d_diff <- 
  d_tot_new |> 
  left_join(d_base) |> 
  mutate(diff_base = value - base_value,
         diff_base_pct = diff_base / base_value) |> 
  bind_rows(crot) |> 
  bind_rows(cpast)



#--separate facets by location
d_diff |>
  filter(grepl("hayr", unit)) |>
  ggplot(aes(
    reorder_within(x = scen_desc, by = diff_base_pct, within = location),
    diff_base_pct
  )) +
  geom_arrowsegment(
    aes(
      x = reorder_within(x = scen_desc, by = diff_base_pct, within = location),
      xend = reorder_within(x = scen_desc, by = diff_base_pct, within = location),
      y = 0,
      yend = diff_base_pct,
      color = diff_base < 0), 
    arrows = arrow(length = unit(0.2, "cm")),
    size = 2,
    show.legend = F) +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_reordered() +
  scale_y_continuous(labels = label_percent()) +
  labs(x = NULL,
       y = "Change in GHG emissions per hectare\n(% change from base scenario)") +
  coord_flip() +
  facet_grid(location ~ ., scales = "free") + 
  theme_bw()



#--trying ggbreak
d_diff |>
  filter(grepl("hayr", unit)) |>
  ggplot(aes(
    reorder_within(x = scen_desc, by = diff_base_pct, within = location),
    diff_base_pct
  )) +
  geom_arrowsegment(
    aes(
      x = reorder_within(x = scen_desc, by = diff_base_pct, within = location),
      xend = reorder_within(x = scen_desc, by = diff_base_pct, within = location),
      y = 0,
      yend = diff_base_pct,
      color = diff_base < 0), 
    arrows = arrow(length = unit(0.2, "cm")),
    size = 2,
    show.legend = F) +
  scale_color_manual(values = c("red", "blue")) +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels = label_percent()) +
  labs(x = NULL,
       y = "Change in GHG emissions per hectare\n(% change from base scenario)",
       title = "Carbon offsets have largest impact on GHG balance",
       subtitle = "Irrigation and electification are of next highest importance") +
  coord_flip() +
  ggbreak::scale_y_break(c(-0.75, -2), scales = 2) +
  facet_grid(location ~ ., scales = "free") + 
  theme_bw() + 
  theme(strip.text.y = element_text(size = rel(1.2),
                                  angle =0))

ggsave("R/figs/sensitivity.png", width = 8, height = 6)

#--scale_wrap?

#--trying ggbreak
d_diff |>
  filter(grepl("hayr", unit)) |>
  ggplot(aes(
    reorder_within(x = scen_desc, by = diff_base_pct, within = location),
    diff_base_pct
  )) +
  geom_arrowsegment(
    aes(
      x = reorder_within(x = scen_desc, by = diff_base_pct, within = location),
      xend = reorder_within(x = scen_desc, by = diff_base_pct, within = location),
      y = 0,
      yend = diff_base_pct,
      color = diff_base < 0), 
    arrows = arrow(length = unit(0.2, "cm")),
    size = 2,
    show.legend = F) +
  scale_color_manual(values = c("red", "blue")) +
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels = label_percent()) +
  labs(x = NULL,
       y = "Change in GHG emissions per hectare\n(% change from base scenario)",
       title = "Carbon offsets have largest impact on GHG balance",
       subtitle = "Irrigation and electification are of next highest importance") +
  coord_flip() +
  ggbreak::scale_wrap(n = 4) +
  facet_grid(location ~ ., scales = "free") + 
  theme_bw() + 
  theme(strip.text.y = element_text(size = rel(1.2),
                                    angle =0))
