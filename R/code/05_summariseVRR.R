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
library(ggh4x)


#Gina's colors
#--palette 1
p1_ltbl <- "#43b7c2"
p1_dkbl <- "#024b79"
p1_ora <- "#ffad48"
p1_red <- "#ba5800"
p1_tan <- "#f2cc86"

#V.R: I use colorbrewer2.org HEX

# read in monster data file -----------------------------------------------

d_raw <- read_csv("R/data_tidy/scen_all.csv")

scen_key <- read_csv("R/data_in/scenbyhand_scenario-key.csv", skip = 5)



# unique categories -------------------------------------------------------

#--what are the categories?
unique(d_raw$cat)

cats <- 
  d_raw %>% 
  select(cat, desc) %>% 
  distinct() %>% 
  arrange(cat, desc)

cats %>% 
  write_csv("R/data_tidy/categories.csv")


# range in category values ------------------------------------------------

d_raw %>% 
  left_join(scen_key) %>% 
  filter(unit == "kgco2e_hayr") %>% 
  #--not 16 and 17, the carbon credit changes
  filter(!grepl("16", scenario_id),
         !grepl("17", scenario_id)) %>% 
  group_by(scenario_id, cat, unit) %>% 
  summarise(value = sum(value)) %>% 
  group_by(cat, unit) %>% 
  summarise(val_min = min(value), 
            val_max = max(value)) %>% 
  ggplot() + 
  geom_linerange(aes(reorder(x = cat, val_max),ymin = val_min, ymax = val_max)) + 
  coord_flip()

#--separated by location
#--no range in siskiyou pesticides?
d_raw %>% 
  left_join(scen_key) %>% 
  filter(unit == "kgco2e_hayr") %>% 
  #--not 16 and 17, the carbon credit changes
  filter(!grepl("16", scenario_id),
         !grepl("17", scenario_id)) %>%
  group_by(scenario_id, cat, unit, location) %>% 
  summarise(value = sum(value)) %>% 
  group_by(cat, unit, location) %>% 
  summarise(val_min = min(value), 
            val_max = max(value)) %>% 
  ggplot() + 
  geom_linerange(aes(reorder(x = cat, val_max),ymin = val_min, ymax = val_max, 
                     color = location), size = 3, position = position_dodge2(width = 0.5)) + 
  coord_flip() +
  labs(title = "Ranges in category values across scenarios",
       subtitle = "Carbon credit ranges not included",
       y = "kgCO2e per ha per year",
       x = NULL)

d_raw %>% 
  left_join(scen_key) %>% 
  filter(unit == "kgco2e_hayr") %>% 
  #--not 16 and 17, the carbon credit changes
  # filter(!grepl("16", scenario_id),
  #        !grepl("17", scenario_id)) %>%
  group_by(scenario_id, cat, unit, location) %>% 
  summarise(value = sum(value)) %>% 
  group_by(cat, unit, location) %>% 
  summarise(val_min = min(value), 
            val_max = max(value)) %>% 
  ggplot() + 
  geom_linerange(aes(reorder(x = cat, val_max),ymin = val_min, ymax = val_max, 
                     color = location), size = 3, position = position_dodge2(width = 0.5)) + 
  coord_flip() +
  labs(title = "Ranges in category values across scenarios",
       y = "kgCO2e per ha per year",
       x = NULL)



# tulare ------------------------------------------------------------------

#--energy
d_raw %>% 
  filter(unit == "GJ_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare", scen_desc == "base") %>%
  group_by(cat) %>% 
  mutate(sum = sum(value)) %>% 
  arrange(-sum) %>% 
  ungroup() %>% 
  mutate(
    cat = fct_inorder(cat),
    desc = fct_inorder(desc)) %>% 
  ggplot(aes(cat, value)) + 
  geom_col(aes(fill = desc), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = NULL,
       y = "GJ energy used per ha per year",
       title = "Tulare, base scenario")

d_raw %>% 
  select(unit) %>% 
  distinct()

#--GHG
d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare", scen_desc == "base") %>%
  group_by(cat) %>% 
  mutate(sum = sum(value)) %>% 
  arrange(-sum) %>% 
  ungroup() %>% 
  mutate(
    cat = fct_inorder(cat),
    desc = fct_inorder(desc)) %>% 
  ggplot(aes(cat, value)) + 
  geom_col(aes(fill = desc), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = NULL,
       y = "kgCO2e released per ha per year",
       title = "Tulare, base scenario")


d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare") %>%
  mutate(scen_desc = fct_inorder(scen_desc)) %>% 
  ggplot(aes(scen_desc, value)) + 
  geom_col(aes(fill = cat), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x= NULL,
       y = "kgCO2e per ha per year",
       title = "Tulare")


# all ------------------------------------------------------------------


#--GHG
d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(scen_desc == "base") %>%
  group_by(cat) %>% 
  mutate(sum = sum(value)) %>% 
  arrange(-sum) %>% 
  ungroup() %>% 
  mutate(
    cat = fct_inorder(cat),
    desc = fct_inorder(desc)) %>% 
  ggplot(aes(location, value)) + 
  geom_col(aes(fill = cat), color = "gray") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = NULL,
       y = "kgCO2e released per ha per year",
       title = "All locations, base scenarios")


d_raw %>% 
  filter(unit == "kgco2e_hayr") %>% 
  left_join(scen_key) %>% 
  filter(location == "tulare") %>%
  mutate(scen_desc = fct_inorder(scen_desc)) %>% 
  ggplot(aes(scen_desc, value)) + 
  geom_col(aes(fill = cat), color = "black") +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x= NULL,
       y = "kgCO2e per ha per year",
       title = "Tulare")
# Vale's code ------------------------------------------------------------------
#Emissions for poster
p <- d_raw %>%
  filter(unit == "kgco2e_hayr", value >= 0) %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base") %>%
  mutate(
    location = recode(location,
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert",
                      "siskiyou" = "Intermountain"),
    location = factor(location,
                      levels = c("Intermountain","Central Valley","Low Desert"))
  ) %>%
  group_by(location, cat) %>%
  summarise(emission = sum(value), .groups="drop") %>%
  group_by(cat) %>%
  mutate(total_cat = sum(emission)) %>%
  ungroup() %>%
  # build your plotmath labels as before
  mutate(cat_label = case_when(
    cat == "n2o"  ~ "N[2]*O~Emissions",
    TRUE           ~ paste0('"', str_to_title(cat), '"')
  ),
  cat = fct_reorder(str_to_title(cat), total_cat),
  # now create a continuous text‐size variable, 
  # scaled into a sensible range (e.g. 3→5 pts)
  text_size = rescale(emission, to = c(3, 5))
  ) %>%
  ggplot(aes(location, emission, fill = cat)) +
  geom_col(color = "gray") +
  geom_text(aes(label = cat_label, size = text_size),
            parse         = TRUE,
            position      = position_stack(vjust = 0.5),
            color         = "white",
            check_overlap = TRUE) +
  scale_size_identity(guide = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05)),
                     labels = comma) +
  labs(x = NULL,
       y = expression(kg~CO[2]*e~ha^-1~yr^-1),
       title = "All locations, base scenario") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        axis.text = element_text(size = 12))


# vector of extensions you want
exts <- c("pdf", "png")

for (ext in exts) {
  ggsave(
    filename = paste0("R/figs/vale/ghg_base_categories.", ext),
    plot     = p,
    device   = ext,
    width    = 8,
    height   = 6,
    units    = "in"
  )
}

# Emissions for manuscript
library(tidyverse)
library(scales)

p <- d_raw %>%
  filter(unit == "kgco2e_hayr", value >= 0) %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base") %>%
  mutate(
    location = recode(location,
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert",
                      "siskiyou" = "Intermountain"),
    location = factor(location,
                      levels = c("Intermountain", "Central Valley", "Low Desert")),
    cat = recode(cat,
                 "n2o"              = "N2O Emissions",
                 "fertility"       = "Fertility",
                 "irrigation"       = "Irrigation",
                 "field passes" = "Field Passes",
                 "diesel"           = "Diesel",
                 "electricity"      = "Electricity",
                 "pesticide"        = "Pesticide",
                 "seed"             = "Seed"
    )
  ) %>%
  group_by(location, cat) %>%
  summarise(emission = sum(value), .groups = "drop") %>%
  group_by(cat) %>%
  mutate(total_cat = sum(emission)) %>%
  ungroup() %>%
  mutate(cat = fct_reorder(cat, total_cat)) %>%
  ggplot(aes(x = location, y = emission, fill = cat)) +
  geom_col(color = "gray40") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = comma
  ) +
  scale_fill_brewer(palette = "Set2", name = "Source") +
  labs(
    x = NULL,
    y = expression(kg~CO[2]*e~ha^-1~yr^-1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  )

p
# vector of extensions you want
exts <- c("pdf", "png")

for (ext in exts) {
  ggsave(
    filename = paste0("R/figs/vale/manuscript/ghg_base_categories.", ext),
    plot     = p,
    device   = ext,
    width    = 6.5,
    height   = 6,
    units    = "in"
  )
}

# 1. Build the ggplot for negative‐only values
#poster
neg_plot <- d_raw %>%
  filter(
    unit       == "kgco2e_hayr",
    value      <  0             # keep only negatives
  ) %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base") %>%
  # rename locations
  mutate(
    location = recode(location,
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert",
                      "siskiyou" = "Intermountain"),
    location = factor(location,
                      levels = c("Intermountain", "Central Valley", "Low Desert"))
  ) %>%
  # sum within each location & category
  group_by(location, cat) %>%
  summarise(emission = sum(value), .groups = "drop") %>%
  # reorder cats by total negative magnitude
  group_by(cat) %>%
  mutate(total_cat = sum(emission)) %>%
  ungroup() %>%
  mutate(
    cat_label = case_when(
      cat == "avoided n2o"        ~ '"Avoided "*N[2]*O~"Emissions"',                        # plotmath subscript
      cat == "carbon credit"      ~ '"Soil Carbon Credit"',                   # literal text in quotes
      cat == "fertilizer avoidance" ~ '"N Fertilizer Prod./Transp."', # literal text in quotes
      TRUE                         ~ paste0('"', str_to_title(cat), '"')     # quote every other label
    ),
    cat = fct_reorder(str_to_title(cat), total_cat),
    text_size = scales::rescale(-emission, to = c(3, 3.5))
  ) %>%
  # plot
  ggplot(aes(x = location, y = emission, fill = cat)) +
  geom_col(color = "gray") +
  geom_text(
    aes(label = cat_label, size= text_size),    # ← use cat_label now
    parse         = TRUE,      # ← turn on plotmath parsing
    position      = position_stack(vjust = 0.5),
    color         = "white",
    check_overlap = TRUE
  ) +
  scale_size_identity(guide = FALSE) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0)),  # give a bit of space above zero
    labels = comma
  ) +
  labs(
    x     = NULL,
    y     = expression(kg~CO[2]*"e "~ha^-1~yr^-1),
    title = "All locations, base scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12)
  )

#manuscript
neg_plot <- d_raw %>%
  filter(
    unit == "kgco2e_hayr",
    value < 0
  ) %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base") %>%
  mutate(
    location = recode(location,
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert",
                      "siskiyou" = "Intermountain"),
    location = factor(location, levels = c("Intermountain", "Central Valley", "Low Desert")),
    
    # Rename and relevel cat to match legend order and colors
    cat = recode(cat,
                 "carbon credit"        = "Carbon Credit",
                 "fertilizer avoidance" = "Avoided Fertilizer",
                 "avoided n2o"          = "Avoided N2O Emissions"
    ),
    cat = factor(cat, levels = c("Carbon Credit", "Avoided Fertilizer", "Avoided N2O Emissions"))
  ) %>%
  group_by(location, cat) %>%
  summarise(emission = sum(value), .groups = "drop") %>%
  ggplot(aes(x = location, y = emission, fill = cat)) +
  geom_col(color = "gray40") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0)),
    labels = comma
  ) +
  scale_fill_brewer(palette = "Set2",direction = 1, name = "Category") +
  labs(
    x = NULL,
    y = expression(kg~CO[2]*"e "~ha^-1~yr^-1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12)
  )

neg_plot

# 2. Save both PDF and PNG in one go
walk(
  c("pdf","png"),
  ~ ggsave(
    filename = paste0("R/figs/vale/manuscript/ghg_base_negatives.", .x),
    plot     = neg_plot,
    device   = if (.x=="pdf") cairo_pdf else .x,
    width    = 6.5,
    height   = 6,
    units    = "in"
  )
)


#Net emissions
d_raw %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(
    scen_desc == "base",
    unit      == "kgco2e_hayr"
  ) %>%
  mutate(
    location = recode(location,
                      "siskiyou" = "Intermountain",
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert"),
    location = factor(location,
                      levels = c("Intermountain", "Central Valley", "Low Desert"))
  ) %>%
  group_by(location) %>%
  summarise(net_emission = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = location, y = net_emission, fill = location)) +
  geom_col(color = "gray50") +
  scale_fill_manual(
    values = c(
      "Intermountain"   = "#91bfdb",  # teal
      "Central Valley"  = "#fc8d62",  # orange
      "Low Desert"      = "#ffffbf"  # yellow
    ),
    guide  = FALSE      # hide the legend if you don’t need it
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    x     = NULL,
    y     = expression(kg~CO[2]*e~ha^-1~yr^-1),
    title = "Net CO2e per Location — Base Scenario"
  ) +
  theme_minimal(base_size = 14)

#GHG emissions vs Net Emissions per location simple

# Compute emissions
ghg_df <- d_raw %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base", unit == "kgco2e_hayr", value >= 0) %>%
  mutate(location = recode(location,
                           "siskiyou" = "Intermountain",
                           "tulare"   = "Central Valley",
                           "imperial" = "Low Desert"),
         location = factor(location, levels = c("Intermountain", "Central Valley", "Low Desert"))) %>%
  group_by(location) %>%
  summarise(total_emission = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(emission_type = "GHG Emissions")

net_df <- d_raw %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base", unit == "kgco2e_hayr") %>%
  mutate(location = recode(location,
                           "siskiyou" = "Intermountain",
                           "tulare"   = "Central Valley",
                           "imperial" = "Low Desert"),
         location = factor(location, levels = c("Intermountain", "Central Valley", "Low Desert"))) %>%
  group_by(location) %>%
  summarise(total_emission = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(emission_type = "Net Emissions")

# Combine
plot_df <- bind_rows(ghg_df, net_df) %>%
  mutate(
    fill_color = recode(location,
                        "Intermountain"   = "#91bfdb",
                        "Central Valley"  = "#fc8d62",
                        "Low Desert"      = "#ffffbf"),
    alpha_val = if_else(emission_type == "GHG Emissions", 1, 0.5)
  )

# Plot
ggplot(plot_df, aes(x = location, y = total_emission, 
                    fill = fill_color, alpha = alpha_val, group = emission_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "gray40") +
  #geom_text(aes(label = round(total_emission, 0)),
  #position = position_dodge(width = 0.7),
  #vjust = -0.5, size = 4) +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_y_continuous(labels = comma) +
  labs(
    x = NULL,
    y = expression("Emissions (kg CO"[2]*"e ha"^-1*" yr"^-1*")"),
    title = "GHG vs Net Emissions per Location — Base Scenario",
    caption = "Emissions = full color, Net = transparent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12)
  )

#GHG emissions vs Net Emissions per location with facets (Figure 1 manuscript)

# Define facet strip and net bar colors by region
loc_cols <- c(
  "Intermountain"   = "#91bfdb",  # teal
  "Central Valley"  = "#fc8d62",  # orange
  "Low Desert"      = "#ffffbf"   # yellow
)

# Define Okabe-Ito colorblind-safe palette (excluding black)
okabe_ito_colors <- c(
  "Fertilizer"       = "#009E73",  # bluish green
  "Seed"            = "#56B4E9",  # sky blue
  "N2O Emissions"   = "#E69F00",  # orange
  "Irrigation"      = "#0072B2",  # blue
  "Field Passes"    = "#F0E442",  # yellow
  "Diesel"          = "#D55E00",  # vermillion
  "Pesticide"       = "#CC79A7",  # reddish purple
  "Electricity"     = "#999999",  # neutral gray
  "Net Emissions"   = "black"     # keep net bar black
)

# Step 1: GHG component data
ghg_df <- d_raw %>%
  filter(unit == "kgco2e_hayr", value >= 0) %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base") %>%
  mutate(
    location = recode(location,
                      "siskiyou" = "Intermountain",
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert"
    ),
    cat = recode(cat,
                 "n2o"           = "N2O Emissions",
                 "fertility"     = "Fertilizer",
                 "irrigation"    = "Irrigation",
                 "field passes"  = "Field Passes",
                 "diesel"        = "Diesel",
                 "electricity"   = "Electricity",
                 "pesticide"     = "Pesticide",
                 "seed"          = "Seed"
    ),
    bar_type = "GHG Emissions"
  ) %>%
  group_by(location, cat, bar_type) %>%
  summarise(emission = sum(value), .groups = "drop")

# Step 2: Net emissions data
net_df <- d_raw %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(scen_desc == "base", unit == "kgco2e_hayr") %>%
  mutate(
    location = recode(location,
                      "siskiyou" = "Intermountain",
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert"
    )
  ) %>%
  group_by(location) %>%
  summarise(emission = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    cat = "Net Emissions",
    bar_type = "Net Emissions"
  )

# Step 3: Combine and factor order
plot_df <- bind_rows(ghg_df, net_df) %>%
  mutate(
    location = factor(location, levels = names(loc_cols)),
    bar_type = factor(bar_type, levels = c("GHG Emissions", "Net Emissions"))
  )

plot_df <- plot_df %>%
  mutate(
    cat = factor(cat, levels = c(
      "Seed",
      "Pesticide",
      "N2O Emissions",
      "Electricity",
      "Diesel",
      "Field Passes",
      "Irrigation",
      "Fertilizer",
      "Net Emissions"  # Make sure this is last (or wherever you want black)
    ))
  )

# Step 4: Plot
ghg_net <-ggplot(plot_df, aes(x = bar_type, y = emission, fill = cat)) +
  # Component bars
  geom_col(
    data = filter(plot_df, bar_type == "GHG Emissions"),
    position = "stack", width = 0.6, color = "gray40"
  ) +
  # Net bars with black fill
  geom_col(
    data = filter(plot_df, bar_type == "Net Emissions") %>% mutate(cat = "Net Emissions"),
    fill = "black", position = "stack", width = 0.6, color = "gray50"
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap2(
    ~ location,
    scales = "free_x",
    nrow = 1,
    strip = strip_themed(background_x = elem_list_rect(fill = loc_cols))
  ) +
  scale_fill_manual(
    values = okabe_ito_colors,
    name = "Source"
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    x = NULL,
    y = expression(kg~CO[2]*e~ha^-1~yr^-1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.spacing.x = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12)
  )

# 4. Save both PDF and PNG in one go
walk(
  c("pdf","png"),
  ~ ggsave(
    filename = paste0("R/figs/vale/manuscript/GHG emissions and net.", .x),
    plot     = ghg_net,
    device   = if (.x=="pdf") cairo_pdf else .x,
    width    = 6.5,
    height   = 6,
    units    = "in"
  )
)

#Emissions per yield only


# 1) Define yields (Mg grain/ha)
yields <- c(
  "Intermountain"   = 14.1,  # Siskiyou
  "Central Valley"= 20.2,  # Tulare
  "Low Desert"    = 16.6   # Imperial
)

yield_net_emissions <- d_raw %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(
    scen_desc == "base",
    unit      == "kgco2e_hayr"
  ) %>%
  mutate(
    location = recode(location,
                      "siskiyou" = "Intermountain",
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert"),
    location = factor(location,
                      levels = names(yields))
  ) %>%
  group_by(location) %>%
  summarise(
    net_emission = sum(value, na.rm = TRUE),  # kg CO2e / ha
    .groups      = "drop"
  ) %>%
  # 2) join in the yield and compute emissions per Mg of yield
  mutate(
    yield_Mg_ha       = yields[as.character(location)],
    emission_per_Mg   = net_emission / yield_Mg_ha  # kg CO2e per Mg grain
  ) %>%
  # 3) plot that ratio
  ggplot(aes(x = location, y = emission_per_Mg, fill = location)) +
  geom_col(color = "gray50") +
  scale_fill_manual(
    values = c(
      "Intermountain"   = "#91bfdb",  # teal
      "Central Valley"  = "#fc8d62",  # orange
      "Low Desert"      = "#ffffbf"   # yellow
    ),
    guide = FALSE
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    x     = NULL,
    y     = expression(kg~CO[2]*e~Mg^-1~dry~weight),
    title = "kg CO2e per Mg dry weight — Base Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12)
  )

# 4. Save both PDF and PNG in one go
walk(
  c("pdf","png"),
  ~ ggsave(
    filename = paste0("R/figs/vale/yield_net_emissions.", .x),
    plot     = yield_net_emissions,
    device   = if (.x=="pdf") cairo_pdf else .x,
    width    = 8,
    height   = 6,
    units    = "in"
  )
)

#GHG Emissions and Energy Use per Mg Alfalfa Yield (Figure 3)

# 2. Emissions per Mg (kg CO2e per Mg dry weight)
em_df <- d_raw %>%
  left_join(scen_key, by="scenario_id") %>%
  filter(scen_desc=="base", unit=="kgco2e_hayr") %>%
  mutate(
    location = recode(location,
                      "siskiyou" = "Intermountain",
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert"
    ),
    location = factor(location, levels = names(yields))
  ) %>%
  group_by(location) %>%
  summarise(
    per_Mg = sum(value, na.rm=TRUE) / yields[as.character(location)],
    .groups="drop"
  ) %>%
  mutate(metric = "Emissions")

# 3. Energy per Mg (GJ per Mg dry weight)
en_df <- d_raw %>%
  left_join(scen_key, by="scenario_id") %>%
  filter(scen_desc=="base", unit=="GJ_hayr") %>%
  mutate(
    location = recode(location,
                      "siskiyou" = "Intermountain",
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert"
    ),
    location = factor(location, levels = names(yields))
  ) %>%
  group_by(location) %>%
  summarise(
    per_Mg = sum(value, na.rm=TRUE) / yields[as.character(location)],
    .groups="drop"
  ) %>%
  mutate(metric = "Energy")

p1 <- ggplot(em_df, aes(x = location, y = per_Mg)) +
  geom_col(width = 0.5, fill = "#fc8d62") +
  labs(
    x = NULL,
    y = expression(kg~CO[2]*e~Mg^-1~dry~weight)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x   = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.title.y  = element_text(size = 12),
    panel.border  = element_rect(color = "black", fill = NA, size = 1)
  )

p2 <- ggplot(en_df, aes(x = location, y = per_Mg)) +
  geom_col(width = 0.5, fill = "#66C2A5") +
  labs(
    x = NULL,
    y = expression(GJ~Mg^-1~dry~weight)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x   = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.title.y  = element_text(size = 12),
    panel.border  = element_rect(color = "black", fill = NA, size = 1)
  )


emission_energy <-p1 | p2   # horizontal stack
emission_energy

# 4. Save both PDF and PNG in one go
walk(
  c("pdf","png"),
  ~ ggsave(
    filename = paste0("R/figs/vale/manuscript/GHG emissions and Energy.", .x),
    plot     = emission_energy,
    device   = if (.x=="pdf") cairo_pdf else .x,
    width    = 6.5,
    height   = 6,
    units    = "in"
  )
)

# Compare scenarios ------------------------------------------------------------------
# Colors for facet strips by region
loc_cols <- c(
  "Intermountain"   = "#91bfdb",  # teal
  "Central Valley"  = "#fc8d62",  # orange, tulare
  "Low Desert"      = "#ffffbf"   # yellow
)

loc_scen <- list(
  "Intermountain"  = c("base", "change stand life from 6 to 8 years"),
  "Central Valley" = c("base", "all surface irr", "defecit irr", "electrify irrigation"),
  "Low Desert"     = c("base", "electrify harvest equip", "change stand life from 3 to 4 years")
)

# 1) Build the “Base” and other standard scenarios
net_scenarios <- d_raw %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(unit == "kgco2e_hayr") %>%
  mutate(
    location = recode(location,
                      "siskiyou" = "Intermountain",
                      "tulare"   = "Central Valley",
                      "imperial" = "Low Desert"
    ),
    location = factor(location, names(loc_cols))
  ) %>%
  filter(map2_lgl(location, scen_desc,
                  ~ .y %in% loc_scen[[.x]]
  )) %>%
  mutate(
    scenario = case_when(
      scen_desc == "base"                           ~ "Base",
      scen_desc == "all surface irr"                ~ "All surface irrigation",
      scen_desc == "defecit irr"                    ~ "Deficit irrigation",
      scen_desc == "electrify irrigation"           ~ "Electrified irrigation",
      scen_desc == "electrify harvest equip"        ~ "Electrify harvest equip",
      scen_desc == "change stand life from 3 to 4 years"
      ~ "Extend stand life 3 to 4 years",
      scen_desc == "change stand life from 6 to 8 years"
      ~ "Extend stand life 6 to 8 years",
      scen_desc == "no fertilizer offsets"          ~ "No fertilizer offsets",
      TRUE                                           ~ scen_desc
    ),
    scenario = fct_relevel(scenario, "Base")
  ) %>%
  group_by(location, scenario) %>%
  summarise(net_emission = sum(value, na.rm = TRUE), .groups = "drop") %>%
  # 2) Drop the unwanted “No fertilizer offsets” scenario
  filter(scenario != "No fertilizer offsets") %>%
  mutate(
    bar_fill = if_else(
      scenario == "Base",
      loc_cols[as.character(location)],
      "lightgray"
    )
  )

# 3) Build “50% Fertilizer (prod/trans)” for Intermountain only
fert50_IM <- d_raw %>%
  left_join(scen_key, by = "scenario_id") %>%
  filter(
    unit      == "kgco2e_hayr",
    scen_desc == "base",
    location  == "siskiyou"
  ) %>%
  # halve only the fertilizer‐related emissions
  mutate(value = if_else(cat == "fertility", value * 0.5, value)) %>%
  mutate(
    location = "Intermountain",
    scenario = "50% Fertilizer (prod/trans)"
  ) %>%
  group_by(location, scenario) %>%
  summarise(net_emission = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(bar_fill = "lightgray")

# 4) Append and enforce ordering
net_scenarios <- bind_rows(net_scenarios, fert50_IM) %>%
  mutate(
    location = factor(location,
                      levels = c("Intermountain", "Central Valley", "Low Desert")
    ),
    scenario = fct_relevel(scenario, "Base", "50% Fertilizer (prod/trans)")
  )

# 5) Plot
scenarios <- ggplot(net_scenarios, aes(x = scenario, y = net_emission)) +
  geom_col(aes(fill = bar_fill),
           color = "grey30", width = 0.7) +
  scale_fill_identity() +
  facet_wrap2(
    ~ location,
    scales = "free_x",
    nrow   = 1,
    strip  = strip_themed(
      background_x = elem_list_rect(fill = loc_cols)
    )
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    x     = "Scenario",
    y     = expression(kg~CO[2]*e~ha^-1~yr^-1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text      = element_text(face = "bold", color = "black"),
    axis.text.x     = element_text(angle = 30, hjust = 1),
    panel.spacing.x = unit(1, "lines"),
    panel.border    = element_rect(color = "black", fill = NA),
    legend.position = "none"
  )

# 2. Save both PDF and PNG in one go
walk(
  c("pdf","png"),
  ~ ggsave(
    filename = paste0("R/figs/vale/manuscript/net.", .x),
    plot     = scenarios,
    device   = if (.x=="pdf") cairo_pdf else .x,
    width    = 6.5,
    height   = 6,
    units    = "in"
  )
)
