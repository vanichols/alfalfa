#--compare nass and cdl acreages

library(tidyverse)
library(readxl)
library(ggthemes)
library(scales)
library(tidytext)

# colors ------------------------------------------------------------------

options(ggplot2.discrete.colour = c("#ffae49", "#44b7c2", "#024b7a", "#aa3939"))
options(ggplot2.discrete.fill = c("#2E652E", "#ffae49", "#aa3939", "#024b7a"))

"#aa6c39"
"#AA3939"

"#0D9B9B"

"#533131"

"#FF7F15"

"#2d882d"

"#2E652E"

"#274327"

#reddish #a42500
theme_set(theme_bw())


# data --------------------------------------------------------------------

d <- read_excel("R/data_raw/raw-bloomberg-data.xlsx", skip = 4)

d %>% 
  mutate(av_wu = (wulo_acft + wuhi_acft)/2,
         rev_per_wu = revenue_ac / wuhi_acft) %>% 
  select(crop, rev_per_ac = revenue_ac, av_wu, rev_per_wu) %>% 
  pivot_longer(2:ncol(.)) %>% 
  mutate(crop2 = reorder_within(crop, value, name)) %>% 
  ggplot(aes(crop2, value)) + 
  geom_col(aes(fill = crop), color = "black") + 
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~name, scales = "free")
