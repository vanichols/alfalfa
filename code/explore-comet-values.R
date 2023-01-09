# purpose: look at COMET estimates/payments for Yolo county
# data extracted by hand from http://comet-planner-cdfahsp.com/
# would be nice to get database?

library(tidyverse)
library(readxl)
library(patchwork)


d <- read_excel("data/Comet-Yolo.xlsx",
                skip = 4) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty() %>% 
  mutate_if(is_character, str_to_lower)


d2 <- 
  d %>% 
  #--make perennial crop appear in both irrigations
  filter(irrigated != "na") %>% 
  bind_rows(
    d %>% 
      filter(irrigated == "na") %>% 
      select(-irrigated) %>%
      slice(rep(1:n(), each = 2)) %>% 
      mutate(irrigated = c("ir", "non-ir", "ir", "non-ir"))
  ) 


d2_long <- 
  d %>% 
  #--make perennial crop appear in both irrigations
  filter(irrigated != "na") %>% 
  bind_rows(
    d %>% 
      filter(irrigated == "na") %>% 
      select(-irrigated) %>%
      slice(rep(1:n(), each = 2)) %>% 
      mutate(irrigated = c("ir", "non-ir", "ir", "non-ir"))
    )  %>% 
  pivot_longer(co2:ncol(.), values_to = "value_ac")


d2 %>% 
  mutate(tot_co2eq = co2 + n2o + ch4) %>% 
  ggplot(aes(reorder(desc, tot_co2eq), tot_co2eq)) + 
  geom_point(aes(size = usd)) + 
  facet_grid(.~irrigated) +
  coord_flip()

d2 %>% 
  mutate(tot_co2eq = co2 + n2o + ch4) %>% 
  ggplot(aes(reorder(desc, tot_co2eq), tot_co2eq)) + 
  geom_point(aes(size = usd, color = irrigated)) + 
  coord_flip() + 
  scale_color_manual(values = c("ir" = "blue", "non-ir" = "brown")) + 
  labs(title = "Reduction in CO2eq (includes all GHG")


d2 %>% 
  ggplot(aes(reorder(desc, co2), co2)) + 
  geom_point(aes(size = usd, color = irrigated)) + 
  coord_flip() + 
  scale_color_manual(values = c("ir" = "blue", "non-ir" = "brown")) + 
  labs(title = "Soil CO2eq sequestration",
       x = "comet practice",
       y = "Mg of co2eq per acre")


f1 <-
  d2 %>%
  mutate(co2_ha = co2 * 2.47) %>% 
  ggplot(aes(reorder(desc, co2_ha), co2_ha)) + 
  geom_jitter(aes(color = irrigated, size = usd), width = 0.1) + 
  coord_flip() + 
  scale_color_manual(values = c("ir" = "blue", "non-ir" = "brown")) + 
  labs(title = "Soil CO2 sequestration from COMET, Yolo County",
       x = "comet practice",
       y = "Mg of co2 sequestered per hectare")

f2 <-
  d2 %>%
  mutate(c_ha = co2 * 2.47 * 12/44) %>% 
  ggplot(aes(reorder(desc, c_ha), c_ha)) + 
  geom_jitter(aes(color = irrigated, size = usd), width = 0.1) + 
  coord_flip() + 
  scale_color_manual(values = c("ir" = "blue", "non-ir" = "brown")) + 
  labs(title = "Soil C sequestration from COMET, Yolo County",
       x = "comet practice",
       y = "Mg of C sequestered per hectare")


f1 / f2 + plot_layout(guides = "collect")
ggsave("images/comet-yolo.png")
