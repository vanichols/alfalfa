m1 <- read_csv("R/data_tidy/energy_fuel-manu.csv")

m2 <- read_csv("R/data_tidy/energy_fuel-manu2.csv")


bind_rows(m1, m2) %>% 
  separate(desc, into = c("desc", "energy source"), sep = ",") %>% 
  ggplot(aes(desc, value, fill = `energy source`)) + 
  geom_col(position = position_dodge())
