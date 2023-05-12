#--look at alfalfa production in california
#--cdl data downloaded by hand

library(tidyverse)
library(maps)
library(ggthemes)



# data --------------------------------------------------------------------

cdl_path <- "R/data_raw/cdl/"
cdl_files <- list.files(paste(cdl_path))

ca_files <- cdl_files[grepl("california", cdl_files)]
im_files <- cdl_files[grepl("imperial", cdl_files)]
si_files <- cdl_files[grepl("siskiyou", cdl_files)]
tu_files <- cdl_files[grepl("tulare", cdl_files)]
yo_files <- cdl_files[grepl("yolo", cdl_files)]


# function ----------------------------------------------------------------


ReadDaFiles <- function(the_files = im_files, the_data = d_null) {
  
  for (i in 1:length(the_files)){
    
    tmp.file <- the_files[i]
    tmp.st <- str_split(tmp.file, "_")
    tmp.yr <- tmp.st[[1]][2]
    tmp.co <- str_split(tmp.st[[1]][3], "\\.")[[1]][1]
    
    tmp.dat <- 
      read_csv(paste0(cdl_path, tmp.file)) %>% 
      mutate(year = tmp.yr,
             county = tmp.co)
    the_data <- 
      bind_rows(the_data, tmp.dat)
  }
  
  return(the_data)
  
}



# get data ----------------------------------------------------------------

d_null <- NULL

d_ca <- ReadDaFiles(the_files = ca_files, the_data = d_null)

d_im <- ReadDaFiles(the_files = im_files, the_data = d_null)
d_si <- ReadDaFiles(the_files = si_files, the_data = d_null)
d_tu <- ReadDaFiles(the_files = tu_files, the_data = d_null)
d_yo <- ReadDaFiles(the_files = yo_files, the_data = d_null)


# all ---------------------------------------------------------------------

d <- 
  d_ca %>% 
  mutate(county = "all counties") %>% 
  bind_rows(d_im) %>% 
  bind_rows(d_yo) %>% 
  bind_rows(d_si) %>% 
  bind_rows(d_tu) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  janitor::clean_names()

d %>% 
  write_csv("R/data_tidy/cdl_landuse-acreage.csv")

d %>% 
  filter(category == "alfalfa") %>% 
  ggplot(aes(year, acreage)) + 
  geom_col(aes(fill = county), position = position_dodge())
