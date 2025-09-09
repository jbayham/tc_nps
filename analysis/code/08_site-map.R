#This script reads in the park visitation data and plots the locations

library(pacman)
p_load(tidyverse,sf,mapview,readxl,janitor,tmap)

source("project_init.R")

###################################
park_attributes <- read_excel("build/inputs/park_attributes.xlsx") %>%
  select(code=park,short_names)

cs_res_dem <- readRDS("analysis/cache/cs_results_dem.rds") %>% 
  select(code_dest)

park_subset <- readRDS("build/cache/park_subset.rds") %>%
  filter(primary==1) %>%
  select(code,code_dest,placekey,dest_lat,dest_lon) %>%
  filter(!(code_dest %in% c("FOLA_02","GRTE_03","GUMO_02","DEWA"))) %>%
  inner_join(cs_res_dem) %>%
  inner_join(park_attributes) 

park_geo <- park_subset %>%
  st_as_sf(coords=c("dest_lon","dest_lat"),crs=4326)

park_map <- tm_basemap("OpenStreetMap.Mapnik") + #start with a basemap
  tm_shape(park_geo) +
  #tm_fill(col="placekey",alpha=.31) +
  tm_symbols(col="#CA78E3",size = 1) +
  tm_text(text = "code",xmod = -1,ymod = 2)

tmap_leaflet(park_map)
tmap_save(park_map,"analysis/outputs/site_map.html")
