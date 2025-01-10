#This script reads in the park visitation data and plots the locations

library(pacman)
p_load(tidyverse,sf,mapview)

source("project_init.R")

###################################
#Read in park visitation data
park_monthly_pat <- read_csv("build/inputs/park_monthly_pat.csv")

park_placekeys <- readRDS("build/cache/park_placekeys.rds")

for_review <- park_monthly_pat %>%
  drop_na(visitor_home_aggregation) %>%
  group_by(placekey,parent_placekey,location_name) %>%
  summarize(min_date = as_date(min(date_range_start,na.rm = TRUE)),
            max_date = as_date(max(date_range_start,na.rm = TRUE)),
            mean_visits = mean(raw_visitor_counts,na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(select(park_placekeys,park,placekey),.,by = "placekey")

for_review %>%
  write_csv("build/cache/location_date_ranges.csv")


park_monthly_pat %>%
  select(placekey,parent_placekey,location_name,date_range_start,raw_visitor_counts,visitor_home_aggregation,wkt_area_sq_meters) %>%
  filter(placekey %in% c("zzy-223@5qg-4zk-9vf")) %>%
  arrange(date_range_start) %>%
  View()

park_geo <- park_monthly_pat %>%
  drop_na(visitor_home_aggregation) %>%
  select(placekey,parent_placekey,location_name,latitude,longitude,wkt_area_sq_meters,polygon_wkt) %>%
  distinct(placekey,.keep_all = TRUE) %>%
  st_as_sf(wkt = "polygon_wkt",crs=4326)

mapview(park_geo,alpha=.5)
park_geo %>%
  filter(str_detect(placekey,"zzy-222@63s-sgd-7yv")) %>%
  mapview(zcol="placekey",alpha=.2)


#Can we identify all of the geos that overlap
overlap <- st_intersects(park_geo)
overlap <- st_covered_by(park_geo)

overlap_area <- st_intersection(park_geo[2,],park_geo) %>%
  mutate(overlap_area = st_area(polygon_wkt))
